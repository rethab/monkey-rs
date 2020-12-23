pub mod code;
pub mod compiler;
mod context;
pub mod frame;
mod stack;

use super::object;
use super::object::builtins;
use code::*;
use compiler::*;
use frame::*;
use stack::*;

use std::collections::HashMap;
use std::convert::TryInto;

const MAX_FRAMES: usize = 1024;

pub struct Vm<'a> {
    frames: Vec<Frame>,
    constants: &'a Vec<object::Object>,
    globals: HashMap<u16, object::Object>,
    stack: Stack,
}

impl<'a> Vm<'a> {
    pub fn new(b: &'a Bytecode) -> Self {
        let main_func = object::CompiledFunction {
            instructions: b.instructions.clone(),
            num_locals: 0,
            num_parameters: 0,
        };
        let main_cl = object::Closure {
            func: main_func,
            free: vec![],
        };
        let main_frame = Frame::new(main_cl, 0);
        Self {
            frames: vec![main_frame],
            constants: b.constants,
            globals: HashMap::new(),

            stack: Stack::with_capacity(MAX_FRAMES),
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        use Op::*;

        while self.current_frame().more_instructions() {
            self.inc_ip(1);

            let instr = self.current_frame().current_instruction();
            let op: Op = instr.clone().try_into()?;

            match op {
                Constant => {
                    let idx = self.current_frame().read_bigendian();
                    let value = self.constants[idx as usize].clone();
                    self.stack.push(value.clone());

                    self.inc_ip(2);
                }
                True => self.stack.push(object::TRUE),
                False => self.stack.push(object::FALSE),
                Pop => {
                    self.stack.pop();
                }
                Null => {
                    self.stack.push(object::NULL);
                }
                Array => {
                    let length = self.current_frame().read_bigendian();
                    let mut array = Vec::with_capacity(length as usize);
                    for _ in 0..length {
                        array.push(self.stack.pop());
                    }
                    array.reverse();
                    self.stack.push(object::Object::Array(array));

                    self.inc_ip(2);
                }
                Hash => {
                    let length = self.current_frame().read_bigendian();
                    let mut hash = Vec::with_capacity((length / 2) as usize);
                    for _ in 0..length {
                        let value = self.stack.pop();
                        let key = self.stack.pop();
                        hash.push((key, value));
                    }
                    hash.reverse();
                    self.stack.push(object::Object::Map(hash));

                    self.inc_ip(2);
                }
                Index => {
                    let index = self.stack.pop();
                    let container = self.stack.pop();

                    match container {
                        object::Object::Array(elems) => {
                            let int_index = int_or_error(index)? as usize;
                            let value = elems.get(int_index).cloned().unwrap_or(object::NULL);
                            self.stack.push(value);
                        }
                        object::Object::Map(values) => {
                            let value = values
                                .into_iter()
                                .find(|(k, _)| *k == index)
                                .map(|(_, v)| v)
                                .unwrap_or(object::NULL);
                            self.stack.push(value);
                        }
                        other => return Err(format!("Cannot index {}", other.inspect())),
                    }
                }
                Add | Sub | Mul | Div => {
                    self.run_binary_op(op)?;
                }
                Equal | NotEqual | GreaterThan | LessThan => {
                    self.run_binary_comp_op(op)?;
                }
                Minus | Bang => {
                    self.run_unary_op(op)?;
                }
                GetGlobal => {
                    let idx = self.current_frame().read_bigendian();
                    let value = self
                        .globals
                        .get(&idx)
                        .unwrap_or_else(|| panic!("Global {} not found", idx));
                    self.stack.push(value.clone());
                    self.inc_ip(2);
                }
                SetGlobal => {
                    let idx = self.current_frame().read_bigendian();
                    let value = self.stack.pop();
                    self.globals.insert(idx, value.clone());
                    self.inc_ip(2);
                }
                SetLocal => {
                    let local_idx = self.current_frame().read_u8() as usize;
                    self.inc_ip(1);

                    let stack_idx = self.current_frame().base_pointer as usize + local_idx;
                    let obj = self.stack.pop();
                    self.stack.push_at(stack_idx, obj);
                }
                GetLocal => {
                    let local_idx = self.current_frame().read_u8() as usize;
                    self.inc_ip(1);

                    let stack_idx = self.current_frame().base_pointer as usize + local_idx;
                    let obj = self.stack.peek_at(stack_idx).clone();
                    self.stack.push(obj);
                }
                GetBuiltin => {
                    let idx = self.current_frame().read_u8();
                    self.inc_ip(1);

                    if let Some(obj) = builtins::lookup_builtin_by_idx(idx) {
                        self.stack.push(obj);
                    } else {
                        panic!("{} is not a builtin", idx);
                    }
                }
                Jump => {
                    let pos = self.current_frame().read_bigendian();
                    self.set_ip(pos as i32 - 1);
                }
                JumpNotTrue => {
                    let jump = !self.stack_pop_bool()?;

                    let pos = self.current_frame().read_bigendian();
                    self.inc_ip(2);

                    if jump {
                        self.set_ip(pos as i32 - 1);
                    }
                }
                Call => {
                    let argc = self.current_frame().read_u8();
                    self.inc_ip(1);
                    self.call_function(argc)?;
                }
                ReturnValue => {
                    let val = self.stack.pop();
                    let frame = self.pop_frame();
                    self.stack.set_sp(frame.base_pointer - 1);
                    self.stack.push(val);
                }
                Return => {
                    let frame = self.pop_frame();
                    self.stack.set_sp(frame.base_pointer - 1);
                    self.stack.push(object::NULL);
                }
                Closure => {
                    let func_idx = self.current_frame().read_bigendian();
                    self.inc_ip(2);

                    let n_free = self.current_frame().read_u8();
                    self.inc_ip(1);

                    self.push_closure(func_idx, n_free)?;
                }
                CurrentClosure => {
                    let cl = self.current_frame().cl.clone();
                    self.stack.push(object::Object::Closure(cl));
                }
                GetFree => {
                    let idx = self.current_frame().read_u8();
                    self.inc_ip(1);

                    let free_vars = &self.current_frame().cl.free;
                    let val = free_vars
                        .get(idx as usize)
                        .unwrap_or_else(|| panic!("Free variable with index {} not found", idx))
                        .clone();
                    self.stack.push(val);
                }
            }
        }
        Ok(())
    }

    fn push_closure(&mut self, func_idx: u16, n_free: u8) -> Result<(), String> {
        let func = if let object::Object::CompiledFunction(func) =
            self.constants[func_idx as usize].clone()
        {
            func
        } else {
            return Err(format!(
                "Object at stack position {} is not a function",
                func_idx
            ));
        };

        let mut free = Vec::new();
        let sp_idx_base: usize = self.stack.sp() - n_free as usize;
        for i in 0..n_free as usize {
            free.push(self.stack.peek_at(sp_idx_base + i).clone());
        }
        self.stack.set_sp(sp_idx_base);
        let cl = object::Closure { func, free };
        self.stack.push(object::Object::Closure(cl));
        Ok(())
    }

    fn call_function(&mut self, argc: u8) -> Result<(), String> {
        use object::Object::*;
        let base_pointer = self.stack.sp() as usize - argc as usize;
        let obj = self.stack.peek_offset(argc as usize);
        match obj.clone() {
            Closure(closure) => self.call_closure(argc, base_pointer, closure),
            Builtin { func } => {
                let mut arguments = Vec::new();
                for _ in 0..argc {
                    arguments.push(self.stack.pop());
                }
                arguments.reverse();
                let result = func(arguments)?;
                self.stack.push(result);
                Ok(())
            }
            other => Err(format!("calling non-closure and non-builtin: {:?}", other)),
        }
    }

    fn call_closure(
        &mut self,
        argc: u8,
        base_pointer: usize,
        cl: object::Closure,
    ) -> Result<(), String> {
        if cl.func.num_parameters != argc {
            return Err(format!(
                "wrong number of arguments: want={}, got={}",
                cl.func.num_parameters, argc
            ));
        }

        let num_locals = cl.func.num_locals;

        let frame = Frame::new(cl, base_pointer);
        self.stack.inc_sp(num_locals);
        self.push_frame(frame);
        Ok(())
    }

    fn current_frame(&self) -> &Frame {
        self.frames.last().expect("Current: No frames left")
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames.push(f)
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames.pop().expect("Pop: No frames left")
    }

    fn inc_ip(&mut self, n: i32) {
        self.frames.last_mut().expect("No frames left").inc_ip(n)
    }

    fn set_ip(&mut self, n: i32) {
        self.frames.last_mut().expect("No frames left").set_ip(n)
    }

    fn run_binary_op(&mut self, op: Op) -> Result<(), String> {
        let b = self.stack.pop();

        match b {
            object::Object::Integer(b) => self.run_binary_int_op(op, b),
            object::Object::String_(b) => {
                let mut a = self.stack_pop_string()?;
                use Op::*;
                match op {
                    Add => {
                        a.push_str(&b);
                        self.stack.push(object::Object::String_(a));
                        Ok(())
                    }
                    other => Err(format!("Not a string operation: {:?}", other)),
                }
            }
            other => Err(format!("{:?} cannot be applied to {}", op, other.inspect())),
        }
    }

    fn run_binary_int_op(&mut self, op: Op, b: i64) -> Result<(), String> {
        let a = self.stack_pop_int()?;
        use Op::*;
        match op {
            Add => {
                self.stack.push(object::Object::Integer(a + b));
            }
            Sub => {
                self.stack.push(object::Object::Integer(a - b));
            }
            Mul => {
                self.stack.push(object::Object::Integer(a * b));
            }
            Div => {
                self.stack.push(object::Object::Integer(a / b));
            }
            other => panic!("Not a binary op: {:?}", other),
        };
        Ok(())
    }

    fn run_binary_comp_op(&mut self, op: Op) -> Result<(), String> {
        let b = self.stack.pop();
        let a = self.stack.pop();
        use Op::*;
        let res = match op {
            Equal => a == b,
            NotEqual => a != b,
            GreaterThan => int_or_error(a)? > int_or_error(b)?,
            LessThan => int_or_error(a)? < int_or_error(b)?,
            other => panic!("Not a binary comp op: {:?}", other),
        };

        self.stack.push(object::Object::Boolean(res));
        Ok(())
    }

    fn run_unary_op(&mut self, op: Op) -> Result<(), String> {
        let a = self.stack.pop();
        use Op::*;
        let res = match op {
            Minus => object::Object::Integer(-int_or_error(a)?),
            Bang => {
                if !bool_or_error(a)? {
                    object::TRUE
                } else {
                    object::FALSE
                }
            }
            other => panic!("Not a binary comp op: {:?}", other),
        };

        self.stack.push(res);
        Ok(())
    }

    pub fn last_popped_stack_elem(&self) -> &object::Object {
        self.stack.last_popped_elem()
    }

    fn stack_pop_int(&mut self) -> Result<i64, String> {
        int_or_error(self.stack.pop())
    }

    fn stack_pop_string(&mut self) -> Result<String, String> {
        string_or_error(self.stack.pop())
    }

    fn stack_pop_bool(&mut self) -> Result<bool, String> {
        bool_or_error(self.stack.pop())
    }
}

fn int_or_error(obj: object::Object) -> Result<i64, String> {
    match obj {
        object::Object::Integer(i) => Ok(i),
        other => Err(format!("Expected integer on stack, but got: {:?}", other)),
    }
}

fn string_or_error(obj: object::Object) -> Result<String, String> {
    match obj {
        object::Object::String_(s) => Ok(s),
        other => Err(format!("Expected String on stack, but got: {:?}", other)),
    }
}

fn bool_or_error(obj: object::Object) -> Result<bool, String> {
    match obj {
        object::Object::Boolean(i) => Ok(i),
        object::NULL => Ok(false),
        other => Err(format!("Expected bool on stack, but got: {:?}", other)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() {
        assert_eq!(run_vm_test("1"), int(1));
        assert_eq!(run_vm_test("2"), int(2));
        assert_eq!(run_vm_test("1 + 2"), int(3));
        assert_eq!(run_vm_test("1 - 2"), int(-1));
        assert_eq!(run_vm_test("2 * 2"), int(4));
        assert_eq!(run_vm_test("4 / 2"), int(2));
        assert_eq!(run_vm_test("4 / 2 + 2"), int(4));
        assert_eq!(run_vm_test("5 * (2 + 10)"), int(60));
        assert_eq!(run_vm_test("50 / 2 * 2 + 10 - 5"), int(55));
        assert_eq!(run_vm_test("true"), boolean(true));
        assert_eq!(run_vm_test("false"), boolean(false));
        assert_eq!(run_vm_test("1 == 1"), boolean(true));
        assert_eq!(run_vm_test("false != true"), boolean(true));
        assert_eq!(run_vm_test("1 > 2"), boolean(false));
        assert_eq!(run_vm_test("12 > 2"), boolean(true));
        assert_eq!(run_vm_test("(2 > 1) != false"), boolean(true));
        assert_eq!(run_vm_test("-1"), int(-1));
        assert_eq!(run_vm_test("!true"), boolean(false));
        assert_eq!(run_vm_test("!!true"), boolean(true));
        assert_eq!(run_vm_test("!(-1 > 2)"), boolean(true));
    }

    #[test]
    fn test_string_operations() {
        assert_eq!(run_vm_test("\"hello\""), string("hello"));
        assert_eq!(run_vm_test("\"hel\" +\"lo\""), string("hello"));
        assert_eq!(run_vm_test("\"hel\" +\"l\" + \"o\""), string("hello"));
        assert_eq!(run_vm_test("(\"hel\" +\"l\") == \"hello\""), boolean(false));
        assert_eq!(run_vm_test("\"he\" == \"he\""), boolean(true));
        assert_eq!(run_vm_test("\"he\" != \"he\""), boolean(false));
    }

    #[test]
    fn test_arrays() {
        assert_eq!(run_vm_test("[]"), array(vec![]));
        assert_eq!(run_vm_test("[1]"), array(vec![int(1)]));
        assert_eq!(
            run_vm_test("[1 + 2, 3 * 4, 5 + 6]"),
            array(vec![int(3), int(12), int(11)])
        );
        assert_eq!(run_vm_test("[1][0]"), int(1));
        assert_eq!(run_vm_test("[][0]"), null());
        assert_eq!(run_vm_test("[1, 2, 3][3]"), null());
        assert_eq!(run_vm_test("[1, 2][1]"), int(2));
        assert_eq!(run_vm_test("[1, 2, 3][1 + 1]"), int(3));
        assert_eq!(run_vm_test("let xs = [1, 2, 3]; xs[1]"), int(2));
    }

    #[test]
    fn test_hash() {
        assert_eq!(run_vm_test("{}"), hash(vec![]));
        assert_eq!(run_vm_test("{1: 2}"), hash(vec![(int(1), int(2))]));
        assert_eq!(
            run_vm_test("{1 + 1: 2 + 3, 3 + 4: 5 + 6}"),
            hash(vec![(int(2), int(5)), (int(7), int(11))])
        );
        assert_eq!(run_vm_test("{1: 2}[1]"), int(2));
        assert_eq!(run_vm_test("{1: 2}[0]"), null());
        assert_eq!(run_vm_test("{1 + 3: 2 + 4}[2 + 2]"), int(6));
        assert_eq!(run_vm_test("{1: 2, 3: 4, 2: 6}[3]"), int(4));
        assert_eq!(run_vm_test("let xs = {1: 2, 3: 4}; xs[1] + xs[3]"), int(6));
    }

    #[test]
    fn test_conditionals() {
        assert_eq!(run_vm_test("if (true) { 10 } "), int(10));
        assert_eq!(run_vm_test("if (true) { 10 } else { 20 }"), int(10));
        assert_eq!(run_vm_test("if (false) { 10 } else { 20 }"), int(20));
        assert_eq!(run_vm_test("if (1 < 2) { 10 } else { 20 } "), int(10));
        assert_eq!(run_vm_test("if (1 > 2) { 10 } else { 20 } "), int(20));
        assert_eq!(run_vm_test("if (1 > 2) { 10 } "), null());
        assert_eq!(run_vm_test("!(if(false){5})"), boolean(true));
        assert_eq!(run_vm_test("if ((if(false){10})){10} else {20}"), int(20));
    }

    #[test]
    #[should_panic]
    fn unsupported_let_in_if_consequence() {
        assert_eq!(
            run_vm_test("if (true) { let a = 1 } else { let a = 2 }"),
            null()
        );
    }

    #[test]
    fn test_functions_without_arguments() {
        assert_eq!(run_vm_test("fn() {}();"), null());
        assert_eq!(
            run_vm_test(
                "
                    let noReturn = fn() {};
                    let noReturnTwo = fn() { noReturn(); };
                    noReturn();
                    noReturnTwo();
                "
            ),
            null()
        );
        assert_eq!(run_vm_test("let a = fn() { 5 + 10; }; a();"), int(15));
        assert_eq!(run_vm_test("fn() { 5 + 10; }();"), int(15));
        assert_eq!(
            run_vm_test(
                "
                    let one = fn() { 1 };
                    let two = fn() { one() + one() };
                    one() + two()
                "
            ),
            int(3)
        );
    }

    #[test]
    fn test_functions_with_arguments() {
        assert_eq!(
            run_vm_test("let identity = fn(a) { a }; identity(4)"),
            int(4)
        );
        assert_eq!(
            run_vm_test("let sum = fn(a, b) { a + b }; sum(1, 2)"),
            int(3)
        );
        assert_eq!(
            run_vm_test("let sum = fn(a, b) { let c = a + b; c }; sum(1, 2)"),
            int(3)
        );
        assert_eq!(run_vm_test("fn(a, b) { let c = a + b; c }(1, 2)"), int(3));
        assert_eq!(
            run_vm_test(
                "
                    let sum = fn(a, b) { let c = a + b; c };
                    let outer = fn() { sum(1, 2) + sum(3, 4) };
                    outer()
                "
            ),
            int(10)
        );
        assert_eq!(
            run_vm_test(
                "
                    let globalNum = 10;
                    let sum = fn(a, b) { let c = a + b; c + globalNum };
                    let outer = fn() { sum(1, 2) + sum(3, 4) + globalNum };
                    outer() + globalNum
                "
            ),
            int(50)
        );
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        assert_eq!(
            run_vm_error("fn() { 1; }(1)"),
            "wrong number of arguments: want=0, got=1"
        );
        assert_eq!(
            run_vm_error("fn(a) { a; }()"),
            "wrong number of arguments: want=1, got=0"
        );
        assert_eq!(
            run_vm_error("fn(a, b) { a + b; }(1)"),
            "wrong number of arguments: want=2, got=1"
        );
    }

    #[test]
    fn test_functions_with_return_statement() {
        assert_eq!(run_vm_test("fn() { return 99; 100; }()"), int(99));
        assert_eq!(run_vm_test("fn() { return 99; return 100; }()"), int(99));
    }

    #[test]
    fn test_local_bindings() {
        assert_eq!(run_vm_test("let a = 4; fn() { a }()"), int(4));
        assert_eq!(run_vm_test("let a = 4; let b = fn() { a }; b()"), int(4));
        assert_eq!(
            run_vm_test("let a = 4; fn() { let b = a + 1; b }()"),
            int(5)
        );
        assert_eq!(run_vm_test("let a = fn() { let a = 1; a }; a()"), int(1));
        assert_eq!(
            run_vm_test(
                "
                let firstFoobar = fn() { let foobar = 50; foobar; };
                let secondFoobar = fn() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar()
            "
            ),
            int(150)
        );
        assert_eq!(
            run_vm_test(
                "
                    let globalSeed = 50;
                    let minusOne = fn() {
                        let num = 1;
                        globalSeed - num
                    };
                    let minusTwo = fn() {
                        let num = 2;
                        globalSeed - num
                    };
                    minusOne() + minusTwo()
                "
            ),
            int(97)
        );
    }

    #[test]
    #[should_panic]
    fn unsupported_function_returning_function() {
        // doesn't work, because an ast::Call refers to a function which can only be a literal or a
        // function. it cannot be an arbitrary expression. If we make the indirection via a let
        // binding (see below), it works though.
        run_vm_test("let a = fn() { 1; }; let b = fn() { a; }; b()();");
    }

    #[test]
    fn function_returning_function() {
        assert_eq!(
            run_vm_test(
                "
                let returnsOne = fn() { 1; };
                let returnsOneReturner = fn() { returnsOne; };
                let returnedOne = returnsOneReturner();
                returnedOne();
            "
            ),
            int(1)
        );
        assert_eq!(
            run_vm_test(
                "
                let returnsOneReturner = fn() {
                    let returnsOne = fn() { 1; };
                    returnsOne;
                };
                let returnedOne = returnsOneReturner();
                returnedOne();
            "
            ),
            int(1)
        );
    }

    #[test]
    fn test_globals() {
        assert_eq!(run_vm_test("let one = 1; one"), int(1));
        assert_eq!(run_vm_test("let one = 1 let two = 2; one + two"), int(3));
        assert_eq!(
            run_vm_test("let one = 1 let two = one + one; one + two"),
            int(3)
        );
        assert_eq!(
            run_vm_test("let one = 1 let two = 2; if (two > one) { two } else { one }"),
            int(2)
        );
    }

    #[test]
    fn test_builtins() {
        assert_eq!(run_vm_test("len(\"\")"), int(0));
        assert_eq!(run_vm_test("len(\"four\")"), int(4));
        assert_eq!(run_vm_test("len(\"hello world\")"), int(11));
        assert_eq!(
            run_vm_error("len(1)"),
            "argument to 'len' not supported, got INTEGER".to_owned()
        );
        assert_eq!(
            run_vm_error("len(\"one\", \"two\")"),
            "wrong number of arguments. got=2, want=1".to_owned()
        );
        assert_eq!(run_vm_test("len([1, 2, 3])"), int(3));
        assert_eq!(run_vm_test("len([])"), int(0));
        assert_eq!(run_vm_test("puts(\"hello, world\")"), null());
        assert_eq!(run_vm_test("head([1, 2, 3])"), int(1));
        assert_eq!(
            run_vm_error("head([])"),
            "Called 'head' on empty array".to_owned()
        );
        assert_eq!(
            run_vm_error("head(1)"),
            "argument to 'head' not supported, got INTEGER".to_owned()
        );
        assert_eq!(run_vm_test("last([1, 2, 3])"), int(3));
        assert_eq!(
            run_vm_error("last([])"),
            "Called 'last' on empty array".to_owned()
        );
        assert_eq!(
            run_vm_error("last(1)"),
            "argument to 'last' not supported, got INTEGER".to_owned()
        );
        assert_eq!(run_vm_test("tail([1, 2, 3])"), array(vec![int(2), int(3)]));
        assert_eq!(
            run_vm_error("tail([])"),
            "Called 'tail' on empty array".to_owned()
        );
        assert_eq!(
            run_vm_error("tail(1)"),
            "argument to 'tail' not supported, got INTEGER".to_owned()
        );
        assert_eq!(run_vm_test("append([], 1)"), array(vec![int(1)]));
        assert_eq!(
            run_vm_error("append(1, 1)"),
            "first argument to 'append' must be array, but got INTEGER".to_owned()
        );
    }

    #[test]
    fn test_closures() {
        assert_eq!(
            run_vm_test(
                "
                    let newClosure = fn(a) { fn() { a; }; };
                    let closure = newClosure(99);
                    closure();
                "
            ),
            int(99)
        );
        assert_eq!(
            run_vm_test(
                "
                    let newAdder = fn(a, b) { fn(c) { a + b + c }; };
                    let adder = newAdder(1, 2);
                    adder(8);
                "
            ),
            int(11)
        );
        assert_eq!(
            run_vm_test(
                "
                    let newAdder = fn(a, b) {
                        let c = a + b;
                        fn(d) { c + d };
                        };
                    let adder = newAdder(1, 2);
                    adder(8);
                "
            ),
            int(11)
        );
        assert_eq!(
            run_vm_test(
                "
                    let newAdderOuter = fn(a, b) {
                        let c = a + b;
                        fn(d) {
                            let e = d + c;
                            fn(f) { e + f; };
                        };
                    };
                    let newAdderInner = newAdderOuter(1, 2)
                    let adder = newAdderInner(3);
                    adder(8);
                "
            ),
            int(14)
        );
        assert_eq!(
            run_vm_test(
                "
                    let a = 1;
                    let newAdderOuter = fn(b) {
                        fn(c) {
                            fn(d) { a + b + c + d };
                        };
                    };
                    let newAdderInner = newAdderOuter(2)
                    let adder = newAdderInner(3);
                    adder(8);
                "
            ),
            int(14)
        );
        assert_eq!(
            run_vm_test(
                "
                let newClosure = fn(a, b) {
                    let one = fn() { a; };
                    let two = fn() { b; };
                    fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();
                "
            ),
            int(99)
        );
    }

    #[test]
    fn test_recurisve_functions() {
        assert_eq!(
            run_vm_test(
                "
                    let countDown = fn(x) {
                        if (x == 0) { return 0; }
                        else { return countDown(x - 1); }
                    };
                    countDown(1);
                "
            ),
            int(0)
        );
        assert_eq!(
            run_vm_test(
                "
                    let countDown = fn(x) {
                        if (x == 0) { return 0; }
                        else { return countDown(x - 1); }
                    };
                    let wrapper = fn() {
                        countDown(1);
                    };
                    wrapper();
                "
            ),
            int(0)
        );
        assert_eq!(
            run_vm_test(
                "
                    let wrapper = fn() {
                        let countDown = fn(x) {
                            if (x == 0) { return 0; }
                            else { return countDown(x - 1); }
                        };
                        countDown(1);
                    };
                    wrapper();
                "
            ),
            int(0)
        );
    }

    fn compile(input: &str) -> Compiler {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p).expect("Failed to compile");
        c
    }

    fn run_vm_test(input: &str) -> object::Object {
        let c = compile(input);
        let bytecode = &c.bytecode();
        let mut vm = Vm::new(bytecode);
        vm.run().expect("Failed to run");

        let result = vm.last_popped_stack_elem();
        result.clone()
    }

    fn run_vm_error(input: &str) -> String {
        let c = compile(input);
        let bytecode = &c.bytecode();
        let mut vm = Vm::new(bytecode);
        vm.run().expect_err("Failed to fail ;-)")
    }

    fn parse(input: &str) -> ast::Program {
        *Parser::new(Lexer::new(input)).parse_program().unwrap()
    }

    fn int(i: i64) -> object::Object {
        object::Object::Integer(i)
    }

    fn string(string: &'static str) -> object::Object {
        object::Object::String_(string.to_owned())
    }

    fn array(elems: Vec<object::Object>) -> object::Object {
        object::Object::Array(elems)
    }

    fn hash(elems: Vec<(object::Object, object::Object)>) -> object::Object {
        object::Object::Map(elems)
    }

    fn boolean(b: bool) -> object::Object {
        object::Object::Boolean(b)
    }

    fn null() -> object::Object {
        object::NULL
    }
}
