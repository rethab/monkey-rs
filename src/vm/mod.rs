pub mod frame;
mod stack;

use crate::code::*;
use crate::compiler::*;
use crate::object;
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
        let main_frame = Frame::new(b.instructions.clone());
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
                    let obj = self.stack.peek();
                    let instructions = if let object::Object::CompiledFunction(instructions) = obj {
                        instructions
                    } else {
                        return Err(format!("Expected function in stack: {:?}", obj));
                    };
                    let frame = Frame::new(instructions.clone());
                    self.push_frame(frame);
                }
                ReturnValue => {
                    let val = self.stack.pop();
                    self.pop_frame();
                    self.stack.pop(); // OpCall TODO: why not use pop above?
                    self.stack.push(val);
                }
                Return => {
                    self.pop_frame();
                    self.stack.pop(); // OpCall TODO: why not use pop above?
                    self.stack.push(object::NULL);
                }
            }
        }
        Ok(())
    }

    fn current_frame(&self) -> &Frame {
        self.frames.last().expect("Current: No frames left")
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames.push(f)
    }

    fn pop_frame(&mut self) {
        self.frames.pop().expect("Pop: No frames left");
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
    fn test_functions_with_return_statement() {
        assert_eq!(run_vm_test("fn() { return 99; 100; }()"), int(99));
        assert_eq!(run_vm_test("fn() { return 99; return 100; }()"), int(99));
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

    fn run_vm_test(input: &str) -> object::Object {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p).expect("Failed to compile");

        let bytecode = &c.bytecode();
        let mut vm = Vm::new(bytecode);
        vm.run().expect("Failed to run");

        let result = vm.last_popped_stack_elem();
        result.clone()
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
