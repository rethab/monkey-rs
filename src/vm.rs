use crate::code::*;
use crate::compiler::*;
use crate::object;

use std::collections::HashMap;
use std::convert::TryInto;

const STACK_SIZE: usize = 2048;

pub struct Vm<'a> {
    instructions: &'a Instructions,
    constants: &'a Vec<object::Object>,
    globals: HashMap<u16, object::Object>,
    stack: Stack,
}

pub struct Stack {
    elems: Vec<object::Object>,
    sp: usize, // points to next free
}

impl Stack {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            elems: vec![object::NULL; capacity],
            sp: 0,
        }
    }

    pub fn pop(&mut self) -> object::Object {
        let object = self.elems[self.sp - 1].clone();
        self.sp -= 1;
        object
    }

    pub fn push(&mut self, obj: object::Object) {
        self.elems[self.sp] = obj;
        self.sp += 1;
    }

    pub fn last_popped_elem(&self) -> &object::Object {
        &self.elems[self.sp]
    }
}

impl<'a> Vm<'a> {
    pub fn new(b: &'a Bytecode) -> Self {
        Self {
            instructions: b.instructions,
            constants: b.constants,
            globals: HashMap::new(),

            stack: Stack::with_capacity(STACK_SIZE),
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        use Op::*;
        let mut i = 0;

        while i < self.instructions.len() {
            let op: Op = self.instructions[i].try_into()?;

            match op {
                Constant => {
                    let idx = read_bigendian(self.instructions, i + 1);
                    let value = self.constants[idx as usize].clone();
                    self.stack.push(value);

                    i += 2;
                }
                True => self.stack.push(object::TRUE),
                False => self.stack.push(object::FALSE),
                Pop => {
                    self.stack.pop();
                }
                Null => {
                    self.stack.push(object::NULL);
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
                    let idx = read_bigendian(self.instructions, i + 1);
                    let value = self
                        .globals
                        .get(&idx)
                        .unwrap_or_else(|| panic!("Global {} not found", idx));
                    self.stack.push(value.clone());
                    i += 2;
                }
                SetGlobal => {
                    let idx = read_bigendian(self.instructions, i + 1);
                    let value = self.stack.pop();
                    self.globals.insert(idx, value);
                    i += 2;
                }
                Jump => {
                    let pos = read_bigendian(self.instructions, i + 1);
                    i = pos as usize - 1;
                }
                JumpNotTrue => {
                    let jump = !self.stack_pop_bool()?;

                    let pos = read_bigendian(self.instructions, i + 1);
                    i += 2;

                    if jump {
                        i = pos as usize - 1;
                    }
                }
            }
            i += 1;
        }
        Ok(())
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
    fn test_string_operatins() {
        assert_eq!(run_vm_test("\"hello\""), string("hello"));
        assert_eq!(run_vm_test("\"hel\" +\"lo\""), string("hello"));
        assert_eq!(run_vm_test("\"hel\" +\"l\" + \"o\""), string("hello"));
        assert_eq!(run_vm_test("(\"hel\" +\"l\") == \"hello\""), boolean(false));
        assert_eq!(run_vm_test("\"he\" == \"he\""), boolean(true));
        assert_eq!(run_vm_test("\"he\" != \"he\""), boolean(false));
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

    fn boolean(b: bool) -> object::Object {
        object::Object::Boolean(b)
    }

    fn null() -> object::Object {
        object::NULL
    }
}
