use crate::code::*;
use crate::compiler::*;
use crate::object;

use std::convert::TryInto;

const STACK_SIZE: usize = 2048;

pub struct Vm<'a> {
    instructions: &'a Instructions,
    constants: &'a Vec<object::Object>,
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

    pub fn pop(&mut self) -> Result<object::Object, String> {
        let object = self.elems[self.sp - 1].clone();
        self.sp -= 1;
        Ok(object)
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

            stack: Stack::with_capacity(STACK_SIZE),
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        use Op::*;
        let mut i = 0;

        while i < self.instructions.len() {
            let op: Op = self.instructions[i].try_into()?;
            i += 1;

            match op {
                Constant => {
                    let idx = read_bigendian(self.instructions, i);
                    let value = self.constants[idx as usize].clone();
                    self.stack.push(value);

                    i += 2;
                }
                True => self.stack.push(object::TRUE),
                False => self.stack.push(object::FALSE),
                Pop => {
                    self.stack.pop()?;
                }
                Add | Sub | Mul | Div => {
                    self.run_binary_int_op(op)?;
                }
                Equal | NotEqual | GreaterThan => {
                    self.run_binary_comp_op(op)?;
                }
                Minus | Bang => {
                    self.run_unary_op(op)?;
                }
                JumpNotTrue | Jump => {
                    unimplemented!("jump(nottrue)")
                }
            }
        }
        Ok(())
    }

    fn run_binary_int_op(&mut self, op: Op) -> Result<(), String> {
        let b = self.stack_pop_int()?;
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
        let b = self.stack.pop()?;
        let a = self.stack.pop()?;
        use Op::*;
        let res = match op {
            Equal => a == b,
            NotEqual => a != b,
            GreaterThan => int_or_error(a)? > int_or_error(b)?,
            other => panic!("Not a binary comp op: {:?}", other),
        };

        self.stack.push(object::Object::Boolean(res));
        Ok(())
    }

    fn run_unary_op(&mut self, op: Op) -> Result<(), String> {
        let a = self.stack.pop()?;
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
        self.stack.pop().and_then(int_or_error)
    }
}

fn int_or_error(obj: object::Object) -> Result<i64, String> {
    match obj {
        object::Object::Integer(i) => Ok(i),
        other => Err(format!("Expected integer on stack, but got: {:?}", other)),
    }
}

fn bool_or_error(obj: object::Object) -> Result<bool, String> {
    match obj {
        object::Object::Boolean(i) => Ok(i),
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

    fn boolean(b: bool) -> object::Object {
        object::Object::Boolean(b)
    }
}
