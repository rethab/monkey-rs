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
                Add => {
                    let a = self.stack_pop_int()?;
                    let b = self.stack_pop_int()?;
                    self.stack.push(object::Object::Integer(a + b));
                }
                Pop => {
                    self.stack.pop()?;
                }
            }
        }
        Ok(())
    }

    pub fn last_popped_stack_elem(&self) -> &object::Object {
        self.stack.last_popped_elem()
    }

    fn stack_pop_int(&mut self) -> Result<i64, String> {
        match self.stack.pop()? {
            object::Object::Integer(i) => Ok(i),
            other => Err(format!("Expected integer on stack, but got: {:?}", other)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() -> Result<(), String> {
        run_vm_test("1", int(1))?;
        run_vm_test("2", int(2))?;
        run_vm_test("1 + 2", int(3))
    }

    fn run_vm_test(input: &str, expected: object::Object) -> Result<(), String> {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p)?;

        let bytecode = &c.bytecode();
        let mut vm = Vm::new(bytecode);
        vm.run()?;

        let result = vm.last_popped_stack_elem();
        assert_eq!(*result, expected);
        Ok(())
    }

    fn parse(input: &str) -> ast::Program {
        *Parser::new(Lexer::new(input)).parse_program().unwrap()
    }

    fn int(i: i64) -> object::Object {
        object::Object::Integer(i)
    }
}
