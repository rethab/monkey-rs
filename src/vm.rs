use crate::code::*;
use crate::compiler::*;
use crate::object;

pub struct Vm<'a> {
    instructions: &'a Instructions,
    constants: &'a Vec<object::Object>,
    stack: Vec<object::Object>,
    sp: usize, // points to next free
}

const STACK_SIZE: usize = 2048;

impl<'a> Vm<'a> {
    pub fn new(b: &'a Bytecode) -> Self {
        Self {
            instructions: b.instructions,
            constants: b.constants,

            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut i = 0;

        while i < self.instructions.len() {
            let op = self.instructions[i];
            i += 1;

            match op {
                OP_CONSTANT => {
                    let idx = read_bigendian(self.instructions, i);
                    let value = self.constants[idx as usize].clone();
                    self.stack.push(value);
                    self.sp += 1;

                    i += 2;
                }
                OP_ADD => {
                    let a = self.stack_pop_int()?;
                    let b = self.stack_pop_int()?;
                    self.stack.push(object::Object::Integer(a + b));
                    self.sp += 1;
                }
                other => unimplemented!("run: {:?}", other),
            }
        }
        Ok(())
    }

    pub fn stack_pop_int(&mut self) -> Result<i64, String> {
        let object = self.stack.pop().ok_or("empty stack")?;
        self.sp -= 1;
        match object {
            object::Object::Integer(i) => Ok(i),
            other => Err(format!("Expected integer on stack, but got: {:?}", other)),
        }
    }

    pub fn stack_top(&self) -> &object::Object {
        &self.stack[self.sp - 1]
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

        let result = vm.stack_top();
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
