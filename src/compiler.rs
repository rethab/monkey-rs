use crate::ast;
use crate::code::*;
use crate::object;

struct Compiler {
    instructions: Instructions,
    constants: Vec<object::Object>,
}

struct Bytecode {
    instructions: Instructions,
    constants: Vec<object::Object>,
}

impl Compiler {
    pub fn compile(&self, p: ast::Program) -> Result<(), String> {
        Ok(())
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() -> Result<(), String> {
        run_copmiler_test(
            "1 + 2",
            vec![int(1), int(2)],
            vec![
                make(OP_CONSTANT, &vec![0]).unwrap(),
                make(OP_CONSTANT, &vec![1]).unwrap(),
            ],
        )
    }

    fn run_copmiler_test(
        input: &str,
        expected_constants: Vec<object::Object>,
        expected_instructions: Vec<Instructions>,
    ) -> Result<(), String> {
        let p = *Parser::new(Lexer::new(input)).parse_program().unwrap();
        let c = Compiler::default();

        c.compile(p)?;
        let bytecode = c.bytecode();

        test_instructions(expected_instructions, bytecode.instructions)?;

        test_constants(expected_constants, bytecode.constants)
    }

    fn test_instructions(
        expected_instructions: Vec<Instructions>,
        actual: Instructions,
    ) -> Result<(), String> {
        let concatted = flatten(expected_instructions);

        if concatted.len() != actual.len() {
            return Err(format!(
                "wrong instructions length. expected={:?}, actual={:?}",
                concatted, actual
            ));
        }

        for (i, instr) in concatted.iter().enumerate() {
            if actual[i] != *instr {
                return Err(format!(
                    "wrong instruction at {}. expected={:?}, actual={:?}",
                    i, concatted, actual
                ));
            }
        }

        Ok(())
    }

    fn test_constants(
        expected: Vec<object::Object>,
        actual: Vec<object::Object>,
    ) -> Result<(), String> {
        if expected.len() != actual.len() {
            return Err(format!(
                "Wrong number of constants. expected={:?}, actual={:?}",
                expected, actual
            ));
        }

        for (i, con) in expected.into_iter().enumerate() {
            if con != actual[i] {
                return Err(format!("Expected constant {:?}, got {:?}", con, actual[i]));
            }
        }

        Ok(())
    }

    fn flatten<T>(xxs: Vec<Vec<T>>) -> Vec<T> {
        let mut result = Vec::new();
        for xs in xxs.into_iter() {
            for x in xs.into_iter() {
                result.push(x);
            }
        }
        result
    }

    fn int(i: i64) -> object::Object {
        object::Object::Integer(i)
    }
}
