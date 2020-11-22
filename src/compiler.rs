use crate::ast;
use crate::code::*;
use crate::object;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<object::Object>,
}

#[derive(Clone)]
pub struct Bytecode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a Vec<object::Object>,
}

type CompileResult<T> = Result<T, String>;

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) -> CompileResult<()> {
        for stmt in p.0 {
            match stmt {
                ast::Statement::Expression { value, .. } => {
                    self.compile_expression(value)?;
                    self.emit(Op::Pop, &[])?;
                }
                other => unimplemented!("compile: {:?}", other),
            }
        }
        Ok(())
    }

    fn compile_expression(&mut self, exp: ast::Expression) -> CompileResult<()> {
        match exp {
            ast::Expression::Infix { op, lhs, rhs, .. } => {
                self.compile_expression(*lhs)?;
                self.compile_expression(*rhs)?;
                let op = match op.as_str() {
                    "+" => Op::Add,
                    "-" => Op::Sub,
                    "*" => Op::Mul,
                    "/" => Op::Div,
                    "==" => Op::Equal,
                    "!=" => Op::NotEqual,
                    ">" => Op::GreaterThan,
                    other => unimplemented!("compile_expression/op: {}", other),
                };
                self.emit(op, &[])?;
            }
            ast::Expression::Prefix { op, rhs, .. } => {
                self.compile_expression(*rhs)?;
                let op = match op.as_str() {
                    "-" => Op::Minus,
                    "!" => Op::Bang,
                    other => unimplemented!("compile_expression/op: {}", other),
                };
                self.emit(op, &[])?;
            }
            ast::Expression::IntLiteral { value, .. } => {
                let int = object::Object::Integer(value as i64);
                let pos = self.add_constant(int);
                self.emit(Op::Constant, &[pos])?;
            }
            ast::Expression::BooleanLiteral { value, .. } => {
                let op = if value { Op::True } else { Op::False };
                self.emit(op, &[])?;
            }
            other => unimplemented!("compile_expression: {:?}", other),
        }
        Ok(())
    }

    fn emit(&mut self, op: Op, operands: &[i32]) -> CompileResult<i32> {
        let instruction = make(op, operands)?;
        Ok(self.add_instruction(&instruction))
    }

    fn add_instruction(&mut self, ins: &[u8]) -> i32 {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos as i32
    }

    fn add_constant(&mut self, obj: object::Object) -> i32 {
        self.constants.push(obj);
        (self.constants.len() - 1) as i32
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: &self.instructions,
            constants: &self.constants,
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
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Add, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "1; 2;",
            vec![int(1), int(2)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "1 == 2",
            vec![int(1), int(2)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Equal, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "true",
            vec![],
            vec![
                make(Op::True, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "!true",
            vec![],
            vec![
                make(Op::True, &vec![]).unwrap(),
                make(Op::Bang, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "-1",
            vec![int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Minus, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    fn run_copmiler_test(
        input: &str,
        expected_constants: Vec<object::Object>,
        expected_instructions: Vec<Instructions>,
    ) -> Result<(), String> {
        let p = *Parser::new(Lexer::new(input)).parse_program().unwrap();
        let mut c = Compiler::default();

        c.compile(p)?;
        let bytecode = c.bytecode();

        test_instructions(expected_instructions, bytecode.instructions)?;

        test_constants(expected_constants, bytecode.constants)
    }

    fn test_instructions(
        expected_instructions: Vec<Instructions>,
        actual: &Instructions,
    ) -> Result<(), String> {
        let concatted = flatten(expected_instructions.clone());

        if concatted.len() != actual.len() {
            return Err(format!(
                "wrong instructions length. expected={:?}, actual={:?}",
                display_instructions(expected_instructions),
                actual
            ));
        }

        for (i, instr) in concatted.iter().enumerate() {
            if actual[i] != *instr {
                return Err(format!(
                    "wrong instruction at {}. expected={:?}, actual={:?}",
                    i,
                    display_instructions(expected_instructions),
                    display_instructions(vec![actual.clone()]),
                ));
            }
        }

        Ok(())
    }

    fn test_constants(
        expected: Vec<object::Object>,
        actual: &Vec<object::Object>,
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
