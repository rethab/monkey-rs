use crate::ast;
use crate::code::*;
use crate::object;

use std::collections::HashMap;

struct EmittedInstruction {
    op: Op,
    position: usize,
}

#[derive(Clone)]
pub struct Context {
    parent: Option<Box<Context>>,
    symbols: HashMap<String, u16>,
    idx: u16,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            parent: None,
            symbols: HashMap::new(),
            idx: 0,
        }
    }
}

impl Context {
    fn define(&mut self, ident: ast::Identifier) -> u16 {
        self.symbols.insert(ident.value, self.idx);
        let idx = self.idx;
        self.idx += 1;
        idx
    }

    fn resolve(&self, ident: &ast::Identifier) -> Option<u16> {
        self.symbols
            .get(&ident.value)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve(ident)))
    }

    fn sub(self) -> Self {
        Self {
            idx: self.idx,
            parent: Some(Box::new(self)),
            symbols: HashMap::new(),
        }
    }

    fn unsub(mut self) -> Self {
        if let Some(mut p) = self.parent.take() {
            p.idx = self.idx;
            *p
        } else {
            self
        }
    }
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<object::Object>,
    context: Context,

    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

#[derive(Clone)]
pub struct Bytecode<'a> {
    pub instructions: &'a Instructions,
    pub constants: &'a Vec<object::Object>,
}

type CompileResult<T> = Result<T, String>;

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) -> CompileResult<()> {
        let mut ctx = self.context.clone();
        for stmt in p.0 {
            ctx = self.compile_statement(stmt, ctx)?;
        }
        self.context = ctx;
        Ok(())
    }

    fn compile_statement(
        &mut self,
        stmt: ast::Statement,
        mut ctx: Context,
    ) -> CompileResult<Context> {
        match stmt {
            ast::Statement::Expression { value, .. } => {
                ctx = self.compile_expression(value, ctx.sub())?;
                ctx = ctx.unsub();
                self.emit(Op::Pop, &[])?;
                Ok(ctx)
            }
            ast::Statement::Block { mut statements, .. } if statements.len() == 1 => {
                ctx = self.compile_statement(statements.remove(0), ctx.sub())?;
                ctx = ctx.unsub();
                Ok(ctx)
            }
            ast::Statement::Let {
                name, expression, ..
            } => {
                ctx = self.compile_expression(*expression, ctx.sub())?;
                ctx = ctx.unsub();
                let idx = ctx.define(name);
                self.emit(Op::SetGlobal, &[idx as i32])?;
                Ok(ctx)
            }
            other => unimplemented!("statement: {:?}", other),
        }
    }

    fn compile_expression(
        &mut self,
        exp: ast::Expression,
        mut ctx: Context,
    ) -> CompileResult<Context> {
        match exp {
            ast::Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                ctx = self.compile_expression(*condition, ctx)?;

                // jump over consequence
                let cons_jump_idx = self.emit(Op::JumpNotTrue, &[-1])?;
                ctx = self.compile_statement(*consequence, ctx)?;
                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                // jump over alternative
                let alt_jump_idx = self.emit(Op::Jump, &[-1])?;

                // fix up the jump
                let after_consequence = self.instructions.len() as i32;
                self.replace_op(cons_jump_idx, Op::JumpNotTrue, &[after_consequence])?;

                ctx = if let Some(alt) = alternative {
                    ctx = self.compile_statement(*alt, ctx)?;
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                    ctx
                } else {
                    self.emit(Op::Null, &[])?;
                    ctx
                };

                let after_alternative = self.instructions.len() as i32;
                self.replace_op(alt_jump_idx, Op::Jump, &[after_alternative])?;
                Ok(ctx)
            }
            ast::Expression::Infix { op, lhs, rhs, .. } => {
                ctx = self.compile_expression(*lhs, ctx)?;
                ctx = self.compile_expression(*rhs, ctx)?;
                let op = match op.as_str() {
                    "+" => Op::Add,
                    "-" => Op::Sub,
                    "*" => Op::Mul,
                    "/" => Op::Div,
                    "==" => Op::Equal,
                    "!=" => Op::NotEqual,
                    ">" => Op::GreaterThan,
                    "<" => Op::LessThan,
                    other => unimplemented!("compile_expression/op: {}", other),
                };
                self.emit(op, &[])?;
                Ok(ctx)
            }
            ast::Expression::Prefix { op, rhs, .. } => {
                ctx = self.compile_expression(*rhs, ctx)?;
                let op = match op.as_str() {
                    "-" => Op::Minus,
                    "!" => Op::Bang,
                    other => unimplemented!("compile_expression/op: {}", other),
                };
                self.emit(op, &[])?;
                Ok(ctx)
            }
            ast::Expression::IntLiteral { value, .. } => {
                let int = object::Object::Integer(value as i64);
                let pos = self.add_constant(int);
                self.emit(Op::Constant, &[pos])?;
                Ok(ctx)
            }
            ast::Expression::BooleanLiteral { value, .. } => {
                let op = if value { Op::True } else { Op::False };
                self.emit(op, &[])?;
                Ok(ctx)
            }
            ast::Expression::StringLiteral { value, .. } => {
                let string = object::Object::String_(value);
                let pos = self.add_constant(string);
                self.emit(Op::Constant, &[pos])?;
                Ok(ctx)
            }
            ast::Expression::ArrayLiteral { values, .. } => {
                let size = values.len();
                for value in values {
                    // TODO: re-assigning the context here is probably not correct because it would allow to define a new symbol as part of an element in the array and then re-use it in a subsequent expression. but such a construct might not be possible in practice
                    ctx = self.compile_expression(value, ctx)?;
                }
                self.emit(Op::Array, &[size as i32])?;
                Ok(ctx)
            }
            ast::Expression::Identifier(ident) => {
                let idx = ctx
                    .resolve(&ident)
                    .ok_or_else(|| format!("Identifier '{}' not found", ident.value))?;
                self.emit(Op::GetGlobal, &[idx as i32])?;
                Ok(ctx)
            }
            other => unimplemented!("compile_expression: {:?}", other),
        }
    }

    fn emit(&mut self, op: Op, operands: &[i32]) -> CompileResult<usize> {
        let instruction = make(op.clone(), operands)?;
        let pos = self.add_instruction(&instruction);

        self.set_last_instruction(op, pos);
        Ok(pos)
    }

    fn replace_op(&mut self, mut idx: usize, op: Op, operands: &[i32]) -> CompileResult<()> {
        let instruction = make(op, operands)?;
        for ins in instruction {
            self.instructions[idx] = ins;
            idx += 1;
        }
        Ok(())
    }

    fn set_last_instruction(&mut self, op: Op, position: usize) {
        if let Some(last) = self.last_instruction.take() {
            self.previous_instruction = Some(last);
        }
        self.last_instruction = Some(EmittedInstruction { op, position });
    }

    fn last_instruction_is_pop(&self) -> bool {
        if let Some(last) = &self.last_instruction {
            last.op == Op::Pop
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        let last_emitted = self
            .last_instruction
            .take()
            .unwrap_or_else(|| panic!("last instruction not set"));

        if last_emitted.op != Op::Pop {
            panic!("Last op is not Pop, but {:?}", last_emitted.op);
        }

        self.instructions.drain(last_emitted.position..);

        if let Some(previous) = self.previous_instruction.take() {
            self.last_instruction = Some(previous);
        }
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let pos = self.instructions.len();
        self.instructions.extend(ins);
        pos
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
            context: Context::default(),

            last_instruction: None,
            previous_instruction: None,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::convert::TryFrom;

    #[test]
    fn test_integer_and_booleans() -> Result<(), String> {
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

    #[test]
    fn test_string_expression() -> Result<(), String> {
        run_copmiler_test(
            "\"monkey\"",
            vec![string("monkey".to_owned())],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "\"mon\" + \"key\"",
            vec![string("mon".to_owned()), string("key".to_owned())],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Add, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_conditionals() -> Result<(), String> {
        run_copmiler_test(
            "if (true) { 10 }; 3333",
            vec![int(10), int(3333)],
            vec![
                // 0000
                make(Op::True, &vec![]).unwrap(),
                // 0001
                make(Op::JumpNotTrue, &vec![10]).unwrap(),
                // 0004
                make(Op::Constant, &vec![0]).unwrap(),
                // 0007
                make(Op::Jump, &vec![11]).unwrap(),
                // 0010
                make(Op::Null, &vec![]).unwrap(),
                // 0011
                make(Op::Pop, &vec![]).unwrap(),
                // 0012
                make(Op::Constant, &vec![1]).unwrap(),
                // 00015
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "if (true) { 10 } else { 20 }; 3333",
            vec![int(10), int(20), int(3333)],
            vec![
                // 0000
                make(Op::True, &vec![]).unwrap(),
                // 0001
                make(Op::JumpNotTrue, &vec![10]).unwrap(),
                // 0004
                make(Op::Constant, &vec![0]).unwrap(),
                // 0007
                make(Op::Jump, &vec![13]).unwrap(),
                // 0010
                make(Op::Constant, &vec![1]).unwrap(),
                // 00013
                make(Op::Pop, &vec![]).unwrap(),
                // 00014
                make(Op::Constant, &vec![2]).unwrap(),
                // 00017
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_global_let_statement() -> Result<(), String> {
        run_copmiler_test(
            "let one = 1; let two = 2;",
            vec![int(1), int(2)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::SetGlobal, &vec![1]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "let one = 1; one;",
            vec![int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::GetGlobal, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "let one = 1; let two = one; two;",
            vec![int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::GetGlobal, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![1]).unwrap(),
                make(Op::GetGlobal, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_arrays() -> Result<(), String> {
        run_copmiler_test(
            "[1]",
            vec![int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Array, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "[1, 2]",
            vec![int(1), int(2)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Array, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "[1 + 2, 3 - 4, 5 * 6]",
            vec![int(1), int(2), int(3), int(4), int(5), int(6)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Add, &vec![]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Sub, &vec![]).unwrap(),
                make(Op::Constant, &vec![4]).unwrap(),
                make(Op::Constant, &vec![5]).unwrap(),
                make(Op::Mul, &vec![]).unwrap(),
                make(Op::Array, &vec![3]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_context() {
        let mut ctx = Context::default();
        let a_idx = ctx.define(ident("a"));
        let b_idx = ctx.define(ident("b"));
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));

        // sub
        ctx = ctx.sub();
        let a_idx_sub = ctx.define(ident("a"));
        let c_idx_sub = ctx.define(ident("c"));
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx_sub));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));
        assert_eq!(ctx.resolve(&ident("c")), Some(c_idx_sub));

        // sub sub
        ctx = ctx.sub();
        let a_idx_sub_sub = ctx.define(ident("a"));
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx_sub_sub));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));
        assert_eq!(ctx.resolve(&ident("c")), Some(c_idx_sub));

        // unsub sub
        ctx = ctx.unsub();
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx_sub));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));
        assert_eq!(ctx.resolve(&ident("c")), Some(c_idx_sub));

        // unsub unsub
        ctx = ctx.unsub();
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));
        assert_eq!(ctx.resolve(&ident("c")), None);

        // unsub unsub (NOP)
        ctx = ctx.unsub();
        assert_eq!(ctx.resolve(&ident("a")), Some(a_idx));
        assert_eq!(ctx.resolve(&ident("b")), Some(b_idx));
        assert_eq!(ctx.resolve(&ident("c")), None);
    }

    fn ident(x: &'static str) -> ast::Identifier {
        use crate::token::Token;
        ast::Identifier {
            token: Token {
                tpe: String::new(),
                literal: String::new(),
            },
            value: x.to_owned(),
        }
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
                display_flat_instructions(actual.to_vec()),
            ));
        }

        for (i, instr) in concatted.iter().enumerate() {
            if actual[i] != *instr {
                return Err(format!(
                    "wrong instruction at {}. expected={:?}, actual={:?}",
                    i,
                    display_instructions(expected_instructions),
                    display_flat_instructions(actual.to_vec()),
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

    fn string(string: String) -> object::Object {
        object::Object::String_(string)
    }

    fn display_flat_instructions(instructions: Vec<u8>) -> String {
        let mut result = String::new();
        let mut index = 0;
        while index < instructions.len() {
            let def: Definition = Op::try_from(instructions[index])
                .unwrap_or_else(|_| {
                    panic!("Definition for instruction {} not found", instructions[0])
                })
                .into();

            let mut length = 0;
            for width in def.operand_widths {
                length += width as usize;
            }

            let instr = &instructions[index..=(index + length)];
            display_instruction(instr, index, &mut result);
            index += 1 + length;
        }
        result
    }
}
