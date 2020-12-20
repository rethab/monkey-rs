use super::code::*;
use super::context::*;
use crate::ast;
use crate::object;

struct EmittedInstruction {
    op: Op,
    position: usize,
}

struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

pub struct Compiler {
    scopes: Vec<CompilationScope>,
    constants: Vec<object::Object>,
    context: Context,
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
                ctx = self.compile_expression(value, ctx)?;
                self.emit(Op::Pop, &[])?;
                Ok(ctx)
            }
            ast::Statement::Block { statements, .. } => {
                for stmt in statements.into_iter() {
                    ctx = self.compile_statement(stmt, ctx)?;
                }
                Ok(ctx)
            }
            ast::Statement::Let {
                name, expression, ..
            } => {
                ctx = self.compile_expression(*expression, ctx)?;
                match ctx.define(name) {
                    ScopedValue::Global(idx) => self.emit(Op::SetGlobal, &[idx as i32]),
                    ScopedValue::Local(idx) => self.emit(Op::SetLocal, &[idx as i32]),
                    ScopedValue::Builtin(_) => panic!("Define can never return a builtin"),
                }?;
                Ok(ctx)
            }
            ast::Statement::Return { value, .. } => {
                ctx = self.compile_expression(value, ctx)?;
                self.emit(Op::ReturnValue, &[])?;
                Ok(ctx)
            }
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
                if self.last_instruction_is(Op::Pop) {
                    self.remove_last_pop();
                }

                // jump over alternative
                let alt_jump_idx = self.emit(Op::Jump, &[-1])?;

                let scope = self.current_scope();

                // fix up the jump
                let after_consequence = scope.instructions.len() as i32;
                self.replace_op(cons_jump_idx, Op::JumpNotTrue, &[after_consequence])?;

                ctx = if let Some(alt) = alternative {
                    ctx = self.compile_statement(*alt, ctx)?;
                    if self.last_instruction_is(Op::Pop) {
                        self.remove_last_pop();
                    }
                    ctx
                } else {
                    self.emit(Op::Null, &[])?;
                    ctx
                };

                let scope = self.current_scope();
                let after_alternative = scope.instructions.len() as i32;
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
            ast::Expression::MapLiteral { values, .. } => {
                let size = values.len();
                for (key, value) in values {
                    ctx = self.compile_expression(key, ctx)?;
                    ctx = self.compile_expression(value, ctx)?;
                }
                self.emit(Op::Hash, &[size as i32])?;
                Ok(ctx)
            }
            ast::Expression::Index {
                container, index, ..
            } => {
                ctx = self.compile_expression(*container, ctx)?;
                ctx = self.compile_expression(*index, ctx)?;
                self.emit(Op::Index, &[])?;
                Ok(ctx)
            }
            ast::Expression::Identifier(ident) => {
                let value = ctx
                    .resolve(&ident)
                    .ok_or_else(|| format!("Identifier '{}' not found", ident.value))?;

                match value {
                    ScopedValue::Global(idx) => self.emit(Op::GetGlobal, &[idx as i32]),
                    ScopedValue::Local(idx) => self.emit(Op::GetLocal, &[idx as i32]),
                    ScopedValue::Builtin(idx) => self.emit(Op::GetBuiltin, &[idx as i32]),
                }?;

                Ok(ctx)
            }
            ast::Expression::FunctionLiteral {
                body, parameters, ..
            } => self.compile_function_literal(*body, parameters, ctx),
            ast::Expression::Call {
                function,
                arguments,
                ..
            } => {
                let mut ctx = match function {
                    ast::Function::Identifier(ident) => {
                        let value = ctx
                            .resolve(&ident)
                            .ok_or_else(|| format!("Function '{}' not found", ident.value))?;
                        match value {
                            ScopedValue::Global(idx) => self.emit(Op::GetGlobal, &[idx as i32]),
                            ScopedValue::Local(idx) => self.emit(Op::GetLocal, &[idx as i32]),
                            ScopedValue::Builtin(idx) => self.emit(Op::GetBuiltin, &[idx as i32]),
                        }?;
                        ctx
                    }
                    ast::Function::Literal {
                        body, parameters, ..
                    } => self.compile_function_literal(*body, parameters, ctx)?,
                };
                let argc = arguments.len();
                for arg in arguments {
                    ctx = self.compile_expression(arg, ctx)?;
                }
                self.emit(Op::Call, &[argc as i32])?;
                Ok(ctx)
            }
        }
    }

    fn compile_function_literal(
        &mut self,
        body: ast::Statement,
        parameters: Vec<ast::Identifier>,
        mut ctx: Context,
    ) -> Result<Context, String> {
        self.enter_scope();
        ctx = ctx.local();
        let num_parameters = parameters.len();
        for param in parameters {
            ctx.define(param);
        }
        ctx = self.compile_statement(body, ctx)?;
        let num_locals = ctx.num_definitions();
        // implicit return
        if self.last_instruction_is(Op::Pop) {
            self.remove_last_pop();
            self.emit(Op::ReturnValue, &[])?;
        }
        if !self.last_instruction_is(Op::ReturnValue) {
            self.emit(Op::Return, &[])?;
        }
        let instructions = self.leave_scope().instructions;
        let function = object::Object::CompiledFunction {
            instructions,
            num_locals,
            num_parameters: num_parameters as u8,
        };
        let idx = self.add_constant(function);
        self.emit(Op::Constant, &[idx])?;
        Ok(ctx.unlocal())
    }

    fn emit(&mut self, op: Op, operands: &[i32]) -> CompileResult<usize> {
        let instruction = make(op.clone(), operands)?;
        let pos = self.add_instruction(&instruction);

        self.set_last_instruction(op, pos);
        Ok(pos)
    }

    fn replace_op(&mut self, mut idx: usize, op: Op, operands: &[i32]) -> CompileResult<()> {
        let scope = self.current_scope_mut();
        let instruction = make(op, operands)?;
        for ins in instruction {
            scope.instructions[idx] = ins;
            idx += 1;
        }
        Ok(())
    }

    fn set_last_instruction(&mut self, op: Op, position: usize) {
        let scope = self.current_scope_mut();
        if let Some(last) = scope.last_instruction.take() {
            scope.previous_instruction = Some(last);
        }
        scope.last_instruction = Some(EmittedInstruction { op, position });
    }

    fn last_instruction_is(&self, op: Op) -> bool {
        let scope = self.current_scope();
        if let Some(last) = &scope.last_instruction {
            last.op == op
        } else {
            false
        }
    }

    fn remove_last_pop(&mut self) {
        let scope = self.current_scope_mut();
        let last_emitted = scope
            .last_instruction
            .take()
            .unwrap_or_else(|| panic!("last instruction not set"));

        if last_emitted.op != Op::Pop {
            panic!("Last op is not Pop, but {:?}", last_emitted.op);
        }

        scope.instructions.drain(last_emitted.position..);

        if let Some(previous) = scope.previous_instruction.take() {
            scope.last_instruction = Some(previous);
        }
    }

    fn add_instruction(&mut self, ins: &[u8]) -> usize {
        let scope = self.current_scope_mut();
        let pos = scope.instructions.len();
        scope.instructions.extend(ins);
        pos
    }

    fn add_constant(&mut self, obj: object::Object) -> i32 {
        self.constants.push(obj);
        (self.constants.len() - 1) as i32
    }

    fn current_scope_mut(&mut self) -> &mut CompilationScope {
        let idx = self.scopes.len() - 1;
        &mut self.scopes[idx]
    }

    fn current_scope(&self) -> &CompilationScope {
        &self.scopes[self.scopes.len() - 1]
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::default());
    }

    fn leave_scope(&mut self) -> CompilationScope {
        self.scopes
            .pop()
            .unwrap_or_else(|| panic!("cannot leave scope that was not entered.."))
    }

    pub fn bytecode(&self) -> Bytecode {
        let scope = self.current_scope();
        Bytecode {
            instructions: &scope.instructions,
            constants: &self.constants,
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        let mut ctx = Context::default();
        for (idx, name) in object::builtins() {
            ctx.define_builtin(idx, name.to_owned());
        }

        Self {
            scopes: vec![CompilationScope::default()],
            constants: vec![],
            context: ctx,
        }
    }
}

impl Default for CompilationScope {
    fn default() -> Self {
        Self {
            instructions: vec![],
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
    fn test_hash() -> Result<(), String> {
        run_copmiler_test(
            "{}",
            vec![],
            vec![
                make(Op::Hash, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "{1: 2, 3: 4, 5:6}",
            vec![int(1), int(2), int(3), int(4), int(5), int(6)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Constant, &vec![4]).unwrap(),
                make(Op::Constant, &vec![5]).unwrap(),
                make(Op::Hash, &vec![3]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "{1: 2+3, 4: 5 * 6}",
            vec![int(1), int(2), int(3), int(4), int(5), int(6)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Add, &vec![]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Constant, &vec![4]).unwrap(),
                make(Op::Constant, &vec![5]).unwrap(),
                make(Op::Mul, &vec![]).unwrap(),
                make(Op::Hash, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_index() -> Result<(), String> {
        run_copmiler_test(
            "[1, 2, 3][1+1]",
            vec![int(1), int(2), int(3), int(1), int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Array, &vec![3]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Constant, &vec![4]).unwrap(),
                make(Op::Add, &vec![]).unwrap(),
                make(Op::Index, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "{1: 2}[2-1]",
            vec![int(1), int(2), int(2), int(1)],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Hash, &vec![1]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Sub, &vec![]).unwrap(),
                make(Op::Index, &vec![]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_functions() -> Result<(), String> {
        run_copmiler_test(
            "fn() { return 5 + 10 }",
            vec![
                int(5),
                int(10),
                function(vec![
                    make(Op::Constant, &vec![0]).unwrap(),
                    make(Op::Constant, &vec![1]).unwrap(),
                    make(Op::Add, &vec![]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { 5 + 10 }",
            vec![
                int(5),
                int(10),
                function(vec![
                    make(Op::Constant, &vec![0]).unwrap(),
                    make(Op::Constant, &vec![1]).unwrap(),
                    make(Op::Add, &vec![]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { 1; 2}",
            vec![
                int(1),
                int(2),
                function(vec![
                    make(Op::Constant, &vec![0]).unwrap(),
                    make(Op::Pop, &vec![]).unwrap(),
                    make(Op::Constant, &vec![1]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { }",
            vec![function(vec![make(Op::Return, &vec![]).unwrap()])],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_function_calls() -> Result<(), String> {
        run_copmiler_test(
            "let noArg = fn() { 24 }; noArg()",
            vec![
                int(24),
                function(vec![
                    make(Op::Constant, &vec![0]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::GetGlobal, &vec![0]).unwrap(),
                make(Op::Call, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { 24 }();",
            vec![
                int(24),
                function(vec![
                    make(Op::Constant, &vec![0]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Call, &vec![0]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "let oneArg = fn(a) { a }; oneArg(24)",
            vec![
                function_locals(
                    vec![
                        make(Op::GetLocal, &vec![0]).unwrap(),
                        make(Op::ReturnValue, &vec![]).unwrap(),
                    ],
                    1,
                    1,
                ),
                int(24),
            ],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::GetGlobal, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Call, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "let manyArg = fn(a, b, c) { a; b; c }; manyArg(24, 25, 26)",
            vec![
                function_locals(
                    vec![
                        make(Op::GetLocal, &vec![0]).unwrap(),
                        make(Op::Pop, &vec![]).unwrap(),
                        make(Op::GetLocal, &vec![1]).unwrap(),
                        make(Op::Pop, &vec![]).unwrap(),
                        make(Op::GetLocal, &vec![2]).unwrap(),
                        make(Op::ReturnValue, &vec![]).unwrap(),
                    ],
                    3,
                    3,
                ),
                int(24),
                int(25),
                int(26),
            ],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::GetGlobal, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Constant, &vec![3]).unwrap(),
                make(Op::Call, &vec![3]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_let_statement_scopes() -> Result<(), String> {
        run_copmiler_test(
            "let num = 55; fn() { num }",
            vec![
                int(55),
                function(vec![
                    make(Op::GetGlobal, &vec![0]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ]),
            ],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::SetGlobal, &vec![0]).unwrap(),
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { let num = 55; num }",
            vec![
                int(55),
                function_locals(
                    vec![
                        make(Op::Constant, &vec![0]).unwrap(),
                        make(Op::SetLocal, &vec![0]).unwrap(),
                        make(Op::GetLocal, &vec![0]).unwrap(),
                        make(Op::ReturnValue, &vec![]).unwrap(),
                    ],
                    1,
                    0,
                ),
            ],
            vec![
                make(Op::Constant, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { let a = 55; let b = 77; a + b }",
            vec![
                int(55),
                int(77),
                function_locals(
                    vec![
                        make(Op::Constant, &vec![0]).unwrap(),
                        make(Op::SetLocal, &vec![0]).unwrap(),
                        make(Op::Constant, &vec![1]).unwrap(),
                        make(Op::SetLocal, &vec![1]).unwrap(),
                        make(Op::GetLocal, &vec![0]).unwrap(),
                        make(Op::GetLocal, &vec![1]).unwrap(),
                        make(Op::Add, &vec![]).unwrap(),
                        make(Op::ReturnValue, &vec![]).unwrap(),
                    ],
                    2,
                    0,
                ),
            ],
            vec![
                make(Op::Constant, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )
    }

    #[test]
    fn test_builtins() -> Result<(), String> {
        run_copmiler_test(
            "len([]); append([], 1);",
            vec![int(1)],
            vec![
                make(Op::GetBuiltin, &vec![0]).unwrap(),
                make(Op::Array, &vec![0]).unwrap(),
                make(Op::Call, &vec![1]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
                make(Op::GetBuiltin, &vec![5]).unwrap(),
                make(Op::Array, &vec![0]).unwrap(),
                make(Op::Constant, &vec![0]).unwrap(),
                make(Op::Call, &vec![2]).unwrap(),
                make(Op::Pop, &vec![]).unwrap(),
            ],
        )?;
        run_copmiler_test(
            "fn() { len([]) }",
            vec![function_locals(
                vec![
                    make(Op::GetBuiltin, &vec![0]).unwrap(),
                    make(Op::Array, &vec![0]).unwrap(),
                    make(Op::Call, &vec![1]).unwrap(),
                    make(Op::ReturnValue, &vec![]).unwrap(),
                ],
                0,
                0,
            )],
            vec![
                make(Op::Constant, &vec![0]).unwrap(),
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
                display_flat_instructions(actual.to_vec(), None),
            ));
        }

        for (i, instr) in concatted.iter().enumerate() {
            if actual[i] != *instr {
                return Err(format!(
                    "wrong instruction at {}. expected={:?}, actual={:?}",
                    i,
                    display_instructions(expected_instructions),
                    display_flat_instructions(actual.to_vec(), None),
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

    fn function(instructions: Vec<Instructions>) -> object::Object {
        object::Object::CompiledFunction {
            instructions: flatten(instructions),
            num_locals: 0,
            num_parameters: 0,
        }
    }

    fn function_locals(
        instructions: Vec<Instructions>,
        num_locals: u8,
        num_parameters: u8,
    ) -> object::Object {
        object::Object::CompiledFunction {
            instructions: flatten(instructions),
            num_locals,
            num_parameters,
        }
    }

    fn string(string: String) -> object::Object {
        object::Object::String_(string)
    }
}
