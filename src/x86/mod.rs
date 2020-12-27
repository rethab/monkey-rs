use crate::ast;
use std::collections::HashMap;

mod context;
mod instructions;

use context::{Context, Ref};
use instructions::Instruction::Label;
use instructions::{AddressingMode as AM, Instruction::*, Register::*, *};

pub struct Compiler {
    main_function: Vec<Instruction>,
    pub globals: HashMap<String, i32>,
    scratch_registers: Vec<(Register, bool)>,
    label_idx: u16,
    ctx: Context,
    functions: HashMap<instructions::Label, Vec<Instruction>>,
    // last one is the one we're currently code-generating for.
    // empty means we are in the main function
    generating_functions: Vec<instructions::Label>,
}

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) {
        if let Some(r) = self.compile_statements(p.0) {
            self.emit(Move(AM::Register(r), AM::Register(RAX)));
            self.free_scratch(r);
        } else {
            self.emit(Move(AM::Immediate(0), AM::Register(RAX)));
        }
    }

    fn compile_statements(&mut self, statements: Vec<ast::Statement>) -> Option<Register> {
        let mut last_reg = None;

        for stmt in statements {
            let maybe_r = self.compile_statement(stmt);

            if let Some(prev_last) = maybe_r.and_then(|r| last_reg.replace(r)) {
                self.free_scratch(prev_last);
            }
        }
        last_reg
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> Option<Register> {
        use ast::Statement::*;
        match stmt {
            Expression { value, .. } => Some(self.compile_expression(value)),
            Block { statements, .. } => self.compile_statements(statements),
            Let {
                name, expression, ..
            } => {
                if let ast::Expression::FunctionLiteral {
                    parameters, body, ..
                } = *expression
                {
                    let label = self.compile_function_literal(parameters, *body);
                    self.ctx.define(name, label);
                } else {
                    let r = self.compile_expression(*expression);
                    self.emit(Move(AM::Register(r), AM::Global(name.value.clone())));
                    self.free_scratch(r);
                    self.declare_data(name.value);
                }
                None
            }
            Return { value, .. } => {
                let r = self.compile_expression(value);
                self.emit(Move(AM::Register(r), AM::Register(RAX)));
                self.emit_function_epilogue();
                self.free_scratch(r);
                self.emit(Ret);
                None
            }
        }
    }

    fn compile_expression(&mut self, exp: ast::Expression) -> Register {
        use ast::Expression::*;
        match exp {
            Infix { op, lhs, rhs, .. } => {
                let l = self.compile_expression(*lhs);
                let r = self.compile_expression(*rhs);
                self.compile_infix(&op, l, r)
            }
            Prefix { op, rhs, .. } => {
                let r = self.compile_expression(*rhs);
                self.compile_prefix(&op, r)
            }
            IntLiteral { value, .. } => {
                let r = self.alloc_scratch();
                self.emit(Move(AM::Immediate(value as i32), AM::Register(r)));
                r
            }
            BooleanLiteral { value, .. } => {
                let r = self.alloc_scratch();
                let x = if value { TRUE } else { FALSE };
                self.emit(Move(AM::Immediate(x), AM::Register(r)));
                r
            }
            Identifier(ident) => {
                let r = self.alloc_scratch();
                match self.ctx.resolve(ident) {
                    Ref::Label(value) => self.emit(Move(AM::Global(value), AM::Register(r))),
                    Ref::Stack(offset) => {
                        self.emit(Move(AM::BaseRelative(RBP, value), AM::Register(r)))
                    }
                }
                r
            }
            If {
                condition,
                consequence,
                alternative,
                ..
            } => self
                .compile_condition(*condition, *consequence, alternative)
                .unwrap_or_else(|| {
                    let r = self.alloc_scratch();
                    self.emit(Move(AM::Immediate(0), AM::Register(r)));
                    r
                }),
            Call {
                function,
                arguments,
                ..
            } => self.compile_call(arguments, function),
            other => unimplemented!("compile_expression: {:?}", other),
        }
    }

    fn compile_call(
        &mut self,
        arguments: Vec<ast::Expression>,
        function: ast::Function,
    ) -> Register {
        let label = match function {
            ast::Function::Identifier(ident) => {
                if let Ref::Label(lbl) = self.ctx.resolve(&ident) {
                    lbl
                } else {
                    panic!("Functions must resolve to label");
                }
            }
            ast::Function::Literal {
                parameters, body, ..
            } => self.compile_function_literal(parameters, *body),
        };

        for (idx, arg) in arguments.into_iter().enumerate() {
            let r = self.compile_expression(arg);
            let arg_r = *self
                .arg_registers()
                .get(idx)
                .unwrap_or_else(|| panic!("stack arguments not supported yet"));
            self.emit(Move(AM::Register(r), AM::Register(arg_r)));
            self.free_scratch(r);
        }

        self.emit(Call(label));
        let r = self.alloc_scratch();
        self.emit(Move(AM::Register(RAX), AM::Register(r)));
        r
    }

    fn compile_function_literal(
        &mut self,
        parameters: Vec<ast::Identifier>,
        body: ast::Statement,
    ) -> instructions::Label {
        let label = self.create_label();
        self.function_start(label.clone());
        self.ctx.enter_function();

        // prologue
        self.emit(Push(RBP));
        self.emit(Move(AM::Register(RSP), AM::Register(RBP)));

        // setup arguments
        for (idx, p) in parameters.into_iter().enumerate() {
            if let Some(r) = self.arg_registers().get(idx as usize) {
                self.emit(Push(*r));
                self.ctx.define_stack(p, idx);
            } else {
                panic!("stack arguments not handled yet");
            }
        }

        // compile body
        let maybe_reg = self.compile_statement(body);

        // setup epilogue if there was no return statement
        if !matches!(self.last_emitted(), Some(Ret)) {
            self.emit_function_epilogue();
            if let Some(r) = maybe_reg {
                self.emit(Move(AM::Register(r), AM::Register(RAX)));
                self.free_scratch(r);
            } else {
                self.emit(Move(AM::Immediate(0), AM::Register(RAX)));
            }
            self.emit(Ret);
        }

        self.ctx.leave_function();
        self.function_end();
        label
    }

    fn compile_infix(&mut self, op: &str, l: Register, r: Register) -> Register {
        match op {
            "+" => {
                self.emit(Add(l, r));
                self.free_scratch(l);
                r
            }
            "-" => {
                self.emit(Sub(r, l));
                self.free_scratch(r);
                l
            }
            "*" => {
                self.emit(Move(AM::Register(r), AM::Register(RAX)));
                self.emit(Mul(l));
                self.emit(Move(AM::Register(RAX), AM::Register(l)));
                self.free_scratch(r);
                l
            }
            "/" => {
                self.emit(Move(AM::Immediate(0), AM::Register(RDX)));
                self.emit(Move(AM::Register(l), AM::Register(RAX)));
                self.emit(Div(r));
                self.emit(Move(AM::Register(RAX), AM::Register(l)));
                self.free_scratch(r);
                l
            }
            "<" | ">" | "==" | "!=" => {
                self.emit(Cmp(AM::Register(r), AM::Register(l)));
                self.free_scratch(l);

                let true_label = self.create_label();
                let after_label = self.create_label();

                match op {
                    "<" => self.emit(Jl(true_label.clone())),
                    ">" => self.emit(Jg(true_label.clone())),
                    "==" => self.emit(Je(true_label.clone())),
                    "!=" => self.emit(Jne(true_label.clone())),
                    _ => unreachable!(op),
                };
                self.emit(Move(AM::Immediate(FALSE), AM::Register(r)));
                self.emit(Jmp(after_label.clone()));
                self.emit(Label(true_label));
                self.emit(Move(AM::Immediate(TRUE), AM::Register(r)));
                self.emit(Label(after_label));
                r
            }
            other => unimplemented!("infix {}", other),
        }
    }

    fn compile_prefix(&mut self, op: &str, r: Register) -> Register {
        match op {
            "!" => {
                self.emit(Xor(AM::Immediate(-1), AM::Register(r)));
                r
            }
            other => unimplemented!("prefix {}", other),
        }
    }

    fn compile_condition(
        &mut self,
        condition: ast::Expression,
        consequence: ast::Statement,
        alternative: Option<Box<ast::Statement>>,
    ) -> Option<Register> {
        let done_label = self.create_label();
        let er = self.compile_expression(condition);
        self.emit(Cmp(AM::Immediate(FALSE), AM::Register(er)));
        self.free_scratch(er);

        // could probably be optimized away
        let mut result = None;

        let alt = if let Some(alt) = alternative {
            let alt_label = self.create_label();
            self.emit(Je(alt_label.clone()));
            Some((alt_label, alt))
        } else {
            self.emit(Je(done_label.clone()));
            None
        };

        let maybe_cr = self.compile_statement(consequence);
        if let Some(cr) = maybe_cr {
            let r = self.alloc_scratch();
            self.emit(Move(AM::Register(cr), AM::Register(r)));
            self.free_scratch(cr);
            result = Some(r);
        }

        if let Some((alt_label, alt)) = alt {
            self.emit(Jmp(done_label.clone()));
            self.emit(Label(alt_label));
            let maybe_ar = self.compile_statement(*alt);
            if let Some(ar) = maybe_ar {
                let r = result.unwrap_or_else(|| {
                    let tmp = self.alloc_scratch();
                    result = Some(tmp);
                    tmp
                });
                self.emit(Move(AM::Register(ar), AM::Register(r)));
                self.free_scratch(ar);
            }
        }
        self.emit(Label(done_label));
        result
    }

    fn last_emitted(&mut self) -> Option<&Instruction> {
        self.emitting_container().last()
    }

    fn emit_function_epilogue(&mut self) {
        self.emit(Move(AM::Register(RBP), AM::Register(RSP)));
        self.emit(Pop(RBP));
    }

    fn emit(&mut self, instr: Instruction) {
        self.emitting_container().push(instr);
    }

    fn emitting_container(&mut self) -> &mut Vec<Instruction> {
        match self.generating_functions.last() {
            Some(current) => self
                .functions
                .get_mut(current)
                .unwrap_or_else(|| panic!("Currently genrating function '{}' not found", current)),
            None => &mut self.main_function,
        }
    }

    fn function_start(&mut self, lbl: instructions::Label) {
        self.generating_functions.push(lbl.clone());
        self.functions.insert(lbl, vec![]);
    }

    fn function_end(&mut self) {
        self.generating_functions.pop().expect("No function to pop");
    }

    fn declare_data(&mut self, label: String) {
        self.globals.insert(label, 0);
    }

    fn alloc_scratch(&mut self) -> Register {
        let next_free = self.scratch_registers.iter_mut().find(|(_, used)| !(*used));

        if let Some((register, used)) = next_free {
            *used = true;
            *register
        } else {
            panic!("No more free registers");
        }
    }

    fn free_scratch(&mut self, r: Register) {
        if r == RAX {
            return;
        }

        let pointer = self.scratch_registers.iter_mut().find(|(reg, _)| *reg == r);

        if let Some((_, used)) = pointer {
            *used = false;
        } else {
            panic!("Register {} not found in scratch_register!", r);
        }
    }

    fn arg_registers(&self) -> Vec<Register> {
        // todo could be constant?
        vec![RDI, RSI, RDX, RCX]
    }

    fn create_label(&mut self) -> instructions::Label {
        let label = instructions::Label(format!("L{}", self.label_idx));
        self.label_idx += 1;
        label
    }

    pub fn main_function(&self) -> Function {
        Function(self.main_function.clone())
    }

    pub fn functions(&self) -> HashMap<instructions::Label, Function> {
        self.functions
            .iter()
            .map(|(k, v)| (k.clone(), Function(v.to_vec())))
            .collect()
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            main_function: vec![],
            globals: HashMap::new(),
            scratch_registers: vec![
                (RBX, false),
                (R10, false),
                (R11, false),
                (R12, false),
                (R13, false),
                (R14, false),
                (R15, false),
            ],
            label_idx: 0,
            ctx: Context::default(),
            functions: HashMap::new(),
            generating_functions: vec![],
        }
    }
}
