use crate::ast;
use std::collections::HashMap;

mod builtins;
mod context;
mod instructions;

use builtins::{builtin_strconcat, is_builtin};
use context::{Context, Ref, Scope};
use instructions::Instruction::Label;
use instructions::{AddressingMode as AM, Instruction::*, Register::*, *};

pub struct Compiler {
    main_function: Vec<Instruction>,
    pub globals: HashMap<instructions::Label, GlobalValue>,
    scratch_registers: Vec<(Register, bool)>,
    label_idx: u16,
    ctx: Context,
    functions: HashMap<instructions::Label, Vec<Instruction>>,
    // last one is the one we're currently code-generating for.
    // empty means we are in the main function
    generating_functions: Vec<instructions::Label>,
}

pub enum GlobalValue {
    GlobalString(String),
    GlobalInt(i32),
}

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) {
        if let Some(r) = self.compile_statements(p.0) {
            self.emit(Move(AM::Register(r), AM::Register(RAX)));
            self.free_scratch(r);
        } else {
            self.emit(Move(AM::Immediate(0), AM::Register(RAX)));
        }

        // for testing, just print RAX
        self.emit(Move(AM::Register(RAX), AM::Register(RSI)));
        self.emit(Move(AM::Immediate(0), AM::Register(RAX)));
        let fmt = instructions::Label(".LC0".to_owned());
        self.emit(Lea(AM::RipRelative(fmt), RDI));
        let target = AM::Global("printf".to_owned());
        self.emit_function_call(vec![], target);
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
                    let label = self.compile_function_literal(parameters, *body, None);
                    self.ctx.define_function(name, label);
                } else {
                    let r = self.compile_expression(*expression);
                    match self.ctx.define(name.clone()) {
                        Ref::Global(label) => {
                            self.emit(Move(AM::Register(r), AM::Global(name.value)));
                            self.declare_data_int(label);
                        }
                        Ref::Local(idx) => {
                            let target = self.local_arg(idx);
                            self.emit(Move(AM::Register(r), target));
                        }
                        Ref::Function(_) | Ref::Stack(_) => {
                            unreachable!()
                        }
                    }
                    self.free_scratch(r);
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
            StringLiteral { value, .. } => {
                let label = self.create_label();
                self.declare_data_string(label.clone(), &value);
                let r = self.alloc_scratch();
                self.emit(Lea(AM::RipRelative(label), r));
                r
            }
            Identifier(ident) => {
                let r = self.alloc_scratch();
                match self.ctx.resolve(&ident) {
                    Ref::Global(label) => self.emit(Move(AM::Global(label.0), AM::Register(r))),
                    Ref::Function(label) => self.emit(Lea(AM::RipRelative(label), r)),
                    Ref::Local(idx) => {
                        // space on the stack for additional arguments
                        let source = self.local_arg(idx);
                        self.emit(Move(source, AM::Register(r)));
                    }
                    Ref::Stack(offset) => self.emit(Move(
                        AM::BaseRelative {
                            register: RBP,
                            offset,
                        },
                        AM::Register(r),
                    )),
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
            } => {
                if let Some(name) = self.get_indirect_builtin(&function) {
                    self.compile_indirect_call(arguments, &name)
                } else {
                    let target = self.resolve_function_address(function);
                    self.compile_call(arguments, target)
                }
            }
            other => unimplemented!("compile_expression: {:?}", other),
        }
    }

    fn compile_call(
        &mut self,
        arguments: Vec<ast::Expression>,
        target: AddressingMode,
    ) -> Register {
        self.emit_function_call(arguments, target.clone());

        if let AM::Register(r) = target {
            self.free_scratch(r);
        };

        let r = self.alloc_scratch();
        self.emit(Move(AM::Register(RAX), AM::Register(r)));
        r
    }

    fn resolve_function_address(&mut self, function: ast::Function) -> AddressingMode {
        match function {
            ast::Function::Identifier(ident) => {
                if is_builtin(&ident) {
                    return AM::Global(ident.value);
                }
                match self.ctx.resolve(&ident) {
                    Ref::Global(lbl) => {
                        let r = self.alloc_scratch();
                        self.emit(Move(AM::Global(lbl.0), AM::Register(r)));
                        AM::Register(r)
                    }
                    Ref::Function(lbl) => AM::Global(lbl.0),
                    Ref::Local(_) | Ref::Stack(_) => unreachable!(),
                }
            }
            ast::Function::Literal {
                parameters, body, ..
            } => {
                let lbl = self.compile_function_literal(parameters, *body, None);
                AM::Global(lbl.0)
            }
        }
    }

    fn get_indirect_builtin(&self, function: &ast::Function) -> Option<String> {
        if let ast::Function::Identifier(ast::Identifier { value, .. }) = function {
            if value == "strconcat" {
                return Some(value.to_owned());
            }
        }
        None
    }

    fn compile_indirect_call(&mut self, arguments: Vec<ast::Expression>, name: &str) -> Register {
        let label = match name {
            "strconcat" => {
                let label = instructions::Label(name.to_owned());
                if !self.functions.contains_key(&label) {
                    if let ast::Function::Literal {
                        parameters, body, ..
                    } = builtin_strconcat()
                    {
                        self.compile_function_literal(parameters, *body, Some(label))
                    } else {
                        panic!("builtin must create function literal");
                    }
                } else {
                    label
                }
            }
            other => unreachable!("indirect call to {}", other),
        };
        self.compile_call(arguments, AM::Global(label.0))
    }

    fn compile_function_literal(
        &mut self,
        parameters: Vec<ast::Identifier>,
        body: ast::Statement,
        predefined_label: Option<instructions::Label>,
    ) -> instructions::Label {
        let label = predefined_label.unwrap_or_else(|| self.create_label());
        self.function_start(label.clone());

        self.ctx.enter_function(parameters.len());

        // prologue
        self.emit(Push(RBP));
        self.emit(Move(AM::Register(RSP), AM::Register(RBP)));

        // setup arguments
        for (idx, p) in parameters.into_iter().enumerate() {
            if let Some(r) = arg_register(idx as usize) {
                // register-passed arguments
                let offset = (idx + 1) * 8;
                self.emit(Push(r));
                self.ctx.define_stack(p, -(offset as i32));
            } else {
                // stack-passed arguments are above the base pointer
                let offset = ((idx as i32) - 4) * 8;
                self.ctx.define_stack(p, offset);
            }
        }

        let before_body_idx = self.current_instruction_idx();

        // save callee-saved registers
        for r in CALLEE_SAVED_REGISTERS {
            self.emit(Push(*r));
        }

        // compile body
        let maybe_reg = self.compile_statement(body);

        // reserve space for local variables
        let definitions = self.ctx.local_definitions();
        if definitions > 0 {
            let offset = (definitions * 8) as i32;
            self.emit_at_index(before_body_idx, Sub(AM::Immediate(offset), RSP));
        }

        // setup epilogue if there was no return statement
        if !matches!(self.last_emitted(), Some(Ret)) {
            if let Some(r) = maybe_reg {
                self.emit(Move(AM::Register(r), AM::Register(RAX)));
                self.free_scratch(r);
            } else {
                self.emit(Move(AM::Immediate(0), AM::Register(RAX)));
            }
            self.emit_function_epilogue();
            self.emit(Ret);
        }

        self.ensure_released_scratch_registers();

        self.ctx.leave_function();
        self.function_end();
        label
    }

    fn compile_infix(&mut self, op: &str, l: Register, r: Register) -> Register {
        match op {
            "+" => {
                self.emit(Add(AM::Register(l), r));
                self.free_scratch(l);
                r
            }
            "-" => {
                self.emit(Sub(AM::Register(r), l));
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

    fn local_arg(&self, idx: u8) -> AddressingMode {
        let offset = (self.ctx.n_params() + idx as usize + 1) * 8;
        AddressingMode::BaseRelative {
            register: RBP,
            offset: -(offset as i32),
        }
    }

    fn last_emitted(&mut self) -> Option<&Instruction> {
        self.emitting_container().last()
    }

    fn current_instruction_idx(&mut self) -> usize {
        self.emitting_container().len()
    }

    fn emit_function_call(&mut self, mut arguments: Vec<ast::Expression>, target: AddressingMode) {
        // first 6 go into registers, rest on stack
        let (reg_args, mut stack_args) = if arguments.len() > 6 {
            let stack = arguments.split_off(6);
            (arguments, stack)
        } else {
            (arguments, vec![])
        };

        // we cannot directly move into the argument registers,
        // because a call like `f(a, g(b))` would move a into %rdi and
        // then b also into %rdi in order to call g
        let mut regs = vec![];
        for arg in reg_args {
            let r = self.compile_expression(arg);
            regs.push(r);
        }

        for (idx, r) in regs.into_iter().enumerate() {
            match arg_register(idx) {
                Some(arg_r) => self.emit(Move(AM::Register(r), AM::Register(arg_r))),
                None => unreachable!("Stack args are split off above"),
            };
            self.free_scratch(r);
        }

        // save caller-saved registers
        self.emit(Push(R10));
        self.emit(Push(R11));

        // n stack args will lead to another n pushes
        let align_stack = self.must_align_stack(stack_args.len());

        if align_stack {
            self.emit(Add(AM::Immediate(8), RSP));
        }

        stack_args.reverse();
        for arg in stack_args {
            let r = self.compile_expression(arg);
            self.emit(Push(r));
            self.free_scratch(r);
        }

        self.emit(Call(target));

        if align_stack {
            self.emit(Sub(AM::Immediate(8), RSP));
        }

        self.emit(Pop(R11));
        self.emit(Pop(R10));
    }

    // stack must be 16-byte (2x 64bit) aligned before calling function
    fn must_align_stack(&mut self, n_stack_args: usize) -> bool {
        let mut offset = 0;
        if self.ctx.scope() == Scope::Global {
            // for testing: the main function has 7 pushes to preseve registers,
            // therefore whatever pushes we do additionally (when calling functions)
            // needs to be added to an odd number in order to detect whether we
            // need to align
            offset = 1;
        }

        let prev_pushes = self
            .emitting_container()
            .iter()
            .filter(|i| matches!(i, Push(_)))
            .count();

        let total_pushes = offset + n_stack_args + prev_pushes;
        total_pushes % 2 != 0
    }

    fn emit_function_epilogue(&mut self) {
        let mut callee_saved = CALLEE_SAVED_REGISTERS.to_vec();
        callee_saved.reverse();
        for r in callee_saved {
            self.emit(Pop(r));
        }
        self.emit(Move(AM::Register(RBP), AM::Register(RSP)));
        self.emit(Pop(RBP));
    }

    fn emit(&mut self, instr: Instruction) {
        self.emitting_container().push(instr);
    }

    fn emit_at_index(&mut self, idx: usize, instr: Instruction) {
        self.emitting_container().insert(idx, instr);
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

    fn declare_data_int(&mut self, label: instructions::Label) {
        self.globals.insert(label, GlobalValue::GlobalInt(0));
    }

    fn declare_data_string(&mut self, label: instructions::Label, v: &str) {
        self.globals
            .insert(label, GlobalValue::GlobalString(v.to_owned()));
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

    fn ensure_released_scratch_registers(&self) {
        for (r, used) in self.scratch_registers.iter() {
            if *used {
                panic!("Register {} is not released", r);
            }
        }
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
            scratch_registers: SCRATCH_REGISTERS.iter().map(|r| (*r, false)).collect(),
            label_idx: 0,
            ctx: Context::default(),
            functions: HashMap::new(),
            generating_functions: vec![],
        }
    }
}
