use crate::ast;
use std::collections::HashMap;
use std::fmt;

mod instructions;

use instructions::Instruction::Label;
use instructions::{AddressingMode as AM, Instruction::*, *};

pub struct Compiler {
    instructions: Vec<Instruction>,
    pub globals: HashMap<String, i32>,
    scratch_registers: Vec<(Register, bool)>,
    label_idx: u16,
}

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) -> Result<(), String> {
        for stmt in p.0 {
            if let Some(r) = self.compile_statement(stmt) {
                self.emit(Move(AM::Register(r), AM::Register(Register::RAX)));
                self.free_scratch(r);
            } else {
                self.emit(Move(AM::Immediate(0), AM::Register(Register::RAX)));
            }
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> Option<Register> {
        use ast::Statement::*;
        match stmt {
            Expression { value, .. } => Some(self.compile_expression(value)),
            Block { mut statements, .. } if statements.len() == 1 => {
                self.compile_statement(statements.remove(0))
            }
            Let {
                name, expression, ..
            } => {
                let r = self.compile_expression(*expression);
                self.emit(Move(AM::Register(r), AM::Global(name.value.clone())));
                self.free_scratch(r);
                self.declare_data(name.value);
                None
            }
            other => unimplemented!("compile_statement: {:?}", other),
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
            Identifier(ast::Identifier { value, .. }) => {
                let r = self.alloc_scratch();
                self.emit(Move(AM::Global(value), AM::Register(r)));
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
            other => unimplemented!("compile_expression: {:?}", other),
        }
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
                self.emit(Move(AM::Register(r), AM::Register(Register::RAX)));
                self.emit(Mul(l));
                self.emit(Move(AM::Register(Register::RAX), AM::Register(l)));
                self.free_scratch(r);
                l
            }
            "/" => {
                self.emit(Move(AM::Immediate(0), AM::Register(Register::RDX)));
                self.emit(Move(AM::Register(l), AM::Register(Register::RAX)));
                self.emit(Div(r));
                self.emit(Move(AM::Register(Register::RAX), AM::Register(l)));
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
        self.emit(Label(done_label.clone()));
        result
    }

    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr)
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
        let pointer = self.scratch_registers.iter_mut().find(|(reg, _)| *reg == r);

        if let Some((_, used)) = pointer {
            *used = false;
        } else {
            panic!("Register {} not found in scratch_register!", r);
        }
    }

    fn create_label(&mut self) -> instructions::Label {
        let label = instructions::Label(format!("L{}", self.label_idx));
        self.label_idx += 1;
        label
    }
}

impl Default for Compiler {
    fn default() -> Self {
        use Register::*;
        Self {
            instructions: vec![],
            scratch_registers: vec![
                (RBX, false),
                (R10, false),
                (R11, false),
                (R12, false),
                (R13, false),
                (R14, false),
                (R15, false),
            ],
            globals: HashMap::new(),
            label_idx: 0,
        }
    }
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for instr in self.instructions.iter() {
            if matches!(instr, Instruction::Label(_)) {
                writeln!(f, "{}", instr)?;
            } else {
                writeln!(f, "        {}", instr)?;
            }
        }
        Ok(())
    }
}
