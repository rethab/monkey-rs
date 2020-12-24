use crate::ast;
use std::collections::HashMap;
use std::fmt;

mod instructions;

use instructions::{AddressingMode as AM, Instruction::*, *};

pub struct Compiler {
    instructions: Vec<Instruction>,
    pub labels: HashMap<String, i32>,
    scratch_registers: Vec<(Register, bool)>,
}

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) -> Result<(), String> {
        for stmt in p.0 {
            self.compile_statement(stmt);
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: ast::Statement) {
        use ast::Statement::*;
        match stmt {
            Expression { value, .. } => {
                let r = self.compile_expression(value);
                self.emit(Move(AM::Register(r), AM::Register(Register::RAX)));
                self.free_scratch(r);
            }
            Let {
                name, expression, ..
            } => {
                let r = self.compile_expression(*expression);
                self.emit(Move(AM::Register(r), AM::Global(name.value.clone())));
                self.declare_data(name.value);
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
            IntLiteral { value, .. } => {
                let r = self.alloc_scratch();
                self.emit(Move(AM::Immediate(value), AM::Register(r)));
                r
            }
            Identifier(ast::Identifier { value, .. }) => {
                let r = self.alloc_scratch();
                self.emit(Move(AM::Global(value), AM::Register(r)));
                r
            }
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
            other => unimplemented!("infix {}", other),
        }
    }

    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }

    fn declare_data(&mut self, label: String) {
        self.labels.insert(label, 0);
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
            labels: HashMap::new(),
        }
    }
}

impl fmt::Display for Compiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for instr in self.instructions.iter() {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}
