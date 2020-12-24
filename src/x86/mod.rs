use crate::ast;
use std::fmt;

mod instructions;

use instructions::*;

pub struct Compiler {
    instructions: Vec<Instruction>,
    scratch_registers: Vec<(Register, bool)>,
}

impl Compiler {
    pub fn compile(&mut self, p: ast::Program) -> Result<(), String> {
        for stmt in p.0 {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }

    fn compile_statement(&mut self, stmt: ast::Statement) -> Result<(), String> {
        use ast::Statement::*;
        match stmt {
            Expression { value, .. } => {
                let r = self.compile_expression(value)?;
                self.emit(Instruction::Move(
                    AddressingMode::Register(r),
                    AddressingMode::Register(Register::RAX),
                ))
            }
            other => unimplemented!("compile_statement: {:?}", other),
        }
        Ok(())
    }

    fn compile_expression(&mut self, exp: ast::Expression) -> Result<Register, String> {
        use ast::Expression::*;
        match exp {
            Infix { op, lhs, rhs, .. } => {
                let r_lhs = self.compile_expression(*lhs)?;
                let r_rhs = self.compile_expression(*rhs)?;
                match op.as_str() {
                    "+" => self.emit(Instruction::Add(r_lhs, r_rhs)),
                    other => unimplemented!("infix {}", other),
                }
                self.scratch_free(r_lhs);
                Ok(r_rhs)
            }
            IntLiteral { value, .. } => {
                let r = self.scratch_alloc();
                self.emit(Instruction::Move(
                    AddressingMode::Immediate(value),
                    AddressingMode::Register(r),
                ));
                Ok(r)
            }
            other => unimplemented!("compile_expression: {:?}", other),
        }
    }

    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }

    fn scratch_alloc(&mut self) -> Register {
        let next_free = self
            .scratch_registers
            .iter_mut()
            .find(|(_, inuse)| *inuse == false);

        if let Some((register, inuse)) = next_free {
            *inuse = true;
            register.clone()
        } else {
            panic!("No more free registers");
        }
    }

    fn scratch_free(&mut self, r: Register) {
        let pointer = self.scratch_registers.iter_mut().find(|(reg, _)| *reg == r);

        if let Some((_, inuse)) = pointer {
            *inuse = false;
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
