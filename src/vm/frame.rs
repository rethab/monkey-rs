use super::super::code;
use std::fmt;

pub struct Frame<'a> {
    pub instructions: &'a [u8],
    pub ip: i32,
}

impl<'a> Frame<'a> {
    pub fn new(instructions: &'a [u8]) -> Self {
        Self {
            instructions,
            ip: -1,
        }
    }

    pub fn inc_ip(&mut self, n: i32) {
        self.ip += n;
    }

    pub fn set_ip(&mut self, n: i32) {
        self.ip = n;
    }

    pub fn next_instruction(&self) -> Option<u8> {
        if self.more_instructions() {
            Some(self.current_instruction())
        } else {
            None
        }
    }

    pub fn more_instructions(&self) -> bool {
        self.ip < (self.instructions.len() - 1) as i32
    }

    pub fn current_instruction(&self) -> u8 {
        self.instructions[self.ip as usize]
    }

    pub fn read_bigendian(&self) -> u16 {
        let instructions = self.instructions;
        code::read_bigendian(instructions, (self.ip as usize) + 1)
    }
}

impl<'a> fmt::Display for Frame<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        writeln!(f, "ip={}", self.ip)?;
        writeln!(
            f,
            "instructions=\n{}",
            code::display_flat_instructions(self.instructions.to_vec())
        )
    }
}
