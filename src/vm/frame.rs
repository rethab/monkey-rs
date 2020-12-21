use super::code;
use super::object::Closure;
use std::fmt;

pub struct Frame {
    pub cl: Closure,
    pub ip: i32,
    pub base_pointer: usize, // value of ip before calling function
}

impl Frame {
    pub fn new(cl: Closure, base_pointer: usize) -> Self {
        Self {
            cl,
            ip: -1,
            base_pointer,
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
        self.ip < (self.cl.func.instructions.len() - 1) as i32
    }

    pub fn current_instruction(&self) -> u8 {
        self.cl.func.instructions[self.ip as usize]
    }

    pub fn read_bigendian(&self) -> u16 {
        code::read_bigendian(&self.cl.func.instructions, (self.ip as usize) + 1)
    }

    pub fn read_u8(&self) -> u8 {
        self.cl.func.instructions[self.ip as usize + 1]
    }
}

impl<'a> fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        writeln!(f, "ip={}, base_pointer={}", self.ip, self.base_pointer)?;
        writeln!(
            f,
            "instructions=\n{}",
            code::display_flat_instructions(self.cl.func.instructions.to_vec(), Some(self.ip))
        )
    }
}
