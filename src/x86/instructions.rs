use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    Move(AddressingMode, AddressingMode),
    Lea(AddressingMode, Register),
    Add(AddressingMode, Register),
    Sub(AddressingMode, Register),
    Mul(Register),
    Div(Register),
    Xor(AddressingMode, AddressingMode),
    Compare(AddressingMode, AddressingMode),
    Jump(Label),
    JumpEqual(Label),
    JumpNotEqual(Label),
    JumpLess(Label),
    JumpGreater(Label),
    Push(Register),
    Pop(Register),
    Call(AddressingMode),
    Return,
    Label(Label),
}

pub struct Function(pub Vec<Instruction>);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Label(pub String);

#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum AddressingMode {
    Global(String),
    Immediate(i32),
    Register(Register),
    Indirect(Register),
    BaseRelative { register: Register, offset: i32 },
    RipRelative(Label),
}

pub const TRUE: i32 = -1;
pub const FALSE: i32 = 0;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[allow(dead_code)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
    RBP,
    RSP,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

// system v abi: the first four arguments go into these registers.
// for subsequent args, the stack is used
const ARG_REGISTERS: &[Register] = &[
    Register::RDI,
    Register::RSI,
    Register::RDX,
    Register::RCX,
    Register::R8,
    Register::R9,
];

pub const CALLEE_SAVED_REGISTERS: &[Register] = &[
    Register::RBX,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

pub const SCRATCH_REGISTERS: &[Register] = &[
    Register::RBX,
    Register::R10,
    Register::R11,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

pub fn arg_register(idx: usize) -> Option<Register> {
    if idx < ARG_REGISTERS.len() {
        Some(ARG_REGISTERS[idx])
    } else {
        None
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Instruction::*;
        match self {
            Move(src, trg) => write!(f, "MOVQ {}, {}", src, trg),
            Lea(src, trg) => write!(f, "LEA {}, {}", src, trg),
            Add(src, trg) => write!(f, "ADDQ {}, {}", src, trg),
            Sub(src, trg) => write!(f, "SUBQ {}, {}", src, trg),
            Mul(src) => write!(f, "IMULQ {}", src),
            Div(src) => write!(f, "IDIVQ {}", src),
            Xor(val, trg) => write!(f, "XOR {}, {}", val, trg),
            Compare(op1, op2) => write!(f, "CMPQ {}, {}", op1, op2),
            Jump(lbl) => write!(f, "JMP {}", lbl.0),
            JumpEqual(lbl) => write!(f, "JE {}", lbl.0),
            JumpNotEqual(lbl) => write!(f, "JNE {}", lbl.0),
            JumpLess(lbl) => write!(f, "JL {}", lbl.0),
            JumpGreater(lbl) => write!(f, "JG {}", lbl.0),
            Call(trg) => write!(f, "CALL {}", trg),
            Push(r) => write!(f, "PUSHQ {}", r),
            Pop(r) => write!(f, "POPQ {}", r),
            Return => write!(f, "RET"),
            Label(lbl) => write!(f, "{}:", lbl),
        }
    }
}

impl fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use AddressingMode::*;
        match self {
            Global(g) => write!(f, "{}", g),
            Immediate(x) => write!(f, "${}", x),
            Register(r) => write!(f, "{}", r),
            Indirect(r) => write!(f, "({})", r),
            RipRelative(lbl) => write!(f, "{}(%rip)", lbl),
            BaseRelative { register, offset } => {
                if *offset != 0 {
                    write!(f, "{}", offset)?;
                }
                write!(f, "({})", register)
            }
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Register::*;
        match self {
            RAX => write!(f, "%rax"),
            RBX => write!(f, "%rbx"),
            RCX => write!(f, "%rcx"),
            RDX => write!(f, "%rdx"),
            RSI => write!(f, "%rsi"),
            RDI => write!(f, "%rdi"),
            RBP => write!(f, "%rbp"),
            RSP => write!(f, "%rsp"),
            R8 => write!(f, "%r8"),
            R9 => write!(f, "%r9"),
            R10 => write!(f, "%r10"),
            R11 => write!(f, "%r11"),
            R12 => write!(f, "%r12"),
            R13 => write!(f, "%r13"),
            R14 => write!(f, "%r14"),
            R15 => write!(f, "%r15"),
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for instr in self.0.iter() {
            if matches!(instr, Instruction::Label(_)) {
                writeln!(f, "{}", instr)?;
            } else {
                writeln!(f, "        {}", instr)?;
            }
        }
        Ok(())
    }
}
