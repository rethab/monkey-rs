use std::fmt;

pub enum Instruction {
    Move(AddressingMode, AddressingMode),
    Add(Register, Register),
    Sub(Register, Register),
    Mul(Register),
    Div(Register),
    Cmp(AddressingMode, AddressingMode),
    Jmp(Label),
    Je(Label),
    Jne(Label),
    Jl(Label),
    Jg(Label),
    Label(Label),
}

#[derive(Clone)]
pub struct Label(pub String);

#[allow(dead_code)]
pub enum AddressingMode {
    Global(String),
    Immediate(u32),
    Register(Register),
    Indirect(Register),
    BaseRelative { register: Register, offset: u8 },
}

#[derive(Copy, Clone, PartialEq, Eq)]
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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Instruction::*;
        match self {
            Move(src, trg) => write!(f, "MOVQ {}, {}", src, trg),
            Add(src, trg) => write!(f, "ADDQ {}, {}", src, trg),
            Sub(src, trg) => write!(f, "SUBQ {}, {}", src, trg),
            Mul(src) => write!(f, "IMULQ {}", src),
            Div(src) => write!(f, "IDIVQ {}", src),
            Cmp(op1, op2) => write!(f, "CMPQ {}, {}", op1, op2),
            Jmp(lbl) => write!(f, "JMP {}", lbl.0),
            Je(lbl) => write!(f, "JE {}", lbl.0),
            Jne(lbl) => write!(f, "JNE {}", lbl.0),
            Jl(lbl) => write!(f, "JL {}", lbl.0),
            Jg(lbl) => write!(f, "JG {}", lbl.0),
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
            BaseRelative { register, offset } => {
                if *offset != 0 {
                    write!(f, "-{}", offset)?;
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
