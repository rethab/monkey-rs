use std::convert::TryFrom;
use std::convert::TryInto;

pub type Instructions = Vec<u8>;

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Constant,

    // booleans
    True,
    False,

    // int ops
    Add,
    Sub,
    Mul,
    Div,

    // comparisons
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,

    // globals
    GetGlobal,
    SetGlobal,

    // locals
    GetLocal,
    SetLocal,

    // prefix
    Minus,
    Bang,

    // postfix
    Index,

    // misc
    Pop,
    Null,
    Array,
    Hash,

    // jumps
    JumpNotTrue,
    Jump,

    // functions
    Call,
    ReturnValue,
    Return,
}

impl Op {
    pub fn byte(self) -> u8 {
        self as u8
    }
}

impl TryFrom<u8> for Op {
    type Error = String;

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == Op::Constant as u8 => Ok(Op::Constant),
            x if x == Op::True as u8 => Ok(Op::True),
            x if x == Op::False as u8 => Ok(Op::False),
            x if x == Op::Add as u8 => Ok(Op::Add),
            x if x == Op::Sub as u8 => Ok(Op::Sub),
            x if x == Op::Mul as u8 => Ok(Op::Mul),
            x if x == Op::Div as u8 => Ok(Op::Div),
            x if x == Op::Equal as u8 => Ok(Op::Equal),
            x if x == Op::NotEqual as u8 => Ok(Op::NotEqual),
            x if x == Op::GreaterThan as u8 => Ok(Op::GreaterThan),
            x if x == Op::LessThan as u8 => Ok(Op::LessThan),
            x if x == Op::GetGlobal as u8 => Ok(Op::GetGlobal),
            x if x == Op::SetGlobal as u8 => Ok(Op::SetGlobal),
            x if x == Op::GetLocal as u8 => Ok(Op::GetLocal),
            x if x == Op::SetLocal as u8 => Ok(Op::SetLocal),
            x if x == Op::Minus as u8 => Ok(Op::Minus),
            x if x == Op::Index as u8 => Ok(Op::Index),
            x if x == Op::Bang as u8 => Ok(Op::Bang),
            x if x == Op::Pop as u8 => Ok(Op::Pop),
            x if x == Op::Null as u8 => Ok(Op::Null),
            x if x == Op::Array as u8 => Ok(Op::Array),
            x if x == Op::Hash as u8 => Ok(Op::Hash),
            x if x == Op::JumpNotTrue as u8 => Ok(Op::JumpNotTrue),
            x if x == Op::Jump as u8 => Ok(Op::Jump),
            x if x == Op::Call as u8 => Ok(Op::Call),
            x if x == Op::ReturnValue as u8 => Ok(Op::ReturnValue),
            x if x == Op::Return as u8 => Ok(Op::Return),
            other => Err(format!("Not an op code: {}", other)),
        }
    }
}

#[derive(Debug)]
pub struct Definition<'a> {
    name: &'a str,
    pub operand_widths: Vec<i32>,
}

pub fn make(op: Op, operands: &[i32]) -> Result<Vec<u8>, String> {
    let def: Definition = op.clone().into();

    let mut instruction_len = 1;
    for width in def.operand_widths.iter() {
        instruction_len += width;
    }

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op.byte();

    let mut offset = 1;
    for (i, o) in operands.iter().enumerate() {
        let width = def.operand_widths[i] as usize;
        match width {
            // TODO: should the operands just be u16? do we ever pass actual i32?
            2 => push_bigendian(&mut instruction, offset, *o as u16),
            1 => instruction[offset] = *o as u8,
            // TODO this should probably panic instead
            bad => return Err(format!("Unhandled with {}", bad)),
        }
        offset += width;
    }

    Ok(instruction)
}

impl<'a> Into<Definition<'a>> for Op {
    fn into(self) -> Definition<'a> {
        use Op::*;
        match self {
            Constant => Definition {
                name: "OpConstant",
                operand_widths: vec![2],
            },
            True => Definition {
                name: "OpTrue",
                operand_widths: vec![],
            },
            False => Definition {
                name: "OpFalse",
                operand_widths: vec![],
            },
            Add => Definition {
                name: "OpAdd",
                operand_widths: vec![],
            },
            Mul => Definition {
                name: "OpMul",
                operand_widths: vec![],
            },
            Sub => Definition {
                name: "OpSub",
                operand_widths: vec![],
            },
            Div => Definition {
                name: "OpDiv",
                operand_widths: vec![],
            },
            Equal => Definition {
                name: "OpEqual",
                operand_widths: vec![],
            },
            NotEqual => Definition {
                name: "OpNotEqual",
                operand_widths: vec![],
            },
            GreaterThan => Definition {
                name: "OpGreaterThan",
                operand_widths: vec![],
            },
            LessThan => Definition {
                name: "OpLessThan",
                operand_widths: vec![],
            },
            GetGlobal => Definition {
                name: "OpGetGlobal",
                operand_widths: vec![2],
            },
            SetGlobal => Definition {
                name: "OpSetGlobal",
                operand_widths: vec![2],
            },
            GetLocal => Definition {
                name: "OpGetLocal",
                operand_widths: vec![1],
            },
            SetLocal => Definition {
                name: "OpSetLocal",
                operand_widths: vec![1],
            },
            Minus => Definition {
                name: "OpMinus",
                operand_widths: vec![],
            },
            Index => Definition {
                name: "OpIndex",
                operand_widths: vec![],
            },
            Bang => Definition {
                name: "OpBang",
                operand_widths: vec![],
            },
            Pop => Definition {
                name: "OpPop",
                operand_widths: vec![],
            },
            Null => Definition {
                name: "OpNull",
                operand_widths: vec![],
            },
            Array => Definition {
                name: "OpArray",
                operand_widths: vec![2],
            },
            Hash => Definition {
                name: "OpHash",
                operand_widths: vec![2],
            },
            JumpNotTrue => Definition {
                name: "OpJumpNotTrue",
                operand_widths: vec![2],
            },
            Jump => Definition {
                name: "OpJump",
                operand_widths: vec![2],
            },
            Call => Definition {
                name: "OpCall",
                operand_widths: vec![1],
            },
            ReturnValue => Definition {
                name: "OpReturnValue",
                operand_widths: vec![],
            },
            Return => Definition {
                name: "OpReturn",
                operand_widths: vec![],
            },
        }
    }
}

fn push_bigendian(vs: &mut [u8], offset: usize, v: u16) {
    vs[offset] = ((v & 0xFF00) >> 8) as u8;
    vs[offset + 1] = (v & 0x00FF) as u8;
}

pub fn read_bigendian(vs: &[u8], offset: usize) -> u16 {
    ((vs[offset] as u16) << 8) | vs[offset + 1] as u16
}

pub fn display_instruction(instr: &[u8], offset: usize, buf: &mut String, current: bool) {
    if !buf.is_empty() {
        buf.push('\n');
    }
    buf.push_str(&format!("{:0width$} ", offset, width = 4));

    let def: Definition = Op::try_from(instr[0])
        .unwrap_or_else(|_| panic!("Definition for instruction {} not found", instr[0]))
        .into();

    buf.push_str(def.name);

    let op = instr[0]
        .try_into()
        .unwrap_or_else(|_| panic!("Unknown op: {}", instr[0]));

    match op {
        Op::Constant => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::JumpNotTrue => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::Jump => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::GetGlobal => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::SetGlobal => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::GetLocal => buf.push_str(&format!(" {}", instr[1])),
        Op::SetLocal => buf.push_str(&format!(" {}", instr[1])),
        Op::Array => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::Hash => buf.push_str(&format!(" {}", read_bigendian(&instr, 1))),
        Op::Call => buf.push_str(&format!(" {}", instr[1])),
        Op::ReturnValue => {}
        Op::Return => {}
        Op::True => {}
        Op::False => {}
        Op::Add => {}
        Op::Sub => {}
        Op::Mul => {}
        Op::Div => {}
        Op::Equal => {}
        Op::NotEqual => {}
        Op::GreaterThan => {}
        Op::LessThan => {}
        Op::Minus => {}
        Op::Index => {}
        Op::Bang => {}
        Op::Pop => {}
        Op::Null => {}
    }

    if current {
        buf.push_str(" (CURRENT)");
    }
}

pub fn display_instructions(instructions: Vec<Vec<u8>>) -> String {
    let mut result = String::new();
    let mut offset = 0;
    for instr in instructions {
        display_instruction(&instr, offset, &mut result, false);
        offset += instr.len();
    }
    result
}

pub fn display_flat_instructions(instructions: Vec<u8>, ip: Option<i32>) -> String {
    let mut result = String::new();
    let mut index = 0;
    while index < instructions.len() {
        let def: Definition = Op::try_from(instructions[index])
            .unwrap_or_else(|_| panic!("Definition for instruction {} not found", instructions[0]))
            .into();

        let mut length = 0;
        for width in def.operand_widths {
            length += width as usize;
        }

        let instr = &instructions[index..=(index + length)];
        let current = match ip {
            Some(ip) => ip == index.try_into().unwrap(),
            None => false,
        };
        display_instruction(instr, index, &mut result, current);
        index += 1 + length;
    }
    result
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_make() {
        let tests = vec![
            (
                Op::Constant,
                vec![65534],
                vec![Op::Constant.byte(), 255, 254],
            ),
            (Op::True, vec![], vec![Op::True.byte()]),
            (Op::False, vec![], vec![Op::False.byte()]),
            (Op::Add, vec![], vec![Op::Add.byte()]),
            (Op::Sub, vec![], vec![Op::Sub.byte()]),
            (Op::Mul, vec![], vec![Op::Mul.byte()]),
            (Op::Div, vec![], vec![Op::Div.byte()]),
            (Op::Equal, vec![], vec![Op::Equal.byte()]),
            (Op::NotEqual, vec![], vec![Op::NotEqual.byte()]),
            (Op::GreaterThan, vec![], vec![Op::GreaterThan.byte()]),
            (Op::LessThan, vec![], vec![Op::LessThan.byte()]),
            (
                Op::GetGlobal,
                vec![65534],
                vec![Op::GetGlobal.byte(), 255, 254],
            ),
            (
                Op::SetGlobal,
                vec![65534],
                vec![Op::SetGlobal.byte(), 255, 254],
            ),
            (Op::GetLocal, vec![122], vec![Op::GetLocal.byte(), 122]),
            (Op::SetLocal, vec![3], vec![Op::SetLocal.byte(), 3]),
            (Op::Minus, vec![], vec![Op::Minus.byte()]),
            (Op::Index, vec![], vec![Op::Index.byte()]),
            (Op::Bang, vec![], vec![Op::Bang.byte()]),
            (Op::Pop, vec![], vec![Op::Pop.byte()]),
            (Op::Null, vec![], vec![Op::Null.byte()]),
            (
                Op::JumpNotTrue,
                vec![65534],
                vec![Op::JumpNotTrue.byte(), 255, 254],
            ),
            (Op::Jump, vec![65534], vec![Op::Jump.byte(), 255, 254]),
            (Op::Call, vec![109], vec![Op::Call.byte(), 109]),
            (Op::ReturnValue, vec![], vec![Op::ReturnValue.byte()]),
            (Op::Return, vec![], vec![Op::Return.byte()]),
            (Op::Array, vec![65534], vec![Op::Array.byte(), 255, 254]),
            (Op::Hash, vec![65534], vec![Op::Hash.byte(), 255, 254]),
        ];

        for (op, operands, expected) in tests {
            let instruction = make(op.clone(), &operands).expect(&format!(
                "Failed to make op={:?} and operands={:?}",
                op, operands
            ));

            assert_eq!(instruction.len(), expected.len());

            for (i, b) in expected.into_iter().enumerate() {
                assert_eq!(b, instruction[i], "Unexpected instruction at {}", i);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Op::Constant, &vec![1]).unwrap(),
            make(Op::Add, &vec![]).unwrap(),
            make(Op::Constant, &vec![2]).unwrap(),
            make(Op::Constant, &vec![65535]).unwrap(),
            make(Op::Sub, &vec![]).unwrap(),
            make(Op::Mul, &vec![]).unwrap(),
            make(Op::Div, &vec![]).unwrap(),
            make(Op::Pop, &vec![]).unwrap(),
            make(Op::Null, &vec![]).unwrap(),
            make(Op::True, &vec![]).unwrap(),
            make(Op::False, &vec![]).unwrap(),
            make(Op::Equal, &vec![]).unwrap(),
            make(Op::NotEqual, &vec![]).unwrap(),
            make(Op::GreaterThan, &vec![]).unwrap(),
            make(Op::LessThan, &vec![]).unwrap(),
            make(Op::GetGlobal, &vec![7657]).unwrap(),
            make(Op::SetGlobal, &vec![8791]).unwrap(),
            make(Op::GetLocal, &vec![123]).unwrap(),
            make(Op::SetLocal, &vec![12]).unwrap(),
            make(Op::Minus, &vec![]).unwrap(),
            make(Op::Index, &vec![]).unwrap(),
            make(Op::Bang, &vec![]).unwrap(),
            make(Op::JumpNotTrue, &vec![36435]).unwrap(),
            make(Op::Jump, &vec![678]).unwrap(),
            make(Op::Call, &vec![124]).unwrap(),
            make(Op::ReturnValue, &vec![]).unwrap(),
            make(Op::Return, &vec![]).unwrap(),
            make(Op::Array, &vec![8987]).unwrap(),
            make(Op::Hash, &vec![8988]).unwrap(),
        ];

        let expected = "
            0000 OpConstant 1
            0003 OpAdd
            0004 OpConstant 2
            0007 OpConstant 65535
            0010 OpSub
            0011 OpMul
            0012 OpDiv
            0013 OpPop
            0014 OpNull
            0015 OpTrue
            0016 OpFalse
            0017 OpEqual
            0018 OpNotEqual
            0019 OpGreaterThan
            0020 OpLessThan
            0021 OpGetGlobal 7657
            0024 OpSetGlobal 8791
            0027 OpGetLocal 123
            0029 OpSetLocal 12
            0031 OpMinus
            0032 OpIndex
            0033 OpBang
            0034 OpJumpNotTrue 36435
            0037 OpJump 678
            0040 OpCall 124
            0042 OpReturnValue
            0043 OpReturn
            0044 OpArray 8987
            0047 OpHash 8988
        "
        .trim()
        .replace("            ", "");

        assert_eq!(expected, display_instructions(instructions));
    }

    #[test]
    fn test_read_big_endian() {
        let mut xs = vec![1, 0, 0, 0];
        assert_eq!(push_and_read(&mut xs, 0, 1), 1);
        assert_eq!(push_and_read(&mut xs, 0, 10000), 10000);
        assert_eq!(push_and_read(&mut xs, 1, 1), 1);
        assert_eq!(push_and_read(&mut xs, 1, 10000), 10000);
        assert_eq!(push_and_read(&mut xs, 2, 1), 1);
        assert_eq!(push_and_read(&mut xs, 2, 10000), 10000);
    }

    fn push_and_read(vs: &mut [u8], offset: usize, v: u16) -> u16 {
        push_bigendian(vs, offset, v);
        read_bigendian(vs, offset)
    }
}
