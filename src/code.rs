use std::convert::TryFrom;
use std::convert::TryInto;

pub type Instructions = Vec<u8>;

#[derive(Clone, Debug)]
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

    // prefix
    Minus,
    Bang,

    // misc
    Pop,
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
            x if x == Op::Minus as u8 => Ok(Op::Minus),
            x if x == Op::Bang as u8 => Ok(Op::Bang),
            x if x == Op::Pop as u8 => Ok(Op::Pop),
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
            2 => push_bigendian(&mut instruction, offset, *o as u16),
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
            Minus => Definition {
                name: "OpMinus",
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

pub fn display_instructions(instructions: Vec<Vec<u8>>) -> String {
    let mut result = String::new();
    let mut offset = 0;
    for instr in instructions {
        if !result.is_empty() {
            result.push('\n');
        }
        result.push_str(&format!("{:0width$} ", offset, width = 4));

        let def: Definition = Op::try_from(instr[0])
            .unwrap_or_else(|_| panic!("Definition for instruction {} not found", instr[0]))
            .into();

        result.push_str(def.name);

        let op = instr[0]
            .try_into()
            .unwrap_or_else(|_| panic!("Unknown op: {}", instr[0]));

        match op {
            Op::Constant => result.push_str(&format!(" {}", read_bigendian(&instr, 1))),
            Op::True => {}
            Op::False => {}
            Op::Add => {}
            Op::Sub => {}
            Op::Mul => {}
            Op::Div => {}
            Op::Equal => {}
            Op::NotEqual => {}
            Op::GreaterThan => {}
            Op::Minus => {}
            Op::Bang => {}
            Op::Pop => {}
        }

        offset += instr.len();
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
            (Op::Minus, vec![], vec![Op::Minus.byte()]),
            (Op::Bang, vec![], vec![Op::Bang.byte()]),
            (Op::Pop, vec![], vec![Op::Pop.byte()]),
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
            make(Op::True, &vec![]).unwrap(),
            make(Op::False, &vec![]).unwrap(),
            make(Op::Equal, &vec![]).unwrap(),
            make(Op::NotEqual, &vec![]).unwrap(),
            make(Op::GreaterThan, &vec![]).unwrap(),
            make(Op::Minus, &vec![]).unwrap(),
            make(Op::Bang, &vec![]).unwrap(),
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
            0014 OpTrue
            0015 OpFalse
            0016 OpEqual
            0017 OpNotEqual
            0018 OpGreaterThan
            0019 OpMinus
            0020 OpBang
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
