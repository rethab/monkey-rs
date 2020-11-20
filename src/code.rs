pub type Instructions = Vec<u8>;
pub type Opcode = u8;

pub const OP_CONSTANT: Opcode = 0x00;

struct Definition<'a> {
    name: &'a str,
    operand_widths: Vec<i32>,
}

pub fn make(op: Opcode, operands: &[i32]) -> Result<Vec<u8>, String> {
    let def = lookup_definition(op)?;

    let mut instruction_len = 1;
    for width in def.operand_widths.iter() {
        instruction_len += width;
    }

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op;

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

fn lookup_definition<'a>(op: u8) -> Result<Definition<'a>, String> {
    match op {
        OP_CONSTANT => Ok(Definition {
            name: "OpConstant",
            operand_widths: vec![2],
        }),
        unknown => Err(format!("opcode {} undefined", unknown)),
    }
}

fn push_bigendian(vs: &mut [u8], offset: usize, v: u16) {
    vs[offset] = ((v & 0xFF00) >> 8) as u8;
    vs[offset + 1] = (v & 0x00FF) as u8;
}

fn read_bigendian(vs: &[u8], offset: usize) -> u16 {
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

        let def = lookup_definition(instr[0])
            .unwrap_or_else(|_| panic!("Definition for instruction {} not found", instr[0]));

        result.push_str(def.name);

        result.push_str(&format!(" {}", read_bigendian(&instr, 1)));

        offset += instr.len();
    }
    result
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_make() {
        let tests = vec![(OP_CONSTANT, vec![65534], vec![OP_CONSTANT, 255, 254])];

        for (op, operands, expected) in tests {
            let instruction = make(op, &operands).expect(&format!(
                "Failed to make op={} and operands={:?}",
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
            make(OP_CONSTANT, &vec![1]).unwrap(),
            make(OP_CONSTANT, &vec![2]).unwrap(),
            make(OP_CONSTANT, &vec![65535]).unwrap(),
        ];

        let expected = "
            0000 OpConstant 1
            0003 OpConstant 2
            0006 OpConstant 65535
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
