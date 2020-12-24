#[cfg(test)]
mod test {
    use monkey_interpreter::ast;
    use monkey_interpreter::lexer::Lexer;
    use monkey_interpreter::parser::Parser;
    use monkey_interpreter::x86::Compiler;
    use std::collections::HashMap;
    use std::fs;
    use std::process::Command;

    #[test]
    fn test_arithmetic_infix_expressions() {
        assert_eq!(run_to_int("1"), 1);
        assert_eq!(run_to_int("42"), 42);
        assert_eq!(run_to_int("5 + 5"), 10);
        assert_eq!(run_to_int("1 + 2"), 3);
        assert_eq!(run_to_int("1 + 2 + 3"), 6);
        assert_eq!(run_to_int("1 + 2 + 3 + 4"), 10);
        assert_eq!(run_to_int("5 - 5"), 0);
        assert_eq!(run_to_int("5 - 1"), 4);
        assert_eq!(run_to_int("5 - 2 - 1"), 2);
        assert_eq!(run_to_int("5 - 2 + 4"), 7);
        assert_eq!(run_to_int("5 * 5"), 25);
        assert_eq!(run_to_int("5 * 5 - 2"), 23);
        assert_eq!(run_to_int("5 / 5"), 1);
        assert_eq!(run_to_int("10 / 5 * 2"), 4);
        assert_eq!(run_to_int("42 / 21 + 7 - 2 * 3"), 3);
    }

    #[test]
    fn test_global_let_statements() {
        assert_eq!(run_to_int("let a = 1; a"), 1);
        assert_eq!(run_to_int("let a = 2; a + 2"), 4);
        assert_eq!(run_to_int("let a = 2; let b = 3; a * b"), 6);
        assert_eq!(run_to_int("let a = 2; let b = 3; b"), 3);
        assert_eq!(run_to_int("let hello = 768; let foo = hello / 2; foo"), 384);
    }

    fn run_to_int(input: &str) -> i32 {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p)
            .unwrap_or_else(|err| panic!("Failed to compile: {}", err));
        let labels = c.labels.clone();
        let assembly = format!("{}", c);
        run(&assembly, labels)
    }

    fn parse(input: &str) -> ast::Program {
        *Parser::new(Lexer::new(input)).parse_program().unwrap()
    }

    /** Runs a fragment of assembly code, which can have one
     *  result in rax. The value of that register is returned. */
    fn run(program: &str, labels: HashMap<String, i32>) -> i32 {
        let entire_program = wrap_with_preamble_and_epilogue(program, labels);
        let source_file = write_to_temp_file(&entire_program);
        let executable_name = assemble_and_link(&source_file);
        let result = run_executable(&executable_name);
        result
    }

    fn write_to_temp_file(program: &str) -> &str {
        let filename = "/tmp/program.s";
        fs::write(filename, program).expect("Failed to write to file");
        filename
    }

    fn assemble_and_link(filename: &str) -> &str {
        let executable_name = "/tmp/program";
        let output = Command::new("gcc")
            .arg("-no-pie")
            .arg(filename)
            .arg("-o")
            .arg(executable_name)
            .output()
            .expect("Failed to run gcc");
        if !output.status.success() {
            panic!("gcc did not run successfully: {:?}", output);
        }
        executable_name
    }

    fn run_executable(executable_name: &str) -> i32 {
        let output = Command::new(executable_name)
            .output()
            .expect("Failed to run executable");
        let str = String::from_utf8_lossy(&output.stdout);
        str.parse()
            .unwrap_or_else(|_| panic!("Failed to parse {} as i32", str))
    }

    fn wrap_with_preamble_and_epilogue(program: &str, labels: HashMap<String, i32>) -> String {
        let mut result = String::new();
        result.push_str(".data\n");
        result.push_str(".LC0:\n        .string \"%d\"\n");
        for (label, value) in labels {
            result.push_str(&format!("{}:\n        .quad {}\n", label, value));
        }
        result.push_str(PREAMBLE);
        result.push_str(&program.replace('\n', "\n        "));
        result.push_str(EPILOGUE);
        result
    }

    const PREAMBLE: &'static str = "
.text
.global main
main:
        PUSHQ   %rbp
        MOVQ    %rsp, %rbp
        ";

    const EPILOGUE: &'static str = "MOVQ    %rax, %rsi
        MOVQ    $.LC0, %rdi
        MOVQ    $0, %rax
        CALL    printf         
        LEAVE
        RET";
}
