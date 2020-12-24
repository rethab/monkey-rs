#[cfg(test)]
mod test {
    use monkey_interpreter::ast;
    use monkey_interpreter::lexer::Lexer;
    use monkey_interpreter::parser::Parser;
    use monkey_interpreter::x86::Compiler;
    use std::fs;
    use std::process::Command;

    #[test]
    fn test_arithmetic_expressions() {
        assert_eq!(run_to_int("1"), 1);
        assert_eq!(run_to_int("42"), 42);
        assert_eq!(run_to_int("5 + 5"), 10);
        assert_eq!(run_to_int("1 + 2"), 3);
        assert_eq!(run_to_int("1 + 2 + 3"), 6);
        assert_eq!(run_to_int("1 + 2 + 3 + 4"), 10);
    }

    fn run_to_int(input: &str) -> i32 {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p)
            .unwrap_or_else(|err| panic!("Failed to compile: {}", err));
        let assembly = format!("{}", c);
        run(&assembly)
    }

    fn parse(input: &str) -> ast::Program {
        *Parser::new(Lexer::new(input)).parse_program().unwrap()
    }

    /** Runs a fragment of assembly code, which can have one
     *  result in rax. The value of that register is returned. */
    fn run(program: &str) -> i32 {
        let entire_program = wrap_with_preamble_and_epilogue(program);
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

    fn wrap_with_preamble_and_epilogue(program: &str) -> String {
        let mut result = String::new();
        result.push_str(PREAMBLE);
        result.push_str(&program.replace('\n', "\n        "));
        result.push_str(EPILOGUE);
        result
    }

    const PREAMBLE: &'static str = ".data
.LC0:
        .string \"%d\"
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
