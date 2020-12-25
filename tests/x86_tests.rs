extern crate tempfile;

#[cfg(test)]
mod test {
    use monkey::ast;
    use monkey::lexer::Lexer;
    use monkey::parser::Parser;
    use monkey::x86::Compiler;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;
    use std::process::Command;
    use tempfile::{self, TempDir};

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
        let dir = TempDir::new().expect("Failed to create tempdir");
        let source = dir.path().join("program.s");
        let executable = dir.path().join("program");

        let entire_program = wrap_with_preamble_and_epilogue(program, labels);
        write_to_temp_file(&source, &entire_program);
        assemble_and_link(&source, &executable);
        run_executable(&executable)
    }

    fn write_to_temp_file(path: &Path, program: &str) {
        let mut file = File::create(path).expect("Failed to create file");
        writeln!(file, "{}", program).expect("Failed to write to file");
    }

    fn assemble_and_link(source: &Path, executable: &Path) {
        let output = Command::new("gcc")
            .arg("-no-pie")
            .arg(source)
            .arg("-o")
            .arg(executable)
            .output()
            .unwrap_or_else(|err| panic!("Failed to run gcc on {:?}: {}", source, err));
        if !output.status.success() {
            panic!("Failed to run gcc on {:?}: {:?}", source, output);
        }
    }

    fn run_executable(executable: &Path) -> i32 {
        let output = Command::new(executable)
            .output()
            .unwrap_or_else(|err| panic!("Failed to run executable {:?}: {:?}", executable, err));
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
