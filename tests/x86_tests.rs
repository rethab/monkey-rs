extern crate tempfile;

#[cfg(test)]
mod test {
    use monkey::ast;
    use monkey::lexer::Lexer;
    use monkey::parser::Parser;
    use monkey::x86::Compiler;
    use std::fs::File;
    use std::io::prelude::*;
    use std::io::Read;
    use std::path::Path;
    use std::process::Command;
    use tempfile::{self, TempDir};

    const TRUE: i32 = -1;
    const FALSE: i32 = 0;

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
    fn test_boolean_expressions() {
        assert_eq!(run_to_int("true"), TRUE);
        assert_eq!(run_to_int("false"), FALSE);
        assert_eq!(run_to_int("1 == 1"), TRUE);
        assert_eq!(run_to_int("1 == 2"), FALSE);
        assert_eq!(run_to_int("1 != 2"), TRUE);
        assert_eq!(run_to_int("1 != 1"), FALSE);
        assert_eq!(run_to_int("1 > 1"), FALSE);
        assert_eq!(run_to_int("2 > 1"), TRUE);
        assert_eq!(run_to_int("2 < 1"), FALSE);
        assert_eq!(run_to_int("(1 + 1) < (2 + 2)"), TRUE);
        assert_eq!(run_to_int("(1 + 1) == (2 + 2)"), FALSE);
        assert_eq!(run_to_int("4 == (2 + 2)"), TRUE);
        assert_eq!(run_to_int("!false"), TRUE);
        assert_eq!(run_to_int("!true"), FALSE);
        assert_eq!(run_to_int("!(2 < 1)"), TRUE);
        assert_eq!(run_to_int("!!(2 < 1)"), FALSE);
        assert_eq!(run_to_int("!!!(2 < 1)"), TRUE);
    }

    #[test]
    fn test_global_let_statements() {
        assert_eq!(run_to_int("let a = 1; a"), 1);
        assert_eq!(run_to_int("let a = 2; a + 2"), 4);
        assert_eq!(run_to_int("let a = 2; let b = 3; a * b"), 6);
        assert_eq!(run_to_int("let a = 2; let b = 3; b"), 3);
        assert_eq!(run_to_int("let hello = 768; let foo = hello / 2; foo"), 384);
    }

    #[test]
    fn test_if_expressions() {
        assert_eq!(run_to_int("if (1 < 2) { 3 }"), 3);
        assert_eq!(run_to_int("if (1 < 2) { 3 } else { 4 }"), 3);
        assert_eq!(run_to_int("if (2 < 2) { 3 } else { 4 }"), 4);
        assert_eq!(run_to_int("if (3 < 2) { 3 } else { 4 }"), 4);
        assert_eq!(run_to_int("if (1 < 2) { 3 + 3 } else { 4 }"), 6);
        assert_eq!(run_to_int("if (1 < 2) { 3 + 3 } else { 4 }"), 6);
        assert_eq!(run_to_int("if (1 < 2) { 3 + 3 } else { 2 + 2 }"), 6);
        assert_eq!(
            run_to_int("if (1 < 2) { if (3 < 2) { 1 } else { 3 } } else { 4 }"),
            3
        );
        assert_eq!(run_to_int("let a = if (200 > 201) { 2 } else { 3 }; a"), 3);
        assert_eq!(
            run_to_int("let a = 3; let b = if (a < 4) { a * 3 } else { a * 2 }; b - a"),
            6
        );
        assert_eq!(
            run_to_int("if (if(false){10}else{false}) {10} else {20}"),
            20
        );
        assert_eq!(run_to_int("if (3 < 2) { 3 }"), 0);
    }

    #[test]
    fn test_functions_without_arguments() {
        assert_eq!(run_to_int("let a = fn() { 5; }; a();"), 5);
        assert_eq!(run_to_int("let a = fn() { 5 + 10; }; a();"), 15);
        assert_eq!(run_to_int("fn() { 1; }();"), 1);
        assert_eq!(
            run_to_int(
                "
                    let fa = fn() { 99 };
                    let fb = fn() { fa(); };
                    fb();
                "
            ),
            99
        );
        assert_eq!(run_to_int("let a = fn() { 5 + 10; }; a();"), 15);
        assert_eq!(
            run_to_int(
                "
                    let one = fn() { 1 };
                    let two = fn() { one() + one() };
                    one() + two()
                "
            ),
            3
        );
        assert_eq!(
            run_to_int(
                "
                    let one = fn() { 1 };
                    let two = fn() { 2 };
                    one()
                "
            ),
            1
        );
        assert_eq!(run_to_int("5; 4;"), 4);
    }

    #[test]
    fn test_return_statement() {
        assert_eq!(run_to_int("let a = fn() { return 5; }; a();"), 5);
        assert_eq!(run_to_int("let a = fn() { return 5 + 10; }; a();"), 15);
        assert_eq!(
            run_to_int(
                "
                fn() {
                    if (true) { return 5 }
                    else { return 4 }
                }();
                "
            ),
            5
        );
        assert_eq!(
            run_to_int(
                "
                fn() {
                    if (2 > 1 ) { return 5 }
                    4
                }();
                "
            ),
            5
        );
        assert_eq!(
            run_to_int(
                "
                fn() {
                    if (2 > 4 ) { return 5 }
                    4
                }();
                "
            ),
            4
        );
        assert_eq!(
            run_to_int(
                "
                fn() {
                    return if (2 > 3) { 5 } else { 4 }
                }();
                "
            ),
            4
        );
    }

    #[test]
    fn test_function_arguments_in_registers() {
        assert_eq!(run_to_int("fn(a) { a }(5)"), 5);
        assert_eq!(run_to_int("fn(a) { a }(5 + 5)"), 10);
        assert_eq!(run_to_int("fn(a) { a + a }(5)"), 10);
        assert_eq!(run_to_int("fn(a, b) { b / a }(4 / 2, 5 * 2)"), 5);
        assert_eq!(run_to_int("let div = fn(a, b) { a / b }; div(10, 2)"), 5);
        assert_eq!(
            run_to_int(
                "
                    let max = fn(a, b) { if (a > b) { a } else { b } };
                    max(10, 11)
                "
            ),
            11
        );
        assert_eq!(
            run_to_int(
                "
                    let max = fn(a, b) { if (a > b) { a } else { b } };
                    let maxthree = fn(a, b, c) { max(a, max(b, c)) };
                    maxthree(1, 3, 2)
                "
            ),
            3
        );
        assert_eq!(
            run_to_int(
                "
                    let four = fn(a, b, c, d) { (a + b) * c / d };
                    four(2, 5, 4, 2)
                "
            ),
            14
        );
        assert_eq!(
            run_to_int(
                "
                    let five = fn(a, b, c, d, e) { (a + b) * c / d - e };
                    five(2, 5, 4, 2, 4)
                "
            ),
            10
        );
        assert_eq!(
            run_to_int(
                "
                    let six = fn(a, b, c, d, e, f) { (a + b) * c / d - e * f };
                    six(2, 5, 4, 2, 4, 6)
                "
            ),
            -10
        );
    }

    #[test]
    fn test_function_arguments_on_stack() {
        assert_eq!(
            run_to_int(
                "
                    let seven = fn(a, b, c, d, e, f, g) { (a + b) * c / d - e * f + g };
                    seven(2, 5, 4, 2, 4, 6, 10)
                "
            ),
            0
        );
        assert_eq!(
            run_to_int(
                "
                    let eight = fn(a, b, c, d, e, f, g, h) { g / h};
                    eight(0, 0, 0, 0, 0, 0, 6, 2)
                "
            ),
            3
        );
        assert_eq!(
            run_to_int(
                "
                    let nine = fn(a, b, c, d, e, f, g, h, i) { h / i};
                    nine(0, 0, 0, 0, 0, 0, 0, 6, 2)
                "
            ),
            3
        );
        assert_eq!(
            run_to_int(
                "
                    let nine = fn(a, b, c, d, e, f, g, h, i) {
                        h / g * i - f
                    };
                    nine(0, 0, 0, 0, 0, 0, 1, 2, 3)
                "
            ),
            6
        );
    }

    #[test]
    fn test_local_let_statements() {
        assert_eq!(run_to_int("let a = 6; fn() { let a = 5; a }()"), 5);
        assert_eq!(
            run_to_int(
                "
                    let a = 6;
                    let f = fn() { let a = 5; a };
                    f() + a
                "
            ),
            11
        );
        assert_eq!(
            run_to_int("let x = fn() { let a = 5; a }; let a = 6; x()"),
            5
        );
        assert_eq!(
            run_to_int(
                "
                let a = 7;
                let f = fn() { let a = 1; let b = 2; a + b };
                let g = fn() { let a = 3; let b = 4; a + b };
                a + f() + g()
                "
            ),
            17
        );
        assert_eq!(
            run_to_int(
                "
                let a = 7;
                let f = fn(b) { let a = 1; a + b };
                let g = fn() { let a = 3; let b = 4; a + b };
                f(2) + g()
                "
            ),
            10
        );
        assert_eq!(run_to_int("fn() { let a = 5; a }()"), 5);
        assert_eq!(run_to_int("fn() { let a = 5; a + a }()"), 10);
        assert_eq!(run_to_int("fn() { let a = 5; let b = 6; b + a }()"), 11);
        assert_eq!(run_to_int("fn() { let a = 5; let b = 6; a }()"), 5);
        assert_eq!(run_to_int("fn() { let a = 5 + 6; a * 2 }()"), 22);
        assert_eq!(run_to_int("let a = fn() { let b = 5; b }; a()"), 5);
        assert_eq!(
            run_to_int("fn() { let a = 5; let b = 6; let c = a + b; c + a + b}()"),
            22
        );
        assert_eq!(
            run_to_int(
                "fn() {
                let a = 5;
                let b = 6;
                let c = a + b;
                let d = a + b + c;
                d
            }()"
            ),
            22
        );
        assert_eq!(
            run_to_int(
                "
                    let nine = fn(a, b, c, d, e, f, g, h, i) {
                        let res = h / g * i - f;
                        res * 2
                    };
                    nine(0, 0, 0, 0, 0, 0, 1, 2, 3)
                "
            ),
            12
        );
    }

    fn run_to_int(input: &str) -> i32 {
        let p = parse(input);
        let mut c = Compiler::default();
        c.compile(p);
        run(c)
    }

    fn parse(input: &str) -> ast::Program {
        *Parser::new(Lexer::new(input)).parse_program().unwrap()
    }

    /** Runs a fragment of assembly code, which can have one
     *  result in rax. The value of that register is returned. */
    fn run(c: Compiler) -> i32 {
        let dir = TempDir::new().expect("Failed to create tempdir");
        let source = dir.path().join("program.s");
        let executable = dir.path().join("program");

        let entire_program = wrap_with_preamble_and_epilogue(c);
        println!("Program: {}", entire_program);
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
            dump_file(source);
            panic!("Failed to run gcc on {:?}: {:?}", source, output);
        }
    }

    fn run_executable(executable: &Path) -> i32 {
        let output = Command::new(executable)
            .output()
            .unwrap_or_else(|err| panic!("Failed to run executable {:?}: {:?}", executable, err));
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        stdout
            .parse()
            .unwrap_or_else(|_| panic!("Failed to parse '{}' as i32: {}", stdout, stderr))
    }

    fn wrap_with_preamble_and_epilogue(c: Compiler) -> String {
        let mut result = String::new();
        result.push_str(".data\n");
        result.push_str(".LC0:\n        .string \"%d\"\n");
        for (label, value) in &c.globals {
            result.push_str(&format!("{}:\n        .quad {}\n", label, value));
        }
        result.push_str(PREAMBLE);
        result.push_str(&format!("{}", c.main_function()));
        result.push_str(EPILOGUE);
        for (label, instructions) in c.functions() {
            result.push_str(&format!("{}:\n", label));
            result.push_str(&format!("{}", instructions));
        }
        result
    }

    fn dump_file(path: &Path) {
        let mut f = File::open(path).expect("Failed to access file to dump");
        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .expect("Failed to read file to string");
        println!("Contents of {:?}:\n{}\n", path, contents);
    }

    const PREAMBLE: &'static str = "
.text
.global main
main:
        PUSHQ   %rbp
        MOVQ    %rsp, %rbp
        PUSHQ   %rbx
        PUSHQ   %r12
        PUSHQ   %r13
        PUSHQ   %r14
        PUSHQ   %r15
";

    const EPILOGUE: &'static str = "        POPQ    %r15
        POPQ    %r14
        POPQ    %r13
        POPQ    %r12
        POPQ    %rbx
        MOVQ    %rbp, %rsp
        POPQ    %rbp
        RET
";
}
