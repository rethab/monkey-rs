#[cfg(test)]
mod test {
    use std::fs;
    use std::process::Command;

    #[test]
    fn test_run() {
        let program = "
                MOVQ    $0, %rax
        count:  INCQ    %rax
                CMPQ    $13, %rax
                JLE     count
        ";

        assert_eq!(run(program), 14);
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
        let output = dbg!(Command::new("gcc")
            .arg("-no-pie")
            .arg(filename)
            .arg("-o")
            .arg(executable_name))
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
        result.push_str(program);
        result.push_str(EPILOGUE);
        result
    }

    const PREAMBLE: &'static str = "
        .data
        .LC0:
                .string \"%d\"
        .text
        .global main
        main:
                PUSHQ   %rbp            # store base pointer
                MOVQ    %rsp, %rbp      # base pointer is our stack pointer
    ";

    const EPILOGUE: &'static str = "
        MOVQ    %rax, %rsi
        MOVQ    $.LC0, %rdi
        MOVQ    $0, %rax
        CALL    printf         
        LEAVE
        RET
    ";
}
