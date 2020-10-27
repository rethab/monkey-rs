use monkey_interpreter::environment::Environment;
use monkey_interpreter::evaluator::eval as evaluate;
use monkey_interpreter::lexer::Lexer;
use monkey_interpreter::parser::Parser;

use std::io;
use std::io::prelude::*;
use std::{cell::RefCell, rc::Rc};

const PROMPT: &str = ">> ";
const QUIT: &str = "\\q";

fn main() {
    let stdin = io::stdin();
    let stdout = io::stdout();

    let env = Rc::new(RefCell::new(Environment::default()));

    prompt(&stdout);
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if line.trim() == QUIT {
            break;
        }
        eval(&line, Rc::clone(&env));
        prompt(&stdout);
    }
}

fn prompt(stdout: &io::Stdout) {
    print!("{}", PROMPT);
    stdout.lock().flush().unwrap();
}

fn eval(line: &str, env: Rc<RefCell<Environment>>) {
    let mut parser = Parser::new(Lexer::new(line));
    match parser.parse_program() {
        Ok(program) => match evaluate(*program, env) {
            Ok(result) => println!("{}", result.inspect()),
            Err(err) => println!("Failed to evaluate: {}", err),
        },
        Err(err) => println!("Failed to pase: {}", err),
    }
}
