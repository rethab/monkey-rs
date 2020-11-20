use monkey_interpreter::ast;
use monkey_interpreter::compiler;
use monkey_interpreter::environment;
use monkey_interpreter::evaluator;
use monkey_interpreter::lexer::Lexer;
use monkey_interpreter::object;
use monkey_interpreter::parser::Parser;
use monkey_interpreter::vm;

use std::io;
use std::io::prelude::*;
use std::{cell::RefCell, rc::Rc};

const PROMPT: &str = ">> ";
const QUIT: &str = "\\q";

fn main() {
    let stdin = io::stdin();
    let stdout = io::stdout();

    let mut interpreter = VirtualMachine::default();

    prompt(&stdout);
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if line.trim() == QUIT {
            break;
        }
        eval(&line, &mut interpreter);
        prompt(&stdout);
    }
}

fn prompt(stdout: &io::Stdout) {
    print!("{}", PROMPT);
    stdout.lock().flush().unwrap();
}

fn eval<T: Interpreter>(line: &str, interpreter: &mut T) {
    let mut parser = Parser::new(Lexer::new(line));
    match parser.parse_program() {
        Ok(program) => match interpreter.eval(*program) {
            Ok(result) => println!("{}", result.inspect()),
            Err(err) => println!("Failed to evaluate: {}", err),
        },
        Err(err) => println!("Failed to pase: {}", err),
    }
}

trait Interpreter {
    fn eval(&self, line: ast::Program) -> Result<object::Object, String>;
}

struct Evaluator {
    env: Rc<RefCell<environment::Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self {
            env: Rc::new(RefCell::new(environment::Environment::default())),
        }
    }
}

impl Interpreter for Evaluator {
    fn eval(&self, line: ast::Program) -> Result<object::Object, String> {
        evaluator::eval(line, Rc::clone(&self.env))
    }
}

struct VirtualMachine {}

impl Interpreter for VirtualMachine {
    fn eval(&self, line: ast::Program) -> Result<object::Object, String> {
        let mut compiler = compiler::Compiler::default();
        compiler.compile(line)?;
        let bytecode = compiler.bytecode();
        let mut vm = vm::Vm::new(&bytecode);
        vm.run()?;
        let result = vm.stack_top();
        Ok(result.clone())
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self {}
    }
}
