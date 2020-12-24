use monkey::ast;
use monkey::environment;
use monkey::evaluator;
use monkey::lexer::Lexer;
use monkey::object;
use monkey::parser::Parser;
use monkey::vm;
use monkey::vm::compiler;

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
            Err(err) => println!("Failed to {}: {}", interpreter.verb(), err),
        },
        Err(err) => println!("Failed to parse: {}", err),
    }
}

trait Interpreter {
    fn eval(&mut self, line: ast::Program) -> Result<object::Object, String>;
    fn verb(&self) -> &'static str;
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
    fn verb(&self) -> &'static str {
        "evaluate"
    }

    fn eval(&mut self, line: ast::Program) -> Result<object::Object, String> {
        evaluator::eval(line, Rc::clone(&self.env))
    }
}

struct VirtualMachine {
    compiler: compiler::Compiler,
}

impl Interpreter for VirtualMachine {
    fn verb(&self) -> &'static str {
        "compile"
    }

    fn eval(&mut self, line: ast::Program) -> Result<object::Object, String> {
        self.compiler.compile(line)?;
        let bytecode = self.compiler.bytecode();
        let mut vm = vm::Vm::new(&bytecode);
        vm.run()?;
        let result = vm.last_popped_stack_elem();
        Ok(result.clone())
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self {
            compiler: compiler::Compiler::default(),
        }
    }
}
