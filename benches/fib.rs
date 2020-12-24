use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use std::{cell::RefCell, rc::Rc};

use monkey::ast;
use monkey::environment;
use monkey::evaluator;
use monkey::lexer::Lexer;
use monkey::object;
use monkey::parser::Parser;
use monkey::vm;
use monkey::vm::compiler;

const FIB: &'static str = "
        let fibonacci = fn(x) {
            if (x < 2) { x }
            else { fibonacci(x - 1) + fibonacci(x - 2) }
        };
        fibonacci(20);
    ";

fn bench_fibs(c: &mut Criterion) {
    let program = parse(FIB);

    let mut group = c.benchmark_group("Fibonacci");
    group.sample_size(10);
    group.bench_function(BenchmarkId::new("Virtual Machine", 20), |b| {
        b.iter(|| compile(program.clone()))
    });
    group.bench_function(BenchmarkId::new("Interpreter", 20), |b| {
        b.iter(|| evaluate(program.clone()))
    });
    group.finish()
}

criterion_group!(benches, bench_fibs);
criterion_main!(benches);

fn parse(input: &str) -> ast::Program {
    let mut parser = Parser::new(Lexer::new(input));
    *parser.parse_program().expect("Failed to parse program")
}

fn evaluate(program: ast::Program) -> object::Object {
    let env = Rc::new(RefCell::new(environment::Environment::default()));
    evaluator::eval(program, Rc::clone(&env)).expect("Failed to evaluate")
}

fn compile(program: ast::Program) -> object::Object {
    let mut compiler = compiler::Compiler::default();
    compiler.compile(program).expect("Failed to compile");
    let bytecode = compiler.bytecode();
    let mut vm = vm::Vm::new(&bytecode);
    vm.run().expect("Failed to run");
    let result = vm.last_popped_stack_elem();
    result.clone()
}
