use super::ast::{self, *};
use super::environment::Environment;
use super::object::Object::*;
use super::object::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type EvalResult = Result<Object, String>;

pub fn eval(p: Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = None;
    for stmt in p.0 {
        match eval_statement(stmt, Rc::clone(&env))? {
            Return(r) => return Ok(*r),
            other => result = Some(other),
        }
    }
    Ok(result.unwrap())
}

fn eval_block(stmts: Vec<Statement>, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = None;
    for stmt in stmts {
        match eval_statement(stmt, Rc::clone(&env))? {
            Return(r) => return Ok(Return(r)),
            other => result = Some(other),
        }
    }
    Ok(result.unwrap())
}

fn eval_statement(s: Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match s {
        Statement::Expression { value, .. } => eval_expression(value, env),
        Statement::Block { statements, .. } => eval_block(statements, env),
        Statement::Return { value, .. } => {
            eval_expression(value, env).map(|res| Return(Box::new(res)))
        }
        Statement::Let {
            expression, name, ..
        } => eval_expression(*expression, Rc::clone(&env)).map(|obj| {
            env.borrow_mut().set(&name.value, obj);
            NULL
        }),
    }
}

fn eval_expression(exp: Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match exp {
        Expression::IntLiteral { value, .. } => Ok(Integer(value as i64)),
        Expression::BooleanLiteral { value: true, .. } => Ok(TRUE),
        Expression::BooleanLiteral { value: false, .. } => Ok(FALSE),
        Expression::StringLiteral { value, .. } => Ok(String_(value)),
        Expression::ArrayLiteral { values, .. } => {
            let objects = values
                .into_iter()
                .map(|v| eval_expression(v, Rc::clone(&env)))
                .collect::<Result<Vec<Object>, String>>()?;
            Ok(Array(objects))
        }
        Expression::Index {
            container, index, ..
        } => eval_index_expression(*container, *index, Rc::clone(&env)),
        Expression::MapLiteral { values, .. } => {
            let objects = values
                .into_iter()
                .map(|(k, v)| {
                    eval_expression(k, Rc::clone(&env)).and_then(|k_eval| {
                        eval_expression(v, Rc::clone(&env)).map(|v_eval| (k_eval, v_eval))
                    })
                })
                .collect::<Result<Vec<(Object, Object)>, String>>()?;
            Ok(Map(objects))
        }
        Expression::Prefix { op, rhs, .. } => eval_prefix_expression(op, *rhs, env),
        Expression::Infix { lhs, op, rhs, .. } => eval_infix_expression(*lhs, op, *rhs, env),
        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => eval_if_expression(*condition, *consequence, alternative.map(|alt| *alt), env),
        Expression::Identifier(ident) => env
            .borrow()
            .get(&ident.value)
            .ok_or(format!("identifier not found: {}", ident.value)),
        Expression::FunctionLiteral {
            parameters, body, ..
        } => Ok(Object::Function {
            parameters,
            body,
            env,
        }),
        Expression::Call {
            function,
            arguments,
            ..
        } => eval_function(function, arguments, env),
    }
}

fn eval_function(
    function: ast::Function,
    arguments: Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    let (name, parameters, body, captured_env) = match function {
        ast::Function::Identifier(ident) => match env.borrow().get(&ident.value) {
            Some(Object::Function {
                parameters,
                body,
                env,
            }) => (ident.value, parameters, body, Some(env)),
            Some(other) => {
                return Err(format!(
                    "{} ({}) is not a function",
                    ident.value,
                    other.tpe()
                ))
            }
            None => match lookup_builtin(&ident) {
                Some(Object::Builtin { func }) => {
                    let mut params = Vec::new();
                    for arg in arguments {
                        params.push(eval_expression(arg, Rc::clone(&env))?);
                    }
                    return func(params);
                }
                Some(_) | None => return Err(format!("function not found: {}", ident.value)),
            },
        },
        ast::Function::Literal {
            parameters, body, ..
        } => ("<anonymous>".into(), parameters, body, None),
    };

    if arguments.len() != parameters.len() {
        return Err(format!(
            "Wrong number of arguments for {}. Expected {}, but got {}",
            name,
            parameters.len(),
            arguments.len()
        ));
    }

    let mut evaluated_params: HashMap<String, Object> = HashMap::new();
    for (arg, param) in arguments.into_iter().zip(parameters.into_iter()) {
        evaluated_params.insert(param.value, eval_expression(arg, Rc::clone(&env))?);
    }

    let outer = match captured_env {
        Some(e) => e,
        None => env,
    };

    let mut sub_env = Environment::new_sub(outer);

    sub_env.set_all(evaluated_params);

    eval_statement(*body, Rc::new(RefCell::new(sub_env))).map(|res| match res {
        Object::Return(v) => *v,
        other => other,
    })
}

fn eval_index_expression(
    container: Expression,
    index: Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    match eval_expression(container, Rc::clone(&env))? {
        Object::Array(objs) => match eval_expression(index, Rc::clone(&env))? {
            Object::Integer(i) => {
                if i >= objs.len() as i64 || i < 0 {
                    Err(format!("Index out of bounds: {}", i))
                } else {
                    Ok(objs[i as usize].clone())
                }
            }
            other => Err(format!("Cannot index with {}", other.tpe())),
        },
        Object::Map(objs) => {
            let key = eval_expression(index, Rc::clone(&env))?;
            Ok(objs
                .iter()
                .find(|(k, _)| *k == key)
                .map(|(_, v)| v.clone())
                .unwrap_or(NULL))
        }
        other => Err(format!("Cannot index {}", other.tpe())),
    }
}

fn eval_prefix_expression(
    op: String,
    rhs: Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    match op.as_str() {
        "!" => eval_bang_operator_expression(rhs, env),
        "-" => eval_negative_operator_expression(rhs, env),
        _ => Err(format!("unhandled prefix: {}", op)),
    }
}

fn eval_infix_expression(
    lhs: Expression,
    op: String,
    rhs: Expression,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    match (
        eval_expression(lhs, Rc::clone(&env))?,
        eval_expression(rhs, Rc::clone(&env))?,
    ) {
        (Integer(l), Integer(r)) => eval_infix_int_expression(l, &op, r),
        (Boolean(l), Boolean(r)) => eval_infix_bool_expression(l, &op, r),
        (String_(l), String_(r)) => eval_infix_string_expression(&l, &op, &r),
        (l, r) => Err(format!("type mismatch: {} {} {}", l.tpe(), op, r.tpe())),
    }
}

fn eval_if_expression(
    condition: Expression,
    consequence: Statement,
    alternative: Option<Statement>,
    env: Rc<RefCell<Environment>>,
) -> EvalResult {
    if eval_to_truthy(condition, Rc::clone(&env))? {
        eval_statement(consequence, Rc::clone(&env))
    } else {
        match alternative {
            Some(alt) => eval_statement(alt, env),
            None => Ok(NULL),
        }
    }
}

fn eval_bang_operator_expression(rhs: Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    eval_expression(rhs, env).map(|res| match res {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    })
}

fn eval_negative_operator_expression(rhs: Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match eval_expression(rhs, env)? {
        Integer(n) => Ok(Integer(-n)),
        other => Err(format!("unknown operator: -{}", other.tpe())),
    }
}

fn eval_infix_int_expression(l: i64, op: &str, r: i64) -> EvalResult {
    match op {
        "+" => Ok(Integer(l + r)),
        "-" => Ok(Integer(l - r)),
        "*" => Ok(Integer(l * r)),
        "/" => Ok(Integer(l / r)),
        ">" => Ok(Boolean(l > r)),
        "<" => Ok(Boolean(l < r)),
        "==" => Ok(Boolean(l == r)),
        "!=" => Ok(Boolean(l != r)),
        other => Err(format!("unknown operator: {}INTEGER", other)),
    }
}

fn eval_infix_bool_expression(l: bool, op: &str, r: bool) -> EvalResult {
    match op {
        "==" => Ok(Boolean(l == r)),
        "!=" => Ok(Boolean(l != r)),
        other => Err(format!("unknown operator: BOOLEAN {} BOOLEAN", other)),
    }
}

fn eval_infix_string_expression(l: &str, op: &str, r: &str) -> EvalResult {
    match op {
        "+" => Ok(String_(format!("{}{}", l, r))),
        other => Err(format!("unknown operator: STRING {} STRING", other)),
    }
}

fn eval_to_truthy(exp: Expression, env: Rc<RefCell<Environment>>) -> Result<bool, String> {
    eval_expression(exp, env).map(|res| match res {
        Boolean(n) => n,
        NULL => false,
        _ => true,
    })
}

#[cfg(test)]
mod test {

    use super::super::lexer::Lexer;
    use super::super::parser::Parser;
    use super::*;

    #[test]
    fn test_literal_objects() {
        assert_eq!(Integer(4), evaluate("4"));
        assert_eq!(Integer(437), evaluate("437"));
        assert_eq!(FALSE, evaluate("false"));
    }

    #[test]
    fn test_band_operator() {
        assert_eq!(FALSE, evaluate("!true"));
        assert_eq!(FALSE, evaluate("!5"));
        assert_eq!(TRUE, evaluate("!!true"));
        assert_eq!(FALSE, evaluate("!!false"));
        assert_eq!(TRUE, evaluate("!!5"));
    }

    #[test]
    fn test_negative_operator() {
        assert_eq!(Integer(-1), evaluate("-1"));
        assert_eq!(Integer(-11), evaluate("-11"));
    }

    #[test]
    fn test_int_expressions() {
        assert_eq!(Integer(10), evaluate("5 + 5 + 5 + 5 - 10"));
        assert_eq!(Integer(32), evaluate("2 * 2 * 2 * 2 * 2"));
        assert_eq!(Integer(0), evaluate("-50 + 100 + -50"));
        assert_eq!(Integer(20), evaluate("5 * 2 + 10"));
        assert_eq!(Integer(25), evaluate("5 + 2 * 10"));
        assert_eq!(Integer(0), evaluate("20 + 2 * -10"));
        assert_eq!(Integer(60), evaluate("50 / 2 * 2 + 10"));
        assert_eq!(Integer(30), evaluate("2 * (5 + 10)"));
        assert_eq!(Integer(37), evaluate("3 * 3 * 3 + 10"));
        assert_eq!(Integer(37), evaluate("3 * (3 * 3) + 10"));
        assert_eq!(Integer(50), evaluate("(5 + 10 * 2 + 15 / 3) * 2 + -10"));
    }

    #[test]
    fn test_boolean_expressions() {
        assert_eq!(TRUE, evaluate("true"));
        assert_eq!(FALSE, evaluate("false"));
        assert_eq!(TRUE, evaluate("1 < 2"));
        assert_eq!(FALSE, evaluate("1 > 2"));
        assert_eq!(FALSE, evaluate("1 < 1"));
        assert_eq!(FALSE, evaluate("1 > 1"));
        assert_eq!(TRUE, evaluate("1 == 1"));
        assert_eq!(FALSE, evaluate("1 != 1"));
        assert_eq!(FALSE, evaluate("1 == 2"));
        assert_eq!(TRUE, evaluate("1 != 2"));

        assert_eq!(TRUE, evaluate("true == true"));
        assert_eq!(TRUE, evaluate("false == false"));
        assert_eq!(FALSE, evaluate("true == false"));
        assert_eq!(TRUE, evaluate("true != false"));
        assert_eq!(TRUE, evaluate("false != true"));
        assert_eq!(TRUE, evaluate("(1 < 2) == true"));
        assert_eq!(FALSE, evaluate("(1 < 2) == false"));
        assert_eq!(FALSE, evaluate("(1 > 2) == true"));
        assert_eq!(TRUE, evaluate("(1 > 2) == false"));
    }

    #[test]
    fn test_string_expressions() {
        assert_eq!(
            Object::String_("hello, world!".into()),
            evaluate("\"hello, world!\"")
        );
        assert_eq!(
            Object::String_("hello, world!".into()),
            evaluate("\"hello, \" + \"world!\"")
        );
    }

    #[test]
    fn test_if_expressions() {
        assert_eq!(Integer(1), evaluate("if (3 > 4) { 0 } else { 1 }"));
        assert_eq!(Integer(0), evaluate("if (3 < 4) { 0 } else { 1 }"));
        assert_eq!(NULL, evaluate("if (false) { 0 }"));
        assert_eq!(TRUE, evaluate("if (true) { true }"));
        assert_eq!(Integer(2), evaluate("if (1) { 2 } else { 3 }"));
    }

    #[test]
    fn test_return_statement() {
        assert_eq!(Integer(10), evaluate("return 10;"));
        assert_eq!(Integer(10), evaluate("return 10; 9"));
        assert_eq!(Integer(10), evaluate("return 2 * 5; 9"));
        assert_eq!(Integer(10), evaluate("9; return 2 * 5; 9"));
        assert_eq!(
            Integer(10),
            evaluate("if (true) { if (true) { return 10; } return 9; }")
        );
    }

    #[test]
    fn test_environment() {
        assert_eq!(Object::Integer(5), evaluate("let a = 5; a;"));
        assert_eq!(Object::Integer(25), evaluate("let a = 5 * 5; a;"));
        assert_eq!(Object::Integer(5), evaluate("let a = 5; let b = a; b;"));
        assert_eq!(
            Object::Integer(15),
            evaluate("let a = 5; let b = a; let c = a + b + 5; c;")
        );
    }

    #[test]
    fn test_array() {
        assert_eq!(
            Object::Array(vec![Object::Integer(1), Object::String_("foo".into())]),
            evaluate("[1, \"foo\"]")
        );
        assert_eq!(Object::Integer(2), evaluate("let xs = [1, 2, 3]; xs[1]"));
        assert_eq!(Object::Integer(2), evaluate("let xs = [1, 1+1, 3]; xs[1]"));
        assert_eq!(Object::Integer(3), evaluate("let xs = [1, 2, 3]; xs[1+1]"));
        assert_eq!(
            Object::Integer(3),
            evaluate(
                "
                let index = fn(arr, i) { return arr[i]; };
                let xs = [1, 2, 3];
                index(xs, 0) + index(xs, 1)"
            )
        );
    }

    #[test]
    fn test_map() {
        assert_eq!(
            Object::Map(vec![(Object::Integer(1), Object::String_("foo".into()))]),
            evaluate("{1: \"foo\"}")
        );
        assert_eq!(
            Object::Map(vec![
                (Object::String_("foo".into()), Object::Integer(2)),
                (Object::Boolean(true), Object::String_("helo".into()))
            ]),
            evaluate("{\"foo\": 1 + 1, true: \"he\"+\"lo\"}")
        );
        assert_eq!(Object::Integer(2), evaluate("{\"foo\": 1 + 1}[\"foo\"]"));
        assert_eq!(
            Object::Integer(2),
            evaluate("let map = {\"foo\": 1 + 1}; map[\"foo\"]")
        );
        assert_eq!(
            Object::Integer(1),
            evaluate("{\"foo\": 1, \"bar\": 2}[\"foo\"]")
        );
        assert_eq!(
            Object::Integer(2),
            evaluate("{\"foo\": 1, \"bar\": 2}[\"bar\"]")
        );
        assert_eq!(Object::Integer(2), evaluate("{1==2: 1, 1==1: 2}[2==2]"));
        assert_eq!(Object::Integer(2), evaluate("{2+2: 1, 1+1: 2}[1+1]"));
    }

    #[test]
    fn test_builtin_len() {
        assert_eq!(Object::Integer(0), evaluate("len(\"\")"));
        assert_eq!(Object::Integer(1), evaluate("len(\"a\")"));
        assert_eq!(Object::Integer(5), evaluate("len(\"a b c\")"));
        assert_eq!(Object::Integer(4), evaluate("let x = \"hell\"; len(x)"));
        assert_eq!(Object::Integer(4), evaluate("len([1, 2, 3, 4])"));
        assert_eq!(Object::Integer(0), evaluate("len([])"));
    }

    #[test]
    fn test_builtin_head() {
        assert_eq!(Object::String_("foo".into()), evaluate("head([\"foo\"])"));
        assert_eq!(Object::Integer(1), evaluate("head([1, 2])"));
        assert_eq!(Object::Integer(1), evaluate("let xs = [1, 2, 3]; head(xs)"));
        assert_eq!(
            Err("Called 'head' on empty array".into()),
            evaluate_res("head([])")
        );
    }

    #[test]
    fn test_builtin_last() {
        assert_eq!(Object::String_("foo".into()), evaluate("last([\"foo\"])"));
        assert_eq!(Object::Integer(2), evaluate("last([1, 2])"));
        assert_eq!(Object::Integer(3), evaluate("let xs = [1, 2, 3]; last(xs)"));
        assert_eq!(
            Err("Called 'last' on empty array".into()),
            evaluate_res("last([])")
        );
    }

    #[test]
    fn test_builtin_tail() {
        assert_eq!(Object::Array(vec![]), evaluate("tail([\"foo\"])"));
        assert_eq!(
            Object::Array(vec![Object::Integer(2)]),
            evaluate("tail([1, 2])")
        );
        assert_eq!(
            Object::Array(vec![Object::Integer(4)]),
            evaluate("tail(tail(tail([1, 2, 3, 4])))")
        );
        assert_eq!(
            Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            evaluate("let xs = [1, 2, 3]; tail(xs)")
        );
        assert_eq!(
            Err("Called 'tail' on empty array".into()),
            evaluate_res("tail([])")
        );
    }

    #[test]
    fn test_builtin_append() {
        assert_eq!(
            Object::Integer(0),
            evaluate("let a = []; append(a, 1); len(a)")
        );
        assert_eq!(
            Object::Integer(1),
            evaluate("let a = []; len(append(a, 2))")
        );
        assert_eq!(
            Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
            evaluate("let a = [1]; append(a, 2)")
        );
        assert_eq!(
            Object::Integer(2),
            evaluate("let a = []; let b = append(a, 2); b[0]")
        );
        assert_eq!(
            Object::Integer(3),
            evaluate("last(append(append(append([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(1),
            evaluate("head(append(append(append([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(3),
            evaluate("len(append(append(append([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(2),
            evaluate("head(tail(append(append(append([], 1), 2), 3)))")
        );
    }

    #[test]
    fn test_builtin_prepend() {
        assert_eq!(
            Object::Integer(0),
            evaluate("let a = []; prepend(a, 1); len(a)")
        );
        assert_eq!(
            Object::Integer(1),
            evaluate("let a = []; len(prepend(a, 2))")
        );
        assert_eq!(
            Object::Array(vec![Object::Integer(2), Object::Integer(1)]),
            evaluate("let a = [1]; prepend(a, 2)")
        );
        assert_eq!(
            Object::Integer(2),
            evaluate("let a = []; let b = prepend(a, 2); b[0]")
        );
        assert_eq!(
            Object::Integer(1),
            evaluate("last(prepend(prepend(prepend([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(3),
            evaluate("head(prepend(prepend(prepend([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(3),
            evaluate("len(prepend(prepend(prepend([], 1), 2), 3))")
        );
        assert_eq!(
            Object::Integer(2),
            evaluate("head(tail(prepend(prepend(prepend([], 1), 2), 3)))")
        );
    }

    #[test]
    fn test_map_function() {
        assert_eq!(
            Object::Array(vec![
                Object::Integer(2),
                Object::Integer(3),
                Object::Integer(4)
            ]),
            evaluate(
                "
          let map = fn(arr, f) {
            if (len(arr) == 0) {
                []
            } else {
                let h = head(arr);
                prepend(map(tail(arr), f), f(h));
            }
          };
          map([1, 2, 3], fn(x) { x + 1 })
        "
            )
        );
    }

    #[test]
    fn test_function_object() {
        let obj = evaluate("fn(x) { x + 2; };");
        let (mut params, body) = match obj {
            Object::Function {
                parameters, body, ..
            } => (parameters, body),
            other => panic!("not a function: {:?}", other),
        };

        assert_eq!(1, params.len());
        assert_eq!("x", &params.remove(0).value);

        assert_eq!("(x + 2)", format!("{}", body));
    }

    #[test]
    fn test_function_application() {
        assert_eq!(
            Object::Integer(5),
            evaluate("let identity = fn(x) { x; }; identity(5);")
        );
        assert_eq!(
            Object::Integer(5),
            evaluate("let identity = fn(x) { return x; }; identity(5);")
        );
        assert_eq!(
            Object::Integer(10),
            evaluate("let double = fn(x) { x * 2; }; double(5);")
        );
        assert_eq!(
            Object::Integer(10),
            evaluate("let add = fn(x, y) { x + y; }; add(5, 5);")
        );
        assert_eq!(
            Object::Integer(20),
            evaluate("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));")
        );
        assert_eq!(Object::Integer(5), evaluate("fn(x) { x; }(5)"));
        assert_eq!(
            Object::Integer(11),
            evaluate(
                "let counter = fn(x) {
                   if (x > 10) { return x; }
                   else { let foobar = 9999; counter(x + 1); }
                };
                counter(0);"
            )
        );
        assert_eq!(
            Object::String_("hell".into()),
            evaluate("fn(a, b) { a + b; }(\"he\", \"ll\")")
        );
    }

    #[test]
    fn test_closures() {
        assert_eq!(
            Object::Integer(4),
            evaluate(
                "let newAdder = fn(x) {
                    fn(y) { x + y};
                 };
                 let addTwo = newAdder(2);
                 addTwo(2); "
            )
        );
        assert_eq!(
            Object::Integer(4),
            evaluate(
                "let add = fn(a, b) { a + b };
                 let applyFunc = fn(a, b, func) { func(a, b) };
                 applyFunc(2, 2, add)"
            )
        );
    }

    #[test]
    fn test_error_messages() {
        assert_eq!(
            Err("type mismatch: INTEGER + BOOLEAN".into()),
            evaluate_res("5 + true;")
        );
        assert_eq!(
            Err("type mismatch: INTEGER + STRING".into()),
            evaluate_res("5 + \"a\";")
        );
        assert_eq!(
            Err("type mismatch: INTEGER + BOOLEAN".into()),
            evaluate_res("5 + true; 5;")
        );
        assert_eq!(
            Err("unknown operator: -BOOLEAN".into()),
            evaluate_res("-true")
        );
        assert_eq!(
            Err("unknown operator: BOOLEAN + BOOLEAN".into()),
            evaluate_res("true + false;")
        );
        assert_eq!(
            Err("unknown operator: BOOLEAN + BOOLEAN".into()),
            evaluate_res("5; true + false; 5")
        );
        assert_eq!(
            Err("unknown operator: BOOLEAN + BOOLEAN".into()),
            evaluate_res("if (10 > 1) { true + false; }")
        );
        assert_eq!(
            Err("unknown operator: BOOLEAN + BOOLEAN".into()),
            evaluate_res(" if (10 > 1) { if (10 > 1) { return true + false; } return 1; }")
        );
        assert_eq!(
            Err("unknown operator: STRING - STRING".into()),
            evaluate_res("\"a\" - \"b\"")
        );
        assert_eq!(
            Err("identifier not found: foobar".into()),
            evaluate_res("foobar")
        );
        assert_eq!(
            Err("identifier not found: x".into()),
            evaluate_res("let func = fn(x) { x }; func(2); x")
        );
        assert_eq!(
            Err("argument to 'len' not supported, got INTEGER".into()),
            evaluate_res("len(1)")
        );
        assert_eq!(
            Err("wrong number of arguments. got=2, want=1".into()),
            evaluate_res("len(\"a\", \"b\")")
        );
    }

    fn evaluate_res(input: &str) -> EvalResult {
        let p = *Parser::new(Lexer::new(input)).parse_program().unwrap();
        let env = Rc::new(RefCell::new(Environment::default()));
        eval(p, env)
    }

    fn evaluate(input: &str) -> Object {
        evaluate_res(input).unwrap()
    }
}
