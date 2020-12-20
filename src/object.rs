use super::ast;
use super::environment::Environment;
use super::vm::code::Instructions;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String_(String),
    Array(Vec<Object>),
    // TODO use HashMap and constrain keys to simpler types
    Map(Vec<(Object, Object)>),
    Null,
    Return(Box<Object>),
    Function {
        parameters: Vec<ast::Identifier>,
        body: Box<ast::Statement>,
        env: Rc<RefCell<Environment>>,
    },
    CompiledFunction {
        instructions: Instructions,
        num_locals: u8,
        num_parameters: u8,
    },
    Builtin {
        func: fn(Vec<Object>) -> Result<Object, String>,
    },
}

pub struct ObjectType(String);

const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const STRING_OBJ: &str = "STRING";
const NULL_OBJ: &str = "NULL";
const RETURN_OBJ: &str = "RETURN";
const FUNCTION_OBJ: &str = "FUNCTION";
const COMPILED_FUNCTION_OBJ: &str = "COMPILED_FUNCTION";
const BUILTIN_OBJ: &str = "BUILTIN";
const ARRAY_OBJ: &str = "ARRAY";
const MAP_OBJ: &str = "MAP";

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

impl Object {
    pub fn tpe(&self) -> ObjectType {
        use Object::*;
        ObjectType(match self {
            Integer(_) => INTEGER_OBJ.into(),
            Boolean(_) => BOOLEAN_OBJ.into(),
            String_(_) => STRING_OBJ.into(),
            Array(_) => ARRAY_OBJ.into(),
            Map(_) => MAP_OBJ.into(),
            Null => NULL_OBJ.into(),
            Return(_) => RETURN_OBJ.into(),
            Function { .. } => FUNCTION_OBJ.into(),
            CompiledFunction { .. } => COMPILED_FUNCTION_OBJ.into(),
            Builtin { .. } => BUILTIN_OBJ.into(),
        })
    }

    pub fn inspect(&self) -> String {
        use Object::*;
        match self {
            Integer(v) => format!("{}", v),
            Boolean(v) => format!("{}", v),
            String_(v) => v.to_string(),
            Array(vs) => {
                let mut out = String::new();
                out.push('[');
                let mut first = true;
                for v in vs {
                    if first {
                        first = false;
                    } else {
                        out.push_str(", ");
                    }
                    out.push_str(&v.inspect());
                }
                out.push(']');
                out
            }
            Map(vs) => {
                let mut out = String::new();
                out.push('{');
                let mut first = true;
                for (k, v) in vs {
                    if first {
                        first = false;
                    } else {
                        out.push_str(", ");
                    }
                    out.push_str(&format!("{}: {}", k.inspect(), v.inspect()));
                }
                out.push('}');
                out
            }
            Return(v) => v.inspect(),
            Null => String::from("null"),
            Builtin { .. } => "builtin".into(),
            CompiledFunction {
                num_locals,
                num_parameters,
                ..
            } => {
                format!("CompiledFunction({}/{})[..]", num_parameters, num_locals)
            }
            Function {
                parameters, body, ..
            } => {
                let mut out = String::new();
                out.push_str("fn(");
                let mut first = true;
                for p in parameters {
                    if first {
                        first = false;
                    } else {
                        out.push_str(", ");
                    }
                    out.push_str(&p.value);
                }
                out.push_str(") {\n");
                out.push_str(&format!("{}", body));
                out.push_str("\n}");
                out
            }
        }
    }

    pub fn boolean(b: bool) -> Object {
        match b {
            true => TRUE,
            false => FALSE,
        }
    }
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.0)
    }
}

pub fn lookup_builtin(ident: &ast::Identifier) -> Option<Object> {
    match ident.value.as_ref() {
        "puts" => Some(Object::Builtin { func: builtin_puts }),
        "len" => Some(Object::Builtin { func: builtin_len }),
        "head" => Some(Object::Builtin { func: builtin_head }),
        "tail" => Some(Object::Builtin { func: builtin_tail }),
        "last" => Some(Object::Builtin { func: builtin_last }),
        "append" => Some(Object::Builtin {
            func: builtin_append,
        }),
        "prepend" => Some(Object::Builtin {
            func: builtin_prepend,
        }),
        _ => None,
    }
}

fn builtin_puts(args: Vec<Object>) -> Result<Object, String> {
    for arg in args {
        println!("{}", arg.inspect());
    }
    Ok(NULL)
}

fn builtin_len(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 1)?;
    match args.remove(0) {
        Object::String_(string) => Ok(Object::Integer(string.len() as i64)),
        Object::Array(xs) => Ok(Object::Integer(xs.len() as i64)),
        other => Err(format!(
            "argument to 'len' not supported, got {}",
            other.tpe()
        )),
    }
}

fn builtin_head(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 1)?;
    match args.remove(0) {
        Object::Array(xs) if xs.is_empty() => Err("Called 'head' on empty array".into()),
        Object::Array(xs) => Ok(xs[0].clone()),
        other => Err(format!(
            "argument to 'head' not supported, got {}",
            other.tpe()
        )),
    }
}

fn builtin_last(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 1)?;
    match args.remove(0) {
        Object::Array(xs) if xs.is_empty() => Err("Called 'last' on empty array".into()),
        Object::Array(xs) => Ok(xs[xs.len() - 1].clone()),
        other => Err(format!(
            "argument to 'last' not supported, got {}",
            other.tpe()
        )),
    }
}

fn builtin_tail(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 1)?;
    match args.remove(0) {
        Object::Array(xs) if xs.is_empty() => Err("Called 'tail' on empty array".into()),
        Object::Array(xs) => {
            let (_, tail) = xs.split_at(1);
            Ok(Object::Array(tail.to_vec()))
        }
        other => Err(format!(
            "argument to 'tail' not supported, got {}",
            other.tpe()
        )),
    }
}

fn builtin_append(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 2)?;
    match (args.remove(0), args.remove(0)) {
        (Object::Array(mut xs), obj) => {
            xs.push(obj);
            Ok(Object::Array(xs))
        }
        (other, _) => Err(format!(
            "first argument to 'append' must be array, but got {}",
            other.tpe()
        )),
    }
}

fn builtin_prepend(mut args: Vec<Object>) -> Result<Object, String> {
    ensure_arg_len(&args, 2)?;
    match (args.remove(0), args.remove(0)) {
        (Object::Array(mut xs), obj) => {
            xs.insert(0, obj);
            Ok(Object::Array(xs))
        }
        (other, _) => Err(format!(
            "first argument to 'prepend' must be array, but got {}",
            other.tpe()
        )),
    }
}

fn ensure_arg_len<T>(args: &[T], expected: usize) -> Result<(), String> {
    if args.len() != expected {
        Err(format!(
            "wrong number of arguments. got={}, want={}",
            args.len(),
            expected
        ))
    } else {
        Ok(())
    }
}
