use super::ast;
use super::environment::Environment;
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
