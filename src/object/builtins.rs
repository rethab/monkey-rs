use super::*;

pub fn builtins() -> Vec<(u8, &'static str)> {
    vec![
        (0, "len"),
        (1, "puts"),
        (2, "head"),
        (3, "last"),
        (4, "tail"),
        (5, "append"),
        (6, "prepend"),
    ]
}

pub fn lookup_builtin_by_idx(idx: u8) -> Option<Object> {
    let (_, name) = builtins().into_iter().find(|(i, _)| *i == idx)?;
    lookup_builtin_by_name(name)
}

pub fn lookup_builtin(ident: &ast::Identifier) -> Option<Object> {
    lookup_builtin_by_name(&ident.value)
}

fn lookup_builtin_by_name(name: &str) -> Option<Object> {
    match name {
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
