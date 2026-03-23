use crate::scheme::error::EvalError;
use crate::scheme::Value;

pub(crate) fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
            | "append"
    )
}

pub(crate) fn call_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => builtin_add(args),
        "-" => builtin_sub(args),
        "*" => builtin_mul(args),
        "/" => builtin_div(args),
        "<" => builtin_cmp(args, "<", |a, b| a < b),
        ">" => builtin_cmp(args, ">", |a, b| a > b),
        "=" => builtin_cmp(args, "=", |a, b| a == b),
        "<=" => builtin_cmp(args, "<=", |a, b| a <= b),
        ">=" => builtin_cmp(args, ">=", |a, b| a >= b),
        "not" => builtin_not(args),
        "cons" => builtin_cons(args),
        "car" => builtin_car(args),
        "cdr" => builtin_cdr(args),
        "null?" => builtin_null(args),
        "list" => Ok(Value::List(args.to_vec())),
        "length" => builtin_length(args),
        "string?" => Ok(Value::Boolean(matches!(args, [Value::SchemeString(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "pair?" => Ok(Value::Boolean(matches!(args, [Value::List(v)] if !v.is_empty()))),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        "append" => builtin_append(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += arg.as_integer("+")?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "- requires at least one argument".to_string(),
        });
    }
    if args.len() == 1 {
        return Ok(Value::Integer(-args[0].as_integer("-")?));
    }
    let mut result = args[0].as_integer("-")?;
    for arg in &args[1..] {
        result -= arg.as_integer("-")?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= arg.as_integer("*")?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "/ requires at least one argument".to_string(),
        });
    }
    if args.len() == 1 {
        let divisor = args[0].as_integer("/")?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / divisor));
    }
    let mut result = args[0].as_integer("/")?;
    for arg in &args[1..] {
        let divisor = arg.as_integer("/")?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn builtin_cmp(args: &[Value], name: &str, op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            message: format!("{name} requires at least two arguments"),
        });
    }
    let mut prev = args[0].as_integer(name)?;
    for arg in &args[1..] {
        let curr = arg.as_integer(name)?;
        if !op(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity {
            message: "cons requires exactly 2 arguments".to_string(),
        });
    }
    match &args[1] {
        Value::List(tail) => {
            let mut new_list = vec![args[0].clone()];
            new_list.extend(tail.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => {
            // Dotted pair — for now just make a 2-element list
            Ok(Value::List(vec![args[0].clone(), args[1].clone()]))
        }
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "car requires exactly 1 argument".to_string(),
        });
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        _ => Err(EvalError::Type {
            message: format!("car: expected pair, got {}", args[0].display()),
        }),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "cdr requires exactly 1 argument".to_string(),
        });
    }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        _ => Err(EvalError::Type {
            message: format!("cdr: expected pair, got {}", args[0].display()),
        }),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "null? requires exactly 1 argument".to_string(),
        });
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(v) if v.is_empty())))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "length requires exactly 1 argument".to_string(),
        });
    }
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type {
            message: format!("length: expected list, got {}", args[0].display()),
        }),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Value::List(items) => result.extend(items.iter().cloned()),
            _ => {
                return Err(EvalError::Type {
                    message: format!("append: expected list, got {}", arg.display()),
                })
            }
        }
    }
    Ok(Value::List(result))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "not requires exactly one argument".to_string(),
        });
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}
