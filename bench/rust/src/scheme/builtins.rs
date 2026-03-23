use crate::scheme::error::EvalError;
use crate::scheme::Value;

pub(crate) fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
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

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "not requires exactly one argument".to_string(),
        });
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}
