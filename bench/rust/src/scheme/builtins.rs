use super::{eval, eval_args, Env, EvalError, Value};

pub(super) fn builtin_add(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    let mut sum: i64 = 0;
    for v in &vals {
        sum += v.as_integer("+")?;
    }
    Ok(Value::Integer(sum))
}

pub(super) fn builtin_sub(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.is_empty() {
        return Err(EvalError::Arity {
            procedure: "-".to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        });
    }
    if vals.len() == 1 {
        return Ok(Value::Integer(-vals[0].as_integer("-")?));
    }
    let mut result = vals[0].as_integer("-")?;
    for v in &vals[1..] {
        result -= v.as_integer("-")?;
    }
    Ok(Value::Integer(result))
}

pub(super) fn builtin_mul(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    let mut product: i64 = 1;
    for v in &vals {
        product *= v.as_integer("*")?;
    }
    Ok(Value::Integer(product))
}

pub(super) fn builtin_div(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: "/".to_string(),
            expected: "2".to_string(),
            got: vals.len(),
        });
    }
    let a = vals[0].as_integer("/")?;
    let b = vals[1].as_integer("/")?;
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(a / b))
}

pub(super) fn builtin_cmp(args: &[Value], env: &mut Env, op: &str) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: op.to_string(),
            expected: "2".to_string(),
            got: vals.len(),
        });
    }
    let a = vals[0].as_integer(op)?;
    let b = vals[1].as_integer(op)?;
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        other => unreachable!("unknown comparison operator: {other}"),
    };
    Ok(Value::Boolean(result))
}

pub(super) fn builtin_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "not".to_string(),
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

pub(super) fn builtin_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(super) fn builtin_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}
