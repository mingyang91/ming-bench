use super::{eval, eval_args, Env, EvalError, Value};

pub(super) fn builtin_cons(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: "cons".to_string(),
            expected: "2".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    match &vals[1] {
        Value::List(tail, _) => {
            let mut new_list = vec![vals[0].clone()];
            new_list.extend(tail.iter().cloned());
            Ok(Value::List(new_list, (0, 0)))
        }
        _ => {
            // cons onto non-list creates a pair (represented as list for now)
            Ok(Value::List(vec![vals[0].clone(), vals[1].clone()], (0, 0)))
        }
    }
}

pub(super) fn builtin_car(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "car".to_string(),
            expected: "1".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    match &vals[0] {
        Value::List(items, _) if !items.is_empty() => Ok(items[0].clone()),
        Value::List(..) => Err(EvalError::TypeError {
            message: "car: empty list".to_string(),
            line: 0,
            col: 0,
        }),
        other => Err(EvalError::TypeError {
            message: format!("car: expected pair, got {}", other.type_name()),
            line: 0,
            col: 0,
        }),
    }
}

pub(super) fn builtin_cdr(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "cdr".to_string(),
            expected: "1".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    match &vals[0] {
        Value::List(items, _) if !items.is_empty() => {
            Ok(Value::List(items[1..].to_vec(), (0, 0)))
        }
        Value::List(..) => Err(EvalError::TypeError {
            message: "cdr: empty list".to_string(),
            line: 0,
            col: 0,
        }),
        other => Err(EvalError::TypeError {
            message: format!("cdr: expected pair, got {}", other.type_name()),
            line: 0,
            col: 0,
        }),
    }
}

pub(super) fn builtin_null(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "null?".to_string(),
            expected: "1".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    Ok(Value::Boolean(matches!(
        &vals[0],
        Value::List(items, _) if items.is_empty()
    )))
}

pub(super) fn builtin_list(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    Ok(Value::List(vals, (0, 0)))
}

pub(super) fn builtin_length(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "length".to_string(),
            expected: "1".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    match &vals[0] {
        Value::List(items, _) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            message: format!("length: expected list, got {}", other.type_name()),
            line: 0,
            col: 0,
        }),
    }
}

pub(super) fn builtin_append(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    let mut result = Vec::new();
    for (i, v) in vals.iter().enumerate() {
        match v {
            Value::List(items, _) => result.extend(items.iter().cloned()),
            other if i == vals.len() - 1 => {
                // Last arg can be non-list (improper list), but for now just error
                return Err(EvalError::TypeError {
                    message: format!("append: expected list, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                });
            }
            other => {
                return Err(EvalError::TypeError {
                    message: format!("append: expected list, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                });
            }
        }
    }
    Ok(Value::List(result, (0, 0)))
}

pub(super) fn builtin_type_pred(
    args: &[Value],
    env: &mut Env,
    pred: &str,
) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 1 {
        return Err(EvalError::Arity {
            procedure: pred.to_string(),
            expected: "1".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
        });
    }
    let result = match pred {
        "string?" => matches!(&vals[0], Value::Str(_)),
        "number?" => matches!(&vals[0], Value::Integer(_)),
        "boolean?" => matches!(&vals[0], Value::Boolean(_)),
        "pair?" => matches!(&vals[0], Value::List(items, _) if !items.is_empty()),
        "symbol?" => matches!(&vals[0], Value::Symbol(..)),
        other => unreachable!("unknown predicate: {other}"),
    };
    Ok(Value::Boolean(result))
}

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
            line: 0,
            col: 0,
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
            line: 0,
            col: 0,
        });
    }
    let a = vals[0].as_integer("/")?;
    let b = vals[1].as_integer("/")?;
    if b == 0 {
        return Err(EvalError::DivisionByZero { line: 0, col: 0 });
    }
    Ok(Value::Integer(a / b))
}

pub(super) fn builtin_cmp(
    args: &[Value],
    env: &mut Env,
    op: &str,
) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: op.to_string(),
            expected: "2".to_string(),
            got: vals.len(),
            line: 0,
            col: 0,
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
            line: 0,
            col: 0,
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
