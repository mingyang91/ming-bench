use super::{eval, eval_args, Env, EvalError, Value};

pub(super) fn builtin_cons(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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
            Ok(Value::List(vec![vals[0].clone(), vals[1].clone()], (0, 0)))
        }
    }
}

pub(super) fn builtin_car(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_cdr(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_null(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_list(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
    Ok(Value::List(vals, (0, 0)))
}

pub(super) fn builtin_length(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_append(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
    let mut result = Vec::new();
    for (i, v) in vals.iter().enumerate() {
        match v {
            Value::List(items, _) => result.extend(items.iter().cloned()),
            other if i == vals.len() - 1 => {
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
    output: &mut String,
) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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
        "char?" => matches!(&vals[0], Value::Char(_)),
        other => unreachable!("unknown predicate: {other}"),
    };
    Ok(Value::Boolean(result))
}

pub(super) fn builtin_add(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
    let mut sum: i64 = 0;
    for v in &vals {
        sum += v.as_integer("+")?;
    }
    Ok(Value::Integer(sum))
}

pub(super) fn builtin_sub(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_mul(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
    let mut product: i64 = 1;
    for v in &vals {
        product *= v.as_integer("*")?;
    }
    Ok(Value::Integer(product))
}

pub(super) fn builtin_div(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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
    output: &mut String,
) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
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

pub(super) fn builtin_not(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "not".to_string(),
            expected: "1".to_string(),
            got: args.len(),
            line: 0,
            col: 0,
        });
    }
    let val = eval(&args[0], env, output)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

pub(super) fn builtin_string_ops(
    args: &[Value],
    env: &mut Env,
    op: &str,
    output: &mut String,
) -> Result<Value, EvalError> {
    let vals = eval_args(args, env, output)?;
    match op {
        "string-append" => {
            let mut result = String::new();
            for v in &vals {
                match v {
                    Value::Str(s) => result.push_str(s),
                    other => return Err(EvalError::TypeError {
                        message: format!("string-append: expected string, got {}", other.type_name()),
                        line: 0,
                        col: 0,
                    }),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity {
                    procedure: "string-length".to_string(),
                    expected: "1".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                other => Err(EvalError::TypeError {
                    message: format!("string-length: expected string, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            }
        }
        "substring" => {
            if vals.len() != 3 {
                return Err(EvalError::Arity {
                    procedure: "substring".to_string(),
                    expected: "3".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            let s = match &vals[0] {
                Value::Str(s) => s,
                other => return Err(EvalError::TypeError {
                    message: format!("substring: expected string, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            };
            let start = vals[1].as_integer("substring")? as usize;
            let end = vals[2].as_integer("substring")? as usize;
            if start > s.len() || end > s.len() || start > end {
                return Err(EvalError::TypeError {
                    message: format!("substring: index out of range ({start}, {end}) for string of length {}", s.len()),
                    line: 0,
                    col: 0,
                });
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity {
                    procedure: "string->number".to_string(),
                    expected: "1".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            match &vals[0] {
                Value::Str(s) => {
                    match s.parse::<i64>() {
                        Ok(n) => Ok(Value::Integer(n)),
                        Err(_) => Ok(Value::Boolean(false)),
                    }
                }
                other => Err(EvalError::TypeError {
                    message: format!("string->number: expected string, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            }
        }
        "number->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity {
                    procedure: "number->string".to_string(),
                    expected: "1".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            let n = vals[0].as_integer("number->string")?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity {
                    procedure: "symbol->string".to_string(),
                    expected: "1".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            match &vals[0] {
                Value::Symbol(s, _) => Ok(Value::Str(s.clone())),
                other => Err(EvalError::TypeError {
                    message: format!("symbol->string: expected symbol, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            }
        }
        "string->symbol" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity {
                    procedure: "string->symbol".to_string(),
                    expected: "1".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            match &vals[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone(), (0, 0))),
                other => Err(EvalError::TypeError {
                    message: format!("string->symbol: expected string, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            }
        }
        "string-ref" => {
            if vals.len() != 2 {
                return Err(EvalError::Arity {
                    procedure: "string-ref".to_string(),
                    expected: "2".to_string(),
                    got: vals.len(),
                    line: 0,
                    col: 0,
                });
            }
            let s = match &vals[0] {
                Value::Str(s) => s,
                other => return Err(EvalError::TypeError {
                    message: format!("string-ref: expected string, got {}", other.type_name()),
                    line: 0,
                    col: 0,
                }),
            };
            let idx = vals[1].as_integer("string-ref")? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::TypeError {
                    message: format!("string-ref: index {idx} out of range for string of length {}", s.len()),
                    line: 0,
                    col: 0,
                }),
            }
        }
        other => unreachable!("unknown string op: {other}"),
    }
}
