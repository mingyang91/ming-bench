use super::{eval, eval_to_integer, is_truthy, Env};
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};

pub fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "boolean?" | "number?" | "pair?" | "string?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "string->symbol" | "symbol->string"
            | "string-ref"
    )
}

pub fn apply_builtin(
    name: &str,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args, env, span, output),
        "<" | ">" | "=" | "<=" | ">=" => apply_comparison(name, args, env, span, output),
        "not" => apply_not(args, env, span, output),
        "cons" => apply_cons(args, env, span, output),
        "car" => apply_car(args, env, span, output),
        "cdr" => apply_cdr(args, env, span, output),
        "null?" => apply_null(args, env, span, output),
        "list" => apply_list(args, env, output),
        "length" => apply_length(args, env, span, output),
        "boolean?" | "number?" | "pair?" | "string?" | "symbol?" | "char?" => {
            apply_type_pred(name, args, env, span, output)
        }
        "string-append" => apply_string_append(args, env, span, output),
        "string-length" => apply_string_length(args, env, span, output),
        "substring" => apply_substring(args, env, span, output),
        "string->number" => apply_string_to_number(args, env, span, output),
        "number->string" => apply_number_to_string(args, env, span, output),
        "string->symbol" => apply_string_to_symbol(args, env, span, output),
        "symbol->string" => apply_symbol_to_string(args, env, span, output),
        "string-ref" => apply_string_ref(args, env, span, output),
        "display" => apply_display(args, env, span, output),
        "write" => apply_write(args, env, span, output),
        "newline" => apply_newline(args, span, output),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn checked_div(acc: i64, x: i64, span: Span) -> Result<i64, EvalError> {
    if x == 0 {
        Err(EvalError::DivisionByZero { span })
    } else {
        Ok(acc / x)
    }
}

fn apply_arithmetic(
    op: &str,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let evaluated: Vec<i64> = args
        .iter()
        .map(|a| eval_to_integer(a, env, output))
        .collect::<Result<_, _>>()?;

    let result = match op {
        "+" => evaluated.iter().sum(),
        "*" => evaluated.iter().product(),
        "-" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                    span,
                });
            };
            if rest.is_empty() {
                -first
            } else {
                rest.iter().fold(*first, |acc, &x| acc - x)
            }
        }
        "/" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                    span,
                });
            };
            rest.iter()
                .try_fold(*first, |acc, &x| checked_div(acc, x, span))?
        }
        _ => unreachable!("apply_arithmetic called with non-arithmetic op"),
    };

    Ok(Value::Integer(result))
}

fn apply_not(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    Ok(Value::Boolean(!is_truthy(&val)))
}

fn apply_cons(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    };
    let head_val = eval(head, env, output)?;
    let tail_val = eval(tail, env, output)?;
    match tail_val {
        Value::List(mut items, list_span) => {
            items.insert(0, head_val);
            Ok(Value::List(items, list_span))
        }
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_car(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    match val {
        Value::List(items, _) if !items.is_empty() => {
            Ok(items.into_iter().next().expect("non-empty list"))
        }
        Value::List(_, _) => Err(EvalError::TypeError {
            expected: "non-empty list".to_string(),
            got: "()".to_string(),
            span,
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_cdr(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    match val {
        Value::List(items, list_span) if !items.is_empty() => {
            Ok(Value::List(items[1..].to_vec(), list_span))
        }
        Value::List(_, _) => Err(EvalError::TypeError {
            expected: "non-empty list".to_string(),
            got: "()".to_string(),
            span,
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_null(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    Ok(Value::Boolean(matches!(
        val,
        Value::List(ref items, _) if items.is_empty()
    )))
}

fn apply_list(args: &[Value], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, output))
        .collect::<Result<_, _>>()?;
    Ok(Value::List(items, Span::default()))
}

fn apply_length(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    match val {
        Value::List(items, _) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_type_pred(
    name: &str,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    let result = match name {
        "boolean?" => matches!(val, Value::Boolean(_)),
        "number?" => matches!(val, Value::Integer(_)),
        "pair?" => matches!(val, Value::List(ref items, _) if !items.is_empty()),
        "string?" => matches!(val, Value::String(_)),
        "symbol?" => matches!(val, Value::Symbol(_, _)),
        "char?" => matches!(val, Value::Char(_)),
        _ => unreachable!("apply_type_pred called with unknown predicate"),
    };
    Ok(Value::Boolean(result))
}

fn apply_comparison(
    op: &str,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    };
    let a = eval_to_integer(left, env, output)?;
    let b = eval_to_integer(right, env, output)?;

    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!("apply_comparison called with non-comparison op"),
    };
    Ok(Value::Boolean(result))
}

fn apply_display(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    output.push_str(&val.to_display_string());
    Ok(Value::Boolean(false))
}

fn apply_write(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    let val = eval(arg, env, output)?;
    output.push_str(&format!("{val}"));
    Ok(Value::Boolean(false))
}

fn apply_newline(args: &[Value], span: Span, output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
            span,
        });
    }
    output.push('\n');
    Ok(Value::Boolean(false))
}

fn eval_to_string(val: &Value, env: &mut Env, span: Span, output: &mut String) -> Result<String, EvalError> {
    match eval(val, env, output)? {
        Value::String(s) => Ok(s),
        other => Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_string_append(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let result: String = args
        .iter()
        .map(|a| eval_to_string(a, env, span, output))
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

fn apply_string_length(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = eval_to_string(arg, env, span, output)?;
    Ok(Value::Integer(s.len() as i64))
}

fn apply_substring(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [s_arg, start_arg, end_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len(), span });
    };
    let s = eval_to_string(s_arg, env, span, output)?;
    let start = eval_to_integer(start_arg, env, output)? as usize;
    let end = eval_to_integer(end_arg, env, output)? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::TypeError {
            expected: format!("valid indices for string of length {}", s.len()),
            got: format!("start={start}, end={end}"),
            span,
        });
    }
    Ok(Value::String(s[start..end].to_string()))
}

fn apply_string_to_number(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = eval_to_string(arg, env, span, output)?;
    let n = s.parse::<i64>().map_err(|_| EvalError::TypeError {
        expected: "numeric string".to_string(),
        got: format!("\"{s}\""),
        span,
    })?;
    Ok(Value::Integer(n))
}

fn apply_number_to_string(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let n = eval_to_integer(arg, env, output)?;
    Ok(Value::String(n.to_string()))
}

fn apply_string_to_symbol(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = eval_to_string(arg, env, span, output)?;
    Ok(Value::Symbol(s, span))
}

fn apply_symbol_to_string(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let val = eval(arg, env, output)?;
    match val {
        Value::Symbol(s, _) => Ok(Value::String(s)),
        other => Err(EvalError::TypeError {
            expected: "symbol".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_string_ref(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [s_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    let s = eval_to_string(s_arg, env, span, output)?;
    let idx = eval_to_integer(idx_arg, env, output)? as usize;
    let ch = s.chars().nth(idx).ok_or_else(|| EvalError::TypeError {
        expected: format!("index < {}", s.len()),
        got: format!("{idx}"),
        span,
    })?;
    Ok(Value::Char(ch))
}
