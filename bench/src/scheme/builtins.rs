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
            | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "map"
            | "apply"
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
        "string-copy" => apply_string_copy(args, env, span, output),
        "string->list" => apply_string_to_list(args, env, span, output),
        "list->string" => apply_list_to_string(args, env, span, output),
        "char->integer" => apply_char_to_integer(args, env, span, output),
        "integer->char" => apply_integer_to_char(args, env, span, output),
        "map" => apply_map(args, env, span, output),
        "apply" => apply_apply(args, env, span, output),
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

fn apply_string_copy(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = eval_to_string(arg, env, span, output)?;
    Ok(Value::String(s))
}

fn apply_string_to_list(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = eval_to_string(arg, env, span, output)?;
    let items: Vec<Value> = s.chars().map(Value::Char).collect();
    Ok(Value::List(items, Span::default()))
}

fn apply_list_to_string(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let val = eval(arg, env, output)?;
    let Value::List(items, _) = val else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{val}"),
            span,
        });
    };
    let s: Result<String, EvalError> = items.iter().map(|item| match item {
        Value::Char(c) => Ok(*c),
        other => Err(EvalError::TypeError {
            expected: "char".to_string(),
            got: format!("{other}"),
            span,
        }),
    }).collect();
    Ok(Value::String(s?))
}

fn apply_char_to_integer(
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
        Value::Char(c) => Ok(Value::Integer(c as i64)),
        other => Err(EvalError::TypeError {
            expected: "char".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn apply_integer_to_char(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let n = eval_to_integer(arg, env, output)?;
    let c = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
        expected: "valid Unicode code point".to_string(),
        got: format!("{n}"),
        span,
    })?;
    Ok(Value::Char(c))
}

fn apply_map(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [proc_arg, list_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    let proc = eval(proc_arg, env, output)?;
    let list_val = eval(list_arg, env, output)?;
    let Value::List(items, _) = list_val else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{list_val}"),
            span,
        });
    };
    let results: Vec<Value> = items.iter().map(|item| {
        call_proc(&proc, std::slice::from_ref(item), env, span, output)
    }).collect::<Result<_, _>>()?;
    Ok(Value::List(results, Span::default()))
}

/// Call a procedure (lambda or builtin symbol) with already-evaluated arguments.
fn call_proc(
    proc: &Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match proc {
        Value::Symbol(name, _) if is_builtin(name) => {
            call_builtin_values(name, args, env, span, output)
        }
        _ => super::apply(proc.clone(), args, env, span, output),
    }
}

/// Call a builtin with already-evaluated Value arguments.
pub fn call_builtin_values(
    name: &str,
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => arithmetic_values(name, args, span),
        "<" | ">" | "=" | "<=" | ">=" => comparison_values(name, args, span),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        "cons" => cons_values(args, span),
        "car" => car_values(args, span),
        "cdr" => cdr_values(args, span),
        "null?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            Ok(Value::Boolean(matches!(arg, Value::List(items, _) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec(), Span::default())),
        "length" => length_values(args, span),
        "boolean?" | "number?" | "pair?" | "string?" | "symbol?" | "char?" => {
            type_pred_values(name, args, span)
        }
        "display" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            output.push_str(&arg.to_display_string());
            Ok(Value::Boolean(false))
        }
        "write" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
            };
            output.push_str(&format!("{arg}"));
            Ok(Value::Boolean(false))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 0, got: args.len(), span });
            }
            output.push('\n');
            Ok(Value::Boolean(false))
        }
        "map" => map_values(args, env, span, output),
        "apply" => apply_values(args, env, span, output),
        "string-append" => string_append_values(args, span),
        "string-length" => string_length_values(args, span),
        "substring" => substring_values(args, span),
        "string->number" => string_to_number_values(args, span),
        "number->string" => number_to_string_values(args, span),
        "string->symbol" => string_to_symbol_values(args, span),
        "symbol->string" => symbol_to_string_values(args, span),
        "string-ref" => string_ref_values(args, span),
        "string-copy" => string_copy_values(args, span),
        "string->list" => string_to_list_values(args, span),
        "list->string" => list_to_string_values(args, span),
        "char->integer" => char_to_integer_values(args, span),
        "integer->char" => integer_to_char_values(args, span),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn to_integer(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn to_string_val(val: &Value, span: Span) -> Result<&str, EvalError> {
    match val {
        Value::String(s) => Ok(s.as_str()),
        other => Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn arithmetic_values(op: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    let evaluated: Vec<i64> = args
        .iter()
        .map(|a| to_integer(a, span))
        .collect::<Result<_, _>>()?;
    let result = match op {
        "+" => evaluated.iter().sum(),
        "*" => evaluated.iter().product(),
        "-" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0, span });
            };
            if rest.is_empty() { -first } else { rest.iter().fold(*first, |acc, &x| acc - x) }
        }
        "/" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0, span });
            };
            rest.iter().try_fold(*first, |acc, &x| checked_div(acc, x, span))?
        }
        _ => unreachable!(),
    };
    Ok(Value::Integer(result))
}

fn comparison_values(op: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    let a = to_integer(left, span)?;
    let b = to_integer(right, span)?;
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!(),
    };
    Ok(Value::Boolean(result))
}

fn cons_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    match tail {
        Value::List(items, list_span) => {
            let mut new_items = vec![head.clone()];
            new_items.extend_from_slice(items);
            Ok(Value::List(new_items, *list_span))
        }
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn car_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    match arg {
        Value::List(items, _) if !items.is_empty() => Ok(items[0].clone()),
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

fn cdr_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    match arg {
        Value::List(items, list_span) if !items.is_empty() => {
            Ok(Value::List(items[1..].to_vec(), *list_span))
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

fn length_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    match arg {
        Value::List(items, _) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn type_pred_values(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let result = match name {
        "boolean?" => matches!(arg, Value::Boolean(_)),
        "number?" => matches!(arg, Value::Integer(_)),
        "pair?" => matches!(arg, Value::List(items, _) if !items.is_empty()),
        "string?" => matches!(arg, Value::String(_)),
        "symbol?" => matches!(arg, Value::Symbol(_, _)),
        "char?" => matches!(arg, Value::Char(_)),
        _ => unreachable!(),
    };
    Ok(Value::Boolean(result))
}

fn map_values(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [proc, list_val] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    let Value::List(items, _) = list_val else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{list_val}"),
            span,
        });
    };
    let results: Vec<Value> = items.iter().map(|item| {
        call_proc(proc, std::slice::from_ref(item), env, span, output)
    }).collect::<Result<_, _>>()?;
    Ok(Value::List(results, Span::default()))
}

fn apply_values(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    }
    let proc = &args[0];
    let last = &args[args.len() - 1];
    let Value::List(tail_args, _) = last else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{last}"),
            span,
        });
    };
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend_from_slice(tail_args);
    call_proc(proc, &all_args, env, span, output)
}

fn string_append_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let result: String = args
        .iter()
        .map(|a| to_string_val(a, span).map(str::to_owned))
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

fn string_length_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = to_string_val(arg, span)?;
    Ok(Value::Integer(s.len() as i64))
}

fn substring_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [s_arg, start_arg, end_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len(), span });
    };
    let s = to_string_val(s_arg, span)?;
    let start = to_integer(start_arg, span)? as usize;
    let end = to_integer(end_arg, span)? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::TypeError {
            expected: format!("valid indices for string of length {}", s.len()),
            got: format!("start={start}, end={end}"),
            span,
        });
    }
    Ok(Value::String(s[start..end].to_string()))
}

fn string_to_number_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = to_string_val(arg, span)?;
    let n = s.parse::<i64>().map_err(|_| EvalError::TypeError {
        expected: "numeric string".to_string(),
        got: format!("\"{s}\""),
        span,
    })?;
    Ok(Value::Integer(n))
}

fn number_to_string_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let n = to_integer(arg, span)?;
    Ok(Value::String(n.to_string()))
}

fn string_to_symbol_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = to_string_val(arg, span)?;
    Ok(Value::Symbol(s.to_owned(), span))
}

fn symbol_to_string_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    match arg {
        Value::Symbol(s, _) => Ok(Value::String(s.clone())),
        other => Err(EvalError::TypeError {
            expected: "symbol".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn string_ref_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [s_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    };
    let s = to_string_val(s_arg, span)?;
    let idx = to_integer(idx_arg, span)? as usize;
    let ch = s.chars().nth(idx).ok_or_else(|| EvalError::TypeError {
        expected: format!("index < {}", s.len()),
        got: format!("{idx}"),
        span,
    })?;
    Ok(Value::Char(ch))
}

fn string_copy_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = to_string_val(arg, span)?;
    Ok(Value::String(s.to_owned()))
}

fn string_to_list_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let s = to_string_val(arg, span)?;
    let items: Vec<Value> = s.chars().map(Value::Char).collect();
    Ok(Value::List(items, Span::default()))
}

fn list_to_string_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let Value::List(items, _) = arg else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{arg}"),
            span,
        });
    };
    let s: Result<String, EvalError> = items.iter().map(|item| match item {
        Value::Char(c) => Ok(*c),
        other => Err(EvalError::TypeError {
            expected: "char".to_string(),
            got: format!("{other}"),
            span,
        }),
    }).collect();
    Ok(Value::String(s?))
}

fn char_to_integer_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    match arg {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        other => Err(EvalError::TypeError {
            expected: "char".to_string(),
            got: format!("{other}"),
            span,
        }),
    }
}

fn integer_to_char_values(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len(), span });
    };
    let n = to_integer(arg, span)?;
    let c = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
        expected: "valid Unicode code point".to_string(),
        got: format!("{n}"),
        span,
    })?;
    Ok(Value::Char(c))
}

fn apply_apply(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len(), span });
    }
    let proc = eval(&args[0], env, output)?;
    let prefix: Vec<Value> = args[1..args.len() - 1]
        .iter()
        .map(|a| eval(a, env, output))
        .collect::<Result<_, _>>()?;
    let last = eval(&args[args.len() - 1], env, output)?;
    let Value::List(tail_args, _) = last else {
        return Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{last}"),
            span,
        });
    };
    let mut all_args: Vec<Value> = prefix;
    all_args.extend(tail_args);
    call_proc(&proc, &all_args, env, span, output)
}
