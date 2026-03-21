use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Shared output buffer for display/write/newline.
pub type Output = Rc<RefCell<String>>;

/// Evaluate a single parsed expression in the given environment.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Void => {
            Ok(expr.clone())
        }
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env
            .borrow()
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
            }),
        Value::List(items) => eval_list(items, env, out),
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol"
            | "string-ref"
    )
}

fn eval_list(items: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Ok(Value::List(vec![]));
    }

    if let Value::Symbol(name) = &items[0] {
        match name.as_str() {
            "define" => return eval_define(&items[1..], env, out),
            "if" => return eval_if(&items[1..], env, out),
            "quote" => return eval_quote(&items[1..]),
            "lambda" => return eval_lambda(&items[1..], env),
            "let" => return eval_let(&items[1..], env, out),
            "begin" => return eval_body(&items[1..], env, out),
            "cond" => return eval_cond(&items[1..], env, out),
            "and" => return eval_and(&items[1..], env, out),
            "or" => return eval_or(&items[1..], env, out),
            s if is_builtin(s) => return eval_builtin(s, &items[1..], env, out),
            _ => {}
        }
    }

    let proc = eval(&items[0], env, out)?;
    let args = &items[1..];
    call_proc(&proc, args, env, out)
}

fn call_proc(
    proc: &Value,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda {
            params,
            body,
            closure,
        } => {
            let eval_args: Vec<Value> = args
                .iter()
                .map(|a| eval(a, env, out))
                .collect::<Result<_, _>>()?;
            if eval_args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: eval_args.len(),
                });
            }
            let child = Env::extend(closure);
            for (param, val) in params.iter().zip(eval_args) {
                child.borrow_mut().define(param.clone(), val);
            }
            eval_body(body, &child, out)
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args, env, out),
        "-" => eval_sub(args, env, out),
        "*" => eval_mul(args, env, out),
        "/" => eval_div(args, env, out),
        "<" => eval_cmp(args, env, out, |a, b| a < b),
        ">" => eval_cmp(args, env, out, |a, b| a > b),
        "=" => eval_cmp(args, env, out, |a, b| a == b),
        "<=" => eval_cmp(args, env, out, |a, b| a <= b),
        ">=" => eval_cmp(args, env, out, |a, b| a >= b),
        "not" => eval_not(args, env, out),
        "cons" => eval_cons(args, env, out),
        "car" => eval_car(args, env, out),
        "cdr" => eval_cdr(args, env, out),
        "null?" => eval_null_pred(args, env, out),
        "list" => eval_list_builtin(args, env, out),
        "length" => eval_length(args, env, out),
        "string?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Str(_))),
        "number?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Integer(_))),
        "boolean?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Boolean(_))),
        "pair?" => eval_type_pred(args, env, out, |v| {
            matches!(v, Value::List(items) if !items.is_empty())
        }),
        "symbol?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Symbol(_))),
        "char?" => eval_type_pred(args, env, out, |v| matches!(v, Value::Char(_))),
        "display" => eval_display(args, env, out),
        "write" => eval_write(args, env, out),
        "newline" => eval_newline(args, out),
        "string-append" => eval_string_append(args, env, out),
        "string-length" => eval_string_length(args, env, out),
        "substring" => eval_substring(args, env, out),
        "string->number" => eval_string_to_number(args, env, out),
        "number->string" => eval_number_to_string(args, env, out),
        "symbol->string" => eval_symbol_to_string(args, env, out),
        "string->symbol" => eval_string_to_symbol(args, env, out),
        "string-ref" => eval_string_ref(args, env, out),
        _ => Err(EvalError::UnknownProcedure {
            name: name.into(),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last.
fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in body {
        result = eval(expr, env, out)?;
    }
    Ok(result)
}

fn eval_define(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    match args {
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env, out)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        [Value::List(sig), body @ ..] if !sig.is_empty() => {
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::Parse {
                    message: "define: expected function name".into(),
                });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        message: format!("define: expected parameter name, got {other}"),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                closure: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse {
            message: "define: bad syntax".into(),
        }),
    }
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let (cond, then, els) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if: expected 2 or 3 arguments".into(),
            })
        }
    };
    let cond_val = eval(cond, env, out)?;
    if cond_val != Value::Boolean(false) {
        eval(then, env, out)
    } else {
        match els {
            Some(e) => eval(e, env, out),
            None => Ok(Value::Void),
        }
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::Parse {
            message: "quote: expected exactly 1 argument".into(),
        });
    };
    Ok(datum.clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "lambda: expected parameters and body".into(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Parse {
            message: "lambda: expected parameter list".into(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                message: format!("lambda: expected parameter name, got {other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        closure: Rc::clone(env),
    })
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, out)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, out)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_args_as_integers(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| {
            let val = eval(a, env, out)?;
            match val {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{other}"),
                }),
            }
        })
        .collect()
}

fn eval_add(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn eval_sub(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-single)),
        [first, rest @ ..] => Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n))),
    }
}

fn eval_mul(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn eval_div(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(*first, |acc, n| {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(acc / n)
                }
            })
            .map(Value::Integer),
    }
}

fn eval_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env, out)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    }
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Boolean(result))
}

fn eval_cons(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let head_val = eval(head, env, out)?;
    let tail_val = eval(tail, env, out)?;
    match tail_val {
        Value::List(mut items) => {
            items.insert(0, head_val);
            Ok(Value::List(items))
        }
        _ => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{tail_val}"),
        }),
    }
}

fn eval_car(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::List(items) if !items.is_empty() => Ok(items.into_iter().next().expect("non-empty")),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_cdr(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_null_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(
        matches!(val, Value::List(ref items) if items.is_empty()),
    ))
}

fn eval_list_builtin(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env, out))
        .collect::<Result<_, _>>()?;
    Ok(Value::List(items))
}

fn eval_length(args: &[Value], env: &Rc<RefCell<Env>>, out: &Output) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_type_pred(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
    pred: fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(pred(&val)))
}

fn eval_let(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "let: expected bindings and body".into(),
        });
    }
    let Value::List(bindings) = &args[0] else {
        return Err(EvalError::Parse {
            message: "let: expected binding list".into(),
        });
    };
    let child = Env::extend(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::Parse {
                message: "let: expected binding pair".into(),
            });
        };
        let [Value::Symbol(name), expr] = pair.as_slice() else {
            return Err(EvalError::Parse {
                message: "let: expected (name expr)".into(),
            });
        };
        let val = eval(expr, env, out)?;
        child.borrow_mut().define(name.clone(), val);
    }
    eval_body(&args[1..], &child, out)
}

fn eval_cond(
    clauses: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    for clause in clauses {
        let Value::List(parts) = clause else {
            return Err(EvalError::Parse {
                message: "cond: expected clause".into(),
            });
        };
        if parts.is_empty() {
            return Err(EvalError::Parse {
                message: "cond: empty clause".into(),
            });
        }
        if matches!(&parts[0], Value::Symbol(s) if s == "else") {
            return eval_body(&parts[1..], env, out);
        }
        let test_val = eval(&parts[0], env, out)?;
        if test_val != Value::Boolean(false) {
            return eval_body(&parts[1..], env, out);
        }
    }
    Ok(Value::Void)
}

// --- L05: Output builtins ---

fn eval_display(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.borrow_mut().push_str(&val.display_value());
    Ok(Value::Void)
}

fn eval_write(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.borrow_mut().push_str(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], out: &Output) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
        });
    }
    out.borrow_mut().push('\n');
    Ok(Value::Void)
}

// --- L05: String/symbol operations ---

fn eval_string_append(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let mut result = String::new();
    for arg in args {
        match eval(arg, env, out)? {
            Value::Str(s) => result.push_str(&s),
            other => {
                return Err(EvalError::TypeError {
                    expected: "string".into(),
                    got: format!("{other}"),
                })
            }
        }
    }
    Ok(Value::Str(result))
}

fn eval_string_length(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_substring(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [s_arg, start_arg, end_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let s = match eval(s_arg, env, out)? {
        Value::Str(s) => s,
        other => {
            return Err(EvalError::TypeError {
                expected: "string".into(),
                got: format!("{other}"),
            })
        }
    };
    let start = match eval(start_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    let end = match eval(end_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    Ok(Value::Str(s[start..end].to_string()))
}

fn eval_string_to_number(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_number_to_string(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Integer(n) => Ok(Value::Str(n.to_string())),
        other => Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_symbol_to_string(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Symbol(s) => Ok(Value::Str(s)),
        other => Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_string_to_symbol(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match eval(arg, env, out)? {
        Value::Str(s) => Ok(Value::Symbol(s)),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_string_ref(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    out: &Output,
) -> Result<Value, EvalError> {
    let [s_arg, idx_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let s = match eval(s_arg, env, out)? {
        Value::Str(s) => s,
        other => {
            return Err(EvalError::TypeError {
                expected: "string".into(),
                got: format!("{other}"),
            })
        }
    };
    let idx = match eval(idx_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
    };
    let ch = s
        .chars()
        .nth(idx)
        .ok_or_else(|| EvalError::TypeError {
            expected: format!("index < {}", s.len()),
            got: format!("{idx}"),
        })?;
    Ok(Value::Char(ch))
}
