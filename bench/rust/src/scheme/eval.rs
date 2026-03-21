use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Trampoline result: either a final value or a tail-call continuation.
enum Bounce {
    Done(Value),
    Tco(Expr, Env),
}

/// Evaluate a parsed expression in the given environment.
/// Uses a trampoline loop for tail call optimization.
pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let mut cur = expr.clone();
    let mut cur_env = env.clone();

    loop {
        match cur {
            Expr::Integer(n, _) => return Ok(Value::Integer(n)),
            Expr::Boolean(b, _) => return Ok(Value::Boolean(b)),
            Expr::String(ref s, _) => return Ok(Value::String(s.clone())),
            Expr::Char(c, _) => return Ok(Value::Char(c)),
            Expr::Symbol(ref name, span) => {
                return cur_env
                    .get(name)
                    .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }.at(span));
            }
            Expr::List(ref elems, span) => match eval_list(elems, span, &cur_env)? {
                Bounce::Done(val) => return Ok(val),
                Bounce::Tco(next_expr, next_env) => {
                    cur = next_expr;
                    cur_env = next_env;
                }
            },
        }
    }
}

/// Evaluate a list form (special forms, builtins, or lambda application).
fn eval_list(elems: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let [ref operator, ref args @ ..] = elems else {
        return Err(EvalError::Parse("empty list".into()).at(span));
    };

    // Check for special forms
    if let Expr::Symbol(ref op, _) = operator {
        if let Some(bounce) = eval_special_form(op, args, span, env)? {
            return Ok(bounce);
        }
        if is_builtin(op) {
            return eval_builtin(op, args, env)
                .map(Bounce::Done)
                .map_err(|e| e.at(span));
        }
    }

    // Evaluate operator to get a callable value
    let op_val = eval(operator, env)?;
    eval_application(op_val, args, span, env)
}

/// Dispatch special forms. Returns `None` if `op` is not a special form.
fn eval_special_form(
    op: &str,
    args: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Option<Bounce>, EvalError> {
    match op {
        "quote" => eval_quote(args).map(Bounce::Done).map(Some).map_err(|e| e.at(span)),
        "if" => eval_if(args, span, env).map(Some),
        "define" => eval_define(args, span, env).map(Bounce::Done).map(Some),
        "lambda" => eval_lambda(args, env)
            .map(Bounce::Done)
            .map(Some)
            .map_err(|e| e.at(span)),
        "let" => eval_let(args, span, env).map(Some),
        "begin" => eval_begin(args, env).map(Some),
        "cond" => eval_cond(args, span, env).map(Some),
        "and" => eval_and(args, env).map(Some),
        "or" => eval_or(args, env).map(Some),
        "string-set!" => eval_string_set(args, env)
            .map(Bounce::Done)
            .map(Some)
            .map_err(|e| e.at(span)),
        _ => Ok(None),
    }
}

fn eval_if(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    let (cond_expr, consequent, alternate) = match args {
        [c, t, f] => (c, t, Some(f)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 3,
                got: args.len(),
            }
            .at(span))
        }
    };
    let cond_val = eval(cond_expr, env)?;
    if cond_val.is_truthy() {
        Ok(Bounce::Tco(consequent.clone(), env.clone()))
    } else if let Some(alt) = alternate {
        Ok(Bounce::Tco(alt.clone(), env.clone()))
    } else {
        Ok(Bounce::Done(Value::Void))
    }
}

fn eval_let(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    // Named let: (let name ((var init) ...) body ...)
    if let [Expr::Symbol(ref name, _), Expr::List(ref bindings, _), ref body @ ..] = args {
        return eval_named_let(name, bindings, body, span, env);
    }
    // Regular let: (let ((var init) ...) body ...)
    let [Expr::List(ref bindings, _), ref body @ ..] = args else {
        return Err(EvalError::Parse("invalid let form".into()).at(span));
    };
    if body.is_empty() {
        return Err(EvalError::Parse("let requires a body".into()).at(span));
    }
    let let_env = Env::extend(env);
    for binding in bindings {
        let (bname, val) = parse_and_eval_binding(binding, span, env)?;
        let_env.define(bname, val);
    }
    eval_body_tco(body, let_env)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Bounce, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Parse("named let requires a body".into()).at(span));
    }
    let mut param_names = Vec::new();
    let mut init_vals = Vec::new();
    for binding in bindings {
        let (pname, val) = parse_and_eval_binding(binding, span, env)?;
        param_names.push(pname);
        init_vals.push(val);
    }
    let loop_body = wrap_body(body, span);
    let let_env = Env::extend(env);
    let lambda = Value::Lambda {
        params: param_names.clone(),
        body: loop_body.clone(),
        closure: let_env.clone(),
    };
    let_env.define(name.into(), lambda);
    for (pname, val) in param_names.iter().zip(init_vals) {
        let_env.define(pname.clone(), val);
    }
    Ok(Bounce::Tco(loop_body, let_env))
}

/// Parse a single let binding `(name expr)` and evaluate the init expression.
fn parse_and_eval_binding(
    binding: &Expr,
    span: Span,
    env: &Env,
) -> Result<(String, Value), EvalError> {
    let Expr::List(ref pair, _) = binding else {
        return Err(EvalError::Parse("let binding must be a list".into()).at(span));
    };
    let [Expr::Symbol(ref bname, _), ref val_expr] = pair.as_slice() else {
        return Err(EvalError::Parse("let binding must be (name expr)".into()).at(span));
    };
    let val = eval(val_expr, env)?;
    Ok((bname.clone(), val))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    eval_body_tco(args, env.clone())
}

fn eval_cond(args: &[Expr], span: Span, env: &Env) -> Result<Bounce, EvalError> {
    for clause in args {
        let Expr::List(ref parts, _) = clause else {
            return Err(EvalError::Parse("cond clause must be a list".into()).at(span));
        };
        let [ref test, ref body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse("cond clause must have a test".into()).at(span));
        };
        let is_else = matches!(test, Expr::Symbol(s, _) if s == "else");
        if is_else || eval(test, env)?.is_truthy() {
            return eval_body_tco(body, env.clone());
        }
    }
    Ok(Bounce::Done(Value::Void))
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(true)));
    };
    for expr in rest {
        let val = eval(expr, env)?;
        if !val.is_truthy() {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::Tco(last.clone(), env.clone()))
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(false)));
    };
    for expr in rest {
        let val = eval(expr, env)?;
        if val.is_truthy() {
            return Ok(Bounce::Done(val));
        }
    }
    Ok(Bounce::Tco(last.clone(), env.clone()))
}

/// Evaluate a body sequence with TCO on the last expression.
fn eval_body_tco(body: &[Expr], env: Env) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(Bounce::Done(Value::Void));
    };
    for expr in rest {
        eval(expr, &env)?;
    }
    Ok(Bounce::Tco(last.clone(), env))
}

/// Apply a callable value to evaluated arguments.
fn eval_application(
    op_val: Value,
    args: &[Expr],
    span: Span,
    env: &Env,
) -> Result<Bounce, EvalError> {
    let Value::Lambda {
        params,
        body,
        closure,
    } = op_val
    else {
        return Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{op_val}"),
        }
        .at(span));
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        }
        .at(span));
    }
    let call_env = Env::extend(&closure);
    for (param, arg_expr) in params.iter().zip(args) {
        let arg_val = eval(arg_expr, env)?;
        call_env.define(param.clone(), arg_val);
    }
    Ok(Bounce::Tco(body, call_env))
}

fn wrap_body(body: &[Expr], span: Span) -> Expr {
    if body.len() == 1 {
        body[0].clone()
    } else {
        let mut begin = vec![Expr::Symbol("begin".into(), span)];
        begin.extend(body.iter().cloned());
        Expr::List(begin, span)
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
            | "string-ref" | "symbol->string" | "string->symbol"
            | "string-copy"
    )
}

fn eval_builtin(op: &str, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    match op {
        "+" => eval_arithmetic(args, env, 0, |a, b| Ok(a + b)),
        "-" => eval_minus(args, env),
        "*" => eval_arithmetic(args, env, 1, |a, b| Ok(a * b)),
        "/" => eval_divide(args, env),
        "<" => eval_comparison(args, env, |a, b| a < b),
        ">" => eval_comparison(args, env, |a, b| a > b),
        "=" => eval_comparison(args, env, |a, b| a == b),
        "<=" => eval_comparison(args, env, |a, b| a <= b),
        ">=" => eval_comparison(args, env, |a, b| a >= b),
        "not" => eval_not(args, env),
        "cons" => eval_cons(args, env),
        "car" => eval_car(args, env),
        "cdr" => eval_cdr(args, env),
        "null?" => eval_null(args, env),
        "list" => eval_list_builtin(args, env),
        "length" => eval_length(args, env),
        "string?" => eval_type_pred(args, env, |v| matches!(v, Value::String(_))),
        "number?" => eval_type_pred(args, env, |v| matches!(v, Value::Integer(_))),
        "boolean?" => eval_type_pred(args, env, |v| matches!(v, Value::Boolean(_))),
        "pair?" => eval_type_pred(args, env, |v| matches!(v, Value::List(l) if !l.is_empty())),
        "symbol?" => eval_type_pred(args, env, |v| matches!(v, Value::Symbol(_))),
        "char?" => eval_type_pred(args, env, |v| matches!(v, Value::Char(_))),
        "display" => eval_display(args, env),
        "write" => eval_write(args, env),
        "newline" => eval_newline(args, env),
        "string-append" => eval_string_append(args, env),
        "string-length" => eval_string_length(args, env),
        "substring" => eval_substring(args, env),
        "string->number" => eval_string_to_number(args, env),
        "number->string" => eval_number_to_string(args, env),
        "string-ref" => eval_string_ref(args, env),
        "symbol->string" => eval_symbol_to_string(args, env),
        "string->symbol" => eval_string_to_symbol(args, env),
        "string-copy" => eval_string_copy(args, env),
        _ => Err(EvalError::UnboundVariable { name: op.into() }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    expr_to_value(arg)
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::String(s, _) => Ok(Value::String(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(s, _) => Ok(Value::Symbol(s.clone())),
        Expr::List(items, _) => {
            let vals: Vec<Value> = items.iter().map(expr_to_value).collect::<Result<_, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_define(args: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Expr::Symbol(name, _), expr] => {
            let val = eval(expr, env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        [Expr::List(name_and_params, _), body @ ..] if !body.is_empty() => {
            let [Expr::Symbol(name, _), params @ ..] = name_and_params.as_slice() else {
                return Err(EvalError::Parse("invalid define form".into()).at(span));
            };
            let param_names: Vec<String> = params
                .iter()
                .map(|p| match p {
                    Expr::Symbol(s, _) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("parameter must be a symbol".into()).at(span)),
                })
                .collect::<Result<_, _>>()?;
            let wrapped_body = wrap_body(body, span);
            let lambda = Value::Lambda {
                params: param_names,
                body: wrapped_body,
                closure: env.clone(),
            };
            env.define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("invalid define form".into()).at(span)),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [Expr::List(params, _), body] = args else {
        return Err(EvalError::Parse("invalid lambda form".into()));
    };
    let param_names: Vec<String> = params
        .iter()
        .map(|p| {
            if let Expr::Symbol(s, _) = p {
                Ok(s.clone())
            } else {
                Err(EvalError::Parse("parameter must be a symbol".into()))
            }
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params: param_names,
        body: body.clone(),
        closure: env.clone(),
    })
}

fn eval_arithmetic(
    args: &[Expr],
    env: &Env,
    identity: i64,
    op: fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, arg| {
            let val = eval(arg, env)?;
            let Value::Integer(n) = val else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{val}"),
                });
            };
            op(acc, n)
        })
        .map(Value::Integer)
}

fn eval_minus(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    if rest.is_empty() {
        return Ok(Value::Integer(-first_val));
    }

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg, env)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            Ok(acc - n)
        })
        .map(Value::Integer)
}

fn eval_divide(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg, env)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            if n == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(acc / n)
        })
        .map(Value::Integer)
}

fn eval_comparison(
    args: &[Expr],
    env: &Env,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let Value::Integer(a) = eval(left, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };
    let Value::Integer(b) = eval(right, env)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    Ok(Value::Boolean(cmp(a, b)))
}

fn eval_not(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let h = eval(head, env)?;
    let t = eval(tail, env)?;
    match t {
        Value::List(mut items) => {
            items.insert(0, h);
            Ok(Value::List(items))
        }
        _ => Ok(Value::List(vec![h, t])),
    }
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{val}"),
        });
    };
    items.into_iter().next().ok_or_else(|| EvalError::TypeError {
        expected: "pair".into(),
        got: "()".into(),
    })
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{val}"),
        });
    };
    if items.is_empty() {
        return Err(EvalError::TypeError {
            expected: "pair".into(),
            got: "()".into(),
        });
    }
    Ok(Value::List(items[1..].to_vec()))
}

fn eval_null(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(
        matches!(val, Value::List(ref items) if items.is_empty()),
    ))
}

fn eval_list_builtin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let items: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    Ok(Value::List(items))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::List(items) = val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(items.len() as i64))
}

fn eval_type_pred(
    args: &[Expr],
    env: &Env,
    pred: fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(pred(&val)))
}

fn eval_display(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let mut buf = String::new();
    val.display_fmt(&mut buf);
    env.write_output(&buf);
    Ok(Value::Void)
}

fn eval_write(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount { expected: 0, got: args.len() });
    }
    env.write_output("\n");
    Ok(Value::Void)
}

fn eval_string_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let result: String = args
        .iter()
        .map(|a| {
            let val = eval(a, env)?;
            let Value::String(s) = val else {
                return Err(EvalError::TypeError {
                    expected: "string".into(),
                    got: format!("{val}"),
                });
            };
            Ok(s)
        })
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

fn eval_string_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(s.len() as i64))
}

fn eval_substring(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [s_expr, start_expr, end_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let val = eval(s_expr, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    let Value::Integer(start) = eval(start_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let Value::Integer(end) = eval(end_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let start = start as usize;
    let end = end as usize;
    if start > end || end > s.len() {
        return Err(EvalError::TypeError {
            expected: "valid substring indices".into(),
            got: format!("start={start}, end={end}, len={}", s.len()),
        });
    }
    Ok(Value::String(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Integer(n)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Integer(n) = val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(n.to_string()))
}

fn eval_string_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [s_expr, idx_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let val = eval(s_expr, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    let Value::Integer(idx) = eval(idx_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let idx = idx as usize;
    s.chars().nth(idx).map(Value::Char).ok_or_else(|| EvalError::TypeError {
        expected: "valid string index".into(),
        got: format!("index {idx} out of range for string of length {}", s.len()),
    })
}

fn eval_symbol_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::Symbol(s) = val else {
        return Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s))
}

fn eval_string_to_symbol(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Symbol(s))
}

fn eval_string_copy(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s))
}

fn eval_string_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [str_expr, idx_expr, char_expr] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let Expr::Symbol(name, _) = str_expr else {
        return Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: "non-symbol".into(),
        });
    };
    let val = env.get(name).ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })?;
    let Value::String(mut s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{val}"),
        });
    };
    let Value::Integer(idx) = eval(idx_expr, env)? else {
        return Err(EvalError::TypeError { expected: "integer".into(), got: "non-integer".into() });
    };
    let Value::Char(ch) = eval(char_expr, env)? else {
        return Err(EvalError::TypeError { expected: "char".into(), got: "non-char".into() });
    };
    let idx = idx as usize;
    if idx >= s.len() {
        return Err(EvalError::TypeError {
            expected: "valid string index".into(),
            got: format!("index {idx} out of range for string of length {}", s.len()),
        });
    }
    // SAFETY: replacing a single byte in an ASCII-safe position
    let bytes = unsafe { s.as_bytes_mut() };
    bytes[idx] = ch as u8;
    env.set(name, Value::String(s));
    Ok(Value::Void)
}
