use std::cell::RefCell;
use std::rc::Rc;

use super::{Env, EvalError, Value};

/// Trampoline result: either a final value, a tail-call in the same env,
/// or a tail-call into a new env (function application).
enum Bounce {
    Done(Value),
    /// Tail call: evaluate expr in the current (caller's) environment.
    Continue(Value),
    /// Tail call into a new environment (lambda application).
    Call { expr: Value, env: Env },
    /// Tail call where the env was updated in place (no allocation).
    TailUpdate(Value),
}

pub(super) fn eval(value: &Value, env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut current = value.clone();
    let mut call_env: Option<Env> = None;

    loop {
        let bounce = {
            let active_env = call_env.as_mut().unwrap_or(env);
            eval_bounce(&current, active_env, out)?
        };
        match bounce {
            Bounce::Done(val) => return Ok(val),
            Bounce::Continue(expr) | Bounce::TailUpdate(expr) => current = expr,
            Bounce::Call { expr, env: new_env } => {
                current = expr;
                call_env = Some(new_env);
            }
        }
    }
}

fn eval_bounce(value: &Value, env: &mut Env, out: &mut String) -> Result<Bounce, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_)
        | Value::Lambda { .. } | Value::BuiltinProc(_) => Ok(Bounce::Done(value.clone())),
        Value::Nil => Ok(Bounce::Done(Value::Nil)),
        Value::Symbol(name) => {
            if let Some(rc) = env.get(name) {
                Ok(Bounce::Done(rc.borrow().clone()))
            } else if is_builtin(name) || name == "apply" {
                Ok(Bounce::Done(Value::BuiltinProc(name.clone())))
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        Value::Pair(..) => eval_pair_bounce(value, env, out),
    }
}

fn eval_pair_bounce(
    value: &Value,
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let items = value.to_list_vec().ok_or_else(|| EvalError::TypeError {
        expected: "proper list".to_string(),
        got: value.display(),
    })?;

    let [operator, args @ ..] = items.as_slice() else {
        return Ok(Bounce::Done(Value::Nil));
    };

    // Special forms
    if let Value::Symbol(name) = operator {
        return match name.as_str() {
            "define" => eval_define(args, env, out).map(Bounce::Done),
            "if" => eval_if_bounce(args, env, out),
            "quote" => eval_quote(args).map(Bounce::Done),
            "lambda" => eval_lambda(args, env).map(Bounce::Done),
            "begin" => eval_begin_bounce(args, env, out),
            "let" => eval_let_bounce(args, env, out),
            "cond" => eval_cond_bounce(args, env, out),
            "and" => eval_and_bounce(args, env, out),
            "or" => eval_or_bounce(args, env, out),
            "set!" => eval_set(args, env, out).map(Bounce::Done),
            "string-set!" => eval_string_set(args, env, out).map(Bounce::Done),
            _ => eval_symbol_call_bounce(name, args, env, out),
        };
    }

    // Evaluate operator and apply
    let proc = eval(operator, env, out)?;
    let evaled_args: Vec<Value> =
        args.iter().map(|a| eval(a, env, out)).collect::<Result<_, _>>()?;
    apply_proc_bounce(&proc, &evaled_args, env, out)
}

fn eval_symbol_call_bounce(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    if is_builtin(name) {
        return eval_builtin(name, args, env, out).map(Bounce::Done);
    }
    if name == "apply" {
        let evaled_args: Vec<Value> =
            args.iter().map(|a| eval(a, env, out)).collect::<Result<_, _>>()?;
        return eval_apply(&evaled_args, env, out).map(Bounce::Done);
    }
    let proc_rc = env
        .get(name)
        .cloned()
        .ok_or_else(|| EvalError::UnboundVariable {
            name: name.to_string(),
        })?;
    let evaled_args: Vec<Value> =
        args.iter().map(|a| eval(a, env, out)).collect::<Result<_, _>>()?;

    // Borrow the proc to avoid cloning the Lambda (and its closure HashMap)
    let proc_ref = proc_rc.borrow();
    match &*proc_ref {
        Value::Lambda {
            params,
            rest_param,
            body,
            closure,
        } => {
            if rest_param.is_some() {
                if evaled_args.len() < params.len() {
                    return Err(EvalError::WrongArgCount {
                        expected: params.len(),
                        got: evaled_args.len(),
                    });
                }
            } else if params.len() != evaled_args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: evaled_args.len(),
                });
            }
            let mut call_env = closure.clone();
            for (k, v) in env.iter() {
                call_env.entry(k.clone()).or_insert_with(|| v.clone());
            }
            for (param, arg) in params.iter().zip(&evaled_args) {
                call_env.insert(param.clone(), Rc::new(RefCell::new(arg.clone())));
            }
            if let Some(rest_name) = rest_param {
                let rest_args = &evaled_args[params.len()..];
                let rest_list = rest_args.iter().rev().fold(Value::Nil, |acc, v| {
                    Value::Pair(Box::new(v.clone()), Box::new(acc))
                });
                call_env.insert(rest_name.clone(), Rc::new(RefCell::new(rest_list)));
            }
            Ok(Bounce::Call {
                expr: body.as_ref().clone(),
                env: call_env,
            })
        }
        Value::BuiltinProc(bname) => {
            call_builtin_with_values(bname, &evaled_args, env, out).map(Bounce::Done)
        }
        _ => Err(EvalError::NotAProcedure {
            value: proc_ref.display(),
        }),
    }
}

/// Build the call environment and return a tail-call bounce.
fn apply_bounce(
    proc: &Value,
    args: &[Value],
    caller_env: &Env,
) -> Result<Bounce, EvalError> {
    apply_bounce_inner(proc, args, Some(caller_env))
}

fn apply_bounce_inner(
    proc: &Value,
    args: &[Value],
    caller_env: Option<&Env>,
) -> Result<Bounce, EvalError> {
    let Value::Lambda {
        params,
        rest_param,
        body,
        closure,
    } = proc
    else {
        return Err(EvalError::NotAProcedure {
            value: proc.display(),
        });
    };

    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::WrongArgCount {
                expected: params.len(),
                got: args.len(),
            });
        }
    } else if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }

    let mut call_env = closure.clone();
    if let Some(caller) = caller_env {
        for (k, v) in caller {
            call_env.entry(k.clone()).or_insert_with(|| v.clone());
        }
    }
    for (param, arg) in params.iter().zip(args) {
        call_env.insert(param.clone(), Rc::new(RefCell::new(arg.clone())));
    }
    if let Some(rest_name) = rest_param {
        let rest_args = &args[params.len()..];
        let rest_list = rest_args.iter().rev().fold(Value::Nil, |acc, v| {
            Value::Pair(Box::new(v.clone()), Box::new(acc))
        });
        call_env.insert(rest_name.clone(), Rc::new(RefCell::new(rest_list)));
    }

    Ok(Bounce::Call {
        expr: body.as_ref().clone(),
        env: call_env,
    })
}

/// Non-tail apply: resolves the bounce immediately via `eval`.
fn apply_value(
    proc: &Value,
    args: &[Value],
    caller_env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    match proc {
        Value::BuiltinProc(name) => call_builtin_with_values(name, args, caller_env, out),
        Value::Lambda { .. } => match apply_bounce(proc, args, caller_env)? {
            Bounce::Done(val) => Ok(val),
            Bounce::Call { expr, mut env } => eval(&expr, &mut env, out),
            Bounce::Continue(expr) => {
                unreachable!("apply_bounce returned Continue for {}", expr.display())
            }
        },
        _ => Err(EvalError::NotAProcedure {
            value: proc.display(),
        }),
    }
}

/// Dispatch to either lambda apply or builtin call, returning a bounce.
fn apply_proc_bounce(
    proc: &Value,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    match proc {
        Value::BuiltinProc(name) => {
            call_builtin_with_values(name, args, env, out).map(Bounce::Done)
        }
        Value::Lambda { .. } => apply_bounce(proc, args, env),
        _ => Err(EvalError::NotAProcedure {
            value: proc.display(),
        }),
    }
}

/// Call a builtin by name with already-evaluated argument values.
fn call_builtin_with_values(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => {
            let values: Vec<i64> = args
                .iter()
                .map(|v| match v {
                    Value::Integer(n) => Ok(*n),
                    other => Err(EvalError::TypeError {
                        expected: "integer".to_string(),
                        got: other.display(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            compute_arithmetic(name, &values)
        }
        "cons" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            Ok(Value::Pair(Box::new(a.clone()), Box::new(b.clone())))
        }
        "car" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let Value::Pair(car, _) = a else {
                return Err(EvalError::TypeError { expected: "pair".to_string(), got: a.display() });
            };
            Ok(*car.clone())
        }
        "cdr" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let Value::Pair(_, cdr) = a else {
                return Err(EvalError::TypeError { expected: "pair".to_string(), got: a.display() });
            };
            Ok(*cdr.clone())
        }
        "null?" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            Ok(Value::Boolean(matches!(a, Value::Nil)))
        }
        "list" => Ok(args.iter().rev().fold(Value::Nil, |acc, v| {
            Value::Pair(Box::new(v.clone()), Box::new(acc))
        })),
        "length" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let items = a.to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "proper list".to_string(),
                got: a.display(),
            })?;
            Ok(Value::Integer(items.len() as i64))
        }
        "not" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            Ok(Value::Boolean(*a == Value::Boolean(false)))
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            let [lhs, rhs] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let l = match lhs { Value::Integer(n) => *n, other => return Err(EvalError::TypeError { expected: "integer".to_string(), got: other.display() }) };
            let r = match rhs { Value::Integer(n) => *n, other => return Err(EvalError::TypeError { expected: "integer".to_string(), got: other.display() }) };
            let result = match name { "<" => l < r, ">" => l > r, "=" => l == r, "<=" => l <= r, ">=" => l >= r, _ => unreachable!() };
            Ok(Value::Boolean(result))
        }
        "apply" => eval_apply(args, env, out),
        _ => {
            // For other builtins, wrap values back as quoted exprs and delegate
            // This is a fallback for builtins not yet handled above
            Err(EvalError::NotAProcedure {
                value: format!("builtin {name} not supported in apply context"),
            })
        }
    }
}

/// Implement (apply proc arg1 ... argN list)
fn eval_apply(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let proc = &args[0];
    let last = &args[args.len() - 1];
    let tail_list = last.to_list_vec().ok_or_else(|| EvalError::TypeError {
        expected: "proper list".to_string(),
        got: last.display(),
    })?;
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail_list);
    apply_value(proc, &all_args, env, out)
}

/// Extract (name, params, rest_param) from a define header like (f x . rest) or (f x y).
fn parse_define_header(header: &Value) -> Result<(String, Vec<String>, Option<String>), EvalError> {
    let Value::Pair(car, cdr) = header else {
        return Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: header.display(),
        });
    };
    let Value::Symbol(name) = car.as_ref() else {
        return Err(EvalError::TypeError {
            expected: "symbol as function name".to_string(),
            got: car.display(),
        });
    };
    let (params, rest_param) = extract_params(cdr)?;
    Ok((name.clone(), params, rest_param))
}

fn expect_symbol(value: &Value) -> Result<String, EvalError> {
    match value {
        Value::Symbol(s) => Ok(s.clone()),
        other => Err(EvalError::TypeError {
            expected: "symbol".to_string(),
            got: other.display(),
        }),
    }
}

/// Extract params and optional rest from a parameter-list value (the cdr of the header pair).
fn extract_params(value: &Value) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut current = value;
    loop {
        match current {
            Value::Nil => return Ok((params, None)),
            Value::Symbol(s) => return Ok((params, Some(s.clone()))),
            Value::Pair(car, cdr) => {
                params.push(expect_symbol(car)?);
                current = cdr;
            }
            other => {
                return Err(EvalError::TypeError {
                    expected: "parameter list".to_string(),
                    got: other.display(),
                })
            }
        }
    }
}

fn eval_define(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env, out)?;
            env.insert(name.clone(), Rc::new(RefCell::new(val)));
            Ok(Value::Symbol(name.clone()))
        }
        // (define (name params...) body) or (define (name p1 . rest) body)
        [Value::Pair(..), body @ ..] => {
            let (name, params, rest_param) = parse_define_header(&args[0])?;
            let func_body = wrap_body(body)?;

            let lambda = Value::Lambda {
                params,
                rest_param,
                body: Rc::new(func_body),
                closure: env.clone(),
            };
            env.insert(name.clone(), Rc::new(RefCell::new(lambda)));
            Ok(Value::Symbol(name))
        }
        _ => Err(EvalError::TypeError {
            expected: "symbol and value".to_string(),
            got: format!("{} args", args.len()),
        }),
    }
}

fn eval_set(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::TypeError {
            expected: "(set! symbol expr)".to_string(),
            got: format!("{} args", args.len()),
        });
    };
    let val = eval(expr, env, out)?;
    let rc = env.get(name).ok_or_else(|| EvalError::UnboundVariable {
        name: name.clone(),
    })?;
    *rc.borrow_mut() = val;
    Ok(Value::Nil)
}

fn eval_if_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let (condition, consequent, alternative) = match args {
        [cond, cons, alt] => (cond, cons, Some(alt)),
        [cond, cons] => (cond, cons, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 3,
                got: args.len(),
            })
        }
    };
    let test = eval(condition, env, out)?;
    if test != Value::Boolean(false) {
        Ok(Bounce::Continue(consequent.clone()))
    } else {
        Ok(alternative.map_or(Bounce::Done(Value::Nil), |alt| {
            Bounce::Continue(alt.clone())
        }))
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(datum.clone())
}

fn eval_let_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    // Named let: (let name ((var init) ...) body ...)
    if let [Value::Symbol(name), bindings_val, body @ ..] = args {
        return eval_named_let_bounce(name, bindings_val, body, env, out);
    }

    let [bindings_val, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let binding_list = bindings_val
        .to_list_vec()
        .ok_or_else(|| EvalError::TypeError {
            expected: "binding list".to_string(),
            got: bindings_val.display(),
        })?;

    let mut params = Vec::new();
    let mut values = Vec::new();
    for binding in &binding_list {
        let pair = binding.to_list_vec().ok_or_else(|| EvalError::TypeError {
            expected: "binding pair".to_string(),
            got: binding.display(),
        })?;
        let [Value::Symbol(name), expr] = pair.as_slice() else {
            return Err(EvalError::TypeError {
                expected: "(symbol expr)".to_string(),
                got: binding.display(),
            });
        };
        params.push(name.clone());
        values.push(eval(expr, env, out)?);
    }

    let func_body = wrap_body(body)?;
    let lambda = Value::Lambda {
        params,
        rest_param: None,
        body: Rc::new(func_body),
        closure: env.clone(),
    };

    apply_bounce(&lambda, &values, env)
}

fn eval_named_let_bounce(
    name: &str,
    bindings_val: &Value,
    body: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let binding_list = bindings_val
        .to_list_vec()
        .ok_or_else(|| EvalError::TypeError {
            expected: "binding list".to_string(),
            got: bindings_val.display(),
        })?;

    let mut params = Vec::new();
    let mut init_values = Vec::new();
    for binding in &binding_list {
        let pair = binding.to_list_vec().ok_or_else(|| EvalError::TypeError {
            expected: "binding pair".to_string(),
            got: binding.display(),
        })?;
        let [Value::Symbol(param), expr] = pair.as_slice() else {
            return Err(EvalError::TypeError {
                expected: "(symbol expr)".to_string(),
                got: binding.display(),
            });
        };
        params.push(param.clone());
        init_values.push(eval(expr, env, out)?);
    }

    let func_body = wrap_body(body)?;

    // Build lambda and bind name in env for self-reference
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body: Box::new(func_body.clone()),
        closure: Env::new(),
    };
    let cell = Rc::new(RefCell::new(lambda));

    let mut loop_env = env.clone();
    loop_env.insert(name.to_string(), cell.clone());

    // Update closure to include the self-reference
    {
        let mut borrowed = cell.borrow_mut();
        if let Value::Lambda { ref mut closure, .. } = *borrowed {
            *closure = loop_env.clone();
        }
    }

    // Bind initial params
    for (param, val) in params.iter().zip(&init_values) {
        loop_env.insert(param.clone(), Rc::new(RefCell::new(val.clone())));
    }

    // Execute the loop body directly using eval, which uses the trampoline
    let result = eval(&func_body, &mut loop_env, out)?;
    Ok(Bounce::Done(result))
}

fn eval_body_bounce(
    body: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(Bounce::Done(Value::Nil));
    };
    for expr in rest {
        eval(expr, env, out)?;
    }
    Ok(Bounce::Continue(last.clone()))
}

fn eval_cond_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    for clause_val in args {
        let clause = clause_val
            .to_list_vec()
            .ok_or_else(|| EvalError::TypeError {
                expected: "cond clause".to_string(),
                got: clause_val.display(),
            })?;

        let [test, body @ ..] = clause.as_slice() else {
            return Err(EvalError::WrongArgCount {
                expected: 1,
                got: 0,
            });
        };

        let is_match = if matches!(test, Value::Symbol(s) if s == "else") {
            true
        } else {
            eval(test, env, out)? != Value::Boolean(false)
        };

        if is_match {
            return eval_body_bounce(body, env, out);
        }
    }

    Ok(Bounce::Done(Value::Nil))
}

fn eval_begin_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    eval_body_bounce(args, env, out)
}

fn wrap_body(body: &[Value]) -> Result<Value, EvalError> {
    match body {
        [single] => Ok(single.clone()),
        [_, ..] => {
            let begin_sym = Value::Symbol("begin".to_string());
            let list = body.iter().rev().fold(Value::Nil, |acc, expr| {
                Value::Pair(Box::new(expr.clone()), Box::new(acc))
            });
            Ok(Value::Pair(Box::new(begin_sym), Box::new(list)))
        }
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
    }
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let [param_list, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let (params, rest_param) = extract_params(param_list)?;

    let func_body = wrap_body(body)?;

    Ok(Value::Lambda {
        params,
        rest_param,
        body: Rc::new(func_body),
        closure: env.clone(),
    })
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
            | "string-ref" | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "map"
    )
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => eval_arithmetic(name, args, env, out),
        "<" | ">" | "=" | "<=" | ">=" => eval_comparison(name, args, env, out),
        "not" => eval_not(args, env, out),
        "and" => eval_and(args, env, out),  // fallback for non-tail contexts
        "or" => eval_or(args, env, out),   // fallback for non-tail contexts
        "cons" | "car" | "cdr" | "null?" | "list" | "length" => {
            eval_list_builtin(name, args, env, out)
        }
        "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?" => {
            eval_type_pred(name, args, env, out)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string->list" | "list->string"
        | "char->integer" | "integer->char" => eval_string_builtin(name, args, env, out),
        "map" => eval_map(args, env, out),
        "display" => eval_display(args, env, out),
        "write" => eval_write(args, env, out),
        "newline" => eval_newline(args, out),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn checked_div(acc: i64, v: i64) -> Result<i64, EvalError> {
    if v == 0 {
        Err(EvalError::DivisionByZero)
    } else {
        Ok(acc / v)
    }
}

fn eval_arithmetic(
    op: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let values: Vec<i64> = args
        .iter()
        .map(|a| {
            let evaled = eval(a, env, out)?;
            match evaled {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".to_string(),
                    got: other.display(),
                }),
            }
        })
        .collect::<Result<_, _>>()?;
    compute_arithmetic(op, &values)
}

fn compute_arithmetic(op: &str, values: &[i64]) -> Result<Value, EvalError> {
    let result = match op {
        "+" => values.iter().sum(),
        "*" => values.iter().product(),
        "-" => {
            let [first, rest @ ..] = values else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            if rest.is_empty() { -first } else { rest.iter().fold(*first, |acc, &v| acc - v) }
        }
        "/" => {
            let [first, rest @ ..] = values else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            rest.iter().try_fold(*first, |acc, &v| checked_div(acc, v))?
        }
        _ => unreachable!("unexpected arithmetic operator: {op}"),
    };
    Ok(Value::Integer(result))
}

fn eval_comparison(
    op: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let lhs = match eval(lhs, env, out)? {
        Value::Integer(n) => n,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let rhs = match eval(rhs, env, out)? {
        Value::Integer(n) => n,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let result = match op {
        "<" => lhs < rhs,
        ">" => lhs > rhs,
        "=" => lhs == rhs,
        "<=" => lhs <= rhs,
        ">=" => lhs >= rhs,
        _ => unreachable!("unexpected comparison operator: {op}"),
    };
    Ok(Value::Boolean(result))
}

fn eval_not(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_and(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    match eval_and_bounce(args, env, out)? {
        Bounce::Done(v) => Ok(v),
        Bounce::Continue(expr) => eval(&expr, env, out),
        Bounce::Call { expr, mut env } => eval(&expr, &mut env, out),
    }
}

fn eval_and_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(true)));
    };
    for arg in rest {
        let result = eval(arg, env, out)?;
        if result == Value::Boolean(false) {
            return Ok(Bounce::Done(Value::Boolean(false)));
        }
    }
    Ok(Bounce::Continue(last.clone()))
}

fn eval_or(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    match eval_or_bounce(args, env, out)? {
        Bounce::Done(v) => Ok(v),
        Bounce::Continue(expr) => eval(&expr, env, out),
        Bounce::Call { expr, mut env } => eval(&expr, &mut env, out),
    }
}

fn eval_or_bounce(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = args.split_last() else {
        return Ok(Bounce::Done(Value::Boolean(false)));
    };
    for arg in rest {
        let result = eval(arg, env, out)?;
        if result != Value::Boolean(false) {
            return Ok(Bounce::Done(result));
        }
    }
    Ok(Bounce::Continue(last.clone()))
}

fn eval_list_builtin(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                });
            };
            let car = eval(a, env, out)?;
            let cdr = eval(b, env, out)?;
            Ok(Value::Pair(Box::new(car), Box::new(cdr)))
        }
        "car" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            let val = eval(a, env, out)?;
            let Value::Pair(car, _) = val else {
                return Err(EvalError::TypeError {
                    expected: "pair".to_string(),
                    got: val.display(),
                });
            };
            Ok(*car)
        }
        "cdr" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            let val = eval(a, env, out)?;
            let Value::Pair(_, cdr) = val else {
                return Err(EvalError::TypeError {
                    expected: "pair".to_string(),
                    got: val.display(),
                });
            };
            Ok(*cdr)
        }
        "null?" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            let val = eval(a, env, out)?;
            Ok(Value::Boolean(matches!(val, Value::Nil)))
        }
        "list" => {
            let vals: Vec<Value> =
                args.iter().map(|a| eval(a, env, out)).collect::<Result<_, _>>()?;
            Ok(vals.into_iter().rev().fold(Value::Nil, |acc, v| {
                Value::Pair(Box::new(v), Box::new(acc))
            }))
        }
        "length" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            let val = eval(a, env, out)?;
            let items = val.to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "proper list".to_string(),
                got: val.display(),
            })?;
            Ok(Value::Integer(items.len() as i64))
        }
        _ => unreachable!("unexpected list builtin: {name}"),
    }
}

fn eval_type_pred(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    let result = match name {
        "string?" => matches!(val, Value::String(_)),
        "number?" => matches!(val, Value::Integer(_)),
        "boolean?" => matches!(val, Value::Boolean(_)),
        "pair?" => matches!(val, Value::Pair(..)),
        "symbol?" => matches!(val, Value::Symbol(_)),
        "char?" => matches!(val, Value::Char(_)),
        _ => unreachable!("unexpected type predicate: {name}"),
    };
    Ok(Value::Boolean(result))
}

fn eval_string_builtin(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let parts: Vec<String> = args
                .iter()
                .map(|a| match eval(a, env, out)? {
                    Value::String(s) => Ok(s),
                    other => Err(EvalError::TypeError {
                        expected: "string".to_string(),
                        got: other.display(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            Ok(Value::String(parts.concat()))
        }
        "string-length" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            match eval(arg, env, out)? {
                Value::String(s) => Ok(Value::Integer(s.len() as i64)),
                other => Err(EvalError::TypeError {
                    expected: "string".to_string(),
                    got: other.display(),
                }),
            }
        }
        "substring" => eval_substring(args, env, out),
        "string->number" | "number->string" | "symbol->string" | "string->symbol"
        | "char->integer" | "integer->char" => eval_conversion_builtin(name, args, env, out),
        "string-copy" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            match eval(arg, env, out)? {
                Value::String(s) => Ok(Value::String(s)),
                other => Err(EvalError::TypeError {
                    expected: "string".to_string(),
                    got: other.display(),
                }),
            }
        }
        "string->list" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            match eval(arg, env, out)? {
                Value::String(s) => Ok(s.chars().rev().fold(Value::Nil, |acc, c| {
                    Value::Pair(Box::new(Value::Char(c)), Box::new(acc))
                })),
                other => Err(EvalError::TypeError {
                    expected: "string".to_string(),
                    got: other.display(),
                }),
            }
        }
        "list->string" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let val = eval(arg, env, out)?;
            let items = val.to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "proper list".to_string(),
                got: val.display(),
            })?;
            let s: String = items
                .iter()
                .map(|v| match v {
                    Value::Char(c) => Ok(*c),
                    other => Err(EvalError::TypeError {
                        expected: "char".to_string(),
                        got: other.display(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            Ok(Value::String(s))
        }
        "string-ref" => {
            let [s_arg, idx_arg] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let s = match eval(s_arg, env, out)? {
                Value::String(s) => s,
                other => {
                    return Err(EvalError::TypeError {
                        expected: "string".to_string(),
                        got: other.display(),
                    })
                }
            };
            let idx = match eval(idx_arg, env, out)? {
                Value::Integer(n) => n as usize,
                other => {
                    return Err(EvalError::TypeError {
                        expected: "integer".to_string(),
                        got: other.display(),
                    })
                }
            };
            s.chars()
                .nth(idx)
                .map(Value::Char)
                .ok_or_else(|| EvalError::TypeError {
                    expected: format!("index < {}", s.len()),
                    got: idx.to_string(),
                })
        }
        _ => unreachable!("unexpected string builtin: {name}"),
    }
}

fn eval_conversion_builtin(
    name: &str,
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env, out)?;
    match name {
        "string->number" => match val {
            Value::String(s) => s
                .parse::<i64>()
                .map(Value::Integer)
                .map_err(|_| EvalError::TypeError {
                    expected: "numeric string".to_string(),
                    got: format!("\"{s}\""),
                }),
            other => Err(EvalError::TypeError {
                expected: "string".to_string(),
                got: other.display(),
            }),
        },
        "number->string" => match val {
            Value::Integer(n) => Ok(Value::String(n.to_string())),
            other => Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            }),
        },
        "symbol->string" => match val {
            Value::Symbol(s) => Ok(Value::String(s)),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: other.display(),
            }),
        },
        "string->symbol" => match val {
            Value::String(s) => Ok(Value::Symbol(s)),
            other => Err(EvalError::TypeError {
                expected: "string".to_string(),
                got: other.display(),
            }),
        },
        "char->integer" => match val {
            Value::Char(c) => Ok(Value::Integer(c as i64)),
            other => Err(EvalError::TypeError {
                expected: "char".to_string(),
                got: other.display(),
            }),
        },
        "integer->char" => match val {
            Value::Integer(n) => {
                let c = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
                    expected: "valid unicode code point".to_string(),
                    got: n.to_string(),
                })?;
                Ok(Value::Char(c))
            }
            other => Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            }),
        },
        _ => unreachable!("unexpected conversion builtin: {name}"),
    }
}

fn eval_substring(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let [s_arg, start_arg, end_arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 3, got: args.len() });
    };
    let s = match eval(s_arg, env, out)? {
        Value::String(s) => s,
        other => {
            return Err(EvalError::TypeError {
                expected: "string".to_string(),
                got: other.display(),
            })
        }
    };
    let start = match eval(start_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let end = match eval(end_arg, env, out)? {
        Value::Integer(n) => n as usize,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    Ok(Value::String(s[start..end].to_string()))
}

fn eval_string_set(
    _args: &[Value],
    _env: &mut Env,
    _out: &mut String,
) -> Result<Value, EvalError> {
    Err(EvalError::ImmutableString)
}

fn eval_map(
    args: &[Value],
    env: &mut Env,
    out: &mut String,
) -> Result<Value, EvalError> {
    let [func_arg, list_arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let func = eval(func_arg, env, out)?;
    let list_val = eval(list_arg, env, out)?;
    let items = list_val.to_list_vec().ok_or_else(|| EvalError::TypeError {
        expected: "proper list".to_string(),
        got: list_val.display(),
    })?;
    let results: Vec<Value> = items
        .iter()
        .map(|item| apply_value(&func, std::slice::from_ref(item), env, out))
        .collect::<Result<_, _>>()?;
    Ok(results.into_iter().rev().fold(Value::Nil, |acc, v| {
        Value::Pair(Box::new(v), Box::new(acc))
    }))
}

fn eval_display(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.push_str(&val.display_human());
    Ok(Value::Nil)
}

fn eval_write(args: &[Value], env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env, out)?;
    out.push_str(&val.display());
    Ok(Value::Nil)
}

fn eval_newline(args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 0,
            got: args.len(),
        });
    }
    out.push('\n');
    Ok(Value::Nil)
}
