use crate::scheme::builtins::dispatch_builtin;
use crate::scheme::error::EvalError;
use crate::scheme::parser::Span;
use crate::scheme::{eval, Env, Tail, Value};
use std::cell::RefCell;
use std::rc::Rc;

fn parse_params_with_rest(
    param_list: &[Value],
    span: Span,
) -> Result<(Vec<String>, Option<String>), EvalError> {
    let (line, col) = span;
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_list.len() {
        match &param_list[i] {
            Value::Symbol(s, _) if s == "." => {
                if i + 1 >= param_list.len() {
                    return Err(EvalError::Parse {
                        message: "missing rest parameter after .".to_string(),
                        line,
                        col,
                    });
                }
                if i + 2 < param_list.len() {
                    return Err(EvalError::Parse {
                        message: "unexpected parameters after rest parameter".to_string(),
                        line,
                        col,
                    });
                }
                match &param_list[i + 1] {
                    Value::Symbol(rest, _) => rest_param = Some(rest.clone()),
                    other => {
                        return Err(EvalError::TypeError {
                            message: format!(
                                "expected symbol for rest parameter, got {}",
                                other.type_name()
                            ),
                            line,
                            col,
                        })
                    }
                }
                break;
            }
            Value::Symbol(s, _) => {
                params.push(s.clone());
                i += 1;
            }
            other => {
                return Err(EvalError::TypeError {
                    message: format!(
                        "expected symbol for parameter, got {}",
                        other.type_name()
                    ),
                    line,
                    col,
                })
            }
        }
    }
    Ok((params, rest_param))
}

fn wrap(val: Value) -> Rc<RefCell<Value>> {
    Rc::new(RefCell::new(val))
}

pub(crate) fn eval_let(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Tail, EvalError> {
    let (line, col) = span;
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "let: missing bindings".to_string(),
            line,
            col,
        });
    }

    // Named let: (let name ((var init) ...) body ...)
    if let Value::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Parse {
                message: "named let: missing bindings or body".to_string(),
                line,
                col,
            });
        }
        let bindings = match &args[1] {
            Value::List(b, _) => b,
            other => {
                return Err(EvalError::TypeError {
                    message: format!(
                        "named let: expected bindings list, got {}",
                        other.type_name()
                    ),
                    line,
                    col,
                })
            }
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings {
            match binding {
                Value::List(pair, _) if pair.len() == 2 => {
                    match &pair[0] {
                        Value::Symbol(s, _) => params.push(s.clone()),
                        other => {
                            return Err(EvalError::TypeError {
                                message: format!(
                                    "let: expected symbol, got {}",
                                    other.type_name()
                                ),
                                line,
                                col,
                            })
                        }
                    }
                    init_vals.push(eval(&pair[1], env, output)?);
                }
                other => {
                    return Err(EvalError::Parse {
                        message: format!("let: bad binding: {}", other.display()),
                        line,
                        col,
                    })
                }
            }
        }
        let body: Vec<Value> = args[2..].to_vec();
        let lambda = Value::Lambda {
            name: Some(name.clone()),
            params: params.clone(),
            rest_param: None,
            body: body.clone(),
            closure_env: env.clone(),
        };
        let mut local_env = env.clone();
        local_env.insert(name.clone(), wrap(lambda));
        for (param, val) in params.iter().zip(init_vals.into_iter()) {
            local_env.insert(param.clone(), wrap(val));
        }
        if body.is_empty() {
            return Ok(Tail::Done(Value::Boolean(false)));
        }
        for expr in &body[..body.len() - 1] {
            eval(expr, &mut local_env, output)?;
        }
        Ok(Tail::Call {
            expr: body[body.len() - 1].clone(),
            env: local_env,
        })
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0] {
            Value::List(b, _) => b,
            other => {
                return Err(EvalError::TypeError {
                    message: format!("let: expected bindings list, got {}", other.type_name()),
                    line,
                    col,
                })
            }
        };
        if args.len() < 2 {
            return Err(EvalError::Parse {
                message: "let: missing body".to_string(),
                line,
                col,
            });
        }
        let mut local_env = env.clone();
        for binding in bindings {
            match binding {
                Value::List(pair, _) if pair.len() == 2 => {
                    let name = match &pair[0] {
                        Value::Symbol(s, _) => s.clone(),
                        other => {
                            return Err(EvalError::TypeError {
                                message: format!(
                                    "let: expected symbol, got {}",
                                    other.type_name()
                                ),
                                line,
                                col,
                            })
                        }
                    };
                    let val = eval(&pair[1], env, output)?;
                    local_env.insert(name, wrap(val));
                }
                other => {
                    return Err(EvalError::Parse {
                        message: format!("let: bad binding: {}", other.display()),
                        line,
                        col,
                    })
                }
            }
        }
        let body = &args[1..];
        if body.is_empty() {
            return Ok(Tail::Done(Value::Boolean(false)));
        }
        for expr in &body[..body.len() - 1] {
            eval(expr, &mut local_env, output)?;
        }
        Ok(Tail::Call {
            expr: body[body.len() - 1].clone(),
            env: local_env,
        })
    }
}

pub(crate) fn eval_define(
    items: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let (line, col) = span;
    if items.len() < 3 {
        return Err(EvalError::Parse {
            message: "define requires at least 2 arguments".to_string(),
            line,
            col,
        });
    }
    match &items[1] {
        Value::Symbol(name, _) => {
            let mut val = eval(&items[2], env, output)?;
            if let Value::Lambda {
                name: ref mut n, ..
            } = val
            {
                *n = Some(name.clone());
            }
            env.insert(name.clone(), wrap(val));
            Ok(Value::Boolean(false))
        }
        Value::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse {
                    message: "define: empty signature".to_string(),
                    line,
                    col,
                });
            }
            let name = match &sig[0] {
                Value::Symbol(n, _) => n.clone(),
                other => {
                    return Err(EvalError::TypeError {
                        message: format!(
                            "define: expected symbol for name, got {}",
                            other.type_name()
                        ),
                        line,
                        col,
                    })
                }
            };
            let (params, rest_param) = parse_params_with_rest(&sig[1..], (line, col))?;
            let body: Vec<Value> = items[2..].to_vec();
            let closure = Value::Lambda {
                name: Some(name.clone()),
                params,
                rest_param,
                body,
                closure_env: env.clone(),
            };
            env.insert(name, wrap(closure));
            Ok(Value::Boolean(false))
        }
        other => Err(EvalError::TypeError {
            message: format!(
                "define: expected symbol or list, got {}",
                other.type_name()
            ),
            line,
            col,
        }),
    }
}

pub(crate) fn eval_string_set(
    items: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let (line, col) = span;
    if items.len() != 4 {
        return Err(EvalError::Arity {
            procedure: "string-set!".to_string(),
            expected: "3".to_string(),
            got: items.len() - 1,
            line,
            col,
        });
    }
    let var_name = match &items[1] {
        Value::Symbol(name, _) => name.clone(),
        other => {
            return Err(EvalError::TypeError {
                message: format!("string-set!: expected variable, got {}", other.type_name()),
                line,
                col,
            })
        }
    };
    let idx = eval(&items[2], env, output)?.as_integer("string-set!")?;
    let ch = match eval(&items[3], env, output)? {
        Value::Char(c) => c,
        other => {
            return Err(EvalError::TypeError {
                message: format!(
                    "string-set!: expected character, got {}",
                    other.type_name()
                ),
                line,
                col,
            })
        }
    };
    let cell = env.get(&var_name).ok_or_else(|| EvalError::UnboundVariable {
        name: var_name.clone(),
        line,
        col,
    })?;
    let mut val = cell.borrow_mut();
    match &mut *val {
        Value::Str(ref mut string) => {
            let idx = idx as usize;
            if idx >= string.len() {
                return Err(EvalError::TypeError {
                    message: format!(
                        "string-set!: index {} out of range for string of length {}",
                        idx,
                        string.len()
                    ),
                    line,
                    col,
                });
            }
            let mut chars: Vec<char> = string.chars().collect();
            chars[idx] = ch;
            *string = chars.into_iter().collect();
            Ok(Value::Void)
        }
        other => Err(EvalError::TypeError {
            message: format!("string-set!: expected string, got {}", other.type_name()),
            line,
            col,
        }),
    }
}

pub(crate) fn eval_and(
    items: &[Value],
    env: &mut Env,
    output: &mut String,
) -> Result<Tail, EvalError> {
    if items.len() <= 1 {
        return Ok(Tail::Done(Value::Boolean(true)));
    }
    for item in &items[1..items.len() - 1] {
        let val = eval(item, env, output)?;
        if !val.is_truthy() {
            return Ok(Tail::Done(val));
        }
    }
    Ok(Tail::Expr(items[items.len() - 1].clone()))
}

pub(crate) fn eval_or(
    items: &[Value],
    env: &mut Env,
    output: &mut String,
) -> Result<Tail, EvalError> {
    if items.len() <= 1 {
        return Ok(Tail::Done(Value::Boolean(false)));
    }
    for item in &items[1..items.len() - 1] {
        let val = eval(item, env, output)?;
        if val.is_truthy() {
            return Ok(Tail::Done(val));
        }
    }
    Ok(Tail::Expr(items[items.len() - 1].clone()))
}

pub(crate) fn eval_cond(
    clauses: &[Value],
    env: &mut Env,
    output: &mut String,
) -> Result<Tail, EvalError> {
    for clause in clauses {
        match clause {
            Value::List(items, _) if !items.is_empty() => {
                if let Value::Symbol(s, _) = &items[0] {
                    if s == "else" {
                        if items.len() <= 1 {
                            return Ok(Tail::Done(Value::Boolean(false)));
                        }
                        for expr in &items[1..items.len() - 1] {
                            eval(expr, env, output)?;
                        }
                        return Ok(Tail::Expr(items[items.len() - 1].clone()));
                    }
                }
                let test = eval(&items[0], env, output)?;
                if test.is_truthy() {
                    if items.len() == 1 {
                        return Ok(Tail::Done(test));
                    }
                    for expr in &items[1..items.len() - 1] {
                        eval(expr, env, output)?;
                    }
                    return Ok(Tail::Expr(items[items.len() - 1].clone()));
                }
            }
            other => {
                return Err(EvalError::Parse {
                    message: format!("cond: bad clause: {}", other.display()),
                    line: other.span().0,
                    col: other.span().1,
                })
            }
        }
    }
    Ok(Tail::Done(Value::Boolean(false)))
}

pub(crate) fn eval_lambda(
    items: &[Value],
    env: &Env,
    span: Span,
) -> Result<Value, EvalError> {
    let (line, col) = span;
    if items.len() < 3 {
        return Err(EvalError::Parse {
            message: "lambda requires params and body".to_string(),
            line,
            col,
        });
    }
    let (params, rest_param) = match &items[1] {
        Value::List(param_list, _) => parse_params_with_rest(param_list, (line, col))?,
        Value::Symbol(s, _) => {
            // (lambda args body) — single symbol captures all args
            (Vec::new(), Some(s.clone()))
        }
        other => {
            return Err(EvalError::TypeError {
                message: format!(
                    "lambda: expected parameter list, got {}",
                    other.type_name()
                ),
                line,
                col,
            })
        }
    };
    let body: Vec<Value> = items[2..].to_vec();
    Ok(Value::Lambda {
        name: None,
        params,
        rest_param,
        body,
        closure_env: env.clone(),
    })
}

fn is_define_form(expr: &Value) -> bool {
    if let Value::List(items, _) = expr {
        if let Some(Value::Symbol(s, _)) = items.first() {
            return s == "define";
        }
    }
    false
}

fn patch_closures(env: &mut Env) {
    let snapshot = env.clone();
    for cell in env.values() {
        let mut val = cell.borrow_mut();
        if let Value::Lambda { closure_env, .. } = &mut *val {
            for (k, v) in &snapshot {
                closure_env.entry(k.clone()).or_insert_with(|| v.clone());
            }
        }
    }
}

pub(crate) fn apply_lambda(
    func: Value,
    args: Vec<Value>,
    caller_env: &Env,
    span: Span,
    output: &mut String,
) -> Result<Tail, EvalError> {
    let (line, col) = span;
    match func {
        Value::Lambda {
            ref name,
            ref params,
            ref rest_param,
            ref body,
            ref closure_env,
        } => {
            match rest_param {
                Some(_) => {
                    if args.len() < params.len() {
                        return Err(EvalError::Arity {
                            procedure: name.as_deref().unwrap_or("lambda").to_string(),
                            expected: format!("at least {}", params.len()),
                            got: args.len(),
                            line,
                            col,
                        });
                    }
                }
                None => {
                    if params.len() != args.len() {
                        return Err(EvalError::Arity {
                            procedure: name.as_deref().unwrap_or("lambda").to_string(),
                            expected: params.len().to_string(),
                            got: args.len(),
                            line,
                            col,
                        });
                    }
                }
            }
            let mut new_env = closure_env.clone();
            for (k, v) in caller_env.iter() {
                new_env.entry(k.clone()).or_insert_with(|| v.clone());
            }
            if let Some(fn_name) = name {
                new_env.insert(fn_name.clone(), wrap(func.clone()));
            }
            let mut args_iter = args.into_iter();
            for param in params {
                new_env.insert(
                    param.clone(),
                    wrap(args_iter.next().expect("arity checked")),
                );
            }
            if let Some(rest_name) = rest_param {
                let rest_vals: Vec<Value> = args_iter.collect();
                new_env.insert(rest_name.clone(), wrap(Value::List(rest_vals, (0, 0))));
            }

            // Scan for internal defines
            let mut body_start = 0;
            for (i, expr) in body.iter().enumerate() {
                if is_define_form(expr) {
                    body_start = i + 1;
                    continue;
                }
                break;
            }

            if body_start > 0 {
                for item in body.iter().take(body_start) {
                    eval(item, &mut new_env, output)?;
                }
                patch_closures(&mut new_env);
            }

            if body.len() > body_start {
                for expr in &body[body_start..body.len() - 1] {
                    eval(expr, &mut new_env, output)?;
                }
                Ok(Tail::Call {
                    expr: body[body.len() - 1].clone(),
                    env: new_env,
                })
            } else {
                Ok(Tail::Done(Value::Boolean(false)))
            }
        }
        Value::Builtin { ref name } => {
            if name == "apply" {
                if args.len() < 2 {
                    return Err(EvalError::Arity {
                        procedure: "apply".to_string(),
                        expected: "at least 2".to_string(),
                        got: args.len(),
                        line,
                        col,
                    });
                }
                let inner_func = args[0].clone();
                let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
                match &args[args.len() - 1] {
                    Value::List(elems, _) => all_args.extend(elems.iter().cloned()),
                    other => {
                        return Err(EvalError::TypeError {
                            message: format!(
                                "apply: last argument must be a list, got {}",
                                other.type_name()
                            ),
                            line,
                            col,
                        })
                    }
                }
                apply_lambda(inner_func, all_args, caller_env, span, output)
            } else {
                Ok(Tail::Done(dispatch_builtin(name, args, span, output)?))
            }
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {}", other.display()),
            line,
            col,
        }),
    }
}
