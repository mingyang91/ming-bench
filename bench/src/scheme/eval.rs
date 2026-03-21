use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// Check if a name is a builtin procedure.
fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
    )
}

/// Evaluate a single expression in the given environment.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => match env.borrow().get(name) {
            Some(val) => Ok(val),
            None if is_builtin(name) => Ok(Value::Symbol(name.clone())),
            None => Err(EvalError::UnboundVariable {
                name: name.clone(),
                span,
            }),
        },
        Value::List(elems) => eval_list(elems, env, span),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let [head, args @ ..] = elems else {
        return Ok(Value::List(vec![]));
    };

    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(args, env, span),
            "or" => return eval_or(args, env, span),
            "if" => return eval_if(args, env, span),
            "define" => return eval_define(args, env, span),
            "quote" => return eval_quote(args, span),
            "lambda" => return eval_lambda(args, env, span),
            "let" => return eval_let(args, env, span),
            "begin" => return eval_begin(args, env, span),
            "cond" => return eval_cond(args, env, span),
            _ => {}
        }
    }

    let proc = eval(head, env, span)?;
    let evaluated_args: Vec<Value> =
        args.iter().map(|a| eval(a, env, span)).collect::<Result<_, _>>()?;
    apply(&proc, &evaluated_args, span)
}

fn apply(proc: &Value, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match proc {
        Value::Symbol(name) => apply_builtin(name, args, span),
        Value::Lambda { params, body, env } => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                    span,
                });
            }
            let local_env = Env::with_parent(env);
            for (param, arg) in params.iter().zip(args) {
                local_env.borrow_mut().define(param.clone(), arg.clone());
            }
            body.iter()
                .try_fold(Value::Void, |_, expr| eval(expr, &local_env, span))
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {other}"),
            span,
        }),
    }
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, span)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, span)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let (condition, consequent, alternate) = match args {
        [cond, cons, alt] => (cond, cons, Some(alt)),
        [cond, cons] => (cond, cons, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 2,
                got: args.len(),
                span,
            });
        }
    };

    if is_truthy(&eval(condition, env, span)?) {
        eval(consequent, env, span)
    } else if let Some(alt) = alternate {
        eval(alt, env, span)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    match args {
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env, span)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        [Value::List(sig), body @ ..] if !sig.is_empty() => {
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::TypeError {
                    message: "define: expected function name".into(),
                    span,
                });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|v| match v {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeError {
                        message: format!("define: expected parameter name, got {other}"),
                        span,
                    }),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                env: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::TypeError {
            message: "define: invalid syntax".into(),
            span,
        }),
    }
}

fn eval_quote(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    Ok(datum.clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let [Value::List(param_list), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "lambda: expected parameter list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "lambda: expected body".into(),
            span,
        });
    }
    let params: Vec<String> = param_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                message: format!("lambda: expected parameter name, got {other}"),
                span,
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

fn eval_let(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    let [Value::List(bindings), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "let: expected bindings list".into(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "let: expected body".into(),
            span,
        });
    }
    let local_env = Env::with_parent(env);
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::TypeError {
                message: "let: binding must be a list".into(),
                span,
            });
        };
        let [Value::Symbol(name), val_expr] = pair.as_slice() else {
            return Err(EvalError::TypeError {
                message: "let: binding must be (name expr)".into(),
                span,
            });
        };
        let val = eval(val_expr, env, span)?;
        local_env.borrow_mut().define(name.clone(), val);
    }
    body.iter()
        .try_fold(Value::Void, |_, expr| eval(expr, &local_env, span))
}

fn eval_begin(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(Value::Void, |_, expr| eval(expr, env, span))
}

fn eval_cond(args: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    for clause in args {
        let Value::List(elems) = clause else {
            return Err(EvalError::TypeError {
                message: "cond: clause must be a list".into(),
                span,
            });
        };
        let [test, body @ ..] = elems.as_slice() else {
            return Err(EvalError::TypeError {
                message: "cond: empty clause".into(),
                span,
            });
        };
        if matches!(test, Value::Symbol(s) if s == "else") || is_truthy(&eval(test, env, span)?) {
            return eval_body(body, env, span);
        }
    }
    Ok(Value::Void)
}

fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>, span: Span) -> Result<Value, EvalError> {
    body.iter()
        .try_fold(Value::Void, |_, expr| eval(expr, env, span))
}

fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b), span),
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b), span),
        "-" => eval_sub(args, span),
        "/" => eval_div(args, span),
        "<" => compare_op(args, |a, b| a < b, span),
        ">" => compare_op(args, |a, b| a > b, span),
        "=" => compare_op(args, |a, b| a == b, span),
        "<=" => compare_op(args, |a, b| a <= b, span),
        ">=" => compare_op(args, |a, b| a >= b, span),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        "cons" => {
            let [car, cdr] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    got: args.len(),
                    span,
                });
            };
            match cdr {
                Value::List(elems) => {
                    let mut new_list = vec![car.clone()];
                    new_list.extend(elems.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Ok(Value::List(vec![car.clone(), cdr.clone()])),
            }
        }
        "car" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                _ => Err(EvalError::TypeError {
                    message: format!("car: expected non-empty pair, got {arg}"),
                    span,
                }),
            }
        }
        "cdr" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
                _ => Err(EvalError::TypeError {
                    message: format!("cdr: expected non-empty pair, got {arg}"),
                    span,
                }),
            }
        }
        "null?" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            Ok(Value::Boolean(matches!(
                arg,
                Value::List(elems) if elems.is_empty()
            )))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                    span,
                });
            };
            match arg {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::TypeError {
                    message: format!("length: expected list, got {arg}"),
                    span,
                }),
            }
        }
        "string?" => Ok(Value::Boolean(matches!(args, [Value::String(_)]))),
        "number?" => Ok(Value::Boolean(matches!(args, [Value::Integer(_)]))),
        "boolean?" => Ok(Value::Boolean(matches!(args, [Value::Boolean(_)]))),
        "pair?" => Ok(Value::Boolean(
            matches!(args, [Value::List(e)] if !e.is_empty()),
        )),
        "symbol?" => Ok(Value::Boolean(matches!(args, [Value::Symbol(_)]))),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
            span,
        }),
    }
}

fn require_integer(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            message: format!("expected integer, got {other}"),
            span,
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
    span: Span,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, val| op(acc, require_integer(val, span)?))
        .map(Value::Integer)
}

fn eval_sub(args: &[Value], span: Span) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
            span,
        }),
        [single] => Ok(Value::Integer(-require_integer(single, span)?)),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first, span)?, |acc, val| {
                Ok(acc - require_integer(val, span)?)
            })
            .map(Value::Integer),
    }
}

fn checked_div(a: i64, b: i64, span: Span) -> Result<i64, EvalError> {
    if b == 0 {
        return Err(EvalError::DivisionByZero { span });
    }
    Ok(a / b)
}

fn eval_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
            span,
        }),
        [single] => checked_div(1, require_integer(single, span)?, span).map(Value::Integer),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first, span)?, |acc, val| {
                checked_div(acc, require_integer(val, span)?, span)
            })
            .map(Value::Integer),
    }
}

fn compare_op(
    args: &[Value],
    op: impl Fn(i64, i64) -> bool,
    span: Span,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
            span,
        });
    }

    let nums: Vec<i64> = args
        .iter()
        .map(|v| require_integer(v, span))
        .collect::<Result<_, _>>()?;
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}
