use super::{eval, is_truthy, Bounce, Env};
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) fn eval_define(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name, _), expr] => {
            let val = eval(expr, env, output)?;
            env.insert(name.clone(), Rc::new(RefCell::new(val)));
            Ok(Value::Symbol(name.clone(), span))
        }
        // (define (f params...) body...)
        [Value::List(signature, _), body @ ..] if !signature.is_empty() && !body.is_empty() => {
            let [Value::Symbol(name, _), param_vals @ ..] = signature.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define: first element of signature must be a symbol".to_string(),
                    span,
                });
            };
            let params = extract_params(param_vals, span)?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                env: env.clone(),
            };
            env.insert(name.clone(), Rc::new(RefCell::new(lambda)));
            Ok(Value::Symbol(name.clone(), span))
        }
        _ => Err(EvalError::Parse {
            message: "define requires a symbol and an expression".to_string(),
            span,
        }),
    }
}

pub(crate) fn eval_if_step(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    let [condition, consequent, alternative] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
            span,
        });
    };
    let cond_val = eval(condition, env, output)?;
    if is_truthy(&cond_val) {
        Ok(Bounce::Continue(consequent.clone()))
    } else {
        Ok(Bounce::Continue(alternative.clone()))
    }
}

pub(crate) fn eval_quote(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
            span,
        });
    };
    Ok(expr.clone())
}

pub(crate) fn eval_lambda(args: &[Value], env: &Env, span: Span) -> Result<Value, EvalError> {
    let [Value::List(param_list, _), body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "lambda requires a parameter list and body".to_string(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "lambda requires a body".to_string(),
            span,
        });
    }
    let params = extract_params(param_list, span)?;
    Ok(Value::Lambda {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })
}

pub(crate) fn eval_let_step(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    let [Value::List(bindings, _), body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "let requires a bindings list and body".to_string(),
            span,
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "let requires a body".to_string(),
            span,
        });
    }
    let mut local_env = env.clone();
    for binding in bindings {
        let Value::List(pair, _) = binding else {
            return Err(EvalError::Parse {
                message: "let binding must be a list".to_string(),
                span,
            });
        };
        let [Value::Symbol(name, _), expr] = pair.as_slice() else {
            return Err(EvalError::Parse {
                message: "let binding must be (name expr)".to_string(),
                span,
            });
        };
        let val = eval(expr, env, output)?;
        local_env.insert(name.clone(), Rc::new(RefCell::new(val)));
    }
    eval_body_step(body, Value::Boolean(false), &mut local_env, output)
        .map(|b| match b {
            Bounce::Continue(expr) => Bounce::ReplaceEnv {
                expr,
                env: local_env,
            },
            other => other,
        })
}

pub(crate) fn eval_begin_step(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "begin requires at least one expression".to_string(),
            span,
        });
    }
    eval_body_step(args, Value::Boolean(false), env, output)
}

pub(crate) fn eval_cond_step(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    for clause in args {
        let Value::List(items, _) = clause else {
            return Err(EvalError::Parse {
                message: "cond clause must be a list".to_string(),
                span,
            });
        };
        let [test, body @ ..] = items.as_slice() else {
            return Err(EvalError::Parse {
                message: "cond clause must have a test and body".to_string(),
                span,
            });
        };
        if matches!(test, Value::Symbol(s, _) if s == "else") {
            return eval_body_step(body, Value::Boolean(false), env, output);
        }
        let test_val = eval(test, env, output)?;
        if is_truthy(&test_val) {
            return eval_body_step(body, test_val, env, output);
        }
    }
    Ok(Bounce::Done(Value::Boolean(false)))
}

pub(crate) fn eval_and(
    args: &[Value],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(crate) fn eval_or(
    args: &[Value],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(crate) fn eval_body_step(
    body: &[Value],
    default: Value,
    env: &mut Env,
    output: &mut String,
) -> Result<Bounce, EvalError> {
    let Some((last, rest)) = body.split_last() else {
        return Ok(Bounce::Done(default));
    };
    for expr in rest {
        eval(expr, env, output)?;
    }
    Ok(Bounce::Continue(last.clone()))
}

pub(crate) fn eval_set(
    args: &[Value],
    env: &mut Env,
    span: Span,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name, name_span), expr] = args else {
        return Err(EvalError::Parse {
            message: "set! requires a variable name and an expression".to_string(),
            span,
        });
    };
    let val = eval(expr, env, output)?;
    let Some(cell) = env.get(name) else {
        return Err(EvalError::UnboundVariable {
            name: name.clone(),
            span: *name_span,
        });
    };
    *cell.borrow_mut() = val;
    Ok(Value::Boolean(false))
}

pub(crate) fn eval_string_set(
    _args: &[Value],
    _env: &mut Env,
    span: Span,
    _output: &mut String,
) -> Result<Value, EvalError> {
    Err(EvalError::TypeError {
        expected: "mutable string".to_string(),
        got: "immutable string (strings are immutable in R7RS)".to_string(),
        span,
    })
}

fn extract_params(param_vals: &[Value], span: Span) -> Result<Vec<String>, EvalError> {
    param_vals
        .iter()
        .map(|v| match v {
            Value::Symbol(s, _) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: format!("{other}"),
                span,
            }),
        })
        .collect()
}
