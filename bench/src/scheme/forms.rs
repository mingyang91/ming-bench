use super::{eval, is_truthy, Env};
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub(crate) fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.insert(name.clone(), val);
            Ok(Value::Symbol(name.clone()))
        }
        // (define (f params...) body...)
        [Value::List(signature), body @ ..] if !signature.is_empty() && !body.is_empty() => {
            let [Value::Symbol(name), param_vals @ ..] = signature.as_slice() else {
                return Err(EvalError::Parse {
                    message: "define: first element of signature must be a symbol".to_string(),
                });
            };
            let params = extract_params(param_vals)?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                env: env.clone(),
            };
            env.insert(name.clone(), lambda);
            Ok(Value::Symbol(name.clone()))
        }
        _ => Err(EvalError::Parse {
            message: "define requires a symbol and an expression".to_string(),
        }),
    }
}

pub(crate) fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [condition, consequent, alternative] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let cond_val = eval(condition, env)?;
    if is_truthy(&cond_val) {
        eval(consequent, env)
    } else {
        eval(alternative, env)
    }
}

pub(crate) fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(expr.clone())
}

pub(crate) fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let [Value::List(param_list), body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "lambda requires a parameter list and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "lambda requires a body".to_string(),
        });
    }
    let params = extract_params(param_list)?;
    Ok(Value::Lambda {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })
}

pub(crate) fn eval_let(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [Value::List(bindings), body @ ..] = args else {
        return Err(EvalError::Parse {
            message: "let requires a bindings list and body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::Parse {
            message: "let requires a body".to_string(),
        });
    }
    let mut local_env = env.clone();
    for binding in bindings {
        let Value::List(pair) = binding else {
            return Err(EvalError::Parse {
                message: "let binding must be a list".to_string(),
            });
        };
        let [Value::Symbol(name), expr] = pair.as_slice() else {
            return Err(EvalError::Parse {
                message: "let binding must be (name expr)".to_string(),
            });
        };
        let val = eval(expr, env)?;
        local_env.insert(name.clone(), val);
    }
    eval_body(body, Value::Boolean(false), &mut local_env)
}

pub(crate) fn eval_begin(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "begin requires at least one expression".to_string(),
        });
    }
    eval_body(args, Value::Boolean(false), env)
}

pub(crate) fn eval_cond(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    for clause in args {
        let Value::List(items) = clause else {
            return Err(EvalError::Parse {
                message: "cond clause must be a list".to_string(),
            });
        };
        let [test, body @ ..] = items.as_slice() else {
            return Err(EvalError::Parse {
                message: "cond clause must have a test and body".to_string(),
            });
        };
        if matches!(test, Value::Symbol(s) if s == "else") {
            return eval_body(body, Value::Boolean(false), env);
        }
        let test_val = eval(test, env)?;
        if is_truthy(&test_val) {
            return eval_body(body, test_val, env);
        }
    }
    Ok(Value::Boolean(false))
}

pub(crate) fn eval_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(crate) fn eval_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(crate) fn eval_body(body: &[Value], default: Value, env: &mut Env) -> Result<Value, EvalError> {
    body.iter().try_fold(default, |_, expr| eval(expr, env))
}

fn extract_params(param_vals: &[Value]) -> Result<Vec<String>, EvalError> {
    param_vals
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}
