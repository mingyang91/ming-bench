use crate::scheme::error::EvalError;
use crate::scheme::{eval, Env, Value};
use crate::scheme::parser::Span;

pub(crate) fn eval_let(args: &[Value], env: &mut Env, span: Span) -> Result<Value, EvalError> {
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
                                message: format!("let: expected symbol, got {}", other.type_name()),
                                line,
                                col,
                            })
                        }
                    }
                    init_vals.push(eval(&pair[1], env)?);
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
            params,
            body,
            closure_env: env.clone(),
        };
        let mut local_env = env.clone();
        local_env.insert(name.clone(), lambda.clone());
        // Apply the named lambda with initial values
        match &lambda {
            Value::Lambda { params, body, .. } => {
                for (param, val) in params.iter().zip(init_vals.iter()) {
                    local_env.insert(param.clone(), val.clone());
                }
                let mut result = Value::Boolean(false);
                for expr in body {
                    result = eval(expr, &mut local_env)?;
                }
                Ok(result)
            }
            Value::Integer(_)
            | Value::Boolean(_)
            | Value::Str(_)
            | Value::Symbol(..)
            | Value::List(..) => unreachable!(),
        }
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
                                message: format!("let: expected symbol, got {}", other.type_name()),
                                line,
                                col,
                            })
                        }
                    };
                    let val = eval(&pair[1], env)?;
                    local_env.insert(name, val);
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
        let mut result = Value::Boolean(false);
        for expr in &args[1..] {
            result = eval(expr, &mut local_env)?;
        }
        Ok(result)
    }
}

pub(crate) fn eval_cond(clauses: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Value::List(items, _) if !items.is_empty() => {
                // Check for else clause
                if let Value::Symbol(s, _) = &items[0] {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &items[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env)?;
                if test.is_truthy() {
                    if items.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &items[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
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
    Ok(Value::Boolean(false))
}
