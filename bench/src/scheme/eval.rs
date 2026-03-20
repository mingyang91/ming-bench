use super::{Env, EvalError, Value};

pub(super) fn eval(value: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Lambda { .. } => {
            Ok(value.clone())
        }
        Value::Nil => Ok(Value::Nil),
        Value::Symbol(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| EvalError::UnboundVariable {
                name: name.clone(),
            }),
        Value::Pair(..) => eval_pair(value, env),
    }
}

fn eval_pair(value: &Value, env: &mut Env) -> Result<Value, EvalError> {
    let items = value.to_list_vec().ok_or_else(|| EvalError::TypeError {
        expected: "proper list".to_string(),
        got: value.display(),
    })?;

    let [operator, args @ ..] = items.as_slice() else {
        return Ok(Value::Nil);
    };

    // Special forms
    if let Value::Symbol(name) = operator {
        return match name.as_str() {
            "define" => eval_define(args, env),
            "if" => eval_if(args, env),
            "quote" => eval_quote(args),
            "lambda" => eval_lambda(args, env),
            _ => return eval_symbol_call(name, args, env),
        };
    }

    // Evaluate operator and apply
    let proc = eval(operator, env)?;
    let evaled_args: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    apply(&proc, &evaled_args, env)
}

fn eval_symbol_call(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if is_builtin(name) {
        return eval_builtin(name, args, env);
    }
    let proc = env
        .get(name)
        .cloned()
        .ok_or_else(|| EvalError::UnboundVariable {
            name: name.to_string(),
        })?;
    let evaled_args: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    apply(&proc, &evaled_args, env)
}

fn apply(proc: &Value, args: &[Value], caller_env: &Env) -> Result<Value, EvalError> {
    let Value::Lambda {
        params,
        body,
        closure,
    } = proc
    else {
        return Err(EvalError::NotAProcedure {
            value: proc.display(),
        });
    };

    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }

    // Build call env: start with captured closure, merge caller env as fallback, add params
    let mut call_env = closure.clone();
    for (k, v) in caller_env {
        call_env.entry(k.clone()).or_insert_with(|| v.clone());
    }
    for (param, arg) in params.iter().zip(args) {
        call_env.insert(param.clone(), arg.clone());
    }

    eval(body, &mut call_env)
}

fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.insert(name.clone(), val);
            Ok(Value::Symbol(name.clone()))
        }
        // (define (name params...) body) → (define name (lambda (params...) body))
        [Value::Pair(..), body @ ..] => {
            let header = args[0].to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "proper list".to_string(),
                got: args[0].display(),
            })?;
            let [Value::Symbol(name), param_vals @ ..] = header.as_slice() else {
                return Err(EvalError::TypeError {
                    expected: "symbol as function name".to_string(),
                    got: header[0].display(),
                });
            };
            let params: Vec<String> = param_vals
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeError {
                        expected: "symbol".to_string(),
                        got: other.display(),
                    }),
                })
                .collect::<Result<_, _>>()?;

            let func_body = if body.len() == 1 {
                body[0].clone()
            } else {
                // Wrap multiple body exprs in (begin ...)
                // For now just use last expression since we don't have begin yet
                body.last()
                    .ok_or(EvalError::WrongArgCount {
                        expected: 1,
                        got: 0,
                    })?
                    .clone()
            };

            let lambda = Value::Lambda {
                params,
                body: Box::new(func_body),
                closure: env.clone(),
            };
            env.insert(name.clone(), lambda);
            Ok(Value::Symbol(name.clone()))
        }
        _ => Err(EvalError::TypeError {
            expected: "symbol and value".to_string(),
            got: format!("{} args", args.len()),
        }),
    }
}

fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
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
    let test = eval(condition, env)?;
    if test != Value::Boolean(false) {
        eval(consequent, env)
    } else {
        alternative.map_or(Ok(Value::Nil), |alt| eval(alt, env))
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

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    let [param_list, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let param_vals = param_list
        .to_list_vec()
        .ok_or_else(|| EvalError::TypeError {
            expected: "parameter list".to_string(),
            got: param_list.display(),
        })?;

    let params: Vec<String> = param_vals
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: other.display(),
            }),
        })
        .collect::<Result<_, _>>()?;

    let func_body = if body.len() == 1 {
        body[0].clone()
    } else {
        body.last()
            .ok_or(EvalError::WrongArgCount {
                expected: 2,
                got: 1,
            })?
            .clone()
    };

    Ok(Value::Lambda {
        params,
        body: Box::new(func_body),
        closure: env.clone(),
    })
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" | "and" | "or"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
    )
}

fn eval_builtin(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => eval_arithmetic(name, args, env),
        "<" | ">" | "=" | "<=" | ">=" => eval_comparison(name, args, env),
        "not" => eval_not(args, env),
        "and" => eval_and(args, env),
        "or" => eval_or(args, env),
        "cons" | "car" | "cdr" | "null?" | "list" | "length" => eval_list_builtin(name, args, env),
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

fn eval_arithmetic(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let values: Vec<i64> = args
        .iter()
        .map(|a| {
            let evaled = eval(a, env)?;
            match evaled {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".to_string(),
                    got: other.display(),
                }),
            }
        })
        .collect::<Result<_, _>>()?;

    let result = match op {
        "+" => values.iter().sum(),
        "*" => values.iter().product(),
        "-" => {
            let [first, rest @ ..] = values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                });
            };
            if rest.is_empty() {
                -first
            } else {
                rest.iter().fold(*first, |acc, &v| acc - v)
            }
        }
        "/" => {
            let [first, rest @ ..] = values.as_slice() else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: 0,
                });
            };
            rest.iter()
                .try_fold(*first, |acc, &v| checked_div(acc, v))?
        }
        _ => unreachable!("unexpected arithmetic operator: {op}"),
    };

    Ok(Value::Integer(result))
}

fn eval_comparison(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let lhs = match eval(lhs, env)? {
        Value::Integer(n) => n,
        other => {
            return Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: other.display(),
            })
        }
    };
    let rhs = match eval(rhs, env)? {
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

fn eval_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_list_builtin(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            let [a, b] = args else {
                return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
            };
            let car = eval(a, env)?;
            let cdr = eval(b, env)?;
            Ok(Value::Pair(Box::new(car), Box::new(cdr)))
        }
        "car" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let val = eval(a, env)?;
            let Value::Pair(car, _) = val else {
                return Err(EvalError::TypeError { expected: "pair".to_string(), got: val.display() });
            };
            Ok(*car)
        }
        "cdr" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let val = eval(a, env)?;
            let Value::Pair(_, cdr) = val else {
                return Err(EvalError::TypeError { expected: "pair".to_string(), got: val.display() });
            };
            Ok(*cdr)
        }
        "null?" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let val = eval(a, env)?;
            Ok(Value::Boolean(matches!(val, Value::Nil)))
        }
        "list" => {
            let vals: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            Ok(vals.into_iter().rev().fold(Value::Nil, |acc, v| {
                Value::Pair(Box::new(v), Box::new(acc))
            }))
        }
        "length" => {
            let [a] = args else {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            };
            let val = eval(a, env)?;
            let items = val.to_list_vec().ok_or_else(|| EvalError::TypeError {
                expected: "proper list".to_string(),
                got: val.display(),
            })?;
            Ok(Value::Integer(items.len() as i64))
        }
        _ => unreachable!("unexpected list builtin: {name}"),
    }
}
