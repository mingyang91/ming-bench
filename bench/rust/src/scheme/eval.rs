use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate a parsed expression in the given environment.
pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(elems) => eval_list(elems, env),
    }
}

fn eval_list(elems: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = elems else {
        return Err(EvalError::Parse("empty list".into()));
    };

    // Check for special forms first (symbol-based)
    if let Expr::Symbol(op) = operator {
        match op.as_str() {
            "quote" => return eval_quote(args),
            "if" => return eval_if(args, env),
            "define" => return eval_define(args, env),
            "lambda" => return eval_lambda(args, env),
            _ => {}
        }
    }

    // Try built-in operators for symbol forms
    if let Expr::Symbol(op) = operator {
        if is_builtin(op) {
            return eval_builtin(op, args, env);
        }
    }

    // Evaluate operator to get a callable value
    let op_val = eval(operator, env)?;
    apply(op_val, args, env)
}

fn apply(op_val: Value, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let Value::Lambda { params, body, closure } = op_val else {
        return Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{op_val}"),
        });
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len(),
            got: args.len(),
        });
    }
    let call_env = Env::extend(&closure);
    for (param, arg_expr) in params.iter().zip(args) {
        let arg_val = eval(arg_expr, env)?;
        call_env.define(param.clone(), arg_val);
    }
    eval(&body, &call_env)
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" | "and" | "or"
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
        "and" => eval_and(args, env),
        "or" => eval_or(args, env),
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
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(s) => Ok(Value::String(s.clone())),
        Expr::List(items) => {
            let vals: Vec<Value> = items.iter().map(expr_to_value).collect::<Result<_, _>>()?;
            Ok(Value::List(vals))
        }
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let (cond, consequent, alternate) = match args {
        [c, t, f] => (c, t, Some(f)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: 3,
                got: args.len(),
            })
        }
    };

    let cond_val = eval(cond, env)?;
    if cond_val.is_truthy() {
        eval(consequent, env)
    } else if let Some(alt) = alternate {
        eval(alt, env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Expr::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body)
        [Expr::List(name_and_params), body] => {
            let [Expr::Symbol(name), params @ ..] = name_and_params.as_slice() else {
                return Err(EvalError::Parse("invalid define form".into()));
            };
            let param_names: Vec<String> = params
                .iter()
                .map(|p| match p {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse("parameter must be a symbol".into())),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                params: param_names,
                body: body.clone(),
                closure: env.clone(),
            };
            env.define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("invalid define form".into())),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [Expr::List(params), body] = args else {
        return Err(EvalError::Parse("invalid lambda form".into()));
    };
    let param_names: Vec<String> = params
        .iter()
        .map(|p| {
            if let Expr::Symbol(s) = p {
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

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}
