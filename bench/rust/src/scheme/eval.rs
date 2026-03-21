use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate a parsed expression in the given environment.
pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::String(s, _) => Ok(Value::String(s.clone())),
        Expr::Symbol(name, span) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }.at(*span)),
        Expr::List(elems, span) => eval_list(elems, *span, env),
    }
}

fn eval_list(elems: &[Expr], span: Span, env: &Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = elems else {
        return Err(EvalError::Parse("empty list".into()).at(span));
    };

    // Check for special forms first (symbol-based)
    if let Expr::Symbol(op, _) = operator {
        match op.as_str() {
            "quote" => return eval_quote(args).map_err(|e| e.at(span)),
            "if" => return eval_if(args, env).map_err(|e| e.at(span)),
            "define" => return eval_define(args, span, env),
            "lambda" => return eval_lambda(args, env).map_err(|e| e.at(span)),
            "let" => return eval_let(args, env).map_err(|e| e.at(span)),
            "begin" => return eval_begin(args, env).map_err(|e| e.at(span)),
            "cond" => return eval_cond(args, env).map_err(|e| e.at(span)),
            _ => {}
        }
    }

    // Try built-in operators for symbol forms
    if let Expr::Symbol(op, _) = operator {
        if is_builtin(op) {
            return eval_builtin(op, args, env).map_err(|e| e.at(span));
        }
    }

    // Evaluate operator to get a callable value
    let op_val = eval(operator, env)?;
    apply(op_val, args, env).map_err(|e| e.at(span))
}

fn apply(op_val: Value, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let Value::Lambda {
        params,
        body,
        closure,
    } = op_val
    else {
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
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
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
        Expr::Symbol(s, _) => Ok(Value::Symbol(s.clone())),
        Expr::List(items, _) => {
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
            let wrapped_body = if body.len() == 1 {
                body[0].clone()
            } else {
                let mut begin = vec![Expr::Symbol("begin".into(), span)];
                begin.extend(body.iter().cloned());
                Expr::List(begin, span)
            };
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

fn eval_let(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let [Expr::List(bindings, _), body @ ..] = args else {
        return Err(EvalError::Parse("invalid let form".into()));
    };
    if body.is_empty() {
        return Err(EvalError::Parse("let requires a body".into()));
    }
    let let_env = Env::extend(env);
    for binding in bindings {
        let Expr::List(pair, _) = binding else {
            return Err(EvalError::Parse("let binding must be a list".into()));
        };
        let [Expr::Symbol(name, _), val_expr] = pair.as_slice() else {
            return Err(EvalError::Parse("let binding must be (name expr)".into()));
        };
        let val = eval(val_expr, env)?;
        let_env.define(name.clone(), val);
    }
    eval_body(body, &let_env)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    eval_body(args, env)
}

fn eval_body(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in args {
        let Expr::List(parts, _) = clause else {
            return Err(EvalError::Parse("cond clause must be a list".into()));
        };
        let [test, body @ ..] = parts.as_slice() else {
            return Err(EvalError::Parse("cond clause must have a test".into()));
        };
        if matches!(test, Expr::Symbol(s, _) if s == "else") {
            return eval_body(body, env);
        }
        let test_val = eval(test, env)?;
        if test_val.is_truthy() {
            return eval_body(body, env);
        }
    }
    Ok(Value::Void)
}
