use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Evaluate a single parsed expression in the given environment.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Void => Ok(expr.clone()),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env.borrow().get(name).ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => eval_list(items, env),
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
    )
}

fn eval_list(items: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Ok(Value::List(vec![]));
    }

    // Check for special forms and builtins when head is a symbol
    if let Value::Symbol(name) = &items[0] {
        match name.as_str() {
            "define" => return eval_define(&items[1..], env),
            "if" => return eval_if(&items[1..], env),
            "quote" => return eval_quote(&items[1..]),
            "lambda" => return eval_lambda(&items[1..], env),
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            s if is_builtin(s) => return eval_builtin(s, &items[1..], env),
            _ => {}
        }
    }

    // Evaluate head to get the procedure
    let proc = eval(&items[0], env)?;
    let args = &items[1..];

    call_proc(&proc, args, env)
}

fn call_proc(
    proc: &Value,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    match proc {
        Value::Lambda { params, body, closure } => {
            let eval_args: Vec<Value> = args
                .iter()
                .map(|a| eval(a, env))
                .collect::<Result<_, _>>()?;
            if eval_args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: eval_args.len(),
                });
            }
            let child = Env::extend(closure);
            for (param, val) in params.iter().zip(eval_args) {
                child.borrow_mut().define(param.clone(), val);
            }
            eval_body(body, &child)
        }
        _ => Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{proc}"),
        }),
    }
}

fn eval_builtin(
    name: &str,
    args: &[Value],
    env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args, env),
        "-" => eval_sub(args, env),
        "*" => eval_mul(args, env),
        "/" => eval_div(args, env),
        "<" => eval_cmp(args, env, |a, b| a < b),
        ">" => eval_cmp(args, env, |a, b| a > b),
        "=" => eval_cmp(args, env, |a, b| a == b),
        "<=" => eval_cmp(args, env, |a, b| a <= b),
        ">=" => eval_cmp(args, env, |a, b| a >= b),
        "not" => eval_not(args, env),
        _ => Err(EvalError::UnknownProcedure {
            name: name.into(),
        }),
    }
}

/// Evaluate a sequence of body expressions, returning the last.
fn eval_body(body: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in body {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match args {
        // (define x expr)
        [Value::Symbol(name), expr] => {
            let val = eval(expr, env)?;
            env.borrow_mut().define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        [Value::List(sig), body @ ..] if !sig.is_empty() => {
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::Parse {
                    message: "define: expected function name".into(),
                });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::Parse {
                        message: format!("define: expected parameter name, got {other}"),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let lambda = Value::Lambda {
                params,
                body: body.to_vec(),
                closure: Rc::clone(env),
            };
            env.borrow_mut().define(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse {
            message: "define: bad syntax".into(),
        }),
    }
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let (cond, then, els) = match args {
        [c, t, e] => (c, t, Some(e)),
        [c, t] => (c, t, None),
        _ => {
            return Err(EvalError::Parse {
                message: "if: expected 2 or 3 arguments".into(),
            })
        }
    };
    let cond_val = eval(cond, env)?;
    if cond_val != Value::Boolean(false) {
        eval(then, env)
    } else {
        match els {
            Some(e) => eval(e, env),
            None => Ok(Value::Void),
        }
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [datum] = args else {
        return Err(EvalError::Parse {
            message: "quote: expected exactly 1 argument".into(),
        });
    };
    Ok(datum.clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "lambda: expected parameters and body".into(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Parse {
            message: "lambda: expected parameter list".into(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::Parse {
                message: format!("lambda: expected parameter name, got {other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params,
        body: args[1..].to_vec(),
        closure: Rc::clone(env),
    })
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_args_as_integers(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| {
            let val = eval(a, env)?;
            match val {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{other}"),
                }),
            }
        })
        .collect()
}

fn eval_add(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn eval_sub(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-single)),
        [first, rest @ ..] => Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n))),
    }
}

fn eval_mul(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn eval_div(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(*first, |acc, n| {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(acc / n)
                }
            })
            .map(Value::Integer),
    }
}

fn eval_cmp(
    args: &[Value],
    env: &Rc<RefCell<Env>>,
    cmp: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args, env)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    }
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Boolean(result))
}
