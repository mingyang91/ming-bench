use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Check if a name is a builtin procedure.
fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
    )
}

/// Evaluate a single expression in the given environment.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            // Try environment first, then check builtins
            match env.borrow().get(name) {
                Ok(val) => Ok(val),
                Err(_) if is_builtin(name) => Ok(Value::Symbol(name.clone())),
                Err(e) => Err(e),
            }
        }
        Value::List(elems) => eval_list(elems, env),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let [head, args @ ..] = elems else {
        return Ok(Value::List(vec![]));
    };

    // Check for special forms when head is a symbol
    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "if" => return eval_if(args, env),
            "define" => return eval_define(args, env),
            "quote" => return eval_quote(args),
            "lambda" => return eval_lambda(args, env),
            _ => {}
        }
    }

    // Evaluate head to get the procedure
    let proc = eval(head, env)?;

    // Evaluate arguments
    let evaluated_args: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;

    apply(&proc, &evaluated_args)
}

fn apply(proc: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match proc {
        Value::Symbol(name) => apply_builtin(name, args),
        Value::Lambda { params, body, env } => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len(),
                    got: args.len(),
                });
            }
            let local_env = Env::with_parent(env);
            for (param, arg) in params.iter().zip(args) {
                local_env.borrow_mut().define(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {other}"),
        }),
    }
}

fn eval_and(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let (condition, consequent, alternate) = match args {
        [cond, cons, alt] => (cond, cons, Some(alt)),
        [cond, cons] => (cond, cons, None),
        _ => {
            return Err(EvalError::TypeError {
                message: "if requires 2 or 3 arguments".into(),
            });
        }
    };

    if is_truthy(&eval(condition, env)?) {
        eval(consequent, env)
    } else if let Some(alt) = alternate {
        eval(alt, env)
    } else {
        Ok(Value::Void)
    }
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
                return Err(EvalError::TypeError {
                    message: "define: expected function name".into(),
                });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|v| match v {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeError {
                        message: format!("define: expected parameter name, got {other}"),
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
        }),
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

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    let [Value::List(param_list), body @ ..] = args else {
        return Err(EvalError::TypeError {
            message: "lambda: expected parameter list".into(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::TypeError {
            message: "lambda: expected body".into(),
        });
    }
    let params: Vec<String> = param_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                message: format!("lambda: expected parameter name, got {other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::Lambda {
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    })
}

fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b)),
        "-" => eval_sub(args),
        "/" => eval_div(args),
        "<" => compare_op(args, |a, b| a < b),
        ">" => compare_op(args, |a, b| a > b),
        "=" => compare_op(args, |a, b| a == b),
        "<=" => compare_op(args, |a, b| a <= b),
        ">=" => compare_op(args, |a, b| a >= b),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn require_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            message: format!("expected integer, got {other}"),
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, val| op(acc, require_integer(val)?))
        .map(Value::Integer)
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-require_integer(single)?)),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first)?, |acc, val| {
                Ok(acc - require_integer(val)?)
            })
            .map(Value::Integer),
    }
}

fn checked_div(a: i64, b: i64) -> Result<i64, EvalError> {
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(a / b)
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => checked_div(1, require_integer(single)?).map(Value::Integer),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(require_integer(first)?, |acc, val| {
                checked_div(acc, require_integer(val)?)
            })
            .map(Value::Integer),
    }
}

fn compare_op(args: &[Value], op: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let nums: Vec<i64> = args.iter().map(require_integer).collect::<Result<_, _>>()?;
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}
