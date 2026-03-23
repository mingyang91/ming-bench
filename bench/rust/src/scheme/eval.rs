use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn eval(expr: &Value, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Closure { .. } => {
            Ok(expr.clone())
        }
        Value::Symbol(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                match name.as_str() {
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" => {
                        Ok(expr.clone())
                    }
                    _ => Err(EvalError::UnboundVariable {
                        name: name.clone(),
                    }),
                }
            }
        }
        Value::List(elems) => eval_list(elems, env),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value], env: &Env) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    }

    let head = &elems[0];

    // Check for special forms
    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(&elems[1..], env),
            "or" => return eval_or(&elems[1..], env),
            "not" => return eval_not(&elems[1..], env),
            "if" => return eval_if(&elems[1..], env),
            "define" => return eval_define(&elems[1..], env),
            "quote" => return eval_quote(&elems[1..]),
            "lambda" => return eval_lambda(&elems[1..], env),
            _ => {}
        }
    }

    // Evaluate all elements
    let op = eval(head, env)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<_, _>>()?;

    apply(&op, &args)
}

fn apply(op: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        Value::Symbol(name) => apply_builtin(name, args),
        Value::Closure {
            params,
            body,
            env: closure_env,
        } => {
            if params.len() != args.len() {
                return Err(EvalError::WrongArgCount {
                    expected: params.len().to_string(),
                    got: args.len(),
                });
            }
            let local_env = Env::with_parent(closure_env);
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.define(param.clone(), arg.clone());
            }
            eval(body, &local_env)
        }
        other => Err(EvalError::NotAProcedure {
            value: other.to_string(),
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(args),
        "*" => arith_mul(args),
        "/" => arith_div(args),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_if(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::WrongArgCount {
            expected: "2 or 3".to_string(),
            got: args.len(),
        });
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_define(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body)
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty define function name".to_string(),
                });
            }
            let Value::Symbol(name) = &elems[0] else {
                return Err(EvalError::TypeMismatch {
                    expected: "symbol".to_string(),
                    got: elems[0].to_string(),
                });
            };
            let params: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Value::Symbol(s) => Ok(s.clone()),
                    other => Err(EvalError::TypeMismatch {
                        expected: "symbol".to_string(),
                        got: other.to_string(),
                    }),
                })
                .collect::<Result<_, _>>()?;
            let body = if args.len() == 2 {
                args[1].clone()
            } else {
                Value::List(
                    std::iter::once(Value::Symbol("begin".to_string()))
                        .chain(args[1..].iter().cloned())
                        .collect(),
                )
            };
            // Capture env *by reference* (Rc clone) so the closure sees
            // itself after we define it — enabling recursion.
            let closure = Value::Closure {
                params,
                body: Box::new(body),
                env: env.clone(),
            };
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        other => Err(EvalError::TypeMismatch {
            expected: "symbol or list".to_string(),
            got: other.to_string(),
        }),
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: args.len(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::TypeMismatch {
            expected: "parameter list".to_string(),
            got: args[0].to_string(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|e| match e {
            Value::Symbol(s) => Ok(s.clone()),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol".to_string(),
                got: other.to_string(),
            }),
        })
        .collect::<Result<_, _>>()?;
    let body = if args.len() == 2 {
        args[1].clone()
    } else {
        Value::List(
            std::iter::once(Value::Symbol("begin".to_string()))
                .chain(args[1..].iter().cloned())
                .collect(),
        )
    };
    Ok(Value::Closure {
        params,
        body: Box::new(body),
        env: env.clone(),
    })
}

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
        });
    }
    if nums.len() == 1 {
        return Ok(Value::Integer(-nums[0]));
    }
    let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
    Ok(Value::Integer(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: nums.len(),
        });
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        if n == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= n;
    }
    Ok(Value::Integer(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] < w[1])))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] > w[1])))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] == w[1])))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn eval_and(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}
