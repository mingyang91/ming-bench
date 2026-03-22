use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
];

fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}

/// Evaluate a Scheme expression in the given environment.
pub fn eval(expr: &Value, env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            match env.borrow().get(name) {
                Ok(val) => Ok(val),
                Err(_) if is_builtin(name) => Ok(expr.clone()),
                Err(e) => Err(e),
            }
        }
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".into(),
                });
            }
            eval_list(items, env)
        }
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(items: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    // Check for special forms first
    if let Value::Symbol(op) = &items[0] {
        match op.as_str() {
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            "if" => return eval_if(&items[1..], env),
            "define" => return eval_define(&items[1..], env),
            "quote" => return eval_quote(&items[1..]),
            "lambda" => return eval_lambda(&items[1..], env),
            _ => {}
        }
    }

    // Evaluate operator
    let operator = eval(&items[0], env)?;

    // Evaluate arguments
    let args: Vec<Value> = items[1..]
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply(&operator, &args)
}

fn apply(operator: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match operator {
        Value::Symbol(name) => apply_builtin(name, args),
        Value::Lambda {
            params,
            body,
            closure,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity {
                    message: format!(
                        "expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ),
                });
            }
            let local = Env::with_parent(closure);
            for (param, arg) in params.iter().zip(args.iter()) {
                local.borrow_mut().set(param.clone(), arg.clone());
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type {
            message: format!("not a procedure: {operator}"),
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += require_int(a, "+")?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    message: "- requires at least 1 argument".into(),
                });
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-require_int(&args[0], "-")?));
            }
            let mut result = require_int(&args[0], "-")?;
            for a in &args[1..] {
                result -= require_int(a, "-")?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= require_int(a, "*")?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    message: "/ requires at least 1 argument".into(),
                });
            }
            let mut result = require_int(&args[0], "/")?;
            for a in &args[1..] {
                let divisor = require_int(a, "/")?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(args, "<", |a, b| a < b),
        ">" => compare_nums(args, ">", |a, b| a > b),
        "=" => compare_nums(args, "=", |a, b| a == b),
        "<=" => compare_nums(args, "<=", |a, b| a <= b),
        ">=" => compare_nums(args, ">=", |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    message: "not requires exactly 1 argument".into(),
                });
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_and(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
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

fn eval_or(exprs: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_if(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity {
            message: "if requires 2 or 3 arguments".into(),
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

fn eval_define(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "define requires at least 2 arguments".into(),
        });
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    message: "define requires exactly 2 arguments".into(),
                });
            }
            let val = eval(&args[1], env)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse {
                    message: "define: empty signature".into(),
                });
            }
            let Value::Symbol(name) = &sig[0] else {
                return Err(EvalError::Type {
                    message: "define: expected function name".into(),
                });
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|p| match p {
                    Value::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type {
                        message: "define: expected parameter name".into(),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                closure: Rc::clone(env),
            };
            env.borrow_mut().set(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type {
            message: "define: expected symbol or list".into(),
        }),
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "quote requires exactly 1 argument".into(),
        });
    }
    Ok(args[0].clone())
}

fn eval_lambda(args: &[Value], env: &Rc<RefCell<Env>>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            message: "lambda requires at least 2 arguments".into(),
        });
    }
    let Value::List(param_list) = &args[0] else {
        return Err(EvalError::Type {
            message: "lambda: expected parameter list".into(),
        });
    };
    let params: Vec<String> = param_list
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Type {
                message: "lambda: expected parameter name".into(),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        closure: Rc::clone(env),
    })
}

fn require_int(val: &Value, op: &str) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type {
            message: format!("{op} requires integer, got {val}"),
        }),
    }
}

fn compare_nums(
    args: &[Value],
    op: &str,
    cmp: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            message: format!("{op} requires at least 2 arguments"),
        });
    }
    let first = require_int(&args[0], op)?;
    let second = require_int(&args[1], op)?;
    Ok(Value::Boolean(cmp(first, second)))
}
