use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate an expression in the given environment.
pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        Expr::Symbol(name) => env.lookup(name),
        Expr::List(elements) => eval_list(elements, env),
    }
}

fn eval_list(elements: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".into(),
        });
    }

    // Check for special forms
    if let Expr::Symbol(name) = &elements[0] {
        match name.as_str() {
            "if" => return eval_if(&elements[1..], env),
            "define" => return eval_define(&elements[1..], env),
            "quote" => return eval_quote(&elements[1..]),
            "lambda" => return eval_lambda(&elements[1..], env),
            "and" => return eval_and(&elements[1..], env),
            "or" => return eval_or(&elements[1..], env),
            "not" => return eval_not(&elements[1..], env),
            _ => {}
        }
    }

    // Check for builtin functions by name before evaluating
    if let Expr::Symbol(name) = &elements[0] {
        if is_builtin(name) {
            let args: Vec<Value> = elements[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<Vec<_>, _>>()?;
            return eval_builtin(name, &args);
        }
    }

    // Evaluate operator and arguments
    let op = eval(&elements[0], env)?;
    let args: Vec<Value> = elements[1..]
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply(&op, &args)
}

fn is_builtin(name: &str) -> bool {
    matches!(name, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=")
}

fn apply(op: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        Value::Lambda {
            params,
            body,
            env: closure_env,
        } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity {
                    name: "#<procedure>".into(),
                    expected: params.len().to_string(),
                    got: args.len(),
                });
            }
            let call_env = Env::with_parent(closure_env);
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }
            let mut result = Value::Nil;
            for expr in body {
                result = eval(expr, &call_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::NotAProcedure {
            value: op.to_string(),
        }),
    }
}

fn eval_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args),
        "-" => eval_sub(args, name),
        "*" => eval_mul(args),
        "/" => eval_div(args, name),
        "<" => eval_cmp(args, name, |a, b| a < b),
        ">" => eval_cmp(args, name, |a, b| a > b),
        "=" => eval_cmp(args, name, |a, b| a == b),
        "<=" => eval_cmp(args, name, |a, b| a <= b),
        ">=" => eval_cmp(args, name, |a, b| a >= b),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Parse {
            message: "if requires 2 or 3 arguments".into(),
        });
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Nil)
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse {
            message: "define requires at least 2 arguments".into(),
        });
    }

    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Parse {
                    message: "define requires exactly 2 arguments".into(),
                });
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Nil)
        }
        // (define (f params...) body...)
        Expr::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse {
                    message: "define: empty signature".into(),
                });
            }
            let name = match &sig[0] {
                Expr::Symbol(n) => n.clone(),
                _ => {
                    return Err(EvalError::Parse {
                        message: "define: expected symbol as function name".into(),
                    })
                }
            };
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Parse {
                        message: "define: expected symbol as parameter".into(),
                    }),
                })
                .collect::<Result<Vec<_>, _>>()?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Parse {
            message: "define: expected symbol or list".into(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Parse {
            message: "quote requires exactly 1 argument".into(),
        });
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        Expr::Symbol(s) => Ok(Value::SchemeString(s.clone())), // symbols as strings for now
        Expr::List(elements) => {
            let mut result = Value::Nil;
            for elem in elements.iter().rev() {
                let val = expr_to_value(elem)?;
                result = Value::Pair(Box::new(val), Box::new(result));
            }
            Ok(result)
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Parse {
            message: "lambda requires parameters and body".into(),
        });
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => param_exprs
            .iter()
            .map(|e| match e {
                Expr::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse {
                    message: "lambda: expected symbol as parameter".into(),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Parse {
                message: "lambda: expected parameter list".into(),
            })
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
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
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            name: "not".into(),
            expected: "1".into(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn require_integer(v: &Value, _op: &str) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Type {
            expected: "integer".into(),
            got: format!("{other}"),
        }),
    }
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += require_integer(arg, "+")?;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Value], name: &str) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        });
    }
    let first = require_integer(&args[0], "-")?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for arg in &args[1..] {
        result -= require_integer(arg, "-")?;
    }
    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= require_integer(arg, "*")?;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Value], name: &str) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            name: name.into(),
            expected: "at least 1".into(),
            got: 0,
        });
    }
    let first = require_integer(&args[0], "/")?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for arg in &args[1..] {
        let divisor = require_integer(arg, "/")?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn eval_cmp(args: &[Value], name: &str, cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }
    let mut prev = require_integer(&args[0], name)?;
    for arg in &args[1..] {
        let curr = require_integer(arg, name)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
