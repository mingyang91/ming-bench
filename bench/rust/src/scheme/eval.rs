use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate an expression and return a Value.
pub fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Expr::List(elements) => eval_list(elements),
    }
}

fn eval_list(elements: &[Expr]) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".into(),
        });
    }

    // Check for special forms
    if let Expr::Symbol(name) = &elements[0] {
        match name.as_str() {
            "and" => return eval_and(&elements[1..]),
            "or" => return eval_or(&elements[1..]),
            "not" => return eval_not(&elements[1..]),
            _ => {}
        }
    }

    // Dispatch on operator symbol (builtins only for now)
    if let Expr::Symbol(name) = &elements[0] {
        let args: Vec<Value> = elements[1..]
            .iter()
            .map(eval)
            .collect::<Result<Vec<_>, _>>()?;

        match name.as_str() {
            "+" => eval_add(&args),
            "-" => eval_sub(&args, name),
            "*" => eval_mul(&args),
            "/" => eval_div(&args, name),
            "<" => eval_cmp(&args, name, |a, b| a < b),
            ">" => eval_cmp(&args, name, |a, b| a > b),
            "=" => eval_cmp(&args, name, |a, b| a == b),
            "<=" => eval_cmp(&args, name, |a, b| a <= b),
            ">=" => eval_cmp(&args, name, |a, b| a >= b),
            _ => Err(EvalError::UnboundVariable {
                name: name.clone(),
            }),
        }
    } else {
        let op = eval(&elements[0])?;
        Err(EvalError::NotAProcedure {
            value: op.to_string(),
        })
    }
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

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            name: "not".into(),
            expected: "1".into(),
            got: args.len(),
        });
    }
    let val = eval(&args[0])?;
    Ok(Value::Boolean(!val.is_truthy()))
}
