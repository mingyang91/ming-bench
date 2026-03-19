mod parser;
mod types;

use std::collections::HashMap;

use parser::Parser;
use types::Value;

type Env = HashMap<String, Value>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use cs61a_bench::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, String> {
    let exprs = Parser::new(input).parse_all().map_err(|e| e.to_string())?;
    let mut env = Env::new();
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(&expr, &mut env)?;
    }
    match result {
        Value::Void => Err("no expression to evaluate".into()),
        _ => Ok(result.to_string()),
    }
}

fn eval(expr: &Value, env: &mut Env) -> Result<Value, String> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unbound variable: {name}")),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err("empty application".into());
            }
            let op = match &elems[0] {
                Value::Symbol(s) => s.as_str(),
                other => return Err(format!("not a procedure: {other}")),
            };
            match op {
                "define" => eval_define(&elems[1..], env),
                "if" => eval_if(&elems[1..], env),
                "quote" => eval_quote(&elems[1..]),
                "and" => eval_and(&elems[1..], env),
                "or" => eval_or(&elems[1..], env),
                _ => {
                    let args: Vec<Value> = elems[1..]
                        .iter()
                        .map(|e| eval(e, env))
                        .collect::<Result<_, _>>()?;
                    apply_primitive(op, &args)
                }
            }
        }
        _ => Err(format!("cannot evaluate: {expr}")),
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, String> {
    if args.len() != 2 {
        return Err(format!("define requires 2 arguments, got {}", args.len()));
    }
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        other => return Err(format!("define: expected symbol, got {other}")),
    };
    let val = eval(&args[1], env)?;
    env.insert(name, val);
    Ok(Value::Void)
}

fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 3 {
        return Err(format!("if requires 2-3 arguments, got {}", args.len()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err(format!("quote requires 1 argument, got {}", args.len()));
    }
    Ok(args[0].clone())
}

fn eval_and(exprs: &[Value], env: &mut Env) -> Result<Value, String> {
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value], env: &mut Env) -> Result<Value, String> {
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn require_nums(args: &[Value]) -> Result<Vec<i64>, String> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(format!("expected number, got: {other}")),
        })
        .collect()
}

fn apply_primitive(op: &str, args: &[Value]) -> Result<Value, String> {
    match op {
        "not" => {
            if args.len() != 1 {
                return Err(format!("not requires 1 argument, got {}", args.len()));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            let nums = require_nums(args)?;
            if nums.len() != 2 {
                return Err(format!("{op} requires 2 arguments, got {}", nums.len()));
            }
            let result = match op {
                "<" => nums[0] < nums[1],
                ">" => nums[0] > nums[1],
                "=" => nums[0] == nums[1],
                "<=" => nums[0] <= nums[1],
                ">=" => nums[0] >= nums[1],
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        _ => {
            let nums = require_nums(args)?;
            match op {
                "+" => Ok(Value::Integer(nums.iter().sum())),
                "-" => match nums.len() {
                    0 => Err("- requires at least 1 argument".into()),
                    1 => Ok(Value::Integer(-nums[0])),
                    _ => Ok(Value::Integer(
                        nums[0] - nums[1..].iter().sum::<i64>(),
                    )),
                },
                "*" => Ok(Value::Integer(nums.iter().product())),
                "/" => checked_div(&nums),
                _ => Err(format!("unknown procedure: {op}")),
            }
        }
    }
}

fn checked_div(nums: &[i64]) -> Result<Value, String> {
    match nums.len() {
        0 => Err("/ requires at least 1 argument".into()),
        1 => Ok(Value::Integer(1 / nums[0])),
        _ => nums[1..].iter().try_fold(nums[0], |acc, &n| {
            if n == 0 { Err("division by zero".into()) } else { Ok(acc / n) }
        }).map(Value::Integer),
    }
}

#[cfg(test)]
mod tests;
