pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use std::collections::HashMap;
use value::Value;

type Env = HashMap<String, Value>;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let mut env = Env::new();
    let mut last = Value::Boolean(false);
    for expr in &exprs {
        last = eval(expr, &mut env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

fn eval(value: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(value.clone()),
        Value::Symbol(name) => env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    };

    match operator {
        Value::Symbol(name) => match name.as_str() {
            "define" => eval_define(args, env),
            "if" => eval_if(args, env),
            "quote" => eval_quote(args),
            "and" => eval_and(args, env),
            "or" => eval_or(args, env),
            _ => apply_builtin(name, args, env),
        },
        _ => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{operator}"),
        }),
    }
}

fn eval_define(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [Value::Symbol(name), expr] = args else {
        return Err(EvalError::Parse {
            message: "define requires a symbol and an expression".to_string(),
        });
    };
    let val = eval(expr, env)?;
    env.insert(name.clone(), val);
    Ok(Value::Symbol(name.clone()))
}

fn eval_if(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [condition, consequent, alternative] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let cond_val = eval(condition, env)?;
    if is_truthy(&cond_val) {
        eval(consequent, env)
    } else {
        eval(alternative, env)
    }
}

fn eval_quote(args: &[Value]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(expr.clone())
}

fn apply_builtin(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args, env),
        "<" | ">" | "=" | "<=" | ">=" => apply_comparison(name, args, env),
        "not" => apply_not(args, env),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_to_integer(value: &Value, env: &mut Env) -> Result<i64, EvalError> {
    match eval(value, env)? {
        Value::Integer(n) => Ok(n),
        other => Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{other}"),
        }),
    }
}

fn checked_div(acc: i64, x: i64) -> Result<i64, EvalError> {
    if x == 0 {
        Err(EvalError::DivisionByZero)
    } else {
        Ok(acc / x)
    }
}

fn apply_arithmetic(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let evaluated: Vec<i64> = args
        .iter()
        .map(|a| eval_to_integer(a, env))
        .collect::<Result<_, _>>()?;

    let result = match op {
        "+" => evaluated.iter().sum(),
        "*" => evaluated.iter().product(),
        "-" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            if rest.is_empty() {
                -first
            } else {
                rest.iter().fold(*first, |acc, &x| acc - x)
            }
        }
        "/" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            rest.iter().try_fold(*first, |acc, &x| checked_div(acc, x))?
        }
        _ => unreachable!("apply_arithmetic called with non-arithmetic op"),
    };

    Ok(Value::Integer(result))
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn eval_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn apply_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(!is_truthy(&val)))
}

fn apply_comparison(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let a = eval_to_integer(left, env)?;
    let b = eval_to_integer(right, env)?;

    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!("apply_comparison called with non-comparison op"),
    };
    Ok(Value::Boolean(result))
}

#[cfg(test)]
mod tests;
