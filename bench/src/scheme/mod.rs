pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use value::Value;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse(input)?;
    let last = exprs
        .into_iter()
        .last()
        .ok_or(EvalError::Parse {
            message: "empty input".to_string(),
        })?;
    Ok(eval(&last)?.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

fn eval(value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(value.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Value]) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    };

    match operator {
        Value::Symbol(name) => apply_builtin(name, args),
        _ => Err(EvalError::TypeError {
            expected: "procedure".to_string(),
            got: format!("{operator}"),
        }),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn eval_to_integer(value: &Value) -> Result<i64, EvalError> {
    match eval(value)? {
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

fn apply_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let evaluated: Vec<i64> = args.iter().map(eval_to_integer).collect::<Result<_, _>>()?;

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

#[cfg(test)]
mod tests;
