pub mod error;
pub mod parser;
pub mod value;

pub use error::EvalError;
use value::Value;

/// Apply an arithmetic operator to evaluated arguments.
fn apply_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let nums: Vec<i64> = args
        .iter()
        .map(|a| match a {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                expected: "number".into(),
                got: format!("{other}"),
            }),
        })
        .collect::<Result<_, _>>()?;

    match (op, nums.as_slice()) {
        ("+", ns) => Ok(Value::Integer(ns.iter().sum())),
        ("*", ns) => Ok(Value::Integer(ns.iter().product())),
        ("-", []) => Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        }),
        ("-", [x]) => Ok(Value::Integer(-x)),
        ("-", [first, rest @ ..]) => Ok(Value::Integer(rest.iter().fold(*first, |a, b| a - b))),
        ("/", []) => Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        }),
        ("/", [x]) => Ok(Value::Integer(1 / x)),
        ("/", [first, rest @ ..]) => Ok(Value::Integer(rest.iter().fold(*first, |a, b| a / b))),
        _ => Err(EvalError::UnboundVariable { name: op.into() }),
    }
}

/// Evaluate a single parsed Scheme value.
fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Value::List(elements) => {
            let [operator, args @ ..] = elements.as_slice() else {
                return Err(EvalError::EmptyList);
            };
            match operator {
                Value::Symbol(op) if matches!(op.as_str(), "+" | "-" | "*" | "/") => {
                    let evaluated: Vec<Value> =
                        args.iter().map(eval).collect::<Result<_, _>>()?;
                    apply_arithmetic(op, &evaluated)
                }
                _ => Err(EvalError::NotAProcedure {
                    value: format!("{operator}"),
                }),
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = parser::parse(input)?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let result = expressions
        .iter()
        .map(eval)
        .next_back()
        .expect("non-empty expressions guaranteed above")?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
