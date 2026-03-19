mod parser;
mod types;

use parser::Parser;
use types::Value;

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
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(&expr)?;
    }
    match result {
        Value::Void => Err("no expression to evaluate".into()),
        _ => Ok(result.to_string()),
    }
}

fn eval(expr: &Value) -> Result<Value, String> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(format!("unbound variable: {name}")),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err("empty application".into());
            }
            let op = match &elems[0] {
                Value::Symbol(s) => s.as_str(),
                other => return Err(format!("not a procedure: {other}")),
            };
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(eval)
                .collect::<Result<_, _>>()?;
            apply_primitive(op, &args)
        }
        _ => Err(format!("cannot evaluate: {expr}")),
    }
}

fn apply_primitive(op: &str, args: &[Value]) -> Result<Value, String> {
    let nums: Vec<i64> = args
        .iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(format!("expected number, got: {other}")),
        })
        .collect::<Result<_, _>>()?;

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
