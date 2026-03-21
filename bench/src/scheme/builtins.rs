use super::{EvalError, Value};

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

pub fn apply_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

pub fn apply_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [first, rest @ ..] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };
    if rest.is_empty() {
        Ok(Value::Integer(-first))
    } else {
        Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n)))
    }
}

pub fn apply_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

pub fn apply_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [first, rest @ ..] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };
    rest.iter().try_fold(*first, |acc, &n| {
        if n == 0 {
            Err(EvalError::DivisionByZero)
        } else {
            Ok(acc / n)
        }
    }).map(Value::Integer)
}

pub fn apply_compare(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| cmp(w[0], w[1]))))
}

pub fn apply_not(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Boolean(false))))
}
