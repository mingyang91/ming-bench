use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Evaluate a single parsed expression.
pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Value]) -> Result<Value, EvalError> {
    let [head, args @ ..] = items else {
        return Ok(Value::List(vec![]));
    };

    let Value::Symbol(name) = head else {
        return Err(EvalError::TypeError {
            expected: "procedure".into(),
            got: format!("{head}"),
        });
    };

    match name.as_str() {
        "+" => eval_add(args),
        "-" => eval_sub(args),
        "*" => eval_mul(args),
        "/" => eval_div(args),
        "<" => eval_cmp(args, |a, b| a < b),
        ">" => eval_cmp(args, |a, b| a > b),
        "=" => eval_cmp(args, |a, b| a == b),
        "<=" => eval_cmp(args, |a, b| a <= b),
        ">=" => eval_cmp(args, |a, b| a >= b),
        "not" => eval_not(args),
        "and" => eval_and(args),
        "or" => eval_or(args),
        _ => Err(EvalError::UnknownProcedure {
            name: name.clone(),
        }),
    }
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg)?;
    Ok(Value::Boolean(val == Value::Boolean(false)))
}

fn eval_args_as_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| {
            let val = eval(a)?;
            match val {
                Value::Integer(n) => Ok(n),
                other => Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{other}"),
                }),
            }
        })
        .collect()
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-single)),
        [first, rest @ ..] => Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n))),
    }
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args)?;
    match nums.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [first, rest @ ..] => rest
            .iter()
            .try_fold(*first, |acc, n| {
                if *n == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(acc / n)
                }
            })
            .map(Value::Integer),
    }
}

fn eval_cmp(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = eval_args_as_integers(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    }
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Boolean(result))
}
