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

pub fn apply_cons(args: &[Value]) -> Result<Value, EvalError> {
    let [car, cdr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    match cdr {
        Value::Nil => Ok(Value::List(vec![car.clone()])),
        Value::List(items) => {
            let mut new_list = vec![car.clone()];
            new_list.extend(items.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Ok(Value::Pair(
            Box::new(car.clone()),
            Box::new(cdr.clone()),
        )),
    }
}

pub fn apply_car(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(car, _) => Ok(*car.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::List(items) if !items.is_empty() => {
            if items.len() == 1 {
                Ok(Value::Nil)
            } else {
                Ok(Value::List(items[1..].to_vec()))
            }
        }
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_null(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Nil)))
}

pub fn apply_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(args.to_vec()))
    }
}

pub fn apply_length(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::Nil => Ok(Value::Integer(0)),
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{val}"),
        }),
    }
}
