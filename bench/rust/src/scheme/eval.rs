use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => match name.as_str() {
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | "not" => Ok(expr.clone()),
            _ => Err(EvalError::UnboundVariable {
                name: name.clone(),
            }),
        },
        Value::List(elems) => eval_list(elems),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value]) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            message: "empty application".to_string(),
        });
    }

    let head = &elems[0];

    // Check for special forms
    if let Value::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(&elems[1..]),
            "or" => return eval_or(&elems[1..]),
            "not" => return eval_not(&elems[1..]),
            _ => {}
        }
    }

    // Evaluate all elements
    let op = eval(head)?;
    let args: Vec<Value> = elems[1..]
        .iter()
        .map(eval)
        .collect::<Result<_, _>>()?;

    apply_builtin(&op, &args)
}

fn apply_builtin(op: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let Value::Symbol(name) = op else {
        return Err(EvalError::NotAProcedure {
            value: op.to_string(),
        });
    };

    match name.as_str() {
        "+" => arith_add(args),
        "-" => arith_sub(args),
        "*" => arith_mul(args),
        "/" => arith_div(args),
        "<" => cmp_lt(args),
        ">" => cmp_gt(args),
        "=" => cmp_eq(args),
        "<=" => cmp_le(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
    }
}

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".to_string(),
            got: 0,
        });
    }
    if nums.len() == 1 {
        return Ok(Value::Integer(-nums[0]));
    }
    let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
    Ok(Value::Integer(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: "at least 2".to_string(),
            got: nums.len(),
        });
    }
    let mut result = nums[0];
    for &n in &nums[1..] {
        if n == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= n;
    }
    Ok(Value::Integer(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] < w[1])))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] > w[1])))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] == w[1])))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| w[0] <= w[1])))
}

fn eval_and(exprs: &[Value]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    let val = eval(&args[0])?;
    Ok(Value::Boolean(!val.is_truthy()))
}
