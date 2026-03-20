use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(elems) => eval_list(elems),
    }
}

fn eval_list(elems: &[Value]) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse {
            msg: "empty application".into(),
        });
    }

    let op = &elems[0];
    let Value::Symbol(name) = op else {
        return Err(EvalError::NotAProcedure {
            value: op.to_string(),
        });
    };

    // Short-circuiting special forms — do not eagerly evaluate args
    match name.as_str() {
        "and" => return eval_and(&elems[1..]),
        "or" => return eval_or(&elems[1..]),
        _ => {}
    }

    let args: Vec<Value> = elems[1..]
        .iter()
        .map(eval)
        .collect::<Result<Vec<_>, _>>()?;

    match name.as_str() {
        "+" => arith_add(&args),
        "-" => arith_sub(name, &args),
        "*" => arith_mul(&args),
        "/" => arith_div(name, &args),
        "<" => cmp_lt(&args),
        ">" => cmp_gt(&args),
        "=" => cmp_eq(&args),
        "<=" => cmp_le(&args),
        "not" => eval_not(name, &args),
        _ => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
    }
}

fn expect_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "integer".into(),
            got: other.to_string(),
        }),
    }
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += expect_integer(arg)?;
    }
    Ok(Value::Integer(sum))
}

fn arith_sub(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: 0,
        });
    }
    let first = expect_integer(&args[0])?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for arg in &args[1..] {
        result -= expect_integer(arg)?;
    }
    Ok(Value::Integer(result))
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= expect_integer(arg)?;
    }
    Ok(Value::Integer(product))
}

fn arith_div(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: 0,
        });
    }
    let first = expect_integer(&args[0])?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for arg in &args[1..] {
        let divisor = expect_integer(arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn cmp_lt(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a < b))
}

fn cmp_gt(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a > b))
}

fn cmp_eq(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a == b))
}

fn cmp_le(args: &[Value]) -> Result<Value, EvalError> {
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    Ok(Value::Boolean(a <= b))
}

fn eval_not(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArityError {
            name: name.into(),
            expected: 1,
            actual: args.len(),
        });
    }
    Ok(Value::Boolean(is_falsy(&args[0])))
}

fn is_falsy(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn eval_and(exprs: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr)?;
        if is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr)?;
        if !is_falsy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}
