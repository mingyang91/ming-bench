use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const BUILTINS: &[&str] = &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"];

pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            if BUILTINS.contains(&name.as_str()) {
                Ok(expr.clone())
            } else {
                Err(EvalError::UnboundVariable(name.clone()))
            }
        }
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms (and/or need short-circuit)
            if let Value::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "and" => return eval_and(&elems[1..]),
                    "or" => return eval_or(&elems[1..]),
                    _ => {}
                }
            }
            let func = eval(&elems[0])?;
            match &func {
                Value::Symbol(op) => apply_builtin(op, &elems[1..]),
                _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
            }
        }
        Value::Void => Ok(Value::Void),
    }
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg)?;
        if !val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1])
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for arg in &args[..args.len() - 1] {
        let val = eval(arg)?;
        if val.is_truthy() {
            return Ok(val);
        }
    }
    eval(&args[args.len() - 1])
}

fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let vals: Vec<Value> = args.iter().map(|a| eval(a)).collect::<Result<_, _>>()?;

    match op {
        "+" => {
            let mut sum: i64 = 0;
            for v in &vals {
                sum += expect_int(v)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if vals.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if vals.len() == 1 {
                return Ok(Value::Integer(-expect_int(&vals[0])?));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                result -= expect_int(v)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for v in &vals {
                product *= expect_int(v)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if vals.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                let d = expect_int(v)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_nums(&vals, |a, b| a < b),
        ">" => compare_nums(&vals, |a, b| a > b),
        "=" => compare_nums(&vals, |a, b| a == b),
        "<=" => compare_nums(&vals, |a, b| a <= b),
        ">=" => compare_nums(&vals, |a, b| a >= b),
        "not" => {
            if vals.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!vals[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn compare_nums(vals: &[Value], pred: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if vals.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = expect_int(&vals[0])?;
    for v in &vals[1..] {
        let curr = expect_int(v)?;
        if !pred(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}
