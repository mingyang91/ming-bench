use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const BUILTINS: &[&str] = &["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"];

fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name)
}

pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Int(_) | Value::Bool(_) | Value::String(_) | Value::Builtin(_) => Ok(expr.clone()),
        Value::Symbol(name) => {
            if is_builtin(name) {
                Ok(Value::Builtin(name.clone()))
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        Value::List(elems) => eval_list(elems),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value]) -> Result<Value, EvalError> {
    if elems.is_empty() {
        return Err(EvalError::Parse { msg: "empty application".into() });
    }

    // Check for special forms
    if let Value::Symbol(op) = &elems[0] {
        match op.as_str() {
            "and" => return eval_and(&elems[1..]),
            "or" => return eval_or(&elems[1..]),
            _ => {}
        }
    }

    let func = eval(&elems[0])?;
    let args: Vec<Value> = elems[1..].iter().map(eval).collect::<Result<_, _>>()?;

    match func {
        Value::Builtin(ref name) => apply_builtin(name, &args),
        _ => Err(EvalError::NotAProcedure { value: func.to_string() }),
    }
}

fn eval_and(exprs: &[Value]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(true));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr)?;
        if is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1])
}

fn eval_or(exprs: &[Value]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Bool(false));
    }
    for expr in &exprs[..exprs.len() - 1] {
        let val = eval(expr)?;
        if !is_false(&val) {
            return Ok(val);
        }
    }
    eval(&exprs[exprs.len() - 1])
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Bool(false))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|a| match a {
            Value::Int(n) => Ok(*n),
            other => Err(EvalError::TypeMismatch {
                expected: "integer".into(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            if nums.len() == 1 {
                Ok(Value::Int(-nums[0]))
            } else {
                let result = nums[1..].iter().fold(nums[0], |acc, n| acc - n);
                Ok(Value::Int(result))
            }
        }
        "*" => {
            let nums = require_ints(args)?;
            Ok(Value::Int(nums.iter().product()))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            }
            let nums = require_ints(args)?;
            for &n in &nums[1..] {
                if n == 0 {
                    return Err(EvalError::DivisionByZero);
                }
            }
            let result = nums[1..].iter().fold(nums[0], |acc, n| acc / n);
            Ok(Value::Int(result))
        }
        "<" => compare_nums(args, |a, b| a < b),
        ">" => compare_nums(args, |a, b| a > b),
        "=" => compare_nums(args, |a, b| a == b),
        "<=" => compare_nums(args, |a, b| a <= b),
        ">=" => compare_nums(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
            }
            Ok(Value::Bool(is_false(&args[0])))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn compare_nums(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    }
    let nums = require_ints(args)?;
    let result = nums.windows(2).all(|w| cmp(w[0], w[1]));
    Ok(Value::Bool(result))
}
