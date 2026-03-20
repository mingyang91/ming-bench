use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Extract an integer from a Value, returning a TypeError if not an integer.
pub fn expect_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{other}"),
        }),
    }
}

/// Apply an arithmetic operator to evaluated arguments.
pub fn apply_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    let nums: Vec<i64> = args.iter().map(expect_integer).collect::<Result<_, _>>()?;

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

/// Apply a comparison operator to evaluated arguments.
pub fn apply_comparison(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    }
    let a = expect_integer(&args[0])?;
    let b = expect_integer(&args[1])?;
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!("invalid comparison op: {op}"),
    };
    Ok(Value::Boolean(result))
}
