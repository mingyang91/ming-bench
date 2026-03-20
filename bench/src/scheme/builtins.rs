use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Integer division that rejects zero divisors.
fn checked_div(a: i64, b: i64) -> Result<i64, EvalError> {
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(a / b)
}

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
        ("/", [x]) => checked_div(1, *x).map(Value::Integer),
        ("/", [first, rest @ ..]) => {
            rest.iter().try_fold(*first, |a, b| checked_div(a, *b)).map(Value::Integer)
        }
        _ => Err(EvalError::UnboundVariable { name: op.into() }),
    }
}

/// Apply `abs` to a single integer argument.
pub fn apply_abs(args: &[Value]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let n = expect_integer(arg)?;
    Ok(Value::Integer(n.abs()))
}

/// Apply `quotient` (truncated integer division).
pub fn apply_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a = expect_integer(a)?;
    let b = expect_integer(b)?;
    checked_div(a, b).map(Value::Integer)
}

/// Apply `remainder` (sign follows dividend).
pub fn apply_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a = expect_integer(a)?;
    let b = expect_integer(b)?;
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(a % b))
}

/// Apply `modulo` (sign follows divisor).
pub fn apply_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a = expect_integer(a)?;
    let b = expect_integer(b)?;
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(((a % b) + b) % b))
}

/// Apply `min` to one or more integer arguments.
pub fn apply_min(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        });
    }
    let nums: Vec<i64> = args.iter().map(expect_integer).collect::<Result<_, _>>()?;
    Ok(Value::Integer(
        nums.into_iter().min().expect("non-empty args"),
    ))
}

/// Apply `max` to one or more integer arguments.
pub fn apply_max(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "at least 1".into(),
            got: 0,
        });
    }
    let nums: Vec<i64> = args.iter().map(expect_integer).collect::<Result<_, _>>()?;
    Ok(Value::Integer(
        nums.into_iter().max().expect("non-empty args"),
    ))
}

/// Apply `expt` (integer exponentiation).
pub fn apply_expt(args: &[Value]) -> Result<Value, EvalError> {
    let [base, exp] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let base = expect_integer(base)?;
    let exp = expect_integer(exp)?;
    if exp < 0 {
        return Err(EvalError::TypeError {
            expected: "non-negative exponent".into(),
            got: format!("{exp}"),
        });
    }
    Ok(Value::Integer(base.pow(exp as u32)))
}

/// Apply a numeric predicate (`zero?`, `positive?`, `negative?`, `odd?`, `even?`).
pub fn apply_numeric_pred(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let n = expect_integer(arg)?;
    let result = match name {
        "zero?" => n == 0,
        "positive?" => n > 0,
        "negative?" => n < 0,
        "odd?" => n % 2 != 0,
        "even?" => n % 2 == 0,
        _ => unreachable!("invalid numeric predicate: {name}"),
    };
    Ok(Value::Boolean(result))
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
