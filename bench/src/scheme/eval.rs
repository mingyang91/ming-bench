use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Evaluate a single expression.
pub fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(elems) => eval_list(elems),
        Value::Void => Ok(Value::Void),
    }
}

fn eval_list(elems: &[Value]) -> Result<Value, EvalError> {
    let [head, args @ ..] = elems else {
        return Ok(Value::List(vec![]));
    };

    let Value::Symbol(name) = head else {
        return Err(EvalError::TypeError {
            message: format!("not a procedure: {head}"),
        });
    };

    // Special forms (short-circuit evaluation)
    match name.as_str() {
        "and" => return eval_and(args),
        "or" => return eval_or(args),
        _ => {}
    }

    // Evaluate args and apply builtin
    let evaluated_args: Vec<Value> = args.iter().map(eval).collect::<Result<_, _>>()?;
    apply_builtin(name, &evaluated_args)
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_variadic(args, 0, |a, b| Ok(a + b)),
        "*" => arith_variadic(args, 1, |a, b| Ok(a * b)),
        "-" => eval_sub(args),
        "/" => eval_div(args),
        "<" => compare_op(args, |a, b| a < b),
        ">" => compare_op(args, |a, b| a > b),
        "=" => compare_op(args, |a, b| a == b),
        "<=" => compare_op(args, |a, b| a <= b),
        ">=" => compare_op(args, |a, b| a >= b),
        "not" => {
            let [arg] = args else {
                return Err(EvalError::WrongArgCount {
                    expected: 1,
                    got: args.len(),
                });
            };
            Ok(Value::Boolean(!is_truthy(arg)))
        }
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn require_integer(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::TypeError {
            message: format!("expected integer, got {other}"),
        }),
    }
}

fn arith_variadic(
    args: &[Value],
    identity: i64,
    op: impl Fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    let result = args
        .iter()
        .try_fold(identity, |acc, val| op(acc, require_integer(val)?));
    result.map(Value::Integer)
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => Ok(Value::Integer(-require_integer(single)?)),
        [first, rest @ ..] => {
            let init = require_integer(first)?;
            let result = rest
                .iter()
                .try_fold(init, |acc, val| Ok(acc - require_integer(val)?));
            result.map(Value::Integer)
        }
    }
}

fn checked_div(a: i64, b: i64) -> Result<i64, EvalError> {
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(a / b)
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        }),
        [single] => checked_div(1, require_integer(single)?).map(Value::Integer),
        [first, rest @ ..] => {
            let init = require_integer(first)?;
            rest.iter()
                .try_fold(init, |acc, val| checked_div(acc, require_integer(val)?))
                .map(Value::Integer)
        }
    }
}

fn compare_op(args: &[Value], op: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    }

    let nums: Vec<i64> = args.iter().map(require_integer).collect::<Result<_, _>>()?;
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}
