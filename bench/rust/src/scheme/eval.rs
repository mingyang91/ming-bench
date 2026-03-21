use crate::scheme::error::EvalError;
use crate::scheme::parser::Expr;
use crate::scheme::value::Value;

/// Evaluate a parsed expression.
pub fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnknownOperator { name: name.clone() }),
        Expr::List(elems) => eval_list(elems),
    }
}

fn eval_list(elems: &[Expr]) -> Result<Value, EvalError> {
    let [operator, args @ ..] = elems else {
        return Err(EvalError::Parse("empty list".into()));
    };

    let Expr::Symbol(op) = operator else {
        return Err(EvalError::TypeError {
            expected: "operator".into(),
            got: format!("{operator:?}"),
        });
    };

    match op.as_str() {
        "+" => eval_arithmetic(args, 0, |a, b| Ok(a + b)),
        "-" => eval_minus(args),
        "*" => eval_arithmetic(args, 1, |a, b| Ok(a * b)),
        "/" => eval_divide(args),
        "<" => eval_comparison(args, |a, b| a < b),
        ">" => eval_comparison(args, |a, b| a > b),
        "=" => eval_comparison(args, |a, b| a == b),
        "<=" => eval_comparison(args, |a, b| a <= b),
        ">=" => eval_comparison(args, |a, b| a >= b),
        "not" => eval_not(args),
        "and" => eval_and(args),
        "or" => eval_or(args),
        _ => Err(EvalError::UnknownOperator { name: op.clone() }),
    }
}

fn eval_arithmetic(
    args: &[Expr],
    identity: i64,
    op: fn(i64, i64) -> Result<i64, EvalError>,
) -> Result<Value, EvalError> {
    args.iter()
        .try_fold(identity, |acc, arg| {
            let val = eval(arg)?;
            let Value::Integer(n) = val else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: format!("{val}"),
                });
            };
            op(acc, n)
        })
        .map(Value::Integer)
}

fn eval_minus(args: &[Expr]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    if rest.is_empty() {
        return Ok(Value::Integer(-first_val));
    }

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            Ok(acc - n)
        })
        .map(Value::Integer)
}

fn eval_divide(args: &[Expr]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };

    let Value::Integer(first_val) = eval(first)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    rest.iter()
        .try_fold(first_val, |acc, arg| {
            let Value::Integer(n) = eval(arg)? else {
                return Err(EvalError::TypeError {
                    expected: "integer".into(),
                    got: "non-integer".into(),
                });
            };
            if n == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(acc / n)
        })
        .map(Value::Integer)
}

fn eval_comparison(args: &[Expr], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };

    let Value::Integer(a) = eval(left)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };
    let Value::Integer(b) = eval(right)? else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: "non-integer".into(),
        });
    };

    Ok(Value::Boolean(cmp(a, b)))
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}
