use super::{eval, eval_to_integer, is_truthy, Env};
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
    )
}

pub fn apply_builtin(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args, env),
        "<" | ">" | "=" | "<=" | ">=" => apply_comparison(name, args, env),
        "not" => apply_not(args, env),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn checked_div(acc: i64, x: i64) -> Result<i64, EvalError> {
    if x == 0 {
        Err(EvalError::DivisionByZero)
    } else {
        Ok(acc / x)
    }
}

fn apply_arithmetic(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let evaluated: Vec<i64> = args
        .iter()
        .map(|a| eval_to_integer(a, env))
        .collect::<Result<_, _>>()?;

    let result = match op {
        "+" => evaluated.iter().sum(),
        "*" => evaluated.iter().product(),
        "-" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            if rest.is_empty() {
                -first
            } else {
                rest.iter().fold(*first, |acc, &x| acc - x)
            }
        }
        "/" => {
            let [first, rest @ ..] = evaluated.as_slice() else {
                return Err(EvalError::WrongArgCount { expected: 1, got: 0 });
            };
            rest.iter().try_fold(*first, |acc, &x| checked_div(acc, x))?
        }
        _ => unreachable!("apply_arithmetic called with non-arithmetic op"),
    };

    Ok(Value::Integer(result))
}

fn apply_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(!is_truthy(&val)))
}

fn apply_comparison(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let a = eval_to_integer(left, env)?;
    let b = eval_to_integer(right, env)?;

    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        _ => unreachable!("apply_comparison called with non-comparison op"),
    };
    Ok(Value::Boolean(result))
}
