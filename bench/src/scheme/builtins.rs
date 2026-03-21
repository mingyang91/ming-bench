use super::{eval, eval_to_integer, is_truthy, Env};
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

pub fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length"
            | "boolean?" | "number?" | "pair?" | "string?" | "symbol?"
    )
}

pub fn apply_builtin(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" => apply_arithmetic(name, args, env),
        "<" | ">" | "=" | "<=" | ">=" => apply_comparison(name, args, env),
        "not" => apply_not(args, env),
        "cons" => apply_cons(args, env),
        "car" => apply_car(args, env),
        "cdr" => apply_cdr(args, env),
        "null?" => apply_null(args, env),
        "list" => apply_list(args, env),
        "length" => apply_length(args, env),
        "boolean?" | "number?" | "pair?" | "string?" | "symbol?" => apply_type_pred(name, args, env),
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

fn apply_cons(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount { expected: 2, got: args.len() });
    };
    let head_val = eval(head, env)?;
    let tail_val = eval(tail, env)?;
    match tail_val {
        Value::List(mut items) => {
            items.insert(0, head_val);
            Ok(Value::List(items))
        }
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
        }),
    }
}

fn apply_car(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(items) if !items.is_empty() => Ok(items.into_iter().next().expect("non-empty list")),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "non-empty list".to_string(),
            got: "()".to_string(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{other}"),
        }),
    }
}

fn apply_cdr(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "non-empty list".to_string(),
            got: "()".to_string(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{other}"),
        }),
    }
}

fn apply_null(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(matches!(val, Value::List(ref items) if items.is_empty())))
}

fn apply_list(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let items: Vec<Value> = args
        .iter()
        .map(|a| eval(a, env))
        .collect::<Result<_, _>>()?;
    Ok(Value::List(items))
}

fn apply_length(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{other}"),
        }),
    }
}

fn apply_type_pred(name: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount { expected: 1, got: args.len() });
    };
    let val = eval(arg, env)?;
    let result = match name {
        "boolean?" => matches!(val, Value::Boolean(_)),
        "number?" => matches!(val, Value::Integer(_)),
        "pair?" => matches!(val, Value::List(ref items) if !items.is_empty()),
        "string?" => matches!(val, Value::String(_)),
        "symbol?" => matches!(val, Value::Symbol(_)),
        _ => unreachable!("apply_type_pred called with unknown predicate"),
    };
    Ok(Value::Boolean(result))
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
