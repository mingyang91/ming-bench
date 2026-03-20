use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::eval;
use crate::scheme::value::Value;

/// Evaluate `(cons head tail)` — prepend head to a list.
pub fn eval_cons(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [head_expr, tail_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let head = eval(head_expr, env)?;
    let tail = eval(tail_expr, env)?;
    match tail {
        Value::List(mut elems) => {
            elems.insert(0, head);
            Ok(Value::List(elems))
        }
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(car lst)` — first element of a list.
pub fn eval_car(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(elems) if !elems.is_empty() => Ok(elems.into_iter().next().expect("non-empty")),
        Value::List(_) => Err(EvalError::TypeError {
            expected: "non-empty list".into(),
            got: "()".into(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(cdr lst)` — all but first element of a list.
pub fn eval_cdr(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(mut elems) if !elems.is_empty() => {
            elems.remove(0);
            Ok(Value::List(elems))
        }
        Value::List(_) => Err(EvalError::TypeError {
            expected: "non-empty list".into(),
            got: "()".into(),
        }),
        other => Err(EvalError::TypeError {
            expected: "pair".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(null? expr)` — test for empty list.
pub fn eval_null_q(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    Ok(Value::Boolean(matches!(val, Value::List(ref elems) if elems.is_empty())))
}

/// Evaluate `(list expr ...)` — create a proper list from evaluated arguments.
pub fn eval_list(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let elems: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    Ok(Value::List(elems))
}

/// Evaluate a type predicate (`string?`, `number?`, `boolean?`, `pair?`, `symbol?`).
pub fn eval_type_pred(pred: &str, args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let result = match pred {
        "string?" => matches!(val, Value::String(_)),
        "number?" => matches!(val, Value::Integer(_)),
        "boolean?" => matches!(val, Value::Boolean(_)),
        "pair?" => matches!(val, Value::List(ref elems) if !elems.is_empty()),
        "symbol?" => matches!(val, Value::Symbol(_)),
        _ => unreachable!("invalid type predicate: {pred}"),
    };
    Ok(Value::Boolean(result))
}

/// Evaluate `(length lst)` — count elements in a list.
pub fn eval_length(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
}
