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
        other => Ok(Value::Pair(Box::new(head), Box::new(other))),
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
        Value::Pair(car, _) => Ok(*car),
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
        Value::Pair(_, cdr) => Ok(*cdr),
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
        "pair?" => matches!(val, Value::List(ref elems) if !elems.is_empty()) || matches!(val, Value::Pair(_, _)),
        "symbol?" => matches!(val, Value::Symbol(_)),
        "char?" => matches!(val, Value::Char(_)),
        "vector?" => matches!(val, Value::Vector(_)),
        "list?" => matches!(val, Value::List(_)),
        _ => unreachable!("invalid type predicate: {pred}"),
    };
    Ok(Value::Boolean(result))
}

/// `cons` on already-evaluated values.
pub fn eval_cons_values(args: &[Value]) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    match tail {
        Value::List(elems) => {
            let mut new_elems = vec![head.clone()];
            new_elems.extend(elems.iter().cloned());
            Ok(Value::List(new_elems))
        }
        other => Ok(Value::Pair(Box::new(head.clone()), Box::new(other.clone()))),
    }
}

/// `car` on already-evaluated values.
pub fn eval_car_values(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    match val {
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Value::Pair(car, _) => Ok(*car.clone()),
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

/// `cdr` on already-evaluated values.
pub fn eval_cdr_values(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    match val {
        Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
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

/// `null?` on already-evaluated values.
pub fn eval_null_q_values(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::List(elems) if elems.is_empty())))
}

/// `length` on already-evaluated values.
pub fn eval_length_values(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    match val {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{other}"),
        }),
    }
}

/// `list?` on already-evaluated values — true for proper lists (including empty).
pub fn eval_list_pred_values(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let result = matches!(val, Value::List(_));
    Ok(Value::Boolean(result))
}

/// `list-ref` on already-evaluated values.
pub fn eval_list_ref_values(args: &[Value]) -> Result<Value, EvalError> {
    let [list_val, index_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let Value::List(elems) = list_val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{list_val}"),
        });
    };
    let Value::Integer(idx) = index_val else {
        return Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{index_val}"),
        });
    };
    let idx = *idx as usize;
    elems.get(idx).cloned().ok_or_else(|| EvalError::TypeError {
        expected: format!("index < {}", elems.len()),
        got: format!("{idx}"),
    })
}

/// `list-tail` on already-evaluated values.
pub fn eval_list_tail_values(args: &[Value]) -> Result<Value, EvalError> {
    let [list_val, index_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let Value::List(elems) = list_val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{list_val}"),
        });
    };
    let Value::Integer(idx) = index_val else {
        return Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{index_val}"),
        });
    };
    let idx = *idx as usize;
    if idx > elems.len() {
        return Err(EvalError::TypeError {
            expected: format!("index <= {}", elems.len()),
            got: format!("{idx}"),
        });
    }
    Ok(Value::List(elems[idx..].to_vec()))
}

/// `assoc` on already-evaluated values — find pair by key using `equal?`.
pub fn eval_assoc_values(args: &[Value]) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let Value::List(pairs) = alist else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{alist}"),
        });
    };
    for pair in pairs {
        let Value::List(elems) = pair else {
            return Err(EvalError::TypeError {
                expected: "pair".into(),
                got: format!("{pair}"),
            });
        };
        if !elems.is_empty() && elems[0] == *key {
            return Ok(pair.clone());
        }
    }
    Ok(Value::Boolean(false))
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
