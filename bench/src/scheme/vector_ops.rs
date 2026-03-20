use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// `(vector arg ...)` — create a vector from arguments.
pub fn vector_new(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

/// `(make-vector k)` or `(make-vector k fill)` — create a vector of size k.
pub fn make_vector(args: &[Value]) -> Result<Value, EvalError> {
    let (size, fill) = match args {
        [Value::Integer(k)] => (*k, Value::Integer(0)),
        [Value::Integer(k), fill] => (*k, fill.clone()),
        [other, ..] => {
            return Err(EvalError::TypeError {
                expected: "integer".into(),
                got: format!("{other}"),
            })
        }
        _ => {
            return Err(EvalError::WrongArgCount {
                expected: "1 or 2".into(),
                got: args.len(),
            })
        }
    };
    let elems = vec![fill; size as usize];
    Ok(Value::Vector(Rc::new(RefCell::new(elems))))
}

/// `(vector-ref vec k)` — get element at index k.
pub fn vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val, idx_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let Value::Vector(cells) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{idx_val}"),
        });
    };
    let elems = cells.borrow();
    let i = *idx as usize;
    elems.get(i).cloned().ok_or_else(|| EvalError::TypeError {
        expected: format!("index in range 0..{}", elems.len()),
        got: format!("{idx}"),
    })
}

/// `(vector-set! vec k val)` — set element at index k.
pub fn vector_set(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val, idx_val, new_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "3".into(),
            got: args.len(),
        });
    };
    let Value::Vector(cells) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".into(),
            got: format!("{idx_val}"),
        });
    };
    let mut elems = cells.borrow_mut();
    let i = *idx as usize;
    if i >= elems.len() {
        return Err(EvalError::TypeError {
            expected: format!("index in range 0..{}", elems.len()),
            got: format!("{idx}"),
        });
    }
    elems[i] = new_val.clone();
    Ok(Value::Void)
}

/// `(vector-length vec)` — return number of elements.
pub fn vector_length(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let Value::Vector(cells) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    Ok(Value::Integer(cells.borrow().len() as i64))
}

/// `(vector? val)` — predicate.
pub fn vector_pred(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Vector(_))))
}

/// `(vector->list vec)` — convert vector to list.
pub fn vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let Value::Vector(cells) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".into(),
            got: format!("{vec_val}"),
        });
    };
    Ok(Value::List(cells.borrow().clone()))
}

/// `(list->vector lst)` — convert list to vector.
pub fn list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    let [lst_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let Value::List(elems) = lst_val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{lst_val}"),
        });
    };
    Ok(Value::Vector(Rc::new(RefCell::new(elems.clone()))))
}
