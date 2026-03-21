use std::cell::RefCell;
use std::rc::Rc;

use super::{EvalError, Value};

fn require_integers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                expected: "integer".to_string(),
                got: format!("{other}"),
            }),
        })
        .collect()
}

pub fn apply_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

pub fn apply_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [first, rest @ ..] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };
    if rest.is_empty() {
        Ok(Value::Integer(-first))
    } else {
        Ok(Value::Integer(rest.iter().fold(*first, |acc, n| acc - n)))
    }
}

pub fn apply_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

pub fn apply_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [first, rest @ ..] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    };
    rest.iter().try_fold(*first, |acc, &n| {
        if n == 0 {
            Err(EvalError::DivisionByZero)
        } else {
            Ok(acc / n)
        }
    }).map(Value::Integer)
}

pub fn apply_compare(args: &[Value], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    Ok(Value::Boolean(nums.windows(2).all(|w| cmp(w[0], w[1]))))
}

pub fn apply_not(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Boolean(false))))
}

pub fn apply_cons(args: &[Value]) -> Result<Value, EvalError> {
    let [car, cdr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    match cdr {
        Value::Nil => Ok(Value::List(vec![car.clone()])),
        Value::List(items) => {
            let mut new_list = vec![car.clone()];
            new_list.extend(items.iter().cloned());
            Ok(Value::List(new_list))
        }
        _ => Ok(Value::Pair(
            Box::new(car.clone()),
            Box::new(cdr.clone()),
        )),
    }
}

pub fn apply_car(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(car, _) => Ok(*car.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::List(items) if !items.is_empty() => {
            if items.len() == 1 {
                Ok(Value::Nil)
            } else {
                Ok(Value::List(items[1..].to_vec()))
            }
        }
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
        _ => Err(EvalError::TypeError {
            expected: "pair".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_null(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Nil)))
}

pub fn apply_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(args.to_vec()))
    }
}

pub fn apply_type_predicate(args: &[Value], pred: fn(&Value) -> bool) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(pred(val)))
}

pub fn apply_string_length(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::String(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [s_val, idx_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let Value::String(s) = s_val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{s_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{idx_val}"),
        });
    };
    s.chars()
        .nth(*idx as usize)
        .map(Value::Char)
        .ok_or_else(|| EvalError::TypeError {
            expected: "valid string index".to_string(),
            got: format!("{idx}"),
        })
}

pub fn apply_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let result: String = args
        .iter()
        .map(|v| match v {
            Value::String(s) => Ok(s.as_str()),
            _ => Err(EvalError::TypeError {
                expected: "string".to_string(),
                got: format!("{v}"),
            }),
        })
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

pub fn apply_substring(args: &[Value]) -> Result<Value, EvalError> {
    let [s_val, start_val, end_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let Value::String(s) = s_val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{s_val}"),
        });
    };
    let Value::Integer(start) = start_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{start_val}"),
        });
    };
    let Value::Integer(end) = end_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{end_val}"),
        });
    };
    let chars: Vec<char> = s.chars().collect();
    let sub: String = chars[*start as usize..*end as usize].iter().collect();
    Ok(Value::String(sub))
}

pub fn apply_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{val}"),
        });
    };
    s.parse::<i64>()
        .map(Value::Integer)
        .map_err(|_| EvalError::TypeError {
            expected: "numeric string".to_string(),
            got: format!("\"{s}\""),
        })
}

pub fn apply_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Integer(n) = val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(n.to_string()))
}

pub fn apply_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Symbol(s) = val else {
        return Err(EvalError::TypeError {
            expected: "symbol".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s.clone()))
}

pub fn apply_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Symbol(s.clone()))
}

pub fn apply_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::String(s.clone()))
}

pub fn apply_is_char(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(matches!(val, Value::Char(_))))
}

pub fn apply_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::String(s) = val else {
        return Err(EvalError::TypeError {
            expected: "string".to_string(),
            got: format!("{val}"),
        });
    };
    let chars: Vec<Value> = s.chars().map(Value::Char).collect();
    if chars.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(chars))
    }
}

pub fn apply_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let items = match val {
        Value::Nil => return Ok(Value::String(String::new())),
        Value::List(items) => items,
        _ => {
            return Err(EvalError::TypeError {
                expected: "list".to_string(),
                got: format!("{val}"),
            })
        }
    };
    let s: String = items
        .iter()
        .map(|v| match v {
            Value::Char(c) => Ok(*c),
            _ => Err(EvalError::TypeError {
                expected: "char".to_string(),
                got: format!("{v}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::String(s))
}

pub fn apply_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Char(c) = val else {
        return Err(EvalError::TypeError {
            expected: "char".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(*c as i64))
}

pub fn apply_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Integer(n) = val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{val}"),
        });
    };
    let ch = char::from_u32(*n as u32).ok_or_else(|| EvalError::TypeError {
        expected: "valid unicode code point".to_string(),
        got: format!("{n}"),
    })?;
    Ok(Value::Char(ch))
}

pub fn apply_abs(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Integer(n) = val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(n.abs()))
}

pub fn apply_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [a, b] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    };
    if *b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(((a % b) + b) % b))
}

pub fn apply_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [a, b] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    };
    if *b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(a % b))
}

pub fn apply_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [a, b] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    };
    if *b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(a / b))
}

pub fn apply_min(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    }
    Ok(Value::Integer(*nums.iter().min().expect("non-empty checked above")))
}

pub fn apply_max(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    if nums.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: 0,
        });
    }
    Ok(Value::Integer(*nums.iter().max().expect("non-empty checked above")))
}

pub fn apply_expt(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_integers(args)?;
    let [base, exp] = nums.as_slice() else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: nums.len(),
        });
    };
    Ok(Value::Integer((*base).pow(*exp as u32)))
}

pub fn apply_equal(args: &[Value]) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    Ok(Value::Boolean(a == b))
}

pub fn apply_eq(args: &[Value]) -> Result<Value, EvalError> {
    let [a, b] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let result = match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        _ => std::ptr::eq(a as *const Value, b as *const Value),
    };
    Ok(Value::Boolean(result))
}

pub fn apply_length(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    match val {
        Value::Nil => Ok(Value::Integer(0)),
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::TypeError {
            expected: "list".to_string(),
            got: format!("{val}"),
        }),
    }
}

pub fn apply_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

pub fn apply_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    let [len_val, fill_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let Value::Integer(len) = len_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{len_val}"),
        });
    };
    Ok(Value::Vector(Rc::new(RefCell::new(
        vec![fill_val.clone(); *len as usize],
    ))))
}

pub fn apply_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val, idx_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 2,
            got: args.len(),
        });
    };
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".to_string(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{idx_val}"),
        });
    };
    let items = v.borrow();
    items
        .get(*idx as usize)
        .cloned()
        .ok_or_else(|| EvalError::TypeError {
            expected: "valid vector index".to_string(),
            got: format!("{idx}"),
        })
}

pub fn apply_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    let [vec_val, idx_val, new_val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 3,
            got: args.len(),
        });
    };
    let Value::Vector(v) = vec_val else {
        return Err(EvalError::TypeError {
            expected: "vector".to_string(),
            got: format!("{vec_val}"),
        });
    };
    let Value::Integer(idx) = idx_val else {
        return Err(EvalError::TypeError {
            expected: "integer".to_string(),
            got: format!("{idx_val}"),
        });
    };
    let mut items = v.borrow_mut();
    let i = *idx as usize;
    if i >= items.len() {
        return Err(EvalError::TypeError {
            expected: "valid vector index".to_string(),
            got: format!("{idx}"),
        });
    }
    items[i] = new_val.clone();
    Ok(Value::Nil)
}

pub fn apply_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Vector(v) = val else {
        return Err(EvalError::TypeError {
            expected: "vector".to_string(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(v.borrow().len() as i64))
}

pub fn apply_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [val] = args else {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    };
    let Value::Vector(v) = val else {
        return Err(EvalError::TypeError {
            expected: "vector".to_string(),
            got: format!("{val}"),
        });
    };
    let items = v.borrow();
    if items.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(items.clone()))
    }
}
