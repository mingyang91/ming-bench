use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use super::{EvalError, PairCell, PairRef, Value};

fn empty_list() -> Value {
    Value::List(Vec::new())
}

pub(super) fn is_empty_list(value: &Value) -> bool {
    matches!(value, Value::List(items) if items.is_empty())
}

fn pair_ptr(pair: &PairRef) -> usize {
    Rc::as_ptr(pair) as usize
}

pub(super) fn list_from_vec(items: Vec<Value>) -> Value {
    let mut list = empty_list();
    for item in items.into_iter().rev() {
        list = Value::Pair(Rc::new(RefCell::new(PairCell {
            car: item,
            cdr: list,
        })));
    }
    list
}

pub(super) fn pair_parts(value: &Value) -> Option<(Value, Value)> {
    match value {
        Value::Pair(pair) => {
            let pair = pair.borrow();
            Some((pair.car.clone(), pair.cdr.clone()))
        }
        Value::List(items) if !items.is_empty() => Some((
            items[0].clone(),
            if items.len() == 1 {
                empty_list()
            } else {
                Value::List(items[1..].to_vec())
            },
        )),
        Value::List(_)
        | Value::Number(_)
        | Value::Boolean(_)
        | Value::String(_)
        | Value::MutableString(_)
        | Value::Symbol(_)
        | Value::Char(_)
        | Value::Vector(_)
        | Value::Procedure(_)
        | Value::NativeProcedure(_)
        | Value::Builtin(_)
        | Value::Continuation(_)
        | Value::Record(_)
        | Value::Uninitialized
        | Value::Void => None,
    }
}

pub(super) fn collect_list_items(value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen_pairs = HashSet::new();

    loop {
        match current {
            Value::List(rest) => {
                items.extend(rest);
                return Ok(items);
            }
            Value::Pair(pair) => {
                if !seen_pairs.insert(pair_ptr(&pair)) {
                    return Err(EvalError::CircularList);
                }

                let pair = pair.borrow();
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "list",
                    found: other.type_name().into(),
                });
            }
        }
    }
}

pub(super) fn is_proper_list(value: &Value) -> bool {
    let mut current = value.clone();
    let mut seen_pairs = HashSet::new();

    loop {
        match current {
            Value::List(_) => return true,
            Value::Pair(pair) => {
                if !seen_pairs.insert(pair_ptr(&pair)) {
                    return false;
                }
                current = pair.borrow().cdr.clone();
            }
            Value::Number(_)
            | Value::Boolean(_)
            | Value::String(_)
            | Value::MutableString(_)
            | Value::Symbol(_)
            | Value::Char(_)
            | Value::Vector(_)
            | Value::Procedure(_)
            | Value::NativeProcedure(_)
            | Value::Builtin(_)
            | Value::Continuation(_)
            | Value::Record(_)
            | Value::Uninitialized
            | Value::Void => return false,
        }
    }
}

pub(super) fn values_eq(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Number(lhs), Value::Number(rhs)) => matches!(lhs.equals(*rhs), Ok(true)),
        (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::MutableString(rhs))
        | (Value::MutableString(rhs), Value::String(lhs)) => {
            lhs.chars().eq(rhs.borrow().iter().copied())
        }
        (Value::MutableString(lhs), Value::MutableString(rhs)) => *lhs.borrow() == *rhs.borrow(),
        (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
        (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
        (Value::Pair(lhs), Value::Pair(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Vector(lhs), Value::Vector(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Continuation(lhs), Value::Continuation(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::List(lhs), Value::List(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs.iter())
                    .all(|(lhs, rhs)| values_eq(lhs, rhs))
        }
        (Value::Record(lhs), Value::Record(rhs)) => Rc::ptr_eq(lhs, rhs),
        (Value::Uninitialized, Value::Uninitialized) => true,
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

pub(super) fn values_eqv(lhs: &Value, rhs: &Value) -> bool {
    values_eq(lhs, rhs)
}

pub(super) fn values_equal(lhs: &Value, rhs: &Value) -> bool {
    fn values_equal_inner(
        lhs: &Value,
        rhs: &Value,
        seen_pairs: &mut HashSet<(usize, usize)>,
        seen_vectors: &mut HashSet<(usize, usize)>,
    ) -> bool {
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => matches!(lhs.equals(*rhs), Ok(true)),
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::MutableString(rhs))
            | (Value::MutableString(rhs), Value::String(lhs)) => {
                lhs.chars().eq(rhs.borrow().iter().copied())
            }
            (Value::MutableString(lhs), Value::MutableString(rhs)) => {
                *lhs.borrow() == *rhs.borrow()
            }
            (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
            (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
            (Value::Continuation(lhs), Value::Continuation(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::List(lhs), Value::List(rhs)) => {
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs.iter())
                        .all(|(lhs, rhs)| values_equal_inner(lhs, rhs, seen_pairs, seen_vectors))
            }
            (Value::Vector(lhs), Value::Vector(rhs)) => {
                let key = (Rc::as_ptr(lhs) as usize, Rc::as_ptr(rhs) as usize);
                if !seen_vectors.insert(key) {
                    return true;
                }

                let lhs = lhs.borrow();
                let rhs = rhs.borrow();
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs.iter())
                        .all(|(lhs, rhs)| values_equal_inner(lhs, rhs, seen_pairs, seen_vectors))
            }
            (Value::Record(lhs), Value::Record(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Uninitialized, Value::Uninitialized) => true,
            (Value::Void, Value::Void) => true,
            _ => {
                let Some((lhs_car, lhs_cdr)) = pair_parts(lhs) else {
                    return false;
                };
                let Some((rhs_car, rhs_cdr)) = pair_parts(rhs) else {
                    return false;
                };

                if let (Value::Pair(lhs_pair), Value::Pair(rhs_pair)) = (lhs, rhs) {
                    let key = (pair_ptr(lhs_pair), pair_ptr(rhs_pair));
                    if !seen_pairs.insert(key) {
                        return true;
                    }
                }

                values_equal_inner(&lhs_car, &rhs_car, seen_pairs, seen_vectors)
                    && values_equal_inner(&lhs_cdr, &rhs_cdr, seen_pairs, seen_vectors)
            }
        }
    }

    values_equal_inner(lhs, rhs, &mut HashSet::new(), &mut HashSet::new())
}
