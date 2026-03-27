use super::super::{
    list_from_values, list_from_values_with_tail, values_eq, values_equal, EvalError, EvaluatedArg,
    PairValue, Value, VectorValue,
};
use super::{apply_procedure, exact_int, parse_index_arg, parse_index_bound, parse_length_arg};
use std::{cell::RefCell, collections::HashSet, rc::Rc};

fn car_value(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) => items.first().cloned(),
        Value::Pair(pair) => Some(pair.head.borrow().clone()),
        _ => None,
    }
}

fn cdr_value(value: &Value) -> Option<Value> {
    match value {
        Value::List(items) => {
            if items.is_empty() {
                None
            } else {
                Some(list_from_values(items[1..].iter().cloned()))
            }
        }
        Value::Pair(pair) => Some(pair.tail.borrow().clone()),
        _ => None,
    }
}

fn list_tail_value(value: &Value, steps: usize) -> Option<Value> {
    let mut current = value.clone();
    let mut remaining = steps;
    let mut seen_pairs = HashSet::new();

    while remaining > 0 {
        match current {
            Value::List(items) => {
                return (items.len() >= remaining)
                    .then(|| list_from_values(items[remaining..].iter().cloned()));
            }
            Value::Pair(pair) => {
                let id = Rc::as_ptr(&pair) as usize;
                if !seen_pairs.insert(id) {
                    return None;
                }

                current = pair.tail.borrow().clone();
                remaining -= 1;
            }
            _ => return None,
        }
    }

    Some(current)
}

fn expect_pair(arg: &EvaluatedArg) -> Result<Rc<PairValue>, EvalError> {
    match &arg.value {
        Value::Pair(pair) => Ok(pair.clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col)),
    }
}

pub(super) fn apply_cons(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cons",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    Ok(Value::Pair(Rc::new(PairValue {
        head: RefCell::new(head.value.clone()),
        tail: RefCell::new(tail.value.clone()),
    })))
}

pub(super) fn apply_car(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    car_value(&value.value).ok_or_else(|| {
        if matches!(&value.value, Value::List(items) if items.is_empty()) {
            EvalError::TypeMismatch {
                expected: "non-empty pair",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col)
        } else {
            EvalError::TypeMismatch {
                expected: "pair",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col)
        }
    })
}

pub(super) fn apply_cdr(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    cdr_value(&value.value).ok_or_else(|| {
        if matches!(&value.value, Value::List(items) if items.is_empty()) {
            EvalError::TypeMismatch {
                expected: "non-empty pair",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col)
        } else {
            EvalError::TypeMismatch {
                expected: "pair",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col)
        }
    })
}

pub(super) fn apply_cddr(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cddr",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let first = cdr_value(&value.value).ok_or_else(|| EvalError::TypeMismatch {
        expected: "pair",
        found: value.value.render(),
    })?;

    cdr_value(&first)
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair",
            found: first.render(),
        })
        .map_err(|error| error.with_position(value.pos.line, value.pos.col))
}

pub(super) fn apply_set_car(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [pair_arg, value_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "set-car!",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let pair = expect_pair(pair_arg)?;
    *pair.head.borrow_mut() = value_arg.value.clone();
    Ok(Value::Void)
}

pub(super) fn apply_set_cdr(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [pair_arg, value_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "set-cdr!",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let pair = expect_pair(pair_arg)?;
    *pair.tail.borrow_mut() = value_arg.value.clone();
    Ok(Value::Void)
}

pub(super) fn apply_null(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(
        matches!(&value.value, Value::List(items) if items.is_empty()),
    ))
}

pub(super) fn apply_list(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    Ok(list_from_values(args.iter().map(|arg| arg.value.clone())))
}

pub(super) fn apply_list_pred(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.value.as_list().is_ok()))
}

pub(super) fn apply_length(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(value.as_list()?.len() as i64))
}

pub(super) fn apply_list_ref(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    let index = parse_index_arg(index, items.len())?;
    Ok(items[index].clone())
}

pub(super) fn apply_list_tail(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-tail",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    let index = parse_index_bound(index, items.len(), true)?;
    list_tail_value(&value.value, index).ok_or_else(|| {
        EvalError::TypeMismatch {
            expected: "list",
            found: value.value.render(),
        }
        .with_position(value.pos.line, value.pos.col)
    })
}

pub(super) fn apply_member(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    find_member("member", args, values_equal)
}

pub(super) fn apply_memq(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    find_member("memq", args, values_eq)
}

pub(super) fn apply_memv(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    find_member("memv", args, values_eq)
}

pub(super) fn apply_assv(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    find_assoc_entry("assv", args, values_eq)
}

pub(super) fn apply_assq(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    find_assoc_entry("assq", args, values_eq)
}

pub(super) fn apply_assoc(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    find_assoc_entry("assoc", args, values_equal)
}

fn find_member(
    name: &'static str,
    args: &[EvaluatedArg],
    predicate: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let [key, list] = args else {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let items = list.as_list()?;
    for (index, item) in items.iter().enumerate() {
        if predicate(&key.value, item) {
            return list_tail_value(&list.value, index).ok_or_else(|| {
                EvalError::TypeMismatch {
                    expected: "list",
                    found: list.value.render(),
                }
                .with_position(list.pos.line, list.pos.col)
            });
        }
    }

    Ok(Value::Bool(false))
}

fn find_assoc_entry(
    name: &'static str,
    args: &[EvaluatedArg],
    predicate: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    let [key, list] = args else {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let entries = list.as_list()?;
    for entry in entries {
        let Some(candidate) = car_value(&entry) else {
            return Err(EvalError::TypeMismatch {
                expected: "association list entry",
                found: entry.render(),
            }
            .with_position(list.pos.line, list.pos.col));
        };

        if predicate(&key.value, &candidate) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Bool(false))
}

pub(super) fn apply_append(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let Some((tail, prefix)) = args.split_last() else {
        return Ok(list_from_values(std::iter::empty::<Value>()));
    };

    let mut items = Vec::new();
    for value in prefix {
        items.extend(value.as_list()?);
    }

    if prefix.is_empty() {
        Ok(tail.value.clone())
    } else {
        Ok(list_from_values_with_tail(items, tail.value.clone()))
    }
}

pub(super) fn apply_reverse(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "reverse",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let mut items = value.as_list()?;
    items.reverse();
    Ok(list_from_values(items))
}

pub(super) fn apply_map(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    let [procedure, lists @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "at least 2",
            got: 0,
        });
    };

    if lists.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "at least 2",
            got: 1,
        });
    }

    let list_values = lists
        .iter()
        .map(EvaluatedArg::as_list)
        .collect::<Result<Vec<_>, _>>()?;
    let limit = list_values
        .iter()
        .map(|items| items.len())
        .min()
        .unwrap_or(0);

    let mut results = Vec::with_capacity(limit);
    for index in 0..limit {
        let call_args = lists
            .iter()
            .zip(list_values.iter())
            .map(|(arg, items)| EvaluatedArg {
                value: items[index].clone(),
                pos: arg.pos,
            })
            .collect::<Vec<_>>();
        let value = apply_procedure(procedure.value.clone(), &call_args, output)
            .map_err(|error| error.with_position(procedure.pos.line, procedure.pos.col))?;
        results.push(value);
    }

    Ok(list_from_values(results))
}

pub(super) fn apply_for_each(
    args: &[EvaluatedArg],
    output: &mut String,
) -> Result<Value, EvalError> {
    let [procedure, lists @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "for-each",
            expected: "at least 2",
            got: 0,
        });
    };

    if lists.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "for-each",
            expected: "at least 2",
            got: 1,
        });
    }

    let list_values = lists
        .iter()
        .map(EvaluatedArg::as_list)
        .collect::<Result<Vec<_>, _>>()?;
    let limit = list_values
        .iter()
        .map(|items| items.len())
        .min()
        .unwrap_or(0);

    for index in 0..limit {
        let call_args = lists
            .iter()
            .zip(list_values.iter())
            .map(|(arg, items)| EvaluatedArg {
                value: items[index].clone(),
                pos: arg.pos,
            })
            .collect::<Vec<_>>();
        apply_procedure(procedure.value.clone(), &call_args, output)
            .map_err(|error| error.with_position(procedure.pos.line, procedure.pos.col))?;
    }

    Ok(Value::Void)
}

pub(super) fn apply_vector(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
    })))
}

pub(super) fn apply_make_vector(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let (size_arg, fill) = match args {
        [size] => (size, Value::Void),
        [size, fill] => (size, fill.value.clone()),
        _ => {
            return Err(EvalError::WrongArgCount {
                name: "make-vector",
                expected: "1 or 2",
                got: args.len(),
            });
        }
    };

    let len = parse_length_arg(size_arg)?;
    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(vec![fill; len]),
    })))
}

pub(super) fn apply_vector_ref(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [vector, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let vector = vector.as_vector()?;
    let elements = vector.elements.borrow();
    let index = parse_index_arg(index, elements.len())?;
    Ok(elements[index].clone())
}

pub(super) fn apply_vector_set(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [vector, index, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!",
            expected: "exactly 3",
            got: args.len(),
        });
    };

    let vector = vector.as_vector()?;
    let mut elements = vector.elements.borrow_mut();
    let index = parse_index_arg(index, elements.len())?;
    elements[index] = value.value.clone();
    Ok(Value::Void)
}

pub(super) fn apply_vector_length(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(vector.as_vector()?.elements.borrow().len() as i64))
}

pub(super) fn apply_vector_pred(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Vector(_))))
}

pub(super) fn apply_vector_to_list(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector->list",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(list_from_values(
        vector.as_vector()?.elements.borrow().iter().cloned(),
    ))
}

pub(super) fn apply_list_to_vector(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list->vector",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(list.as_list()?),
    })))
}
