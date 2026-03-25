use super::{PairRef, Procedure, RenderMode, Value, VectorRef};
use std::collections::HashSet;
use std::rc::Rc;

pub(super) fn value_type_name(value: &Value) -> String {
    match value {
        Value::Record(record) => record.record_type.type_name.clone(),
        other => other.type_name().to_string(),
    }
}

pub(super) fn eq_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::String(left), Value::String(right)) => left.ptr_eq(right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        (
            Value::Procedure(Procedure::Builtin(left)),
            Value::Procedure(Procedure::Builtin(right)),
        ) => left == right,
        (Value::Procedure(Procedure::Lambda(left)), Value::Procedure(Procedure::Lambda(right))) => {
            Rc::ptr_eq(left, right)
        }
        (
            Value::Procedure(Procedure::CaseLambda(left)),
            Value::Procedure(Procedure::CaseLambda(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordConstructor(left)),
            Value::Procedure(Procedure::RecordConstructor(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordPredicate(left)),
            Value::Procedure(Procedure::RecordPredicate(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordAccessor {
                record_type: left_record_type,
                field_index: left_field_index,
                name: left_name,
            }),
            Value::Procedure(Procedure::RecordAccessor {
                record_type: right_record_type,
                field_index: right_field_index,
                name: right_name,
            }),
        ) => {
            Rc::ptr_eq(left_record_type, right_record_type)
                && left_field_index == right_field_index
                && left_name == right_name
        }
        _ => false,
    }
}

pub(super) fn eqv_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.ptr_eq(right),
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        (
            Value::Procedure(Procedure::Builtin(left)),
            Value::Procedure(Procedure::Builtin(right)),
        ) => left == right,
        (Value::Procedure(Procedure::Lambda(left)), Value::Procedure(Procedure::Lambda(right))) => {
            Rc::ptr_eq(left, right)
        }
        (
            Value::Procedure(Procedure::CaseLambda(left)),
            Value::Procedure(Procedure::CaseLambda(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordConstructor(left)),
            Value::Procedure(Procedure::RecordConstructor(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordPredicate(left)),
            Value::Procedure(Procedure::RecordPredicate(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordAccessor {
                record_type: left_record_type,
                field_index: left_field_index,
                name: left_name,
            }),
            Value::Procedure(Procedure::RecordAccessor {
                record_type: right_record_type,
                field_index: right_field_index,
                name: right_name,
            }),
        ) => {
            Rc::ptr_eq(left_record_type, right_record_type)
                && left_field_index == right_field_index
                && left_name == right_name
        }
        _ => false,
    }
}

pub(super) fn equal_value(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    equal_value_inner(left, right, &mut seen_pairs, &mut seen_vectors)
}

fn equal_value_inner(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::String(left), Value::String(right)) => *left.borrow() == *right.borrow(),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_value_inner(left, right, seen_pairs, seen_vectors))
        }
        (Value::Pair(left), Value::Pair(right)) => {
            let pair_key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if !seen_pairs.insert(pair_key) {
                return true;
            }

            let left_pair = left.borrow();
            let left_car = left_pair.car.clone();
            let left_cdr = left_pair.cdr.clone();
            drop(left_pair);

            let right_pair = right.borrow();
            let right_car = right_pair.car.clone();
            let right_cdr = right_pair.cdr.clone();
            drop(right_pair);

            equal_value_inner(&left_car, &right_car, seen_pairs, seen_vectors)
                && equal_value_inner(&left_cdr, &right_cdr, seen_pairs, seen_vectors)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let vector_key = (Rc::as_ptr(left) as usize, Rc::as_ptr(right) as usize);
            if !seen_vectors.insert(vector_key) {
                return true;
            }

            let left_items = left.borrow().clone();
            let right_items = right.borrow().clone();
            left_items.len() == right_items.len()
                && left_items.iter().zip(right_items.iter()).all(|(left, right)| {
                    equal_value_inner(left, right, seen_pairs, seen_vectors)
                })
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        (Value::Uninitialized, Value::Uninitialized) => true,
        (
            Value::Procedure(Procedure::Builtin(left)),
            Value::Procedure(Procedure::Builtin(right)),
        ) => left == right,
        (Value::Procedure(Procedure::Lambda(left)), Value::Procedure(Procedure::Lambda(right))) => {
            Rc::ptr_eq(left, right)
        }
        (
            Value::Procedure(Procedure::CaseLambda(left)),
            Value::Procedure(Procedure::CaseLambda(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordConstructor(left)),
            Value::Procedure(Procedure::RecordConstructor(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordPredicate(left)),
            Value::Procedure(Procedure::RecordPredicate(right)),
        ) => Rc::ptr_eq(left, right),
        (
            Value::Procedure(Procedure::RecordAccessor {
                record_type: left_record_type,
                field_index: left_field_index,
                name: left_name,
            }),
            Value::Procedure(Procedure::RecordAccessor {
                record_type: right_record_type,
                field_index: right_field_index,
                name: right_name,
            }),
        ) => {
            Rc::ptr_eq(left_record_type, right_record_type)
                && left_field_index == right_field_index
                && left_name == right_name
        }
        _ => false,
    }
}

pub(super) fn render_value(value: &Value, mode: RenderMode) -> String {
    let mut active_pairs = HashSet::new();
    let mut active_vectors = HashSet::new();
    render_value_inner(value, mode, &mut active_pairs, &mut active_vectors)
}

fn render_value_inner(
    value: &Value,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    match value {
        Value::Number(value) => value.render(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::Character(value) => render_char(*value, mode),
        Value::String(value) => {
            let value = value.borrow();
            match mode {
                RenderMode::Display => value.clone(),
                RenderMode::Write => render_string(&value),
            }
        }
        Value::Symbol(value) => value.clone(),
        Value::List(items) => render_list(items, mode, active_pairs, active_vectors),
        Value::Pair(pair) => render_pair(pair, mode, active_pairs, active_vectors),
        Value::Vector(values) => render_vector(values, mode, active_pairs, active_vectors),
        Value::Record(record) => format!("#<record {}>", record.record_type.type_name),
        Value::Procedure(_) => "#<procedure>".into(),
        Value::Void => "#<void>".into(),
        Value::Uninitialized => "#<undefined>".into(),
    }
}

pub(super) fn render_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');

    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }

    out.push('"');
    out
}

pub(super) fn render_char(ch: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => ch.to_string(),
        RenderMode::Write => match ch {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            _ => format!("#\\{ch}"),
        },
    }
}

fn render_list(
    items: &[Value],
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let mut out = String::from("(");

    for (index, value) in items.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&render_value_inner(value, mode, active_pairs, active_vectors));
    }

    out.push(')');
    out
}

fn render_pair(
    pair: &PairRef,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let pair_id = Rc::as_ptr(pair) as usize;
    if !active_pairs.insert(pair_id) {
        return "#<circular>".into();
    }

    let mut inserted_pairs = vec![pair_id];
    let pair_ref = pair.borrow();
    let car = pair_ref.car.clone();
    let mut tail = pair_ref.cdr.clone();
    drop(pair_ref);

    let mut out = String::from("(");
    out.push_str(&render_value_inner(&car, mode, active_pairs, active_vectors));

    loop {
        match tail {
            Value::Pair(pair) => {
                let next_pair_id = Rc::as_ptr(&pair) as usize;
                if !active_pairs.insert(next_pair_id) {
                    out.push_str(" . #<circular>)");
                    break;
                }

                inserted_pairs.push(next_pair_id);
                let pair_ref = pair.borrow();
                let car = pair_ref.car.clone();
                let cdr = pair_ref.cdr.clone();
                drop(pair_ref);

                out.push(' ');
                out.push_str(&render_value_inner(&car, mode, active_pairs, active_vectors));
                tail = cdr;
            }
            Value::List(items) => {
                for value in &items {
                    out.push(' ');
                    out.push_str(&render_value_inner(value, mode, active_pairs, active_vectors));
                }
                out.push(')');
                break;
            }
            other => {
                out.push_str(" . ");
                out.push_str(&render_value_inner(&other, mode, active_pairs, active_vectors));
                out.push(')');
                break;
            }
        }
    }

    for pair_id in inserted_pairs {
        active_pairs.remove(&pair_id);
    }

    out
}

fn render_vector(
    values: &VectorRef,
    mode: RenderMode,
    active_pairs: &mut HashSet<usize>,
    active_vectors: &mut HashSet<usize>,
) -> String {
    let vector_id = Rc::as_ptr(values) as usize;
    if !active_vectors.insert(vector_id) {
        return "#<circular>".into();
    }

    let values = values.borrow().clone();
    let mut out = String::from("#(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&render_value_inner(value, mode, active_pairs, active_vectors));
    }

    out.push(')');
    active_vectors.remove(&vector_id);
    out
}

pub(super) fn byte_index_for_char(input: &str, char_index: usize) -> Option<usize> {
    if char_index == input.chars().count() {
        Some(input.len())
    } else {
        input
            .char_indices()
            .nth(char_index)
            .map(|(byte_index, _)| byte_index)
    }
}
