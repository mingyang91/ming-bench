use super::{Procedure, RenderMode, Value};
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
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(left, right),
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
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(left, right),
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
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
                    .all(|(left, right)| equal_value(left, right))
        }
        (Value::Pair(left), Value::Pair(right)) => {
            equal_value(&left.car, &right.car) && equal_value(&left.cdr, &right.cdr)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let left = left.borrow();
            let right = right.borrow();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_value(left, right))
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

pub(super) fn render_list(items: &[Value], mode: RenderMode) -> String {
    let mut out = String::from("(");

    for (index, value) in items.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&value.render_with_mode(mode));
    }

    out.push(')');
    out
}

pub(super) fn render_pair(car: &Value, cdr: &Value, mode: RenderMode) -> String {
    let mut out = String::from("(");
    out.push_str(&car.render_with_mode(mode));

    let mut tail = cdr;
    loop {
        match tail {
            Value::Pair(pair) => {
                out.push(' ');
                out.push_str(&pair.car.render_with_mode(mode));
                tail = &pair.cdr;
            }
            Value::List(items) => {
                for value in items {
                    out.push(' ');
                    out.push_str(&value.render_with_mode(mode));
                }
                out.push(')');
                return out;
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.render_with_mode(mode));
                out.push(')');
                return out;
            }
        }
    }
}

pub(super) fn render_vector(items: &[Value], mode: RenderMode) -> String {
    let mut out = String::from("#(");

    for (index, value) in items.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&value.render_with_mode(mode));
    }

    out.push(')');
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
