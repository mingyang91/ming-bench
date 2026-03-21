use std::rc::Rc;

use crate::scheme::value::Value;

pub fn is_eq(left: &Value, right: &Value) -> bool {
    is_eqv(left, right)
}

pub fn is_eqv(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.eqv(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.is_same_object(right),
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Vector(left), Value::Vector(right)) => left.is_same_object(right),
        (Value::Record(left), Value::Record(right)) => left.is_same_object(right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::RecordProcedure(left), Value::RecordProcedure(right)) => left.is_same_object(right),
        (Value::CallWithCurrentContinuation, Value::CallWithCurrentContinuation) => true,
        (Value::CallWithValues, Value::CallWithValues) => true,
        (Value::DynamicWind, Value::DynamicWind) => true,
        (Value::ValuesProcedure, Value::ValuesProcedure) => true,
        (Value::Raise, Value::Raise) => true,
        (Value::WithExceptionHandler, Value::WithExceptionHandler) => true,
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        (Value::Uninitialized, Value::Uninitialized) => true,
        _ => false,
    }
}

pub fn is_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.as_string() == right.as_string(),
        (Value::Character(left), Value::Character(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left_car, left_cdr), Value::Pair(right_car, right_cdr)) => {
            is_equal(left_car, right_car) && is_equal(left_cdr, right_cdr)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            left.len() == right.len()
                && left
                    .items()
                    .into_iter()
                    .zip(right.items())
                    .all(|(left, right)| is_equal(&left, &right))
        }
        (Value::MultipleValues(left), Value::MultipleValues(right)) => {
            left.values().len() == right.values().len()
                && left
                    .values()
                    .iter()
                    .zip(right.values())
                    .all(|(left, right)| is_equal(left, right))
        }
        _ => is_eqv(left, right),
    }
}
