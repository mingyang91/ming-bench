use super::{EvalError, NumberError, SourcePos, StringRef, Value};
use std::cell::RefCell;
use std::rc::Rc;

pub(super) fn syntax_error(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::Syntax {
        pos,
        message: message.into(),
    }
}

pub(super) fn wrong_arg_count(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    got: usize,
) -> EvalError {
    EvalError::WrongArgCount {
        pos,
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

pub(super) fn type_mismatch(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    found: impl Into<String>,
) -> EvalError {
    EvalError::TypeMismatch {
        pos,
        name: name.into(),
        expected: expected.into(),
        found: found.into(),
    }
}

pub(super) fn invalid_argument(
    pos: SourcePos,
    name: impl Into<String>,
    message: impl Into<String>,
) -> EvalError {
    EvalError::InvalidArgument {
        pos,
        name: name.into(),
        message: message.into(),
    }
}

pub(super) fn number_error(pos: SourcePos, name: &str, error: NumberError) -> EvalError {
    match error {
        NumberError::DivisionByZero => EvalError::DivisionByZero { pos },
        NumberError::Overflow => invalid_argument(pos, name, "numeric overflow"),
        NumberError::NonFinite => invalid_argument(
            pos,
            name,
            "cannot convert a non-finite inexact number to exact",
        ),
    }
}

pub(super) fn make_string_value(value: impl Into<String>) -> Value {
    make_mutable_string_value(value)
}

pub(super) fn make_mutable_string_value(value: impl Into<String>) -> Value {
    Value::String(StringRef::new(value, true))
}

pub(super) fn make_immutable_string_value(value: impl Into<String>) -> Value {
    Value::String(StringRef::new(value, false))
}

pub(super) fn make_vector_value(values: Vec<Value>) -> Value {
    Value::Vector(Rc::new(RefCell::new(values)))
}
