use super::{
    invalid_argument, type_mismatch, wrong_arg_count, EvalError, Number, SourcePos, StringRef,
    Value, VectorRef,
};
use std::rc::Rc;

pub(super) fn number_predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    test: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Number) -> Result<bool, EvalError>,
{
    match args {
        [value] => Ok(Value::Boolean(test(&expect_number(name, value, pos)?)?)),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

pub(super) fn predicate<F>(
    args: &[Value],
    name: &str,
    pos: SourcePos,
    test: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(test(value))),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

pub(super) fn compare<F>(
    name: &str,
    args: &[Value],
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Number, &Number) -> bool,
{
    let values = expect_numbers(name, args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            name,
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values.windows(2).all(|pair| predicate(&pair[0], &pair[1]));
    Ok(Value::Boolean(result))
}

pub(super) fn compare_chars<F>(
    name: &str,
    args: &[Value],
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    let values = expect_chars(name, args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            name,
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(result))
}

pub(super) fn compare_strings<F>(
    name: &str,
    args: &[Value],
    pos: SourcePos,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    let values = expect_string_values(name, args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            name,
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values
        .windows(2)
        .all(|pair| predicate(pair[0].as_str(), pair[1].as_str()));
    Ok(Value::Boolean(result))
}

pub(super) fn expect_number(
    name: &str,
    value: &Value,
    pos: SourcePos,
) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        other => Err(type_mismatch(pos, name, "number", other.type_name())),
    }
}

pub(super) fn expect_numbers(
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<Vec<Number>, EvalError> {
    args.iter()
        .map(|value| expect_number(name, value, pos))
        .collect()
}

pub(super) fn expect_two_numbers(
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<(Number, Number), EvalError> {
    match args {
        [left, right] => Ok((
            expect_number(name, left, pos)?,
            expect_number(name, right, pos)?,
        )),
        _ => Err(wrong_arg_count(
            pos,
            name,
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

pub(super) fn expect_char(name: &str, value: &Value, pos: SourcePos) -> Result<char, EvalError> {
    match value {
        Value::Character(ch) => Ok(*ch),
        other => Err(type_mismatch(pos, name, "char", other.type_name())),
    }
}

pub(super) fn expect_chars(
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<Vec<char>, EvalError> {
    args.iter()
        .map(|value| expect_char(name, value, pos))
        .collect()
}

pub(super) fn expect_string(
    name: &str,
    value: &Value,
    pos: SourcePos,
) -> Result<StringRef, EvalError> {
    match value {
        Value::String(value) => Ok(Rc::clone(value)),
        other => Err(type_mismatch(pos, name, "string", other.type_name())),
    }
}

pub(super) fn expect_string_values(
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<Vec<String>, EvalError> {
    args.iter()
        .map(|value| Ok(expect_string(name, value, pos)?.borrow().clone()))
        .collect()
}

pub(super) fn expect_symbol<'a>(
    name: &str,
    value: &'a Value,
    pos: SourcePos,
) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(value) => Ok(value),
        other => Err(type_mismatch(pos, name, "symbol", other.type_name())),
    }
}

pub(super) fn expect_non_negative_integer(
    name: &str,
    value: &Value,
    pos: SourcePos,
    label: &str,
) -> Result<usize, EvalError> {
    match value {
        Value::Number(number) => match number.exact_integer_value() {
            Some(number) if number >= 0 => Ok(number as usize),
            Some(number) => Err(invalid_argument(
                pos,
                name,
                format!("{label} must be non-negative, got {number}"),
            )),
            None => Err(type_mismatch(pos, name, "exact integer", "number")),
        },
        other => Err(type_mismatch(pos, name, "exact integer", other.type_name())),
    }
}

pub(super) fn expect_list<'a>(
    name: &str,
    value: &'a Value,
    pos: SourcePos,
) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(type_mismatch(pos, name, "list", other.type_name())),
    }
}

pub(super) fn expect_vector(
    name: &str,
    value: &Value,
    pos: SourcePos,
) -> Result<VectorRef, EvalError> {
    match value {
        Value::Vector(items) => Ok(Rc::clone(items)),
        other => Err(type_mismatch(pos, name, "vector", other.type_name())),
    }
}
