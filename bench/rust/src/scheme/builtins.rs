use std::cell::RefCell;
use std::rc::Rc;

use super::{env_define, Builtin, BuiltinKind, EnvRef, Environment, EvalError, OutputRef, Value};

pub(super) fn default_env(output: OutputRef) -> EnvRef {
    let env = Environment::new(None);

    for builtin in [
        BuiltinKind::Add,
        BuiltinKind::Sub,
        BuiltinKind::Mul,
        BuiltinKind::Div,
        BuiltinKind::LessThan,
        BuiltinKind::GreaterThan,
        BuiltinKind::Equal,
        BuiltinKind::LessThanOrEqual,
        BuiltinKind::Not,
        BuiltinKind::Cons,
        BuiltinKind::Car,
        BuiltinKind::Cdr,
        BuiltinKind::NullPred,
        BuiltinKind::List,
        BuiltinKind::Length,
        BuiltinKind::Append,
        BuiltinKind::Apply,
        BuiltinKind::StringPred,
        BuiltinKind::NumberPred,
        BuiltinKind::BooleanPred,
        BuiltinKind::PairPred,
        BuiltinKind::SymbolPred,
        BuiltinKind::Display,
        BuiltinKind::Write,
        BuiltinKind::Newline,
        BuiltinKind::StringAppend,
        BuiltinKind::StringLength,
        BuiltinKind::Substring,
        BuiltinKind::StringToNumber,
        BuiltinKind::NumberToString,
        BuiltinKind::SymbolToString,
        BuiltinKind::StringToSymbol,
        BuiltinKind::StringRef,
        BuiltinKind::StringCopy,
        BuiltinKind::StringSet,
        BuiltinKind::CharPred,
    ] {
        env_define(
            &env,
            builtin.name().into(),
            Value::Builtin(Builtin::new(builtin, Rc::clone(&output))),
        );
    }

    env
}

pub(super) fn apply_builtin(
    kind: BuiltinKind,
    args: &[Value],
    output: &OutputRef,
) -> Result<Value, EvalError> {
    match kind {
        BuiltinKind::Add => eval_add(args),
        BuiltinKind::Sub => eval_sub(args),
        BuiltinKind::Mul => eval_mul(args),
        BuiltinKind::Div => eval_div(args),
        BuiltinKind::LessThan => eval_compare(kind.name(), args, |lhs, rhs| lhs < rhs),
        BuiltinKind::GreaterThan => eval_compare(kind.name(), args, |lhs, rhs| lhs > rhs),
        BuiltinKind::Equal => eval_compare(kind.name(), args, |lhs, rhs| lhs == rhs),
        BuiltinKind::LessThanOrEqual => eval_compare(kind.name(), args, |lhs, rhs| lhs <= rhs),
        BuiltinKind::Not => eval_not(args),
        BuiltinKind::Cons => eval_cons(args),
        BuiltinKind::Car => eval_car(args),
        BuiltinKind::Cdr => eval_cdr(args),
        BuiltinKind::NullPred => eval_null(args),
        BuiltinKind::List => Ok(Value::List(args.to_vec())),
        BuiltinKind::Length => eval_length(args),
        BuiltinKind::Append => eval_append(args),
        BuiltinKind::Apply => eval_apply(args),
        BuiltinKind::StringPred => eval_predicate("string?", args, |value| {
            matches!(value, Value::String(_) | Value::MutableString(_))
        }),
        BuiltinKind::NumberPred => {
            eval_predicate("number?", args, |value| matches!(value, Value::Integer(_)))
        }
        BuiltinKind::BooleanPred => {
            eval_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        BuiltinKind::PairPred => eval_pair_pred(args),
        BuiltinKind::SymbolPred => {
            eval_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
        }
        BuiltinKind::Display => eval_display(args, output),
        BuiltinKind::Write => eval_write(args, output),
        BuiltinKind::Newline => eval_newline(args, output),
        BuiltinKind::StringAppend => eval_string_append(args),
        BuiltinKind::StringLength => eval_string_length(args),
        BuiltinKind::Substring => eval_substring(args),
        BuiltinKind::StringToNumber => eval_string_to_number(args),
        BuiltinKind::NumberToString => eval_number_to_string(args),
        BuiltinKind::SymbolToString => eval_symbol_to_string(args),
        BuiltinKind::StringToSymbol => eval_string_to_symbol(args),
        BuiltinKind::StringRef => eval_string_ref(args),
        BuiltinKind::StringCopy => eval_string_copy(args),
        BuiltinKind::StringSet => eval_string_set(args),
        BuiltinKind::CharPred => {
            eval_predicate("char?", args, |value| matches!(value, Value::Char(_)))
        }
    }
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;
    for arg in args {
        total = total
            .checked_add(arg.as_integer()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }
    Ok(Value::Integer(total))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = numeric_args(args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        })?;

    let result = if rest.is_empty() {
        first.checked_neg().ok_or(EvalError::IntegerOverflow)?
    } else {
        let mut total = *first;
        for value in rest {
            total = total
                .checked_sub(*value)
                .ok_or(EvalError::IntegerOverflow)?;
        }
        total
    };

    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;
    for arg in args {
        total = total
            .checked_mul(arg.as_integer()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }
    Ok(Value::Integer(total))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = numeric_args(args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 1,
        });
    }

    let mut total = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total = total
            .checked_div(*value)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn eval_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = numeric_args(args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_string(),
            expected: "at least 2 arguments".into(),
            got: values.len(),
        });
    }

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = rest else {
        return Err(EvalError::TypeMismatch {
            expected: "list",
            found: rest.type_name().into(),
        });
    };

    let mut result = Vec::with_capacity(items.len() + 1);
    result.push(first.clone());
    result.extend(items.iter().cloned());
    Ok(Value::List(result))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    };

    items
        .first()
        .cloned()
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        })
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    };

    if items.is_empty() {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    }

    Ok(Value::List(items[1..].to_vec()))
}

fn eval_null(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "list",
            found: value.type_name().into(),
        });
    };

    Ok(Value::Integer(items.len() as i64))
}

fn eval_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();

    for value in args {
        let Value::List(items) = value else {
            return Err(EvalError::TypeMismatch {
                expected: "list",
                found: value.type_name().into(),
            });
        };
        result.extend(items.iter().cloned());
    }

    Ok(Value::List(result))
}

fn eval_apply(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2 arguments".into(),
            got: args.len(),
        });
    }

    let callable = args[0].clone();
    let last = &args[args.len() - 1];
    let Value::List(tail_args) = last else {
        return Err(EvalError::TypeMismatch {
            expected: "list",
            found: last.type_name().into(),
        });
    };

    let mut applied_args = Vec::with_capacity(args.len() - 2 + tail_args.len());
    applied_args.extend(args[1..args.len() - 1].iter().cloned());
    applied_args.extend(tail_args.iter().cloned());

    super::apply_callable(callable, &applied_args)
}

fn eval_display(args: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "display".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let rendered = value.render_display();
    output.borrow_mut().push_str(&rendered);
    Ok(Value::Void)
}

fn eval_write(args: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "write".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let rendered = value.render();
    output.borrow_mut().push_str(&rendered);
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], output: &OutputRef) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline".into(),
            expected: "exactly 0 arguments".into(),
            got: args.len(),
        });
    }

    output.borrow_mut().push('\n');
    Ok(Value::Void)
}

fn eval_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for value in args {
        let segment = value.as_string()?;
        result.push_str(&segment);
    }
    Ok(Value::String(result))
}

fn eval_string_length(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Integer(value.as_string()?.chars().count() as i64))
}

fn eval_substring(args: &[Value]) -> Result<Value, EvalError> {
    let [value, start, end] = args else {
        return Err(EvalError::WrongArgCount {
            name: "substring".into(),
            expected: "exactly 3 arguments".into(),
            got: args.len(),
        });
    };

    let source = value.as_string()?;
    let chars = source.chars().collect::<Vec<_>>();
    let start = non_negative_index(start)?;
    let end = non_negative_index(end)?;

    if start > end || end > chars.len() {
        return Err(EvalError::InvalidRange {
            start,
            end,
            len: chars.len(),
        });
    }

    Ok(Value::String(chars[start..end].iter().collect()))
}

fn eval_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->number".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    match value.as_string()?.parse::<i64>() {
        Ok(number) => Ok(Value::Integer(number)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn eval_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "number->string".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::String(value.as_integer()?.to_string()))
}

fn eval_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::String(value.as_symbol()?.to_string()))
}

fn eval_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Symbol(value.as_string()?.to_string()))
}

fn eval_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-ref".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let source = value.as_string()?;
    let index = non_negative_index(index)?;

    match source.chars().nth(index) {
        Some(ch) => Ok(Value::Char(ch)),
        None => Err(EvalError::IndexOutOfBounds {
            index,
            len: source.chars().count(),
        }),
    }
}

fn eval_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-copy".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::MutableString(Rc::new(RefCell::new(
        value.as_string()?.chars().collect(),
    ))))
}

fn eval_string_set(args: &[Value]) -> Result<Value, EvalError> {
    let [value, index, ch] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-set!".into(),
            expected: "exactly 3 arguments".into(),
            got: args.len(),
        });
    };

    let index = non_negative_index(index)?;
    let ch = ch.as_char()?;

    let Value::MutableString(chars) = value else {
        return match value {
            Value::String(_) => Err(EvalError::ImmutableString),
            _ => Err(EvalError::TypeMismatch {
                expected: "string",
                found: value.type_name().into(),
            }),
        };
    };

    let mut chars = chars.borrow_mut();
    if index >= chars.len() {
        return Err(EvalError::IndexOutOfBounds {
            index,
            len: chars.len(),
        });
    }

    chars[index] = ch;
    Ok(Value::Void)
}

fn non_negative_index(value: &Value) -> Result<usize, EvalError> {
    let index = value.as_integer()?;
    if index < 0 {
        return Err(EvalError::NegativeIndex { value: index });
    }
    Ok(index as usize)
}

fn eval_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: FnOnce(&Value) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(predicate(value)))
}

fn eval_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "pair?".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(
        matches!(value, Value::List(items) if !items.is_empty()),
    ))
}

fn numeric_args(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(Value::as_integer).collect()
}
