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
        BuiltinKind::EqPred,
        BuiltinKind::EqualPred,
        BuiltinKind::Map,
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
        BuiltinKind::Abs,
        BuiltinKind::Modulo,
        BuiltinKind::Remainder,
        BuiltinKind::Quotient,
        BuiltinKind::Min,
        BuiltinKind::Max,
        BuiltinKind::Expt,
        BuiltinKind::ZeroPred,
        BuiltinKind::PositivePred,
        BuiltinKind::NegativePred,
        BuiltinKind::OddPred,
        BuiltinKind::EvenPred,
        BuiltinKind::ListRef,
        BuiltinKind::ListTail,
        BuiltinKind::ListPred,
        BuiltinKind::Assoc,
        BuiltinKind::CharAlphabeticPred,
        BuiltinKind::CharNumericPred,
        BuiltinKind::CharUpcase,
        BuiltinKind::CharDowncase,
        BuiltinKind::CharEqual,
        BuiltinKind::CharLessThan,
        BuiltinKind::StringEqual,
        BuiltinKind::StringLessThan,
        BuiltinKind::StringCiEqual,
        BuiltinKind::StringUpcase,
        BuiltinKind::StringDowncase,
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
        BuiltinKind::EqPred => eval_equality("eq?", args),
        BuiltinKind::EqualPred => eval_equality("equal?", args),
        BuiltinKind::Map => eval_map(args),
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
        BuiltinKind::Abs => eval_abs(args),
        BuiltinKind::Modulo => eval_modulo(args),
        BuiltinKind::Remainder => eval_remainder(args),
        BuiltinKind::Quotient => eval_quotient(args),
        BuiltinKind::Min => eval_min(args),
        BuiltinKind::Max => eval_max(args),
        BuiltinKind::Expt => eval_expt(args),
        BuiltinKind::ZeroPred => eval_integer_predicate("zero?", args, |value| value == 0),
        BuiltinKind::PositivePred => eval_integer_predicate("positive?", args, |value| value > 0),
        BuiltinKind::NegativePred => eval_integer_predicate("negative?", args, |value| value < 0),
        BuiltinKind::OddPred => eval_integer_predicate("odd?", args, |value| value % 2 != 0),
        BuiltinKind::EvenPred => eval_integer_predicate("even?", args, |value| value % 2 == 0),
        BuiltinKind::ListRef => eval_list_ref(args),
        BuiltinKind::ListTail => eval_list_tail(args),
        BuiltinKind::ListPred => {
            eval_predicate("list?", args, |value| matches!(value, Value::List(_)))
        }
        BuiltinKind::Assoc => eval_assoc(args),
        BuiltinKind::CharAlphabeticPred => {
            eval_char_predicate("char-alphabetic?", args, char::is_alphabetic)
        }
        BuiltinKind::CharNumericPred => {
            eval_char_predicate("char-numeric?", args, char::is_numeric)
        }
        BuiltinKind::CharUpcase => eval_char_transform("char-upcase", args, |value| {
            value.to_uppercase().next().unwrap_or(value)
        }),
        BuiltinKind::CharDowncase => eval_char_transform("char-downcase", args, |value| {
            value.to_lowercase().next().unwrap_or(value)
        }),
        BuiltinKind::CharEqual => eval_char_compare("char=?", args, |lhs, rhs| lhs == rhs),
        BuiltinKind::CharLessThan => eval_char_compare("char<?", args, |lhs, rhs| lhs < rhs),
        BuiltinKind::StringEqual => eval_string_compare("string=?", args, |lhs, rhs| lhs == rhs),
        BuiltinKind::StringLessThan => eval_string_compare("string<?", args, |lhs, rhs| lhs < rhs),
        BuiltinKind::StringCiEqual => eval_string_compare("string-ci=?", args, |lhs, rhs| {
            lhs.to_lowercase() == rhs.to_lowercase()
        }),
        BuiltinKind::StringUpcase => {
            eval_string_transform("string-upcase", args, |value| value.to_uppercase())
        }
        BuiltinKind::StringDowncase => {
            eval_string_transform("string-downcase", args, |value| value.to_lowercase())
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

    match rest {
        Value::List(items) => {
            let mut result = Vec::with_capacity(items.len() + 1);
            result.push(first.clone());
            result.extend(items.iter().cloned());
            Ok(Value::List(result))
        }
        Value::ImproperList(items, tail) => {
            let mut result = Vec::with_capacity(items.len() + 1);
            result.push(first.clone());
            result.extend(items.iter().cloned());
            Ok(Value::ImproperList(result, tail.clone()))
        }
        _ => Ok(Value::ImproperList(
            vec![first.clone()],
            Box::new(rest.clone()),
        )),
    }
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    match value {
        Value::List(items) => items
            .first()
            .cloned()
            .ok_or_else(|| EvalError::TypeMismatch {
                expected: "pair",
                found: value.type_name().into(),
            }),
        Value::ImproperList(items, _) => Ok(items[0].clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        }),
    }
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    match value {
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::TypeMismatch {
                    expected: "pair",
                    found: value.type_name().into(),
                });
            }

            Ok(Value::List(items[1..].to_vec()))
        }
        Value::ImproperList(items, tail) => {
            if items.len() == 1 {
                Ok((**tail).clone())
            } else {
                Ok(Value::ImproperList(items[1..].to_vec(), tail.clone()))
            }
        }
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        }),
    }
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

fn eval_equality(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(super::values_equal(lhs, rhs)))
}

fn eval_map(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2 arguments".into(),
            got: args.len(),
        });
    }

    let callable = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(list_items)
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = lists[0].len();
    for list in &lists[1..] {
        if list.len() != expected_len {
            return Err(EvalError::LengthMismatch {
                name: "map".into(),
                expected: expected_len,
                got: list.len(),
            });
        }
    }

    let mut result = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        result.push(super::apply_callable(callable.clone(), &call_args)?);
    }

    Ok(Value::List(result))
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

fn eval_abs(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "abs".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Integer(
        value
            .as_integer()?
            .checked_abs()
            .ok_or(EvalError::IntegerOverflow)?,
    ))
}

fn eval_modulo(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_numeric_args("modulo", args)?;
    let remainder = dividend
        .checked_rem(divisor)
        .ok_or(EvalError::IntegerOverflow)?;
    let result = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder
            .checked_add(divisor)
            .ok_or(EvalError::IntegerOverflow)?
    } else {
        remainder
    };
    Ok(Value::Integer(result))
}

fn eval_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_numeric_args("remainder", args)?;
    Ok(Value::Integer(
        dividend
            .checked_rem(divisor)
            .ok_or(EvalError::IntegerOverflow)?,
    ))
}

fn eval_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_numeric_args("quotient", args)?;
    Ok(Value::Integer(
        dividend
            .checked_div(divisor)
            .ok_or(EvalError::IntegerOverflow)?,
    ))
}

fn eval_min(args: &[Value]) -> Result<Value, EvalError> {
    let values = at_least_one_numeric_arg("min", args)?;
    Ok(Value::Integer(
        *values.iter().min().expect("at least one value"),
    ))
}

fn eval_max(args: &[Value]) -> Result<Value, EvalError> {
    let values = at_least_one_numeric_arg("max", args)?;
    Ok(Value::Integer(
        *values.iter().max().expect("at least one value"),
    ))
}

fn eval_expt(args: &[Value]) -> Result<Value, EvalError> {
    let [base, exponent] = args else {
        return Err(EvalError::WrongArgCount {
            name: "expt".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let base = base.as_integer()?;
    let exponent = exponent.as_integer()?;
    if exponent < 0 {
        return Err(EvalError::NegativeExponent { value: exponent });
    }

    let mut result = 1_i64;
    let mut factor = base;
    let mut power = exponent as u64;

    while power > 0 {
        if power & 1 == 1 {
            result = result
                .checked_mul(factor)
                .ok_or(EvalError::IntegerOverflow)?;
        }
        power >>= 1;
        if power > 0 {
            factor = factor
                .checked_mul(factor)
                .ok_or(EvalError::IntegerOverflow)?;
        }
    }

    Ok(Value::Integer(result))
}

fn eval_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let items = list_items(list)?;
    let index = non_negative_index(index)?;

    items
        .get(index)
        .cloned()
        .ok_or(EvalError::IndexOutOfBounds {
            index,
            len: items.len(),
        })
}

fn eval_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-tail".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let items = list_items(list)?;
    let index = non_negative_index(index)?;
    if index > items.len() {
        return Err(EvalError::IndexOutOfBounds {
            index,
            len: items.len(),
        });
    }

    Ok(Value::List(items[index..].to_vec()))
}

fn eval_assoc(args: &[Value]) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(EvalError::WrongArgCount {
            name: "assoc".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    for entry in list_items(alist)? {
        let entry_key = pair_head(entry)?;
        if super::values_equal(key, entry_key) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Boolean(false))
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
        matches!(value, Value::List(items) if !items.is_empty())
            || matches!(value, Value::ImproperList(_, _)),
    ))
}

fn numeric_args(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(Value::as_integer).collect()
}

fn at_least_one_numeric_arg(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let values = numeric_args(args)?;
    if values.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 1 argument".into(),
            got: 0,
        });
    }
    Ok(values)
}

fn binary_numeric_args(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let rhs = rhs.as_integer()?;
    if rhs == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok((lhs.as_integer()?, rhs))
}

fn eval_integer_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: FnOnce(i64) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(predicate(value.as_integer()?)))
}

fn list_items(value: &Value) -> Result<&[Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        _ => Err(EvalError::TypeMismatch {
            expected: "list",
            found: value.type_name().into(),
        }),
    }
}

fn pair_head(value: &Value) -> Result<&Value, EvalError> {
    match value {
        Value::List(items) if !items.is_empty() => Ok(&items[0]),
        Value::ImproperList(items, _) => Ok(&items[0]),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        }),
    }
}

fn eval_char_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: FnOnce(char) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(predicate(value.as_char()?)))
}

fn eval_char_transform<F>(name: &str, args: &[Value], transform: F) -> Result<Value, EvalError>
where
    F: FnOnce(char) -> char,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Char(transform(value.as_char()?)))
}

fn eval_char_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    let values = args
        .iter()
        .map(Value::as_char)
        .collect::<Result<Vec<_>, _>>()?;

    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
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

fn eval_string_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    let values = args
        .iter()
        .map(Value::as_string)
        .collect::<Result<Vec<_>, _>>()?;

    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2 arguments".into(),
            got: values.len(),
        });
    }

    for pair in values.windows(2) {
        if !compare(&pair[0], &pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn eval_string_transform<F>(name: &str, args: &[Value], transform: F) -> Result<Value, EvalError>
where
    F: FnOnce(String) -> String,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::String(transform(value.as_string()?)))
}
