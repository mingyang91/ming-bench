use super::{
    apply, byte_index_for_char, compare, compare_chars, compare_strings, equal_value, expect_char,
    expect_list, expect_non_negative_integer, expect_number, expect_numbers, expect_string,
    expect_string_values, expect_symbol, expect_two_numbers, invalid_argument, make_string_value,
    number_error, number_predicate, predicate, type_mismatch, wrong_arg_count, EvalContext,
    EvalError, Number, PairValue, SourcePos, Value,
};

pub(super) fn builtin_name(name: &str) -> Option<&'static str> {
    match name {
        "+" => Some("+"),
        "-" => Some("-"),
        "*" => Some("*"),
        "/" => Some("/"),
        "<" => Some("<"),
        ">" => Some(">"),
        "=" => Some("="),
        "<=" => Some("<="),
        "abs" => Some("abs"),
        "apply" => Some("apply"),
        "append" => Some("append"),
        "assoc" => Some("assoc"),
        "boolean?" => Some("boolean?"),
        "char-alphabetic?" => Some("char-alphabetic?"),
        "char-downcase" => Some("char-downcase"),
        "char-numeric?" => Some("char-numeric?"),
        "char-upcase" => Some("char-upcase"),
        "char=?" => Some("char=?"),
        "char<?" => Some("char<?"),
        "char?" => Some("char?"),
        "car" => Some("car"),
        "cdr" => Some("cdr"),
        "cons" => Some("cons"),
        "display" => Some("display"),
        "eq?" => Some("eq?"),
        "exact?" => Some("exact?"),
        "exact->inexact" => Some("exact->inexact"),
        "equal?" => Some("equal?"),
        "even?" => Some("even?"),
        "expt" => Some("expt"),
        "inexact?" => Some("inexact?"),
        "inexact->exact" => Some("inexact->exact"),
        "integer?" => Some("integer?"),
        "length" => Some("length"),
        "list" => Some("list"),
        "list-ref" => Some("list-ref"),
        "list-tail" => Some("list-tail"),
        "list?" => Some("list?"),
        "map" => Some("map"),
        "max" => Some("max"),
        "min" => Some("min"),
        "modulo" => Some("modulo"),
        "negative?" => Some("negative?"),
        "newline" => Some("newline"),
        "number->string" => Some("number->string"),
        "not" => Some("not"),
        "null?" => Some("null?"),
        "number?" => Some("number?"),
        "odd?" => Some("odd?"),
        "pair?" => Some("pair?"),
        "positive?" => Some("positive?"),
        "quotient" => Some("quotient"),
        "rational?" => Some("rational?"),
        "remainder" => Some("remainder"),
        "string-ci=?" => Some("string-ci=?"),
        "string-downcase" => Some("string-downcase"),
        "string->number" => Some("string->number"),
        "string->symbol" => Some("string->symbol"),
        "string-append" => Some("string-append"),
        "string-copy" => Some("string-copy"),
        "string-upcase" => Some("string-upcase"),
        "string=?" => Some("string=?"),
        "string<?" => Some("string<?"),
        "string-length" => Some("string-length"),
        "string-ref" => Some("string-ref"),
        "string-set!" => Some("string-set!"),
        "string?" => Some("string?"),
        "substring" => Some("substring"),
        "symbol->string" => Some("symbol->string"),
        "symbol?" => Some("symbol?"),
        "numerator" => Some("numerator"),
        "denominator" => Some("denominator"),
        "write" => Some("write"),
        "zero?" => Some("zero?"),
        _ => None,
    }
}

pub(super) fn apply_builtin(
    name: &str,
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    match name {
        "+" => add(args, pos),
        "-" => subtract(args, pos),
        "*" => multiply(args, pos),
        "/" => divide(args, pos),
        "<" => compare(name, args, pos, |left, right| left < right),
        ">" => compare(name, args, pos, |left, right| left > right),
        "=" => compare(name, args, pos, |left, right| left == right),
        "<=" => compare(name, args, pos, |left, right| left <= right),
        "abs" => abs_value(args, pos),
        "apply" => builtin_apply(args, pos, context),
        "append" => append(args, pos),
        "assoc" => assoc(args, pos),
        "boolean?" => predicate(args, "boolean?", pos, |value| {
            matches!(value, Value::Boolean(_))
        }),
        "char-alphabetic?" => predicate(
            args,
            "char-alphabetic?",
            pos,
            |value| matches!(value, Value::Character(ch) if ch.is_alphabetic()),
        ),
        "char-downcase" => char_downcase(args, pos),
        "char-numeric?" => predicate(
            args,
            "char-numeric?",
            pos,
            |value| matches!(value, Value::Character(ch) if ch.is_numeric()),
        ),
        "char-upcase" => char_upcase(args, pos),
        "char=?" => compare_chars(name, args, pos, |left, right| left == right),
        "char<?" => compare_chars(name, args, pos, |left, right| left < right),
        "char?" => predicate(args, "char?", pos, |value| {
            matches!(value, Value::Character(_))
        }),
        "car" => car(args, pos),
        "cdr" => cdr(args, pos),
        "cons" => cons(args, pos),
        "denominator" => denominator(args, pos),
        "display" => display(args, pos, context),
        "eq?" => eq_predicate(args, pos),
        "exact?" => predicate(args, "exact?", pos, |value| {
            matches!(value, Value::Number(number) if number.is_exact())
        }),
        "exact->inexact" => exact_to_inexact(args, pos),
        "equal?" => equal_predicate(args, pos),
        "even?" => number_predicate(args, "even?", pos, |value| {
            Ok(expect_exact_integer_value("even?", *value, pos)? % 2 == 0)
        }),
        "expt" => expt(args, pos),
        "inexact?" => predicate(args, "inexact?", pos, |value| {
            matches!(value, Value::Number(number) if number.is_inexact())
        }),
        "inexact->exact" => inexact_to_exact(args, pos),
        "integer?" => predicate(args, "integer?", pos, |value| {
            matches!(value, Value::Number(number) if number.is_integer())
        }),
        "length" => length(args, pos),
        "list" => Ok(Value::List(args.to_vec())),
        "list-ref" => list_ref(args, pos),
        "list-tail" => list_tail(args, pos),
        "list?" => predicate(args, "list?", pos, |value| matches!(value, Value::List(_))),
        "map" => builtin_map(args, pos, context),
        "max" => max_value(args, pos),
        "min" => min_value(args, pos),
        "modulo" => modulo(args, pos),
        "negative?" => number_predicate(args, "negative?", pos, |value| {
            Ok(*value < Number::exact_integer(0))
        }),
        "newline" => newline(args, pos, context),
        "number->string" => number_to_string(args, pos),
        "not" => builtin_not(args, pos),
        "null?" => predicate(
            args,
            "null?",
            pos,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate(args, "number?", pos, |value| {
            matches!(value, Value::Number(_))
        }),
        "numerator" => numerator(args, pos),
        "odd?" => number_predicate(args, "odd?", pos, |value| {
            Ok(expect_exact_integer_value("odd?", *value, pos)? % 2 != 0)
        }),
        "pair?" => predicate(args, "pair?", pos, |value| {
            matches!(value, Value::List(items) if !items.is_empty())
                || matches!(value, Value::Pair(_))
        }),
        "positive?" => number_predicate(args, "positive?", pos, |value| {
            Ok(*value > Number::exact_integer(0))
        }),
        "quotient" => quotient(args, pos),
        "rational?" => predicate(args, "rational?", pos, |value| {
            matches!(value, Value::Number(number) if number.is_rational())
        }),
        "remainder" => remainder(args, pos),
        "string-ci=?" => string_ci_equal(args, pos),
        "string-downcase" => string_downcase(args, pos),
        "string->number" => string_to_number(args, pos),
        "string->symbol" => string_to_symbol(args, pos),
        "string-append" => string_append(args, pos),
        "string-copy" => string_copy(args, pos),
        "string-upcase" => string_upcase(args, pos),
        "string=?" => compare_strings(name, args, pos, |left, right| left == right),
        "string<?" => compare_strings(name, args, pos, |left, right| left < right),
        "string-length" => string_length(args, pos),
        "string-ref" => string_ref(args, pos),
        "string-set!" => string_set(args, pos),
        "string?" => predicate(args, "string?", pos, |value| {
            matches!(value, Value::String(_))
        }),
        "substring" => substring(args, pos),
        "symbol->string" => symbol_to_string(args, pos),
        "symbol?" => predicate(args, "symbol?", pos, |value| {
            matches!(value, Value::Symbol(_))
        }),
        "write" => write(args, pos, context),
        "zero?" => number_predicate(args, "zero?", pos, |value| {
            Ok(*value == Number::exact_integer(0))
        }),
        _ => unreachable!("unsupported builtin: {name}"),
    }
}

fn add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("+", args, pos)?;
    let mut total = Number::exact_integer(0);
    for value in values {
        total = total.add(value).map_err(|error| number_error(pos, "+", error))?;
    }
    Ok(Value::Number(total))
}

fn subtract(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("-", args, pos)?;
    match values.as_slice() {
        [] => Err(wrong_arg_count(pos, "-", "at least 1 argument", 0)),
        [value] => Ok(Value::Number(
            Number::exact_integer(0)
                .sub(*value)
                .map_err(|error| number_error(pos, "-", error))?,
        )),
        [first, rest @ ..] => {
            let mut total = *first;
            for value in rest {
                total = total
                    .sub(*value)
                    .map_err(|error| number_error(pos, "-", error))?;
            }
            Ok(Value::Number(total))
        }
    }
}

fn multiply(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("*", args, pos)?;
    let mut total = Number::exact_integer(1);
    for value in values {
        total = total.mul(value).map_err(|error| number_error(pos, "*", error))?;
    }
    Ok(Value::Number(total))
}

fn divide(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("/", args, pos)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "/", "at least 2 arguments", 0))?;

    if rest.is_empty() {
        return Err(wrong_arg_count(pos, "/", "at least 2 arguments", 1));
    }

    let mut total = *first;
    for value in rest {
        total = total
            .div(*value)
            .map_err(|error| number_error(pos, "/", error))?;
    }

    Ok(Value::Number(total))
}

fn abs_value(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_number("abs", value, pos)?;
            let value = value.abs().map_err(|error| number_error(pos, "abs", error))?;
            Ok(Value::Number(value))
        }
        _ => Err(wrong_arg_count(
            pos,
            "abs",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn min_value(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("min", args, pos)?;
    match values.into_iter().reduce(|left, right| {
        if right < left {
            right
        } else {
            left
        }
    }) {
        Some(value) => Ok(Value::Number(value)),
        None => Err(wrong_arg_count(pos, "min", "at least 1 argument", 0)),
    }
}

fn max_value(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("max", args, pos)?;
    match values.into_iter().reduce(|left, right| {
        if right > left {
            right
        } else {
            left
        }
    }) {
        Some(value) => Ok(Value::Number(value)),
        None => Err(wrong_arg_count(pos, "max", "at least 1 argument", 0)),
    }
}

fn quotient(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("quotient", args, pos)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero { pos });
    }

    Ok(Value::Number(Number::exact_integer(dividend / divisor)))
}

fn remainder(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("remainder", args, pos)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero { pos });
    }

    Ok(Value::Number(Number::exact_integer(dividend % divisor)))
}

fn modulo(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_exact_integers("modulo", args, pos)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero { pos });
    }

    let remainder = dividend % divisor;
    let modulo = if remainder != 0 && (remainder < 0) != (divisor < 0) {
        remainder + divisor
    } else {
        remainder
    };

    Ok(Value::Number(Number::exact_integer(modulo)))
}

fn expt(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [base, exponent] => {
            let base = expect_exact_integer_arg("expt", base, pos)?;
            let exponent = expect_exact_integer_arg("expt", exponent, pos)?;
            let exponent = u32::try_from(exponent).map_err(|_| {
                invalid_argument(pos, "expt", "exponent must be a non-negative integer")
            })?;
            let value = base.checked_pow(exponent).ok_or_else(|| {
                invalid_argument(pos, "expt", "result overflowed the integer range")
            })?;
            Ok(Value::Number(Number::exact_integer(value)))
        }
        _ => Err(wrong_arg_count(
            pos,
            "expt",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn builtin_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count(
            pos,
            "not",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [head, Value::List(tail)] => {
            let mut items = Vec::with_capacity(tail.len() + 1);
            items.push(head.clone());
            items.extend(tail.iter().cloned());
            Ok(Value::List(items))
        }
        [head, tail] => Ok(Value::Pair(Box::new(PairValue {
            car: head.clone(),
            cdr: tail.clone(),
        }))),
        _ => Err(wrong_arg_count(
            pos,
            "cons",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [Value::List(items)] if !items.is_empty() => Ok(items[0].clone()),
        [Value::Pair(pair)] => Ok(pair.car.clone()),
        [other] => Err(type_mismatch(pos, "car", "pair", other.type_name())),
        _ => Err(wrong_arg_count(
            pos,
            "car",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [Value::List(items)] if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        [Value::Pair(pair)] => Ok(pair.cdr.clone()),
        [other] => Err(type_mismatch(pos, "cdr", "pair", other.type_name())),
        _ => Err(wrong_arg_count(
            pos,
            "cdr",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for value in args {
        let list = expect_list("append", value, pos)?;
        items.extend(list.iter().cloned());
    }

    Ok(Value::List(items))
}

fn builtin_apply(
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (operator, rest) = args
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "apply", "at least 2 arguments", 0))?;
    let (list_arg, prefix_args) = rest
        .split_last()
        .ok_or_else(|| wrong_arg_count(pos, "apply", "at least 2 arguments", 1))?;

    let list_args = expect_list("apply", list_arg, pos)?;
    let mut expanded_args = Vec::with_capacity(prefix_args.len() + list_args.len());
    expanded_args.extend(prefix_args.iter().cloned());
    expanded_args.extend(list_args.iter().cloned());

    apply(operator.clone(), &expanded_args, pos, context)
}

fn length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(Number::exact_integer(
            expect_list("length", value, pos)?.len() as i64,
        ))),
        _ => Err(wrong_arg_count(
            pos,
            "length",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn list_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [list, index] => {
            let list = expect_list("list-ref", list, pos)?;
            let index = expect_non_negative_integer("list-ref", index, pos, "index")?;
            list.get(index).cloned().ok_or_else(|| {
                invalid_argument(
                    pos,
                    "list-ref",
                    format!(
                        "index {index} out of range for list of length {}",
                        list.len()
                    ),
                )
            })
        }
        _ => Err(wrong_arg_count(
            pos,
            "list-ref",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn list_tail(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [list, index] => {
            let list = expect_list("list-tail", list, pos)?;
            let index = expect_non_negative_integer("list-tail", index, pos, "index")?;
            if index > list.len() {
                return Err(invalid_argument(
                    pos,
                    "list-tail",
                    format!(
                        "index {index} out of range for list of length {}",
                        list.len()
                    ),
                ));
            }

            Ok(Value::List(list[index..].to_vec()))
        }
        _ => Err(wrong_arg_count(
            pos,
            "list-tail",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn assoc(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [key, alist] => {
            let alist = expect_list("assoc", alist, pos)?;

            for entry in alist {
                let entry_key = match entry {
                    Value::List(items) if !items.is_empty() => &items[0],
                    Value::Pair(pair) => &pair.car,
                    other => return Err(type_mismatch(pos, "assoc", "pair", other.type_name())),
                };

                if equal_value(key, entry_key) {
                    return Ok(entry.clone());
                }
            }

            Ok(Value::Boolean(false))
        }
        _ => Err(wrong_arg_count(
            pos,
            "assoc",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn builtin_map(
    args: &[Value],
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (operator, list_args) = args
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "map", "at least 2 arguments", 0))?;

    if list_args.is_empty() {
        return Err(wrong_arg_count(pos, "map", "at least 2 arguments", 1));
    }

    let lists = list_args
        .iter()
        .map(|value| expect_list("map", value, pos))
        .collect::<Result<Vec<_>, _>>()?;
    let len = lists[0].len();

    if lists.iter().any(|list| list.len() != len) {
        return Err(invalid_argument(
            pos,
            "map",
            "all input lists must have the same length",
        ));
    }

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply(operator.clone(), &call_args, pos, context)?);
    }

    Ok(Value::List(results))
}

fn display(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    match args {
        [value] => {
            context.output.push_str(&value.render_display());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count(
            pos,
            "display",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn write(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    match args {
        [value] => {
            context.output.push_str(&value.render());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count(
            pos,
            "write",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn newline(args: &[Value], pos: SourcePos, context: &mut EvalContext) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(wrong_arg_count(
            pos,
            "newline",
            "exactly 0 arguments",
            args.len(),
        ));
    }

    context.output.push('\n');
    Ok(Value::Void)
}

fn string_append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut result = String::new();

    for value in args {
        let value = expect_string("string-append", value, pos)?;
        result.push_str(&value.borrow());
    }

    Ok(make_string_value(result))
}

fn string_ci_equal(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_string_values("string-ci=?", args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            "string-ci=?",
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values
        .windows(2)
        .all(|pair| pair[0].to_lowercase() == pair[1].to_lowercase());
    Ok(Value::Boolean(result))
}

fn string_copy(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-copy", value, pos)?;
            let copy = value.borrow().clone();
            Ok(make_string_value(copy))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-copy",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_upcase(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-upcase", value, pos)?;
            let transformed: String = value
                .borrow()
                .chars()
                .flat_map(char::to_uppercase)
                .collect();
            Ok(make_string_value(transformed))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-upcase",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_downcase(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-downcase", value, pos)?;
            let transformed: String = value
                .borrow()
                .chars()
                .flat_map(char::to_lowercase)
                .collect();
            Ok(make_string_value(transformed))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-downcase",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string-length", value, pos)?;
            let len = value.borrow().chars().count() as i64;
            Ok(Value::Number(Number::exact_integer(len)))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-length",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn substring(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, start, end] => {
            let string = expect_string("substring", string, pos)?;
            let start = expect_non_negative_integer("substring", start, pos, "start index")?;
            let end = expect_non_negative_integer("substring", end, pos, "end index")?;
            let string = string.borrow();
            let len = string.chars().count();

            if start > end {
                return Err(invalid_argument(
                    pos,
                    "substring",
                    format!("start index {start} cannot exceed end index {end}"),
                ));
            }

            if end > len {
                return Err(invalid_argument(
                    pos,
                    "substring",
                    format!("end index {end} out of range for string of length {len}"),
                ));
            }

            let start_byte = byte_index_for_char(&string, start).unwrap_or(string.len());
            let end_byte = byte_index_for_char(&string, end).unwrap_or(string.len());
            Ok(make_string_value(string[start_byte..end_byte].to_string()))
        }
        _ => Err(wrong_arg_count(
            pos,
            "substring",
            "exactly 3 arguments",
            args.len(),
        )),
    }
}

fn string_to_number(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let string = expect_string("string->number", value, pos)?;
            let parsed = super::parse_number_literal(&string.borrow());
            Ok(match parsed {
                Ok(number) => Value::Number(number),
                Err(_) => Value::Boolean(false),
            })
        }
        _ => Err(wrong_arg_count(
            pos,
            "string->number",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn number_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [Value::Number(number)] => Ok(make_string_value(number.render())),
        [other] => Err(type_mismatch(
            pos,
            "number->string",
            "number",
            other.type_name(),
        )),
        _ => Err(wrong_arg_count(
            pos,
            "number->string",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn symbol_to_string(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(make_string_value(
            expect_symbol("symbol->string", value, pos)?.to_string(),
        )),
        _ => Err(wrong_arg_count(
            pos,
            "symbol->string",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_to_symbol(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let value = expect_string("string->symbol", value, pos)?;
            let symbol = value.borrow().clone();
            Ok(Value::Symbol(symbol))
        }
        _ => Err(wrong_arg_count(
            pos,
            "string->symbol",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn string_ref(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, index] => {
            let string = expect_string("string-ref", string, pos)?;
            let index = expect_non_negative_integer("string-ref", index, pos, "index")?;
            let string = string.borrow();
            let len = string.chars().count();

            match string.chars().nth(index) {
                Some(ch) => Ok(Value::Character(ch)),
                None => Err(invalid_argument(
                    pos,
                    "string-ref",
                    format!("index {index} out of range for string of length {len}"),
                )),
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "string-ref",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn string_set(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [string, index, Value::Character(ch)] => {
            let string = expect_string("string-set!", string, pos)?;
            let index = expect_non_negative_integer("string-set!", index, pos, "index")?;
            let mut string = string.borrow_mut();
            let len = string.chars().count();

            if index >= len {
                return Err(invalid_argument(
                    pos,
                    "string-set!",
                    format!("index {index} out of range for string of length {len}"),
                ));
            }

            let start = byte_index_for_char(&string, index)
                .expect("valid character index must have a byte offset");
            let end = byte_index_for_char(&string, index + 1)
                .expect("valid character index must have an end byte offset");
            let replacement = ch.to_string();
            string.replace_range(start..end, &replacement);
            Ok(Value::Void)
        }
        [_, _, other] => Err(type_mismatch(pos, "string-set!", "char", other.type_name())),
        _ => Err(wrong_arg_count(
            pos,
            "string-set!",
            "exactly 3 arguments",
            args.len(),
        )),
    }
}

fn char_upcase(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let ch = expect_char("char-upcase", value, pos)?;
            Ok(Value::Character(ch.to_uppercase().next().unwrap_or(ch)))
        }
        _ => Err(wrong_arg_count(
            pos,
            "char-upcase",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn char_downcase(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let ch = expect_char("char-downcase", value, pos)?;
            Ok(Value::Character(ch.to_lowercase().next().unwrap_or(ch)))
        }
        _ => Err(wrong_arg_count(
            pos,
            "char-downcase",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn eq_predicate(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(equal_value(left, right))),
        _ => Err(wrong_arg_count(
            pos,
            "eq?",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn equal_predicate(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(equal_value(left, right))),
        _ => Err(wrong_arg_count(
            pos,
            "equal?",
            "exactly 2 arguments",
            args.len(),
        )),
    }
}

fn exact_to_inexact(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(expect_number("exact->inexact", value, pos)?.to_inexact())),
        _ => Err(wrong_arg_count(
            pos,
            "exact->inexact",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn inexact_to_exact(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let number = expect_number("inexact->exact", value, pos)?
                .to_exact()
                .map_err(|error| number_error(pos, "inexact->exact", error))?;
            Ok(Value::Number(number))
        }
        _ => Err(wrong_arg_count(
            pos,
            "inexact->exact",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn numerator(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let number = expect_number("numerator", value, pos)?;
            match number.numerator() {
                Some(numerator) => Ok(Value::Number(Number::exact_integer(numerator))),
                None => Err(type_mismatch(pos, "numerator", "exact number", "number")),
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "numerator",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn denominator(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let number = expect_number("denominator", value, pos)?;
            match number.denominator() {
                Some(denominator) => Ok(Value::Number(Number::exact_integer(denominator))),
                None => Err(type_mismatch(pos, "denominator", "exact number", "number")),
            }
        }
        _ => Err(wrong_arg_count(
            pos,
            "denominator",
            "exactly 1 argument",
            args.len(),
        )),
    }
}

fn expect_exact_integer_arg(name: &str, value: &Value, pos: SourcePos) -> Result<i64, EvalError> {
    let number = expect_number(name, value, pos)?;
    expect_exact_integer_value(name, number, pos)
}

fn expect_exact_integer_value(name: &str, value: Number, pos: SourcePos) -> Result<i64, EvalError> {
    value
        .exact_integer_value()
        .ok_or_else(|| type_mismatch(pos, name, "exact integer", "number"))
}

fn expect_two_exact_integers(
    name: &str,
    args: &[Value],
    pos: SourcePos,
) -> Result<(i64, i64), EvalError> {
    let (left, right) = expect_two_numbers(name, args, pos)?;
    Ok((
        expect_exact_integer_value(name, left, pos)?,
        expect_exact_integer_value(name, right, pos)?,
    ))
}
