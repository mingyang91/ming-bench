use std::rc::Rc;

use super::{
    apply,
    model::{Builtin, SchemeString, Value},
    wrong_arg_count, EvalError,
};

pub(super) fn apply_builtin(
    builtin: Builtin,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => builtin_add(args),
        Builtin::Sub => builtin_sub(args),
        Builtin::Mul => builtin_mul(args),
        Builtin::Div => builtin_div(args),
        Builtin::Abs => builtin_abs(args),
        Builtin::Modulo => builtin_modulo(args),
        Builtin::Remainder => builtin_remainder(args),
        Builtin::Quotient => builtin_quotient(args),
        Builtin::Min => builtin_min_max("min", args, |left, right| left.min(right)),
        Builtin::Max => builtin_min_max("max", args, |left, right| left.max(right)),
        Builtin::Expt => builtin_expt(args),
        Builtin::ZeroPred => builtin_number_predicate("zero?", args, |value| value == 0),
        Builtin::PositivePred => builtin_number_predicate("positive?", args, |value| value > 0),
        Builtin::NegativePred => builtin_number_predicate("negative?", args, |value| value < 0),
        Builtin::OddPred => builtin_number_predicate("odd?", args, |value| value % 2 != 0),
        Builtin::EvenPred => builtin_number_predicate("even?", args, |value| value % 2 == 0),
        Builtin::Less => compare_numbers("<", args, |left, right| left < right),
        Builtin::Greater => compare_numbers(">", args, |left, right| left > right),
        Builtin::Equal => compare_numbers("=", args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers("<=", args, |left, right| left <= right),
        Builtin::EqPred => builtin_eq(args),
        Builtin::EqualPred => builtin_equal(args),
        Builtin::Not => builtin_not(args),
        Builtin::Display => builtin_display(args, output),
        Builtin::Write => builtin_write(args, output),
        Builtin::Newline => builtin_newline(args, output),
        Builtin::Cons => builtin_cons(args),
        Builtin::Car => builtin_car(args),
        Builtin::Cdr => builtin_cdr(args),
        Builtin::Append => builtin_append(args),
        Builtin::List => builtin_list(args),
        Builtin::Length => builtin_length(args),
        Builtin::ListRef => builtin_list_ref(args),
        Builtin::ListTail => builtin_list_tail(args),
        Builtin::ListPred => {
            builtin_predicate("list?", args, |value| matches!(value, Value::List(_)))
        }
        Builtin::Assoc => builtin_assoc(args),
        Builtin::Map => builtin_map(args, output),
        Builtin::StringAppend => builtin_string_append(args),
        Builtin::StringLength => builtin_string_length(args),
        Builtin::Substring => builtin_substring(args),
        Builtin::StringToNumber => builtin_string_to_number(args),
        Builtin::NumberToString => builtin_number_to_string(args),
        Builtin::SymbolToString => builtin_symbol_to_string(args),
        Builtin::StringToSymbol => builtin_string_to_symbol(args),
        Builtin::StringRef => builtin_string_ref(args),
        Builtin::StringSet => builtin_string_set(args),
        Builtin::StringCopy => builtin_string_copy(args),
        Builtin::NullPred => builtin_predicate(
            "null?",
            args,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        Builtin::NumberPred => {
            builtin_predicate("number?", args, |value| matches!(value, Value::Integer(_)))
        }
        Builtin::StringPred => {
            builtin_predicate("string?", args, |value| matches!(value, Value::String(_)))
        }
        Builtin::BooleanPred => {
            builtin_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        Builtin::PairPred => builtin_predicate("pair?", args, is_pair),
        Builtin::SymbolPred => {
            builtin_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
        }
        Builtin::CharPred => {
            builtin_predicate("char?", args, |value| matches!(value, Value::Char(_)))
        }
        Builtin::CharAlphabeticPred => {
            builtin_char_predicate("char-alphabetic?", args, |ch| ch.is_alphabetic())
        }
        Builtin::CharNumericPred => {
            builtin_char_predicate("char-numeric?", args, |ch| ch.is_ascii_digit())
        }
        Builtin::CharUpcase => builtin_char_map("char-upcase", args, |ch| ch.to_ascii_uppercase()),
        Builtin::CharDowncase => {
            builtin_char_map("char-downcase", args, |ch| ch.to_ascii_lowercase())
        }
        Builtin::CharEqual => compare_chars("char=?", args, |left, right| left == right),
        Builtin::CharLess => compare_chars("char<?", args, |left, right| left < right),
        Builtin::StringEqual => compare_strings("string=?", args, |left, right| left == right),
        Builtin::StringLess => compare_strings("string<?", args, |left, right| left < right),
        Builtin::StringCiEqual => builtin_string_ci_equal(args),
        Builtin::StringUpcase => {
            builtin_string_map("string-upcase", args, |value| value.to_ascii_uppercase())
        }
        Builtin::StringDowncase => {
            builtin_string_map("string-downcase", args, |value| value.to_ascii_lowercase())
        }
        Builtin::Apply => builtin_apply(args, output),
    }
}

fn builtin_apply(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    let [callable, prefix_and_list @ ..] = args else {
        return Err(wrong_arg_count("apply", "at least 2", 0));
    };

    if prefix_and_list.is_empty() {
        return Err(wrong_arg_count("apply", "at least 2", 1));
    }

    let (list_arg, prefix_args) = prefix_and_list
        .split_last()
        .expect("prefix_and_list is known to be non-empty");
    let list_items = expect_list("apply", list_arg)?;

    let mut applied_args = Vec::with_capacity(prefix_args.len() + list_items.len());
    applied_args.extend(prefix_args.iter().cloned());
    applied_args.extend(list_items.iter().cloned());
    apply(callable.clone(), &applied_args, output)
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0;
    for arg in args {
        total += expect_number("+", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_abs(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(expect_number("abs", value)?.abs())),
        _ => Err(wrong_arg_count("abs", "1", args.len())),
    }
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(wrong_arg_count("-", "at least 1", 0)),
        [arg] => Ok(Value::Integer(-expect_number("-", arg)?)),
        [first, rest @ ..] => {
            let mut total = expect_number("-", first)?;
            for arg in rest {
                total -= expect_number("-", arg)?;
            }
            Ok(Value::Integer(total))
        }
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1;
    for arg in args {
        total *= expect_number("*", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(wrong_arg_count("/", "at least 2", 0));
    };

    if rest.is_empty() {
        return Err(wrong_arg_count("/", "at least 2", 1));
    }

    let mut total = expect_number("/", first)?;
    for arg in rest {
        let divisor = expect_number("/", arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }

    Ok(Value::Integer(total))
}

fn builtin_modulo(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_number("modulo", dividend)?;
            let divisor = expect_number("modulo", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }

            let mut remainder = dividend % divisor;
            if remainder != 0 && (remainder > 0) != (divisor > 0) {
                remainder += divisor;
            }
            Ok(Value::Integer(remainder))
        }
        _ => Err(wrong_arg_count("modulo", "2", args.len())),
    }
}

fn builtin_remainder(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_number("remainder", dividend)?;
            let divisor = expect_number("remainder", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Integer(dividend % divisor))
        }
        _ => Err(wrong_arg_count("remainder", "2", args.len())),
    }
}

fn builtin_quotient(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_number("quotient", dividend)?;
            let divisor = expect_number("quotient", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Integer(dividend / divisor))
        }
        _ => Err(wrong_arg_count("quotient", "2", args.len())),
    }
}

fn builtin_min_max<F>(name: &str, args: &[Value], choose: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> i64,
{
    let [first, rest @ ..] = args else {
        return Err(wrong_arg_count(name, "at least 1", 0));
    };

    let mut result = expect_number(name, first)?;
    for arg in rest {
        result = choose(result, expect_number(name, arg)?);
    }
    Ok(Value::Integer(result))
}

fn builtin_expt(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [base, exponent] => {
            let base = expect_number("expt", base)?;
            let exponent = expect_number("expt", exponent)?;
            let exponent = exponent.max(0) as u32;
            Ok(Value::Integer(base.pow(exponent)))
        }
        _ => Err(wrong_arg_count("expt", "2", args.len())),
    }
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count("not", "1", args.len())),
    }
}

fn builtin_display(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match args {
        [value] => {
            output.push_str(&value.render_display());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("display", "1", args.len())),
    }
}

fn builtin_write(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match args {
        [value] => {
            output.push_str(&value.render());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("write", "1", args.len())),
    }
}

fn builtin_newline(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match args {
        [] => {
            output.push('\n');
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("newline", "0", args.len())),
    }
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [head, tail] => match tail {
            Value::List(items) => {
                let mut new_items = Vec::with_capacity(items.len() + 1);
                new_items.push(head.clone());
                new_items.extend(items.iter().cloned());
                Ok(Value::List(new_items))
            }
            _ => Ok(Value::Pair(Box::new(head.clone()), Box::new(tail.clone()))),
        },
        _ => Err(wrong_arg_count("cons", "2", args.len())),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [Value::List(items)] if !items.is_empty() => Ok(items[0].clone()),
        [Value::Pair(head, _)] => Ok((**head).clone()),
        [value] => Err(EvalError::TypeMismatch {
            name: "car".into(),
            expected: "pair".into(),
            got: value.type_name().into(),
        }),
        _ => Err(wrong_arg_count("car", "1", args.len())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [Value::List(items)] if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        [Value::Pair(_, tail)] => Ok((**tail).clone()),
        [value] => Err(EvalError::TypeMismatch {
            name: "cdr".into(),
            expected: "pair".into(),
            got: value.type_name().into(),
        }),
        _ => Err(wrong_arg_count("cdr", "1", args.len())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut items = Vec::new();
    for value in args {
        items.extend(expect_list("append", value)?.iter().cloned());
    }
    Ok(Value::List(items))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(expect_list("length", value)?.len() as i64)),
        _ => Err(wrong_arg_count("length", "1", args.len())),
    }
}

fn builtin_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [list, index_value] => {
            let items = expect_list("list-ref", list)?;
            let index = expect_number("list-ref", index_value)?;
            if index < 0 || index as usize >= items.len() {
                return Err(EvalError::IndexOutOfBounds {
                    name: "list-ref".into(),
                    index,
                    len: items.len(),
                });
            }
            Ok(items[index as usize].clone())
        }
        _ => Err(wrong_arg_count("list-ref", "2", args.len())),
    }
}

fn builtin_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [list, index_value] => {
            let items = expect_list("list-tail", list)?;
            let index = expect_number("list-tail", index_value)?;
            if index < 0 || index as usize > items.len() {
                return Err(EvalError::IndexOutOfBounds {
                    name: "list-tail".into(),
                    index,
                    len: items.len(),
                });
            }
            Ok(Value::List(items[index as usize..].to_vec()))
        }
        _ => Err(wrong_arg_count("list-tail", "2", args.len())),
    }
}

fn builtin_assoc(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [key, list] => {
            for entry in expect_list("assoc", list)? {
                let entry_key = match entry {
                    Value::List(items) if !items.is_empty() => &items[0],
                    Value::Pair(head, _) => head,
                    _ => {
                        return Err(EvalError::TypeMismatch {
                            name: "assoc".into(),
                            expected: "pair".into(),
                            got: entry.type_name().into(),
                        });
                    }
                };

                if equal_values(key, entry_key) {
                    return Ok(entry.clone());
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(wrong_arg_count("assoc", "2", args.len())),
    }
}

fn builtin_map(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    let [callable, list_args @ ..] = args else {
        return Err(wrong_arg_count("map", "at least 2", 0));
    };

    if list_args.is_empty() {
        return Err(wrong_arg_count("map", "at least 2", 1));
    }

    let mut lists = Vec::with_capacity(list_args.len());
    for list in list_args {
        lists.push(expect_list("map", list)?);
    }

    let len = lists.iter().map(|list| list.len()).min().unwrap_or(0);
    let mut results = Vec::with_capacity(len);

    for index in 0..len {
        let mut mapped_args = Vec::with_capacity(lists.len());
        for list in &lists {
            mapped_args.push(list[index].clone());
        }
        results.push(apply(callable.clone(), &mapped_args, output)?);
    }

    Ok(Value::List(results))
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();

    for arg in args {
        result.push_str(&expect_string("string-append", arg)?.to_plain_string());
    }

    Ok(Value::String(SchemeString::fresh(result)))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(
            expect_string("string-length", value)?.len() as i64
        )),
        _ => Err(wrong_arg_count("string-length", "1", args.len())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value, start_value, end_value] => {
            let string = expect_string("substring", value)?;
            let chars = string.chars();
            let len = chars.len();
            let start = expect_number("substring", start_value)?;
            let end = expect_number("substring", end_value)?;

            if start < 0 || end < 0 || start > end || end as usize > len {
                return Err(EvalError::InvalidRange {
                    name: "substring".into(),
                    start,
                    end,
                    len,
                });
            }

            let slice: String = chars[start as usize..end as usize].iter().collect();
            Ok(Value::String(SchemeString::fresh(slice)))
        }
        _ => Err(wrong_arg_count("substring", "3", args.len())),
    }
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let string = expect_string("string->number", value)?.to_plain_string();
            match string.parse::<i64>() {
                Ok(number) => Ok(Value::Integer(number)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        _ => Err(wrong_arg_count("string->number", "1", args.len())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::String(SchemeString::fresh(
            expect_number("number->string", value)?.to_string(),
        ))),
        _ => Err(wrong_arg_count("number->string", "1", args.len())),
    }
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::String(SchemeString::fresh(
            expect_symbol("symbol->string", value)?.to_string(),
        ))),
        _ => Err(wrong_arg_count("symbol->string", "1", args.len())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Symbol(
            expect_string("string->symbol", value)?.to_plain_string(),
        )),
        _ => Err(wrong_arg_count("string->symbol", "1", args.len())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value, index_value] => {
            let string = expect_string("string-ref", value)?;
            let index = expect_number("string-ref", index_value)?;

            if index < 0 || index as usize >= string.len() {
                return Err(EvalError::IndexOutOfBounds {
                    name: "string-ref".into(),
                    index,
                    len: string.len(),
                });
            }

            Ok(Value::Char(
                string
                    .get(index as usize)
                    .expect("bounds checked before string-ref access"),
            ))
        }
        _ => Err(wrong_arg_count("string-ref", "2", args.len())),
    }
}

fn builtin_string_set(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [string_value, index_value, char_value] => {
            let string = expect_string("string-set!", string_value)?;
            if !string.is_mutable() {
                return Err(EvalError::ImmutableString {
                    name: "string-set!".into(),
                });
            }

            let index = expect_number("string-set!", index_value)?;
            let len = string.len();
            if index < 0 || index as usize >= len {
                return Err(EvalError::IndexOutOfBounds {
                    name: "string-set!".into(),
                    index,
                    len,
                });
            }

            let ch = expect_char("string-set!", char_value)?;
            let updated = string.set(index as usize, ch);
            debug_assert!(updated, "string-set! index already validated");
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("string-set!", "3", args.len())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::String(
            expect_string("string-copy", value)?.mutable_copy(),
        )),
        _ => Err(wrong_arg_count("string-copy", "1", args.len())),
    }
}

fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(eq_values(left, right))),
        _ => Err(wrong_arg_count("eq?", "2", args.len())),
    }
}

fn builtin_equal(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(equal_values(left, right))),
        _ => Err(wrong_arg_count("equal?", "2", args.len())),
    }
}

fn builtin_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(predicate(value))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_number_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(predicate(expect_number(name, value)?))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_char_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(predicate(expect_char(name, value)?))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_char_map<F>(name: &str, args: &[Value], map: F) -> Result<Value, EvalError>
where
    F: Fn(char) -> char,
{
    match args {
        [value] => Ok(Value::Char(map(expect_char(name, value)?))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_string_map<F>(name: &str, args: &[Value], map: F) -> Result<Value, EvalError>
where
    F: Fn(String) -> String,
{
    match args {
        [value] => Ok(Value::String(SchemeString::fresh(map(expect_string(
            name, value,
        )?
        .to_plain_string())))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_string_ci_equal(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(
            expect_string("string-ci=?", left)?
                .to_plain_string()
                .eq_ignore_ascii_case(&expect_string("string-ci=?", right)?.to_plain_string()),
        )),
        _ => Err(wrong_arg_count("string-ci=?", "2", args.len())),
    }
}

fn compare_numbers<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_number(name, iter.next().expect("len checked"))?;

    for arg in iter {
        let right = expect_number(name, arg)?;
        if !predicate(left, right) {
            return Ok(Value::Boolean(false));
        }
        left = right;
    }

    Ok(Value::Boolean(true))
}

fn compare_chars<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_char(name, iter.next().expect("len checked"))?;

    for arg in iter {
        let right = expect_char(name, arg)?;
        if !predicate(left, right) {
            return Ok(Value::Boolean(false));
        }
        left = right;
    }

    Ok(Value::Boolean(true))
}

fn compare_strings<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_string(name, iter.next().expect("len checked"))?.to_plain_string();

    for arg in iter {
        let right = expect_string(name, arg)?.to_plain_string();
        if !predicate(&left, &right) {
            return Ok(Value::Boolean(false));
        }
        left = right;
    }

    Ok(Value::Boolean(true))
}

fn is_pair(value: &Value) -> bool {
    matches!(value, Value::Pair(_, _)) || matches!(value, Value::List(items) if !items.is_empty())
}

fn eq_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.shares_storage(right),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::List(left), Value::List(right)) => left.is_empty() && right.is_empty(),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Integer(left), Value::Integer(right)) => left == right,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::List(left), Value::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| equal_values(left, right))
        }
        (Value::Pair(left_head, left_tail), Value::Pair(right_head, right_tail)) => {
            equal_values(left_head, right_head) && equal_values(left_tail, right_tail)
        }
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn expect_number(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "number".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expect_string(name: &str, value: &Value) -> Result<SchemeString, EvalError> {
    match value {
        Value::String(string) => Ok(string.clone()),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "string".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expect_symbol<'a>(name: &str, value: &'a Value) -> Result<&'a str, EvalError> {
    match value {
        Value::Symbol(symbol) => Ok(symbol),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "symbol".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expect_char(name: &str, value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "char".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expect_list<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "list".into(),
            got: value.type_name().into(),
        }),
    }
}
