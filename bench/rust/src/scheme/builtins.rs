use std::cmp::Ordering;
use std::collections::HashSet;
use std::rc::Rc;

use super::{
    apply,
    model::{
        is_proper_list, list_from_values, Builtin, SchemePair, SchemeString, SchemeVector, Value,
    },
    number::Number,
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
        Builtin::Gcd => builtin_gcd(args),
        Builtin::Lcm => builtin_lcm(args),
        Builtin::Min => builtin_min_max("min", args, |ordering| ordering == Ordering::Less),
        Builtin::Max => builtin_min_max("max", args, |ordering| ordering == Ordering::Greater),
        Builtin::Expt => builtin_expt(args),
        Builtin::Truncate => builtin_truncate(args),
        Builtin::Round => builtin_round(args),
        Builtin::ZeroPred => builtin_number_predicate("zero?", args, Number::is_zero),
        Builtin::PositivePred => builtin_number_predicate("positive?", args, |value| {
            matches!(value.compare(Number::exact_int(0)), Some(Ordering::Greater))
        }),
        Builtin::NegativePred => builtin_number_predicate("negative?", args, |value| {
            matches!(value.compare(Number::exact_int(0)), Some(Ordering::Less))
        }),
        Builtin::OddPred => builtin_exact_integer_predicate("odd?", args, |value| value % 2 != 0),
        Builtin::EvenPred => builtin_exact_integer_predicate("even?", args, |value| value % 2 == 0),
        Builtin::ExactPred => builtin_predicate(
            "exact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_exact()),
        ),
        Builtin::InexactPred => builtin_predicate(
            "inexact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_inexact()),
        ),
        Builtin::IntegerPred => builtin_predicate(
            "integer?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_integer()),
        ),
        Builtin::RationalPred => builtin_predicate(
            "rational?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_rational()),
        ),
        Builtin::ExactToInexact => builtin_exact_to_inexact(args),
        Builtin::InexactToExact => builtin_inexact_to_exact(args),
        Builtin::Numerator => builtin_numerator(args),
        Builtin::Denominator => builtin_denominator(args),
        Builtin::Less => compare_numbers("<", args, |ordering| ordering == Ordering::Less),
        Builtin::Greater => compare_numbers(">", args, |ordering| ordering == Ordering::Greater),
        Builtin::Equal => compare_numbers("=", args, |ordering| ordering == Ordering::Equal),
        Builtin::LessEqual => compare_numbers("<=", args, |ordering| {
            matches!(ordering, Ordering::Less | Ordering::Equal)
        }),
        Builtin::GreaterEqual => compare_numbers(">=", args, |ordering| {
            matches!(ordering, Ordering::Greater | Ordering::Equal)
        }),
        Builtin::EqPred => builtin_eq(args),
        Builtin::EqvPred => builtin_eqv(args),
        Builtin::EqualPred => builtin_equal(args),
        Builtin::Not => builtin_not(args),
        Builtin::Display => builtin_display(args, output),
        Builtin::Write => builtin_write(args, output),
        Builtin::Newline => builtin_newline(args, output),
        Builtin::Cons => builtin_cons(args),
        Builtin::Car => builtin_car(args),
        Builtin::Cdr => builtin_cdr(args),
        Builtin::Cddr => builtin_cddr(args),
        Builtin::SetCar => builtin_set_car(args),
        Builtin::SetCdr => builtin_set_cdr(args),
        Builtin::Append => builtin_append(args),
        Builtin::Reverse => builtin_reverse(args),
        Builtin::List => builtin_list(args),
        Builtin::Length => builtin_length(args),
        Builtin::ListRef => builtin_list_ref(args),
        Builtin::ListTail => builtin_list_tail(args),
        Builtin::ListPred => builtin_predicate("list?", args, is_proper_list),
        Builtin::Member => builtin_member(args),
        Builtin::Vector => builtin_vector(args),
        Builtin::MakeVector => builtin_make_vector(args),
        Builtin::VectorRef => builtin_vector_ref(args),
        Builtin::VectorSet => builtin_vector_set(args),
        Builtin::VectorLength => builtin_vector_length(args),
        Builtin::VectorPred => {
            builtin_predicate("vector?", args, |value| matches!(value, Value::Vector(_)))
        }
        Builtin::VectorToList => builtin_vector_to_list(args),
        Builtin::ListToVector => builtin_list_to_vector(args),
        Builtin::Assoc => builtin_assoc(args),
        Builtin::Assv => builtin_assv(args),
        Builtin::Map => builtin_map(args, output),
        Builtin::ForEach => builtin_for_each(args, output),
        Builtin::MakeString => builtin_make_string(args),
        Builtin::String => builtin_string(args),
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
        Builtin::StringToList => builtin_string_to_list(args),
        Builtin::ListToString => builtin_list_to_string(args),
        Builtin::NullPred => {
            builtin_predicate("null?", args, |value| matches!(value, Value::EmptyList))
        }
        Builtin::NumberPred => {
            builtin_predicate("number?", args, |value| matches!(value, Value::Number(_)))
        }
        Builtin::StringPred => {
            builtin_predicate("string?", args, |value| matches!(value, Value::String(_)))
        }
        Builtin::BooleanPred => {
            builtin_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        Builtin::ProcedurePred => builtin_predicate("procedure?", args, |value| {
            matches!(
                value,
                Value::Builtin(_)
                    | Value::Procedure(_)
                    | Value::Continuation(_)
                    | Value::RecordProcedure(_)
            )
        }),
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
        Builtin::CharToInteger => builtin_char_to_integer(args),
        Builtin::IntegerToChar => builtin_integer_to_char(args),
        Builtin::CharUpcase => builtin_char_map("char-upcase", args, |ch| ch.to_ascii_uppercase()),
        Builtin::CharDowncase => {
            builtin_char_map("char-downcase", args, |ch| ch.to_ascii_lowercase())
        }
        Builtin::CharEqual => compare_chars("char=?", args, |left, right| left == right),
        Builtin::CharLess => compare_chars("char<?", args, |left, right| left < right),
        Builtin::StringEqual => compare_strings("string=?", args, |left, right| left == right),
        Builtin::StringLess => compare_strings("string<?", args, |left, right| left < right),
        Builtin::StringGreater => compare_strings("string>?", args, |left, right| left > right),
        Builtin::StringLessEqual => compare_strings("string<=?", args, |left, right| left <= right),
        Builtin::StringGreaterEqual => {
            compare_strings("string>=?", args, |left, right| left >= right)
        }
        Builtin::StringCiEqual => builtin_string_ci_equal(args),
        Builtin::StringUpcase => {
            builtin_string_map("string-upcase", args, |value| value.to_ascii_uppercase())
        }
        Builtin::StringDowncase => {
            builtin_string_map("string-downcase", args, |value| value.to_ascii_lowercase())
        }
        Builtin::Raise => Err(EvalError::Syntax {
            message: "raise requires continuation-aware evaluation".into(),
        }),
        Builtin::WithExceptionHandler => Err(EvalError::Syntax {
            message: "with-exception-handler requires continuation-aware evaluation".into(),
        }),
        Builtin::DynamicWind => Err(EvalError::Syntax {
            message: "dynamic-wind requires continuation-aware evaluation".into(),
        }),
        Builtin::Values => Ok(Value::from_values(args.to_vec())),
        Builtin::CallWithValues => builtin_call_with_values(args, output),
        Builtin::Apply => builtin_apply(args, output),
        Builtin::CallCc => Err(EvalError::Syntax {
            message: "call/cc requires continuation-aware evaluation".into(),
        }),
    }
}

fn builtin_call_with_values(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    let [producer, consumer] = args else {
        return Err(wrong_arg_count("call-with-values", "2", args.len()));
    };

    let produced = apply(producer.clone(), &[], output)?;
    let consumer_args = produced.into_values();
    apply(consumer.clone(), &consumer_args, output)
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
    let list_items = collect_list("apply", list_arg)?;

    let mut applied_args = Vec::with_capacity(prefix_args.len() + list_items.len());
    applied_args.extend(prefix_args.iter().cloned());
    applied_args.extend(list_items);
    apply(callable.clone(), &applied_args, output)
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = Number::exact_int(0);
    for arg in args {
        total = total.add(expect_number("+", arg)?, "+")?;
    }
    Ok(Value::Number(total))
}

fn builtin_abs(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(expect_number("abs", value)?.abs("abs")?)),
        _ => Err(wrong_arg_count("abs", "1", args.len())),
    }
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(wrong_arg_count("-", "at least 1", 0)),
        [arg] => Ok(Value::Number(expect_number("-", arg)?.negate("-")?)),
        [first, rest @ ..] => {
            let mut total = expect_number("-", first)?;
            for arg in rest {
                total = total.sub(expect_number("-", arg)?, "-")?;
            }
            Ok(Value::Number(total))
        }
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = Number::exact_int(1);
    for arg in args {
        total = total.mul(expect_number("*", arg)?, "*")?;
    }
    Ok(Value::Number(total))
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
        if divisor.is_zero() {
            return Err(EvalError::DivisionByZero);
        }
        total = total.div(divisor, "/")?;
    }

    Ok(Value::Number(total))
}

fn builtin_modulo(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_exact_integer("modulo", dividend)?;
            let divisor = expect_exact_integer("modulo", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }

            let mut remainder = dividend % divisor;
            if remainder != 0 && (remainder > 0) != (divisor > 0) {
                remainder += divisor;
            }
            Ok(Value::Number(Number::exact_int(remainder)))
        }
        _ => Err(wrong_arg_count("modulo", "2", args.len())),
    }
}

fn builtin_remainder(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_exact_integer("remainder", dividend)?;
            let divisor = expect_exact_integer("remainder", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Number(Number::exact_int(dividend % divisor)))
        }
        _ => Err(wrong_arg_count("remainder", "2", args.len())),
    }
}

fn builtin_quotient(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [dividend, divisor] => {
            let dividend = expect_exact_integer("quotient", dividend)?;
            let divisor = expect_exact_integer("quotient", divisor)?;
            if divisor == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Number(Number::exact_int(dividend / divisor)))
        }
        _ => Err(wrong_arg_count("quotient", "2", args.len())),
    }
}

fn builtin_gcd(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 0i64;
    for arg in args {
        result = gcd_i64(result, expect_exact_integer("gcd", arg)?);
    }
    Ok(Value::Number(Number::exact_int(result.abs())))
}

fn builtin_lcm(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 1i64;
    if args.is_empty() {
        return Ok(Value::Number(Number::exact_int(1)));
    }

    for arg in args {
        result = lcm_i64(result, expect_exact_integer("lcm", arg)?);
    }
    Ok(Value::Number(Number::exact_int(result.abs())))
}

fn builtin_min_max<F>(name: &str, args: &[Value], choose: F) -> Result<Value, EvalError>
where
    F: Fn(Ordering) -> bool,
{
    let [first, rest @ ..] = args else {
        return Err(wrong_arg_count(name, "at least 1", 0));
    };

    let mut result = expect_number(name, first)?;
    for arg in rest {
        let candidate = expect_number(name, arg)?;
        if matches!(candidate.compare(result), Some(ordering) if choose(ordering)) {
            result = candidate;
        }
    }
    Ok(Value::Number(result))
}

fn builtin_expt(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [base, exponent] => {
            let base = expect_exact_integer("expt", base)?;
            let exponent = expect_exact_integer("expt", exponent)?;
            let exponent = exponent.max(0) as u32;
            let result = base
                .checked_pow(exponent)
                .ok_or_else(|| EvalError::NumericOverflow {
                    name: "expt".into(),
                })?;
            Ok(Value::Number(Number::exact_int(result)))
        }
        _ => Err(wrong_arg_count("expt", "2", args.len())),
    }
}

fn builtin_truncate(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(truncate_number(expect_number(
            "truncate", value,
        )?))),
        _ => Err(wrong_arg_count("truncate", "1", args.len())),
    }
}

fn builtin_round(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(round_number(expect_number("round", value)?))),
        _ => Err(wrong_arg_count("round", "1", args.len())),
    }
}

fn builtin_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(
            expect_number("exact->inexact", value)?.to_inexact(),
        )),
        _ => Err(wrong_arg_count("exact->inexact", "1", args.len())),
    }
}

fn builtin_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(
            expect_number("inexact->exact", value)?.to_exact("inexact->exact")?,
        )),
        _ => Err(wrong_arg_count("inexact->exact", "1", args.len())),
    }
}

fn builtin_numerator(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(Number::exact_int(
            expect_exact_number("numerator", value)?
                .numerator()
                .expect("exact numbers always have numerators"),
        ))),
        _ => Err(wrong_arg_count("numerator", "1", args.len())),
    }
}

fn builtin_denominator(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(Number::exact_int(
            expect_exact_number("denominator", value)?
                .denominator()
                .expect("exact numbers always have denominators"),
        ))),
        _ => Err(wrong_arg_count("denominator", "1", args.len())),
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
        [head, tail] => Ok(Value::Pair(SchemePair::new(head.clone(), tail.clone()))),
        _ => Err(wrong_arg_count("cons", "2", args.len())),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(expect_pair("car", value)?.car()),
        _ => Err(wrong_arg_count("car", "1", args.len())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(expect_pair("cdr", value)?.cdr()),
        _ => Err(wrong_arg_count("cdr", "1", args.len())),
    }
}

fn builtin_cddr(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let tail = expect_pair("cddr", value)?.cdr();
            Ok(expect_pair("cddr", &tail)?.cdr())
        }
        _ => Err(wrong_arg_count("cddr", "1", args.len())),
    }
}

fn builtin_set_car(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [pair_value, value] => {
            expect_pair("set-car!", pair_value)?.set_car(value.clone());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("set-car!", "2", args.len())),
    }
}

fn builtin_set_cdr(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [pair_value, value] => {
            expect_pair("set-cdr!", pair_value)?.set_cdr(value.clone());
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("set-cdr!", "2", args.len())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut items = Vec::new();
    for value in args {
        items.extend(collect_list("append", value)?);
    }
    Ok(list_from_values(items))
}

fn builtin_reverse(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let mut items = collect_list("reverse", value)?;
            items.reverse();
            Ok(list_from_values(items))
        }
        _ => Err(wrong_arg_count("reverse", "1", args.len())),
    }
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_values(args.iter().cloned()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(Number::exact_int(
            collect_list("length", value)?.len() as i64,
        ))),
        _ => Err(wrong_arg_count("length", "1", args.len())),
    }
}

fn builtin_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [list, index_value] => {
            let index = expect_exact_integer("list-ref", index_value)?;
            if index < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "list-ref".into(),
                    index,
                    len: 0,
                });
            }
            list_ref_value("list-ref", list, index as usize)
        }
        _ => Err(wrong_arg_count("list-ref", "2", args.len())),
    }
}

fn builtin_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [list, index_value] => {
            let index = expect_exact_integer("list-tail", index_value)?;
            if index < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "list-tail".into(),
                    index,
                    len: 0,
                });
            }
            list_tail_value("list-tail", list, index as usize)
        }
        _ => Err(wrong_arg_count("list-tail", "2", args.len())),
    }
}

fn builtin_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(SchemeVector::new(args.to_vec())))
}

fn builtin_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [len_value] => {
            let len = expect_exact_integer("make-vector", len_value)?;
            if len < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "make-vector".into(),
                    index: len,
                    len: 0,
                });
            }
            Ok(Value::Vector(SchemeVector::new(vec![
                Value::Boolean(false);
                len as usize
            ])))
        }
        [len_value, fill] => {
            let len = expect_exact_integer("make-vector", len_value)?;
            if len < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "make-vector".into(),
                    index: len,
                    len: 0,
                });
            }
            Ok(Value::Vector(SchemeVector::new(vec![
                fill.clone();
                len as usize
            ])))
        }
        _ => Err(wrong_arg_count("make-vector", "1 or 2", args.len())),
    }
}

fn builtin_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [vector_value, index_value] => {
            let vector = expect_vector("vector-ref", vector_value)?;
            let index = expect_exact_integer("vector-ref", index_value)?;
            if index < 0 || index as usize >= vector.len() {
                return Err(EvalError::IndexOutOfBounds {
                    name: "vector-ref".into(),
                    index,
                    len: vector.len(),
                });
            }
            Ok(vector
                .get(index as usize)
                .expect("vector-ref index validated before access"))
        }
        _ => Err(wrong_arg_count("vector-ref", "2", args.len())),
    }
}

fn builtin_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [vector_value, index_value, value] => {
            let vector = expect_vector("vector-set!", vector_value)?;
            let index = expect_exact_integer("vector-set!", index_value)?;
            let len = vector.len();
            if index < 0 || index as usize >= len {
                return Err(EvalError::IndexOutOfBounds {
                    name: "vector-set!".into(),
                    index,
                    len,
                });
            }
            let updated = vector.set(index as usize, value.clone());
            debug_assert!(updated, "vector-set! index already validated");
            Ok(Value::Void)
        }
        _ => Err(wrong_arg_count("vector-set!", "3", args.len())),
    }
}

fn builtin_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [vector_value] => Ok(Value::Number(Number::exact_int(
            expect_vector("vector-length", vector_value)?.len() as i64,
        ))),
        _ => Err(wrong_arg_count("vector-length", "1", args.len())),
    }
}

fn builtin_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [vector_value] => Ok(list_from_values(
            expect_vector("vector->list", vector_value)?.to_vec(),
        )),
        _ => Err(wrong_arg_count("vector->list", "1", args.len())),
    }
}

fn builtin_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [list_value] => Ok(Value::Vector(SchemeVector::new(collect_list(
            "list->vector",
            list_value,
        )?))),
        _ => Err(wrong_arg_count("list->vector", "1", args.len())),
    }
}

fn builtin_assoc(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [key, list] => {
            for entry in collect_list("assoc", list)? {
                let entry_key = match &entry {
                    Value::Pair(pair) => pair.car(),
                    _ => {
                        return Err(EvalError::TypeMismatch {
                            name: "assoc".into(),
                            expected: "pair".into(),
                            got: entry.type_name().into(),
                        });
                    }
                };

                if equal_values(key, &entry_key) {
                    return Ok(entry);
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(wrong_arg_count("assoc", "2", args.len())),
    }
}

fn builtin_assv(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [key, list] => {
            for entry in collect_list("assv", list)? {
                let entry_key = match &entry {
                    Value::Pair(pair) => pair.car(),
                    _ => {
                        return Err(EvalError::TypeMismatch {
                            name: "assv".into(),
                            expected: "pair".into(),
                            got: entry.type_name().into(),
                        });
                    }
                };

                if eqv_values(key, &entry_key) {
                    return Ok(entry);
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(wrong_arg_count("assv", "2", args.len())),
    }
}

fn builtin_member(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [key, list] => {
            let mut current = list.clone();
            let mut seen = HashSet::new();

            loop {
                match current {
                    Value::EmptyList => return Ok(Value::Boolean(false)),
                    Value::Pair(pair) => {
                        if !seen.insert(pair.id()) {
                            return Err(EvalError::CircularList {
                                name: "member".into(),
                            });
                        }

                        if equal_values(key, &pair.car()) {
                            return Ok(Value::Pair(pair));
                        }
                        current = pair.cdr();
                    }
                    other => {
                        return Err(EvalError::TypeMismatch {
                            name: "member".into(),
                            expected: "list".into(),
                            got: other.type_name().into(),
                        });
                    }
                }
            }
        }
        _ => Err(wrong_arg_count("member", "2", args.len())),
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
        lists.push(collect_list("map", list)?);
    }

    let len = lists.iter().map(Vec::len).min().unwrap_or(0);
    let mut results = Vec::with_capacity(len);

    for index in 0..len {
        let mut mapped_args = Vec::with_capacity(lists.len());
        for list in &lists {
            mapped_args.push(list[index].clone());
        }
        results.push(apply(callable.clone(), &mapped_args, output)?);
    }

    Ok(list_from_values(results))
}

fn builtin_for_each(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    let [callable, list_args @ ..] = args else {
        return Err(wrong_arg_count("for-each", "at least 2", 0));
    };

    if list_args.is_empty() {
        return Err(wrong_arg_count("for-each", "at least 2", 1));
    }

    let mut lists = Vec::with_capacity(list_args.len());
    for list in list_args {
        lists.push(collect_list("for-each", list)?);
    }

    let len = lists.iter().map(Vec::len).min().unwrap_or(0);
    for index in 0..len {
        let mut call_args = Vec::with_capacity(lists.len());
        for list in &lists {
            call_args.push(list[index].clone());
        }
        apply(callable.clone(), &call_args, output)?;
    }

    Ok(Value::Void)
}

fn builtin_make_string(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [len_value] => {
            let len = expect_exact_integer("make-string", len_value)?;
            if len < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "make-string".into(),
                    index: len,
                    len: 0,
                });
            }
            Ok(Value::String(SchemeString::fresh(
                "\0".repeat(len as usize),
            )))
        }
        [len_value, fill_value] => {
            let len = expect_exact_integer("make-string", len_value)?;
            if len < 0 {
                return Err(EvalError::IndexOutOfBounds {
                    name: "make-string".into(),
                    index: len,
                    len: 0,
                });
            }
            let fill = expect_char("make-string", fill_value)?;
            Ok(Value::String(SchemeString::fresh(
                std::iter::repeat_n(fill, len as usize).collect(),
            )))
        }
        _ => Err(wrong_arg_count("make-string", "1 or 2", args.len())),
    }
}

fn builtin_string(args: &[Value]) -> Result<Value, EvalError> {
    let chars = args
        .iter()
        .map(|value| expect_char("string", value))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::String(SchemeString::fresh(
        chars.into_iter().collect::<String>(),
    )))
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
        [value] => Ok(Value::Number(Number::exact_int(
            expect_string("string-length", value)?.len() as i64,
        ))),
        _ => Err(wrong_arg_count("string-length", "1", args.len())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value, start_value, end_value] => {
            let string = expect_string("substring", value)?;
            let chars = string.chars();
            let len = chars.len();
            let start = expect_exact_integer("substring", start_value)?;
            let end = expect_exact_integer("substring", end_value)?;

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
            match Number::parse(&string) {
                Some(number) => Ok(Value::Number(number)),
                None => Ok(Value::Boolean(false)),
            }
        }
        _ => Err(wrong_arg_count("string->number", "1", args.len())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::String(SchemeString::fresh(
            expect_number("number->string", value)?.render(),
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
            let index = expect_exact_integer("string-ref", index_value)?;

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

            let index = expect_exact_integer("string-set!", index_value)?;
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

fn builtin_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(list_from_values(
            expect_string("string->list", value)?
                .chars()
                .into_iter()
                .map(Value::Char),
        )),
        _ => Err(wrong_arg_count("string->list", "1", args.len())),
    }
}

fn builtin_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let chars = collect_list("list->string", value)?
                .into_iter()
                .map(|item| expect_char("list->string", &item))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(SchemeString::fresh(
                chars.into_iter().collect::<String>(),
            )))
        }
        _ => Err(wrong_arg_count("list->string", "1", args.len())),
    }
}

fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(eq_values(left, right))),
        _ => Err(wrong_arg_count("eq?", "2", args.len())),
    }
}

fn builtin_eqv(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [left, right] => Ok(Value::Boolean(eqv_values(left, right))),
        _ => Err(wrong_arg_count("eqv?", "2", args.len())),
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
    F: Fn(Number) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(predicate(expect_number(name, value)?))),
        _ => Err(wrong_arg_count(name, "1", args.len())),
    }
}

fn builtin_exact_integer_predicate<F>(
    name: &str,
    args: &[Value],
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(i64) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(predicate(expect_exact_integer(
            name, value,
        )?))),
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

fn builtin_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Number(Number::exact_int(i64::from(u32::from(
            expect_char("char->integer", value)?,
        ))))),
        _ => Err(wrong_arg_count("char->integer", "1", args.len())),
    }
}

fn builtin_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => {
            let code = expect_exact_integer("integer->char", value)?;
            let ch = u32::try_from(code)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| EvalError::InvalidCharCode {
                    name: "integer->char".into(),
                    code,
                })?;
            Ok(Value::Char(ch))
        }
        _ => Err(wrong_arg_count("integer->char", "1", args.len())),
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
    F: Fn(Ordering) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_number(name, iter.next().expect("len checked"))?;

    for arg in iter {
        let right = expect_number(name, arg)?;
        let Some(ordering) = left.compare(right) else {
            return Ok(Value::Boolean(false));
        };
        if !predicate(ordering) {
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
    matches!(value, Value::Pair(_))
}

fn eq_values(left: &Value, right: &Value) -> bool {
    eqv_values(left, right)
}

pub(super) fn eqv_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => {
            left.compare(*right) == Some(Ordering::Equal)
        }
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left.shares_storage(right),
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => left.shares_storage(right),
        (Value::Vector(left), Value::Vector(right)) => left.shares_storage(right),
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::MultipleValues(left), Value::MultipleValues(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| eqv_values(left, right))
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::RecordProcedure(left), Value::RecordProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn equal_values(left: &Value, right: &Value) -> bool {
    let mut seen_pairs = HashSet::new();
    let mut seen_vectors = HashSet::new();
    equal_values_inner(left, right, &mut seen_pairs, &mut seen_vectors)
}

fn equal_values_inner(
    left: &Value,
    right: &Value,
    seen_pairs: &mut HashSet<(usize, usize)>,
    seen_vectors: &mut HashSet<(usize, usize)>,
) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => {
            left.compare(*right) == Some(Ordering::Equal)
        }
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            left.to_plain_string() == right.to_plain_string()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::EmptyList, Value::EmptyList) => true,
        (Value::Pair(left), Value::Pair(right)) => {
            let key = (left.id(), right.id());
            if !seen_pairs.insert(key) {
                return true;
            }
            equal_values_inner(&left.car(), &right.car(), seen_pairs, seen_vectors)
                && equal_values_inner(&left.cdr(), &right.cdr(), seen_pairs, seen_vectors)
        }
        (Value::Vector(left), Value::Vector(right)) => {
            let key = (left.id(), right.id());
            if !seen_vectors.insert(key) {
                return true;
            }

            let left = left.to_vec();
            let right = right.to_vec();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_values_inner(left, right, seen_pairs, seen_vectors))
        }
        (Value::Builtin(left), Value::Builtin(right)) => left == right,
        (Value::Procedure(left), Value::Procedure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::MultipleValues(left), Value::MultipleValues(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| equal_values_inner(left, right, seen_pairs, seen_vectors))
        }
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::RecordProcedure(left), Value::RecordProcedure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn truncate_number(number: Number) -> Number {
    match number {
        Number::ExactInt(value) => Number::ExactInt(value),
        Number::ExactRational { numer, denom } => Number::ExactInt(numer / denom),
        Number::Inexact(value) => Number::Inexact(value.trunc()),
    }
}

fn round_number(number: Number) -> Number {
    match number {
        Number::ExactInt(value) => Number::ExactInt(value),
        Number::ExactRational { numer, denom } => {
            Number::ExactInt((numer as f64 / denom as f64).round() as i64)
        }
        Number::Inexact(value) => Number::Inexact(value.round()),
    }
}

fn gcd_i64(left: i64, right: i64) -> i64 {
    let mut left = left.abs();
    let mut right = right.abs();
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left
}

fn lcm_i64(left: i64, right: i64) -> i64 {
    if left == 0 || right == 0 {
        0
    } else {
        (left / gcd_i64(left, right)) * right
    }
}

fn expect_number(name: &str, value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "number".into(),
            got: value.type_name().into(),
        }),
    }
}

fn expect_exact_number(name: &str, value: &Value) -> Result<Number, EvalError> {
    let number = expect_number(name, value)?;
    if number.is_exact() {
        Ok(number)
    } else {
        Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "exact number".into(),
            got: value.type_name().into(),
        })
    }
}

fn expect_exact_integer(name: &str, value: &Value) -> Result<i64, EvalError> {
    let number = expect_number(name, value)?;
    match number.exact_integer() {
        Some(integer) => Ok(integer),
        None => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "exact integer".into(),
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

fn expect_pair(name: &str, value: &Value) -> Result<SchemePair, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "pair".into(),
            got: value.type_name().into(),
        }),
    }
}

fn collect_list(name: &str, value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen = HashSet::new();

    loop {
        match current {
            Value::EmptyList => return Ok(items),
            Value::Pair(pair) => {
                if !seen.insert(pair.id()) {
                    return Err(EvalError::CircularList { name: name.into() });
                }
                items.push(pair.car());
                current = pair.cdr();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    name: name.into(),
                    expected: "list".into(),
                    got: other.type_name().into(),
                })
            }
        }
    }
}

fn list_ref_value(name: &str, value: &Value, index: usize) -> Result<Value, EvalError> {
    let mut current = value.clone();
    let mut seen = HashSet::new();
    let mut len = 0usize;

    loop {
        match current {
            Value::EmptyList => {
                return Err(EvalError::IndexOutOfBounds {
                    name: name.into(),
                    index: index as i64,
                    len,
                })
            }
            Value::Pair(pair) => {
                if !seen.insert(pair.id()) {
                    return Err(EvalError::CircularList { name: name.into() });
                }
                if len == index {
                    return Ok(pair.car());
                }
                len += 1;
                current = pair.cdr();
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    name: name.into(),
                    expected: "list".into(),
                    got: other.type_name().into(),
                })
            }
        }
    }
}

fn list_tail_value(name: &str, value: &Value, index: usize) -> Result<Value, EvalError> {
    let mut current = value.clone();
    let mut seen = HashSet::new();
    let mut offset = 0usize;

    loop {
        if offset == index {
            return match current {
                Value::EmptyList | Value::Pair(_) => Ok(current),
                other => Err(EvalError::TypeMismatch {
                    name: name.into(),
                    expected: "list".into(),
                    got: other.type_name().into(),
                }),
            };
        }

        match current {
            Value::EmptyList => {
                return Err(EvalError::IndexOutOfBounds {
                    name: name.into(),
                    index: index as i64,
                    len: offset,
                })
            }
            Value::Pair(pair) => {
                if !seen.insert(pair.id()) {
                    return Err(EvalError::CircularList { name: name.into() });
                }
                current = pair.cdr();
                offset += 1;
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    name: name.into(),
                    expected: "list".into(),
                    got: other.type_name().into(),
                })
            }
        }
    }
}

fn expect_vector(name: &str, value: &Value) -> Result<SchemeVector, EvalError> {
    match value {
        Value::Vector(vector) => Ok(vector.clone()),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "vector".into(),
            got: value.type_name().into(),
        }),
    }
}
