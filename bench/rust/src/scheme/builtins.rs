use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

use super::continuation::{
    current_continuation_handle, expire_continuation_handle, Continuation, ContinuationRef,
    EvalResult, EvalSignal, RaisedException,
};
use super::macros::{datum_from_syntax_value, datum_to_syntax_value};
use super::number::{parse_number_string, Number};
use super::value_ops::{
    collect_list_items, is_empty_list, is_proper_list, list_from_vec, pair_parts, values_eq,
    values_equal, values_eqv,
};
use super::{
    env_define, Builtin, BuiltinKind, EnvRef, Environment, EvalError, OutputRef, PairCell, Value,
};

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
        BuiltinKind::GreaterThanOrEqual,
        BuiltinKind::Not,
        BuiltinKind::Cons,
        BuiltinKind::Car,
        BuiltinKind::Cdr,
        BuiltinKind::SetCar,
        BuiltinKind::SetCdr,
        BuiltinKind::NullPred,
        BuiltinKind::List,
        BuiltinKind::Length,
        BuiltinKind::Append,
        BuiltinKind::Reverse,
        BuiltinKind::Apply,
        BuiltinKind::Values,
        BuiltinKind::CallWithValues,
        BuiltinKind::CallCc,
        BuiltinKind::Raise,
        BuiltinKind::WithExceptionHandler,
        BuiltinKind::EqPred,
        BuiltinKind::EqvPred,
        BuiltinKind::EqualPred,
        BuiltinKind::Map,
        BuiltinKind::ForEach,
        BuiltinKind::StringPred,
        BuiltinKind::NumberPred,
        BuiltinKind::IntegerPred,
        BuiltinKind::RationalPred,
        BuiltinKind::ExactPred,
        BuiltinKind::InexactPred,
        BuiltinKind::BooleanPred,
        BuiltinKind::PairPred,
        BuiltinKind::SymbolPred,
        BuiltinKind::ProcedurePred,
        BuiltinKind::Display,
        BuiltinKind::Write,
        BuiltinKind::Newline,
        BuiltinKind::MakeString,
        BuiltinKind::String,
        BuiltinKind::StringAppend,
        BuiltinKind::StringLength,
        BuiltinKind::Substring,
        BuiltinKind::StringToNumber,
        BuiltinKind::NumberToString,
        BuiltinKind::ExactToInexact,
        BuiltinKind::InexactToExact,
        BuiltinKind::Numerator,
        BuiltinKind::Denominator,
        BuiltinKind::SymbolToString,
        BuiltinKind::StringToSymbol,
        BuiltinKind::StringRef,
        BuiltinKind::StringCopy,
        BuiltinKind::StringSet,
        BuiltinKind::StringToList,
        BuiltinKind::ListToString,
        BuiltinKind::CharPred,
        BuiltinKind::CharToInteger,
        BuiltinKind::IntegerToChar,
        BuiltinKind::Abs,
        BuiltinKind::Modulo,
        BuiltinKind::Remainder,
        BuiltinKind::Quotient,
        BuiltinKind::Gcd,
        BuiltinKind::Lcm,
        BuiltinKind::Min,
        BuiltinKind::Max,
        BuiltinKind::Expt,
        BuiltinKind::Truncate,
        BuiltinKind::Round,
        BuiltinKind::ZeroPred,
        BuiltinKind::PositivePred,
        BuiltinKind::NegativePred,
        BuiltinKind::OddPred,
        BuiltinKind::EvenPred,
        BuiltinKind::ListRef,
        BuiltinKind::ListTail,
        BuiltinKind::ListPred,
        BuiltinKind::Member,
        BuiltinKind::Assoc,
        BuiltinKind::Assv,
        BuiltinKind::CharAlphabeticPred,
        BuiltinKind::CharNumericPred,
        BuiltinKind::CharUpcase,
        BuiltinKind::CharDowncase,
        BuiltinKind::CharEqual,
        BuiltinKind::CharLessThan,
        BuiltinKind::StringEqual,
        BuiltinKind::StringLessThan,
        BuiltinKind::StringGreaterThan,
        BuiltinKind::StringLessThanOrEqual,
        BuiltinKind::StringGreaterThanOrEqual,
        BuiltinKind::StringCiEqual,
        BuiltinKind::StringUpcase,
        BuiltinKind::StringDowncase,
        BuiltinKind::Vector,
        BuiltinKind::MakeVector,
        BuiltinKind::VectorRef,
        BuiltinKind::VectorSet,
        BuiltinKind::VectorLength,
        BuiltinKind::VectorPred,
        BuiltinKind::VectorToList,
        BuiltinKind::ListToVector,
        BuiltinKind::SyntaxToDatum,
        BuiltinKind::DatumToSyntax,
    ] {
        env_define(
            &env,
            builtin.name().into(),
            Value::Builtin(Builtin::new(builtin, Rc::clone(&output))),
        );
    }

    env_define(
        &env,
        "call-with-current-continuation".into(),
        Value::Builtin(Builtin::new(BuiltinKind::CallCc, Rc::clone(&output))),
    );

    for accessor in [
        "caar", "cadr", "cdar", "cddr", "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr",
        "cddar", "cdddr", "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar",
        "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar", "cddddr",
    ] {
        env_define(
            &env,
            accessor.into(),
            Value::Builtin(Builtin::new(
                BuiltinKind::ComposedCarCdr(accessor),
                Rc::clone(&output),
            )),
        );
    }

    env
}

pub(super) fn apply_builtin(
    kind: BuiltinKind,
    args: &[Value],
    output: &OutputRef,
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    match kind {
        BuiltinKind::Apply => eval_apply(args, continuation),
        BuiltinKind::CallWithValues => eval_call_with_values(args, continuation),
        BuiltinKind::CallCc => eval_call_cc(args, continuation),
        BuiltinKind::Raise => eval_raise(args),
        BuiltinKind::WithExceptionHandler => eval_with_exception_handler(args, continuation),
        BuiltinKind::Map => eval_map(args, continuation),
        BuiltinKind::ForEach => eval_for_each(args, continuation),
        _ => apply_builtin_without_context(kind, args, output).map_err(Into::into),
    }
}

fn apply_builtin_without_context(
    kind: BuiltinKind,
    args: &[Value],
    output: &OutputRef,
) -> Result<Value, EvalError> {
    match kind {
        BuiltinKind::Add => eval_add(args),
        BuiltinKind::Sub => eval_sub(args),
        BuiltinKind::Mul => eval_mul(args),
        BuiltinKind::Div => eval_div(args),
        BuiltinKind::LessThan => {
            eval_compare(kind.name(), args, |ordering| ordering == Ordering::Less)
        }
        BuiltinKind::GreaterThan => {
            eval_compare(kind.name(), args, |ordering| ordering == Ordering::Greater)
        }
        BuiltinKind::Equal => {
            eval_compare(kind.name(), args, |ordering| ordering == Ordering::Equal)
        }
        BuiltinKind::LessThanOrEqual => {
            eval_compare(kind.name(), args, |ordering| ordering != Ordering::Greater)
        }
        BuiltinKind::GreaterThanOrEqual => {
            eval_compare(kind.name(), args, |ordering| ordering != Ordering::Less)
        }
        BuiltinKind::Not => eval_not(args),
        BuiltinKind::Cons => eval_cons(args),
        BuiltinKind::Car => eval_car(args),
        BuiltinKind::Cdr => eval_cdr(args),
        BuiltinKind::ComposedCarCdr(name) => eval_composed_car_cdr(name, args),
        BuiltinKind::SetCar => eval_set_car(args),
        BuiltinKind::SetCdr => eval_set_cdr(args),
        BuiltinKind::NullPred => eval_null(args),
        BuiltinKind::List => Ok(list_from_vec(args.to_vec())),
        BuiltinKind::Length => eval_length(args),
        BuiltinKind::Append => eval_append(args),
        BuiltinKind::Reverse => eval_reverse(args),
        BuiltinKind::Apply => unreachable!("apply requires the current continuation"),
        BuiltinKind::Values => eval_values(args),
        BuiltinKind::CallWithValues => {
            unreachable!("call-with-values requires the current continuation")
        }
        BuiltinKind::CallCc => unreachable!("call/cc requires the current continuation"),
        BuiltinKind::Raise => unreachable!("raise requires the current continuation"),
        BuiltinKind::WithExceptionHandler => {
            unreachable!("with-exception-handler requires the current continuation")
        }
        BuiltinKind::EqPred => eval_eq_like("eq?", args, values_eq),
        BuiltinKind::EqvPred => eval_eq_like("eqv?", args, values_eqv),
        BuiltinKind::EqualPred => eval_equality("equal?", args),
        BuiltinKind::Map => unreachable!("map requires the current continuation"),
        BuiltinKind::ForEach => unreachable!("for-each requires the current continuation"),
        BuiltinKind::StringPred => eval_predicate("string?", args, |value| {
            matches!(value, Value::String(_) | Value::MutableString(_))
        }),
        BuiltinKind::NumberPred => {
            eval_predicate("number?", args, |value| matches!(value, Value::Number(_)))
        }
        BuiltinKind::IntegerPred => eval_predicate(
            "integer?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_integer()),
        ),
        BuiltinKind::RationalPred => eval_predicate(
            "rational?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_rational()),
        ),
        BuiltinKind::ExactPred => eval_predicate(
            "exact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_exact()),
        ),
        BuiltinKind::InexactPred => eval_predicate(
            "inexact?",
            args,
            |value| matches!(value, Value::Number(number) if number.is_inexact()),
        ),
        BuiltinKind::BooleanPred => {
            eval_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
        }
        BuiltinKind::PairPred => eval_pair_pred(args),
        BuiltinKind::SymbolPred => {
            eval_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
        }
        BuiltinKind::ProcedurePred => eval_predicate("procedure?", args, |value| {
            matches!(
                value,
                Value::Procedure(_)
                    | Value::NativeProcedure(_)
                    | Value::Builtin(_)
                    | Value::Continuation(_)
                    | Value::ContinuationHandle(_)
                    | Value::ExpiredContinuation
            )
        }),
        BuiltinKind::Display => eval_display(args, output),
        BuiltinKind::Write => eval_write(args, output),
        BuiltinKind::Newline => eval_newline(args, output),
        BuiltinKind::MakeString => eval_make_string(args),
        BuiltinKind::String => eval_string(args),
        BuiltinKind::StringAppend => eval_string_append(args),
        BuiltinKind::StringLength => eval_string_length(args),
        BuiltinKind::Substring => eval_substring(args),
        BuiltinKind::StringToNumber => eval_string_to_number(args),
        BuiltinKind::NumberToString => eval_number_to_string(args),
        BuiltinKind::ExactToInexact => eval_exact_to_inexact(args),
        BuiltinKind::InexactToExact => eval_inexact_to_exact(args),
        BuiltinKind::Numerator => eval_numerator(args),
        BuiltinKind::Denominator => eval_denominator(args),
        BuiltinKind::SymbolToString => eval_symbol_to_string(args),
        BuiltinKind::StringToSymbol => eval_string_to_symbol(args),
        BuiltinKind::StringRef => eval_string_ref(args),
        BuiltinKind::StringCopy => eval_string_copy(args),
        BuiltinKind::StringSet => eval_string_set(args),
        BuiltinKind::StringToList => eval_string_to_list(args),
        BuiltinKind::ListToString => eval_list_to_string(args),
        BuiltinKind::CharPred => {
            eval_predicate("char?", args, |value| matches!(value, Value::Char(_)))
        }
        BuiltinKind::CharToInteger => eval_char_to_integer(args),
        BuiltinKind::IntegerToChar => eval_integer_to_char(args),
        BuiltinKind::Abs => eval_abs(args),
        BuiltinKind::Modulo => eval_modulo(args),
        BuiltinKind::Remainder => eval_remainder(args),
        BuiltinKind::Quotient => eval_quotient(args),
        BuiltinKind::Gcd => eval_gcd(args),
        BuiltinKind::Lcm => eval_lcm(args),
        BuiltinKind::Min => eval_min(args),
        BuiltinKind::Max => eval_max(args),
        BuiltinKind::Expt => eval_expt(args),
        BuiltinKind::Truncate => eval_truncate(args),
        BuiltinKind::Round => eval_round(args),
        BuiltinKind::ZeroPred => {
            eval_numeric_order_predicate("zero?", args, |ordering| ordering == Ordering::Equal)
        }
        BuiltinKind::PositivePred => eval_numeric_order_predicate("positive?", args, |ordering| {
            ordering == Ordering::Greater
        }),
        BuiltinKind::NegativePred => {
            eval_numeric_order_predicate("negative?", args, |ordering| ordering == Ordering::Less)
        }
        BuiltinKind::OddPred => eval_integer_predicate("odd?", args, |value| value % 2 != 0),
        BuiltinKind::EvenPred => eval_integer_predicate("even?", args, |value| value % 2 == 0),
        BuiltinKind::ListRef => eval_list_ref(args),
        BuiltinKind::ListTail => eval_list_tail(args),
        BuiltinKind::ListPred => eval_predicate("list?", args, is_proper_list),
        BuiltinKind::Member => eval_member(args),
        BuiltinKind::Assoc => eval_assoc(args),
        BuiltinKind::Assv => eval_assv(args),
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
        BuiltinKind::StringGreaterThan => {
            eval_string_compare("string>?", args, |lhs, rhs| lhs > rhs)
        }
        BuiltinKind::StringLessThanOrEqual => {
            eval_string_compare("string<=?", args, |lhs, rhs| lhs <= rhs)
        }
        BuiltinKind::StringGreaterThanOrEqual => {
            eval_string_compare("string>=?", args, |lhs, rhs| lhs >= rhs)
        }
        BuiltinKind::StringCiEqual => eval_string_compare("string-ci=?", args, |lhs, rhs| {
            lhs.to_lowercase() == rhs.to_lowercase()
        }),
        BuiltinKind::StringUpcase => {
            eval_string_transform("string-upcase", args, |value| value.to_uppercase())
        }
        BuiltinKind::StringDowncase => {
            eval_string_transform("string-downcase", args, |value| value.to_lowercase())
        }
        BuiltinKind::Vector => eval_vector(args),
        BuiltinKind::MakeVector => eval_make_vector(args),
        BuiltinKind::VectorRef => eval_vector_ref(args),
        BuiltinKind::VectorSet => eval_vector_set(args),
        BuiltinKind::VectorLength => eval_vector_length(args),
        BuiltinKind::VectorPred => {
            eval_predicate("vector?", args, |value| matches!(value, Value::Vector(_)))
        }
        BuiltinKind::VectorToList => eval_vector_to_list(args),
        BuiltinKind::ListToVector => eval_list_to_vector(args),
        BuiltinKind::SyntaxToDatum => eval_syntax_to_datum(args),
        BuiltinKind::DatumToSyntax => eval_datum_to_syntax(args),
    }
}

fn integer_value(value: i64) -> Value {
    Value::Number(Number::Integer(value))
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = Number::zero();
    for arg in numeric_args(args)? {
        total = total.add(arg)?;
    }
    Ok(Value::Number(total))
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
        first.negate()?
    } else {
        let mut total = *first;
        for value in rest {
            total = total.sub(*value)?;
        }
        total
    };

    Ok(Value::Number(result))
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = Number::one();
    for arg in numeric_args(args)? {
        total = total.mul(arg)?;
    }
    Ok(Value::Number(total))
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
        total = total.div(*value)?;
    }

    Ok(Value::Number(total))
}

fn eval_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(Ordering) -> bool,
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
        if !compare(pair[0].compare(pair[1])?) {
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

    Ok(Value::Pair(Rc::new(RefCell::new(PairCell {
        car: first.clone(),
        cdr: rest.clone(),
    }))))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    pair_parts(value)
        .map(|(car, _)| car)
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

    pair_parts(value)
        .map(|(_, cdr)| cdr)
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        })
}

fn eval_composed_car_cdr(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let mut current = value.clone();
    for op in name[1..name.len() - 1].chars().rev() {
        current =
            match op {
                'a' => pair_parts(&current).map(|(car, _)| car).ok_or_else(|| {
                    EvalError::TypeMismatch {
                        expected: "pair",
                        found: current.type_name().into(),
                    }
                })?,
                'd' => pair_parts(&current).map(|(_, cdr)| cdr).ok_or_else(|| {
                    EvalError::TypeMismatch {
                        expected: "pair",
                        found: current.type_name().into(),
                    }
                })?,
                _ => unreachable!("composed car/cdr builtin names must only contain a and d"),
            };
    }

    Ok(current)
}

fn eval_set_car(args: &[Value]) -> Result<Value, EvalError> {
    let [pair, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "set-car!".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let Value::Pair(pair) = pair else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: pair.type_name().into(),
        });
    };

    pair.borrow_mut().car = value.clone();
    Ok(Value::Void)
}

fn eval_set_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [pair, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "set-cdr!".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let Value::Pair(pair) = pair else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: pair.type_name().into(),
        });
    };

    pair.borrow_mut().cdr = value.clone();
    Ok(Value::Void)
}

fn eval_null(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(is_empty_list(value)))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(integer_value(collect_list_items(value)?.len() as i64))
}

fn eval_append(args: &[Value]) -> Result<Value, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(list_from_vec(Vec::new()));
    };

    let mut prefix_items = Vec::new();
    for value in prefix {
        prefix_items.extend(collect_list_items(value)?);
    }

    let mut result = last.clone();
    for item in prefix_items.into_iter().rev() {
        result = Value::Pair(Rc::new(RefCell::new(PairCell {
            car: item,
            cdr: result,
        })));
    }

    Ok(result)
}

fn eval_reverse(args: &[Value]) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "reverse".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let mut items = collect_list_items(list)?;
    items.reverse();
    Ok(list_from_vec(items))
}

fn eval_apply(args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply".into(),
            expected: "at least 2 arguments".into(),
            got: args.len(),
        }
        .into());
    }

    let callable = args[0].clone();
    let tail_args = collect_list_items(&args[args.len() - 1])?;

    let mut applied_args = Vec::with_capacity(args.len() - 2 + tail_args.len());
    applied_args.extend(args[1..args.len() - 1].iter().cloned());
    applied_args.extend(tail_args);

    super::apply_callable(callable, &applied_args, continuation)
}

fn eval_values(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::from_values(args.to_vec()))
}

fn eval_call_with_values(args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
    let [producer, consumer] = args else {
        return Err(EvalError::WrongArgCount {
            name: "call-with-values".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }
        .into());
    };

    let producer_continuation = Rc::new(Continuation::CallWithValues {
        consumer: consumer.clone(),
        next: Rc::clone(continuation),
    });
    let produced = super::apply_callable(producer.clone(), &[], &producer_continuation)?;
    super::continue_with(producer_continuation, produced)
}

fn eval_call_cc(args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
    let [callable] = args else {
        return Err(EvalError::WrongArgCount {
            name: "call/cc".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }
        .into());
    };

    let handle = current_continuation_handle(continuation);
    let result = super::apply_callable(
        callable.clone(),
        &[Value::ContinuationHandle(handle.clone())],
        continuation,
    );

    if result.is_ok() {
        expire_continuation_handle(&handle);
    }

    result
}

fn eval_raise(args: &[Value]) -> EvalResult<Value> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "raise".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }
        .into());
    };

    Err(EvalSignal::Raise(RaisedException::new(value.clone())))
}

fn eval_with_exception_handler(
    args: &[Value],
    continuation: &ContinuationRef,
) -> EvalResult<Value> {
    let [handler, thunk] = args else {
        return Err(EvalError::WrongArgCount {
            name: "with-exception-handler".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }
        .into());
    };

    match super::apply_callable(thunk.clone(), &[], continuation) {
        Ok(value) => Ok(value),
        Err(EvalSignal::Raise(exception)) => {
            super::apply_callable(handler.clone(), &[exception.value], continuation)
        }
        Err(signal) => Err(signal),
    }
}

fn eval_eq_like<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: FnOnce(&Value, &Value) -> bool,
{
    let [lhs, rhs] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(compare(lhs, rhs)))
}

fn eval_equality(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    eval_eq_like(name, args, values_equal)
}

fn eval_map(args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map".into(),
            expected: "at least 2 arguments".into(),
            got: args.len(),
        }
        .into());
    }

    let callable = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(collect_list_items)
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = lists[0].len();
    for list in &lists[1..] {
        if list.len() != expected_len {
            return Err(EvalError::LengthMismatch {
                name: "map".into(),
                expected: expected_len,
                got: list.len(),
            }
            .into());
        }
    }

    let mut result = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        result.push(super::apply_callable(
            callable.clone(),
            &call_args,
            continuation,
        )?);
    }

    Ok(list_from_vec(result))
}

fn eval_for_each(args: &[Value], continuation: &ContinuationRef) -> EvalResult<Value> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "for-each".into(),
            expected: "at least 2 arguments".into(),
            got: args.len(),
        }
        .into());
    }

    let callable = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(collect_list_items)
        .collect::<Result<Vec<_>, _>>()?;

    let expected_len = lists[0].len();
    for list in &lists[1..] {
        if list.len() != expected_len {
            return Err(EvalError::LengthMismatch {
                name: "for-each".into(),
                expected: expected_len,
                got: list.len(),
            }
            .into());
        }
    }

    for index in 0..expected_len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        super::apply_callable(callable.clone(), &call_args, continuation)?;
    }

    Ok(Value::Void)
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

fn eval_make_string(args: &[Value]) -> Result<Value, EvalError> {
    let (len, fill) = match args {
        [len] => (non_negative_index(len)?, ' '),
        [len, fill] => (non_negative_index(len)?, fill.as_char()?),
        _ => {
            return Err(EvalError::WrongArgCount {
                name: "make-string".into(),
                expected: "1 or 2 arguments".into(),
                got: args.len(),
            });
        }
    };

    Ok(Value::String(std::iter::repeat_n(fill, len).collect()))
}

fn eval_string(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::String(
        args.iter()
            .map(Value::as_char)
            .collect::<Result<String, _>>()?,
    ))
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

    Ok(integer_value(value.as_string()?.chars().count() as i64))
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

    Ok(match parse_number_string(&value.as_string()?) {
        Some(number) => Value::Number(number),
        None => Value::Boolean(false),
    })
}

fn eval_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "number->string".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::String(value.as_number()?.render()))
}

fn eval_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "exact->inexact".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Number(Number::from_inexact(
        value.as_number()?.to_f64(),
    )?))
}

fn eval_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "inexact->exact".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Number(value.as_number()?.to_exact()?))
}

fn eval_numerator(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "numerator".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(integer_value(value.as_number()?.to_exact()?.numerator()?))
}

fn eval_denominator(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "denominator".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(integer_value(value.as_number()?.to_exact()?.denominator()?))
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

fn eval_syntax_to_datum(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "syntax->datum".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    datum_from_syntax_value(value)
}

fn eval_datum_to_syntax(args: &[Value]) -> Result<Value, EvalError> {
    let [context, datum] = args else {
        return Err(EvalError::WrongArgCount {
            name: "datum->syntax".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    datum_to_syntax_value(context, datum)
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

fn eval_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->list".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(list_from_vec(
        value.as_string()?.chars().map(Value::Char).collect(),
    ))
}

fn eval_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list->string".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let result = collect_list_items(list)?
        .iter()
        .map(Value::as_char)
        .collect::<Result<String, _>>()?;
    Ok(Value::String(result))
}

fn eval_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char->integer".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(integer_value(value.as_char()? as u32 as i64))
}

fn eval_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "integer->char".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let code_point = value.as_integer()?;
    let Some(ch) = char::from_u32(code_point as u32) else {
        return Err(EvalError::InvalidCodePoint { value: code_point });
    };

    Ok(Value::Char(ch))
}

fn eval_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

fn eval_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    let (len, fill) = match args {
        [len] => (non_negative_index(len)?, Value::Void),
        [len, fill] => (non_negative_index(len)?, fill.clone()),
        _ => {
            return Err(EvalError::WrongArgCount {
                name: "make-vector".into(),
                expected: "1 or 2 arguments".into(),
                got: args.len(),
            });
        }
    };

    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

fn eval_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [vector, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let vector = vector_items(vector)?;
    let index = non_negative_index(index)?;
    let vector = vector.borrow();

    vector
        .get(index)
        .cloned()
        .ok_or(EvalError::IndexOutOfBounds {
            index,
            len: vector.len(),
        })
}

fn eval_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    let [vector, index, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!".into(),
            expected: "exactly 3 arguments".into(),
            got: args.len(),
        });
    };

    let vector = vector_items(vector)?;
    let index = non_negative_index(index)?;
    let mut vector = vector.borrow_mut();
    if index >= vector.len() {
        return Err(EvalError::IndexOutOfBounds {
            index,
            len: vector.len(),
        });
    }

    vector[index] = value.clone();
    Ok(Value::Void)
}

fn eval_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(integer_value(vector_items(vector)?.borrow().len() as i64))
}

fn eval_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector->list".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(list_from_vec(vector_items(vector)?.borrow().clone()))
}

fn eval_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list->vector".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Vector(Rc::new(RefCell::new(collect_list_items(
        list,
    )?))))
}

fn eval_abs(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "abs".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Number(value.as_number()?.abs()?))
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
    Ok(integer_value(result))
}

fn eval_remainder(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_numeric_args("remainder", args)?;
    Ok(integer_value(
        dividend
            .checked_rem(divisor)
            .ok_or(EvalError::IntegerOverflow)?,
    ))
}

fn eval_quotient(args: &[Value]) -> Result<Value, EvalError> {
    let (dividend, divisor) = binary_numeric_args("quotient", args)?;
    Ok(integer_value(
        dividend
            .checked_div(divisor)
            .ok_or(EvalError::IntegerOverflow)?,
    ))
}

fn eval_gcd(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 0_i64;
    for value in args {
        result = gcd_i64(result, value.as_integer()?);
    }
    Ok(integer_value(result.abs()))
}

fn eval_lcm(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = 1_i64;
    if args.is_empty() {
        return Ok(integer_value(result));
    }

    for value in args {
        let value = value.as_integer()?;
        if value == 0 || result == 0 {
            result = 0;
            continue;
        }

        let gcd = gcd_i64(result, value);
        result = result
            .checked_div(gcd)
            .and_then(|partial| partial.checked_mul(value))
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(integer_value(result.abs()))
}

fn eval_min(args: &[Value]) -> Result<Value, EvalError> {
    let values = at_least_one_numeric_arg("min", args)?;
    let mut best = *values.first().expect("at least one value");
    for value in values.into_iter().skip(1) {
        if value.compare(best)? == Ordering::Less {
            best = value;
        }
    }
    Ok(Value::Number(best))
}

fn eval_max(args: &[Value]) -> Result<Value, EvalError> {
    let values = at_least_one_numeric_arg("max", args)?;
    let mut best = *values.first().expect("at least one value");
    for value in values.into_iter().skip(1) {
        if value.compare(best)? == Ordering::Greater {
            best = value;
        }
    }
    Ok(Value::Number(best))
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

    Ok(integer_value(result))
}

fn eval_truncate(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "truncate".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let number = value.as_number()?;
    Ok(match number {
        Number::Integer(value) => integer_value(value),
        Number::Rational(_) => integer_value(number.numerator()? / number.denominator()?),
        Number::Inexact(value) => Value::Number(Number::from_inexact(value.trunc())?),
    })
}

fn eval_round(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "round".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let number = value.as_number()?;
    Ok(match number {
        Number::Integer(value) => integer_value(value),
        Number::Rational(_) => integer_value(number.to_f64().round() as i64),
        Number::Inexact(value) => Value::Number(Number::from_inexact(value.round())?),
    })
}

fn eval_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let items = collect_list_items(list)?;
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

    let index = non_negative_index(index)?;
    let mut current = list.clone();
    let mut seen_pairs = std::collections::HashSet::new();
    let mut remaining = index;

    while remaining > 0 {
        match current {
            Value::List(items) => {
                if remaining > items.len() {
                    return Err(EvalError::IndexOutOfBounds {
                        index,
                        len: index - remaining + items.len(),
                    });
                }
                return Ok(list_from_vec(items[remaining..].to_vec()));
            }
            Value::Pair(pair) => {
                if !seen_pairs.insert(Rc::as_ptr(&pair) as usize) {
                    return Err(EvalError::CircularList);
                }
                current = pair.borrow().cdr.clone();
                remaining -= 1;
            }
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "list",
                    found: other.type_name().into(),
                })
            }
        }
    }

    if is_proper_list(&current) {
        Ok(current)
    } else {
        Err(EvalError::TypeMismatch {
            expected: "list",
            found: current.type_name().into(),
        })
    }
}

fn eval_member(args: &[Value]) -> Result<Value, EvalError> {
    let [needle, list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "member".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let mut current = list.clone();
    let mut seen_pairs = std::collections::HashSet::new();

    loop {
        if is_empty_list(&current) {
            return Ok(Value::Boolean(false));
        }

        let Some((car, cdr)) = pair_parts(&current) else {
            return Err(EvalError::TypeMismatch {
                expected: "list",
                found: current.type_name().into(),
            });
        };

        if values_equal(needle, &car) {
            return Ok(current);
        }

        if let Value::Pair(pair) = &current {
            if !seen_pairs.insert(Rc::as_ptr(pair) as usize) {
                return Err(EvalError::CircularList);
            }
        }

        current = cdr;
    }
}

fn eval_assoc(args: &[Value]) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(EvalError::WrongArgCount {
            name: "assoc".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    for entry in collect_list_items(alist)? {
        let entry_key = pair_head(&entry)?;
        if values_equal(key, &entry_key) {
            return Ok(entry);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_assv(args: &[Value]) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(EvalError::WrongArgCount {
            name: "assv".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    for entry in collect_list_items(alist)? {
        let entry_key = pair_head(&entry)?;
        if values_eqv(key, &entry_key) {
            return Ok(entry);
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

fn gcd_i64(mut lhs: i64, mut rhs: i64) -> i64 {
    lhs = lhs.abs();
    rhs = rhs.abs();

    if lhs == 0 {
        return rhs;
    }
    if rhs == 0 {
        return lhs;
    }

    while rhs != 0 {
        let remainder = lhs % rhs;
        lhs = rhs;
        rhs = remainder;
    }

    lhs
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

    Ok(Value::Boolean(pair_parts(value).is_some()))
}

fn numeric_args(args: &[Value]) -> Result<Vec<Number>, EvalError> {
    args.iter().map(Value::as_number).collect()
}

fn at_least_one_numeric_arg(name: &str, args: &[Value]) -> Result<Vec<Number>, EvalError> {
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

fn eval_numeric_order_predicate<F>(
    name: &str,
    args: &[Value],
    predicate: F,
) -> Result<Value, EvalError>
where
    F: FnOnce(Ordering) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(predicate(
        value.as_number()?.compare(Number::zero())?,
    )))
}

fn vector_items(value: &Value) -> Result<&Rc<RefCell<Vec<Value>>>, EvalError> {
    match value {
        Value::Vector(items) => Ok(items),
        _ => Err(EvalError::TypeMismatch {
            expected: "vector",
            found: value.type_name().into(),
        }),
    }
}

fn pair_head(value: &Value) -> Result<Value, EvalError> {
    pair_parts(value)
        .map(|(car, _)| car)
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        })
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
