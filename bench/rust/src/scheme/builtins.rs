mod procedure;
mod sequence;

use self::procedure::exact_int;
pub(super) use self::procedure::{apply_procedure, quote_expr};
use self::sequence::{
    apply_append, apply_assoc, apply_assv, apply_car, apply_cddr, apply_cdr, apply_cons,
    apply_for_each, apply_length, apply_list, apply_list_pred, apply_list_ref, apply_list_tail,
    apply_list_to_vector, apply_make_vector, apply_map, apply_member, apply_null, apply_reverse,
    apply_set_car, apply_set_cdr, apply_vector, apply_vector_length, apply_vector_pred,
    apply_vector_ref, apply_vector_set, apply_vector_to_list,
};
use super::{
    list_from_values,
    number::{parse_number_token, Number, Rational},
    values_eq, values_equal, BuiltinFn, EnvRef, Environment, EvalError, EvaluatedArg, Procedure,
    RecordType, RecordValue, SchemeString, Value,
};
use std::{cell::RefCell, cmp::Ordering, rc::Rc};

pub(super) fn default_env() -> EnvRef {
    let env = Environment::new(None);
    for name in ["call/cc", "call-with-current-continuation"] {
        env.define(
            name,
            Value::Procedure(Rc::new(Procedure::ContinuationCapture { name })),
        );
    }
    env.define(
        "dynamic-wind",
        Value::Procedure(Rc::new(Procedure::DynamicWind {
            name: "dynamic-wind",
        })),
    );
    for (name, func) in [
        ("+", apply_add as BuiltinFn),
        ("-", apply_sub as BuiltinFn),
        ("*", apply_mul as BuiltinFn),
        ("/", apply_div as BuiltinFn),
        ("abs", apply_abs as BuiltinFn),
        ("gcd", apply_gcd as BuiltinFn),
        ("lcm", apply_lcm as BuiltinFn),
        ("modulo", apply_modulo as BuiltinFn),
        ("remainder", apply_remainder as BuiltinFn),
        ("quotient", apply_quotient as BuiltinFn),
        ("min", apply_min as BuiltinFn),
        ("max", apply_max as BuiltinFn),
        ("expt", apply_expt as BuiltinFn),
        ("truncate", apply_truncate as BuiltinFn),
        ("round", apply_round as BuiltinFn),
        ("<", apply_lt as BuiltinFn),
        (">", apply_gt as BuiltinFn),
        ("=", apply_eq as BuiltinFn),
        ("<=", apply_lte as BuiltinFn),
        (">=", apply_gte as BuiltinFn),
        ("not", apply_not as BuiltinFn),
        ("eq?", apply_eq_pred as BuiltinFn),
        ("eqv?", apply_eqv_pred as BuiltinFn),
        ("equal?", apply_equal_pred as BuiltinFn),
        ("cons", apply_cons as BuiltinFn),
        ("car", apply_car as BuiltinFn),
        ("cdr", apply_cdr as BuiltinFn),
        ("cddr", apply_cddr as BuiltinFn),
        ("set-car!", apply_set_car as BuiltinFn),
        ("set-cdr!", apply_set_cdr as BuiltinFn),
        ("null?", apply_null as BuiltinFn),
        ("list", apply_list as BuiltinFn),
        ("list?", apply_list_pred as BuiltinFn),
        ("length", apply_length as BuiltinFn),
        ("list-ref", apply_list_ref as BuiltinFn),
        ("list-tail", apply_list_tail as BuiltinFn),
        ("member", apply_member as BuiltinFn),
        ("assv", apply_assv as BuiltinFn),
        ("assoc", apply_assoc as BuiltinFn),
        ("append", apply_append as BuiltinFn),
        ("reverse", apply_reverse as BuiltinFn),
        ("map", apply_map as BuiltinFn),
        ("for-each", apply_for_each as BuiltinFn),
        ("vector", apply_vector as BuiltinFn),
        ("make-vector", apply_make_vector as BuiltinFn),
        ("vector-ref", apply_vector_ref as BuiltinFn),
        ("vector-set!", apply_vector_set as BuiltinFn),
        ("vector-length", apply_vector_length as BuiltinFn),
        ("vector?", apply_vector_pred as BuiltinFn),
        ("vector->list", apply_vector_to_list as BuiltinFn),
        ("list->vector", apply_list_to_vector as BuiltinFn),
        ("zero?", apply_zero_pred as BuiltinFn),
        ("positive?", apply_positive_pred as BuiltinFn),
        ("negative?", apply_negative_pred as BuiltinFn),
        ("odd?", apply_odd_pred as BuiltinFn),
        ("even?", apply_even_pred as BuiltinFn),
        ("string?", apply_string_pred as BuiltinFn),
        ("number?", apply_number_pred as BuiltinFn),
        ("integer?", apply_integer_pred as BuiltinFn),
        ("rational?", apply_rational_pred as BuiltinFn),
        ("exact?", apply_exact_pred as BuiltinFn),
        ("inexact?", apply_inexact_pred as BuiltinFn),
        ("exact->inexact", apply_exact_to_inexact as BuiltinFn),
        ("inexact->exact", apply_inexact_to_exact as BuiltinFn),
        ("numerator", apply_numerator as BuiltinFn),
        ("denominator", apply_denominator as BuiltinFn),
        ("boolean?", apply_boolean_pred as BuiltinFn),
        ("pair?", apply_pair_pred as BuiltinFn),
        ("symbol?", apply_symbol_pred as BuiltinFn),
        ("procedure?", apply_procedure_pred as BuiltinFn),
        ("apply", apply_apply as BuiltinFn),
        ("display", apply_display as BuiltinFn),
        ("write", apply_write as BuiltinFn),
        ("newline", apply_newline as BuiltinFn),
        ("make-string", apply_make_string as BuiltinFn),
        ("string", apply_string as BuiltinFn),
        ("string-append", apply_string_append as BuiltinFn),
        ("string-copy", apply_string_copy as BuiltinFn),
        ("string->list", apply_string_to_list as BuiltinFn),
        ("list->string", apply_list_to_string as BuiltinFn),
        ("string-length", apply_string_length as BuiltinFn),
        ("substring", apply_substring as BuiltinFn),
        ("string->number", apply_string_to_number as BuiltinFn),
        ("number->string", apply_number_to_string as BuiltinFn),
        ("symbol->string", apply_symbol_to_string as BuiltinFn),
        ("string->symbol", apply_string_to_symbol as BuiltinFn),
        ("string-ref", apply_string_ref as BuiltinFn),
        ("string-set!", apply_string_set as BuiltinFn),
        ("string=?", apply_string_eq_pred as BuiltinFn),
        ("string<?", apply_string_lt_pred as BuiltinFn),
        ("string>?", apply_string_gt_pred as BuiltinFn),
        ("string<=?", apply_string_lte_pred as BuiltinFn),
        ("string>=?", apply_string_gte_pred as BuiltinFn),
        ("string-ci=?", apply_string_ci_eq_pred as BuiltinFn),
        ("string-upcase", apply_string_upcase as BuiltinFn),
        ("string-downcase", apply_string_downcase as BuiltinFn),
        ("char?", apply_char_pred as BuiltinFn),
        ("char-alphabetic?", apply_char_alphabetic_pred as BuiltinFn),
        ("char-numeric?", apply_char_numeric_pred as BuiltinFn),
        ("char-upcase", apply_char_upcase as BuiltinFn),
        ("char-downcase", apply_char_downcase as BuiltinFn),
        ("char->integer", apply_char_to_integer as BuiltinFn),
        ("integer->char", apply_integer_to_char as BuiltinFn),
        ("char=?", apply_char_eq as BuiltinFn),
        ("char<?", apply_char_lt as BuiltinFn),
    ] {
        env.define(
            name,
            Value::Procedure(Rc::new(Procedure::Builtin { name, func })),
        );
    }
    env
}

fn gcd_i64(mut left: i64, mut right: i64) -> i64 {
    left = left.abs();
    right = right.abs();
    while right != 0 {
        let next = left % right;
        left = right;
        right = next;
    }
    left
}

fn number_to_truncated_i64(number: Number) -> Option<i64> {
    match number {
        Number::Exact(rational) => Some(rational.numerator() / rational.denominator()),
        Number::Inexact(value) if value.is_finite() => Some(value.trunc() as i64),
        Number::Inexact(_) => None,
    }
}

fn extract_rational(arg: &EvaluatedArg) -> Result<Rational, EvalError> {
    arg.as_number()?.as_rational().ok_or_else(|| {
        EvalError::TypeMismatch {
            expected: "exact number",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col)
    })
}

fn apply_record_constructor(
    record_type: Rc<RecordType>,
    field_count: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    if args.len() != field_count {
        return Err(EvalError::WrongArgCount {
            name: "record constructor",
            expected: "exact parameter count",
            got: args.len(),
        });
    }

    Ok(Value::Record(Rc::new(RecordValue {
        record_type,
        fields: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
    })))
}

fn apply_record_predicate(
    record_type: &Rc<RecordType>,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record predicate",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(
        &value.value,
        Value::Record(record) if Rc::ptr_eq(&record.record_type, record_type)
    )))
}

fn apply_record_accessor(
    record_type: &Rc<RecordType>,
    field_index: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [record_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record accessor",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let record = expect_record_instance(record_arg, record_type)?;
    let value = {
        let fields = record.fields.borrow();
        fields[field_index].clone()
    };
    Ok(value)
}

fn apply_record_mutator(
    record_type: &Rc<RecordType>,
    field_index: usize,
    args: &[EvaluatedArg],
) -> Result<Value, EvalError> {
    let [record_arg, value_arg] = args else {
        return Err(EvalError::WrongArgCount {
            name: "record mutator",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let record = expect_record_instance(record_arg, record_type)?;
    record.fields.borrow_mut()[field_index] = value_arg.value.clone();
    Ok(Value::Void)
}

fn expect_record_instance(
    arg: &EvaluatedArg,
    record_type: &Rc<RecordType>,
) -> Result<Rc<RecordValue>, EvalError> {
    let Value::Record(record) = &arg.value else {
        return Err(EvalError::TypeMismatch {
            expected: "record",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col));
    };

    if !Rc::ptr_eq(&record.record_type, record_type) {
        return Err(EvalError::TypeMismatch {
            expected: "record",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col));
    }

    Ok(record.clone())
}

fn apply_add(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut sum = Number::exact_int(0);
    for arg in args {
        sum = sum.add(arg.as_number()?);
    }
    Ok(Value::Number(sum))
}

fn apply_sub(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            name: "-",
            expected: "at least 1",
            got: 0,
        }),
        [value] => Ok(Value::Number(value.as_number()?.neg())),
        [first, rest @ ..] => {
            let mut result = first.as_number()?;
            for arg in rest {
                result = result.sub(arg.as_number()?);
            }
            Ok(Value::Number(result))
        }
    }
}

fn apply_mul(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut product = Number::exact_int(1);
    for arg in args {
        product = product.mul(arg.as_number()?);
    }
    Ok(Value::Number(product))
}

fn apply_div(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "/",
            expected: "at least 2",
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/",
            expected: "at least 2",
            got: 1,
        });
    }

    let mut result = first.as_number()?;
    for arg in rest {
        let divisor = arg.as_number()?;
        if divisor.is_zero() {
            return Err(EvalError::DivisionByZero.with_position(arg.pos.line, arg.pos.col));
        }
        result = result.div(divisor).expect("non-zero divisor must divide");
    }
    Ok(Value::Number(result))
}

fn apply_abs(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "abs",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Number(value.as_number()?.abs()))
}

fn apply_gcd(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = 0i64;
    for arg in args {
        result = gcd_i64(result, arg.as_int()?);
    }
    Ok(exact_int(result))
}

fn apply_lcm(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = 1i64;
    for arg in args {
        let value = arg.as_int()?;
        if value == 0 || result == 0 {
            result = 0;
        } else {
            result = (result / gcd_i64(result, value)) * value.abs();
        }
    }
    Ok(exact_int(result.abs()))
}

fn apply_modulo(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(EvalError::WrongArgCount {
            name: "modulo",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let left = dividend.as_int()?;
    let right = divisor.as_int()?;
    if right == 0 {
        return Err(EvalError::DivisionByZero.with_position(divisor.pos.line, divisor.pos.col));
    }

    let remainder = left % right;
    let result = if remainder != 0 && (remainder > 0) != (right > 0) {
        remainder + right
    } else {
        remainder
    };
    Ok(exact_int(result))
}

fn apply_remainder(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(EvalError::WrongArgCount {
            name: "remainder",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let left = dividend.as_int()?;
    let right = divisor.as_int()?;
    if right == 0 {
        return Err(EvalError::DivisionByZero.with_position(divisor.pos.line, divisor.pos.col));
    }

    Ok(exact_int(left % right))
}

fn apply_quotient(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [dividend, divisor] = args else {
        return Err(EvalError::WrongArgCount {
            name: "quotient",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let left = dividend.as_int()?;
    let right = divisor.as_int()?;
    if right == 0 {
        return Err(EvalError::DivisionByZero.with_position(divisor.pos.line, divisor.pos.col));
    }

    Ok(exact_int(left / right))
}

fn apply_min(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "min",
            expected: "at least 1",
            got: 0,
        });
    };

    let mut current = first.as_number()?;
    for arg in rest {
        let candidate = arg.as_number()?;
        if candidate.compare(current) == Some(Ordering::Less) {
            current = candidate;
        }
    }
    Ok(Value::Number(current))
}

fn apply_max(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "max",
            expected: "at least 1",
            got: 0,
        });
    };

    let mut current = first.as_number()?;
    for arg in rest {
        let candidate = arg.as_number()?;
        if candidate.compare(current) == Some(Ordering::Greater) {
            current = candidate;
        }
    }
    Ok(Value::Number(current))
}

fn apply_expt(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [base, exponent] = args else {
        return Err(EvalError::WrongArgCount {
            name: "expt",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let base = base.as_int()?;
    let exponent_value = exponent.as_int()?;
    let Ok(exponent) = u32::try_from(exponent_value) else {
        return Err(EvalError::TypeMismatch {
            expected: "non-negative integer",
            found: exponent_value.to_string(),
        }
        .with_position(exponent.pos.line, exponent.pos.col));
    };

    Ok(exact_int(base.pow(exponent)))
}

fn apply_truncate(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "truncate",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let Some(result) = number_to_truncated_i64(value.as_number()?) else {
        return Err(EvalError::TypeMismatch {
            expected: "finite number",
            found: value.value.render(),
        }
        .with_position(value.pos.line, value.pos.col));
    };

    Ok(exact_int(result))
}

fn apply_round(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "round",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let number = value.as_number()?;
    let result = match number {
        Number::Exact(rational) => {
            (rational.numerator() as f64 / rational.denominator() as f64).round() as i64
        }
        Number::Inexact(raw) if raw.is_finite() => raw.round() as i64,
        Number::Inexact(_) => {
            return Err(EvalError::TypeMismatch {
                expected: "finite number",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col));
        }
    };

    Ok(exact_int(result))
}

fn apply_lt(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_comparison("<", args, |ordering| ordering == Ordering::Less)
}

fn apply_gt(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_comparison(">", args, |ordering| ordering == Ordering::Greater)
}

fn apply_eq(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_comparison("=", args, |ordering| ordering == Ordering::Equal)
}

fn apply_lte(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_comparison("<=", args, |ordering| {
        matches!(ordering, Ordering::Less | Ordering::Equal)
    })
}

fn apply_gte(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_comparison(">=", args, |ordering| {
        matches!(ordering, Ordering::Greater | Ordering::Equal)
    })
}

fn apply_comparison(
    name: &'static str,
    args: &[EvaluatedArg],
    predicate: impl Fn(Ordering) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let numbers = args
        .iter()
        .map(EvaluatedArg::as_number)
        .collect::<Result<Vec<_>, _>>()?;
    let is_match = numbers
        .windows(2)
        .all(|pair| pair[0].compare(pair[1]).map(&predicate).unwrap_or(false));
    Ok(Value::Bool(is_match))
}

fn apply_not(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(!value.value.is_truthy()))
}

fn apply_eq_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            name: "eq?",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    Ok(Value::Bool(values_eq(&left.value, &right.value)))
}

fn apply_eqv_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            name: "eqv?",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    Ok(Value::Bool(values_eq(&left.value, &right.value)))
}

fn apply_equal_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [left, right] = args else {
        return Err(EvalError::WrongArgCount {
            name: "equal?",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    Ok(Value::Bool(values_equal(&left.value, &right.value)))
}

fn apply_string_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::String(_))))
}

fn apply_number_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "number?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Number(_))))
}

fn apply_integer_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "integer?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_integer()))
}

fn apply_rational_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "rational?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_rational()))
}

fn apply_exact_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "exact?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_exact()))
}

fn apply_inexact_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "inexact?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_inexact()))
}

fn apply_exact_to_inexact(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "exact->inexact",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Number(value.as_number()?.to_inexact()))
}

fn apply_inexact_to_exact(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "inexact->exact",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let number = value.as_number()?;
    let Some(exact) = number.to_exact() else {
        return Err(EvalError::TypeMismatch {
            expected: "finite number",
            found: value.value.render(),
        }
        .with_position(value.pos.line, value.pos.col));
    };

    Ok(Value::Number(exact))
}

fn apply_numerator(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "numerator",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(extract_rational(value)?.numerator()))
}

fn apply_denominator(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "denominator",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(extract_rational(value)?.denominator()))
}

fn apply_boolean_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "boolean?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Bool(_))))
}

fn apply_pair_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "pair?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(
        matches!(&value.value, Value::List(items) if !items.is_empty())
            || matches!(&value.value, Value::Pair(_)),
    ))
}

fn apply_symbol_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "symbol?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Symbol(_))))
}

fn apply_procedure_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "procedure?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Procedure(_))))
}

fn apply_zero_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "zero?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_zero()))
}

fn apply_positive_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "positive?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_positive()))
}

fn apply_negative_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "negative?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_number()?.is_negative()))
}

fn apply_odd_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "odd?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_int()? % 2 != 0))
}

fn apply_even_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "even?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_int()? % 2 == 0))
}

fn apply_apply(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    let [procedure, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "apply",
            expected: "at least 2",
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "apply",
            expected: "at least 2",
            got: 1,
        });
    }

    let (tail, prefix) = rest.split_last().expect("checked for non-empty tail");
    let tail_items = tail.as_list()?;

    let mut applied_args = Vec::with_capacity(prefix.len() + tail_items.len());
    applied_args.extend(prefix.iter().cloned());
    applied_args.extend(tail_items.into_iter().map(|value| EvaluatedArg {
        value,
        pos: tail.pos,
    }));

    apply_procedure(procedure.value.clone(), &applied_args, output)
        .map_err(|error| error.with_position(procedure.pos.line, procedure.pos.col))
}

fn apply_display(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "display",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    output.push_str(&value.value.display());
    Ok(Value::Void)
}

fn apply_write(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "write",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    output.push_str(&value.value.render());
    Ok(Value::Void)
}

fn apply_newline(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline",
            expected: "exactly 0",
            got: args.len(),
        });
    }

    output.push('\n');
    Ok(Value::Void)
}

fn apply_make_string(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let (length, fill) = match args {
        [length] => (parse_length_arg(length)?, '\0'),
        [length, fill] => (parse_length_arg(length)?, fill.as_char()?),
        _ => {
            return Err(EvalError::WrongArgCount {
                name: "make-string",
                expected: "1 or 2",
                got: args.len(),
            });
        }
    };

    Ok(Value::String(SchemeString::runtime(
        std::iter::repeat_n(fill, length).collect::<String>(),
    )))
}

fn apply_string(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let string = args
        .iter()
        .map(EvaluatedArg::as_char)
        .collect::<Result<String, _>>()?;
    Ok(Value::String(SchemeString::runtime(string)))
}

fn apply_string_append(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut combined = String::new();
    for arg in args {
        let value = arg.as_string()?;
        combined.push_str(&value.to_plain_string());
    }
    Ok(Value::String(SchemeString::runtime(combined)))
}

fn apply_string_copy(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-copy",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(value.as_string()?.runtime_copy()))
}

fn apply_string_to_list(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->list",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let items: Vec<_> = value
        .as_string()?
        .to_plain_string()
        .chars()
        .map(Value::Char)
        .collect();
    Ok(list_from_values(items))
}

fn apply_list_to_string(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list->string",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let chars = list
        .as_list()?
        .into_iter()
        .map(|value| {
            value
                .as_char()
                .map_err(|error| error.with_position(list.pos.line, list.pos.col))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let string = chars.into_iter().collect::<String>();
    Ok(Value::String(SchemeString::runtime(string)))
}

fn apply_string_length(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(value.as_string()?.len() as i64))
}

fn apply_substring(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value, start, end] = args else {
        return Err(EvalError::WrongArgCount {
            name: "substring",
            expected: "exactly 3",
            got: args.len(),
        });
    };

    let source = value.as_string()?;
    let len = source.len();
    let start_index = parse_index_bound(start, len, true)?;
    let end_index = parse_index_bound(end, len, true)?;

    if start_index > end_index {
        return Err(EvalError::InvalidSubstringRange {
            start: start.as_int()?,
            end: end.as_int()?,
            len,
        }
        .with_position(end.pos.line, end.pos.col));
    }

    Ok(Value::String(SchemeString::runtime(
        source.substring(start_index, end_index),
    )))
}

fn apply_string_to_number(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->number",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let source = value.as_string()?;
    let owned = source.to_plain_string();
    let candidate = owned.trim();
    if let Some(number) = parse_number_token(candidate) {
        return Ok(Value::Number(number));
    }

    Ok(Value::Bool(false))
}

fn apply_number_to_string(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "number->string",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(SchemeString::runtime(
        value.as_number()?.render(),
    )))
}

fn apply_symbol_to_string(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(SchemeString::runtime(value.as_symbol()?)))
}

fn apply_string_to_symbol(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Symbol(value.as_string()?.to_plain_string()))
}

fn apply_string_ref(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let source = value.as_string()?;
    let char_index = parse_index_arg(index, source.len())?;
    Ok(Value::Char(
        source
            .char_at(char_index)
            .expect("validated string-ref index must exist"),
    ))
}

fn apply_string_set(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [target, index, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-set!",
            expected: "exactly 3",
            got: args.len(),
        });
    };

    let string = target.as_string()?;
    let char_index = parse_index_arg(index, string.len())?;
    string
        .set_char(char_index, value.as_char()?)
        .map_err(|error| error.with_position(target.pos.line, target.pos.col))?;
    Ok(Value::Void)
}

fn apply_string_eq_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_string_comparison("string=?", args, |left, right| left == right)
}

fn apply_string_lt_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_string_comparison("string<?", args, |left, right| left < right)
}

fn apply_string_gt_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_string_comparison("string>?", args, |left, right| left > right)
}

fn apply_string_lte_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_string_comparison("string<=?", args, |left, right| left <= right)
}

fn apply_string_gte_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_string_comparison("string>=?", args, |left, right| left >= right)
}

fn apply_string_ci_eq_pred(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    apply_string_comparison("string-ci=?", args, |left, right| {
        left.to_lowercase() == right.to_lowercase()
    })
}

fn apply_string_comparison(
    name: &'static str,
    args: &[EvaluatedArg],
    predicate: impl Fn(&str, &str) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let strings = args
        .iter()
        .map(|arg| Ok(arg.as_string()?.to_plain_string()))
        .collect::<Result<Vec<_>, EvalError>>()?;
    let is_match = strings
        .windows(2)
        .all(|pair| predicate(pair[0].as_str(), pair[1].as_str()));
    Ok(Value::Bool(is_match))
}

fn apply_string_upcase(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-upcase",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(SchemeString::runtime(
        value.as_string()?.to_plain_string().to_uppercase(),
    )))
}

fn apply_string_downcase(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-downcase",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(SchemeString::runtime(
        value.as_string()?.to_plain_string().to_lowercase(),
    )))
}

fn apply_char_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Char(_))))
}

fn apply_char_alphabetic_pred(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char-alphabetic?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_char()?.is_alphabetic()))
}

fn apply_char_numeric_pred(
    args: &[EvaluatedArg],
    _output: &mut String,
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char-numeric?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(value.as_char()?.is_numeric()))
}

fn apply_char_upcase(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char-upcase",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Char(value.as_char()?.to_ascii_uppercase()))
}

fn apply_char_downcase(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char-downcase",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Char(value.as_char()?.to_ascii_lowercase()))
}

fn apply_char_to_integer(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "char->integer",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(i64::from(u32::from(value.as_char()?))))
}

fn apply_integer_to_char(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "integer->char",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let code = value.as_int()?;
    let scalar = u32::try_from(code).map_err(|_| {
        EvalError::InvalidCharCode { value: code }.with_position(value.pos.line, value.pos.col)
    })?;
    let ch = char::from_u32(scalar).ok_or_else(|| {
        EvalError::InvalidCharCode { value: code }.with_position(value.pos.line, value.pos.col)
    })?;
    Ok(Value::Char(ch))
}

fn apply_char_eq(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_char_comparison("char=?", args, |left, right| left == right)
}

fn apply_char_lt(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    apply_char_comparison("char<?", args, |left, right| left < right)
}

fn apply_char_comparison(
    name: &'static str,
    args: &[EvaluatedArg],
    predicate: impl Fn(char, char) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let chars = args
        .iter()
        .map(EvaluatedArg::as_char)
        .collect::<Result<Vec<_>, _>>()?;
    let is_match = chars.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_match))
}

fn parse_index_arg(arg: &EvaluatedArg, len: usize) -> Result<usize, EvalError> {
    parse_index_bound(arg, len, false)
}

fn parse_length_arg(arg: &EvaluatedArg) -> Result<usize, EvalError> {
    let len = arg.as_int()?;
    usize::try_from(len).map_err(|_| {
        EvalError::TypeMismatch {
            expected: "non-negative integer",
            found: arg.value.render(),
        }
        .with_position(arg.pos.line, arg.pos.col)
    })
}

fn parse_index_bound(arg: &EvaluatedArg, len: usize, allow_end: bool) -> Result<usize, EvalError> {
    let index = arg.as_int()?;
    let Ok(index) = usize::try_from(index) else {
        return Err(
            EvalError::IndexOutOfBounds { index, len }.with_position(arg.pos.line, arg.pos.col)
        );
    };

    let is_out_of_bounds = if allow_end { index > len } else { index >= len };
    if is_out_of_bounds {
        return Err(EvalError::IndexOutOfBounds {
            index: index as i64,
            len,
        }
        .with_position(arg.pos.line, arg.pos.col));
    }

    Ok(index)
}
