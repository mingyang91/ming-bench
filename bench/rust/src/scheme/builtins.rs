use super::{
    eval_program,
    number::{parse_number_token, Number, Rational},
    values_eq, values_equal, BuiltinFn, EnvRef, Environment, EvalError, EvaluatedArg, Expr,
    PairValue, Procedure, RecordType, RecordValue, SchemeString, Value, VectorValue,
};
use std::{cell::RefCell, cmp::Ordering, rc::Rc};

pub(super) fn default_env() -> EnvRef {
    let env = Environment::new(None);
    for (name, func) in [
        ("+", apply_add as BuiltinFn),
        ("-", apply_sub as BuiltinFn),
        ("*", apply_mul as BuiltinFn),
        ("/", apply_div as BuiltinFn),
        ("abs", apply_abs as BuiltinFn),
        ("modulo", apply_modulo as BuiltinFn),
        ("remainder", apply_remainder as BuiltinFn),
        ("quotient", apply_quotient as BuiltinFn),
        ("min", apply_min as BuiltinFn),
        ("max", apply_max as BuiltinFn),
        ("expt", apply_expt as BuiltinFn),
        ("<", apply_lt as BuiltinFn),
        (">", apply_gt as BuiltinFn),
        ("=", apply_eq as BuiltinFn),
        ("<=", apply_lte as BuiltinFn),
        ("not", apply_not as BuiltinFn),
        ("eq?", apply_eq_pred as BuiltinFn),
        ("eqv?", apply_eqv_pred as BuiltinFn),
        ("equal?", apply_equal_pred as BuiltinFn),
        ("cons", apply_cons as BuiltinFn),
        ("car", apply_car as BuiltinFn),
        ("cdr", apply_cdr as BuiltinFn),
        ("null?", apply_null as BuiltinFn),
        ("list", apply_list as BuiltinFn),
        ("list?", apply_list_pred as BuiltinFn),
        ("length", apply_length as BuiltinFn),
        ("list-ref", apply_list_ref as BuiltinFn),
        ("list-tail", apply_list_tail as BuiltinFn),
        ("assoc", apply_assoc as BuiltinFn),
        ("append", apply_append as BuiltinFn),
        ("map", apply_map as BuiltinFn),
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
        ("string-append", apply_string_append as BuiltinFn),
        ("string-copy", apply_string_copy as BuiltinFn),
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
        ("string-ci=?", apply_string_ci_eq_pred as BuiltinFn),
        ("string-upcase", apply_string_upcase as BuiltinFn),
        ("string-downcase", apply_string_downcase as BuiltinFn),
        ("char?", apply_char_pred as BuiltinFn),
        ("char-alphabetic?", apply_char_alphabetic_pred as BuiltinFn),
        ("char-numeric?", apply_char_numeric_pred as BuiltinFn),
        ("char-upcase", apply_char_upcase as BuiltinFn),
        ("char-downcase", apply_char_downcase as BuiltinFn),
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

pub(super) fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool { value, .. } => Value::Bool(*value),
        Expr::Number { value, .. } => Value::Number(*value),
        Expr::Char { value, .. } => Value::Char(*value),
        Expr::String { value, .. } => Value::String(SchemeString::immutable(value)),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => Value::List(items.iter().map(quote_expr).collect()),
    }
}

pub(super) fn apply_procedure(
    value: Value,
    args: &[EvaluatedArg],
    output: &mut String,
) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = value else {
        return Err(EvalError::NotAProcedure {
            found: value.render(),
        });
    };

    match procedure.as_ref() {
        Procedure::Builtin { func, .. } => func(args, output),
        Procedure::Lambda { params, body, env } => {
            apply_lambda_procedure("lambda", params, body, env, args, output)
        }
        Procedure::CaseLambda { clauses, env } => {
            let Some(clause) = clauses
                .iter()
                .find(|clause| clause.params.matches_arity(args.len()))
            else {
                return Err(EvalError::WrongArgCount {
                    name: "case-lambda",
                    expected: "matching clause",
                    got: args.len(),
                });
            };

            apply_lambda_procedure(
                "case-lambda",
                &clause.params,
                &clause.body,
                env,
                args,
                output,
            )
        }
        Procedure::RecordConstructor {
            record_type,
            field_count,
            ..
        } => apply_record_constructor(record_type.clone(), *field_count, args),
        Procedure::RecordPredicate { record_type, .. } => apply_record_predicate(record_type, args),
        Procedure::RecordAccessor {
            record_type,
            field_index,
            ..
        } => apply_record_accessor(record_type, *field_index, args),
        Procedure::RecordMutator {
            record_type,
            field_index,
            ..
        } => apply_record_mutator(record_type, *field_index, args),
    }
}

fn apply_lambda_procedure(
    name: &'static str,
    params: &super::LambdaParams,
    body: &[Expr],
    env: &EnvRef,
    args: &[EvaluatedArg],
    output: &mut String,
) -> Result<Value, EvalError> {
    if args.len() < params.fixed_arity() {
        return Err(EvalError::WrongArgCountAtLeast {
            name,
            min: params.fixed_arity(),
            got: args.len(),
        });
    }

    if !params.matches_arity(args.len()) {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exact parameter count",
            got: args.len(),
        });
    }

    let call_env = Environment::new(Some(env.clone()));
    for (binding_name, arg) in params.fixed.iter().zip(args.iter()) {
        call_env.define(binding_name.clone(), arg.value.clone());
    }

    if let Some(rest_name) = &params.rest {
        let rest_items = args[params.fixed_arity()..]
            .iter()
            .map(|arg| arg.value.clone())
            .collect();
        call_env.define(rest_name.clone(), Value::List(rest_items));
    }

    eval_program(body, call_env, output)
}

fn exact_int(value: i64) -> Value {
    Value::Number(Number::exact_int(value))
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

fn apply_cons(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cons",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    match &tail.value {
        Value::List(tail_items) => {
            let mut items = Vec::with_capacity(tail_items.len() + 1);
            items.push(head.value.clone());
            items.extend(tail_items.iter().cloned());
            Ok(Value::List(items))
        }
        _ => Ok(Value::Pair(Rc::new(PairValue {
            head: head.value.clone(),
            tail: tail.value.clone(),
        }))),
    }
}

fn apply_car(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    match &value.value {
        Value::List(items) => items.first().cloned().ok_or_else(|| {
            EvalError::TypeMismatch {
                expected: "non-empty pair",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col)
        }),
        Value::Pair(pair) => Ok(pair.head.clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.value.render(),
        }
        .with_position(value.pos.line, value.pos.col)),
    }
}

fn apply_cdr(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    match &value.value {
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::TypeMismatch {
                    expected: "non-empty pair",
                    found: value.value.render(),
                }
                .with_position(value.pos.line, value.pos.col));
            }

            Ok(Value::List(items[1..].to_vec()))
        }
        Value::Pair(pair) => Ok(pair.tail.clone()),
        _ => Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.value.render(),
        }
        .with_position(value.pos.line, value.pos.col)),
    }
}

fn apply_null(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(
        matches!(&value.value, Value::List(items) if items.is_empty()),
    ))
}

fn apply_list(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    Ok(Value::List(
        args.iter().map(|arg| arg.value.clone()).collect(),
    ))
}

fn apply_list_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::List(_))))
}

fn apply_length(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(value.as_list()?.len() as i64))
}

fn apply_list_ref(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    let index = parse_index_arg(index, items.len())?;
    Ok(items[index].clone())
}

fn apply_list_tail(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list-tail",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    let index = parse_index_bound(index, items.len(), true)?;
    Ok(Value::List(items[index..].to_vec()))
}

fn apply_assoc(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [key, list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "assoc",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let entries = list.as_list()?;
    for entry in entries {
        let candidate = match entry {
            Value::List(items) => items.first(),
            Value::Pair(pair) => Some(&pair.head),
            _ => {
                return Err(EvalError::TypeMismatch {
                    expected: "association list entry",
                    found: entry.render(),
                }
                .with_position(list.pos.line, list.pos.col));
            }
        };

        if let Some(candidate) = candidate {
            if values_equal(&key.value, candidate) {
                return Ok(entry.clone());
            }
        }
    }

    Ok(Value::Bool(false))
}

fn apply_append(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut items = Vec::new();
    for value in args {
        items.extend(value.as_list()?.iter().cloned());
    }
    Ok(Value::List(items))
}

fn apply_map(args: &[EvaluatedArg], output: &mut String) -> Result<Value, EvalError> {
    let [procedure, lists @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "at least 2",
            got: 0,
        });
    };

    if lists.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "at least 2",
            got: 1,
        });
    }

    let list_values = lists
        .iter()
        .map(EvaluatedArg::as_list)
        .collect::<Result<Vec<_>, _>>()?;
    let limit = list_values
        .iter()
        .map(|items| items.len())
        .min()
        .unwrap_or(0);

    let mut results = Vec::with_capacity(limit);
    for index in 0..limit {
        let call_args = lists
            .iter()
            .zip(list_values.iter())
            .map(|(arg, items)| EvaluatedArg {
                value: items[index].clone(),
                pos: arg.pos,
            })
            .collect::<Vec<_>>();
        let value = apply_procedure(procedure.value.clone(), &call_args, output)
            .map_err(|error| error.with_position(procedure.pos.line, procedure.pos.col))?;
        results.push(value);
    }

    Ok(Value::List(results))
}

fn apply_vector(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(args.iter().map(|arg| arg.value.clone()).collect()),
    })))
}

fn apply_make_vector(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let (size_arg, fill) = match args {
        [size] => (size, Value::Void),
        [size, fill] => (size, fill.value.clone()),
        _ => {
            return Err(EvalError::WrongArgCount {
                name: "make-vector",
                expected: "1 or 2",
                got: args.len(),
            });
        }
    };

    let len = parse_length_arg(size_arg)?;
    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(vec![fill; len]),
    })))
}

fn apply_vector_ref(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [vector, index] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let vector = vector.as_vector()?;
    let elements = vector.elements.borrow();
    let index = parse_index_arg(index, elements.len())?;
    Ok(elements[index].clone())
}

fn apply_vector_set(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [vector, index, value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!",
            expected: "exactly 3",
            got: args.len(),
        });
    };

    let vector = vector.as_vector()?;
    let mut elements = vector.elements.borrow_mut();
    let index = parse_index_arg(index, elements.len())?;
    elements[index] = value.value.clone();
    Ok(Value::Void)
}

fn apply_vector_length(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector-length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(exact_int(vector.as_vector()?.elements.borrow().len() as i64))
}

fn apply_vector_pred(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Vector(_))))
}

fn apply_vector_to_list(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [vector] = args else {
        return Err(EvalError::WrongArgCount {
            name: "vector->list",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::List(vector.as_vector()?.elements.borrow().clone()))
}

fn apply_list_to_vector(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [list] = args else {
        return Err(EvalError::WrongArgCount {
            name: "list->vector",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Vector(Rc::new(VectorValue {
        elements: RefCell::new(list.as_list()?.to_vec()),
    })))
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
    applied_args.extend(tail_items.iter().cloned().map(|value| EvaluatedArg {
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

fn apply_string_append(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let mut combined = String::new();
    for arg in args {
        let value = arg.as_string()?;
        combined.push_str(&value.to_plain_string());
    }
    Ok(Value::String(SchemeString::immutable(combined)))
}

fn apply_string_copy(args: &[EvaluatedArg], _output: &mut String) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string-copy",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::String(value.as_string()?.mutable_copy()))
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

    Ok(Value::String(SchemeString::immutable(
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

    Ok(Value::String(SchemeString::immutable(
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

    Ok(Value::String(SchemeString::immutable(value.as_symbol()?)))
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

    Ok(Value::String(SchemeString::immutable(
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

    Ok(Value::String(SchemeString::immutable(
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
