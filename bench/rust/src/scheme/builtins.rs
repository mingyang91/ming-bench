use super::core::{
    make_pair, make_string, value_equal, BuiltinProcedure, EnvRef, Environment, Runtime, StringRef,
    Value,
};
use super::error::EvalError;
use super::eval::apply_procedure;

const BUILTINS: &[BuiltinProcedure] = &[
    BuiltinProcedure {
        name: "abs",
        func: builtin_abs,
    },
    BuiltinProcedure {
        name: "+",
        func: builtin_add,
    },
    BuiltinProcedure {
        name: "-",
        func: builtin_sub,
    },
    BuiltinProcedure {
        name: "*",
        func: builtin_mul,
    },
    BuiltinProcedure {
        name: "/",
        func: builtin_div,
    },
    BuiltinProcedure {
        name: "<",
        func: builtin_lt,
    },
    BuiltinProcedure {
        name: ">",
        func: builtin_gt,
    },
    BuiltinProcedure {
        name: "=",
        func: builtin_eq,
    },
    BuiltinProcedure {
        name: "eq?",
        func: builtin_is_eq,
    },
    BuiltinProcedure {
        name: "equal?",
        func: builtin_is_equal,
    },
    BuiltinProcedure {
        name: "<=",
        func: builtin_le,
    },
    BuiltinProcedure {
        name: "not",
        func: builtin_not,
    },
    BuiltinProcedure {
        name: "cons",
        func: builtin_cons,
    },
    BuiltinProcedure {
        name: "car",
        func: builtin_car,
    },
    BuiltinProcedure {
        name: "cdr",
        func: builtin_cdr,
    },
    BuiltinProcedure {
        name: "null?",
        func: builtin_null,
    },
    BuiltinProcedure {
        name: "list",
        func: builtin_list,
    },
    BuiltinProcedure {
        name: "length",
        func: builtin_length,
    },
    BuiltinProcedure {
        name: "list-ref",
        func: builtin_list_ref,
    },
    BuiltinProcedure {
        name: "list-tail",
        func: builtin_list_tail,
    },
    BuiltinProcedure {
        name: "list?",
        func: builtin_is_list,
    },
    BuiltinProcedure {
        name: "map",
        func: builtin_map,
    },
    BuiltinProcedure {
        name: "append",
        func: builtin_append,
    },
    BuiltinProcedure {
        name: "assoc",
        func: builtin_assoc,
    },
    BuiltinProcedure {
        name: "string?",
        func: builtin_is_string,
    },
    BuiltinProcedure {
        name: "number?",
        func: builtin_is_number,
    },
    BuiltinProcedure {
        name: "boolean?",
        func: builtin_is_boolean,
    },
    BuiltinProcedure {
        name: "pair?",
        func: builtin_is_pair,
    },
    BuiltinProcedure {
        name: "symbol?",
        func: builtin_is_symbol,
    },
    BuiltinProcedure {
        name: "display",
        func: builtin_display,
    },
    BuiltinProcedure {
        name: "write",
        func: builtin_write,
    },
    BuiltinProcedure {
        name: "newline",
        func: builtin_newline,
    },
    BuiltinProcedure {
        name: "string-append",
        func: builtin_string_append,
    },
    BuiltinProcedure {
        name: "string-length",
        func: builtin_string_length,
    },
    BuiltinProcedure {
        name: "substring",
        func: builtin_substring,
    },
    BuiltinProcedure {
        name: "string->number",
        func: builtin_string_to_number,
    },
    BuiltinProcedure {
        name: "number->string",
        func: builtin_number_to_string,
    },
    BuiltinProcedure {
        name: "symbol->string",
        func: builtin_symbol_to_string,
    },
    BuiltinProcedure {
        name: "string->symbol",
        func: builtin_string_to_symbol,
    },
    BuiltinProcedure {
        name: "string-copy",
        func: builtin_string_copy,
    },
    BuiltinProcedure {
        name: "string-set!",
        func: builtin_string_set,
    },
    BuiltinProcedure {
        name: "string-ref",
        func: builtin_string_ref,
    },
    BuiltinProcedure {
        name: "char?",
        func: builtin_is_char,
    },
    BuiltinProcedure {
        name: "char-alphabetic?",
        func: builtin_is_char_alphabetic,
    },
    BuiltinProcedure {
        name: "char-numeric?",
        func: builtin_is_char_numeric,
    },
    BuiltinProcedure {
        name: "char-upcase",
        func: builtin_char_upcase,
    },
    BuiltinProcedure {
        name: "char-downcase",
        func: builtin_char_downcase,
    },
    BuiltinProcedure {
        name: "char=?",
        func: builtin_char_eq,
    },
    BuiltinProcedure {
        name: "char<?",
        func: builtin_char_lt,
    },
    BuiltinProcedure {
        name: "string=?",
        func: builtin_string_eq,
    },
    BuiltinProcedure {
        name: "string<?",
        func: builtin_string_lt,
    },
    BuiltinProcedure {
        name: "string-ci=?",
        func: builtin_string_ci_eq,
    },
    BuiltinProcedure {
        name: "string-upcase",
        func: builtin_string_upcase,
    },
    BuiltinProcedure {
        name: "string-downcase",
        func: builtin_string_downcase,
    },
    BuiltinProcedure {
        name: "modulo",
        func: builtin_modulo,
    },
    BuiltinProcedure {
        name: "remainder",
        func: builtin_remainder,
    },
    BuiltinProcedure {
        name: "quotient",
        func: builtin_quotient,
    },
    BuiltinProcedure {
        name: "min",
        func: builtin_min,
    },
    BuiltinProcedure {
        name: "max",
        func: builtin_max,
    },
    BuiltinProcedure {
        name: "expt",
        func: builtin_expt,
    },
    BuiltinProcedure {
        name: "zero?",
        func: builtin_is_zero,
    },
    BuiltinProcedure {
        name: "positive?",
        func: builtin_is_positive,
    },
    BuiltinProcedure {
        name: "negative?",
        func: builtin_is_negative,
    },
    BuiltinProcedure {
        name: "odd?",
        func: builtin_is_odd,
    },
    BuiltinProcedure {
        name: "even?",
        func: builtin_is_even,
    },
    BuiltinProcedure {
        name: "apply",
        func: builtin_apply,
    },
];

#[derive(Clone, Copy)]
enum IndexBound {
    AllowEnd,
    Exact,
}

pub(crate) fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for builtin in BUILTINS {
        Environment::define(
            &env,
            builtin.name.to_string(),
            Value::Procedure(std::rc::Rc::new(super::core::Procedure::Builtin(*builtin))),
        );
    }

    env
}

fn builtin_abs(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("abs", args)?)?;
    Ok(Value::Int(value.abs()))
}

fn builtin_add(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total += expect_number(arg)?;
    }

    Ok(Value::Int(total))
}

fn builtin_sub(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = eval_number_args("-", args, 1)?;
    let result = match values.split_first() {
        Some((first, [])) => -*first,
        Some((first, rest)) => rest.iter().fold(*first, |acc, value| acc - value),
        None => return Err(wrong_arg_count("-", "at least 1", 0)),
    };

    Ok(Value::Int(result))
}

fn builtin_mul(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut product = 1_i64;

    for arg in args {
        product *= expect_number(arg)?;
    }

    Ok(Value::Int(product))
}

fn builtin_div(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = eval_number_args("/", args, 2)?;
    let Some((first, rest)) = values.split_first() else {
        return Err(wrong_arg_count("/", "at least 2", 0));
    };
    let mut result = *first;

    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        if result % value != 0 {
            return Err(EvalError::NonIntegerDivision);
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn builtin_lt(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    builtin_compare("<", args, |lhs, rhs| lhs < rhs)
}

fn builtin_gt(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    builtin_compare(">", args, |lhs, rhs| lhs > rhs)
}

fn builtin_eq(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    builtin_compare("=", args, |lhs, rhs| lhs == rhs)
}

fn builtin_is_eq(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(wrong_arg_count("eq?", "exactly 2", args.len()));
    };

    Ok(Value::Bool(eq_value(lhs, rhs)))
}

fn builtin_is_equal(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [lhs, rhs] = args else {
        return Err(wrong_arg_count("equal?", "exactly 2", args.len()));
    };

    Ok(Value::Bool(value_equal(lhs, rhs)))
}

fn builtin_le(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    builtin_compare("<=", args, |lhs, rhs| lhs <= rhs)
}

fn builtin_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_number_args(name, args, 2)?;

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn builtin_not(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    Ok(Value::Bool(!expect_single_arg("not", args)?.is_truthy()))
}

fn builtin_cons(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(wrong_arg_count("cons", "exactly 2", args.len()));
    };

    match tail {
        Value::List(values) => {
            let mut list = Vec::with_capacity(values.len() + 1);
            list.push(head.clone());
            list.extend(values.iter().cloned());
            Ok(Value::List(list))
        }
        _ => Ok(make_pair(head.clone(), tail.clone())),
    }
}

fn builtin_car(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    pair_car(expect_single_arg("car", args)?)
}

fn builtin_cdr(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    pair_cdr(expect_single_arg("cdr", args)?)
}

fn builtin_null(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    Ok(Value::Bool(matches!(
        expect_single_arg("null?", args)?,
        Value::List(values) if values.is_empty()
    )))
}

fn builtin_list(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("length", args)?, "list")?;
    Ok(Value::Int(values.len() as i64))
}

fn builtin_list_ref(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(wrong_arg_count("list-ref", "exactly 2", args.len()));
    };

    let values = expect_list(list, "list")?;
    let index = expect_index(index, values.len(), IndexBound::Exact)?;
    Ok(values[index].clone())
}

fn builtin_list_tail(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [list, index] = args else {
        return Err(wrong_arg_count("list-tail", "exactly 2", args.len()));
    };

    let values = expect_list(list, "list")?;
    let index = expect_index(index, values.len(), IndexBound::AllowEnd)?;
    Ok(Value::List(values[index..].to_vec()))
}

fn builtin_is_list(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("list?", args, |value| matches!(value, Value::List(_)))
}

fn builtin_map(args: &[Value], runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((operator, list_args)) = args.split_first() else {
        return Err(wrong_arg_count("map", "at least 2", 0));
    };

    if list_args.is_empty() {
        return Err(wrong_arg_count("map", "at least 2", 1));
    }

    let lists = list_args
        .iter()
        .map(|list| expect_list(list, "list"))
        .collect::<Result<Vec<_>, _>>()?;
    let len = lists.iter().map(|list| list.len()).min().unwrap_or(0);
    let mut results = Vec::with_capacity(len);

    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_procedure(operator.clone(), &call_args, runtime)?);
    }

    Ok(Value::List(results))
}

fn builtin_append(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut combined = Vec::new();

    for arg in args {
        combined.extend(expect_list(arg, "list")?.iter().cloned());
    }

    Ok(Value::List(combined))
}

fn builtin_assoc(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [key, alist] = args else {
        return Err(wrong_arg_count("assoc", "exactly 2", args.len()));
    };

    for entry in expect_list(alist, "list")? {
        if value_equal(key, &pair_car(entry)?) {
            return Ok(entry.clone());
        }
    }

    Ok(Value::Bool(false))
}

fn builtin_is_string(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("string?", args, |value| matches!(value, Value::String(_)))
}

fn builtin_is_number(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("number?", args, |value| matches!(value, Value::Int(_)))
}

fn builtin_is_boolean(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("boolean?", args, |value| matches!(value, Value::Bool(_)))
}

fn builtin_is_pair(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("pair?", args, |value| {
        matches!(value, Value::List(values) if !values.is_empty())
            || matches!(value, Value::Pair(_))
    })
}

fn builtin_is_symbol(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
}

fn builtin_display(args: &[Value], runtime: &mut Runtime) -> Result<Value, EvalError> {
    runtime.display(expect_single_arg("display", args)?);
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], runtime: &mut Runtime) -> Result<Value, EvalError> {
    runtime.write(expect_single_arg("write", args)?);
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], runtime: &mut Runtime) -> Result<Value, EvalError> {
    expect_no_args("newline", args)?;
    runtime.newline();
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut combined = String::new();

    for arg in args {
        let text = expect_string(arg)?;
        combined.push_str(&text);
    }

    Ok(make_string(combined))
}

fn builtin_string_length(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_string(expect_single_arg("string-length", args)?)?;
    Ok(Value::Int(value.chars().count() as i64))
}

fn builtin_substring(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [string, start, end] = args else {
        return Err(wrong_arg_count("substring", "exactly 3", args.len()));
    };

    let chars = expect_string(string)?.chars().collect::<Vec<_>>();
    let start_index = expect_index(start, chars.len(), IndexBound::AllowEnd)?;
    let end_index = expect_index(end, chars.len(), IndexBound::AllowEnd)?;

    if start_index > end_index {
        return Err(EvalError::InvalidSubstringRange {
            start: start_index as i64,
            end: end_index as i64,
            len: chars.len(),
        });
    }

    Ok(make_string(
        chars[start_index..end_index].iter().collect::<String>(),
    ))
}

fn builtin_string_to_number(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_string(expect_single_arg("string->number", args)?)?;
    match value.parse::<i64>() {
        Ok(number) => Ok(Value::Int(number)),
        Err(_) => Ok(Value::Bool(false)),
    }
}

fn builtin_number_to_string(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("number->string", args)?)?;
    Ok(make_string(value.to_string()))
}

fn builtin_symbol_to_string(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_symbol(expect_single_arg("symbol->string", args)?)?;
    Ok(make_string(value.to_string()))
}

fn builtin_string_to_symbol(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_string(expect_single_arg("string->symbol", args)?)?;
    Ok(Value::Symbol(value))
}

fn builtin_string_copy(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    Ok(make_string(expect_string(expect_single_arg(
        "string-copy",
        args,
    )?)?))
}

fn builtin_string_set(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [string, index, character] = args else {
        return Err(wrong_arg_count("string-set!", "exactly 3", args.len()));
    };

    let string_ref = expect_string_ref(string)?;
    let mut chars = {
        let text = string_ref.borrow();
        text.chars().collect::<Vec<_>>()
    };
    let index = expect_index(index, chars.len(), IndexBound::Exact)?;
    chars[index] = expect_char(character)?;
    *string_ref.borrow_mut() = chars.into_iter().collect();

    Ok(Value::Void)
}

fn builtin_string_ref(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [string, index] = args else {
        return Err(wrong_arg_count("string-ref", "exactly 2", args.len()));
    };

    let chars = expect_string(string)?.chars().collect::<Vec<_>>();
    let index = expect_index(index, chars.len(), IndexBound::Exact)?;
    Ok(Value::Char(chars[index]))
}

fn builtin_is_char(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    unary_predicate("char?", args, |value| matches!(value, Value::Char(_)))
}

fn builtin_is_char_alphabetic(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_char(expect_single_arg("char-alphabetic?", args)?)?;
    Ok(Value::Bool(value.is_alphabetic()))
}

fn builtin_is_char_numeric(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_char(expect_single_arg("char-numeric?", args)?)?;
    Ok(Value::Bool(value.is_numeric()))
}

fn builtin_char_upcase(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_char(expect_single_arg("char-upcase", args)?)?;
    Ok(Value::Char(value.to_ascii_uppercase()))
}

fn builtin_char_downcase(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_char(expect_single_arg("char-downcase", args)?)?;
    Ok(Value::Char(value.to_ascii_lowercase()))
}

fn builtin_char_eq(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    compare_char_args("char=?", args, |lhs, rhs| lhs == rhs)
}

fn builtin_char_lt(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    compare_char_args("char<?", args, |lhs, rhs| lhs < rhs)
}

fn builtin_string_eq(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    compare_string_args("string=?", args, |lhs, rhs| lhs == rhs)
}

fn builtin_string_lt(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    compare_string_args("string<?", args, |lhs, rhs| lhs < rhs)
}

fn builtin_string_ci_eq(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    compare_string_args("string-ci=?", args, |lhs, rhs| {
        lhs.to_lowercase() == rhs.to_lowercase()
    })
}

fn builtin_string_upcase(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_string(expect_single_arg("string-upcase", args)?)?;
    Ok(make_string(value.to_uppercase()))
}

fn builtin_string_downcase(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_string(expect_single_arg("string-downcase", args)?)?;
    Ok(make_string(value.to_lowercase()))
}

fn builtin_modulo(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("modulo", args)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let remainder = dividend % divisor;
    let result = if remainder != 0 && remainder.signum() != divisor.signum() {
        remainder + divisor
    } else {
        remainder
    };

    Ok(Value::Int(result))
}

fn builtin_remainder(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("remainder", args)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Int(dividend % divisor))
}

fn builtin_quotient(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (dividend, divisor) = expect_two_numbers("quotient", args)?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    Ok(Value::Int(dividend / divisor))
}

fn builtin_min(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = eval_number_args("min", args, 1)?;
    Ok(Value::Int(
        *values.iter().min().expect("min has at least one arg"),
    ))
}

fn builtin_max(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = eval_number_args("max", args, 1)?;
    Ok(Value::Int(
        *values.iter().max().expect("max has at least one arg"),
    ))
}

fn builtin_expt(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (base, exponent) = expect_two_numbers("expt", args)?;
    if exponent < 0 {
        return Err(EvalError::NegativeExponent { exponent });
    }

    Ok(Value::Int(base.pow(exponent as u32)))
}

fn builtin_is_zero(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("zero?", args)?)?;
    Ok(Value::Bool(value == 0))
}

fn builtin_is_positive(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("positive?", args)?)?;
    Ok(Value::Bool(value > 0))
}

fn builtin_is_negative(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("negative?", args)?)?;
    Ok(Value::Bool(value < 0))
}

fn builtin_is_odd(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("odd?", args)?)?;
    Ok(Value::Bool(value % 2 != 0))
}

fn builtin_is_even(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let value = expect_number(expect_single_arg("even?", args)?)?;
    Ok(Value::Bool(value % 2 == 0))
}

fn builtin_apply(args: &[Value], runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((operator, arg_parts)) = args.split_first() else {
        return Err(wrong_arg_count("apply", "at least 2", 0));
    };

    let Some((list_arg, prefix_args)) = arg_parts.split_last() else {
        return Err(wrong_arg_count("apply", "at least 2", 1));
    };

    let mut applied_args = prefix_args.to_vec();
    applied_args.extend(expect_list(list_arg, "list")?.iter().cloned());
    apply_procedure(operator.clone(), &applied_args, runtime)
}

fn unary_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: FnOnce(&Value) -> bool,
{
    Ok(Value::Bool(predicate(expect_single_arg(name, args)?)))
}

fn pair_car(value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::List(values) => values.first().cloned().ok_or_else(empty_pair_error),
        Value::Pair(pair) => Ok(pair.borrow().car.clone()),
        other => Err(type_mismatch("pair", other)),
    }
}

fn pair_cdr(value: &Value) -> Result<Value, EvalError> {
    match value {
        Value::List(values) => {
            if values.is_empty() {
                Err(empty_pair_error())
            } else {
                Ok(Value::List(values[1..].to_vec()))
            }
        }
        Value::Pair(pair) => Ok(pair.borrow().cdr.clone()),
        other => Err(type_mismatch("pair", other)),
    }
}

fn expect_list<'a>(value: &'a Value, expected: &str) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(values) => Ok(values),
        other => Err(type_mismatch(expected, other)),
    }
}

fn expect_single_arg<'a>(name: &str, args: &'a [Value]) -> Result<&'a Value, EvalError> {
    match args {
        [value] => Ok(value),
        _ => Err(wrong_arg_count(name, "exactly 1", args.len())),
    }
}

fn expect_no_args(name: &str, args: &[Value]) -> Result<(), EvalError> {
    if args.is_empty() {
        Ok(())
    } else {
        Err(wrong_arg_count(name, "exactly 0", args.len()))
    }
}

fn expect_string_ref(value: &Value) -> Result<&StringRef, EvalError> {
    match value {
        Value::String(text) => Ok(text),
        other => Err(type_mismatch("string", other)),
    }
}

fn expect_string(value: &Value) -> Result<String, EvalError> {
    let text = expect_string_ref(value)?.borrow();
    Ok(text.to_string())
}

fn expect_symbol(value: &Value) -> Result<String, EvalError> {
    match value {
        Value::Symbol(text) => Ok(text.clone()),
        other => Err(type_mismatch("symbol", other)),
    }
}

fn expect_char(value: &Value) -> Result<char, EvalError> {
    match value {
        Value::Char(ch) => Ok(*ch),
        other => Err(type_mismatch("character", other)),
    }
}

fn expect_two_numbers(name: &str, args: &[Value]) -> Result<(i64, i64), EvalError> {
    let [lhs, rhs] = args else {
        return Err(wrong_arg_count(name, "exactly 2", args.len()));
    };

    Ok((expect_number(lhs)?, expect_number(rhs)?))
}

fn expect_index(value: &Value, len: usize, bound: IndexBound) -> Result<usize, EvalError> {
    let index = expect_number(value)?;

    if index < 0 {
        return Err(EvalError::IndexOutOfBounds { index, len });
    }

    let index = index as usize;
    let in_bounds = match bound {
        IndexBound::AllowEnd => index <= len,
        IndexBound::Exact => index < len,
    };

    if in_bounds {
        Ok(index)
    } else {
        Err(EvalError::IndexOutOfBounds {
            index: index as i64,
            len,
        })
    }
}

fn eval_number_args(name: &str, args: &[Value], min: usize) -> Result<Vec<i64>, EvalError> {
    if args.len() < min {
        return Err(wrong_arg_count(name, format!("at least {min}"), args.len()));
    }

    args.iter().map(expect_number).collect()
}

fn expect_number(value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(*number),
        other => Err(type_mismatch("number", other)),
    }
}

fn compare_char_args<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let values = args
        .iter()
        .map(expect_char)
        .collect::<Result<Vec<_>, _>>()?;

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn compare_string_args<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let values = args
        .iter()
        .map(expect_string)
        .collect::<Result<Vec<_>, _>>()?;

    for pair in values.windows(2) {
        if !compare(pair[0].as_str(), pair[1].as_str()) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eq_value(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
        (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
        (Value::Symbol(lhs), Value::Symbol(rhs)) => lhs == rhs,
        (Value::Char(lhs), Value::Char(rhs)) => lhs == rhs,
        (Value::String(lhs), Value::String(rhs)) => std::rc::Rc::ptr_eq(lhs, rhs),
        (Value::List(lhs), Value::List(rhs)) => lhs.is_empty() && rhs.is_empty(),
        (Value::Pair(lhs), Value::Pair(rhs)) => std::rc::Rc::ptr_eq(lhs, rhs),
        (Value::Procedure(lhs), Value::Procedure(rhs)) => std::rc::Rc::ptr_eq(lhs, rhs),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn empty_pair_error() -> EvalError {
    EvalError::TypeMismatch {
        expected: "pair".into(),
        found: "list".into(),
    }
}

fn wrong_arg_count(name: &str, expected: impl Into<String>, actual: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        actual,
    }
}

fn type_mismatch(expected: &str, value: &Value) -> EvalError {
    EvalError::TypeMismatch {
        expected: expected.into(),
        found: value.type_name().into(),
    }
}
