use super::core::{make_string, BuiltinProcedure, EnvRef, Environment, Runtime, StringRef, Value};
use super::error::EvalError;
use super::eval::apply_procedure;

const BUILTINS: &[BuiltinProcedure] = &[
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
        name: "append",
        func: builtin_append,
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

    let mut values = vec![head.clone()];
    values.extend(expect_list(tail, "list")?.iter().cloned());
    Ok(Value::List(values))
}

fn builtin_car(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("car", args)?, "pair")?;
    values.first().cloned().ok_or_else(empty_pair_error)
}

fn builtin_cdr(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let values = expect_list(expect_single_arg("cdr", args)?, "pair")?;
    if values.is_empty() {
        return Err(empty_pair_error());
    }

    Ok(Value::List(values[1..].to_vec()))
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

fn builtin_append(args: &[Value], _runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut combined = Vec::new();

    for arg in args {
        combined.extend(expect_list(arg, "list")?.iter().cloned());
    }

    Ok(Value::List(combined))
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
    unary_predicate(
        "pair?",
        args,
        |value| matches!(value, Value::List(values) if !values.is_empty()),
    )
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
