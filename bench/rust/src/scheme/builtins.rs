use super::{
    apply_procedure, list_from_values, list_to_vec, make_pair, make_string, Env, EnvRef,
    EvalContext, EvalError, NativeFunc, Value,
};

pub(super) fn default_env() -> EnvRef {
    let env = Env::new(None);
    for (name, func) in [
        ("+", native_add as NativeFunc),
        ("-", native_sub as NativeFunc),
        ("*", native_mul as NativeFunc),
        ("/", native_div as NativeFunc),
        ("<", native_lt as NativeFunc),
        (">", native_gt as NativeFunc),
        ("=", native_num_eq as NativeFunc),
        ("<=", native_lte as NativeFunc),
        ("not", native_not as NativeFunc),
        ("cons", native_cons as NativeFunc),
        ("car", native_car as NativeFunc),
        ("cdr", native_cdr as NativeFunc),
        ("null?", native_null_pred as NativeFunc),
        ("list", native_list as NativeFunc),
        ("length", native_length as NativeFunc),
        ("append", native_append as NativeFunc),
        ("display", native_display as NativeFunc),
        ("write", native_write as NativeFunc),
        ("newline", native_newline as NativeFunc),
        ("string-copy", native_string_copy as NativeFunc),
        ("string-append", native_string_append as NativeFunc),
        ("string-length", native_string_length as NativeFunc),
        ("substring", native_substring as NativeFunc),
        ("string->number", native_string_to_number as NativeFunc),
        ("number->string", native_number_to_string as NativeFunc),
        ("symbol->string", native_symbol_to_string as NativeFunc),
        ("string->symbol", native_string_to_symbol as NativeFunc),
        ("string-ref", native_string_ref as NativeFunc),
        ("string-set!", native_string_set as NativeFunc),
        ("string?", native_string_pred as NativeFunc),
        ("number?", native_number_pred as NativeFunc),
        ("boolean?", native_boolean_pred as NativeFunc),
        ("char?", native_char_pred as NativeFunc),
        ("pair?", native_pair_pred as NativeFunc),
        ("symbol?", native_symbol_pred as NativeFunc),
        ("apply", native_apply as NativeFunc),
    ] {
        env.define(name.to_string(), Value::NativeProc { name, func });
    }
    env
}

fn native_not(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn native_cons(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    Ok(make_pair(args[0].clone(), args[1].clone()))
}

fn native_car(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "car",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("car")?;
    let car = pair.borrow().car.clone();
    Ok(car)
}

fn native_cdr(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "cdr",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("cdr")?;
    let cdr = pair.borrow().cdr.clone();
    Ok(cdr)
}

fn native_null_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("null?", args, ctx, |value| matches!(value, Value::Nil))
}

fn native_list(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    Ok(list_from_values(args.to_vec()))
}

fn native_length(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Integer(list_to_vec(&args[0], "length")?.len() as i64))
}

fn native_append(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(last) = args.last().cloned() else {
        return Ok(Value::Nil);
    };

    let mut result = last;
    for list in args[..args.len() - 1].iter().rev() {
        let mut items = list_to_vec(list, "append")?;
        while let Some(item) = items.pop() {
            result = make_pair(item, result);
        }
    }

    Ok(result)
}

fn native_display(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "display",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    ctx.push_output(&args[0].display_render());
    Ok(Value::Void)
}

fn native_write(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "write",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    ctx.push_output(&args[0].render());
    Ok(Value::Void)
}

fn native_newline(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "newline",
            expected: "exactly 0",
            got: args.len(),
        });
    }

    ctx.push_output("\n");
    Ok(Value::Void)
}

fn native_string_copy(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-copy",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_string(args[0].as_string("string-copy")?))
}

fn native_string_append(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut result = String::new();
    for value in args {
        let string = value.as_string("string-append")?;
        result.push_str(&string);
    }
    Ok(make_string(result))
}

fn native_string_length(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-length",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Integer(
        args[0].as_string("string-length")?.chars().count() as i64,
    ))
}

fn native_string_set(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "string-set!",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    let string = args[0].as_string_ref("string-set!")?;
    let index = args[1].as_number("string-set!")?;
    let value = args[2].as_char("string-set!")?;
    let mut string = string.borrow_mut();
    let len = string.len();
    let Some(index) = usize::try_from(index).ok().filter(|index| *index < len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-set!",
            index,
            len,
        });
    };

    string[index] = value;
    Ok(Value::Void)
}

fn native_substring(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "substring",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    let string = args[0].as_string("substring")?;
    let start = args[1].as_number("substring")?;
    let end = args[2].as_number("substring")?;
    let chars: Vec<char> = string.chars().collect();
    let len = chars.len();
    let (Ok(start), Ok(end)) = (usize::try_from(start), usize::try_from(end)) else {
        return Err(EvalError::InvalidRange {
            name: "substring",
            start,
            end,
            len,
        });
    };

    if start > end || end > len {
        return Err(EvalError::InvalidRange {
            name: "substring",
            start: start as i64,
            end: end as i64,
            len,
        });
    }

    let substring: String = chars[start..end].iter().copied().collect();
    Ok(make_string(substring))
}

fn native_string_to_number(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->number",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let string = args[0].as_string("string->number")?;
    match string.parse::<i64>() {
        Ok(value) => Ok(Value::Integer(value)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

fn native_number_to_string(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "number->string",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_string(
        args[0].as_number("number->string")?.to_string(),
    ))
}

fn native_symbol_to_string(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "symbol->string",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_string(args[0].as_symbol("symbol->string")?))
}

fn native_string_to_symbol(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->symbol",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Symbol(args[0].as_string("string->symbol")?))
}

fn native_string_ref(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "string-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let string = args[0].as_string("string-ref")?;
    let index = args[1].as_number("string-ref")?;
    let chars: Vec<char> = string.chars().collect();
    let len = chars.len();
    let Some(index) = usize::try_from(index).ok().filter(|index| *index < len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-ref",
            index,
            len,
        });
    };

    Ok(Value::Char(chars[index]))
}

fn native_string_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("string?", args, ctx, |value| {
        matches!(value, Value::String(_))
    })
}

fn native_number_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("number?", args, ctx, |value| {
        matches!(value, Value::Integer(_))
    })
}

fn native_boolean_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("boolean?", args, ctx, |value| {
        matches!(value, Value::Boolean(_))
    })
}

fn native_char_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("char?", args, ctx, |value| matches!(value, Value::Char(_)))
}

fn native_pair_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("pair?", args, ctx, |value| matches!(value, Value::Pair(_)))
}

fn native_symbol_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("symbol?", args, ctx, |value| {
        matches!(value, Value::Symbol(_))
    })
}

fn native_apply(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "apply",
            expected: "at least 2",
            got: args.len(),
        });
    }

    let mut expanded = Vec::with_capacity(args.len().saturating_sub(2));
    expanded.extend(args[1..args.len() - 1].iter().cloned());
    expanded.extend(list_to_vec(
        args.last().expect("apply requires a final list"),
        "apply",
    )?);

    apply_procedure(args[0].clone(), &expanded, ctx)
}

fn native_predicate<F>(
    name: &'static str,
    args: &[Value],
    _ctx: &EvalContext,
    predicate: F,
) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn native_add(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for value in values_as_numbers("+", args)? {
        sum += value;
    }
    Ok(Value::Integer(sum))
}

fn native_sub(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let values = values_as_numbers("-", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "-",
        expected: "at least 1",
        got: 0,
    })?;

    let result = if rest.is_empty() {
        -*first
    } else {
        rest.iter().fold(*first, |acc, value| acc - value)
    };

    Ok(Value::Integer(result))
}

fn native_mul(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for value in values_as_numbers("*", args)? {
        product *= value;
    }
    Ok(Value::Integer(product))
}

fn native_div(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let values = values_as_numbers("/", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "/",
        expected: "at least 1",
        got: 0,
    })?;

    if rest.is_empty() {
        if *first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }

    let mut result = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= value;
    }
    Ok(Value::Integer(result))
}

fn native_lt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "<", |left, right| left < right)
}

fn native_gt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, ">", |left, right| left > right)
}

fn native_num_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "=", |left, right| left == right)
}

fn native_lte(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "<=", |left, right| left <= right)
}

fn native_compare<F>(args: &[Value], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = values_as_numbers(name, args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: values.len(),
        });
    }

    for pair in values.windows(2) {
        if !cmp(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn values_as_numbers(name: &'static str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for value in args {
        values.push(value.as_number(name)?);
    }
    Ok(values)
}
