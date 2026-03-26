use super::{
    apply_procedure, list_from_values, list_to_vec, make_mutable_string, make_pair, make_string,
    make_vector, number::Number, ControlProc, Env, EnvRef, EvalContext, EvalError, NativeFunc,
    Value,
};
use std::collections::HashSet;
use std::rc::Rc;

pub(super) fn default_env() -> EnvRef {
    let env = Env::new(None);
    for (name, func) in [
        ("+", native_add as NativeFunc),
        ("-", native_sub as NativeFunc),
        ("*", native_mul as NativeFunc),
        ("/", native_div as NativeFunc),
        ("abs", native_abs as NativeFunc),
        ("gcd", native_gcd as NativeFunc),
        ("lcm", native_lcm as NativeFunc),
        ("truncate", native_truncate as NativeFunc),
        ("round", native_round as NativeFunc),
        ("quotient", native_quotient as NativeFunc),
        ("remainder", native_remainder as NativeFunc),
        ("modulo", native_modulo as NativeFunc),
        ("min", native_min as NativeFunc),
        ("max", native_max as NativeFunc),
        ("expt", native_expt as NativeFunc),
        ("<", native_lt as NativeFunc),
        (">", native_gt as NativeFunc),
        ("=", native_num_eq as NativeFunc),
        ("<=", native_lte as NativeFunc),
        (">=", native_gte as NativeFunc),
        ("not", native_not as NativeFunc),
        ("zero?", native_zero_pred as NativeFunc),
        ("positive?", native_positive_pred as NativeFunc),
        ("negative?", native_negative_pred as NativeFunc),
        ("odd?", native_odd_pred as NativeFunc),
        ("even?", native_even_pred as NativeFunc),
        ("cons", native_cons as NativeFunc),
        ("car", native_car as NativeFunc),
        ("cdr", native_cdr as NativeFunc),
        ("caar", native_caar as NativeFunc),
        ("cadr", native_cadr as NativeFunc),
        ("cdar", native_cdar as NativeFunc),
        ("cddr", native_cddr as NativeFunc),
        ("set-car!", native_set_car as NativeFunc),
        ("set-cdr!", native_set_cdr as NativeFunc),
        ("null?", native_null_pred as NativeFunc),
        ("list", native_list as NativeFunc),
        ("list?", native_list_pred as NativeFunc),
        ("list-ref", native_list_ref as NativeFunc),
        ("list-tail", native_list_tail as NativeFunc),
        ("length", native_length as NativeFunc),
        ("append", native_append as NativeFunc),
        ("reverse", native_reverse as NativeFunc),
        ("member", native_member as NativeFunc),
        ("assv", native_assv as NativeFunc),
        ("assoc", native_assoc as NativeFunc),
        ("map", native_map as NativeFunc),
        ("for-each", native_for_each as NativeFunc),
        ("eqv?", native_eqv as NativeFunc),
        ("eq?", native_eq as NativeFunc),
        ("equal?", native_equal as NativeFunc),
        ("vector", native_vector as NativeFunc),
        ("make-vector", native_make_vector as NativeFunc),
        ("vector-ref", native_vector_ref as NativeFunc),
        ("vector-set!", native_vector_set as NativeFunc),
        ("vector-length", native_vector_length as NativeFunc),
        ("vector?", native_vector_pred as NativeFunc),
        ("vector->list", native_vector_to_list as NativeFunc),
        ("list->vector", native_list_to_vector as NativeFunc),
        ("display", native_display as NativeFunc),
        ("write", native_write as NativeFunc),
        ("newline", native_newline as NativeFunc),
        ("string-copy", native_string_copy as NativeFunc),
        ("make-string", native_make_string as NativeFunc),
        ("string", native_string as NativeFunc),
        ("string-append", native_string_append as NativeFunc),
        ("string-length", native_string_length as NativeFunc),
        ("string->list", native_string_to_list as NativeFunc),
        ("list->string", native_list_to_string as NativeFunc),
        ("string=?", native_string_eq as NativeFunc),
        ("string<?", native_string_lt as NativeFunc),
        ("string>?", native_string_gt as NativeFunc),
        ("string<=?", native_string_lte as NativeFunc),
        ("string>=?", native_string_gte as NativeFunc),
        ("string-ci=?", native_string_ci_eq as NativeFunc),
        ("string-upcase", native_string_upcase as NativeFunc),
        ("string-downcase", native_string_downcase as NativeFunc),
        ("substring", native_substring as NativeFunc),
        ("string->number", native_string_to_number as NativeFunc),
        ("number->string", native_number_to_string as NativeFunc),
        ("symbol->string", native_symbol_to_string as NativeFunc),
        ("string->symbol", native_string_to_symbol as NativeFunc),
        ("string-ref", native_string_ref as NativeFunc),
        ("string-set!", native_string_set as NativeFunc),
        ("string?", native_string_pred as NativeFunc),
        ("number?", native_number_pred as NativeFunc),
        ("integer?", native_integer_pred as NativeFunc),
        ("rational?", native_rational_pred as NativeFunc),
        ("exact?", native_exact_pred as NativeFunc),
        ("inexact?", native_inexact_pred as NativeFunc),
        ("exact->inexact", native_exact_to_inexact as NativeFunc),
        ("inexact->exact", native_inexact_to_exact as NativeFunc),
        ("numerator", native_numerator as NativeFunc),
        ("denominator", native_denominator as NativeFunc),
        ("boolean?", native_boolean_pred as NativeFunc),
        ("char?", native_char_pred as NativeFunc),
        (
            "char-alphabetic?",
            native_char_alphabetic_pred as NativeFunc,
        ),
        ("char-numeric?", native_char_numeric_pred as NativeFunc),
        ("char-upcase", native_char_upcase as NativeFunc),
        ("char-downcase", native_char_downcase as NativeFunc),
        ("char->integer", native_char_to_integer as NativeFunc),
        ("integer->char", native_integer_to_char as NativeFunc),
        ("char=?", native_char_eq as NativeFunc),
        ("char<?", native_char_lt as NativeFunc),
        ("pair?", native_pair_pred as NativeFunc),
        ("procedure?", native_procedure_pred as NativeFunc),
        ("symbol?", native_symbol_pred as NativeFunc),
        ("apply", native_apply as NativeFunc),
    ] {
        env.define(name.to_string(), Value::NativeProc { name, func });
    }
    env.define(
        "call/cc".to_string(),
        Value::ControlProc(ControlProc::CallCc),
    );
    env.define(
        "call-with-current-continuation".to_string(),
        Value::ControlProc(ControlProc::CallCc),
    );
    env.define(
        "dynamic-wind".to_string(),
        Value::ControlProc(ControlProc::DynamicWind),
    );
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

fn native_caar(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_cxr(args, "caar", b"aa")
}

fn native_cadr(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_cxr(args, "cadr", b"da")
}

fn native_cdar(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_cxr(args, "cdar", b"ad")
}

fn native_cddr(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_cxr(args, "cddr", b"dd")
}

fn native_cxr(args: &[Value], name: &'static str, ops: &[u8]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let mut value = args[0].clone();
    for op in ops {
        let pair = value.as_pair(name)?;
        value = match op {
            b'a' => pair.borrow().car.clone(),
            b'd' => pair.borrow().cdr.clone(),
            _ => unreachable!("invalid composed accessor"),
        };
    }

    Ok(value)
}

fn native_set_car(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set-car!",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("set-car!")?;
    pair.borrow_mut().car = args[1].clone();
    Ok(Value::Void)
}

fn native_set_cdr(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set-cdr!",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("set-cdr!")?;
    pair.borrow_mut().cdr = args[1].clone();
    Ok(Value::Void)
}

fn native_null_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("null?", args, ctx, |value| matches!(value, Value::Nil))
}

fn native_list(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    Ok(list_from_values(args.to_vec()))
}

fn native_list_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "list?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(is_proper_list(&args[0])))
}

fn native_list_ref(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let items = list_to_vec(&args[0], "list-ref")?;
    let index = args[1].as_integer("list-ref")?;
    let Some(index) = usize::try_from(index)
        .ok()
        .filter(|index| *index < items.len())
    else {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-ref",
            index,
            len: items.len(),
        });
    };

    Ok(items[index].clone())
}

fn native_list_tail(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "list-tail",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let index = args[1].as_integer("list-tail")?;
    let Ok(index) = usize::try_from(index) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "list-tail",
            index,
            len: 0,
        });
    };

    let mut cursor = args[0].clone();
    let mut advanced = 0;
    while advanced < index {
        match cursor {
            Value::Pair(pair) => {
                cursor = pair.borrow().cdr.clone();
                advanced += 1;
            }
            Value::Nil => {
                return Err(EvalError::IndexOutOfBounds {
                    name: "list-tail",
                    index: index as i64,
                    len: advanced,
                });
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name: "list-tail",
                    found: other.render(),
                });
            }
        }
    }

    if !is_proper_list(&cursor) {
        return Err(EvalError::ExpectedList {
            name: "list-tail",
            found: cursor.render(),
        });
    }

    Ok(cursor)
}

fn native_length(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(list_to_vec(&args[0], "length")?.len() as i64))
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

fn native_reverse(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "reverse",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let mut items = list_to_vec(&args[0], "reverse")?;
    items.reverse();
    Ok(list_from_values(items))
}

fn native_member(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "member",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let needle = &args[0];
    let mut cursor = args[1].clone();
    loop {
        match cursor.clone() {
            Value::Nil => return Ok(Value::Boolean(false)),
            Value::Pair(pair) => {
                let (item, next) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                if value_equal(needle, &item) {
                    return Ok(cursor);
                }
                cursor = next;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name: "member",
                    found: other.render(),
                });
            }
        }
    }
}

fn native_assv(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "assv",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let needle = &args[0];
    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Boolean(false)),
            Value::Pair(pair) => {
                let (entry, next) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                let entry_pair = entry.as_pair("assv")?;
                let entry_key = entry_pair.borrow().car.clone();
                if value_eq(needle, &entry_key) {
                    return Ok(entry);
                }
                cursor = next;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name: "assv",
                    found: other.render(),
                });
            }
        }
    }
}

fn native_assoc(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "assoc",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let needle = &args[0];
    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Boolean(false)),
            Value::Pair(pair) => {
                let (entry, next) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                let entry_pair = entry.as_pair("assoc")?;
                let entry_key = entry_pair.borrow().car.clone();
                if value_equal(needle, &entry_key) {
                    return Ok(entry);
                }
                cursor = next;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name: "assoc",
                    found: other.render(),
                });
            }
        }
    }
}

fn native_map(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "map",
            expected: "at least 2",
            got: args.len(),
        });
    }

    let procedure = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| list_to_vec(value, "map"))
        .collect::<Result<Vec<_>, _>>()?;
    let Some(len) = lists.iter().map(Vec::len).min() else {
        return Ok(Value::Nil);
    };

    let mut results = Vec::with_capacity(len);
    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        results.push(apply_procedure(procedure.clone(), &call_args, ctx)?);
    }

    Ok(list_from_values(results))
}

fn native_for_each(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "for-each",
            expected: "at least 2",
            got: args.len(),
        });
    }

    let procedure = args[0].clone();
    let lists = args[1..]
        .iter()
        .map(|value| list_to_vec(value, "for-each"))
        .collect::<Result<Vec<_>, _>>()?;
    let Some(len) = lists.iter().map(Vec::len).min() else {
        return Ok(Value::Void);
    };

    for index in 0..len {
        let call_args = lists
            .iter()
            .map(|list| list[index].clone())
            .collect::<Vec<_>>();
        apply_procedure(procedure.clone(), &call_args, ctx)?;
    }

    Ok(Value::Void)
}

fn native_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "eq?",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(value_eq(&args[0], &args[1])))
}

fn native_eqv(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "eqv?",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(value_eq(&args[0], &args[1])))
}

fn native_equal(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "equal?",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(value_equal(&args[0], &args[1])))
}

fn native_vector(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    Ok(make_vector(args.to_vec()))
}

fn native_make_vector(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "make-vector",
            expected: "1 or 2",
            got: args.len(),
        });
    }

    let len = args[0].as_integer("make-vector")?;
    let Ok(len) = usize::try_from(len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "make-vector",
            index: len,
            len: 0,
        });
    };
    let fill = args.get(1).cloned().unwrap_or(Value::Void);
    Ok(make_vector(vec![fill; len]))
}

fn native_vector_ref(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "vector-ref",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let vector = args[0].as_vector("vector-ref")?;
    let index = args[1].as_integer("vector-ref")?;
    let vector = vector.borrow();
    let len = vector.len();
    let Some(index) = usize::try_from(index).ok().filter(|index| *index < len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "vector-ref",
            index,
            len,
        });
    };

    Ok(vector[index].clone())
}

fn native_vector_set(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "vector-set!",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    let vector = args[0].as_vector("vector-set!")?;
    let index = args[1].as_integer("vector-set!")?;
    let mut vector = vector.borrow_mut();
    let len = vector.len();
    let Some(index) = usize::try_from(index).ok().filter(|index| *index < len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "vector-set!",
            index,
            len,
        });
    };

    vector[index] = args[2].clone();
    Ok(Value::Void)
}

fn native_vector_length(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "vector-length",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(
        args[0].as_vector("vector-length")?.borrow().len() as i64,
    ))
}

fn native_vector_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("vector?", args, ctx, |value| {
        matches!(value, Value::Vector(_))
    })
}

fn native_vector_to_list(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "vector->list",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(list_from_values(
        args[0].as_vector("vector->list")?.borrow().clone(),
    ))
}

fn native_list_to_vector(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "list->vector",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_vector(list_to_vec(&args[0], "list->vector")?))
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

    Ok(make_mutable_string(args[0].as_string("string-copy")?))
}

fn native_make_string(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if !(1..=2).contains(&args.len()) {
        return Err(EvalError::WrongArgCount {
            name: "make-string",
            expected: "1 or 2",
            got: args.len(),
        });
    }

    let len = args[0].as_integer("make-string")?;
    let Ok(len) = usize::try_from(len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "make-string",
            index: len,
            len: 0,
        });
    };
    let fill = match args.get(1) {
        Some(value) => value.as_char("make-string")?,
        None => '\0',
    };

    let mut string = String::with_capacity(len);
    for _ in 0..len {
        string.push(fill);
    }
    Ok(make_string(string))
}

fn native_string(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut string = String::with_capacity(args.len());
    for value in args {
        string.push(value.as_char("string")?);
    }
    Ok(make_string(string))
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

    Ok(exact_integer(
        args[0].as_string("string-length")?.chars().count() as i64,
    ))
}

fn native_string_to_list(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string->list",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(list_from_values(
        args[0]
            .as_string("string->list")?
            .chars()
            .map(Value::Char)
            .collect(),
    ))
}

fn native_list_to_string(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "list->string",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let chars = list_to_vec(&args[0], "list->string")?
        .into_iter()
        .map(|value| value.as_char("list->string"))
        .collect::<Result<Vec<_>, _>>()?;
    let string = chars.into_iter().collect::<String>();
    Ok(make_string(string))
}

fn native_string_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string=?", |left, right| left == right)
}

fn native_string_lt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string<?", |left, right| left < right)
}

fn native_string_gt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string>?", |left, right| left > right)
}

fn native_string_lte(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string<=?", |left, right| left <= right)
}

fn native_string_gte(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string>=?", |left, right| left >= right)
}

fn native_string_ci_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_strings(args, "string-ci=?", |left, right| {
        left.to_lowercase() == right.to_lowercase()
    })
}

fn native_string_upcase(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-upcase",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_string(
        args[0].as_string("string-upcase")?.to_uppercase(),
    ))
}

fn native_string_downcase(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "string-downcase",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(make_string(
        args[0].as_string("string-downcase")?.to_lowercase(),
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
    let index = args[1].as_integer("string-set!")?;
    let value = args[2].as_char("string-set!")?;
    let len = string.chars.borrow().len();
    let Some(index) = usize::try_from(index).ok().filter(|index| *index < len) else {
        return Err(EvalError::IndexOutOfBounds {
            name: "string-set!",
            index,
            len,
        });
    };

    if !string.mutable {
        return Err(EvalError::ImmutableString {
            name: "string-set!",
        });
    }

    string.chars.borrow_mut()[index] = value;
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
    let start = args[1].as_integer("substring")?;
    let end = args[2].as_integer("substring")?;
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
    match Number::parse_literal(&string)? {
        Some(value) => Ok(Value::Number(value)),
        None => Ok(Value::Boolean(false)),
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

    Ok(make_string(args[0].as_number("number->string")?.render()))
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
    let index = args[1].as_integer("string-ref")?;
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
        matches!(value, Value::Number(_))
    })
}

fn native_integer_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate(
        "integer?",
        args,
        ctx,
        |value| matches!(value, Value::Number(number) if number.is_integer()),
    )
}

fn native_rational_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate(
        "rational?",
        args,
        ctx,
        |value| matches!(value, Value::Number(number) if number.is_rational()),
    )
}

fn native_exact_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate(
        "exact?",
        args,
        ctx,
        |value| matches!(value, Value::Number(number) if number.is_exact()),
    )
}

fn native_inexact_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate(
        "inexact?",
        args,
        ctx,
        |value| matches!(value, Value::Number(number) if number.is_inexact()),
    )
}

fn native_exact_to_inexact(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "exact->inexact",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Number(
        args[0].as_number("exact->inexact")?.exact_to_inexact(),
    ))
}

fn native_inexact_to_exact(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "inexact->exact",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Number(
        args[0].as_number("inexact->exact")?.inexact_to_exact()?,
    ))
}

fn native_numerator(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "numerator",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(args[0].as_number("numerator")?.numerator()?))
}

fn native_denominator(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "denominator",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(
        args[0].as_number("denominator")?.denominator()?,
    ))
}

fn native_boolean_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("boolean?", args, ctx, |value| {
        matches!(value, Value::Boolean(_))
    })
}

fn native_char_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("char?", args, ctx, |value| matches!(value, Value::Char(_)))
}

fn native_char_alphabetic_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-alphabetic?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(
        args[0].as_char("char-alphabetic?")?.is_alphabetic(),
    ))
}

fn native_char_numeric_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-numeric?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(
        args[0].as_char("char-numeric?")?.is_numeric(),
    ))
}

fn native_char_upcase(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-upcase",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Char(
        args[0].as_char("char-upcase")?.to_ascii_uppercase(),
    ))
}

fn native_char_downcase(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char-downcase",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Char(
        args[0].as_char("char-downcase")?.to_ascii_lowercase(),
    ))
}

fn native_char_to_integer(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "char->integer",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(args[0].as_char("char->integer")? as i64))
}

fn native_integer_to_char(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "integer->char",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let value = args[0].as_integer("integer->char")?;
    let Ok(value) = u32::try_from(value) else {
        return Err(EvalError::InvalidCharCode {
            name: "integer->char",
            value,
        });
    };
    let Some(ch) = char::from_u32(value) else {
        return Err(EvalError::InvalidCharCode {
            name: "integer->char",
            value: i64::from(value),
        });
    };
    Ok(Value::Char(ch))
}

fn native_char_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_chars(args, "char=?", |left, right| left == right)
}

fn native_char_lt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare_chars(args, "char<?", |left, right| left < right)
}

fn native_pair_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("pair?", args, ctx, |value| matches!(value, Value::Pair(_)))
}

fn native_procedure_pred(args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
    native_predicate("procedure?", args, ctx, |value| {
        matches!(
            value,
            Value::ControlProc(_)
                | Value::NativeProc { .. }
                | Value::RecordProc(_)
                | Value::Closure(_)
                | Value::Continuation(_)
        )
    })
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
    let mut sum = Number::integer(0);
    for value in values_as_numbers("+", args)? {
        sum = sum.add(value)?;
    }
    Ok(Value::Number(sum))
}

fn native_abs(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "abs",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let value = args[0].as_number("abs")?;
    let value = if value.compare(Number::integer(0)).is_lt() {
        value.neg()?
    } else {
        value
    };
    Ok(Value::Number(value))
}

fn native_gcd(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut result = 0_i128;
    for value in args {
        result = gcd_i128(result, i128::from(value.as_integer("gcd")?));
    }
    Ok(exact_integer(i128_to_i64(result, "gcd")?))
}

fn native_lcm(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut result = 1_i128;
    for value in args {
        let value = i128::from(value.as_integer("lcm")?);
        if result == 0 || value == 0 {
            result = 0;
            continue;
        }

        let gcd = gcd_i128(result, value);
        result = (result / gcd)
            .checked_mul(value)
            .ok_or_else(|| overflow_error("lcm"))?
            .abs();
    }

    Ok(exact_integer(i128_to_i64(result, "lcm")?))
}

fn native_truncate(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "truncate",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(exact_integer(truncate_number(
        args[0].as_number("truncate")?,
        "truncate",
    )?))
}

fn native_round(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "round",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let number = args[0].as_number("round")?;
    let rounded = match number {
        Number::Exact(_) => match number.exact_to_inexact() {
            Number::Inexact(value) => float_to_i64(value.round(), "round")?,
            Number::Exact(_) => unreachable!("exact->inexact should yield an inexact number"),
        },
        Number::Inexact(value) => float_to_i64(value.round(), "round")?,
    };
    Ok(exact_integer(rounded))
}

fn native_sub(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let values = values_as_numbers("-", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "-",
        expected: "at least 1",
        got: 0,
    })?;

    let result = if rest.is_empty() {
        first.neg()?
    } else {
        let mut result = *first;
        for value in rest {
            result = result.sub(*value)?;
        }
        result
    };

    Ok(Value::Number(result))
}

fn native_mul(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut product = Number::integer(1);
    for value in values_as_numbers("*", args)? {
        product = product.mul(value)?;
    }
    Ok(Value::Number(product))
}

fn native_div(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let values = values_as_numbers("/", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "/",
        expected: "at least 1",
        got: 0,
    })?;

    if rest.is_empty() {
        return Ok(Value::Number(Number::integer(1).div(*first)?));
    }

    let mut result = *first;
    for value in rest {
        result = result.div(*value)?;
    }
    Ok(Value::Number(result))
}

fn native_quotient(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "quotient",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let dividend = args[0].as_integer("quotient")?;
    let divisor = args[1].as_integer("quotient")?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let quotient = dividend
        .checked_div(divisor)
        .ok_or_else(|| overflow_error("quotient"))?;
    Ok(exact_integer(quotient))
}

fn native_remainder(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "remainder",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let dividend = args[0].as_integer("remainder")?;
    let divisor = args[1].as_integer("remainder")?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let remainder = dividend
        .checked_rem(divisor)
        .ok_or_else(|| overflow_error("remainder"))?;
    Ok(exact_integer(remainder))
}

fn native_modulo(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "modulo",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let dividend = args[0].as_integer("modulo")?;
    let divisor = args[1].as_integer("modulo")?;
    if divisor == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let remainder = dividend
        .checked_rem(divisor)
        .ok_or_else(|| overflow_error("modulo"))?;
    let result = if remainder != 0 && (remainder > 0) != (divisor > 0) {
        remainder
            .checked_add(divisor)
            .ok_or_else(|| overflow_error("modulo"))?
    } else {
        remainder
    };

    Ok(exact_integer(result))
}

fn native_min(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut values = values_as_numbers("min", args)?.into_iter();
    let Some(mut best) = values.next() else {
        return Err(EvalError::WrongArgCount {
            name: "min",
            expected: "at least 1",
            got: 0,
        });
    };
    for value in values {
        if value.compare(best).is_lt() {
            best = value;
        }
    }
    Ok(Value::Number(best))
}

fn native_max(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut values = values_as_numbers("max", args)?.into_iter();
    let Some(mut best) = values.next() else {
        return Err(EvalError::WrongArgCount {
            name: "max",
            expected: "at least 1",
            got: 0,
        });
    };
    for value in values {
        if value.compare(best).is_gt() {
            best = value;
        }
    }
    Ok(Value::Number(best))
}

fn native_expt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "expt",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let mut base = args[0].as_integer("expt")?;
    let exponent = args[1].as_integer("expt")?;
    if exponent < 0 {
        return Err(EvalError::InvalidSyntax {
            message: format!("expt: expected a non-negative exponent, got {exponent}"),
        });
    }

    let mut exponent = exponent as u64;
    let mut result = 1_i64;
    while exponent > 0 {
        if exponent & 1 == 1 {
            result = result
                .checked_mul(base)
                .ok_or_else(|| overflow_error("expt"))?;
        }
        exponent >>= 1;
        if exponent > 0 {
            base = base
                .checked_mul(base)
                .ok_or_else(|| overflow_error("expt"))?;
        }
    }

    Ok(exact_integer(result))
}

fn native_lt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "<", |left, right| left.compare(right).is_lt())
}

fn native_gt(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, ">", |left, right| left.compare(right).is_gt())
}

fn native_num_eq(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "=", |left, right| left.numeric_eq(right))
}

fn native_lte(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, "<=", |left, right| !left.compare(right).is_gt())
}

fn native_gte(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    native_compare(args, ">=", |left, right| !left.compare(right).is_lt())
}

fn native_zero_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "zero?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(args[0].as_number("zero?")?.is_zero()))
}

fn native_positive_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "positive?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(
        args[0]
            .as_number("positive?")?
            .compare(Number::integer(0))
            .is_gt(),
    ))
}

fn native_negative_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "negative?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(
        args[0]
            .as_number("negative?")?
            .compare(Number::integer(0))
            .is_lt(),
    ))
}

fn native_odd_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "odd?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(args[0].as_integer("odd?")? % 2 != 0))
}

fn native_even_pred(args: &[Value], _ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "even?",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(args[0].as_integer("even?")? % 2 == 0))
}

fn native_compare<F>(args: &[Value], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(Number, Number) -> bool,
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

fn native_compare_chars<F>(args: &[Value], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(char, char) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let chars = args
        .iter()
        .map(|value| value.as_char(name))
        .collect::<Result<Vec<_>, _>>()?;
    for pair in chars.windows(2) {
        if !cmp(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn native_compare_strings<F>(args: &[Value], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(&str, &str) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let strings = args
        .iter()
        .map(|value| value.as_string(name))
        .collect::<Result<Vec<_>, _>>()?;
    for pair in strings.windows(2) {
        if !cmp(pair[0].as_str(), pair[1].as_str()) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn is_proper_list(value: &Value) -> bool {
    let mut seen = HashSet::new();
    let mut cursor = value.clone();

    loop {
        match cursor {
            Value::Nil => return true,
            Value::Pair(pair) => {
                if !seen.insert(Rc::as_ptr(&pair) as usize) {
                    return false;
                }
                cursor = pair.borrow().cdr.clone();
            }
            _ => return false,
        }
    }
}

pub(super) fn value_eq(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Nil, Value::Nil) => true,
        (Value::String(left), Value::String(right)) => Rc::ptr_eq(left, right),
        (Value::Vector(left), Value::Vector(right)) => Rc::ptr_eq(left, right),
        (Value::Pair(left), Value::Pair(right)) => Rc::ptr_eq(left, right),
        (Value::Record(left), Value::Record(right)) => Rc::ptr_eq(left, right),
        (Value::ControlProc(left), Value::ControlProc(right)) => left == right,
        (Value::NativeProc { name: left, .. }, Value::NativeProc { name: right, .. }) => {
            left == right
        }
        (Value::RecordProc(left), Value::RecordProc(right)) => Rc::ptr_eq(left, right),
        (Value::Closure(left), Value::Closure(right)) => Rc::ptr_eq(left, right),
        (Value::Continuation(left), Value::Continuation(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn value_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.numeric_eq(*right),
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::Char(left), Value::Char(right)) => left == right,
        (Value::String(left), Value::String(right)) => {
            *left.chars.borrow() == *right.chars.borrow()
        }
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Nil, Value::Nil) => true,
        (Value::Vector(left), Value::Vector(right)) => {
            let left = left.borrow();
            let right = right.borrow();
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| value_equal(left, right))
        }
        (Value::Pair(left), Value::Pair(right)) => {
            let (left_car, left_cdr) = {
                let borrowed = left.borrow();
                (borrowed.car.clone(), borrowed.cdr.clone())
            };
            let (right_car, right_cdr) = {
                let borrowed = right.borrow();
                (borrowed.car.clone(), borrowed.cdr.clone())
            };
            value_equal(&left_car, &right_car) && value_equal(&left_cdr, &right_cdr)
        }
        _ => value_eq(left, right),
    }
}

fn overflow_error(name: &'static str) -> EvalError {
    EvalError::InvalidSyntax {
        message: format!("{name}: integer overflow"),
    }
}

fn i128_to_i64(value: i128, name: &'static str) -> Result<i64, EvalError> {
    i64::try_from(value).map_err(|_| overflow_error(name))
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    left = left.abs();
    right = right.abs();
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left
}

fn float_to_i64(value: f64, name: &'static str) -> Result<i64, EvalError> {
    if !value.is_finite() || value < i64::MIN as f64 || value > i64::MAX as f64 {
        return Err(overflow_error(name));
    }

    Ok(value as i64)
}

fn truncate_number(value: Number, name: &'static str) -> Result<i64, EvalError> {
    match value {
        Number::Exact(_) => {
            let numer = i128::from(value.numerator()?);
            let denom = i128::from(value.denominator()?);
            i128_to_i64(numer / denom, name)
        }
        Number::Inexact(value) => float_to_i64(value.trunc(), name),
    }
}

fn exact_integer(value: i64) -> Value {
    Value::Number(Number::integer(value))
}

fn values_as_numbers(name: &'static str, args: &[Value]) -> Result<Vec<Number>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for value in args {
        values.push(value.as_number(name)?);
    }
    Ok(values)
}
