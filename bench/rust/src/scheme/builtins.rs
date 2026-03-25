use std::cell::RefCell;
use std::rc::Rc;
use super::{apply, eqv, make_rational, Environment, Env, Value, EvalError};

/// Extract numerator/denominator pair from a numeric value (exact representation).
fn to_rational(v: &Value) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        Value::Float(_) => Err(EvalError::Type("expected exact number".into())),
        _ => Err(EvalError::Type(format!("expected number, got {}", v.display_value()))),
    }
}

fn num_add(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(a.as_f64()? + b.as_f64()?));
    }
    let (an, ad) = to_rational(a)?;
    let (bn, bd) = to_rational(b)?;
    Ok(make_rational(an * bd + bn * ad, ad * bd))
}

fn num_sub(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(a.as_f64()? - b.as_f64()?));
    }
    let (an, ad) = to_rational(a)?;
    let (bn, bd) = to_rational(b)?;
    Ok(make_rational(an * bd - bn * ad, ad * bd))
}

fn num_mul(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(a.as_f64()? * b.as_f64()?));
    }
    let (an, ad) = to_rational(a)?;
    let (bn, bd) = to_rational(b)?;
    Ok(make_rational(an * bn, ad * bd))
}

fn num_div(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        let bd = b.as_f64()?;
        if bd == 0.0 { return Err(EvalError::DivisionByZero); }
        return Ok(Value::Float(a.as_f64()? / bd));
    }
    let (an, ad) = to_rational(a)?;
    let (bn, bd) = to_rational(b)?;
    if bn == 0 { return Err(EvalError::DivisionByZero); }
    Ok(make_rational(an * bd, ad * bn))
}

fn builtin_add(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut acc = Value::Integer(0);
    for a in args { acc = num_add(&acc, a)?; }
    Ok(acc)
}

fn builtin_sub(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
    if args.len() == 1 {
        return num_sub(&Value::Integer(0), &args[0]);
    }
    let mut acc = args[0].clone();
    for a in &args[1..] { acc = num_sub(&acc, a)?; }
    Ok(acc)
}

fn builtin_mul(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut acc = Value::Integer(1);
    for a in args { acc = num_mul(&acc, a)?; }
    Ok(acc)
}

fn builtin_div(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("/ requires at least 1 argument".into())); }
    if args.len() == 1 {
        return num_div(&Value::Integer(1), &args[0]);
    }
    let mut acc = args[0].clone();
    for a in &args[1..] { acc = num_div(&acc, a)?; }
    Ok(acc)
}

fn args_to_f64s(args: &[Value]) -> Result<Vec<f64>, EvalError> {
    args.iter().map(|a| a.as_f64()).collect()
}

fn builtin_lt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_f64s(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] < w[1])))
}
fn builtin_gt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_f64s(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] > w[1])))
}
fn builtin_eq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_f64s(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] == w[1])))
}
fn builtin_le(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_f64s(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] <= w[1])))
}
fn builtin_ge(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let v = args_to_f64s(args)?;
    Ok(Value::Boolean(v.windows(2).all(|w| w[0] >= w[1])))
}
fn builtin_cons(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("cons requires exactly 2 arguments".into())); }
    match &args[1] {
        Value::List(tail) => {
            let mut new = vec![args[0].clone()];
            new.extend(tail.iter().cloned());
            Ok(Value::List(new))
        }
        _ => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
    }
}

fn builtin_car(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("car requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(a, _) => Ok((**a).clone()),
        _ => Err(EvalError::Type("car: expected non-empty list".into())),
    }
}

fn builtin_cdr(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("cdr requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, b) => Ok((**b).clone()),
        _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
    }
}

fn builtin_list(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_null(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("null? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
}

fn builtin_length(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("length requires exactly 1 argument".into())); }
    match &args[0] {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match a {
            Value::List(items) => result.extend(items.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_display(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); }
    output.push_str(&args[0].display_repr());
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); }
    output.push_str(&args[0].display_value());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if !args.is_empty() { return Err(EvalError::Arity("newline takes no arguments".into())); }
    output.push('\n');
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("substring: expected string".into())),
    };
    let start = args[1].as_integer()? as usize;
    let end = args[2].as_integer()? as usize;
    Ok(Value::Str(s[start..end].to_string()))
}

fn builtin_string_to_number(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => {
            if let Ok(n) = s.parse::<i64>() {
                return Ok(Value::Integer(n));
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Float(f));
            }
            Ok(Value::Boolean(false))
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
    Ok(Value::Str(args[0].display_value()))
}

fn builtin_symbol_to_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-ref: expected string".into())),
    };
    let idx = args[1].as_integer()? as usize;
    Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
        EvalError::Type("string-ref: index out of bounds".into())
    })?))
}

fn builtin_string_copy(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_apply(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = match last {
        Value::List(items) => items.clone(),
        _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
    };
    let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail);
    apply(func, &all_args, output)
}

fn builtin_is_char(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
}

fn builtin_is_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
}
fn builtin_is_number(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].is_number()))
}
fn builtin_is_boolean(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}
fn builtin_is_pair(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
}
fn builtin_is_symbol(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn builtin_not(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("not requires exactly 1 argument".into())); }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

// ---- L09 Builtins: Numeric/Char/String Utilities ----

fn builtin_abs(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
    Ok(Value::Integer(args[0].as_integer()?.abs()))
}

fn builtin_modulo(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(((a % b) + b) % b))
}

fn builtin_remainder(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(a % b))
}

fn builtin_quotient(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(a / b))
}

fn builtin_min(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
    let mut m = args[0].as_integer()?;
    for a in &args[1..] { m = m.min(a.as_integer()?); }
    Ok(Value::Integer(m))
}

fn builtin_max(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
    let mut m = args[0].as_integer()?;
    for a in &args[1..] { m = m.max(a.as_integer()?); }
    Ok(Value::Integer(m))
}

fn builtin_expt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
    let base = args[0].as_integer()?;
    let exp = args[1].as_integer()?;
    Ok(Value::Integer(base.pow(exp as u32)))
}

fn builtin_is_zero(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? == 0))
}

fn builtin_is_positive(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? > 0))
}

fn builtin_is_negative(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? < 0))
}

fn builtin_is_odd(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? % 2 != 0))
}

fn builtin_is_even(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? % 2 == 0))
}

fn builtin_list_ref(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
    let items = match &args[0] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("list-ref: expected list".into())),
    };
    let idx = args[1].as_integer()? as usize;
    items.get(idx).cloned().ok_or_else(|| EvalError::Type("list-ref: index out of bounds".into()))
}

fn builtin_list_tail(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
    let items = match &args[0] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("list-tail: expected list".into())),
    };
    let idx = args[1].as_integer()? as usize;
    if idx > items.len() { return Err(EvalError::Type("list-tail: index out of bounds".into())); }
    Ok(Value::List(items[idx..].to_vec()))
}

fn builtin_is_list(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::List(_))))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Pair(a1, b1), Value::Pair(a2, b2)) => values_equal(a1, a2) && values_equal(b1, b2),
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn builtin_equal(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
    Ok(Value::Boolean(values_equal(&args[0], &args[1])))
}

fn builtin_eq_pred(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn builtin_assoc(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
    let key = &args[0];
    let alist = match &args[1] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("assoc: expected list".into())),
    };
    for entry in alist {
        if let Value::List(pair) = entry {
            if !pair.is_empty() && values_equal(&pair[0], key) {
                return Ok(entry.clone());
            }
        }
    }
    Ok(Value::Boolean(false))
}

fn builtin_map(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
        Value::List(v) => Ok(v),
        _ => Err(EvalError::Type("map: expected list".into())),
    }).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        result.push(apply(func, &call_args, output)?);
    }
    Ok(Value::List(result))
}

fn builtin_char_alphabetic(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
        _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
    }
}

fn builtin_char_numeric(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
        _ => Err(EvalError::Type("char-numeric?: expected char".into())),
    }
}

fn builtin_char_upcase(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
        _ => Err(EvalError::Type("char-upcase: expected char".into())),
    }
}

fn builtin_char_downcase(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
        _ => Err(EvalError::Type("char-downcase: expected char".into())),
    }
}

fn builtin_char_eq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("char=?: expected chars".into())),
    }
}

fn builtin_char_lt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("char<?: expected chars".into())),
    }
}

fn builtin_string_eq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("string=?: expected strings".into())),
    }
}

fn builtin_string_lt(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("string<?: expected strings".into())),
    }
}

fn builtin_string_ci_eq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
        _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
    }
}

fn builtin_string_upcase(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
        _ => Err(EvalError::Type("string-upcase: expected string".into())),
    }
}

fn builtin_string_downcase(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
        _ => Err(EvalError::Type("string-downcase: expected string".into())),
    }
}

// ---- L11: Exact Arithmetic & Rationals ----

fn builtin_is_exact(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].is_exact()))
}

fn builtin_is_inexact(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
}

fn builtin_exact_to_inexact(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into())); }
    Ok(Value::Float(args[0].as_f64()?))
}

fn builtin_inexact_to_exact(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, d) => Ok(Value::Rational(*n, *d)),
        Value::Float(f) => {
            // Convert float to rational using fraction approximation
            // Use the standard approach: multiply by large power of 2, simplify
            let bits = 1_i64 << 53; // f64 mantissa precision
            let n = (*f * bits as f64).round() as i64;
            Ok(make_rational(n, bits))
        }
        _ => Err(EvalError::Type("inexact->exact: expected number".into())),
    }
}

fn builtin_numerator(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, _) => Ok(Value::Integer(*n)),
        _ => Err(EvalError::Type("numerator: expected rational number".into())),
    }
}

fn builtin_denominator(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); }
    match &args[0] {
        Value::Integer(_) => Ok(Value::Integer(1)),
        Value::Rational(_, d) => Ok(Value::Integer(*d)),
        _ => Err(EvalError::Type("denominator: expected rational number".into())),
    }
}

fn builtin_is_integer(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
}

fn builtin_is_rational(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].is_exact()))
}

fn builtin_is_procedure(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("procedure? requires 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0],
        Value::Lambda { .. } | Value::Builtin(_) | Value::CaseLambda { .. }
        | Value::RecordConstructor { .. } | Value::RecordPredicate { .. } | Value::RecordAccessor { .. }
    )))
}

// ---- L14: Vectors, eqv?, for-each, assq, memq ----

fn builtin_vector(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

fn builtin_make_vector(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
    }
    let len = args[0].as_integer()? as usize;
    let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

fn builtin_vector_ref(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let idx = args[1].as_integer()? as usize;
            let items = v.borrow();
            items.get(idx).cloned().ok_or_else(|| EvalError::Type("vector-ref: index out of bounds".into()))
        }
        _ => Err(EvalError::Type("vector-ref: expected vector".into())),
    }
}

fn builtin_vector_set(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let idx = args[1].as_integer()? as usize;
            let mut items = v.borrow_mut();
            if idx >= items.len() {
                return Err(EvalError::Type("vector-set!: index out of bounds".into()));
            }
            items[idx] = args[2].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into())),
    }
}

fn builtin_vector_length(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
        _ => Err(EvalError::Type("vector-length: expected vector".into())),
    }
}

fn builtin_is_vector(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into())); }
    Ok(Value::Boolean(args[0].is_vector()))
}

fn builtin_vector_to_list(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
        _ => Err(EvalError::Type("vector->list: expected vector".into())),
    }
}

fn builtin_list_to_vector(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into())); }
    match &args[0] {
        Value::List(items) => Ok(Value::Vector(Rc::new(RefCell::new(items.clone())))),
        _ => Err(EvalError::Type("list->vector: expected list".into())),
    }
}

fn builtin_eqv(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eqv? requires 2 arguments".into())); }
    Ok(Value::Boolean(eqv(&args[0], &args[1])))
}

fn builtin_for_each(args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
        Value::List(v) => Ok(v),
        _ => Err(EvalError::Type("for-each: expected list".into())),
    }).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    for i in 0..len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        apply(func, &call_args, output)?;
    }
    Ok(Value::Void)
}

fn builtin_assq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assq requires 2 arguments".into())); }
    let key = &args[0];
    let alist = match &args[1] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("assq: expected list".into())),
    };
    for entry in alist {
        if let Value::List(pair) = entry {
            if !pair.is_empty() && eqv(&pair[0], key) {
                return Ok(entry.clone());
            }
        }
    }
    Ok(Value::Boolean(false))
}

fn builtin_memq(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("memq requires 2 arguments".into())); }
    let key = &args[0];
    let list = match &args[1] {
        Value::List(v) => v,
        _ => return Err(EvalError::Type("memq: expected list".into())),
    };
    for (i, item) in list.iter().enumerate() {
        if eqv(item, key) {
            return Ok(Value::List(list[i..].to_vec()));
        }
    }
    Ok(Value::Boolean(false))
}

fn builtin_char_to_integer(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        _ => Err(EvalError::Type("char->integer: expected char".into())),
    }
}

fn builtin_integer_to_char(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); }
    let n = args[0].as_integer()?;
    Ok(Value::Char(char::from_u32(n as u32).ok_or_else(|| EvalError::Type("integer->char: invalid code point".into()))?))
}

fn builtin_string_to_list(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->list requires 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
        _ => Err(EvalError::Type("string->list: expected string".into())),
    }
}

fn builtin_list_to_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->string requires 1 argument".into())); }
    match &args[0] {
        Value::List(items) => {
            let mut s = String::new();
            for item in items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected char in list".into())),
                }
            }
            Ok(Value::Str(s))
        }
        _ => Err(EvalError::Type("list->string: expected list".into())),
    }
}

fn builtin_make_string(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into()));
    }
    let len = args[0].as_integer()? as usize;
    let ch = if args.len() == 2 {
        match &args[1] {
            Value::Char(c) => *c,
            _ => return Err(EvalError::Type("make-string: expected char".into())),
        }
    } else {
        '\0'
    };
    Ok(Value::Str(std::iter::repeat_n(ch, len).collect()))
}

fn builtin_reverse(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("reverse requires 1 argument".into())); }
    match &args[0] {
        Value::List(items) => {
            let mut reversed = items.clone();
            reversed.reverse();
            Ok(Value::List(reversed))
        }
        _ => Err(EvalError::Type("reverse: expected list".into())),
    }
}

fn builtin_gcd(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::Integer(0)); }
    let mut result = args[0].as_integer()?.abs();
    for a in &args[1..] {
        let mut b = a.as_integer()?.abs();
        while b != 0 {
            let t = b;
            b = result % b;
            result = t;
        }
    }
    Ok(Value::Integer(result))
}

fn builtin_lcm(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() { return Ok(Value::Integer(1)); }
    let mut result = args[0].as_integer()?.abs();
    for a in &args[1..] {
        let b = a.as_integer()?.abs();
        if b == 0 { return Ok(Value::Integer(0)); }
        result = result / super::gcd(result, b) * b;
    }
    Ok(Value::Integer(result))
}

fn builtin_error(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    let msg = if args.is_empty() {
        "error".to_string()
    } else {
        args.iter().map(|a| a.display_value()).collect::<Vec<_>>().join(" ")
    };
    Err(EvalError::Type(msg))
}

fn builtin_vector_fill(args: &[Value], _output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-fill! requires 2 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let fill = args[1].clone();
            let mut items = v.borrow_mut();
            for item in items.iter_mut() {
                *item = fill.clone();
            }
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-fill!: expected vector".into())),
    }
}

pub(crate) fn make_global_env() -> Env {
    let env = Environment::new();
    {
        let mut e = env.borrow_mut();
        e.set("+".into(), Value::Builtin(builtin_add));
        e.set("-".into(), Value::Builtin(builtin_sub));
        e.set("*".into(), Value::Builtin(builtin_mul));
        e.set("/".into(), Value::Builtin(builtin_div));
        e.set("<".into(), Value::Builtin(builtin_lt));
        e.set(">".into(), Value::Builtin(builtin_gt));
        e.set("=".into(), Value::Builtin(builtin_eq));
        e.set("<=".into(), Value::Builtin(builtin_le));
        e.set(">=".into(), Value::Builtin(builtin_ge));
        e.set("not".into(), Value::Builtin(builtin_not));
        e.set("cons".into(), Value::Builtin(builtin_cons));
        e.set("car".into(), Value::Builtin(builtin_car));
        e.set("cdr".into(), Value::Builtin(builtin_cdr));
        e.set("list".into(), Value::Builtin(builtin_list));
        e.set("null?".into(), Value::Builtin(builtin_null));
        e.set("length".into(), Value::Builtin(builtin_length));
        e.set("append".into(), Value::Builtin(builtin_append));
        e.set("string?".into(), Value::Builtin(builtin_is_string));
        e.set("number?".into(), Value::Builtin(builtin_is_number));
        e.set("boolean?".into(), Value::Builtin(builtin_is_boolean));
        e.set("pair?".into(), Value::Builtin(builtin_is_pair));
        e.set("symbol?".into(), Value::Builtin(builtin_is_symbol));
        e.set("char?".into(), Value::Builtin(builtin_is_char));
        e.set("display".into(), Value::Builtin(builtin_display));
        e.set("write".into(), Value::Builtin(builtin_write));
        e.set("newline".into(), Value::Builtin(builtin_newline));
        e.set("string-append".into(), Value::Builtin(builtin_string_append));
        e.set("string-length".into(), Value::Builtin(builtin_string_length));
        e.set("substring".into(), Value::Builtin(builtin_substring));
        e.set("string->number".into(), Value::Builtin(builtin_string_to_number));
        e.set("number->string".into(), Value::Builtin(builtin_number_to_string));
        e.set("symbol->string".into(), Value::Builtin(builtin_symbol_to_string));
        e.set("string->symbol".into(), Value::Builtin(builtin_string_to_symbol));
        e.set("string-ref".into(), Value::Builtin(builtin_string_ref));
        e.set("string-copy".into(), Value::Builtin(builtin_string_copy));
        e.set("apply".into(), Value::Builtin(builtin_apply));
        // L09: Numeric utilities
        e.set("abs".into(), Value::Builtin(builtin_abs));
        e.set("modulo".into(), Value::Builtin(builtin_modulo));
        e.set("remainder".into(), Value::Builtin(builtin_remainder));
        e.set("quotient".into(), Value::Builtin(builtin_quotient));
        e.set("min".into(), Value::Builtin(builtin_min));
        e.set("max".into(), Value::Builtin(builtin_max));
        e.set("expt".into(), Value::Builtin(builtin_expt));
        e.set("zero?".into(), Value::Builtin(builtin_is_zero));
        e.set("positive?".into(), Value::Builtin(builtin_is_positive));
        e.set("negative?".into(), Value::Builtin(builtin_is_negative));
        e.set("odd?".into(), Value::Builtin(builtin_is_odd));
        e.set("even?".into(), Value::Builtin(builtin_is_even));
        // L09: List utilities
        e.set("list-ref".into(), Value::Builtin(builtin_list_ref));
        e.set("list-tail".into(), Value::Builtin(builtin_list_tail));
        e.set("list?".into(), Value::Builtin(builtin_is_list));
        e.set("assoc".into(), Value::Builtin(builtin_assoc));
        e.set("map".into(), Value::Builtin(builtin_map));
        e.set("eq?".into(), Value::Builtin(builtin_eq_pred));
        e.set("equal?".into(), Value::Builtin(builtin_equal));
        // L09: Character utilities
        e.set("char-alphabetic?".into(), Value::Builtin(builtin_char_alphabetic));
        e.set("char-numeric?".into(), Value::Builtin(builtin_char_numeric));
        e.set("char-upcase".into(), Value::Builtin(builtin_char_upcase));
        e.set("char-downcase".into(), Value::Builtin(builtin_char_downcase));
        e.set("char=?".into(), Value::Builtin(builtin_char_eq));
        e.set("char<?".into(), Value::Builtin(builtin_char_lt));
        // L09: String comparison/case utilities
        e.set("string=?".into(), Value::Builtin(builtin_string_eq));
        e.set("string<?".into(), Value::Builtin(builtin_string_lt));
        e.set("string-ci=?".into(), Value::Builtin(builtin_string_ci_eq));
        e.set("string-upcase".into(), Value::Builtin(builtin_string_upcase));
        e.set("string-downcase".into(), Value::Builtin(builtin_string_downcase));
        // L11: Exact arithmetic & rationals
        e.set("exact?".into(), Value::Builtin(builtin_is_exact));
        e.set("inexact?".into(), Value::Builtin(builtin_is_inexact));
        e.set("exact->inexact".into(), Value::Builtin(builtin_exact_to_inexact));
        e.set("inexact->exact".into(), Value::Builtin(builtin_inexact_to_exact));
        e.set("numerator".into(), Value::Builtin(builtin_numerator));
        e.set("denominator".into(), Value::Builtin(builtin_denominator));
        e.set("integer?".into(), Value::Builtin(builtin_is_integer));
        e.set("rational?".into(), Value::Builtin(builtin_is_rational));
        // L13: procedure?
        e.set("procedure?".into(), Value::Builtin(builtin_is_procedure));
        // L14: Vectors
        e.set("vector".into(), Value::Builtin(builtin_vector));
        e.set("make-vector".into(), Value::Builtin(builtin_make_vector));
        e.set("vector-ref".into(), Value::Builtin(builtin_vector_ref));
        e.set("vector-set!".into(), Value::Builtin(builtin_vector_set));
        e.set("vector-length".into(), Value::Builtin(builtin_vector_length));
        e.set("vector?".into(), Value::Builtin(builtin_is_vector));
        e.set("vector->list".into(), Value::Builtin(builtin_vector_to_list));
        e.set("list->vector".into(), Value::Builtin(builtin_list_to_vector));
        e.set("vector-fill!".into(), Value::Builtin(builtin_vector_fill));
        // L14: eqv?, for-each, assq, memq
        e.set("eqv?".into(), Value::Builtin(builtin_eqv));
        e.set("for-each".into(), Value::Builtin(builtin_for_each));
        e.set("assq".into(), Value::Builtin(builtin_assq));
        e.set("memq".into(), Value::Builtin(builtin_memq));
        // L14: Additional utilities
        e.set("char->integer".into(), Value::Builtin(builtin_char_to_integer));
        e.set("integer->char".into(), Value::Builtin(builtin_integer_to_char));
        e.set("string->list".into(), Value::Builtin(builtin_string_to_list));
        e.set("list->string".into(), Value::Builtin(builtin_list_to_string));
        e.set("make-string".into(), Value::Builtin(builtin_make_string));
        e.set("reverse".into(), Value::Builtin(builtin_reverse));
        e.set("gcd".into(), Value::Builtin(builtin_gcd));
        e.set("lcm".into(), Value::Builtin(builtin_lcm));
        e.set("error".into(), Value::Builtin(builtin_error));
    }
    env
}
