use super::{
    apply_proc, display_value, expect_integer, f64_to_exact, is_number, is_truthy, make_rational,
    make_str, nums_equal, nums_less, value_to_f64, values_equal, EvalError, Value,
};

pub fn eval_builtin(op: &str, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match op {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "abs" | "modulo" | "remainder"
        | "quotient" | "min" | "max" | "expt" | "zero?" | "positive?" | "negative?" | "odd?"
        | "even?" => eval_arithmetic(op, args),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "list-ref"
        | "list-tail" | "list?" | "assoc" => eval_list(op, args),

        "map" | "apply" => eval_higher_order(op, args, output),

        "not" | "eq?" | "equal?" | "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
        | "char?" | "integer?" | "rational?" | "exact?" | "inexact?"
        | "procedure?" => eval_predicate(op, args),

        "exact->inexact" | "inexact->exact" | "numerator" | "denominator" => {
            eval_number_conversion(op, args)
        }

        "display" | "write" | "newline" => eval_io(op, args, output),

        "string-append" | "string-length" | "substring" | "string->number" | "number->string"
        | "symbol->string" | "string->symbol" | "string-ref" | "string-copy" | "string-set!"
        | "string=?" | "string<?" | "string-ci=?" | "string-upcase"
        | "string-downcase" => eval_string(op, args),

        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?"
        | "char<?" => eval_char(op, args),

        _ if op.starts_with("##record-ctor##") => {
            let tag = &op["##record-ctor##".len()..];
            let fields: Vec<String> = args.iter().enumerate().map(|(i, _)| format!("f{i}")).collect();
            // We just store them positionally
            Ok(Value::Record {
                type_tag: tag.to_string(),
                fields: fields.into_iter().zip(args.iter().cloned()).collect(),
            })
        }
        _ if op.starts_with("##record-pred##") => {
            let tag = &op["##record-pred##".len()..];
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_tag, .. } if type_tag == tag)))
        }
        _ if op.starts_with("##record-acc##") => {
            let rest = &op["##record-acc##".len()..];
            let parts: Vec<&str> = rest.splitn(2, "##").collect();
            if parts.len() != 2 {
                return Err(EvalError::Generic("invalid record accessor".into()));
            }
            let tag = parts[0];
            let idx: usize = parts[1].parse().map_err(|_| EvalError::Generic("invalid field index".into()))?;
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor requires 1 argument".into()));
            }
            match &args[0] {
                Value::Record { type_tag, fields } if type_tag == tag => {
                    fields.get(idx).map(|(_, v)| v.clone())
                        .ok_or_else(|| EvalError::Generic("record field index out of bounds".into()))
                }
                _ => Err(EvalError::Type(format!("expected record of type {tag}"))),
            }
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

// Helper: extract (numerator, denominator) from a numeric Value
fn to_rational_parts(v: &Value) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        _ => Err(EvalError::Type(format!("expected exact number, got {v}"))),
    }
}

fn num_add(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(value_to_f64(a)? + value_to_f64(b)?));
    }
    let (n1, d1) = to_rational_parts(a)?;
    let (n2, d2) = to_rational_parts(b)?;
    Ok(make_rational(n1 * d2 + n2 * d1, d1 * d2))
}

fn num_sub(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(value_to_f64(a)? - value_to_f64(b)?));
    }
    let (n1, d1) = to_rational_parts(a)?;
    let (n2, d2) = to_rational_parts(b)?;
    Ok(make_rational(n1 * d2 - n2 * d1, d1 * d2))
}

fn num_mul(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        return Ok(Value::Float(value_to_f64(a)? * value_to_f64(b)?));
    }
    let (n1, d1) = to_rational_parts(a)?;
    let (n2, d2) = to_rational_parts(b)?;
    Ok(make_rational(n1 * n2, d1 * d2))
}

fn num_div(a: &Value, b: &Value) -> Result<Value, EvalError> {
    if matches!(a, Value::Float(_)) || matches!(b, Value::Float(_)) {
        let db = value_to_f64(b)?;
        if db == 0.0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Float(value_to_f64(a)? / db));
    }
    let (n1, d1) = to_rational_parts(a)?;
    let (n2, d2) = to_rational_parts(b)?;
    if n2 == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(make_rational(n1 * d2, d1 * n2))
}

fn eval_arithmetic(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut result = Value::Integer(0);
            for a in args {
                result = num_add(&result, a)?;
            }
            Ok(result)
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return num_sub(&Value::Integer(0), &args[0]);
            }
            let mut result = args[0].clone();
            for a in &args[1..] {
                result = num_sub(&result, a)?;
            }
            Ok(result)
        }
        "*" => {
            let mut result = Value::Integer(1);
            for a in args {
                result = num_mul(&result, a)?;
            }
            Ok(result)
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return num_div(&Value::Integer(1), &args[0]);
            }
            let mut result = args[0].clone();
            for a in &args[1..] {
                result = num_div(&result, a)?;
            }
            Ok(result)
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{op} requires 2 arguments")));
            }
            let result = match op {
                "=" => nums_equal(&args[0], &args[1]),
                "<" => nums_less(&args[0], &args[1])?,
                ">" => nums_less(&args[1], &args[0])?,
                "<=" => !nums_less(&args[1], &args[0])?,
                ">=" => !nums_less(&args[0], &args[1])?,
                _ => unreachable!(),
            };
            Ok(Value::Boolean(result))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("abs requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "abs")?;
            Ok(Value::Integer(n.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("modulo requires 2 arguments".into()));
            }
            let a = expect_integer(&args[0], "modulo")?;
            let b = expect_integer(&args[1], "modulo")?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("remainder requires 2 arguments".into()));
            }
            let a = expect_integer(&args[0], "remainder")?;
            let b = expect_integer(&args[1], "remainder")?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("quotient requires 2 arguments".into()));
            }
            let a = expect_integer(&args[0], "quotient")?;
            let b = expect_integer(&args[1], "quotient")?;
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity("min requires at least 1 argument".into()));
            }
            let mut result = expect_integer(&args[0], "min")?;
            for a in &args[1..] {
                let n = expect_integer(a, "min")?;
                if n < result {
                    result = n;
                }
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity("max requires at least 1 argument".into()));
            }
            let mut result = expect_integer(&args[0], "max")?;
            for a in &args[1..] {
                let n = expect_integer(a, "max")?;
                if n > result {
                    result = n;
                }
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("expt requires 2 arguments".into()));
            }
            let base = expect_integer(&args[0], "expt")?;
            let exp = expect_integer(&args[1], "expt")?;
            if exp < 0 {
                return Ok(Value::Integer(0));
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("zero? requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "zero?")?;
            Ok(Value::Boolean(n == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("positive? requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "positive?")?;
            Ok(Value::Boolean(n > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("negative? requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "negative?")?;
            Ok(Value::Boolean(n < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("odd? requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "odd?")?;
            Ok(Value::Boolean(n % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("even? requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "even?")?;
            Ok(Value::Boolean(n % 2 == 0))
        }
        _ => unreachable!(),
    }
}

fn eval_list(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Ok(Value::Pair(
                    Box::new(args[0].clone()),
                    Box::new(args[1].clone()),
                )),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(a, _) => Ok(*a.clone()),
                _ => Err(EvalError::Type("car: expected non-empty list".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(_, b) => Ok(*b.clone()),
                _ => Err(EvalError::Type("cdr: expected non-empty list".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(items) if items.is_empty()
            )))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type("length: expected list".into())),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, a) in args.iter().enumerate() {
                if i == args.len() - 1 {
                    match a {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => result.push(a.clone()),
                    }
                } else {
                    match a {
                        Value::List(items) => result.extend(items.iter().cloned()),
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
            }
            Ok(Value::List(result))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-ref requires 2 arguments".into()));
            }
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-ref: expected list".into())),
            };
            let idx = expect_integer(&args[1], "list-ref")? as usize;
            if idx >= items.len() {
                return Err(EvalError::Generic("list-ref: index out of range".into()));
            }
            Ok(items[idx].clone())
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-tail requires 2 arguments".into()));
            }
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-tail: expected list".into())),
            };
            let idx = expect_integer(&args[1], "list-tail")? as usize;
            if idx > items.len() {
                return Err(EvalError::Generic(
                    "list-tail: index out of range".into(),
                ));
            }
            Ok(Value::List(items[idx..].to_vec()))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("assoc requires 2 arguments".into()));
            }
            let key = &args[0];
            let alist = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("assoc: expected list".into())),
            };
            for item in alist {
                if let Value::List(pair) = item {
                    if !pair.is_empty() && values_equal(key, &pair[0]) {
                        return Ok(item.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => unreachable!(),
    }
}

fn eval_higher_order(
    op: &str,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    match op {
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(
                    "map requires at least 2 arguments".into(),
                ));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..]
                .iter()
                .map(|a| match a {
                    Value::List(items) => Ok(items),
                    _ => Err(EvalError::Type("map: expected list".into())),
                })
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_proc(func, &call_args, output)?);
            }
            Ok(Value::List(result))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(
                    "apply requires at least 2 arguments".into(),
                ));
            }
            let func = &args[0];
            let last = match &args[args.len() - 1] {
                Value::List(items) => items.clone(),
                _ => {
                    return Err(EvalError::Type(
                        "apply: last argument must be a list".into(),
                    ))
                }
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(last);
            apply_proc(func, &all_args, output)
        }
        _ => unreachable!(),
    }
}

fn eval_predicate(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("eq? requires 2 arguments".into()));
            }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("equal? requires 2 arguments".into()));
            }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number? requires 1 argument".into()));
            }
            Ok(Value::Boolean(is_number(&args[0])))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("integer? requires 1 argument".into()));
            }
            let result = match &args[0] {
                Value::Integer(_) => true,
                Value::Rational(_, _) => false, // always simplified, so den != 1
                Value::Float(f) => *f == f.floor() && f.is_finite(),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("rational? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Rational(_, _)
            )))
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("exact? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Rational(_, _)
            )))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("inexact? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("boolean? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("pair? requires 1 argument".into()));
            }
            Ok(Value::Boolean(
                matches!(&args[0], Value::List(items) if !items.is_empty())
                    || matches!(&args[0], Value::Pair(_, _)),
            ))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("procedure? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Procedure(..) | Value::Builtin(_) | Value::CaseLambda(_)
            )))
        }
        _ => unreachable!(),
    }
}

fn eval_io(op: &str, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match op {
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("display requires 1 argument".into()));
            }
            output.push_str(&display_value(&args[0]));
            Ok(Value::Boolean(false))
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("write requires 1 argument".into()));
            }
            output.push_str(&args[0].to_string());
            Ok(Value::Boolean(false))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity("newline requires 0 arguments".into()));
            }
            output.push('\n');
            Ok(Value::Boolean(false))
        }
        _ => unreachable!(),
    }
}

fn eval_string(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(&s.borrow()),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(make_str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string-length requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.borrow().len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s.borrow().clone(),
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = expect_integer(&args[1], "substring")? as usize;
            let end = expect_integer(&args[2], "substring")? as usize;
            Ok(make_str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string->number requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s) => match s.borrow().parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "number->string requires 1 argument".into(),
                ));
            }
            let n = expect_integer(&args[0], "number->string")?;
            Ok(make_str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "symbol->string requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(make_str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string->symbol requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.borrow().clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s.borrow().clone(),
                _ => return Err(EvalError::Type("string-ref: expected string".into())),
            };
            let idx = expect_integer(&args[1], "string-ref")? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Generic(
                    "string-ref: index out of range".into(),
                )),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-copy requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(make_str(s.borrow().clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(
                    "string-set! requires 3 arguments".into(),
                ));
            }
            let s = match &args[0] {
                Value::Str(s) => s.clone(),
                _ => return Err(EvalError::Type("string-set!: expected string".into())),
            };
            let idx = expect_integer(&args[1], "string-set!")? as usize;
            let c = match &args[2] {
                Value::Char(c) => *c,
                _ => return Err(EvalError::Type("string-set!: expected char".into())),
            };
            let mut borrowed = s.borrow_mut();
            let mut chars: Vec<char> = borrowed.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Generic(
                    "string-set!: index out of range".into(),
                ));
            }
            chars[idx] = c;
            *borrowed = chars.into_iter().collect();
            Ok(Value::Boolean(false))
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    Ok(Value::Boolean(*a.borrow() == *b.borrow()))
                }
                _ => Err(EvalError::Type("string=?: expected strings".into())),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string<? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    Ok(Value::Boolean(*a.borrow() < *b.borrow()))
                }
                _ => Err(EvalError::Type("string<?: expected strings".into())),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(
                    "string-ci=? requires 2 arguments".into(),
                ));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(
                    a.borrow().to_lowercase() == b.borrow().to_lowercase(),
                )),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string-upcase requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s) => Ok(make_str(s.borrow().to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: expected string".into())),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string-downcase requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s) => Ok(make_str(s.borrow().to_lowercase())),
                _ => Err(EvalError::Type("string-downcase: expected string".into())),
            }
        }
        _ => unreachable!(),
    }
}

fn eval_char(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "char-alphabetic? requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(
                    "char-alphabetic?: expected char".into(),
                )),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "char-numeric? requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: expected char".into())),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "char-upcase requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: expected char".into())),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "char-downcase requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: expected char".into())),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("char=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into())),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("char<? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into())),
            }
        }
        _ => unreachable!(),
    }
}

fn eval_number_conversion(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{op} requires 1 argument")));
    }
    match op {
        "exact->inexact" => {
            let f = value_to_f64(&args[0])?;
            Ok(Value::Float(f))
        }
        "inexact->exact" => match &args[0] {
            Value::Float(f) => Ok(f64_to_exact(*f)),
            Value::Integer(_) | Value::Rational(_, _) => Ok(args[0].clone()),
            _ => Err(EvalError::Type("inexact->exact: expected number".into())),
        },
        "numerator" => match &args[0] {
            Value::Integer(n) => Ok(Value::Integer(*n)),
            Value::Rational(n, _) => Ok(Value::Integer(*n)),
            _ => Err(EvalError::Type("numerator: expected exact number".into())),
        },
        "denominator" => match &args[0] {
            Value::Integer(_) => Ok(Value::Integer(1)),
            Value::Rational(_, d) => Ok(Value::Integer(*d)),
            _ => Err(EvalError::Type("denominator: expected exact number".into())),
        },
        _ => unreachable!(),
    }
}
