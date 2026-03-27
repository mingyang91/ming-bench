use super::{
    apply_proc, collect_list, display_value, expect_integer, f64_to_exact, is_number, is_truthy,
    list_from_vec, make_pair, make_rational, make_str, nums_equal, nums_less, value_to_f64,
    values_equal, EvalError, Value,
};
use std::cell::RefCell;
use std::rc::Rc;

fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_) => {
            let mut tortoise = v.clone();
            let mut hare = v.clone();
            loop {
                // Advance hare by 2 steps
                for _ in 0..2 {
                    let next = match hare {
                        Value::List(_) => return true,
                        Value::Pair(ref cell) => cell.borrow().1.clone(),
                        _ => return false,
                    };
                    hare = next;
                }
                // Advance tortoise by 1 step
                let next = match tortoise {
                    Value::Pair(ref cell) => cell.borrow().1.clone(),
                    _ => return true,
                };
                tortoise = next;
                // Check for cycle
                if let (Value::Pair(ref a), Value::Pair(ref b)) = (&tortoise, &hare) {
                    if Rc::ptr_eq(a, b) {
                        return false;
                    }
                }
            }
        }
        _ => false,
    }
}

pub fn eval_builtin(op: &str, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match op {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "abs" | "modulo" | "remainder"
        | "quotient" | "min" | "max" | "expt" | "zero?" | "positive?" | "negative?" | "odd?"
        | "even?" | "gcd" | "lcm" | "truncate" | "round" => eval_arithmetic(op, args),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append" | "list-ref"
        | "list-tail" | "list?" | "assoc" | "assq" | "assv"
        | "member" | "memq" | "memv"
        | "set-car!" | "set-cdr!" | "reverse" => eval_list(op, args),

        "map" | "apply" | "for-each" => eval_higher_order(op, args, output),

        "not" | "eq?" | "eqv?" | "equal?" | "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
        | "char?" | "integer?" | "rational?" | "exact?" | "inexact?"
        | "procedure?" | "vector?" => eval_predicate(op, args),

        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector->list" | "list->vector" => eval_vector(op, args),

        "exact->inexact" | "inexact->exact" | "numerator" | "denominator" => {
            eval_number_conversion(op, args)
        }

        "display" | "write" | "newline" => eval_io(op, args, output),

        "string-append" | "string-length" | "substring" | "string->number" | "number->string"
        | "symbol->string" | "string->symbol" | "string-ref" | "string-copy" | "string-set!"
        | "string=?" | "string<?" | "string>?" | "string<=?" | "string>=?"
        | "string-ci=?" | "string-upcase"
        | "string-downcase" | "string->list" | "list->string"
        | "make-string" | "string" => eval_string(op, args),

        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?"
        | "char<?" | "char->integer" | "integer->char" => eval_char(op, args),

        "error" => {
            if args.is_empty() {
                return Err(EvalError::Generic("error".into()));
            }
            let msg = display_value(&args[0]);
            if args.len() > 1 {
                let rest: Vec<String> = args[1..].iter().map(display_value).collect();
                Err(EvalError::Generic(format!("{} {}", msg, rest.join(" "))))
            } else {
                Err(EvalError::Generic(msg))
            }
        }

        _ if op.len() > 2 && op.starts_with('c') && op.ends_with('r')
            && op[1..op.len()-1].bytes().all(|b| b == b'a' || b == b'd') =>
        {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{op} requires 1 argument")));
            }
            let mut val = args[0].clone();
            for c in op[1..op.len()-1].bytes().rev() {
                match c {
                    b'a' => val = eval_list("car", &[val])?,
                    b'd' => val = eval_list("cdr", &[val])?,
                    _ => unreachable!(),
                }
            }
            Ok(val)
        }

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
        "gcd" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("gcd requires 2 arguments".into()));
            }
            let a = expect_integer(&args[0], "gcd")?;
            let b = expect_integer(&args[1], "gcd")?;
            fn gcd(mut a: i64, mut b: i64) -> i64 {
                a = a.abs(); b = b.abs();
                while b != 0 { let t = b; b = a % b; a = t; }
                a
            }
            Ok(Value::Integer(gcd(a, b)))
        }
        "lcm" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("lcm requires 2 arguments".into()));
            }
            let a = expect_integer(&args[0], "lcm")?;
            let b = expect_integer(&args[1], "lcm")?;
            fn gcd(mut a: i64, mut b: i64) -> i64 {
                a = a.abs(); b = b.abs();
                while b != 0 { let t = b; b = a % b; a = t; }
                a
            }
            if a == 0 && b == 0 {
                Ok(Value::Integer(0))
            } else {
                Ok(Value::Integer((a / gcd(a, b) * b).abs()))
            }
        }
        "truncate" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("truncate requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                _ => Err(EvalError::Type("truncate: expected number".into())),
            }
        }
        "round" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("round requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                _ => Err(EvalError::Type("round: expected number".into())),
            }
        }
        _ => unreachable!(),
    }
}

fn eq_identity(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Str(a, _), Value::Str(b, _)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn eqv_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn assoc_search(
    name: &str,
    args: &[Value],
    cmp: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{name} requires 2 arguments")));
    }
    let key = &args[0];
    let items = collect_list(&args[1])
        .ok_or_else(|| EvalError::Type(format!("{name}: expected list")))?;
    for item in &items {
        let car = match item {
            Value::List(pair) if !pair.is_empty() => pair[0].clone(),
            Value::Pair(cell) => cell.borrow().0.clone(),
            _ => continue,
        };
        if cmp(key, &car) {
            return Ok(item.clone());
        }
    }
    Ok(Value::Boolean(false))
}

fn mem_search(
    name: &str,
    args: &[Value],
    cmp: fn(&Value, &Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{name} requires 2 arguments")));
    }
    let key = &args[0];
    let mut current = args[1].clone();
    loop {
        match &current {
            Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
            Value::List(items) => {
                for (i, item) in items.iter().enumerate() {
                    if cmp(key, item) {
                        return Ok(list_from_vec(&items[i..]));
                    }
                }
                return Ok(Value::Boolean(false));
            }
            Value::Pair(cell) => {
                let (car, cdr) = {
                    let b = cell.borrow();
                    (b.0.clone(), b.1.clone())
                };
                if cmp(key, &car) {
                    return Ok(current.clone());
                }
                current = cdr;
            }
            _ => return Ok(Value::Boolean(false)),
        }
    }
}

fn eval_list(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(cell) => Ok(cell.borrow().0.clone()),
                _ => Err(EvalError::Type("car: expected non-empty list or pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    Ok(list_from_vec(&items[1..]))
                }
                Value::Pair(cell) => Ok(cell.borrow().1.clone()),
                _ => Err(EvalError::Type("cdr: expected non-empty list or pair".into())),
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
        "list" => Ok(list_from_vec(args)),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("length: expected proper list".into()))?;
            Ok(Value::Integer(items.len() as i64))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::List(vec![]));
            }
            if args.len() == 1 {
                return Ok(args[0].clone());
            }
            let mut result = Vec::new();
            for a in &args[..args.len() - 1] {
                let items = collect_list(a)
                    .ok_or_else(|| EvalError::Type("append: expected list".into()))?;
                result.extend(items);
            }
            // Last arg: if it's a list, extend; otherwise create improper list
            let last = &args[args.len() - 1];
            match last {
                Value::List(items) if items.is_empty() && result.is_empty() => {
                    Ok(Value::List(vec![]))
                }
                _ => {
                    if let Some(items) = collect_list(last) {
                        result.extend(items);
                        return Ok(list_from_vec(&result));
                    }
                    // Improper: build pair chain ending with last
                    let mut tail = last.clone();
                    for item in result.into_iter().rev() {
                        tail = make_pair(item, tail);
                    }
                    Ok(tail)
                }
            }
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-ref requires 2 arguments".into()));
            }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list-ref: expected list".into()))?;
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
            let idx = expect_integer(&args[1], "list-tail")? as usize;
            let mut current = args[0].clone();
            for _ in 0..idx {
                let next = match &current {
                    Value::List(items) if !items.is_empty() => list_from_vec(&items[1..]),
                    Value::Pair(cell) => cell.borrow().1.clone(),
                    _ => return Err(EvalError::Generic("list-tail: index out of range".into())),
                };
                current = next;
            }
            Ok(current)
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list? requires 1 argument".into()));
            }
            Ok(Value::Boolean(is_proper_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("assoc requires 2 arguments".into()));
            }
            let key = &args[0];
            let items = collect_list(&args[1])
                .ok_or_else(|| EvalError::Type("assoc: expected list".into()))?;
            for item in &items {
                match item {
                    Value::List(pair) if !pair.is_empty() => {
                        if values_equal(key, &pair[0]) {
                            return Ok(item.clone());
                        }
                    }
                    Value::Pair(cell) => {
                        let car = cell.borrow().0.clone();
                        if values_equal(key, &car) {
                            return Ok(item.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        "assq" => assoc_search("assq", args, eq_identity),
        "assv" => assoc_search("assv", args, eqv_equal),
        "memq" => mem_search("memq", args, eq_identity),
        "memv" => mem_search("memv", args, eqv_equal),
        "member" => mem_search("member", args, values_equal),
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("reverse requires 1 argument".into()));
            }
            let mut items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("reverse: expected list".into()))?;
            items.reverse();
            Ok(list_from_vec(&items))
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("set-car! requires 2 arguments".into()));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().0 = args[1].clone();
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type("set-car!: expected mutable pair".into())),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("set-cdr! requires 2 arguments".into()));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().1 = args[1].clone();
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type("set-cdr!: expected mutable pair".into())),
            }
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
            let lists: Vec<Vec<Value>> = args[1..]
                .iter()
                .map(|a| collect_list(a).ok_or_else(|| EvalError::Type("map: expected list".into())))
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_proc(func, &call_args, output)?);
            }
            Ok(list_from_vec(&result))
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(
                    "for-each requires at least 2 arguments".into(),
                ));
            }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..]
                .iter()
                .map(|a| collect_list(a).ok_or_else(|| EvalError::Type("for-each: expected list".into())))
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply_proc(func, &call_args, output)?;
            }
            Ok(Value::Boolean(false))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(
                    "apply requires at least 2 arguments".into(),
                ));
            }
            let func = &args[0];
            let last = collect_list(&args[args.len() - 1])
                .ok_or_else(|| EvalError::Type("apply: last argument must be a list".into()))?;
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
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                (Value::Str(a, _), Value::Str(b, _)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("eqv? requires 2 arguments".into()));
            }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
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
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_, _))))
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
                    || matches!(&args[0], Value::Pair(_)),
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
                Value::Procedure(..) | Value::Builtin(_) | Value::CaseLambda(_) | Value::Continuation(..)
            )))
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
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
                    Value::Str(s, _) => result.push_str(&s.borrow()),
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
                Value::Str(s, _) => Ok(Value::Integer(s.borrow().len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s, _) => s.borrow().clone(),
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
                Value::Str(s, _) => match s.borrow().parse::<i64>() {
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
                Value::Str(s, _) => Ok(Value::Symbol(s.borrow().clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s, _) => s.borrow().clone(),
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
                Value::Str(s, _) => Ok(make_str(s.borrow().clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
            }
            match (&args[0], &args[1], &args[2]) {
                (Value::Str(s, mutable), Value::Integer(idx), Value::Char(c)) => {
                    if !mutable {
                        return Err(EvalError::Type("string-set!: strings are immutable".into()));
                    }
                    let idx = *idx as usize;
                    let mut st = s.borrow_mut();
                    if idx >= st.len() {
                        return Err(EvalError::Generic(format!("string-set!: index {} out of range", idx)));
                    }
                    let mut chars: Vec<char> = st.chars().collect();
                    if idx >= chars.len() {
                        return Err(EvalError::Generic(format!("string-set!: index {} out of range", idx)));
                    }
                    chars[idx] = *c;
                    *st = chars.into_iter().collect();
                    Ok(Value::List(vec![]))  // void
                }
                _ => Err(EvalError::Type("string-set!: expected (string, integer, char)".into())),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->list requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s, _) => {
                    let chars: Vec<Value> = s.borrow().chars().map(Value::Char).collect();
                    Ok(Value::List(chars))
                }
                _ => Err(EvalError::Type("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->string requires 1 argument".into()));
            }
            let vals = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list->string: expected list".into()))?;
            let mut s = String::new();
            for v in &vals {
                match v {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected list of chars".into())),
                }
            }
            Ok(make_str(s))
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => {
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
                (Value::Str(a, _), Value::Str(b, _)) => {
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
                (Value::Str(a, _), Value::Str(b, _)) => Ok(Value::Boolean(
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
                Value::Str(s, _) => Ok(make_str(s.borrow().to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: expected string".into())),
            }
        }
        "string>?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string>? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => {
                    Ok(Value::Boolean(*a.borrow() > *b.borrow()))
                }
                _ => Err(EvalError::Type("string>?: expected strings".into())),
            }
        }
        "string<=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string<=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => {
                    Ok(Value::Boolean(*a.borrow() <= *b.borrow()))
                }
                _ => Err(EvalError::Type("string<=?: expected strings".into())),
            }
        }
        "string>=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string>=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a, _), Value::Str(b, _)) => {
                    Ok(Value::Boolean(*a.borrow() >= *b.borrow()))
                }
                _ => Err(EvalError::Type("string>=?: expected strings".into())),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into()));
            }
            let len = expect_integer(&args[0], "make-string")? as usize;
            let c = if args.len() == 2 {
                match &args[1] {
                    Value::Char(c) => *c,
                    _ => return Err(EvalError::Type("make-string: expected char".into())),
                }
            } else {
                '\0'
            };
            Ok(make_str(std::iter::repeat_n(c, len).collect()))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("string: expected chars".into())),
                }
            }
            Ok(make_str(s))
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(
                    "string-downcase requires 1 argument".into(),
                ));
            }
            match &args[0] {
                Value::Str(s, _) => Ok(make_str(s.borrow().to_lowercase())),
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
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char->integer requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type("char->integer: expected char".into())),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("integer->char requires 1 argument".into()));
            }
            let n = expect_integer(&args[0], "integer->char")?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Generic("integer->char: invalid code point".into())),
            }
        }
        _ => unreachable!(),
    }
}

fn eval_vector(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
            }
            let len = expect_integer(&args[0], "make-vector")? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("vector-ref requires 2 arguments".into()));
            }
            let v = match &args[0] {
                Value::Vector(v) => v.borrow(),
                _ => return Err(EvalError::Type("vector-ref: expected vector".into())),
            };
            let idx = expect_integer(&args[1], "vector-ref")? as usize;
            if idx >= v.len() {
                return Err(EvalError::Generic("vector-ref: index out of range".into()));
            }
            Ok(v[idx].clone())
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("vector-set! requires 3 arguments".into()));
            }
            let v = match &args[0] {
                Value::Vector(v) => v.clone(),
                _ => return Err(EvalError::Type("vector-set!: expected vector".into())),
            };
            let idx = expect_integer(&args[1], "vector-set!")? as usize;
            let mut borrowed = v.borrow_mut();
            if idx >= borrowed.len() {
                return Err(EvalError::Generic("vector-set!: index out of range".into()));
            }
            borrowed[idx] = args[2].clone();
            Ok(Value::Boolean(false))
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector-length requires 1 argument".into()));
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type("vector-length: expected vector".into())),
            }
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector->list requires 1 argument".into()));
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                _ => Err(EvalError::Type("vector->list: expected vector".into())),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->vector requires 1 argument".into()));
            }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list->vector: expected list".into()))?;
            Ok(Value::Vector(Rc::new(RefCell::new(items))))
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
