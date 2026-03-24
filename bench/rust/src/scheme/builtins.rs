use super::{
    as_integer, apply_func, is_proper_list, make_rational, value_to_f64,
    values_eq, values_equal, DisplayValue, EvalError, Pos, Value,
};

/// Internal numeric representation for mixed-type arithmetic.
enum Num {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

fn as_num(v: &Value, pos: Pos) -> Result<Num, EvalError> {
    match v {
        Value::Integer(n) => Ok(Num::Int(*n)),
        Value::Rational(n, d) => Ok(Num::Rat(*n, *d)),
        Value::Float(f) => Ok(Num::Flt(*f)),
        _ => Err(EvalError::Type(format!("{pos}: expected number, got {v}"))),
    }
}

fn num_to_value(n: Num) -> Value {
    match n {
        Num::Int(n) => Value::Integer(n),
        Num::Rat(n, d) => make_rational(n, d),
        Num::Flt(f) => Value::Float(f),
    }
}

fn num_to_f64(n: &Num) -> f64 {
    match n {
        Num::Int(i) => *i as f64,
        Num::Rat(n, d) => *n as f64 / *d as f64,
        Num::Flt(f) => *f,
    }
}

fn num_add(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
        (Num::Int(a), Num::Rat(bn, bd)) | (Num::Rat(bn, bd), Num::Int(a)) => Num::Rat(a * bd + bn, bd),
        (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bd + bn * ad, ad * bd),
        (a, b) => Num::Flt(num_to_f64(&a) + num_to_f64(&b)),
    }
}

fn num_sub(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
        (Num::Int(a), Num::Rat(bn, bd)) => Num::Rat(a * bd - bn, bd),
        (Num::Rat(an, ad), Num::Int(b)) => Num::Rat(an - b * ad, ad),
        (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bd - bn * ad, ad * bd),
        (a, b) => Num::Flt(num_to_f64(&a) - num_to_f64(&b)),
    }
}

fn num_mul(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
        (Num::Int(a), Num::Rat(bn, bd)) | (Num::Rat(bn, bd), Num::Int(a)) => Num::Rat(a * bn, bd),
        (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bn, ad * bd),
        (a, b) => Num::Flt(num_to_f64(&a) * num_to_f64(&b)),
    }
}

fn num_div(a: Num, b: Num, pos: Pos) -> Result<Num, EvalError> {
    match (a, b) {
        (_, Num::Int(0)) | (_, Num::Rat(0, _)) => {
            Err(EvalError::DivisionByZero(format!("{pos}: division by zero")))
        }
        (Num::Int(a), Num::Int(b)) => Ok(Num::Rat(a, b)),
        (Num::Int(a), Num::Rat(bn, bd)) => Ok(Num::Rat(a * bd, bn)),
        (Num::Rat(an, ad), Num::Int(b)) => Ok(Num::Rat(an, ad * b)),
        (Num::Rat(an, ad), Num::Rat(bn, bd)) => Ok(Num::Rat(an * bd, ad * bn)),
        (a, b) => {
            let d = num_to_f64(&b);
            if d == 0.0 {
                Err(EvalError::DivisionByZero(format!("{pos}: division by zero")))
            } else {
                Ok(Num::Flt(num_to_f64(&a) / d))
            }
        }
    }
}

fn num_cmp(a: &Value, b: &Value, pos: Pos) -> Result<std::cmp::Ordering, EvalError> {
    let af = value_to_f64(a, pos)?;
    let bf = value_to_f64(b, pos)?;
    af.partial_cmp(&bf).ok_or_else(|| EvalError::Type(format!("{pos}: cannot compare {a} and {b}")))
}

fn num_eq(a: &Value, b: &Value, pos: Pos) -> Result<bool, EvalError> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(a == b),
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => Ok(an == bn && ad == bd),
        (Value::Float(a), Value::Float(b)) => Ok(a == b),
        _ => {
            let af = value_to_f64(a, pos)?;
            let bf = value_to_f64(b, pos)?;
            Ok(af == bf)
        }
    }
}

fn apply_numeric_builtin(name: &str, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut acc = Num::Int(0);
            for a in args {
                acc = num_add(acc, as_num(a, call_pos)?);
            }
            Ok(num_to_value(acc))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: - requires at least 1 argument")));
            }
            if args.len() == 1 {
                let n = as_num(&args[0], call_pos)?;
                return Ok(num_to_value(match n {
                    Num::Int(i) => Num::Int(-i),
                    Num::Rat(n, d) => Num::Rat(-n, d),
                    Num::Flt(f) => Num::Flt(-f),
                }));
            }
            let mut acc = as_num(&args[0], call_pos)?;
            for a in &args[1..] {
                acc = num_sub(acc, as_num(a, call_pos)?);
            }
            Ok(num_to_value(acc))
        }
        "*" => {
            let mut acc = Num::Int(1);
            for a in args {
                acc = num_mul(acc, as_num(a, call_pos)?);
            }
            Ok(num_to_value(acc))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: / requires at least 2 arguments")));
            }
            let mut acc = as_num(&args[0], call_pos)?;
            for a in &args[1..] {
                acc = num_div(acc, as_num(a, call_pos)?, call_pos)?;
            }
            Ok(num_to_value(acc))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: < requires 2 arguments")));
            }
            Ok(Value::Boolean(num_cmp(&args[0], &args[1], call_pos)? == std::cmp::Ordering::Less))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: > requires 2 arguments")));
            }
            Ok(Value::Boolean(num_cmp(&args[0], &args[1], call_pos)? == std::cmp::Ordering::Greater))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: = requires 2 arguments")));
            }
            Ok(Value::Boolean(num_eq(&args[0], &args[1], call_pos)?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: <= requires 2 arguments")));
            }
            Ok(Value::Boolean(num_cmp(&args[0], &args[1], call_pos)? != std::cmp::Ordering::Greater))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: >= requires 2 arguments")));
            }
            Ok(Value::Boolean(num_cmp(&args[0], &args[1], call_pos)? != std::cmp::Ordering::Less))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: abs requires 1 argument")));
            }
            match as_num(&args[0], call_pos)? {
                Num::Int(n) => Ok(Value::Integer(n.abs())),
                Num::Rat(n, d) => Ok(make_rational(n.abs(), d)),
                Num::Flt(f) => Ok(Value::Float(f.abs())),
            }
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: modulo requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: remainder requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: quotient requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: min requires at least 1 argument")));
            }
            let mut best = value_to_f64(&args[0], call_pos)?;
            let mut best_val = args[0].clone();
            for a in &args[1..] {
                let f = value_to_f64(a, call_pos)?;
                if f < best {
                    best = f;
                    best_val = a.clone();
                }
            }
            Ok(best_val)
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: max requires at least 1 argument")));
            }
            let mut best = value_to_f64(&args[0], call_pos)?;
            let mut best_val = args[0].clone();
            for a in &args[1..] {
                let f = value_to_f64(a, call_pos)?;
                if f > best {
                    best = f;
                    best_val = a.clone();
                }
            }
            Ok(best_val)
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: expt requires 2 arguments")));
            }
            let base = as_integer(&args[0], call_pos)?;
            let exp = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: zero? requires 1 argument")));
            }
            Ok(Value::Boolean(value_to_f64(&args[0], call_pos)? == 0.0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: positive? requires 1 argument")));
            }
            Ok(Value::Boolean(value_to_f64(&args[0], call_pos)? > 0.0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: negative? requires 1 argument")));
            }
            Ok(Value::Boolean(value_to_f64(&args[0], call_pos)? < 0.0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: odd? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: even? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? % 2 == 0))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: integer? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: rational? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: exact? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: inexact? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: exact->inexact requires 1 argument")));
            }
            Ok(Value::Float(value_to_f64(&args[0], call_pos)?))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: inexact->exact requires 1 argument")));
            }
            match &args[0] {
                Value::Integer(_) | Value::Rational(_, _) => Ok(args[0].clone()),
                Value::Float(f) => {
                    // Convert float to exact rational using continued fraction approximation
                    let (n, d) = float_to_rational(*f);
                    Ok(make_rational(n, d))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: inexact->exact: expected number"))),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: numerator requires 1 argument")));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type(format!("{call_pos}: numerator: expected rational"))),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: denominator requires 1 argument")));
            }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type(format!("{call_pos}: denominator: expected rational"))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

/// Convert a float to an exact rational (numerator, denominator).
fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 {
        return (0, 1);
    }
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    // Use the fact that many common fractions are exact in float
    // Try multiplying by powers of 2 to find exact representation
    let mut d: i64 = 1;
    let mut val = f;
    for _ in 0..53 {
        if val == val.floor() {
            return (sign * val as i64, d);
        }
        val *= 2.0;
        d *= 2;
    }
    // Fallback: just use large denominator
    let d = 1_000_000_000i64;
    let n = (f * d as f64).round() as i64;
    (sign * n, d)
}

fn apply_list_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: cons requires 2 arguments")));
            }
            match &args[1] {
                Value::List(elems) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(elems.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: car requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(a, _) => Ok(a.as_ref().clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: car: not a pair"))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: cdr requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec()))
                }
                Value::Pair(_, d) => Ok(d.as_ref().clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: cdr: not a pair"))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: null? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if e.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: length requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: length: not a list"))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(elems) => result.extend(elems.iter().cloned()),
                    _ => return Err(EvalError::Type(format!("{call_pos}: append: not a list"))),
                }
            }
            Ok(Value::List(result))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: list-ref requires 2 arguments")));
            }
            let idx = as_integer(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(elems) => Ok(elems[idx].clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: list-ref: not a list"))),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: list-tail requires 2 arguments")));
            }
            let idx = as_integer(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(elems) => Ok(Value::List(elems[idx..].to_vec())),
                _ => Err(EvalError::Type(format!("{call_pos}: list-tail: not a list"))),
            }
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: list? requires 1 argument")));
            }
            Ok(Value::Boolean(is_proper_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: assoc requires 2 arguments")));
            }
            let key = &args[0];
            match &args[1] {
                Value::List(alist) => {
                    for entry in alist {
                        match entry {
                            Value::List(pair) if !pair.is_empty() => {
                                if values_equal(key, &pair[0]) {
                                    return Ok(entry.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: assoc: expected list"))),
            }
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: apply requires at least 2 arguments")));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(elems) => elems.clone(),
                _ => return Err(EvalError::Type(format!("{call_pos}: apply: last argument must be a list"))),
            };
            let mut combined = args[1..args.len() - 1].to_vec();
            combined.extend(tail);
            apply_func(func, &combined, call_pos, output)
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: eq? requires 2 arguments")));
            }
            Ok(Value::Boolean(values_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: equal? requires 2 arguments")));
            }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: map requires at least 2 arguments")));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
                Value::List(elems) => Ok(elems),
                _ => Err(EvalError::Type(format!("{call_pos}: map: expected list"))),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::new();
            for i in 0..len {
                let map_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_func(func, &map_args, call_pos, output)?);
            }
            Ok(Value::List(result))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: boolean? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: pair? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn apply_string_io_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: display requires 1 argument")));
            }
            let s = format!("{}", DisplayValue(&args[0]));
            output.push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: write requires 1 argument")));
            }
            let s = format!("{}", &args[0]);
            output.push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: newline takes 0 arguments")));
            }
            output.push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("{call_pos}: string-append: expected string"))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-length requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: string-length: expected string"))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("{call_pos}: substring requires 3 arguments")));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: substring: expected string"))),
            };
            let start = as_integer(&args[1], call_pos)? as usize;
            let end = as_integer(&args[2], call_pos)? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->number requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type(format!("{call_pos}: string->number: expected string"))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number->string requires 1 argument")));
            }
            let n = as_integer(&args[0], call_pos)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol->string requires 1 argument")));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: symbol->string: expected symbol"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->symbol requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string->symbol: expected string"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string-ref requires 2 arguments")));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: string-ref: expected string"))),
            };
            let idx = as_integer(&args[1], call_pos)? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                EvalError::Type(format!("{call_pos}: string-ref: index out of range"))
            })?))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-copy requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-copy: expected string"))),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: char=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("{call_pos}: char=?: expected characters"))),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: char<? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("{call_pos}: char<?: expected characters"))),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-alphabetic? requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-alphabetic?: expected character"))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-numeric? requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-numeric?: expected character"))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-upcase requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-upcase: expected character"))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-downcase requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-downcase: expected character"))),
            }
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("{call_pos}: string=?: expected strings"))),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string<? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("{call_pos}: string<?: expected strings"))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string-ci=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-ci=?: expected strings"))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-upcase requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-upcase: expected string"))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-downcase requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-downcase: expected string"))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "number?" | "integer?" | "rational?" | "exact?" | "inexact?"
        | "exact->inexact" | "inexact->exact"
        | "numerator" | "denominator" => apply_numeric_builtin(name, args, call_pos),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list-ref" | "list-tail" | "list?" | "assoc"
        | "apply" | "eq?" | "equal?" | "map"
        | "boolean?" | "pair?" | "symbol?" => apply_list_builtin(name, args, call_pos, output),

        "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string" | "symbol->string" | "string->symbol"
        | "string-ref" | "string-copy" | "string?" | "char?"
        | "char=?" | "char<?" | "char-alphabetic?" | "char-numeric?"
        | "char-upcase" | "char-downcase"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => apply_string_io_builtin(name, args, call_pos, output),

        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: procedure? expects 1 argument")));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Lambda { .. }
                    | Value::CaseLambda { .. }
                    | Value::Builtin(_)
                    | Value::RecordConstructor { .. }
                    | Value::RecordPredicate { .. }
                    | Value::RecordAccessor { .. }
            )))
        }

        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}
