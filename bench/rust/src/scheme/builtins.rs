use std::cell::RefCell;
use std::rc::Rc;

use super::{ApplyFn, Pos, Value, make_pair, list_from_vec, value_to_vec};
use crate::scheme::EvalError;

// --- Numeric helpers ---

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % t;
        a = t;
    }
    if a == 0 { 1 } else { a }
}

pub(super) fn make_rational(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d.abs();
    let g = gcd(n.abs(), d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

#[derive(Clone, Copy)]
enum NumVal {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

impl NumVal {
    fn to_f64(self) -> f64 {
        match self {
            NumVal::Int(i) => i as f64,
            NumVal::Rat(n, d) => n as f64 / d as f64,
            NumVal::Flt(f) => f,
        }
    }
    fn to_exact(self) -> (i64, i64) {
        match self {
            NumVal::Int(i) => (i, 1),
            NumVal::Rat(n, d) => (n, d),
            NumVal::Flt(_) => unreachable!(),
        }
    }
}

fn expect_num(val: &Value, call_pos: Pos) -> Result<NumVal, EvalError> {
    match val {
        Value::Integer(n) => Ok(NumVal::Int(*n)),
        Value::Rational(n, d) => Ok(NumVal::Rat(*n, *d)),
        Value::Float(f) => Ok(NumVal::Flt(*f)),
        _ => Err(EvalError::Type(format!(
            "{call_pos}: expected number, got {}",
            val.display_scheme()
        ))),
    }
}

pub(super) fn expect_int(val: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!(
            "{call_pos}: expected integer, got {}",
            val.display_scheme()
        ))),
    }
}

fn has_float(nums: &[NumVal]) -> bool {
    nums.iter().any(|n| matches!(n, NumVal::Flt(_)))
}

fn collect_nums(args: &[Value], call_pos: Pos) -> Result<Vec<NumVal>, EvalError> {
    args.iter().map(|a| expect_num(a, call_pos)).collect()
}

fn exact_add(rn: i64, rd: i64, an: i64, ad: i64) -> (i64, i64) {
    let n = rn * ad + an * rd;
    let d = rd * ad;
    let g = gcd(n.abs(), d.abs());
    (n / g, d / g)
}

fn exact_sub(rn: i64, rd: i64, an: i64, ad: i64) -> (i64, i64) {
    let n = rn * ad - an * rd;
    let d = rd * ad;
    let g = gcd(n.abs(), d.abs());
    (n / g, d / g)
}

fn exact_mul(rn: i64, rd: i64, an: i64, ad: i64) -> (i64, i64) {
    let n = rn * an;
    let d = rd * ad;
    let g = gcd(n.abs(), d.abs());
    (n / g, d / g)
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f.fract() == 0.0 {
        return (f as i64, 1);
    }
    let sign = if f < 0.0 { -1i64 } else { 1 };
    let f = f.abs();
    let mut n = f;
    let mut d = 1i64;
    for _ in 0..53 {
        if (n - n.round()).abs() < 1e-10 {
            return (sign * n.round() as i64, d);
        }
        n *= 2.0;
        d *= 2;
    }
    let scale = 1_000_000_000i64;
    (sign * (f * scale as f64).round() as i64, scale)
}

// --- String builtins ---

fn apply_string_ext_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "string=?" | "string<?" | "string<=?" | "string>=?" | "string>?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: {name} requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    let result = match name {
                        "string=?" => a == b,
                        "string<?" => a < b,
                        "string<=?" => a <= b,
                        "string>=?" => a >= b,
                        "string>?" => a > b,
                        _ => unreachable!(),
                    };
                    Ok(Value::Boolean(result))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: {name}: expected strings"
                ))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-ci=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-ci=?: expected strings"
                ))),
            }
        }
        "string-upcase" | "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: {name} requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let result = if name == "string-upcase" {
                        s.to_uppercase()
                    } else {
                        s.to_lowercase()
                    };
                    Ok(Value::Str(result))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: {name}: expected string"
                ))),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string->list requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(list_from_vec(s.chars().map(Value::Char).collect())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string->list: expected string"
                ))),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list->string requires 1 argument"
                )));
            }
            match value_to_vec(&args[0]) {
                Some(items) => {
                    let mut s = String::new();
                    for item in &items {
                        match item {
                            Value::Char(c) => s.push(*c),
                            _ => return Err(EvalError::Type(format!(
                                "{call_pos}: list->string: expected list of chars"
                            ))),
                        }
                    }
                    Ok(Value::Str(s))
                }
                None => Err(EvalError::Type(format!(
                    "{call_pos}: list->string: expected list"
                ))),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: make-string requires 1 or 2 arguments"
                )));
            }
            let len = expect_int(&args[0], call_pos)? as usize;
            let ch = if args.len() == 2 {
                match &args[1] {
                    Value::Char(c) => *c,
                    _ => return Err(EvalError::Type(format!(
                        "{call_pos}: make-string: expected char"
                    ))),
                }
            } else {
                '\0'
            };
            Ok(Value::Str(std::iter::repeat_n(ch, len).collect()))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type(format!(
                        "{call_pos}: string: expected char"
                    ))),
                }
            }
            Ok(Value::Str(s))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_string_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: string-append: expected string"
                        )))
                    }
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-length requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-length: expected string"
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: substring requires 3 arguments"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let start = expect_int(&args[1], call_pos)? as usize;
                    let end = expect_int(&args[2], call_pos)? as usize;
                    if end > s.len() || start > end {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: substring: index out of range"
                        )));
                    }
                    Ok(Value::Str(s[start..end].to_string()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: substring: expected string"
                ))),
            }
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string->number requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    if let Ok(n) = s.parse::<i64>() {
                        Ok(Value::Integer(n))
                    } else if let Some(slash) = s.find('/') {
                        if let (Ok(n), Ok(d)) =
                            (s[..slash].parse::<i64>(), s[slash + 1..].parse::<i64>())
                        {
                            if d != 0 {
                                Ok(make_rational(n, d))
                            } else {
                                Ok(Value::Boolean(false))
                            }
                        } else {
                            Ok(Value::Boolean(false))
                        }
                    } else if let Ok(f) = s.parse::<f64>() {
                        Ok(Value::Float(f))
                    } else {
                        Ok(Value::Boolean(false))
                    }
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string->number: expected string"
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: number->string requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Str(n.to_string())),
                Value::Float(f) => {
                    let s = format!("{f}");
                    if f.is_finite() && !s.contains('.') {
                        Ok(Value::Str(format!("{f}.0")))
                    } else {
                        Ok(Value::Str(s))
                    }
                }
                Value::Rational(n, d) => Ok(Value::Str(format!("{n}/{d}"))),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: number->string: expected number"
                ))),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: symbol->string requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: symbol->string: expected symbol"
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string->symbol requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string->symbol: expected string"
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-ref requires 2 arguments"
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let idx = expect_int(&args[1], call_pos)? as usize;
                    if idx >= s.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: string-ref: index out of range"
                        )));
                    }
                    Ok(Value::Char(s.as_bytes()[idx] as char))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-ref: expected string"
                ))),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-copy requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-copy: expected string"
                ))),
            }
        }
        "string=?" | "string<?" | "string-ci=?" | "string<=?" | "string>=?" | "string>?"
        | "string-upcase" | "string-downcase" | "string->list" | "list->string"
        | "make-string" | "string"
            => apply_string_ext_builtin(name, args, call_pos),
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_char_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-alphabetic? requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-alphabetic?: expected char"
                ))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-numeric? requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-numeric?: expected char"
                ))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-upcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_uppercase().next().unwrap_or(*c))),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-upcase: expected char"
                ))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char-downcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_lowercase().next().unwrap_or(*c))),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char-downcase: expected char"
                ))),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char=?: expected chars"
                ))),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char<? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char<?: expected chars"
                ))),
            }
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: char->integer requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: char->integer: expected char"
                ))),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: integer->char requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(n) => {
                    let c = char::from_u32(*n as u32).ok_or_else(|| {
                        EvalError::Type(format!("{call_pos}: integer->char: invalid code point"))
                    })?;
                    Ok(Value::Char(c))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: integer->char: expected integer"
                ))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

// --- Numeric predicate builtins ---

fn apply_numeric_pred_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: zero? requires 1 argument"
                )));
            }
            let n = expect_num(&args[0], call_pos)?;
            Ok(Value::Boolean(match n {
                NumVal::Int(i) => i == 0,
                NumVal::Rat(n, _) => n == 0,
                NumVal::Flt(f) => f == 0.0,
            }))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: positive? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_num(&args[0], call_pos)?.to_f64() > 0.0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: negative? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_num(&args[0], call_pos)?.to_f64() < 0.0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: odd? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: even? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(expect_int(&args[0], call_pos)? % 2 == 0))
        }
        "gcd" => {
            if args.is_empty() { return Ok(Value::Integer(0)); }
            let mut result = expect_int(&args[0], call_pos)?.abs();
            for a in &args[1..] {
                let b = expect_int(a, call_pos)?.abs();
                result = gcd(result, b);
            }
            Ok(Value::Integer(result))
        }
        "lcm" => {
            if args.is_empty() { return Ok(Value::Integer(1)); }
            let mut result = expect_int(&args[0], call_pos)?.abs();
            for a in &args[1..] {
                let b = expect_int(a, call_pos)?.abs();
                if result == 0 && b == 0 { result = 0; }
                else { result = result / gcd(result, b) * b; }
            }
            Ok(Value::Integer(result))
        }
        "round" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: round requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: round: expected number"
                ))),
            }
        }
        "truncate" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: truncate requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: truncate: expected number"
                ))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

// --- Numeric builtins ---

pub(super) fn apply_numeric_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let nums = collect_nums(args, call_pos)?;
            if has_float(&nums) {
                Ok(Value::Float(nums.iter().map(|n| n.to_f64()).sum()))
            } else {
                let (mut rn, mut rd) = (0i64, 1i64);
                for n in &nums {
                    let (an, ad) = n.to_exact();
                    let r = exact_add(rn, rd, an, ad);
                    rn = r.0;
                    rd = r.1;
                }
                Ok(make_rational(rn, rd))
            }
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: - requires at least 1 argument"
                )));
            }
            let nums = collect_nums(args, call_pos)?;
            if nums.len() == 1 {
                return match nums[0] {
                    NumVal::Int(i) => Ok(Value::Integer(-i)),
                    NumVal::Rat(n, d) => Ok(make_rational(-n, d)),
                    NumVal::Flt(f) => Ok(Value::Float(-f)),
                };
            }
            if has_float(&nums) {
                let mut result = nums[0].to_f64();
                for n in &nums[1..] {
                    result -= n.to_f64();
                }
                Ok(Value::Float(result))
            } else {
                let (mut rn, mut rd) = nums[0].to_exact();
                for n in &nums[1..] {
                    let (an, ad) = n.to_exact();
                    let r = exact_sub(rn, rd, an, ad);
                    rn = r.0;
                    rd = r.1;
                }
                Ok(make_rational(rn, rd))
            }
        }
        "*" => {
            let nums = collect_nums(args, call_pos)?;
            if has_float(&nums) {
                let mut result = 1.0f64;
                for n in &nums {
                    result *= n.to_f64();
                }
                Ok(Value::Float(result))
            } else {
                let (mut rn, mut rd) = (1i64, 1i64);
                for n in &nums {
                    let (an, ad) = n.to_exact();
                    let r = exact_mul(rn, rd, an, ad);
                    rn = r.0;
                    rd = r.1;
                }
                Ok(make_rational(rn, rd))
            }
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: / requires at least 2 arguments"
                )));
            }
            let nums = collect_nums(args, call_pos)?;
            if has_float(&nums) {
                let mut result = nums[0].to_f64();
                for n in &nums[1..] {
                    let d = n.to_f64();
                    if d == 0.0 {
                        return Err(EvalError::DivisionByZero(format!(
                            "{call_pos}: division by zero"
                        )));
                    }
                    result /= d;
                }
                Ok(Value::Float(result))
            } else {
                let (mut rn, mut rd) = nums[0].to_exact();
                for n in &nums[1..] {
                    let (an, ad) = n.to_exact();
                    if an == 0 {
                        return Err(EvalError::DivisionByZero(format!(
                            "{call_pos}: division by zero"
                        )));
                    }
                    let r = exact_mul(rn, rd, ad, an); // divide = multiply by reciprocal
                    rn = r.0;
                    rd = r.1;
                }
                Ok(make_rational(rn, rd))
            }
        }
        "<" | ">" | "=" | "<=" | ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: {name} requires 2 arguments"
                )));
            }
            let a = expect_num(&args[0], call_pos)?;
            let b = expect_num(&args[1], call_pos)?;
            let is_float = matches!(a, NumVal::Flt(_)) || matches!(b, NumVal::Flt(_));
            let result = if is_float {
                let fa = a.to_f64();
                let fb = b.to_f64();
                match name {
                    "<" => fa < fb,
                    ">" => fa > fb,
                    "=" => fa == fb,
                    "<=" => fa <= fb,
                    ">=" => fa >= fb,
                    _ => unreachable!(),
                }
            } else {
                let (an, ad) = a.to_exact();
                let (bn, bd) = b.to_exact();
                let lhs = an * bd;
                let rhs = bn * ad;
                match name {
                    "<" => lhs < rhs,
                    ">" => lhs > rhs,
                    "=" => lhs == rhs,
                    "<=" => lhs <= rhs,
                    ">=" => lhs >= rhs,
                    _ => unreachable!(),
                }
            };
            Ok(Value::Boolean(result))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: abs requires 1 argument"
                )));
            }
            match expect_num(&args[0], call_pos)? {
                NumVal::Int(i) => Ok(Value::Integer(i.abs())),
                NumVal::Rat(n, d) => Ok(make_rational(n.abs(), d)),
                NumVal::Flt(f) => Ok(Value::Float(f.abs())),
            }
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: modulo requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: remainder requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: quotient requires 2 arguments"
                )));
            }
            let a = expect_int(&args[0], call_pos)?;
            let b = expect_int(&args[1], call_pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!(
                    "{call_pos}: division by zero"
                )));
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: min requires at least 1 argument"
                )));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result = result.min(expect_int(a, call_pos)?);
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: max requires at least 1 argument"
                )));
            }
            let mut result = expect_int(&args[0], call_pos)?;
            for a in &args[1..] {
                result = result.max(expect_int(a, call_pos)?);
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: expt requires 2 arguments"
                )));
            }
            let base = expect_int(&args[0], call_pos)?;
            let exp = expect_int(&args[1], call_pos)?;
            if exp < 0 {
                return Err(EvalError::Type(format!(
                    "{call_pos}: expt: negative exponent"
                )));
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "gcd" | "lcm" | "round" | "truncate"
            => apply_numeric_pred_builtin(name, args, call_pos),
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

// --- List builtins ---

pub(super) fn pair_cdr(val: &Value) -> Option<Value> {
    match val {
        Value::Pair(p) => Some(p.borrow().1.clone()),
        _ => None,
    }
}

fn is_proper_list(val: &Value) -> bool {
    match val {
        Value::List(_) => true,
        Value::Pair(_) => {
            // Tortoise and hare cycle detection
            let mut slow = val.clone();
            let mut fast = val.clone();
            loop {
                // Advance fast by 2
                for _ in 0..2 {
                    match pair_cdr(&fast) {
                        Some(next) => fast = next,
                        None => return matches!(&fast, Value::List(items) if items.is_empty()),
                    }
                }
                // Advance slow by 1
                match pair_cdr(&slow) {
                    Some(next) => slow = next,
                    None => return false,
                }
                // Check if they point to the same pair
                if let (Value::Pair(ref sp), Value::Pair(ref fp)) = (&slow, &fast) {
                    if Rc::ptr_eq(sp, fp) {
                        return false; // cycle detected
                    }
                }
            }
        }
        _ => false,
    }
}

fn apply_list_search_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "{call_pos}: {name} requires 2 arguments"
        )));
    }
    let items = value_to_vec(&args[1]).ok_or_else(|| {
        EvalError::Type(format!("{call_pos}: {name}: expected list"))
    })?;
    match name {
        "memq" => {
            for (i, item) in items.iter().enumerate() {
                let eq = match (&args[0], item) {
                    (Value::Integer(a), Value::Integer(b)) => a == b,
                    (Value::Boolean(a), Value::Boolean(b)) => a == b,
                    (Value::Char(a), Value::Char(b)) => a == b,
                    (Value::Symbol(a), Value::Symbol(b)) => a == b,
                    (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                    (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                    _ => false,
                };
                if eq {
                    return Ok(list_from_vec(items[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        "memv" => {
            for (i, item) in items.iter().enumerate() {
                let eq = match (&args[0], item) {
                    (Value::Integer(a), Value::Integer(b)) => a == b,
                    (Value::Float(a), Value::Float(b)) => a == b,
                    (Value::Rational(a1, a2), Value::Rational(b1, b2)) => a1 == b1 && a2 == b2,
                    (Value::Boolean(a), Value::Boolean(b)) => a == b,
                    (Value::Char(a), Value::Char(b)) => a == b,
                    (Value::Symbol(a), Value::Symbol(b)) => a == b,
                    (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                    (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                    _ => false,
                };
                if eq {
                    return Ok(list_from_vec(items[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        "member" => {
            for (i, item) in items.iter().enumerate() {
                if args[0] == *item {
                    return Ok(list_from_vec(items[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        "assq" => {
            for item in &items {
                if let Value::Pair(p) = item {
                    let car = p.borrow().0.clone();
                    let eq = match (&args[0], &car) {
                        (Value::Integer(a), Value::Integer(b)) => a == b,
                        (Value::Symbol(a), Value::Symbol(b)) => a == b,
                        (Value::Boolean(a), Value::Boolean(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                        _ => false,
                    };
                    if eq { return Ok(item.clone()); }
                }
            }
            Ok(Value::Boolean(false))
        }
        "assv" => {
            for item in &items {
                if let Value::Pair(p) = item {
                    let car = p.borrow().0.clone();
                    let eq = match (&args[0], &car) {
                        (Value::Integer(a), Value::Integer(b)) => a == b,
                        (Value::Float(a), Value::Float(b)) => a == b,
                        (Value::Rational(a1, a2), Value::Rational(b1, b2)) => a1 == b1 && a2 == b2,
                        (Value::Symbol(a), Value::Symbol(b)) => a == b,
                        (Value::Boolean(a), Value::Boolean(b)) => a == b,
                        (Value::Char(a), Value::Char(b)) => a == b,
                        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                        _ => false,
                    };
                    if eq { return Ok(item.clone()); }
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn apply_cxr_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!(
            "{call_pos}: {name} requires 1 argument"
        )));
    }
    let ops: Vec<char> = name[1..name.len()-1].chars().rev().collect();
    let mut val = args[0].clone();
    for op in ops {
        let next = match (&val, op) {
            (Value::Pair(p), 'a') => p.borrow().0.clone(),
            (Value::Pair(p), 'd') => p.borrow().1.clone(),
            (Value::List(items), 'a') if !items.is_empty() => items[0].clone(),
            (Value::List(items), 'd') if !items.is_empty() => {
                list_from_vec(items[1..].to_vec())
            }
            _ => return Err(EvalError::Type(format!(
                "{call_pos}: {name}: expected pair"
            ))),
        };
        val = next;
    }
    Ok(val)
}

fn apply_list_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
    out: &RefCell<String>,
    apply_func: ApplyFn,
) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: cons requires 2 arguments"
                )));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: car requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Pair(p) => Ok(p.borrow().0.clone()),
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: car: expected pair"
                ))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: cdr requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Pair(p) => Ok(p.borrow().1.clone()),
                Value::List(items) if !items.is_empty() => {
                    Ok(list_from_vec(items[1..].to_vec()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: cdr: expected pair"
                ))),
            }
        }
        "list" => Ok(list_from_vec(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: length requires 1 argument"
                )));
            }
            match value_to_vec(&args[0]) {
                Some(items) => Ok(Value::Integer(items.len() as i64)),
                None => Err(EvalError::Type(format!(
                    "{call_pos}: length: expected proper list"
                ))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: null? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::List(items) if items.is_empty()
            )))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: pair? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Pair(_))))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list? requires 1 argument"
                )));
            }
            // Use tortoise-and-hare cycle detection
            let result = is_proper_list(&args[0]);
            Ok(Value::Boolean(result))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::List(vec![]));
            }
            if args.len() == 1 {
                return Ok(args[0].clone());
            }
            // Collect all but last into a vec, append last
            let mut items = Vec::new();
            for arg in &args[..args.len() - 1] {
                match value_to_vec(arg) {
                    Some(v) => items.extend(v),
                    None => return Err(EvalError::Type(format!(
                        "{call_pos}: append: expected proper list"
                    ))),
                }
            }
            let last = &args[args.len() - 1];
            // Build result: cons chain of items onto last
            let mut result = last.clone();
            for item in items.into_iter().rev() {
                result = make_pair(item, result);
            }
            Ok(result)
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list-ref requires 2 arguments"
                )));
            }
            let idx = expect_int(&args[1], call_pos)? as usize;
            match value_to_vec(&args[0]) {
                Some(items) => {
                    if idx >= items.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: list-ref: index out of range"
                        )));
                    }
                    Ok(items[idx].clone())
                }
                None => Err(EvalError::Type(format!(
                    "{call_pos}: list-ref: expected list"
                ))),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list-tail requires 2 arguments"
                )));
            }
            let idx = expect_int(&args[1], call_pos)? as usize;
            // Walk idx steps down the cdr chain
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = match current {
                    Value::Pair(p) => p.borrow().1.clone(),
                    Value::List(ref items) if !items.is_empty() => {
                        list_from_vec(items[1..].to_vec())
                    }
                    _ => return Err(EvalError::Type(format!(
                        "{call_pos}: list-tail: index out of range"
                    ))),
                };
            }
            Ok(current)
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: assoc requires 2 arguments"
                )));
            }
            let key = &args[0];
            match value_to_vec(&args[1]) {
                Some(alist) => {
                    for item in &alist {
                        if let Value::Pair(p) = item {
                            let car = p.borrow().0.clone();
                            if car == *key {
                                return Ok(item.clone());
                            }
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                None => Err(EvalError::Type(format!(
                    "{call_pos}: assoc: expected list"
                ))),
            }
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: map requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..]
                .iter()
                .map(|a| value_to_vec(a).ok_or_else(|| EvalError::Type(format!(
                    "{call_pos}: map: expected list"
                ))))
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len {
                    return Err(EvalError::Type(format!(
                        "{call_pos}: map: lists must have same length"
                    )));
                }
            }
            let mut result = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_func(func, &call_args, call_pos, out)?);
            }
            Ok(list_from_vec(result))
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: for-each requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..]
                .iter()
                .map(|a| value_to_vec(a).ok_or_else(|| EvalError::Type(format!(
                    "{call_pos}: for-each: expected list"
                ))))
                .collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply_func(func, &call_args, call_pos, out)?;
            }
            Ok(Value::Void)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: apply requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = value_to_vec(last).ok_or_else(|| EvalError::Type(format!(
                "{call_pos}: apply: last argument must be a list"
            )))?;
            let mut final_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            final_args.extend(tail);
            apply_func(func, &final_args, call_pos, out)
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: set-car! requires 2 arguments"
                )));
            }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().0 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: set-car!: expected pair"
                ))),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: set-cdr! requires 2 arguments"
                )));
            }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().1 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: set-cdr!: expected pair"
                ))),
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: reverse requires 1 argument")));
            }
            let mut items = value_to_vec(&args[0]).ok_or_else(|| {
                EvalError::Type(format!("{call_pos}: reverse: expected list"))
            })?;
            items.reverse();
            Ok(list_from_vec(items))
        }
        "memq" | "memv" | "member" | "assq" | "assv"
            => apply_list_search_builtin(name, args, call_pos),
        _ if name.len() >= 3 && name.starts_with('c') && name.ends_with('r')
            && name[1..name.len()-1].chars().all(|c| c == 'a' || c == 'd')
            => apply_cxr_builtin(name, args, call_pos),
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

// --- Vector builtins ---

fn apply_vector_builtin(
    name: &str,
    args: &[Value],
    call_pos: Pos,
) -> Result<Value, EvalError> {
    match name {
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec())))),
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: make-vector requires 1 or 2 arguments"
                )));
            }
            let len = expect_int(&args[0], call_pos)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: vector-ref requires 2 arguments"
                )));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = expect_int(&args[1], call_pos)? as usize;
                    let v = v.borrow();
                    if idx >= v.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: vector-ref: index out of range"
                        )));
                    }
                    Ok(v[idx].clone())
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: vector-ref: expected vector"
                ))),
            }
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: vector-set! requires 3 arguments"
                )));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = expect_int(&args[1], call_pos)? as usize;
                    let mut v = v.borrow_mut();
                    if idx >= v.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: vector-set!: index out of range"
                        )));
                    }
                    v[idx] = args[2].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: vector-set!: expected vector"
                ))),
            }
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: vector-length requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: vector-length: expected vector"
                ))),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: vector? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: vector->list requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Vector(v) => Ok(list_from_vec(v.borrow().clone())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: vector->list: expected vector"
                ))),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list->vector requires 1 argument"
                )));
            }
            match value_to_vec(&args[0]) {
                Some(items) => Ok(Value::Vector(Rc::new(RefCell::new(items)))),
                None => Err(EvalError::Type(format!(
                    "{call_pos}: list->vector: expected list"
                ))),
            }
        }
        _ => Err(EvalError::Type(format!(
            "{call_pos}: unknown vector builtin: {name}"
        ))),
    }
}

// --- Main dispatch ---

pub(super) fn apply_builtin_by_name(
    name: &str,
    args: &[Value],
    call_pos: Pos,
    out: &RefCell<String>,
    apply_func: ApplyFn,
) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "abs" | "modulo"
        | "remainder" | "quotient" | "min" | "max" | "expt" | "zero?" | "positive?"
        | "negative?" | "odd?" | "even?" | "gcd" | "lcm" | "round" | "truncate"
        => apply_numeric_builtin(name, args, call_pos),
        "cons" | "car" | "cdr" | "list" | "length" | "null?" | "pair?" | "list?" | "append"
        | "list-ref" | "list-tail" | "assoc" | "map" | "for-each" | "apply"
        | "set-car!" | "set-cdr!" | "reverse" | "memq" | "memv" | "member"
        | "assq" | "assv" => {
            apply_list_builtin(name, args, call_pos, out, apply_func)
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: not requires 1 argument"
                )));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: number? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Float(_) | Value::Rational(_, _)
            )))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: integer? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: rational? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Rational(_, _)
            )))
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: exact? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(
                &args[0],
                Value::Integer(_) | Value::Rational(_, _)
            )))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: inexact? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: exact->inexact requires 1 argument"
                )));
            }
            let n = expect_num(&args[0], call_pos)?;
            Ok(Value::Float(n.to_f64()))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: inexact->exact requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Float(f) => {
                    let (n, d) = float_to_rational(*f);
                    Ok(make_rational(n, d))
                }
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, d) => Ok(make_rational(*n, *d)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: inexact->exact: expected number"
                ))),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: numerator requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: numerator: expected rational"
                ))),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: denominator requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: denominator: expected rational"
                ))),
            }
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: boolean? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: symbol? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: procedure? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0],
                Value::Lambda { .. } | Value::CaseLambda { .. }
                | Value::RecordConstructor { .. } | Value::RecordPredicate { .. }
                | Value::RecordAccessor { .. } | Value::Continuation(_)
            )))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: display requires 1 argument"
                )));
            }
            let s = args[0].display_output();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: write requires 1 argument"
                )));
            }
            let s = args[0].display_scheme();
            out.borrow_mut().push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: newline requires 0 arguments"
                )));
            }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref" | "char?"
        | "string-copy" | "string=?" | "string<?" | "string-ci=?" | "string-upcase"
        | "string-downcase" | "string->list" | "list->string"
        | "string<=?" | "string>=?" | "string>?" | "make-string" | "string"
        => apply_string_builtin(name, args, call_pos),
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?"
        | "char<?" | "char->integer" | "integer->char" => apply_char_builtin(name, args, call_pos),
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: equal? requires 2 arguments"
                )));
            }
            Ok(Value::Boolean(args[0] == args[1]))
        }
        "eq?" | "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: {name} requires 2 arguments"
                )));
            }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Rational(a1, a2), Value::Rational(b1, b2)) => a1 == b1 && a2 == b2,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                (Value::Void, Value::Void) => true,
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" | "vector?"
        | "vector->list" | "list->vector" => apply_vector_builtin(name, args, call_pos),
        _ if name.len() >= 3 && name.starts_with('c') && name.ends_with('r')
            && name[1..name.len()-1].chars().all(|c| c == 'a' || c == 'd') => {
            apply_list_builtin(name, args, call_pos, out, apply_func)
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}
