use std::cell::RefCell;
use std::rc::Rc;

use super::{ApplyFn, Pos, Value};
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
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string=? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string=?: expected strings"
                ))),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string<? requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string<?: expected strings"
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
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-upcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-upcase: expected string"
                ))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: string-downcase requires 1 argument"
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: string-downcase: expected string"
                ))),
            }
        }
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
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

// --- List builtins ---

pub(super) fn apply_list_builtin(
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
                return Err(EvalError::Arity(format!(
                    "{call_pos}: car requires 1 argument"
                )));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: car: expected non-empty list"
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
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: cdr: expected non-empty list"
                ))),
            }
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: length requires 1 argument"
                )));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: length: expected list"
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
            Ok(Value::Boolean(
                matches!(&args[0], Value::List(items) if !items.is_empty())
                    || matches!(&args[0], Value::Pair(_, _)),
            ))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list? requires 1 argument"
                )));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "append" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: append requires 2 arguments"
                )));
            }
            match (&args[0], &args[1]) {
                (Value::List(a), Value::List(b)) => {
                    let mut result = a.clone();
                    result.extend(b.iter().cloned());
                    Ok(Value::List(result))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: append: expected lists"
                ))),
            }
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: list-ref requires 2 arguments"
                )));
            }
            let idx = expect_int(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(items) => {
                    if idx >= items.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: list-ref: index out of range"
                        )));
                    }
                    Ok(items[idx].clone())
                }
                _ => Err(EvalError::Type(format!(
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
            match &args[0] {
                Value::List(items) => {
                    if idx > items.len() {
                        return Err(EvalError::Type(format!(
                            "{call_pos}: list-tail: index out of range"
                        )));
                    }
                    Ok(Value::List(items[idx..].to_vec()))
                }
                _ => Err(EvalError::Type(format!(
                    "{call_pos}: list-tail: expected list"
                ))),
            }
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: assoc requires 2 arguments"
                )));
            }
            let key = &args[0];
            match &args[1] {
                Value::List(alist) => {
                    for item in alist {
                        if let Value::List(pair) = item {
                            if !pair.is_empty() && pair[0] == *key {
                                return Ok(item.clone());
                            }
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type(format!(
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
            let lists: Vec<&Vec<Value>> = args[1..]
                .iter()
                .map(|a| match a {
                    Value::List(items) => Ok(items),
                    _ => Err(EvalError::Type(format!(
                        "{call_pos}: map: expected list"
                    ))),
                })
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
            Ok(Value::List(result))
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: for-each requires at least 2 arguments"
                )));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..]
                .iter()
                .map(|a| match a {
                    Value::List(items) => Ok(items),
                    _ => Err(EvalError::Type(format!(
                        "{call_pos}: for-each: expected list"
                    ))),
                })
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
            let tail = match last {
                Value::List(items) => items.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "{call_pos}: apply: last argument must be a list"
                    )))
                }
            };
            let mut final_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            final_args.extend(tail);
            apply_func(func, &final_args, call_pos, out)
        }
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
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
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
            match &args[0] {
                Value::List(items) => Ok(Value::Vector(Rc::new(RefCell::new(items.clone())))),
                _ => Err(EvalError::Type(format!(
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
        | "negative?" | "odd?" | "even?" => apply_numeric_builtin(name, args, call_pos),
        "cons" | "car" | "cdr" | "list" | "length" | "null?" | "pair?" | "list?" | "append"
        | "list-ref" | "list-tail" | "assoc" | "map" | "for-each" | "apply" => {
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
                | Value::RecordAccessor { .. }
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
        | "string-downcase" => apply_string_builtin(name, args, call_pos),
        "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase" | "char=?"
        | "char<?" => apply_char_builtin(name, args, call_pos),
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
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" | "vector?"
        | "vector->list" | "list->vector" => apply_vector_builtin(name, args, call_pos),
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}
