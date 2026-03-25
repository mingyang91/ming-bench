use super::{Val, Env, apply_val, make_rational, make_pair, vec_to_cons, collect_list, is_proper_list};
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use crate::scheme::error::EvalError;

// --- Numeric tower helpers ---

// Internal representation for arithmetic
enum Num {
    Exact(i64, i64),   // (numerator, denominator), always simplified, d > 0
    Inexact(f64),
}

fn val_to_num(v: &Val, op: &str) -> Result<Num, EvalError> {
    match v {
        Val::Int(n) => Ok(Num::Exact(*n, 1)),
        Val::Rational(n, d) => Ok(Num::Exact(*n, *d)),
        Val::Float(x) => Ok(Num::Inexact(*x)),
        _ => Err(EvalError::Type(format!("{op}: expected number"))),
    }
}

fn num_to_val(n: Num) -> Val {
    match n {
        Num::Exact(n, d) => make_rational(n, d),
        Num::Inexact(x) => Val::Float(x),
    }
}

fn num_to_f64(n: &Num) -> f64 {
    match n {
        Num::Exact(n, d) => *n as f64 / *d as f64,
        Num::Inexact(x) => *x,
    }
}

fn num_add(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            Num::Exact(an * bd + bn * ad, ad * bd)
        }
        (a, b) => Num::Inexact(num_to_f64(&a) + num_to_f64(&b)),
    }
}

fn num_sub(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            Num::Exact(an * bd - bn * ad, ad * bd)
        }
        (a, b) => Num::Inexact(num_to_f64(&a) - num_to_f64(&b)),
    }
}

fn num_mul(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            Num::Exact(an * bn, ad * bd)
        }
        (a, b) => Num::Inexact(num_to_f64(&a) * num_to_f64(&b)),
    }
}

fn num_div(a: Num, b: Num) -> Result<Num, EvalError> {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            if bn == 0 {
                return Err(EvalError::Runtime("division by zero".into()));
            }
            Ok(Num::Exact(an * bd, ad * bn))
        }
        (a, b) => {
            let bv = num_to_f64(&b);
            if bv == 0.0 {
                return Err(EvalError::Runtime("division by zero".into()));
            }
            Ok(Num::Inexact(num_to_f64(&a) / bv))
        }
    }
}

fn require_nums(args: &[Val], op: &str) -> Result<Vec<Num>, EvalError> {
    args.iter().map(|a| val_to_num(a, op)).collect()
}

// --- Arithmetic builtins ---

pub fn builtin_add(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_nums(args, "+")?;
    let mut acc = Num::Exact(0, 1);
    for n in nums {
        acc = num_add(acc, n);
    }
    Ok(num_to_val(acc))
}

pub fn builtin_sub(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("-: need at least 1 argument".into()));
    }
    let nums = require_nums(args, "-")?;
    if nums.len() == 1 {
        let neg = match &nums[0] {
            Num::Exact(n, d) => Num::Exact(-n, *d),
            Num::Inexact(x) => Num::Inexact(-x),
        };
        Ok(num_to_val(neg))
    } else {
        let mut acc = nums.into_iter().next().expect("nums guaranteed non-empty for len >= 2");
        for n in require_nums(&args[1..], "-")? {
            acc = num_sub(acc, n);
        }
        Ok(num_to_val(acc))
    }
}

pub fn builtin_mul(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums = require_nums(args, "*")?;
    let mut acc = Num::Exact(1, 1);
    for n in nums {
        acc = num_mul(acc, n);
    }
    Ok(num_to_val(acc))
}

pub fn builtin_div(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("/: need at least 2 arguments".into()));
    }
    let nums = require_nums(args, "/")?;
    let mut iter = nums.into_iter();
    let mut acc = iter.next().expect("nums guaranteed non-empty after arity check");
    for n in iter {
        acc = num_div(acc, n)?;
    }
    Ok(num_to_val(acc))
}

// --- Comparison builtins ---

fn num_cmp_f64(a: &Val, op: &str) -> Result<f64, EvalError> {
    match a {
        Val::Int(n) => Ok(*n as f64),
        Val::Float(x) => Ok(*x),
        Val::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(format!("{op}: expected number"))),
    }
}

pub fn builtin_lt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, "<")).collect::<Result<_, _>>()?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] < w[1])))
}

pub fn builtin_gt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, ">")).collect::<Result<_, _>>()?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] > w[1])))
}

pub fn builtin_eq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, "=")).collect::<Result<_, _>>()?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] == w[1])))
}

pub fn builtin_le(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, "<=")).collect::<Result<_, _>>()?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] <= w[1])))
}

pub fn builtin_ge(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, ">=")).collect::<Result<_, _>>()?;
    Ok(Val::Bool(nums.windows(2).all(|w| w[0] >= w[1])))
}

pub fn builtin_not(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not: expected 1 argument".into()));
    }
    Ok(Val::Bool(!args[0].is_truthy()))
}

pub fn builtin_cons(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons: expected 2 arguments".into()));
    }
    Ok(make_pair(args[0].clone(), args[1].clone()))
}

pub fn builtin_car(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Val::Pair(rc) => Ok(rc.borrow().0.clone()),
        _ => Err(EvalError::Type("car: expected pair".into())),
    }
}

pub fn builtin_cdr(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr: expected 1 argument".into()));
    }
    match &args[0] {
        Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
        Val::Pair(rc) => Ok(rc.borrow().1.clone()),
        _ => Err(EvalError::Type("cdr: expected pair".into())),
    }
}

pub fn builtin_list(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    Ok(vec_to_cons(args.to_vec()))
}

pub fn builtin_null(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null?: expected 1 argument".into()));
    }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if v.is_empty())))
}

pub fn builtin_length(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length: expected 1 argument".into()));
    }
    let items = collect_list(&args[0]).map_err(|_| EvalError::Type("length: expected proper list".into()))?;
    Ok(Val::Int(items.len() as i64))
}

pub fn builtin_append(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Ok(Val::List(vec![]));
    }
    let mut result = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if i == args.len() - 1 {
            // Last arg: if proper list, extend; otherwise create improper list
            match collect_list(arg) {
                Ok(items) => result.extend(items),
                Err(_) => {
                    // Last arg is not a proper list - return as improper
                    let mut tail = arg.clone();
                    for item in result.into_iter().rev() {
                        tail = make_pair(item, tail);
                    }
                    return Ok(tail);
                }
            }
        } else {
            let items = collect_list(arg).map_err(|_| EvalError::Type("append: expected list".into()))?;
            result.extend(items);
        }
    }
    Ok(vec_to_cons(result))
}

pub fn builtin_is_boolean(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("boolean?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Bool(_))))
}

pub fn builtin_is_number(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_) | Val::Float(_) | Val::Rational(_, _))))
}

pub fn builtin_is_integer(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_))))
}

pub fn builtin_is_rational(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("rational?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_) | Val::Rational(_, _))))
}

pub fn builtin_is_exact(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Int(_) | Val::Rational(_, _))))
}

pub fn builtin_is_inexact(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Float(_))))
}

pub fn builtin_exact_to_inexact(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact->inexact: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Float(*n as f64)),
        Val::Rational(n, d) => Ok(Val::Float(*n as f64 / *d as f64)),
        Val::Float(x) => Ok(Val::Float(*x)),
        _ => Err(EvalError::Type("exact->inexact: expected number".into())),
    }
}

pub fn builtin_inexact_to_exact(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact->exact: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Rational(n, d) => Ok(Val::Rational(*n, *d)),
        Val::Float(x) => {
            // Convert float to exact rational using continued fraction approximation
            // For simple cases like 0.5 -> 1/2
            let denom = 1_000_000_000i64;
            let numer = (*x * denom as f64).round() as i64;
            Ok(make_rational(numer, denom))
        }
        _ => Err(EvalError::Type("inexact->exact: expected number".into())),
    }
}

pub fn builtin_numerator(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("numerator: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Rational(n, _) => Ok(Val::Int(*n)),
        _ => Err(EvalError::Type("numerator: expected rational number".into())),
    }
}

pub fn builtin_denominator(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("denominator: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(_) => Ok(Val::Int(1)),
        Val::Rational(_, d) => Ok(Val::Int(*d)),
        _ => Err(EvalError::Type("denominator: expected rational number".into())),
    }
}

pub fn builtin_is_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Str(_))))
}

pub fn builtin_is_symbol(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Symbol(_))))
}

pub fn builtin_is_pair(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("pair?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(&args[0], Val::List(v) if !v.is_empty()) || matches!(&args[0], Val::Pair(_))))
}

pub fn builtin_is_char(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Char(_))))
}

pub fn builtin_is_procedure(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("procedure?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(args[0], Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(_) | Val::CallCC | Val::DynamicWind | Val::Continuation(..) | Val::Values | Val::CallWithValues)))
}

fn display_format(val: &Val) -> String {
    match val {
        Val::Str(s) => s.clone(),
        Val::Char(c) => c.to_string(),
        Val::List(elems) => {
            let mut out = String::from("(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                out.push_str(&display_format(e));
            }
            out.push(')');
            out
        }
        Val::Pair(rc) => {
            let mut seen = HashSet::new();
            seen.insert(Rc::as_ptr(rc) as usize);
            let (car_s, first_cdr) = {
                let pair = rc.borrow();
                (display_format(&pair.0), pair.1.clone())
            };
            let mut out = format!("({car_s}");
            let mut cur = first_cdr;
            loop {
                match &cur {
                    Val::List(v) if v.is_empty() => break,
                    Val::List(v) => {
                        for e in v {
                            out.push_str(&format!(" {}", display_format(e)));
                        }
                        break;
                    }
                    Val::Pair(rc2) => {
                        let ptr = Rc::as_ptr(rc2) as usize;
                        if !seen.insert(ptr) {
                            out.push_str(" ...");
                            break;
                        }
                        let (car_s2, next) = {
                            let p = rc2.borrow();
                            (display_format(&p.0), p.1.clone())
                        };
                        out.push_str(&format!(" {car_s2}"));
                        cur = next;
                    }
                    other => {
                        out.push_str(&format!(" . {}", display_format(other)));
                        break;
                    }
                }
            }
            out.push(')');
            out
        }
        Val::Vector(v) => {
            let elems = v.borrow();
            let mut out = String::from("#(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { out.push(' '); }
                out.push_str(&display_format(e));
            }
            out.push(')');
            out
        }
        other => other.to_string(),
    }
}

pub fn builtin_display(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("display: expected 1 argument".into())); }
    let text = display_format(&args[0]);
    env.output.borrow_mut().push_str(&text);
    Ok(Val::Void)
}

pub fn builtin_write(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("write: expected 1 argument".into())); }
    let text = args[0].to_string();
    env.output.borrow_mut().push_str(&text);
    Ok(Val::Void)
}

pub fn builtin_newline(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if !args.is_empty() { return Err(EvalError::Arity("newline: expected 0 arguments".into())); }
    env.output.borrow_mut().push('\n');
    Ok(Val::Void)
}

pub fn builtin_string_append(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let mut result = String::new();
    for arg in args {
        match arg {
            Val::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Val::Str(result))
}

pub fn builtin_string_length(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-length: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => Ok(Val::Int(s.chars().count() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

pub fn builtin_substring(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("substring: expected 3 arguments".into())); }
    let s = match &args[0] { Val::Str(s) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
    let start = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("substring: expected integer".into())) };
    let end = match &args[2] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("substring: expected integer".into())) };
    let chars: Vec<char> = s.chars().collect();
    if start > end || end > chars.len() {
        return Err(EvalError::Runtime("substring: index out of range".into()));
    }
    Ok(Val::Str(chars[start..end].iter().collect()))
}

pub fn builtin_string_to_number(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->number: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Val::Int(n)),
            Err(_) => match s.parse::<f64>() {
                Ok(x) => Ok(Val::Float(x)),
                Err(_) => Ok(Val::Bool(false)),
            },
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

pub fn builtin_number_to_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("number->string: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Str(n.to_string())),
        Val::Float(x) => Ok(Val::Str(format!("{}", x))),
        Val::Rational(n, d) => Ok(Val::Str(format!("{}/{}", n, d))),
        _ => Err(EvalError::Type("number->string: expected number".into())),
    }
}

pub fn builtin_symbol_to_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("symbol->string: expected 1 argument".into())); }
    match &args[0] {
        Val::Symbol(s) => Ok(Val::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

pub fn builtin_string_to_symbol(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string->symbol: expected 1 argument".into())); }
    match &args[0] {
        Val::Str(s) => Ok(Val::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

pub fn builtin_string_ref(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ref: expected 2 arguments".into())); }
    let s = match &args[0] { Val::Str(s) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
    let idx = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("string-ref: expected integer".into())) };
    let chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Runtime("string-ref: index out of range".into()));
    }
    Ok(Val::Char(chars[idx]))
}

pub fn builtin_string_copy(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy: expected 1 argument".into()));
    }
    match &args[0] {
        Val::Str(s) => Ok(Val::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

pub fn builtin_string_to_list(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->list: expected 1 argument".into()));
    }
    match &args[0] {
        Val::Str(s) => {
            let chars: Vec<Val> = s.chars().map(Val::Char).collect();
            Ok(vec_to_cons(chars))
        }
        _ => Err(EvalError::Type("string->list: expected string".into())),
    }
}

pub fn builtin_list_to_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("list->string: expected 1 argument".into()));
    }
    let items = collect_list(&args[0]).map_err(|_| EvalError::Type("list->string: expected list".into()))?;
    let mut s = String::new();
    for item in &items {
        match item {
            Val::Char(c) => s.push(*c),
            _ => return Err(EvalError::Type("list->string: expected list of characters".into())),
        }
    }
    Ok(Val::Str(s))
}

pub fn builtin_char_to_integer(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char->integer: expected 1 argument".into()));
    }
    match &args[0] {
        Val::Char(c) => Ok(Val::Int(*c as i64)),
        _ => Err(EvalError::Type("char->integer: expected char".into())),
    }
}

pub fn builtin_integer_to_char(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("integer->char: expected 1 argument".into()));
    }
    match &args[0] {
        Val::Int(n) => {
            let c = char::from_u32(*n as u32)
                .ok_or_else(|| EvalError::Runtime(format!("integer->char: invalid code point {n}")))?;
            Ok(Val::Char(c))
        }
        _ => Err(EvalError::Type("integer->char: expected integer".into())),
    }
}

pub fn vals_equal(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(xn, xd), Val::Rational(yn, yd)) => xn == yn && xd == yd,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Str(x), Val::Str(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::List(x), Val::List(y)) => x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| vals_equal(a, b)),
        (Val::Pair(rc1), Val::Pair(rc2)) => {
            if Rc::ptr_eq(rc1, rc2) { return true; }
            let p1 = rc1.borrow();
            let p2 = rc2.borrow();
            vals_equal(&p1.0, &p2.0) && vals_equal(&p1.1, &p2.1)
        }
        // Cross-type: compare Pair chain with Val::List
        (Val::Pair(_), Val::List(_)) | (Val::List(_), Val::Pair(_)) => {
            let a_items = match collect_list(a) { Ok(v) => v, Err(_) => return false };
            let b_items = match collect_list(b) { Ok(v) => v, Err(_) => return false };
            a_items.len() == b_items.len() && a_items.iter().zip(b_items.iter()).all(|(x, y)| vals_equal(x, y))
        }
        (Val::Vector(v1), Val::Vector(v2)) => {
            let a = v1.borrow();
            let b = v2.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| vals_equal(x, y))
        }
        (Val::Void, Val::Void) => true,
        _ => false,
    }
}

pub fn builtin_equal(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("equal?: expected 2 arguments".into())); }
    Ok(Val::Bool(vals_equal(&args[0], &args[1])))
}

pub fn builtin_eq_pred(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eq?: expected 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Val::Bool(a), Val::Bool(b)) => a == b,
        (Val::Int(a), Val::Int(b)) => a == b,
        (Val::Char(a), Val::Char(b)) => a == b,
        (Val::Symbol(a), Val::Symbol(b)) => a == b,
        (Val::List(a), Val::List(b)) => a.is_empty() && b.is_empty(),
        (Val::Void, Val::Void) => true,
        (Val::Vector(a), Val::Vector(b)) => Rc::ptr_eq(a, b),
        (Val::Pair(a), Val::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    };
    Ok(Val::Bool(result))
}

pub fn builtin_abs(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("abs: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(n.abs())),
        Val::Float(x) => Ok(Val::Float(x.abs())),
        Val::Rational(n, d) => Ok(Val::Rational(n.abs(), *d)),
        _ => Err(EvalError::Type("abs: expected number".into())),
    }
}

pub fn builtin_modulo(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("modulo: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Int(a), Val::Int(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("modulo: division by zero".into())); }
            Ok(Val::Int(((a % b) + b) % b))
        }
        _ => Err(EvalError::Type("modulo: expected integers".into())),
    }
}

pub fn builtin_remainder(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("remainder: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Int(a), Val::Int(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("remainder: division by zero".into())); }
            Ok(Val::Int(a % b))
        }
        _ => Err(EvalError::Type("remainder: expected integers".into())),
    }
}

pub fn builtin_quotient(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("quotient: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Int(a), Val::Int(b)) => {
            if *b == 0 { return Err(EvalError::Runtime("quotient: division by zero".into())); }
            Ok(Val::Int(a / b))
        }
        _ => Err(EvalError::Type("quotient: expected integers".into())),
    }
}

pub fn builtin_min(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("min: need at least 1 argument".into())); }
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, "min")).collect::<Result<_, _>>()?;
    let min_idx = nums.iter().enumerate().min_by(|(_, a), (_, b)| a.partial_cmp(b).expect("non-NaN after numeric coercion")).expect("nums non-empty after arity check").0;
    Ok(args[min_idx].clone())
}

pub fn builtin_max(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("max: need at least 1 argument".into())); }
    let nums: Vec<f64> = args.iter().map(|a| num_cmp_f64(a, "max")).collect::<Result<_, _>>()?;
    let max_idx = nums.iter().enumerate().max_by(|(_, a), (_, b)| a.partial_cmp(b).expect("non-NaN after numeric coercion")).expect("nums non-empty after arity check").0;
    Ok(args[max_idx].clone())
}

pub fn builtin_expt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("expt: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Int(base), Val::Int(exp)) => Ok(Val::Int((*base).pow(*exp as u32))),
        _ => {
            let b = num_cmp_f64(&args[0], "expt")?;
            let e = num_cmp_f64(&args[1], "expt")?;
            Ok(Val::Float(b.powf(e)))
        }
    }
}

pub fn builtin_zero(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("zero?: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Bool(*n == 0)),
        Val::Float(x) => Ok(Val::Bool(*x == 0.0)),
        Val::Rational(n, _) => Ok(Val::Bool(*n == 0)),
        _ => Err(EvalError::Type("zero?: expected number".into())),
    }
}

pub fn builtin_positive(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive?: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Bool(*n > 0)),
        Val::Float(x) => Ok(Val::Bool(*x > 0.0)),
        Val::Rational(n, _) => Ok(Val::Bool(*n > 0)),
        _ => Err(EvalError::Type("positive?: expected number".into())),
    }
}

pub fn builtin_negative(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative?: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Bool(*n < 0)),
        Val::Float(x) => Ok(Val::Bool(*x < 0.0)),
        Val::Rational(n, _) => Ok(Val::Bool(*n < 0)),
        _ => Err(EvalError::Type("negative?: expected number".into())),
    }
}

pub fn builtin_odd(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("odd?: expected 1 argument".into())); }
    match &args[0] { Val::Int(n) => Ok(Val::Bool(n % 2 != 0)), _ => Err(EvalError::Type("odd?: expected number".into())) }
}

pub fn builtin_even(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("even?: expected 1 argument".into())); }
    match &args[0] { Val::Int(n) => Ok(Val::Bool(n % 2 == 0)), _ => Err(EvalError::Type("even?: expected number".into())) }
}

pub fn builtin_list_ref(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-ref: expected 2 arguments".into())); }
    let idx = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("list-ref: expected integer".into())) };
    let items = collect_list(&args[0]).map_err(|_| EvalError::Type("list-ref: expected list".into()))?;
    items.get(idx).cloned().ok_or_else(|| EvalError::Runtime("list-ref: index out of range".into()))
}

pub fn builtin_list_tail(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-tail: expected 2 arguments".into())); }
    let idx = match &args[1] { Val::Int(n) => *n as usize, _ => return Err(EvalError::Type("list-tail: expected integer".into())) };
    // Walk the pair chain idx times, returning the tail
    let mut cur = args[0].clone();
    for _ in 0..idx {
        let next = match &cur {
            Val::List(elems) if !elems.is_empty() => Val::List(elems[1..].to_vec()),
            Val::Pair(rc) => rc.borrow().1.clone(),
            _ => return Err(EvalError::Runtime("list-tail: index out of range".into())),
        };
        cur = next;
    }
    Ok(cur)
}

pub fn builtin_is_list(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list?: expected 1 argument".into())); }
    Ok(Val::Bool(is_proper_list(&args[0]) || matches!(&args[0], Val::List(_))))
}

pub fn builtin_assoc(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assoc: expected 2 arguments".into())); }
    let key = &args[0];
    let alist = collect_list(&args[1]).map_err(|_| EvalError::Type("assoc: expected list".into()))?;
    for entry in &alist {
        let entry_items = match collect_list(entry) {
            Ok(items) if !items.is_empty() => items,
            _ => continue,
        };
        if vals_equal(key, &entry_items[0]) {
            return Ok(entry.clone());
        }
    }
    Ok(Val::Bool(false))
}

pub fn builtin_map(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("map: expected at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<Vec<Val>> = args[1..].iter().map(|a|
        collect_list(a).map_err(|_| EvalError::Type("map: expected list".into()))
    ).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let call_args: Vec<Val> = lists.iter().map(|l| l[i].clone()).collect();
        result.push(apply_val(func, &call_args, env)?);
    }
    Ok(vec_to_cons(result))
}

pub fn builtin_char_alphabetic(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic?: expected 1 argument".into())); }
    match &args[0] { Val::Char(c) => Ok(Val::Bool(c.is_alphabetic())), _ => Err(EvalError::Type("char-alphabetic?: expected char".into())) }
}

pub fn builtin_char_numeric(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-numeric?: expected 1 argument".into())); }
    match &args[0] { Val::Char(c) => Ok(Val::Bool(c.is_ascii_digit())), _ => Err(EvalError::Type("char-numeric?: expected char".into())) }
}

pub fn builtin_char_upcase(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-upcase: expected 1 argument".into())); }
    match &args[0] { Val::Char(c) => Ok(Val::Char(c.to_ascii_uppercase())), _ => Err(EvalError::Type("char-upcase: expected char".into())) }
}

pub fn builtin_char_downcase(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-downcase: expected 1 argument".into())); }
    match &args[0] { Val::Char(c) => Ok(Val::Char(c.to_ascii_lowercase())), _ => Err(EvalError::Type("char-downcase: expected char".into())) }
}

pub fn builtin_char_eq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a == b)),
        _ => Err(EvalError::Type("char=?: expected chars".into())),
    }
}

pub fn builtin_char_lt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a < b)),
        _ => Err(EvalError::Type("char<?: expected chars".into())),
    }
}

pub fn builtin_string_eq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a == b)),
        _ => Err(EvalError::Type("string=?: expected strings".into())),
    }
}

pub fn builtin_string_lt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a < b)),
        _ => Err(EvalError::Type("string<?: expected strings".into())),
    }
}

pub fn builtin_string_ci_eq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ci=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a.to_lowercase() == b.to_lowercase())),
        _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
    }
}

pub fn builtin_string_upcase(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-upcase: expected 1 argument".into())); }
    match &args[0] { Val::Str(s) => Ok(Val::Str(s.to_uppercase())), _ => Err(EvalError::Type("string-upcase: expected string".into())) }
}

pub fn builtin_string_downcase(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-downcase: expected 1 argument".into())); }
    match &args[0] { Val::Str(s) => Ok(Val::Str(s.to_lowercase())), _ => Err(EvalError::Type("string-downcase: expected string".into())) }
}

pub fn builtin_apply(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply: expected at least 2 arguments".into()));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = collect_list(last).map_err(|_| EvalError::Type("apply: last argument must be a list".into()))?;
    let mut all_args: Vec<Val> = args[1..args.len() - 1].to_vec();
    all_args.extend(tail);
    apply_val(func, &all_args, env)
}

pub fn builtin_eqv(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eqv?: expected 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Val::Bool(a), Val::Bool(b)) => a == b,
        (Val::Int(a), Val::Int(b)) => a == b,
        (Val::Float(a), Val::Float(b)) => a == b,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Char(a), Val::Char(b)) => a == b,
        (Val::Symbol(a), Val::Symbol(b)) => a == b,
        (Val::List(a), Val::List(b)) => a.is_empty() && b.is_empty(),
        (Val::Pair(a), Val::Pair(b)) => Rc::ptr_eq(a, b),
        (Val::Vector(a), Val::Vector(b)) => Rc::ptr_eq(a, b),
        (Val::Void, Val::Void) => true,
        _ => false,
    };
    Ok(Val::Bool(result))
}

pub fn builtin_vector(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    Ok(Val::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

pub fn builtin_make_vector(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-vector: expected 1 or 2 arguments".into()));
    }
    let len = match &args[0] {
        Val::Int(n) => *n as usize,
        _ => return Err(EvalError::Type("make-vector: expected integer".into())),
    };
    let fill = if args.len() == 2 { args[1].clone() } else { Val::Int(0) };
    Ok(Val::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

pub fn builtin_vector_ref(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-ref: expected 2 arguments".into())); }
    let v = match &args[0] {
        Val::Vector(v) => v.borrow(),
        _ => return Err(EvalError::Type("vector-ref: expected vector".into())),
    };
    let idx = match &args[1] {
        Val::Int(n) => *n as usize,
        _ => return Err(EvalError::Type("vector-ref: expected integer index".into())),
    };
    v.get(idx).cloned().ok_or_else(|| EvalError::Runtime("vector-ref: index out of range".into()))
}

pub fn builtin_vector_set(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set!: expected 3 arguments".into())); }
    let v = match &args[0] {
        Val::Vector(v) => v.clone(),
        _ => return Err(EvalError::Type("vector-set!: expected vector".into())),
    };
    let idx = match &args[1] {
        Val::Int(n) => *n as usize,
        _ => return Err(EvalError::Type("vector-set!: expected integer index".into())),
    };
    let mut vec = v.borrow_mut();
    if idx >= vec.len() {
        return Err(EvalError::Runtime("vector-set!: index out of range".into()));
    }
    vec[idx] = args[2].clone();
    Ok(Val::Void)
}

pub fn builtin_vector_length(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector-length: expected 1 argument".into())); }
    match &args[0] {
        Val::Vector(v) => Ok(Val::Int(v.borrow().len() as i64)),
        _ => Err(EvalError::Type("vector-length: expected vector".into())),
    }
}

pub fn builtin_is_vector(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector?: expected 1 argument".into())); }
    Ok(Val::Bool(matches!(&args[0], Val::Vector(_))))
}

pub fn builtin_vector_to_list(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector->list: expected 1 argument".into())); }
    match &args[0] {
        Val::Vector(v) => Ok(vec_to_cons(v.borrow().clone())),
        _ => Err(EvalError::Type("vector->list: expected vector".into())),
    }
}

pub fn builtin_list_to_vector(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->vector: expected 1 argument".into())); }
    let items = collect_list(&args[0]).map_err(|_| EvalError::Type("list->vector: expected list".into()))?;
    Ok(Val::Vector(Rc::new(RefCell::new(items))))
}

pub fn builtin_set_car(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("set-car!: expected 2 arguments".into())); }
    match &args[0] {
        Val::Pair(rc) => { rc.borrow_mut().0 = args[1].clone(); Ok(Val::Void) }
        _ => Err(EvalError::Type("set-car!: expected pair".into())),
    }
}

pub fn builtin_set_cdr(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("set-cdr!: expected 2 arguments".into())); }
    match &args[0] {
        Val::Pair(rc) => { rc.borrow_mut().1 = args[1].clone(); Ok(Val::Void) }
        _ => Err(EvalError::Type("set-cdr!: expected pair".into())),
    }
}

// --- L17 builtins ---

pub fn builtin_for_each(args: &[Val], env: &Env) -> Result<Val, EvalError> {
    if args.len() < 2 { return Err(EvalError::Arity("for-each: expected at least 2 arguments".into())); }
    let func = &args[0];
    let lists: Vec<Vec<Val>> = args[1..].iter().map(|a|
        collect_list(a).map_err(|_| EvalError::Type("for-each: expected list".into()))
    ).collect::<Result<_, _>>()?;
    let len = lists[0].len();
    for i in 0..len {
        let call_args: Vec<Val> = lists.iter().map(|l| l[i].clone()).collect();
        apply_val(func, &call_args, env)?;
    }
    Ok(Val::Void)
}

pub fn builtin_assq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assq: expected 2 arguments".into())); }
    let key = &args[0];
    let alist = collect_list(&args[1]).map_err(|_| EvalError::Type("assq: expected list".into()))?;
    for entry in &alist {
        match entry {
            Val::Pair(rc) => {
                let p = rc.borrow();
                if val_eq(key, &p.0) { return Ok(entry.clone()); }
            }
            Val::List(elems) if !elems.is_empty() => {
                if val_eq(key, &elems[0]) { return Ok(entry.clone()); }
            }
            _ => {}
        }
    }
    Ok(Val::Bool(false))
}

pub fn builtin_assv(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assv: expected 2 arguments".into())); }
    let key = &args[0];
    let alist = collect_list(&args[1]).map_err(|_| EvalError::Type("assv: expected list".into()))?;
    for entry in &alist {
        match entry {
            Val::Pair(rc) => {
                let p = rc.borrow();
                if val_eqv(key, &p.0) { return Ok(entry.clone()); }
            }
            Val::List(elems) if !elems.is_empty() => {
                if val_eqv(key, &elems[0]) { return Ok(entry.clone()); }
            }
            _ => {}
        }
    }
    Ok(Val::Bool(false))
}

pub fn builtin_memq(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("memq: expected 2 arguments".into())); }
    let key = &args[0];
    // Walk the list as a pair chain to return the tail from the match point
    let mut cur = args[1].clone();
    loop {
        match &cur {
            Val::List(elems) if elems.is_empty() => return Ok(Val::Bool(false)),
            Val::Pair(rc) => {
                let p = rc.borrow();
                if val_eq(key, &p.0) {
                    drop(p);
                    return Ok(cur);
                }
                let next = p.1.clone();
                drop(p);
                cur = next;
            }
            Val::List(elems) => {
                // Quoted list fallback
                for (i, e) in elems.iter().enumerate() {
                    if val_eq(key, e) {
                        return Ok(Val::List(elems[i..].to_vec()));
                    }
                }
                return Ok(Val::Bool(false));
            }
            _ => return Ok(Val::Bool(false)),
        }
    }
}

pub fn builtin_memv(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("memv: expected 2 arguments".into())); }
    let key = &args[0];
    let mut cur = args[1].clone();
    loop {
        match &cur {
            Val::List(elems) if elems.is_empty() => return Ok(Val::Bool(false)),
            Val::Pair(rc) => {
                let p = rc.borrow();
                if val_eqv(key, &p.0) {
                    drop(p);
                    return Ok(cur);
                }
                let next = p.1.clone();
                drop(p);
                cur = next;
            }
            Val::List(elems) => {
                for (i, e) in elems.iter().enumerate() {
                    if val_eqv(key, e) {
                        return Ok(Val::List(elems[i..].to_vec()));
                    }
                }
                return Ok(Val::Bool(false));
            }
            _ => return Ok(Val::Bool(false)),
        }
    }
}

pub fn builtin_member(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("member: expected 2 arguments".into())); }
    let key = &args[0];
    let mut cur = args[1].clone();
    loop {
        match &cur {
            Val::List(elems) if elems.is_empty() => return Ok(Val::Bool(false)),
            Val::Pair(rc) => {
                let p = rc.borrow();
                if vals_equal(key, &p.0) {
                    drop(p);
                    return Ok(cur);
                }
                let next = p.1.clone();
                drop(p);
                cur = next;
            }
            Val::List(elems) => {
                for (i, e) in elems.iter().enumerate() {
                    if vals_equal(key, e) {
                        return Ok(Val::List(elems[i..].to_vec()));
                    }
                }
                return Ok(Val::Bool(false));
            }
            _ => return Ok(Val::Bool(false)),
        }
    }
}

pub fn builtin_reverse(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("reverse: expected 1 argument".into())); }
    let mut items = collect_list(&args[0]).map_err(|_| EvalError::Type("reverse: expected list".into()))?;
    items.reverse();
    Ok(vec_to_cons(items))
}

pub fn builtin_gcd(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    fn gcd(mut a: i64, mut b: i64) -> i64 {
        a = a.abs(); b = b.abs();
        while b != 0 { let t = b; b = a % b; a = t; }
        a
    }
    if args.is_empty() { return Ok(Val::Int(0)); }
    let mut result = match &args[0] {
        Val::Int(n) => n.abs(),
        _ => return Err(EvalError::Type("gcd: expected integer".into())),
    };
    for arg in &args[1..] {
        let n = match arg { Val::Int(n) => *n, _ => return Err(EvalError::Type("gcd: expected integer".into())) };
        result = gcd(result, n);
    }
    Ok(Val::Int(result))
}

pub fn builtin_lcm(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    fn gcd(mut a: i64, mut b: i64) -> i64 {
        a = a.abs(); b = b.abs();
        while b != 0 { let t = b; b = a % b; a = t; }
        a
    }
    if args.is_empty() { return Ok(Val::Int(1)); }
    let mut result = match &args[0] {
        Val::Int(n) => n.abs(),
        _ => return Err(EvalError::Type("lcm: expected integer".into())),
    };
    for arg in &args[1..] {
        let n = match arg { Val::Int(n) => n.abs(), _ => return Err(EvalError::Type("lcm: expected integer".into())) };
        result = result / gcd(result, n) * n;
    }
    Ok(Val::Int(result))
}

pub fn builtin_truncate(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("truncate: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Float(x) => Ok(Val::Int(x.trunc() as i64)),
        _ => Err(EvalError::Type("truncate: expected number".into())),
    }
}

pub fn builtin_round(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("round: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Float(x) => Ok(Val::Int(x.round() as i64)),
        _ => Err(EvalError::Type("round: expected number".into())),
    }
}

pub fn builtin_make_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-string: expected 1 or 2 arguments".into()));
    }
    let len = match &args[0] {
        Val::Int(n) => *n as usize,
        _ => return Err(EvalError::Type("make-string: expected integer".into())),
    };
    let ch = if args.len() == 2 {
        match &args[1] { Val::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into())) }
    } else { '\0' };
    Ok(Val::Str(std::iter::repeat_n(ch, len).collect()))
}

pub fn builtin_string(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    let mut s = String::new();
    for arg in args {
        match arg {
            Val::Char(c) => s.push(*c),
            _ => return Err(EvalError::Type("string: expected characters".into())),
        }
    }
    Ok(Val::Str(s))
}

pub fn builtin_string_gt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string>?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a > b)),
        _ => Err(EvalError::Type("string>?: expected strings".into())),
    }
}

pub fn builtin_string_le(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a <= b)),
        _ => Err(EvalError::Type("string<=?: expected strings".into())),
    }
}

pub fn builtin_string_ge(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string>=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a >= b)),
        _ => Err(EvalError::Type("string>=?: expected strings".into())),
    }
}

pub fn builtin_error(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.is_empty() { return Err(EvalError::Runtime("error".into())); }
    let msg = match &args[0] {
        Val::Str(s) => s.clone(),
        other => format!("{other}"),
    };
    if args.len() > 1 {
        let irritants: Vec<String> = args[1..].iter().map(|a| format!("{a}")).collect();
        Err(EvalError::Runtime(format!("{}: {}", msg, irritants.join(" "))))
    } else {
        Err(EvalError::Runtime(msg))
    }
}

pub fn builtin_null_environment(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    // Stub that returns void — needed by some realworld tests
    if args.len() != 1 { return Err(EvalError::Arity("null-environment: expected 1 argument".into())); }
    Ok(Val::Void)
}

pub fn builtin_floor(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("floor: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Float(x) => Ok(Val::Int(x.floor() as i64)),
        _ => Err(EvalError::Type("floor: expected number".into())),
    }
}

pub fn builtin_ceiling(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("ceiling: expected 1 argument".into())); }
    match &args[0] {
        Val::Int(n) => Ok(Val::Int(*n)),
        Val::Float(x) => Ok(Val::Int(x.ceil() as i64)),
        _ => Err(EvalError::Type("ceiling: expected number".into())),
    }
}

pub fn builtin_char_gt(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char>?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a > b)),
        _ => Err(EvalError::Type("char>?: expected chars".into())),
    }
}

pub fn builtin_char_le(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a <= b)),
        _ => Err(EvalError::Type("char<=?: expected chars".into())),
    }
}

pub fn builtin_char_ge(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char>=?: expected 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a >= b)),
        _ => Err(EvalError::Type("char>=?: expected chars".into())),
    }
}

pub fn builtin_vector_fill(args: &[Val], _env: &Env) -> Result<Val, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-fill!: expected 2 arguments".into())); }
    let v = match &args[0] {
        Val::Vector(v) => v.clone(),
        _ => return Err(EvalError::Type("vector-fill!: expected vector".into())),
    };
    let fill = args[1].clone();
    let mut vec = v.borrow_mut();
    for e in vec.iter_mut() { *e = fill.clone(); }
    Ok(Val::Void)
}

fn val_eq(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::List(x), Val::List(y)) => x.is_empty() && y.is_empty(),
        (Val::Void, Val::Void) => true,
        (Val::Pair(x), Val::Pair(y)) => Rc::ptr_eq(x, y),
        (Val::Vector(x), Val::Vector(y)) => Rc::ptr_eq(x, y),
        _ => false,
    }
}

fn val_eqv(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::List(x), Val::List(y)) => x.is_empty() && y.is_empty(),
        (Val::Pair(x), Val::Pair(y)) => Rc::ptr_eq(x, y),
        (Val::Vector(x), Val::Vector(y)) => Rc::ptr_eq(x, y),
        (Val::Void, Val::Void) => true,
        _ => false,
    }
}
