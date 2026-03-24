use std::cell::RefCell;
use std::rc::Rc;

use super::error::{EvalError, Span};
use super::{Output, Value, make_pair};

/// Convert a list-like Value (List or Pair chain) to a Vec<Value>.
/// Returns None for improper lists or circular lists.
fn collect_list(v: &Value) -> Option<Vec<Value>> {
    match v {
        Value::List(items) => Some(items.iter().map(|s| s.val.clone()).collect()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = v.clone();
            let mut seen = std::collections::HashSet::new();
            loop {
                match &current {
                    Value::List(items) if items.is_empty() => return Some(result),
                    Value::List(items) => {
                        result.extend(items.iter().map(|s| s.val.clone()));
                        return Some(result);
                    }
                    Value::Pair(cell) => {
                        let ptr = Rc::as_ptr(cell) as usize;
                        if !seen.insert(ptr) { return None; }
                        let inner = cell.borrow();
                        result.push(inner.0.clone());
                        let next = inner.1.clone();
                        drop(inner);
                        current = next;
                    }
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

/// Convert a slice of Values to a proper list (chain of Pairs ending in empty List).
fn vec_to_list(items: &[Value]) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.iter().rev() {
        result = make_pair(item.clone(), result);
    }
    result
}

/// Tortoise-and-hare cycle detection for proper list check.
fn is_proper_list(v: &Value) -> bool {
    let mut slow = v.clone();
    let mut fast = v.clone();
    loop {
        // Advance fast by 2
        for _ in 0..2 {
            fast = match &fast {
                Value::List(items) if items.is_empty() => return true,
                Value::List(items) if items.len() == 1 => {
                    // A single-element List followed by implicit null
                    return true; // we'd need more context, but List is always proper
                }
                Value::List(_) => return true, // Vec-based lists are always proper
                Value::Pair(cell) => {
                    let inner = cell.borrow();
                    inner.1.clone()
                }
                _ => return false, // improper
            };
        }
        // Advance slow by 1
        slow = match &slow {
            Value::List(_) => return true,
            Value::Pair(cell) => {
                let inner = cell.borrow();
                inner.1.clone()
            }
            _ => return false,
        };
        // Check if slow == fast (cycle)
        if let (Value::Pair(a), Value::Pair(b)) = (&slow, &fast) {
            if Rc::ptr_eq(a, b) {
                return false; // cycle detected
            }
        }
    }
}

fn format_float(f: f64) -> String {
    let s = format!("{f}");
    if s.contains('.') || s.contains('e') || s.contains('E') {
        s
    } else {
        format!("{f}.0")
    }
}

pub(crate) fn format_float_value(f: f64) -> String {
    format_float(f)
}

fn gcd_val(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub(crate) fn make_rational(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd_val(n, d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

#[derive(Debug, Clone, Copy)]
enum Num {
    Exact(i64, i64),
    Inexact(f64),
}

impl Num {
    fn from_value(v: &Value, span: Span) -> Result<Num, EvalError> {
        match v {
            Value::Integer(n) => Ok(Num::Exact(*n, 1)),
            Value::Rational(n, d) => Ok(Num::Exact(*n, *d)),
            Value::Float(f) => Ok(Num::Inexact(*f)),
            _ => Err(EvalError::Type(format!("expected number, got {v:?}"), span)),
        }
    }

    fn to_value(self) -> Value {
        match self {
            Num::Exact(n, d) => make_rational(n, d),
            Num::Inexact(f) => Value::Float(f),
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            Num::Exact(n, d) => n as f64 / d as f64,
            Num::Inexact(f) => f,
        }
    }

    fn add(self, other: Num) -> Num {
        match (self, other) {
            (Num::Exact(n1, d1), Num::Exact(n2, d2)) => Num::Exact(n1 * d2 + n2 * d1, d1 * d2),
            (a, b) => Num::Inexact(a.to_f64() + b.to_f64()),
        }
    }

    fn sub(self, other: Num) -> Num {
        match (self, other) {
            (Num::Exact(n1, d1), Num::Exact(n2, d2)) => Num::Exact(n1 * d2 - n2 * d1, d1 * d2),
            (a, b) => Num::Inexact(a.to_f64() - b.to_f64()),
        }
    }

    fn mul(self, other: Num) -> Num {
        match (self, other) {
            (Num::Exact(n1, d1), Num::Exact(n2, d2)) => Num::Exact(n1 * n2, d1 * d2),
            (a, b) => Num::Inexact(a.to_f64() * b.to_f64()),
        }
    }

    fn div(self, other: Num, span: Span) -> Result<Num, EvalError> {
        match (self, other) {
            (Num::Exact(n1, d1), Num::Exact(n2, d2)) => {
                if n2 == 0 { return Err(EvalError::DivisionByZero(span)); }
                Ok(Num::Exact(n1 * d2, d1 * n2))
            }
            (a, b) => {
                let d = b.to_f64();
                if d == 0.0 { return Err(EvalError::DivisionByZero(span)); }
                Ok(Num::Inexact(a.to_f64() / d))
            }
        }
    }

    fn neg(self) -> Num {
        match self {
            Num::Exact(n, d) => Num::Exact(-n, d),
            Num::Inexact(f) => Num::Inexact(-f),
        }
    }
}

fn as_integer(val: &Value, span: Span) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {val:?}"), span)),
    }
}

fn compare_nums(args: &[Value], cmp: impl Fn(f64, f64) -> bool, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into(), span));
    }
    let mut prev = Num::from_value(&args[0], span)?.to_f64();
    for a in &args[1..] {
        let curr = Num::from_value(a, span)?.to_f64();
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

pub(crate) fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Integer(n), Value::Rational(n2, d2)) => *n * d2 == *n2,
        (Value::Rational(n1, d1), Value::Integer(n)) => *n1 == *n * d1,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(&x.val, &y.val))
        }
        (Value::Pair(a), Value::Pair(b)) => {
            if Rc::ptr_eq(a, b) { return true; }
            let (a_car, a_cdr) = { let i = a.borrow(); (i.0.clone(), i.1.clone()) };
            let (b_car, b_cdr) = { let i = b.borrow(); (i.0.clone(), i.1.clone()) };
            values_equal(&a_car, &b_car) && values_equal(&a_cdr, &b_cdr)
        }
        // Cross-type: Pair chain vs List
        (Value::Pair(_), Value::List(_)) | (Value::List(_), Value::Pair(_)) => {
            let a_items = collect_list(a);
            let b_items = collect_list(b);
            match (a_items, b_items) {
                (Some(av), Some(bv)) => av.len() == bv.len() && av.iter().zip(bv.iter()).all(|(x, y)| values_equal(x, y)),
                _ => false,
            }
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

pub(crate) fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Integer(n), Value::Rational(n2, d2)) => *n * d2 == *n2,
        (Value::Rational(n1, d1), Value::Integer(n)) => *n1 == *n * d1,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Void, Value::Void) => true,
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn builtin_arithmetic(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum = Num::Exact(0, 1);
            for a in args {
                sum = sum.add(Num::from_value(a, span)?);
            }
            Ok(sum.to_value())
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into(), span));
            }
            if args.len() == 1 {
                return Ok(Num::from_value(&args[0], span)?.neg().to_value());
            }
            let mut result = Num::from_value(&args[0], span)?;
            for a in &args[1..] {
                result = result.sub(Num::from_value(a, span)?);
            }
            Ok(result.to_value())
        }
        "*" => {
            let mut product = Num::Exact(1, 1);
            for a in args {
                product = product.mul(Num::from_value(a, span)?);
            }
            Ok(product.to_value())
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into(), span));
            }
            let mut result = Num::from_value(&args[0], span)?;
            if args.len() == 1 {
                return Ok(Num::Exact(1, 1).div(result, span)?.to_value());
            }
            for a in &args[1..] {
                result = result.div(Num::from_value(a, span)?, span)?;
            }
            Ok(result.to_value())
        }
        "modulo" | "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span));
            }
            let a = as_integer(&args[0], span)?;
            let b = as_integer(&args[1], span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(span)); }
            Ok(Value::Integer(if name == "modulo" { ((a % b) + b) % b } else { a % b }))
        }
        "<" => compare_nums(args, |a, b| a < b, span),
        ">" => compare_nums(args, |a, b| a > b, span),
        "=" => compare_nums(args, |a, b| a == b, span),
        "<=" => compare_nums(args, |a, b| a <= b, span),
        ">=" => compare_nums(args, |a, b| a >= b, span),
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(as_integer(&args[0], span)? % 2 == 0))
        }
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into(), span)); }
            Ok(Value::Integer(as_integer(&args[0], span)?.abs()))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into(), span)); }
            let a = as_integer(&args[0], span)?;
            let b = as_integer(&args[1], span)?;
            if b == 0 { return Err(EvalError::DivisionByZero(span)); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into(), span)); }
            let mut m = as_integer(&args[0], span)?;
            for a in &args[1..] { m = m.min(as_integer(a, span)?); }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into(), span)); }
            let mut m = as_integer(&args[0], span)?;
            for a in &args[1..] { m = m.max(as_integer(a, span)?); }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into(), span)); }
            let base = as_integer(&args[0], span)?;
            let exp = as_integer(&args[1], span)?;
            if exp < 0 { return Err(EvalError::Type("expt: negative exponent".into(), span)); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

type ApplyFn = fn(&Value, &[Value], &Output, Span) -> Result<Value, EvalError>;

fn builtin_list(name: &str, args: &[Value], out: &Output, span: Span, apply_fn: ApplyFn) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into(), span));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].val.clone()),
                Value::Pair(cell) => Ok(cell.borrow().0.clone()),
                _ => Err(EvalError::Type("car: not a pair".into(), span)),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(cell) => Ok(cell.borrow().1.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into(), span)),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into(), span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => {
            if args.is_empty() {
                return Ok(Value::List(vec![]));
            }
            Ok(vec_to_list(args))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into(), span));
            }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("length: not a proper list".into(), span))?;
            Ok(Value::Integer(items.len() as i64))
        }
        "append" => {
            if args.is_empty() { return Ok(Value::List(vec![])); }
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i < args.len() - 1 {
                    let items = collect_list(arg)
                        .ok_or_else(|| EvalError::Type("append: not a list".into(), span))?;
                    result.extend(items);
                } else {
                    // Last arg can be anything (for improper list append)
                    if let Some(items) = collect_list(arg) {
                        result.extend(items);
                    } else {
                        // Improper append: build proper prefix then tack on last
                        let tail = arg.clone();
                        let mut lst = tail;
                        for item in result.into_iter().rev() {
                            lst = make_pair(item, lst);
                        }
                        return Ok(lst);
                    }
                }
            }
            Ok(vec_to_list(&result))
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into(), span)); }
            match &args[0] {
                Value::List(_) => Ok(Value::Boolean(true)),
                Value::Pair(_) => {
                    // Use tortoise-and-hare cycle detection
                    Ok(Value::Boolean(is_proper_list(&args[0])))
                }
                _ => Ok(Value::Boolean(false)),
            }
        }
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into(), span)); }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list-ref: not a list".into(), span))?;
            let idx = as_integer(&args[1], span)? as usize;
            items.get(idx).cloned()
                .ok_or_else(|| EvalError::Type("list-ref: index out of bounds".into(), span))
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into(), span)); }
            let idx = as_integer(&args[1], span)? as usize;
            // Walk the list structure idx times
            let mut current = args[0].clone();
            for _ in 0..idx {
                current = match &current {
                    Value::List(items) if !items.is_empty() => Value::List(items[1..].to_vec()),
                    Value::Pair(cell) => cell.borrow().1.clone(),
                    _ => return Err(EvalError::Type("list-tail: index out of bounds".into(), span)),
                };
            }
            Ok(current)
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into(), span)); }
            let key = &args[0];
            let alist = collect_list(&args[1])
                .ok_or_else(|| EvalError::Type("assoc: not a list".into(), span))?;
            for entry in &alist {
                if let Some(pair_items) = collect_list(entry) {
                    if !pair_items.is_empty() && values_equal(&pair_items[0], key) {
                        return Ok(entry.clone());
                    }
                } else if let Value::List(items) = entry {
                    if !items.is_empty() && values_equal(&items[0].val, key) {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into(), span)); }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                collect_list(a).ok_or_else(|| EvalError::Type("map: expected list".into(), span))
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_fn(func, &call_args, out, span)?);
            }
            Ok(vec_to_list(&result))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into(), span)); }
            let func = &args[0];
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                collect_list(a).ok_or_else(|| EvalError::Type("for-each: expected list".into(), span))
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply_fn(func, &call_args, out, span)?;
            }
            Ok(Value::Void)
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity("reverse requires 1 argument".into(), span)); }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("reverse: not a list".into(), span))?;
            let reversed: Vec<Value> = items.into_iter().rev().collect();
            Ok(vec_to_list(&reversed))
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_string_char_io(name: &str, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match name {
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into(), span)); }
            out.borrow_mut().push_str(&args[0].format_display());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into(), span)); }
            out.borrow_mut().push_str(&args[0].display_value());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into(), span)); }
            out.borrow_mut().push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into(), span)),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into(), span)),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into(), span)); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("substring: expected string".into(), span)),
            };
            let start = as_integer(&args[1], span)? as usize;
            let end = as_integer(&args[2], span)? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into(), span)),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into(), span)); }
            let n = as_integer(&args[0], span)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into(), span)),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into(), span)),
            }
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->list requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => {
                    let items: Vec<Value> = s.chars().map(Value::Char).collect();
                    Ok(vec_to_list(&items))
                }
                _ => Err(EvalError::Type("string->list: expected string".into(), span)),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->string requires 1 argument".into(), span)); }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list->string: expected list".into(), span))?;
            let mut s = String::new();
            for item in &items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected list of chars".into(), span)),
                }
            }
            Ok(Value::Str(s))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type("char->integer: expected char".into(), span)),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into(), span)); }
            let n = as_integer(&args[0], span)?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("integer->char: invalid code point".into(), span)),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into(), span)),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into(), span)); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("string-ref: expected string".into(), span)),
            };
            let idx = as_integer(&args[1], span)? as usize;
            match s.chars().nth(idx) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("string-ref: index out of bounds".into(), span)),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type("char-alphabetic?: expected char".into(), span)),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: expected char".into(), span)),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: expected char".into(), span)),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: expected char".into(), span)),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into(), span)),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into(), span)),
            }
        }
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into(), span)),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into(), span)),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into(), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into(), span)),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: expected string".into(), span)),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type("string-downcase: expected string".into(), span)),
            }
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_type_predicate(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{name} requires 1 argument"), span));
    }
    let val = &args[0];
    let result = match name {
        "number?" => val.is_number(),
        "integer?" => matches!(val, Value::Integer(_)),
        "rational?" => val.is_exact_number(),
        "exact?" => val.is_exact_number(),
        "inexact?" => matches!(val, Value::Float(_)),
        "boolean?" => matches!(val, Value::Boolean(_)),
        "string?" => matches!(val, Value::Str(_)),
        "pair?" => matches!(val, Value::List(items) if !items.is_empty()) || matches!(val, Value::Pair(..)),
        "symbol?" => matches!(val, Value::Symbol(_)),
        "char?" => matches!(val, Value::Char(_)),
        "procedure?" => matches!(val, Value::Lambda(..) | Value::CaseLambda(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..)),
        _ => return Err(EvalError::UnboundVariable(name.into(), span)),
    };
    Ok(Value::Boolean(result))
}

fn builtin_numeric_convert(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into(), span)); }
            let n = Num::from_value(&args[0], span)?;
            Ok(Value::Float(n.to_f64()))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Integer(_) | Value::Rational(..) => Ok(args[0].clone()),
                Value::Float(f) => {
                    let f = *f;
                    if f == f.floor() && f.is_finite() {
                        Ok(Value::Integer(f as i64))
                    } else {
                        let s = format!("{f}");
                        if let Some(dot_pos) = s.find('.') {
                            let decimals = s.len() - dot_pos - 1;
                            let denom = 10_i64.pow(decimals as u32);
                            let numer = (f * denom as f64).round() as i64;
                            Ok(make_rational(numer, denom))
                        } else {
                            Ok(Value::Integer(f as i64))
                        }
                    }
                }
                _ => Err(EvalError::Type("inexact->exact: expected number".into(), span)),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type("numerator: expected rational".into(), span)),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type("denominator: expected rational".into(), span)),
            }
        }
        "gcd" => {
            if args.len() != 2 { return Err(EvalError::Arity("gcd requires 2 arguments".into(), span)); }
            let a = as_integer(&args[0], span)?.abs();
            let b = as_integer(&args[1], span)?.abs();
            let mut x = a; let mut y = b;
            while y != 0 { let t = y; y = x % y; x = t; }
            Ok(Value::Integer(x))
        }
        "lcm" => {
            if args.len() != 2 { return Err(EvalError::Arity("lcm requires 2 arguments".into(), span)); }
            let a = as_integer(&args[0], span)?.abs();
            let b = as_integer(&args[1], span)?.abs();
            if a == 0 || b == 0 { return Ok(Value::Integer(0)); }
            let mut x = a; let mut y = b;
            while y != 0 { let t = y; y = x % y; x = t; }
            Ok(Value::Integer(a / x * b))
        }
        "truncate" => {
            if args.len() != 1 { return Err(EvalError::Arity("truncate requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer(n / d)),
                _ => Err(EvalError::Type("truncate: expected number".into(), span)),
            }
        }
        "round" => {
            if args.len() != 1 { return Err(EvalError::Arity("round requires 1 argument".into(), span)); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer((*n as f64 / *d as f64).round() as i64)),
                _ => Err(EvalError::Type("round: expected number".into(), span)),
            }
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_apply(args: &[Value], out: &Output, span: Span, apply_fn: ApplyFn) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("apply requires at least 2 arguments".into(), span));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let tail = collect_list(last)
        .ok_or_else(|| EvalError::Type("apply: last argument must be a list".into(), span))?;
    let mut all_args: Vec<Value> = args[1..args.len()-1].to_vec();
    all_args.extend(tail);
    apply_fn(func, &all_args, out, span)
}

fn builtin_vector(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "vector" => Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec())))),
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into(), span));
            }
            let len = as_integer(&args[0], span)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("vector-ref requires 2 arguments".into(), span)); }
            let Value::Vector(v) = &args[0] else {
                return Err(EvalError::Type("vector-ref: expected vector".into(), span));
            };
            let idx = as_integer(&args[1], span)? as usize;
            let v = v.borrow();
            if idx >= v.len() {
                return Err(EvalError::Type("vector-ref: index out of bounds".into(), span));
            }
            Ok(v[idx].clone())
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires 3 arguments".into(), span)); }
            let Value::Vector(v) = &args[0] else {
                return Err(EvalError::Type("vector-set!: expected vector".into(), span));
            };
            let idx = as_integer(&args[1], span)? as usize;
            let mut v = v.borrow_mut();
            if idx >= v.len() {
                return Err(EvalError::Type("vector-set!: index out of bounds".into(), span));
            }
            v[idx] = args[2].clone();
            Ok(Value::Void)
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector-length requires 1 argument".into(), span)); }
            let Value::Vector(v) = &args[0] else {
                return Err(EvalError::Type("vector-length: expected vector".into(), span));
            };
            Ok(Value::Integer(v.borrow().len() as i64))
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector? requires 1 argument".into(), span)); }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector->list requires 1 argument".into(), span)); }
            let Value::Vector(v) = &args[0] else {
                return Err(EvalError::Type("vector->list: expected vector".into(), span));
            };
            let items: Vec<Value> = v.borrow().to_vec();
            Ok(vec_to_list(&items))
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->vector requires 1 argument".into(), span)); }
            let items = collect_list(&args[0])
                .ok_or_else(|| EvalError::Type("list->vector: expected list".into(), span))?;
            Ok(Value::Vector(Rc::new(RefCell::new(items))))
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn assoc_lookup<'a>(key: &Value, alist: &'a [Value], cmp: fn(&Value, &Value) -> bool) -> Option<&'a Value> {
    for entry in alist {
        match entry {
            Value::Pair(cell) => {
                let inner = cell.borrow();
                if cmp(&inner.0, key) {
                    return Some(entry);
                }
            }
            Value::List(items) if !items.is_empty() => {
                if cmp(&items[0].val, key) {
                    return Some(entry);
                }
            }
            _ => {}
        }
    }
    None
}

fn member_lookup(key: &Value, items: &[Value], cmp: fn(&Value, &Value) -> bool) -> Option<usize> {
    items.iter().position(|item| cmp(item, key))
}

fn builtin_assoc_member(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span));
    }
    let key = &args[0];
    match name {
        "assq" | "assv" => {
            let alist = collect_list(&args[1])
                .ok_or_else(|| EvalError::Type(format!("{name}: not a list"), span))?;
            match assoc_lookup(key, &alist, values_eqv) {
                Some(entry) => Ok(entry.clone()),
                None => Ok(Value::Boolean(false)),
            }
        }
        "memq" | "memv" => {
            let items = collect_list(&args[1])
                .ok_or_else(|| EvalError::Type(format!("{name}: not a list"), span))?;
            match member_lookup(key, &items, values_eqv) {
                Some(i) => Ok(vec_to_list(&items[i..])),
                None => Ok(Value::Boolean(false)),
            }
        }
        "member" => {
            let items = collect_list(&args[1])
                .ok_or_else(|| EvalError::Type("member: not a list".into(), span))?;
            match member_lookup(key, &items, values_equal) {
                Some(i) => Ok(vec_to_list(&items[i..])),
                None => Ok(Value::Boolean(false)),
            }
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

fn builtin_pair_mutation(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span));
    }
    let Value::Pair(cell) = &args[0] else {
        return Err(EvalError::Type(format!("{name}: not a pair"), span));
    };
    match name {
        "set-car!" => cell.borrow_mut().0 = args[1].clone(),
        "set-cdr!" => cell.borrow_mut().1 = args[1].clone(),
        _ => return Err(EvalError::UnboundVariable(name.into(), span)),
    }
    Ok(Value::Void)
}

fn builtin_string_extra(name: &str, args: &[Value], span: Span) -> Result<Value, EvalError> {
    match name {
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into(), span));
            }
            let len = as_integer(&args[0], span)? as usize;
            let ch = if args.len() == 2 {
                match &args[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into(), span)) }
            } else { '\0' };
            Ok(Value::Str(std::iter::repeat_n(ch, len).collect()))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a { Value::Char(c) => s.push(*c), _ => return Err(EvalError::Type("string: expected char".into(), span)) }
            }
            Ok(Value::Str(s))
        }
        "string<=?" | "string>=?" | "string>?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span)); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    let result = match name {
                        "string<=?" => a <= b,
                        "string>=?" => a >= b,
                        "string>?" => a > b,
                        _ => false,
                    };
                    Ok(Value::Boolean(result))
                }
                _ => Err(EvalError::Type(format!("{name}: expected strings"), span)),
            }
        }
        _ => Err(EvalError::UnboundVariable(name.into(), span)),
    }
}

pub(crate) fn apply_builtin(name: &str, args: &[Value], out: &Output, span: Span, apply_fn: ApplyFn) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "modulo" | "remainder"
        | "<" | ">" | "=" | "<=" | ">="
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "abs" | "quotient" | "min" | "max" | "expt" => builtin_arithmetic(name, args, span),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list?" | "list-ref" | "list-tail" | "assoc"
        | "map" | "for-each" | "reverse" => builtin_list(name, args, out, span, apply_fn),

        "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string"
        | "symbol->string" | "string->symbol"
        | "string-copy" | "string-ref"
        | "string->list" | "list->string"
        | "char->integer" | "integer->char"
        | "char-alphabetic?" | "char-numeric?"
        | "char-upcase" | "char-downcase" | "char=?" | "char<?"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => builtin_string_char_io(name, args, out, span),

        "number?" | "integer?" | "rational?" | "exact?" | "inexact?"
        | "boolean?" | "string?" | "pair?" | "symbol?" | "char?"
        | "procedure?" => builtin_type_predicate(name, args, span),

        "exact->inexact" | "inexact->exact" | "numerator" | "denominator"
        | "gcd" | "lcm" | "truncate" | "round" => builtin_numeric_convert(name, args, span),

        "apply" => builtin_apply(args, out, span, apply_fn),

        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into(), span)); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "eq?" | "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity(format!("{name} requires 2 arguments"), span)); }
            Ok(Value::Boolean(values_eqv(&args[0], &args[1])))
        }

        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector?" | "vector->list" | "list->vector" => builtin_vector(name, args, span),

        "assq" | "assv" | "memq" | "memv" | "member" => builtin_assoc_member(name, args, span),

        "error" => {
            let msg = if args.is_empty() {
                "error".to_string()
            } else {
                args.iter().map(|a| a.display_value()).collect::<Vec<_>>().join(" ")
            };
            Err(EvalError::Type(msg, span))
        }

        "set-car!" | "set-cdr!" => builtin_pair_mutation(name, args, span),

        "make-string" | "string" | "string<=?" | "string>=?" | "string>?" =>
            builtin_string_extra(name, args, span),

        _ => {
            // Dynamic c..r handler: c[ad]+r
            let bytes = name.as_bytes();
            if bytes.len() >= 3 && bytes[0] == b'c' && bytes[bytes.len()-1] == b'r'
                && bytes[1..bytes.len()-1].iter().all(|&b| b == b'a' || b == b'd')
            {
                if args.len() != 1 { return Err(EvalError::Arity(format!("{name} requires 1 argument"), span)); }
                let ops = &bytes[1..bytes.len()-1];
                let mut val = args[0].clone();
                for &op in ops.iter().rev() {
                    val = if op == b'a' {
                        apply_builtin("car", &[val], out, span, apply_fn)?
                    } else {
                        apply_builtin("cdr", &[val], out, span, apply_fn)?
                    };
                }
                return Ok(val);
            }
            Err(EvalError::UnboundVariable(name.into(), span))
        }
    }
}
