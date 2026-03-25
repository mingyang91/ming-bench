use std::cell::RefCell;

use super::{apply_function, make_rational, Env, EnvRef, EvalError, Span, Value};

thread_local! {
    pub(super) static OUTPUT_BUFFER: RefCell<String> = const { RefCell::new(String::new()) };
}

/// Exact numeric representation for arithmetic.
#[derive(Clone, Copy)]
enum Num {
    Int(i64),
    Rat(i64, i64),
    Flo(f64),
}

impl Num {
    fn from_value(v: &Value, span: Span, op: &str) -> Result<Num, EvalError> {
        match v {
            Value::Integer(n) => Ok(Num::Int(*n)),
            Value::Rational(n, d) => Ok(Num::Rat(*n, *d)),
            Value::Float(x) => Ok(Num::Flo(*x)),
            _ => Err(EvalError::Type(format!("at {span}: {op} expects numbers"))),
        }
    }

    fn to_value(self) -> Value {
        match self {
            Num::Int(n) => Value::Integer(n),
            Num::Rat(n, d) => make_rational(n, d),
            Num::Flo(x) => Value::Float(x),
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            Num::Int(n) => n as f64,
            Num::Rat(n, d) => n as f64 / d as f64,
            Num::Flo(x) => x,
        }
    }

    fn add(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flo(a), b) | (b, Num::Flo(a)) => Num::Flo(a + b.to_f64()),
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bd + bn * ad, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) | (Num::Rat(bn, bd), Num::Int(a)) => {
                Num::Rat(a * bd + bn, bd)
            }
        }
    }

    fn sub(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flo(a), b) => Num::Flo(a - b.to_f64()),
            (a, Num::Flo(b)) => Num::Flo(a.to_f64() - b),
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bd - bn * ad, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) => Num::Rat(a * bd - bn, bd),
            (Num::Rat(an, ad), Num::Int(b)) => Num::Rat(an - b * ad, ad),
        }
    }

    fn mul(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flo(a), b) | (b, Num::Flo(a)) => Num::Flo(a * b.to_f64()),
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => Num::Rat(an * bn, ad * bd),
            (Num::Int(a), Num::Rat(bn, bd)) | (Num::Rat(bn, bd), Num::Int(a)) => {
                Num::Rat(a * bn, bd)
            }
        }
    }

    fn div(self, other: Num, span: Span) -> Result<Num, EvalError> {
        match (self, other) {
            (Num::Flo(a), b) => Ok(Num::Flo(a / b.to_f64())),
            (a, Num::Flo(b)) => Ok(Num::Flo(a.to_f64() / b)),
            (_, Num::Int(0)) | (_, Num::Rat(0, _)) => Err(EvalError::DivisionByZero(span)),
            (Num::Int(a), Num::Int(b)) => Ok(Num::Rat(a, b)),
            (Num::Rat(an, ad), Num::Rat(bn, bd)) => Ok(Num::Rat(an * bd, ad * bn)),
            (Num::Int(a), Num::Rat(bn, bd)) => Ok(Num::Rat(a * bd, bn)),
            (Num::Rat(an, ad), Num::Int(b)) => Ok(Num::Rat(an, ad * b)),
        }
    }
}

fn builtin_add(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let mut sum = Num::Int(0);
    for a in args {
        sum = sum.add(Num::from_value(a, span, "+")?);
    }
    Ok(sum.to_value())
}

fn builtin_sub(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("at {span}: - requires at least 1 argument")));
    }
    let first = Num::from_value(&args[0], span, "-")?;
    if args.len() == 1 {
        return Ok(Num::Int(0).sub(first).to_value());
    }
    let mut result = first;
    for a in &args[1..] {
        result = result.sub(Num::from_value(a, span, "-")?);
    }
    Ok(result.to_value())
}

fn builtin_mul(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let mut product = Num::Int(1);
    for a in args {
        product = product.mul(Num::from_value(a, span, "*")?);
    }
    Ok(product.to_value())
}

fn builtin_div(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("at {span}: / requires at least 2 arguments")));
    }
    let mut result = Num::from_value(&args[0], span, "/")?;
    for a in &args[1..] {
        result = result.div(Num::from_value(a, span, "/")?, span)?;
    }
    Ok(result.to_value())
}

fn num_cmp(a: &Value, b: &Value, span: Span, op: &str) -> Result<f64, EvalError> {
    let an = Num::from_value(a, span, op)?;
    let bn = Num::from_value(b, span, op)?;
    // For exact-exact comparison, use cross-multiply to avoid float imprecision
    match (an, bn) {
        (Num::Int(a), Num::Int(b)) => Ok((a - b) as f64),
        (Num::Rat(an, ad), Num::Rat(bn, bd)) => Ok((an * bd - bn * ad) as f64),
        (Num::Int(a), Num::Rat(bn, bd)) => Ok((a * bd - bn) as f64),
        (Num::Rat(an, ad), Num::Int(b)) => Ok((an - b * ad) as f64),
        _ => Ok(an.to_f64() - bn.to_f64()),
    }
}

fn builtin_lt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: < requires 2 arguments")));
    }
    Ok(Value::Boolean(num_cmp(&args[0], &args[1], span, "<")? < 0.0))
}

fn builtin_gt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: > requires 2 arguments")));
    }
    Ok(Value::Boolean(num_cmp(&args[0], &args[1], span, ">")? > 0.0))
}

fn builtin_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: = requires 2 arguments")));
    }
    let a = Num::from_value(&args[0], span, "=")?;
    let b = Num::from_value(&args[1], span, "=")?;
    let eq = a.to_f64() == b.to_f64();
    Ok(Value::Boolean(eq))
}

fn builtin_le(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: <= requires 2 arguments")));
    }
    Ok(Value::Boolean(num_cmp(&args[0], &args[1], span, "<=")? <= 0.0))
}

fn builtin_not(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: not requires 1 argument")));
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: cons requires 2 arguments")));
    }
    match &args[1] {
        Value::List(elems) => {
            let mut new = vec![args[0].clone()];
            new.extend(elems.iter().cloned());
            Ok(Value::List(new))
        }
        Value::Nil => Ok(Value::List(vec![args[0].clone()])),
        _ => {
            Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
    }
}

fn builtin_car(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: car requires 1 argument")));
    }
    match &args[0] {
        Value::Pair(a, _) => Ok(*a.clone()),
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        _ => Err(EvalError::Type(format!("at {span}: car: not a pair"))),
    }
}

fn builtin_cdr(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: cdr requires 1 argument")));
    }
    match &args[0] {
        Value::Pair(_, b) => Ok(*b.clone()),
        Value::List(elems) if !elems.is_empty() => {
            if elems.len() == 1 {
                Ok(Value::Nil)
            } else {
                Ok(Value::List(elems[1..].to_vec()))
            }
        }
        _ => Err(EvalError::Type(format!("at {span}: cdr: not a pair"))),
    }
}

fn builtin_null(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: null? requires 1 argument")));
    }
    let is_null = matches!(&args[0], Value::Nil) || matches!(&args[0], Value::List(v) if v.is_empty());
    Ok(Value::Boolean(is_null))
}

fn builtin_list(args: &[Value], _span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(args.to_vec()))
    }
}

fn builtin_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: length requires 1 argument")));
    }
    match &args[0] {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        Value::Nil => Ok(Value::Integer(0)),
        _ => Err(EvalError::Type(format!("at {span}: length: not a list"))),
    }
}

fn builtin_number_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: number? requires 1 argument")));
    }
    Ok(Value::Boolean(args[0].is_numeric()))
}

fn builtin_boolean_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: boolean? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
}

fn builtin_string_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
}

fn builtin_pair_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: pair? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(v) if !v.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
}

fn builtin_symbol_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: symbol? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
}

fn builtin_append(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        match arg {
            Value::List(elems) => result.extend(elems.iter().cloned()),
            Value::Nil => {}
            _ if i == args.len() - 1 => {
                result.push(arg.clone());
            }
            _ => return Err(EvalError::Type(format!("at {span}: append: not a list"))),
        }
    }
    if result.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(result))
    }
}

fn builtin_display(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: display requires 1 argument")));
    }
    let s = args[0].display_fmt();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_write(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: write requires 1 argument")));
    }
    let s = args[0].to_string();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity(format!("at {span}: newline takes 0 arguments")));
    }
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push('\n'));
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type(format!("at {span}: string-append expects strings"))),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string-length requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.chars().count() as i64)),
        _ => Err(EvalError::Type(format!("at {span}: string-length expects a string"))),
    }
}

fn builtin_substring(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("at {span}: substring requires 3 arguments")));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::Str(s), Value::Integer(start), Value::Integer(end)) => {
            let start = *start as usize;
            let end = *end as usize;
            let chars: Vec<char> = s.chars().collect();
            if start > end || end > chars.len() {
                return Err(EvalError::Type(format!("at {span}: substring: index out of range")));
            }
            Ok(Value::Str(chars[start..end].iter().collect()))
        }
        _ => Err(EvalError::Type(format!("at {span}: substring expects (string int int)"))),
    }
}

fn builtin_string_to_number(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string->number requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type(format!("at {span}: string->number expects a string"))),
    }
}

fn builtin_number_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: number->string requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Str(n.to_string())),
        Value::Float(x) => Ok(Value::Str(format!("{}", x))),
        Value::Rational(n, d) => Ok(Value::Str(format!("{}/{}", n, d))),
        _ => Err(EvalError::Type(format!("at {span}: number->string expects a number"))),
    }
}

fn builtin_symbol_to_string(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: symbol->string requires 1 argument")));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type(format!("at {span}: symbol->string expects a symbol"))),
    }
}

fn builtin_string_to_symbol(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string->symbol requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type(format!("at {span}: string->symbol expects a string"))),
    }
}

fn builtin_string_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: string-ref requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Str(s), Value::Integer(idx)) => {
            let idx = *idx as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type(format!("at {span}: string-ref: index out of range")));
            }
            Ok(Value::Char(chars[idx]))
        }
        _ => Err(EvalError::Type(format!("at {span}: string-ref expects (string int)"))),
    }
}

fn builtin_eq_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: eq? requires 2 arguments")));
    }
    Ok(Value::Boolean(args[0] == args[1]))
}

fn builtin_equal_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: equal? requires 2 arguments")));
    }
    Ok(Value::Boolean(args[0] == args[1]))
}

fn builtin_ge(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: >= requires 2 arguments")));
    }
    Ok(Value::Boolean(num_cmp(&args[0], &args[1], span, ">=")? >= 0.0))
}

// --- L09 Numeric ---

fn builtin_abs(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: abs requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _ => Err(EvalError::Type(format!("at {span}: abs expects a number"))),
    }
}

fn builtin_modulo(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: modulo requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Integer(_), Value::Integer(0)) => Err(EvalError::DivisionByZero(span)),
        (Value::Integer(a), Value::Integer(b)) => {
            let r = a % b;
            let result = if r != 0 && (r ^ b) < 0 { r + b } else { r };
            Ok(Value::Integer(result))
        }
        _ => Err(EvalError::Type(format!("at {span}: modulo expects numbers"))),
    }
}

fn builtin_remainder(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: remainder requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Integer(_), Value::Integer(0)) => Err(EvalError::DivisionByZero(span)),
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a % b)),
        _ => Err(EvalError::Type(format!("at {span}: remainder expects numbers"))),
    }
}

fn builtin_quotient(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: quotient requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Integer(_), Value::Integer(0)) => Err(EvalError::DivisionByZero(span)),
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
        _ => Err(EvalError::Type(format!("at {span}: quotient expects numbers"))),
    }
}

fn builtin_min(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("at {span}: min requires at least 1 argument")));
    }
    let mut result = match &args[0] {
        Value::Integer(n) => *n,
        _ => return Err(EvalError::Type(format!("at {span}: min expects numbers"))),
    };
    for a in &args[1..] {
        match a {
            Value::Integer(n) => {
                if *n < result {
                    result = *n;
                }
            }
            _ => return Err(EvalError::Type(format!("at {span}: min expects numbers"))),
        }
    }
    Ok(Value::Integer(result))
}

fn builtin_max(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("at {span}: max requires at least 1 argument")));
    }
    let mut result = match &args[0] {
        Value::Integer(n) => *n,
        _ => return Err(EvalError::Type(format!("at {span}: max expects numbers"))),
    };
    for a in &args[1..] {
        match a {
            Value::Integer(n) => {
                if *n > result {
                    result = *n;
                }
            }
            _ => return Err(EvalError::Type(format!("at {span}: max expects numbers"))),
        }
    }
    Ok(Value::Integer(result))
}

fn builtin_expt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: expt requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Integer(base), Value::Integer(exp)) => {
            if *exp < 0 {
                Ok(Value::Integer(0)) // integer division truncates
            } else {
                Ok(Value::Integer(base.pow(*exp as u32)))
            }
        }
        _ => Err(EvalError::Type(format!("at {span}: expt expects numbers"))),
    }
}

// --- L09 Numeric Predicates ---

fn builtin_zero_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: zero? requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n == 0)),
        _ => Err(EvalError::Type(format!("at {span}: zero? expects a number"))),
    }
}

fn builtin_positive_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: positive? requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n > 0)),
        _ => Err(EvalError::Type(format!("at {span}: positive? expects a number"))),
    }
}

fn builtin_negative_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: negative? requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n < 0)),
        _ => Err(EvalError::Type(format!("at {span}: negative? expects a number"))),
    }
}

fn builtin_odd_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: odd? requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 != 0)),
        _ => Err(EvalError::Type(format!("at {span}: odd? expects a number"))),
    }
}

fn builtin_even_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: even? requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 == 0)),
        _ => Err(EvalError::Type(format!("at {span}: even? expects a number"))),
    }
}

// --- L09 Char ---

fn builtin_char_alphabetic(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: char-alphabetic? requires 1 argument")));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
        _ => Err(EvalError::Type(format!("at {span}: char-alphabetic? expects a character"))),
    }
}

fn builtin_char_numeric(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: char-numeric? requires 1 argument")));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
        _ => Err(EvalError::Type(format!("at {span}: char-numeric? expects a character"))),
    }
}

fn builtin_char_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: char=? requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type(format!("at {span}: char=? expects characters"))),
    }
}

fn builtin_char_lt(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: char<? requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type(format!("at {span}: char<? expects characters"))),
    }
}

fn builtin_char_upcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: char-upcase requires 1 argument")));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
        _ => Err(EvalError::Type(format!("at {span}: char-upcase expects a character"))),
    }
}

fn builtin_char_downcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: char-downcase requires 1 argument")));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
        _ => Err(EvalError::Type(format!("at {span}: char-downcase expects a character"))),
    }
}

// --- L09 String ---

fn builtin_string_eq_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: string=? requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type(format!("at {span}: string=? expects strings"))),
    }
}

fn builtin_string_lt_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: string<? requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type(format!("at {span}: string<? expects strings"))),
    }
}

fn builtin_string_ci_eq(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: string-ci=? requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
        _ => Err(EvalError::Type(format!("at {span}: string-ci=? expects strings"))),
    }
}

fn builtin_string_upcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string-upcase requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
        _ => Err(EvalError::Type(format!("at {span}: string-upcase expects a string"))),
    }
}

fn builtin_string_downcase(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string-downcase requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
        _ => Err(EvalError::Type(format!("at {span}: string-downcase expects a string"))),
    }
}

// --- L09 List ---

fn builtin_list_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: list? requires 1 argument")));
    }
    let is_list = matches!(&args[0], Value::Nil | Value::List(_));
    Ok(Value::Boolean(is_list))
}

fn builtin_list_ref(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: list-ref requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::List(elems), Value::Integer(idx)) => {
            let idx = *idx as usize;
            if idx >= elems.len() {
                return Err(EvalError::Type(format!("at {span}: list-ref: index out of range")));
            }
            Ok(elems[idx].clone())
        }
        _ => Err(EvalError::Type(format!("at {span}: list-ref expects (list int)"))),
    }
}

fn builtin_list_tail(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: list-tail requires 2 arguments")));
    }
    match (&args[0], &args[1]) {
        (Value::List(elems), Value::Integer(idx)) => {
            let idx = *idx as usize;
            if idx > elems.len() {
                return Err(EvalError::Type(format!("at {span}: list-tail: index out of range")));
            }
            let tail = &elems[idx..];
            if tail.is_empty() {
                Ok(Value::Nil)
            } else {
                Ok(Value::List(tail.to_vec()))
            }
        }
        (Value::Nil, Value::Integer(0)) => Ok(Value::Nil),
        _ => Err(EvalError::Type(format!("at {span}: list-tail expects (list int)"))),
    }
}

fn builtin_assoc(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("at {span}: assoc requires 2 arguments")));
    }
    let key = &args[0];
    match &args[1] {
        Value::List(alist) => {
            for entry in alist {
                if let Value::List(pair) = entry {
                    if !pair.is_empty() && pair[0] == *key {
                        return Ok(entry.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        Value::Nil => Ok(Value::Boolean(false)),
        _ => Err(EvalError::Type(format!("at {span}: assoc expects a list"))),
    }
}

fn builtin_map(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("at {span}: map requires at least 2 arguments")));
    }
    let func = &args[0];
    let lists: Vec<&[Value]> = args[1..].iter().map(|a| match a {
        Value::List(elems) => Ok(elems.as_slice()),
        Value::Nil => Ok(&[] as &[Value]),
        _ => Err(EvalError::Type(format!("at {span}: map expects lists"))),
    }).collect::<Result<Vec<_>, _>>()?;

    let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
    let mut result = Vec::with_capacity(min_len);

    for i in 0..min_len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        result.push(apply_function(func, &call_args, span)?);
    }

    if result.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::List(result))
    }
}

fn builtin_for_each(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("at {span}: for-each requires at least 2 arguments")));
    }
    let func = &args[0];
    let lists: Vec<&[Value]> = args[1..].iter().map(|a| match a {
        Value::List(elems) => Ok(elems.as_slice()),
        Value::Nil => Ok(&[] as &[Value]),
        _ => Err(EvalError::Type(format!("at {span}: for-each expects lists"))),
    }).collect::<Result<Vec<_>, _>>()?;

    let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);

    for i in 0..min_len {
        let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
        apply_function(func, &call_args, span)?;
    }

    Ok(Value::Void)
}

fn builtin_char_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: char? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
}

fn builtin_string_copy(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: string-copy requires 1 argument")));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type(format!("at {span}: string-copy expects a string"))),
    }
}

// --- L11 Exact Arithmetic & Rationals ---

fn builtin_exact_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: exact? requires 1 argument")));
    }
    Ok(Value::Boolean(args[0].is_exact()))
}

fn builtin_inexact_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: inexact? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
}

fn builtin_exact_to_inexact(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: exact->inexact requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Float(*n as f64)),
        Value::Rational(n, d) => Ok(Value::Float(*n as f64 / *d as f64)),
        Value::Float(x) => Ok(Value::Float(*x)),
        _ => Err(EvalError::Type(format!("at {span}: exact->inexact expects a number"))),
    }
}

fn builtin_inexact_to_exact(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: inexact->exact requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, d) => Ok(make_rational(*n, *d)),
        Value::Float(x) => {
            // Convert float to rational via continued fraction / power-of-2 approach
            // Simple approach: multiply by large power of 2 and simplify
            if x.fract() == 0.0 {
                Ok(Value::Integer(*x as i64))
            } else {
                // Use ratio approximation: x = n/d
                let denom = 1_i64 << 53; // 2^53
                let numer = (*x * denom as f64).round() as i64;
                Ok(make_rational(numer, denom))
            }
        }
        _ => Err(EvalError::Type(format!("at {span}: inexact->exact expects a number"))),
    }
}

fn builtin_numerator(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: numerator requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, _) => Ok(Value::Integer(*n)),
        _ => Err(EvalError::Type(format!("at {span}: numerator expects a rational number"))),
    }
}

fn builtin_denominator(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: denominator requires 1 argument")));
    }
    match &args[0] {
        Value::Integer(_) => Ok(Value::Integer(1)),
        Value::Rational(_, d) => Ok(Value::Integer(*d)),
        _ => Err(EvalError::Type(format!("at {span}: denominator expects a rational number"))),
    }
}

fn builtin_integer_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: integer? requires 1 argument")));
    }
    let is_int = match &args[0] {
        Value::Integer(_) => true,
        Value::Rational(_, _) => false, // simplified rationals with denom=1 become Integer
        Value::Float(x) => x.fract() == 0.0,
        _ => false,
    };
    Ok(Value::Boolean(is_int))
}

fn builtin_rational_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: rational? requires 1 argument")));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(_, _))))
}

pub(super) fn builtin_apply(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("at {span}: apply requires at least 2 arguments")));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    match last {
        Value::List(elems) => call_args.extend(elems.iter().cloned()),
        Value::Nil => {}
        _ => return Err(EvalError::Type(format!("at {span}: apply: last argument must be a list"))),
    }
    apply_function(func, &call_args, span)
}

fn builtin_procedure_pred(args: &[Value], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("at {span}: procedure? expects 1 argument")));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Builtin(..) | Value::Lambda { .. } | Value::CaseLambda { .. })))
}

pub(super) fn default_env() -> EnvRef {
    let env = Env::new(None);
    {
        let mut e = env.borrow_mut();
        e.set("+".into(), Value::Builtin("+".into(), builtin_add));
        e.set("-".into(), Value::Builtin("-".into(), builtin_sub));
        e.set("*".into(), Value::Builtin("*".into(), builtin_mul));
        e.set("/".into(), Value::Builtin("/".into(), builtin_div));
        e.set("<".into(), Value::Builtin("<".into(), builtin_lt));
        e.set(">".into(), Value::Builtin(">".into(), builtin_gt));
        e.set("=".into(), Value::Builtin("=".into(), builtin_eq));
        e.set("<=".into(), Value::Builtin("<=".into(), builtin_le));
        e.set("not".into(), Value::Builtin("not".into(), builtin_not));
        e.set("cons".into(), Value::Builtin("cons".into(), builtin_cons));
        e.set("car".into(), Value::Builtin("car".into(), builtin_car));
        e.set("cdr".into(), Value::Builtin("cdr".into(), builtin_cdr));
        e.set("null?".into(), Value::Builtin("null?".into(), builtin_null));
        e.set("list".into(), Value::Builtin("list".into(), builtin_list));
        e.set("length".into(), Value::Builtin("length".into(), builtin_length));
        e.set("number?".into(), Value::Builtin("number?".into(), builtin_number_pred));
        e.set("boolean?".into(), Value::Builtin("boolean?".into(), builtin_boolean_pred));
        e.set("string?".into(), Value::Builtin("string?".into(), builtin_string_pred));
        e.set("pair?".into(), Value::Builtin("pair?".into(), builtin_pair_pred));
        e.set("symbol?".into(), Value::Builtin("symbol?".into(), builtin_symbol_pred));
        e.set("append".into(), Value::Builtin("append".into(), builtin_append));
        e.set("display".into(), Value::Builtin("display".into(), builtin_display));
        e.set("write".into(), Value::Builtin("write".into(), builtin_write));
        e.set("newline".into(), Value::Builtin("newline".into(), builtin_newline));
        e.set("string-append".into(), Value::Builtin("string-append".into(), builtin_string_append));
        e.set("string-length".into(), Value::Builtin("string-length".into(), builtin_string_length));
        e.set("substring".into(), Value::Builtin("substring".into(), builtin_substring));
        e.set("string->number".into(), Value::Builtin("string->number".into(), builtin_string_to_number));
        e.set("number->string".into(), Value::Builtin("number->string".into(), builtin_number_to_string));
        e.set("symbol->string".into(), Value::Builtin("symbol->string".into(), builtin_symbol_to_string));
        e.set("string->symbol".into(), Value::Builtin("string->symbol".into(), builtin_string_to_symbol));
        e.set("string-ref".into(), Value::Builtin("string-ref".into(), builtin_string_ref));
        e.set("char?".into(), Value::Builtin("char?".into(), builtin_char_pred));
        e.set("string-copy".into(), Value::Builtin("string-copy".into(), builtin_string_copy));
        e.set("eq?".into(), Value::Builtin("eq?".into(), builtin_eq_pred));
        e.set("equal?".into(), Value::Builtin("equal?".into(), builtin_equal_pred));
        e.set(">=".into(), Value::Builtin(">=".into(), builtin_ge));
        // L09 numeric
        e.set("abs".into(), Value::Builtin("abs".into(), builtin_abs));
        e.set("modulo".into(), Value::Builtin("modulo".into(), builtin_modulo));
        e.set("remainder".into(), Value::Builtin("remainder".into(), builtin_remainder));
        e.set("quotient".into(), Value::Builtin("quotient".into(), builtin_quotient));
        e.set("min".into(), Value::Builtin("min".into(), builtin_min));
        e.set("max".into(), Value::Builtin("max".into(), builtin_max));
        e.set("expt".into(), Value::Builtin("expt".into(), builtin_expt));
        // L09 numeric predicates
        e.set("zero?".into(), Value::Builtin("zero?".into(), builtin_zero_pred));
        e.set("positive?".into(), Value::Builtin("positive?".into(), builtin_positive_pred));
        e.set("negative?".into(), Value::Builtin("negative?".into(), builtin_negative_pred));
        e.set("odd?".into(), Value::Builtin("odd?".into(), builtin_odd_pred));
        e.set("even?".into(), Value::Builtin("even?".into(), builtin_even_pred));
        // L09 char
        e.set("char-alphabetic?".into(), Value::Builtin("char-alphabetic?".into(), builtin_char_alphabetic));
        e.set("char-numeric?".into(), Value::Builtin("char-numeric?".into(), builtin_char_numeric));
        e.set("char=?".into(), Value::Builtin("char=?".into(), builtin_char_eq));
        e.set("char<?".into(), Value::Builtin("char<?".into(), builtin_char_lt));
        e.set("char-upcase".into(), Value::Builtin("char-upcase".into(), builtin_char_upcase));
        e.set("char-downcase".into(), Value::Builtin("char-downcase".into(), builtin_char_downcase));
        // L09 string
        e.set("string=?".into(), Value::Builtin("string=?".into(), builtin_string_eq_pred));
        e.set("string<?".into(), Value::Builtin("string<?".into(), builtin_string_lt_pred));
        e.set("string-ci=?".into(), Value::Builtin("string-ci=?".into(), builtin_string_ci_eq));
        e.set("string-upcase".into(), Value::Builtin("string-upcase".into(), builtin_string_upcase));
        e.set("string-downcase".into(), Value::Builtin("string-downcase".into(), builtin_string_downcase));
        // L09 list
        e.set("list?".into(), Value::Builtin("list?".into(), builtin_list_pred));
        e.set("list-ref".into(), Value::Builtin("list-ref".into(), builtin_list_ref));
        e.set("list-tail".into(), Value::Builtin("list-tail".into(), builtin_list_tail));
        e.set("assoc".into(), Value::Builtin("assoc".into(), builtin_assoc));
        e.set("map".into(), Value::Builtin("map".into(), builtin_map));
        e.set("for-each".into(), Value::Builtin("for-each".into(), builtin_for_each));
        // L11 exact arithmetic & rationals
        e.set("exact?".into(), Value::Builtin("exact?".into(), builtin_exact_pred));
        e.set("inexact?".into(), Value::Builtin("inexact?".into(), builtin_inexact_pred));
        e.set("exact->inexact".into(), Value::Builtin("exact->inexact".into(), builtin_exact_to_inexact));
        e.set("inexact->exact".into(), Value::Builtin("inexact->exact".into(), builtin_inexact_to_exact));
        e.set("numerator".into(), Value::Builtin("numerator".into(), builtin_numerator));
        e.set("denominator".into(), Value::Builtin("denominator".into(), builtin_denominator));
        e.set("integer?".into(), Value::Builtin("integer?".into(), builtin_integer_pred));
        e.set("rational?".into(), Value::Builtin("rational?".into(), builtin_rational_pred));
        // L13 case-lambda
        e.set("procedure?".into(), Value::Builtin("procedure?".into(), builtin_procedure_pred));
        // apply is handled specially in eval, but needs to be a value for (define f apply)
        e.set("apply".into(), Value::Builtin("apply".into(), |_args, _span| unreachable!()));
    }

    env
}
