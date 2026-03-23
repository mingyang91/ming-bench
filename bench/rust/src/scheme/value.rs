use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::Span;

/// Whether a Scheme string can be mutated via `string-set!`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Debug, Clone)]
pub struct SyntaxRules {
    pub literals: Vec<String>,
    pub rules: Vec<(Value, Value)>,
    pub def_env: Env,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64, Span),
    Rational(i64, i64, Span), // numerator, denominator (always simplified, denom > 0)
    Float(f64, Span),
    Boolean(bool, Span),
    String(Rc<RefCell<String>>, Mutability, Span),
    Symbol(String, Span),
    Char(char, Span),
    List(Vec<Value>, Span),
    Vector(Rc<RefCell<Vec<Value>>>, Span),
    Closure {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Box<Value>,
        env: Env,
    },
    Continuation(u64),
    Macro(SyntaxRules),
    Values(Vec<Value>),
    Void,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Create a rational or integer value, always in simplified form.
pub fn make_rational(numer: i64, denom: i64, span: Span) -> Value {
    assert!(denom != 0, "rational with zero denominator");
    let sign = if denom < 0 { -1 } else { 1 };
    let n = numer * sign;
    let d = denom * sign;
    let g = gcd(n, d);
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Value::Integer(n, span)
    } else {
        Value::Rational(n, d, span)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a, _), Value::Integer(b, _)) => a == b,
            (Value::Rational(an, ad, _), Value::Rational(bn, bd, _)) => an == bn && ad == bd,
            (Value::Float(a, _), Value::Float(b, _)) => a == b,
            (Value::Boolean(a, _), Value::Boolean(b, _)) => a == b,
            (Value::String(a, _, _), Value::String(b, _, _)) => *a.borrow() == *b.borrow(),
            (Value::Symbol(a, _), Value::Symbol(b, _)) => a == b,
            (Value::Char(a, _), Value::Char(b, _)) => a == b,
            (Value::List(a, _), Value::List(b, _)) => a == b,
            (Value::Closure { params: p1, rest_param: r1, body: b1, env: e1 },
             Value::Closure { params: p2, rest_param: r2, body: b2, env: e2 }) => {
                p1 == p2 && r1 == r2 && b1 == b2 && e1 == e2
            }
            (Value::Vector(a, _), Value::Vector(b, _)) => *a.borrow() == *b.borrow(),
            (Value::Continuation(a), Value::Continuation(b)) => a == b,
            (Value::Values(a), Value::Values(b)) => a == b,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n, _) => write!(f, "{n}"),
            Value::Rational(n, d, _) => write!(f, "{n}/{d}"),
            Value::Float(x, _) => {
                if x.fract() == 0.0 && x.is_finite() {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
            Value::Boolean(true, _) => write!(f, "#t"),
            Value::Boolean(false, _) => write!(f, "#f"),
            Value::String(s, _, _) => write!(f, "\"{}\"", s.borrow()),
            Value::Symbol(s, _) => write!(f, "{s}"),
            Value::Char(c, _) => write!(f, "#\\{c}"),
            Value::List(elems, _) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Vector(elems, _) => {
                write!(f, "#(")?;
                let borrowed = elems.borrow();
                for (i, elem) in borrowed.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, ")")
            }
            Value::Closure { .. } => write!(f, "#<procedure>"),
            Value::Continuation(_) => write!(f, "#<continuation>"),
            Value::Macro(_) => write!(f, "#<macro>"),
            Value::Values(_) => write!(f, "#<values>"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes, chars as bare characters.
    pub fn display_string(&self) -> String {
        match self {
            Value::String(s, _, _) => s.borrow().clone(),
            Value::Char(c, _) => c.to_string(),
            Value::List(elems, _) => {
                let mut out = String::from("(");
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&elem.display_string());
                }
                out.push(')');
                out
            }
            Value::Vector(elems, _) => {
                let borrowed = elems.borrow();
                let mut out = String::from("#(");
                for (i, elem) in borrowed.iter().enumerate() {
                    if i > 0 {
                        out.push(' ');
                    }
                    out.push_str(&elem.display_string());
                }
                out.push(')');
                out
            }
            Value::Integer(_, _)
            | Value::Rational(_, _, _)
            | Value::Float(_, _)
            | Value::Boolean(_, _)
            | Value::Symbol(_, _)
            | Value::Closure { .. }
            | Value::Continuation(_)
            | Value::Macro(_)
            | Value::Values(_)
            | Value::Void => self.to_string(),
        }
    }
}

impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Integer(_, s)
            | Value::Rational(_, _, s)
            | Value::Float(_, s)
            | Value::Boolean(_, s)
            | Value::String(_, _, s)
            | Value::Symbol(_, s)
            | Value::Char(_, s)
            | Value::List(_, s)
            | Value::Vector(_, s) => *s,
            Value::Closure { .. } => Span::default(),
            Value::Continuation(_) => Span::default(),
            Value::Macro(_) => Span::default(),
            Value::Values(_) => Span::default(),
            Value::Void => Span::default(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false, _))
    }

    // Convenience constructors for runtime-created values (no source position)
    pub fn int(n: i64) -> Self {
        Value::Integer(n, Span::default())
    }
    pub fn bool(b: bool) -> Self {
        Value::Boolean(b, Span::default())
    }
    pub fn string(s: String) -> Self {
        Value::String(Rc::new(RefCell::new(s)), Mutability::Mutable, Span::default())
    }
    pub fn immutable_string(s: String, span: Span) -> Self {
        Value::String(Rc::new(RefCell::new(s)), Mutability::Immutable, span)
    }
    pub fn symbol(s: String) -> Self {
        Value::Symbol(s, Span::default())
    }
    pub fn list(elems: Vec<Value>) -> Self {
        Value::List(elems, Span::default())
    }
    pub fn float(x: f64) -> Self {
        Value::Float(x, Span::default())
    }
    pub fn vector(elems: Vec<Value>) -> Self {
        Value::Vector(Rc::new(RefCell::new(elems)), Span::default())
    }
}
