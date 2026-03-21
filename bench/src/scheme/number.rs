use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// Internal numeric representation for arithmetic operations.
#[derive(Debug, Clone, Copy)]
pub enum Num {
    Int(i64),
    Rat(i64, i64), // always simplified, denom > 0
    Flt(f64),
}

pub fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Build a simplified rational, returning Int when denom == 1.
pub fn simplify(numer: i64, denom: i64) -> Num {
    debug_assert!(denom != 0, "simplify called with zero denominator");
    let sign = if denom < 0 { -1 } else { 1 };
    let n = numer * sign;
    let d = denom * sign;
    let g = gcd(n.abs(), d);
    let (n, d) = (n / g, d / g);
    if d == 1 { Num::Int(n) } else { Num::Rat(n, d) }
}

impl Num {
    pub fn from_value(val: &Value, span: Span) -> Result<Self, EvalError> {
        match val {
            Value::Integer(n) => Ok(Num::Int(*n)),
            Value::Rational(n, d) => Ok(Num::Rat(*n, *d)),
            Value::Float(f) => Ok(Num::Flt(*f)),
            other => Err(EvalError::TypeError {
                message: format!("expected number, got {other}"),
                span,
            }),
        }
    }

    pub fn to_value(self) -> Value {
        match self {
            Num::Int(n) => Value::Integer(n),
            Num::Rat(n, d) => Value::Rational(n, d),
            Num::Flt(f) => Value::Float(f),
        }
    }

    pub fn to_f64(self) -> f64 {
        match self {
            Num::Int(n) => n as f64,
            Num::Rat(n, d) => n as f64 / d as f64,
            Num::Flt(f) => f,
        }
    }

    pub fn add(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flt(a), b) => Num::Flt(a + b.to_f64()),
            (a, Num::Flt(b)) => Num::Flt(a.to_f64() + b),
            (Num::Int(a), Num::Int(b)) => Num::Int(a + b),
            (Num::Int(a), Num::Rat(n, d)) | (Num::Rat(n, d), Num::Int(a)) => {
                simplify(a * d + n, d)
            }
            (Num::Rat(n1, d1), Num::Rat(n2, d2)) => simplify(n1 * d2 + n2 * d1, d1 * d2),
        }
    }

    pub fn sub(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flt(a), b) => Num::Flt(a - b.to_f64()),
            (a, Num::Flt(b)) => Num::Flt(a.to_f64() - b),
            (Num::Int(a), Num::Int(b)) => Num::Int(a - b),
            (Num::Int(a), Num::Rat(n, d)) => simplify(a * d - n, d),
            (Num::Rat(n, d), Num::Int(a)) => simplify(n - a * d, d),
            (Num::Rat(n1, d1), Num::Rat(n2, d2)) => simplify(n1 * d2 - n2 * d1, d1 * d2),
        }
    }

    pub fn mul(self, other: Num) -> Num {
        match (self, other) {
            (Num::Flt(a), b) => Num::Flt(a * b.to_f64()),
            (a, Num::Flt(b)) => Num::Flt(a.to_f64() * b),
            (Num::Int(a), Num::Int(b)) => Num::Int(a * b),
            (Num::Int(a), Num::Rat(n, d)) | (Num::Rat(n, d), Num::Int(a)) => simplify(a * n, d),
            (Num::Rat(n1, d1), Num::Rat(n2, d2)) => simplify(n1 * n2, d1 * d2),
        }
    }

    pub fn div(self, other: Num, span: Span) -> Result<Num, EvalError> {
        match (self, other) {
            (_, Num::Int(0)) | (_, Num::Rat(0, _)) => {
                Err(EvalError::DivisionByZero { span })
            }
            (_, Num::Flt(0.0)) => Err(EvalError::DivisionByZero { span }),
            (Num::Flt(a), b) => Ok(Num::Flt(a / b.to_f64())),
            (a, Num::Flt(b)) => Ok(Num::Flt(a.to_f64() / b)),
            (Num::Int(a), Num::Int(b)) => Ok(simplify(a, b)),
            (Num::Int(a), Num::Rat(n, d)) => Ok(simplify(a * d, n)),
            (Num::Rat(n, d), Num::Int(a)) => Ok(simplify(n, d * a)),
            (Num::Rat(n1, d1), Num::Rat(n2, d2)) => Ok(simplify(n1 * d2, d1 * n2)),
        }
    }

    pub fn negate(self) -> Num {
        match self {
            Num::Int(n) => Num::Int(-n),
            Num::Rat(n, d) => Num::Rat(-n, d),
            Num::Flt(f) => Num::Flt(-f),
        }
    }
}

/// Convert a float to an exact rational.
pub fn float_to_exact(f: f64) -> Num {
    if f == f.floor() && f.abs() < i64::MAX as f64 {
        return Num::Int(f as i64);
    }
    let s = format!("{f}");
    let decimal_places = s.find('.').map_or(0, |i| s.len() - i - 1);
    let denom = 10i64.pow(decimal_places as u32);
    let numer = (f * denom as f64).round() as i64;
    simplify(numer, denom)
}

/// Format a float for Scheme display — always includes decimal point.
pub fn fmt_float(f: f64) -> String {
    let s = format!("{f}");
    if s.contains('.') { s } else { format!("{f}.0") }
}
