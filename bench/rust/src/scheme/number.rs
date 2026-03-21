use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Intermediate numeric representation for arithmetic operations.
#[derive(Debug, Clone, Copy)]
pub enum Num {
    /// Exact rational: (numerator, denominator), not necessarily reduced.
    Exact(i64, i64),
    /// Inexact floating-point.
    Inexact(f64),
}

/// Greatest common divisor (always non-negative).
pub fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Build a simplified exact Value from numerator/denominator.
/// Returns `Value::Integer` when the fraction reduces to a whole number.
pub fn make_rational(numer: i64, denom: i64) -> Value {
    debug_assert!(denom != 0, "make_rational called with zero denominator");
    let sign = if denom < 0 { -1 } else { 1 };
    let n = numer * sign;
    let d = denom * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

/// Extract a `Num` from a `Value`.
pub fn value_to_num(v: &Value) -> Result<Num, EvalError> {
    match v {
        Value::Integer(n) => Ok(Num::Exact(*n, 1)),
        Value::Rational(n, d) => Ok(Num::Exact(*n, *d)),
        Value::Float(f) => Ok(Num::Inexact(*f)),
        other => Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{other}"),
        }),
    }
}

/// Convert `Num` back to a `Value`, simplifying exact rationals.
pub fn num_to_value(n: Num) -> Value {
    match n {
        Num::Exact(n, d) => make_rational(n, d),
        Num::Inexact(f) => Value::Float(f),
    }
}

fn exact_to_f64(n: i64, d: i64) -> f64 {
    n as f64 / d as f64
}

/// Add two `Num` values.
pub fn num_add(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bd + bn * ad, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a + b),
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => {
            Num::Inexact(exact_to_f64(n, d) + f)
        }
    }
}

/// Subtract two `Num` values.
pub fn num_sub(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bd - bn * ad, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a - b),
        (Num::Exact(n, d), Num::Inexact(f)) => Num::Inexact(exact_to_f64(n, d) - f),
        (Num::Inexact(f), Num::Exact(n, d)) => Num::Inexact(f - exact_to_f64(n, d)),
    }
}

/// Multiply two `Num` values.
pub fn num_mul(a: Num, b: Num) -> Num {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => Num::Exact(an * bn, ad * bd),
        (Num::Inexact(a), Num::Inexact(b)) => Num::Inexact(a * b),
        (Num::Exact(n, d), Num::Inexact(f)) | (Num::Inexact(f), Num::Exact(n, d)) => {
            Num::Inexact(exact_to_f64(n, d) * f)
        }
    }
}

/// Divide two `Num` values.
pub fn num_div(a: Num, b: Num) -> Result<Num, EvalError> {
    match (a, b) {
        (Num::Exact(an, ad), Num::Exact(bn, bd)) => {
            if bn == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Num::Exact(an * bd, ad * bn))
        }
        (Num::Inexact(a), Num::Inexact(b)) => {
            if b == 0.0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Num::Inexact(a / b))
        }
        (Num::Exact(n, d), Num::Inexact(f)) => {
            if f == 0.0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Num::Inexact(exact_to_f64(n, d) / f))
        }
        (Num::Inexact(f), Num::Exact(n, d)) => {
            if n == 0 {
                return Err(EvalError::DivisionByZero);
            }
            Ok(Num::Inexact(f / exact_to_f64(n, d)))
        }
    }
}

/// Convert a `Num` to f64 for comparison purposes.
pub fn num_to_f64(n: &Num) -> f64 {
    match n {
        Num::Exact(n, d) => exact_to_f64(*n, *d),
        Num::Inexact(f) => *f,
    }
}

/// Negate a `Num`.
pub fn num_neg(a: Num) -> Num {
    match a {
        Num::Exact(n, d) => Num::Exact(-n, d),
        Num::Inexact(f) => Num::Inexact(-f),
    }
}

/// Convert an exact number to inexact (f64).
pub fn exact_to_inexact(v: &Value) -> Result<Value, EvalError> {
    match v {
        Value::Integer(n) => Ok(Value::Float(*n as f64)),
        Value::Rational(n, d) => Ok(Value::Float(*n as f64 / *d as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        other => Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{other}"),
        }),
    }
}

/// Convert an inexact number to exact (rational or integer).
pub fn inexact_to_exact(v: &Value) -> Result<Value, EvalError> {
    match v {
        Value::Integer(_) | Value::Rational(_, _) => Ok(v.clone()),
        Value::Float(f) => Ok(float_to_exact(*f)),
        other => Err(EvalError::TypeError {
            expected: "number".into(),
            got: format!("{other}"),
        }),
    }
}

/// Convert f64 to exact rational.
fn float_to_exact(f: f64) -> Value {
    if f.fract() == 0.0 && f.is_finite() {
        return Value::Integer(f as i64);
    }
    // Multiply by 2 until we get an integer numerator
    let mut numer = f;
    let mut denom = 1i64;
    while numer.fract() != 0.0 && denom < (1i64 << 53) {
        numer *= 2.0;
        denom *= 2;
    }
    make_rational(numer as i64, denom)
}

/// Check if a value is an exact number.
pub fn is_exact(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _))
}

/// Check if a value is an inexact number.
pub fn is_inexact(v: &Value) -> bool {
    matches!(v, Value::Float(_))
}

/// Check if a value is any number.
pub fn is_number(v: &Value) -> bool {
    matches!(
        v,
        Value::Integer(_) | Value::Rational(_, _) | Value::Float(_)
    )
}

/// Check if a value is an integer (exact integer or rational that simplifies to one).
pub fn is_integer(v: &Value) -> bool {
    matches!(v, Value::Integer(_))
}

/// Check if a value is rational (all exact numbers are rational).
pub fn is_rational(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _))
}
