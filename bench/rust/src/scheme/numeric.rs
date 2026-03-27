use super::{EvalError, Value};

pub(crate) fn num_gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub(crate) fn make_rational(num: i64, den: i64) -> Value {
    assert!(den != 0, "division by zero in make_rational");
    let sign = if den < 0 { -1 } else { 1 };
    let num = num * sign;
    let den = den.abs();
    let g = num_gcd(num.abs(), den);
    let num = num / g;
    let den = den / g;
    if den == 1 {
        Value::Integer(num)
    } else {
        Value::Rational(num, den)
    }
}

pub(crate) fn value_to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        Value::Float(f) => Ok(*f),
        _ => Err(EvalError::Type(format!("expected number, got {v}"))),
    }
}

pub(crate) fn f64_to_exact(f: f64) -> Value {
    if f == f.floor() && f.abs() < i64::MAX as f64 {
        return Value::Integer(f as i64);
    }
    // Decompose IEEE 754 double to exact rational
    let bits = f.to_bits();
    let sign: i64 = if bits >> 63 == 1 { -1 } else { 1 };
    let raw_exp = ((bits >> 52) & 0x7FF) as i64;
    let mantissa = if raw_exp == 0 {
        (bits & 0x000F_FFFF_FFFF_FFFF) as i64
    } else {
        (bits & 0x000F_FFFF_FFFF_FFFF | 0x0010_0000_0000_0000) as i64
    };
    let exp = raw_exp - 1023 - 52;
    if exp >= 0 {
        Value::Integer(sign * mantissa * (1i64 << exp as u32))
    } else {
        let den = 1i64 << ((-exp) as u32);
        make_rational(sign * mantissa, den)
    }
}

pub(crate) fn is_number(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
}

pub(crate) fn values_equal(a: &Value, b: &Value) -> bool {
    if is_number(a) && is_number(b) {
        return nums_equal(a, b);
    }
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a, _), Value::Str(b, _)) => *a.borrow() == *b.borrow(),
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

pub(crate) fn nums_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Float(a), Value::Float(b)) => a == b,
        _ => {
            // Cross-tower: convert to f64
            if let (Ok(a), Ok(b)) = (value_to_f64(a), value_to_f64(b)) {
                a == b
            } else {
                false
            }
        }
    }
}

pub(crate) fn nums_less(a: &Value, b: &Value) -> Result<bool, EvalError> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(a < b),
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => Ok(n1 * d2 < n2 * d1),
        (Value::Integer(a), Value::Rational(n, d)) => Ok(*a * d < *n),
        (Value::Rational(n, d), Value::Integer(b)) => Ok(*n < *b * d),
        _ => {
            let a = value_to_f64(a)?;
            let b = value_to_f64(b)?;
            Ok(a < b)
        }
    }
}
