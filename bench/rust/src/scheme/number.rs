use super::EvalError;
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Number {
    Exact(ExactNumber),
    Inexact(InexactNumber),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ExactNumber {
    numerator: i64,
    denominator: i64,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct InexactNumber(pub(super) f64);

impl PartialEq for InexactNumber {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for InexactNumber {}

impl Number {
    pub(super) fn integer(value: i64) -> Self {
        Self::Exact(ExactNumber {
            numerator: value,
            denominator: 1,
        })
    }

    pub(super) fn inexact(value: f64) -> Self {
        Self::Inexact(InexactNumber(value))
    }

    pub(super) fn parse_token(token: &str) -> Result<Option<Self>, EvalError> {
        if let Some(value) = parse_integer_token(token) {
            return Ok(Some(Self::integer(value)));
        }

        if let Some(value) = parse_rational_token(token)? {
            return Ok(Some(value));
        }

        if let Some(value) = parse_inexact_token(token) {
            return Ok(Some(Self::inexact(value)));
        }

        Ok(None)
    }

    pub(super) fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_integer(self) -> bool {
        match self {
            Self::Exact(value) => value.denominator == 1,
            Self::Inexact(value) => value.0.is_finite() && value.0.fract() == 0.0,
        }
    }

    pub(super) fn is_rational(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::Exact(value) => value.render(),
            Self::Inexact(value) => render_inexact(value.0),
        }
    }

    pub(super) fn to_f64(self) -> f64 {
        match self {
            Self::Exact(value) => value.to_f64(),
            Self::Inexact(value) => value.0,
        }
    }

    pub(super) fn compare(self, other: Self) -> Ordering {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left.compare(right),
            (left, right) => left
                .to_f64()
                .partial_cmp(&right.to_f64())
                .unwrap_or(Ordering::Equal),
        }
    }

    pub(super) fn numeric_eq(self, other: Self) -> bool {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            (left, right) => left.to_f64() == right.to_f64(),
        }
    }

    pub(super) fn add(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => normalize_exact(
                i128::from(left.numerator) * i128::from(right.denominator)
                    + i128::from(right.numerator) * i128::from(left.denominator),
                i128::from(left.denominator) * i128::from(right.denominator),
                "+",
            ),
            (left, right) => Ok(Self::inexact(left.to_f64() + right.to_f64())),
        }
    }

    pub(super) fn sub(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => normalize_exact(
                i128::from(left.numerator) * i128::from(right.denominator)
                    - i128::from(right.numerator) * i128::from(left.denominator),
                i128::from(left.denominator) * i128::from(right.denominator),
                "-",
            ),
            (left, right) => Ok(Self::inexact(left.to_f64() - right.to_f64())),
        }
    }

    pub(super) fn mul(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => normalize_exact(
                i128::from(left.numerator) * i128::from(right.numerator),
                i128::from(left.denominator) * i128::from(right.denominator),
                "*",
            ),
            (left, right) => Ok(Self::inexact(left.to_f64() * right.to_f64())),
        }
    }

    pub(super) fn div(self, other: Self) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::DivisionByZero);
        }

        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => normalize_exact(
                i128::from(left.numerator) * i128::from(right.denominator),
                i128::from(left.denominator) * i128::from(right.numerator),
                "/",
            ),
            (left, right) => Ok(Self::inexact(left.to_f64() / right.to_f64())),
        }
    }

    pub(super) fn negate(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(value) => normalize_exact(
                -i128::from(value.numerator),
                i128::from(value.denominator),
                "-",
            ),
            Self::Inexact(value) => Ok(Self::inexact(-value.0)),
        }
    }

    pub(super) fn abs(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(value) => normalize_exact(
                i128::from(value.numerator).abs(),
                i128::from(value.denominator),
                "abs",
            ),
            Self::Inexact(value) => Ok(Self::inexact(value.0.abs())),
        }
    }

    pub(super) fn is_zero(self) -> bool {
        match self {
            Self::Exact(value) => value.numerator == 0,
            Self::Inexact(value) => value.0 == 0.0,
        }
    }

    pub(super) fn is_positive(self) -> bool {
        self.compare(Self::integer(0)) == Ordering::Greater
    }

    pub(super) fn is_negative(self) -> bool {
        self.compare(Self::integer(0)) == Ordering::Less
    }

    pub(super) fn exact_to_inexact(self) -> Self {
        match self {
            Self::Exact(value) => Self::inexact(value.to_f64()),
            Self::Inexact(value) => Self::Inexact(value),
        }
    }

    pub(super) fn inexact_to_exact(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(value) => Ok(Self::Exact(value)),
            Self::Inexact(value) => exact_from_decimal(&render_inexact(value.0), "inexact->exact"),
        }
    }

    pub(super) fn expect_exact_integer(self) -> Result<i64, EvalError> {
        match self {
            Self::Exact(value) if value.denominator == 1 => Ok(value.numerator),
            _ => Err(EvalError::TypeMismatch {
                expected: "integer",
                actual: "number",
            }),
        }
    }

    pub(super) fn numerator(self) -> Result<i64, EvalError> {
        match self {
            Self::Exact(value) => Ok(value.numerator),
            Self::Inexact(_) => Err(EvalError::TypeMismatch {
                expected: "exact number",
                actual: "number",
            }),
        }
    }

    pub(super) fn denominator(self) -> Result<i64, EvalError> {
        match self {
            Self::Exact(value) => Ok(value.denominator),
            Self::Inexact(_) => Err(EvalError::TypeMismatch {
                expected: "exact number",
                actual: "number",
            }),
        }
    }
}

impl ExactNumber {
    fn render(self) -> String {
        if self.denominator == 1 {
            self.numerator.to_string()
        } else {
            format!("{}/{}", self.numerator, self.denominator)
        }
    }

    fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }

    fn compare(self, other: Self) -> Ordering {
        (i128::from(self.numerator) * i128::from(other.denominator))
            .cmp(&(i128::from(other.numerator) * i128::from(self.denominator)))
    }
}

fn parse_integer_token(token: &str) -> Option<i64> {
    let digits = match token.bytes().next() {
        Some(b'+') | Some(b'-') => &token[1..],
        _ => token,
    };

    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    token.parse().ok()
}

fn parse_rational_token(token: &str) -> Result<Option<Number>, EvalError> {
    let Some((numerator, denominator)) = token.split_once('/') else {
        return Ok(None);
    };

    if denominator.contains('/') {
        return Ok(None);
    }

    let Some(numerator) = parse_integer_token(numerator) else {
        return Ok(None);
    };
    let Some(denominator) = parse_unsigned_integer_token(denominator) else {
        return Ok(None);
    };

    if denominator == 0 {
        return Err(EvalError::SyntaxError {
            message: "invalid rational literal".to_string(),
        });
    }

    Ok(Some(normalize_exact(
        i128::from(numerator),
        i128::from(denominator),
        "number literal",
    )?))
}

fn parse_unsigned_integer_token(token: &str) -> Option<i64> {
    if token.is_empty() || !token.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    token.parse().ok()
}

fn parse_inexact_token(token: &str) -> Option<f64> {
    if !token.contains('.') && !token.contains('e') && !token.contains('E') {
        return None;
    }

    token.parse().ok()
}

fn normalize_exact(
    numerator: i128,
    denominator: i128,
    operation: &'static str,
) -> Result<Number, EvalError> {
    if denominator == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let mut numerator = numerator;
    let mut denominator = denominator;

    if denominator < 0 {
        numerator = -numerator;
        denominator = -denominator;
    }

    let divisor = gcd_i128(numerator.abs(), denominator);
    numerator /= divisor;
    denominator /= divisor;

    Ok(Number::Exact(ExactNumber {
        numerator: cast_i128_to_i64(numerator, operation)?,
        denominator: cast_i128_to_i64(denominator, operation)?,
    }))
}

fn cast_i128_to_i64(value: i128, operation: &'static str) -> Result<i64, EvalError> {
    i64::try_from(value).map_err(|_| EvalError::NumericOverflow { operation })
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left.abs().max(1)
}

fn render_inexact(value: f64) -> String {
    format!("{value:?}")
}

fn exact_from_decimal(source: &str, operation: &'static str) -> Result<Number, EvalError> {
    let (mantissa, exponent) = match source.find(['e', 'E']) {
        Some(index) => {
            let exponent = source[index + 1..].parse::<i32>().map_err(|_| {
                EvalError::InvalidNumberConversion {
                    value: source.to_string(),
                }
            })?;
            (&source[..index], exponent)
        }
        None => (source, 0),
    };

    let (sign, mantissa) = if let Some(rest) = mantissa.strip_prefix('-') {
        (-1_i128, rest)
    } else if let Some(rest) = mantissa.strip_prefix('+') {
        (1_i128, rest)
    } else {
        (1_i128, mantissa)
    };

    let (whole, fractional) = match mantissa.split_once('.') {
        Some(parts) => parts,
        None => (mantissa, ""),
    };

    if whole.is_empty() && fractional.is_empty() {
        return Err(EvalError::InvalidNumberConversion {
            value: source.to_string(),
        });
    }

    if !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fractional.chars().all(|ch| ch.is_ascii_digit())
    {
        return Err(EvalError::InvalidNumberConversion {
            value: source.to_string(),
        });
    }

    let digits = format!("{whole}{fractional}");
    let mut numerator = if digits.is_empty() {
        0
    } else {
        digits
            .parse::<i128>()
            .map_err(|_| EvalError::InvalidNumberConversion {
                value: source.to_string(),
            })?
    };
    numerator *= sign;

    let mut denominator = power_of_ten(fractional.len(), operation)?;

    if exponent > 0 {
        numerator *= power_of_ten(exponent as usize, operation)?;
    } else if exponent < 0 {
        denominator *= power_of_ten((-exponent) as usize, operation)?;
    }

    normalize_exact(numerator, denominator, operation)
}

fn power_of_ten(power: usize, operation: &'static str) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..power {
        value = value
            .checked_mul(10)
            .ok_or(EvalError::NumericOverflow { operation })?;
    }
    Ok(value)
}
