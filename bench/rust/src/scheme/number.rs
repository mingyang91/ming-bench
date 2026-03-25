use std::cmp::Ordering;

use super::error::EvalError;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Rational {
    numer: i128,
    denom: i128,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Number {
    Exact(Rational),
    Inexact(f64),
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.numeric_eq(*other)
    }
}

impl Rational {
    fn new(numer: i128, denom: i128) -> Result<Self, EvalError> {
        if denom == 0 {
            return Err(EvalError::DivisionByZero);
        }

        let mut numer = numer;
        let mut denom = denom;
        if denom < 0 {
            numer = numer.checked_neg().ok_or(EvalError::NumericOverflow)?;
            denom = denom.checked_neg().ok_or(EvalError::NumericOverflow)?;
        }

        let divisor = gcd(numer, denom);
        Ok(Self {
            numer: numer / divisor,
            denom: denom / divisor,
        })
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        let lhs = checked_mul(self.numer, other.denom)?;
        let rhs = checked_mul(other.numer, self.denom)?;
        let numer = checked_add(lhs, rhs)?;
        let denom = checked_mul(self.denom, other.denom)?;
        Self::new(numer, denom)
    }

    fn sub(self, other: Self) -> Result<Self, EvalError> {
        let lhs = checked_mul(self.numer, other.denom)?;
        let rhs = checked_mul(other.numer, self.denom)?;
        let numer = checked_sub(lhs, rhs)?;
        let denom = checked_mul(self.denom, other.denom)?;
        Self::new(numer, denom)
    }

    fn mul(self, other: Self) -> Result<Self, EvalError> {
        let numer = checked_mul(self.numer, other.numer)?;
        let denom = checked_mul(self.denom, other.denom)?;
        Self::new(numer, denom)
    }

    fn div(self, other: Self) -> Result<Self, EvalError> {
        if other.numer == 0 {
            return Err(EvalError::DivisionByZero);
        }

        let numer = checked_mul(self.numer, other.denom)?;
        let denom = checked_mul(self.denom, other.numer)?;
        Self::new(numer, denom)
    }

    fn abs(self) -> Self {
        Self {
            numer: self.numer.abs(),
            denom: self.denom,
        }
    }

    fn neg(self) -> Result<Self, EvalError> {
        Ok(Self {
            numer: self.numer.checked_neg().ok_or(EvalError::NumericOverflow)?,
            denom: self.denom,
        })
    }

    fn compare(self, other: Self) -> Ordering {
        match (
            self.numer.checked_mul(other.denom),
            other.numer.checked_mul(self.denom),
        ) {
            (Some(lhs), Some(rhs)) => lhs.cmp(&rhs),
            _ => self
                .to_f64()
                .partial_cmp(&other.to_f64())
                .unwrap_or(Ordering::Equal),
        }
    }

    fn to_f64(self) -> f64 {
        self.numer as f64 / self.denom as f64
    }
}

impl Number {
    pub(crate) fn exact_integer(value: i64) -> Self {
        Self::Exact(Rational {
            numer: value as i128,
            denom: 1,
        })
    }

    pub(crate) fn exact_integer_i128(value: i128) -> Self {
        Self::Exact(Rational {
            numer: value,
            denom: 1,
        })
    }

    pub(crate) fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(crate) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(crate) fn is_integer(self) -> bool {
        match self {
            Self::Exact(value) => value.denom == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(crate) fn is_rational(self) -> bool {
        match self {
            Self::Exact(_) => true,
            Self::Inexact(value) => value.is_finite(),
        }
    }

    pub(crate) fn is_zero(self) -> bool {
        match self {
            Self::Exact(value) => value.numer == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    pub(crate) fn is_positive(self) -> bool {
        self.compare(Self::exact_integer(0)) == Ordering::Greater
    }

    pub(crate) fn is_negative(self) -> bool {
        self.compare(Self::exact_integer(0)) == Ordering::Less
    }

    pub(crate) fn exact_parts(self) -> Option<(i128, i128)> {
        match self {
            Self::Exact(value) => Some((value.numer, value.denom)),
            Self::Inexact(_) => None,
        }
    }

    pub(crate) fn abs(self) -> Self {
        match self {
            Self::Exact(value) => Self::Exact(value.abs()),
            Self::Inexact(value) => Self::Inexact(value.abs()),
        }
    }

    pub(crate) fn neg(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(value) => Ok(Self::Exact(value.neg()?)),
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
        }
    }

    pub(crate) fn add(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(lhs), Self::Exact(rhs)) => Ok(Self::Exact(lhs.add(rhs)?)),
            _ => Ok(Self::Inexact(self.to_f64() + other.to_f64())),
        }
    }

    pub(crate) fn sub(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(lhs), Self::Exact(rhs)) => Ok(Self::Exact(lhs.sub(rhs)?)),
            _ => Ok(Self::Inexact(self.to_f64() - other.to_f64())),
        }
    }

    pub(crate) fn mul(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(lhs), Self::Exact(rhs)) => Ok(Self::Exact(lhs.mul(rhs)?)),
            _ => Ok(Self::Inexact(self.to_f64() * other.to_f64())),
        }
    }

    pub(crate) fn div(self, other: Self) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::DivisionByZero);
        }

        match (self, other) {
            (Self::Exact(lhs), Self::Exact(rhs)) => Ok(Self::Exact(lhs.div(rhs)?)),
            _ => Ok(Self::Inexact(self.to_f64() / other.to_f64())),
        }
    }

    pub(crate) fn compare(self, other: Self) -> Ordering {
        match (self, other) {
            (Self::Exact(lhs), Self::Exact(rhs)) => lhs.compare(rhs),
            _ => self
                .to_f64()
                .partial_cmp(&other.to_f64())
                .unwrap_or(Ordering::Equal),
        }
    }

    pub(crate) fn numeric_eq(self, other: Self) -> bool {
        self.compare(other) == Ordering::Equal
    }

    pub(crate) fn to_inexact(self) -> Self {
        match self {
            Self::Exact(value) => Self::Inexact(value.to_f64()),
            Self::Inexact(value) => Self::Inexact(value),
        }
    }

    pub(crate) fn to_exact(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(_) => Ok(self),
            Self::Inexact(value) => Ok(Self::Exact(exact_from_inexact(value)?)),
        }
    }

    pub(crate) fn render(self) -> String {
        match self {
            Self::Exact(value) if value.denom == 1 => value.numer.to_string(),
            Self::Exact(value) => format!("{}/{}", value.numer, value.denom),
            Self::Inexact(value) => render_inexact(value),
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            Self::Exact(value) => value.to_f64(),
            Self::Inexact(value) => value,
        }
    }
}

fn exact_from_inexact(value: f64) -> Result<Rational, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::NonFiniteNumber);
    }

    parse_decimal_rational(&format!("{value:?}"))
}

pub(crate) fn parse_number_literal(token: &str) -> Result<Option<Number>, EvalError> {
    if let Ok(value) = token.parse::<i128>() {
        return Ok(Some(Number::exact_integer_i128(value)));
    }

    if !looks_numeric(token) {
        return Ok(None);
    }

    if token.contains('/') {
        return parse_rational_literal(token).map(Some);
    }

    if token.contains('.') || token.contains('e') || token.contains('E') {
        let value = token
            .parse::<f64>()
            .map_err(|_| invalid_number_literal(token))?;
        if !value.is_finite() {
            return Err(invalid_number_literal(token));
        }

        return Ok(Some(Number::Inexact(value)));
    }

    Ok(None)
}

fn parse_rational_literal(token: &str) -> Result<Number, EvalError> {
    if token.matches('/').count() != 1 {
        return Err(invalid_number_literal(token));
    }

    let Some((numer, denom)) = token.split_once('/') else {
        return Err(invalid_number_literal(token));
    };

    let numer = numer
        .parse::<i128>()
        .map_err(|_| invalid_number_literal(token))?;
    let denom = denom
        .parse::<i128>()
        .map_err(|_| invalid_number_literal(token))?;
    Rational::new(numer, denom)
        .map(Number::Exact)
        .map_err(|_| invalid_number_literal(token))
}

fn parse_decimal_rational(text: &str) -> Result<Rational, EvalError> {
    let Some(first) = text.chars().next() else {
        return Err(EvalError::InvalidNumberLiteral {
            literal: text.into(),
        });
    };

    let (sign, rest) = match first {
        '+' => (1_i128, &text[1..]),
        '-' => (-1_i128, &text[1..]),
        _ => (1_i128, text),
    };

    let (mantissa, exponent_text) = split_exponent(rest);
    let exponent = exponent_text
        .map(|value| value.parse::<i32>())
        .transpose()
        .map_err(|_| EvalError::InvalidNumberLiteral {
            literal: text.into(),
        })?
        .unwrap_or(0);

    let (whole, fractional) = mantissa.split_once('.').unwrap_or((mantissa, ""));
    if whole.is_empty() && fractional.is_empty() {
        return Err(EvalError::InvalidNumberLiteral {
            literal: text.into(),
        });
    }

    if !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fractional.chars().all(|ch| ch.is_ascii_digit())
    {
        return Err(EvalError::InvalidNumberLiteral {
            literal: text.into(),
        });
    }

    let digits = format!("{whole}{fractional}");
    let mut numer = digits
        .parse::<i128>()
        .map_err(|_| EvalError::InvalidNumberLiteral {
            literal: text.into(),
        })?;
    let mut denom = pow10(fractional.len())?;

    if exponent >= 0 {
        numer = checked_mul(numer, pow10(exponent as usize)?)?;
    } else {
        denom = checked_mul(denom, pow10((-exponent) as usize)?)?;
    }

    numer = checked_mul(sign, numer)?;
    Rational::new(numer, denom)
}

fn render_inexact(value: f64) -> String {
    let mut rendered = value.to_string();
    if !rendered.contains('.') && !rendered.contains('e') && !rendered.contains('E') {
        rendered.push_str(".0");
    }
    rendered
}

fn looks_numeric(token: &str) -> bool {
    starts_like_number(token)
        && token.chars().any(|ch| ch.is_ascii_digit())
        && token
            .chars()
            .all(|ch| ch.is_ascii_digit() || matches!(ch, '+' | '-' | '.' | '/' | 'e' | 'E'))
}

fn starts_like_number(token: &str) -> bool {
    let mut chars = token.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    let second = chars.next();
    match (first, second) {
        (ch, _) if ch.is_ascii_digit() => true,
        ('+' | '-', Some(ch)) if ch.is_ascii_digit() || ch == '.' => true,
        ('.', Some(ch)) if ch.is_ascii_digit() => true,
        _ => false,
    }
}

fn split_exponent(text: &str) -> (&str, Option<&str>) {
    for (index, ch) in text.char_indices() {
        if matches!(ch, 'e' | 'E') {
            return (&text[..index], Some(&text[index + 1..]));
        }
    }

    (text, None)
}

fn pow10(exp: usize) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..exp {
        value = checked_mul(value, 10)?;
    }
    Ok(value)
}

fn gcd(mut lhs: i128, mut rhs: i128) -> i128 {
    lhs = lhs.abs();
    rhs = rhs.abs();

    while rhs != 0 {
        let remainder = lhs % rhs;
        lhs = rhs;
        rhs = remainder;
    }

    if lhs == 0 {
        1
    } else {
        lhs
    }
}

fn checked_add(lhs: i128, rhs: i128) -> Result<i128, EvalError> {
    lhs.checked_add(rhs).ok_or(EvalError::NumericOverflow)
}

fn checked_sub(lhs: i128, rhs: i128) -> Result<i128, EvalError> {
    lhs.checked_sub(rhs).ok_or(EvalError::NumericOverflow)
}

fn checked_mul(lhs: i128, rhs: i128) -> Result<i128, EvalError> {
    lhs.checked_mul(rhs).ok_or(EvalError::NumericOverflow)
}

fn invalid_number_literal(token: &str) -> EvalError {
    EvalError::InvalidNumberLiteral {
        literal: token.into(),
    }
}
