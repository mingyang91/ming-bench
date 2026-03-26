use super::EvalError;
use std::cmp::Ordering;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Number {
    Exact(Rational),
    Inexact(f64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Rational {
    numer: i64,
    denom: i64,
}

impl Number {
    pub(super) fn integer(value: i64) -> Self {
        Self::Exact(Rational::integer(value))
    }

    pub(super) fn parse_literal(atom: &str) -> Result<Option<Self>, EvalError> {
        if let Some(number) = parse_rational(atom)? {
            return Ok(Some(number));
        }

        if let Some(number) = parse_decimal(atom)? {
            return Ok(Some(number));
        }

        if let Ok(value) = atom.parse::<i64>() {
            return Ok(Some(Self::integer(value)));
        }

        Ok(None)
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::Exact(rational) => rational.render(),
            Self::Inexact(value) => format!("{value:?}"),
        }
    }

    pub(super) fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_zero(self) -> bool {
        match self {
            Self::Exact(rational) => rational.numer == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    pub(super) fn is_integer(self) -> bool {
        match self {
            Self::Exact(rational) => rational.is_integer(),
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(super) fn is_rational(self) -> bool {
        match self {
            Self::Exact(_) => true,
            Self::Inexact(value) => value.is_finite(),
        }
    }

    pub(super) fn exact_to_inexact(self) -> Self {
        match self {
            Self::Exact(rational) => Self::Inexact(rational.to_f64()),
            Self::Inexact(value) => Self::Inexact(value),
        }
    }

    pub(super) fn inexact_to_exact(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational)),
            Self::Inexact(value) => {
                let rendered = format!("{value:?}");
                match parse_decimal_to_rational(&rendered)? {
                    Some(rational) => Ok(Self::Exact(rational)),
                    None => Ok(Self::Inexact(value)),
                }
            }
        }
    }

    pub(super) fn exact_integer(self) -> Option<i64> {
        match self {
            Self::Exact(rational) if rational.is_integer() => Some(rational.numer),
            Self::Inexact(value) if value.is_finite() && value.fract() == 0.0 => {
                let truncated = value.trunc();
                if truncated >= i64::MIN as f64 && truncated <= i64::MAX as f64 {
                    Some(truncated as i64)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn numerator(self) -> Result<i64, EvalError> {
        Ok(self.to_rational()?.numer)
    }

    pub(super) fn denominator(self) -> Result<i64, EvalError> {
        Ok(self.to_rational()?.denom)
    }

    pub(super) fn add(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.add(right)?)),
            (left, right) => Ok(Self::Inexact(left.to_f64() + right.to_f64())),
        }
    }

    pub(super) fn sub(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.sub(right)?)),
            (left, right) => Ok(Self::Inexact(left.to_f64() - right.to_f64())),
        }
    }

    pub(super) fn mul(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.mul(right)?)),
            (left, right) => Ok(Self::Inexact(left.to_f64() * right.to_f64())),
        }
    }

    pub(super) fn div(self, other: Self) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::DivisionByZero);
        }

        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.div(right)?)),
            (left, right) => Ok(Self::Inexact(left.to_f64() / right.to_f64())),
        }
    }

    pub(super) fn neg(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.neg()?)),
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
        }
    }

    pub(super) fn numeric_eq(self, other: Self) -> bool {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            (left, right) => left.to_f64() == right.to_f64(),
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

    fn to_f64(self) -> f64 {
        match self {
            Self::Exact(rational) => rational.to_f64(),
            Self::Inexact(value) => value,
        }
    }

    fn to_rational(self) -> Result<Rational, EvalError> {
        match self {
            Self::Exact(rational) => Ok(rational),
            Self::Inexact(value) => {
                let rendered = format!("{value:?}");
                parse_decimal_to_rational(&rendered)?.ok_or_else(|| EvalError::InvalidSyntax {
                    message: format!("cannot convert {rendered} to an exact rational"),
                })
            }
        }
    }
}

impl Rational {
    fn integer(value: i64) -> Self {
        Self {
            numer: value,
            denom: 1,
        }
    }

    fn new(numer: i64, denom: i64) -> Result<Self, EvalError> {
        if denom == 0 {
            return Err(EvalError::DivisionByZero);
        }

        normalize_rational(numer as i128, denom as i128)
    }

    fn render(self) -> String {
        if self.denom == 1 {
            self.numer.to_string()
        } else {
            format!("{}/{}", self.numer, self.denom)
        }
    }

    fn is_integer(self) -> bool {
        self.denom == 1
    }

    fn to_f64(self) -> f64 {
        self.numer as f64 / self.denom as f64
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        normalize_rational(
            self.numer as i128 * other.denom as i128 + other.numer as i128 * self.denom as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn sub(self, other: Self) -> Result<Self, EvalError> {
        normalize_rational(
            self.numer as i128 * other.denom as i128 - other.numer as i128 * self.denom as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn mul(self, other: Self) -> Result<Self, EvalError> {
        normalize_rational(
            self.numer as i128 * other.numer as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn div(self, other: Self) -> Result<Self, EvalError> {
        normalize_rational(
            self.numer as i128 * other.denom as i128,
            self.denom as i128 * other.numer as i128,
        )
    }

    fn neg(self) -> Result<Self, EvalError> {
        normalize_rational(-(self.numer as i128), self.denom as i128)
    }

    fn compare(self, other: Self) -> Ordering {
        let left = self.numer as i128 * other.denom as i128;
        let right = other.numer as i128 * self.denom as i128;
        left.cmp(&right)
    }
}

fn parse_rational(atom: &str) -> Result<Option<Number>, EvalError> {
    let Some((numerator, denominator)) = atom.split_once('/') else {
        return Ok(None);
    };

    if numerator.is_empty() || denominator.is_empty() {
        return Ok(None);
    }

    if !is_signed_digits(numerator) || !is_digits(denominator) {
        return Ok(None);
    }

    let numer = numerator
        .parse::<i64>()
        .map_err(|_| EvalError::InvalidSyntax {
            message: format!("invalid rational literal: {atom}"),
        })?;
    let denom = denominator
        .parse::<i64>()
        .map_err(|_| EvalError::InvalidSyntax {
            message: format!("invalid rational literal: {atom}"),
        })?;

    Ok(Some(Number::Exact(Rational::new(numer, denom)?)))
}

fn parse_decimal(atom: &str) -> Result<Option<Number>, EvalError> {
    let Some(rational) = parse_decimal_to_rational(atom)? else {
        return Ok(None);
    };
    Ok(Some(Number::Inexact(rational.to_f64())))
}

fn parse_decimal_to_rational(atom: &str) -> Result<Option<Rational>, EvalError> {
    let atom = match atom.strip_prefix('+') {
        Some(rest) => rest,
        None => atom,
    };

    let Some((whole, frac)) = atom.split_once('.') else {
        return Ok(None);
    };

    if whole.is_empty() || frac.is_empty() {
        return Ok(None);
    }

    let negative = whole.starts_with('-');
    let whole_digits = if negative { &whole[1..] } else { whole };
    if whole_digits.is_empty() || !is_digits(whole_digits) || !is_digits(frac) {
        return Ok(None);
    }

    let whole_value = whole_digits
        .parse::<i128>()
        .map_err(|_| EvalError::InvalidSyntax {
            message: format!("invalid decimal literal: {atom}"),
        })?;
    let frac_value = frac.parse::<i128>().map_err(|_| EvalError::InvalidSyntax {
        message: format!("invalid decimal literal: {atom}"),
    })?;

    let scale = pow10(frac.len())?;
    let signed_whole = if negative { -whole_value } else { whole_value };
    let numer = if negative {
        signed_whole * scale - frac_value
    } else {
        signed_whole * scale + frac_value
    };

    normalize_rational(numer, scale).map(Some)
}

fn normalize_rational(numer: i128, denom: i128) -> Result<Rational, EvalError> {
    if denom == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let mut numer = numer;
    let mut denom = denom;
    if denom < 0 {
        numer = -numer;
        denom = -denom;
    }

    if numer == 0 {
        denom = 1;
    } else {
        let gcd = gcd_i128(numer.abs(), denom);
        numer /= gcd;
        denom /= gcd;
    }

    let numer = i64::try_from(numer).map_err(|_| EvalError::InvalidSyntax {
        message: "numeric overflow".to_string(),
    })?;
    let denom = i64::try_from(denom).map_err(|_| EvalError::InvalidSyntax {
        message: "numeric overflow".to_string(),
    })?;

    Ok(Rational { numer, denom })
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left.abs()
}

fn pow10(len: usize) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..len {
        value = value
            .checked_mul(10)
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "numeric overflow".to_string(),
            })?;
    }
    Ok(value)
}

fn is_digits(value: &str) -> bool {
    !value.is_empty() && value.chars().all(|ch| ch.is_ascii_digit())
}

fn is_signed_digits(value: &str) -> bool {
    if let Some(rest) = value.strip_prefix('-').or_else(|| value.strip_prefix('+')) {
        is_digits(rest)
    } else {
        is_digits(value)
    }
}
