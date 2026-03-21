use super::EvalError;
use std::{cmp::Ordering, fmt};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Number {
    Exact(Rational),
    Inexact(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Rational {
    numer: i64,
    denom: i64,
}

impl Number {
    pub(crate) const fn exact_integer(value: i64) -> Self {
        Self::Exact(Rational::integer(value))
    }

    pub(crate) fn exact_rational(numer: i64, denom: i64) -> Result<Self, EvalError> {
        Ok(Self::Exact(Rational::new(numer, denom)?))
    }

    pub(crate) const fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(crate) const fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(crate) fn is_integer(self) -> bool {
        match self {
            Self::Exact(rational) => rational.is_integer(),
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(crate) const fn is_rational(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(crate) fn as_exact_integer(self) -> Option<i64> {
        match self {
            Self::Exact(rational) if rational.is_integer() => Some(rational.numer),
            _ => None,
        }
    }

    pub(crate) fn numerator(self) -> Option<i64> {
        match self {
            Self::Exact(rational) => Some(rational.numer),
            Self::Inexact(_) => None,
        }
    }

    pub(crate) fn denominator(self) -> Option<i64> {
        match self {
            Self::Exact(rational) => Some(rational.denom),
            Self::Inexact(_) => None,
        }
    }

    pub(crate) fn add(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.add(right)?)),
            _ => Ok(Self::Inexact(self.to_f64() + other.to_f64())),
        }
    }

    pub(crate) fn sub(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.sub(right)?)),
            _ => Ok(Self::Inexact(self.to_f64() - other.to_f64())),
        }
    }

    pub(crate) fn mul(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.mul(right)?)),
            _ => Ok(Self::Inexact(self.to_f64() * other.to_f64())),
        }
    }

    pub(crate) fn div(self, other: Self) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(_), Self::Exact(right)) if right.numer == 0 => {
                Err(EvalError::DivisionByZero)
            }
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.div(right)?)),
            (_, Self::Exact(right)) if right.numer == 0 => Err(EvalError::DivisionByZero),
            (_, Self::Inexact(value)) if value == 0.0 => Err(EvalError::DivisionByZero),
            _ => Ok(Self::Inexact(self.to_f64() / other.to_f64())),
        }
    }

    pub(crate) fn neg(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.neg()?)),
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
        }
    }

    pub(crate) fn abs(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.abs()?)),
            Self::Inexact(value) => Ok(Self::Inexact(value.abs())),
        }
    }

    pub(crate) fn compare(self, other: Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Some(left.cmp(right)),
            _ => self.to_f64().partial_cmp(&other.to_f64()),
        }
    }

    pub(crate) fn numeric_eq(self, other: Self) -> bool {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            _ => self.compare(other) == Some(Ordering::Equal),
        }
    }

    pub(crate) fn min(self, other: Self) -> Self {
        match self.compare(other) {
            Some(Ordering::Greater) => other,
            _ => self,
        }
    }

    pub(crate) fn max(self, other: Self) -> Self {
        match self.compare(other) {
            Some(Ordering::Less) => other,
            _ => self,
        }
    }

    pub(crate) fn exact_to_inexact(self) -> Self {
        match self {
            Self::Exact(_) => Self::Inexact(self.to_f64()),
            Self::Inexact(_) => self,
        }
    }

    pub(crate) fn inexact_to_exact(self) -> Result<Self, EvalError> {
        match self {
            Self::Exact(_) => Ok(self),
            Self::Inexact(value) => {
                if !value.is_finite() {
                    return Err(EvalError::InvalidArgument(
                        "inexact->exact requires a finite number".into(),
                    ));
                }

                let rendered = render_inexact(value);
                let rational = Rational::from_decimal_str(rendered.as_str())?;
                Ok(Self::Exact(rational))
            }
        }
    }

    pub(crate) fn to_f64(self) -> f64 {
        match self {
            Self::Exact(rational) => rational.to_f64(),
            Self::Inexact(value) => value,
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Exact(rational) => rational.fmt(f),
            Self::Inexact(value) => f.write_str(&render_inexact(*value)),
        }
    }
}

impl Rational {
    const fn integer(value: i64) -> Self {
        Self {
            numer: value,
            denom: 1,
        }
    }

    fn new(numer: i64, denom: i64) -> Result<Self, EvalError> {
        Self::from_i128(i128::from(numer), i128::from(denom))
    }

    fn from_i128(numer: i128, denom: i128) -> Result<Self, EvalError> {
        if denom == 0 {
            return Err(EvalError::DivisionByZero);
        }

        if numer == 0 {
            return Ok(Self::integer(0));
        }

        let mut numer = numer;
        let mut denom = denom;

        if denom < 0 {
            numer = -numer;
            denom = -denom;
        }

        let divisor = gcd_i128(numer, denom);
        numer /= divisor;
        denom /= divisor;

        Ok(Self {
            numer: i64::try_from(numer).map_err(|_| EvalError::IntegerOverflow)?,
            denom: i64::try_from(denom).map_err(|_| EvalError::IntegerOverflow)?,
        })
    }

    const fn is_integer(self) -> bool {
        self.denom == 1
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        Self::from_i128(
            i128::from(self.numer) * i128::from(other.denom)
                + i128::from(other.numer) * i128::from(self.denom),
            i128::from(self.denom) * i128::from(other.denom),
        )
    }

    fn sub(self, other: Self) -> Result<Self, EvalError> {
        Self::from_i128(
            i128::from(self.numer) * i128::from(other.denom)
                - i128::from(other.numer) * i128::from(self.denom),
            i128::from(self.denom) * i128::from(other.denom),
        )
    }

    fn mul(self, other: Self) -> Result<Self, EvalError> {
        Self::from_i128(
            i128::from(self.numer) * i128::from(other.numer),
            i128::from(self.denom) * i128::from(other.denom),
        )
    }

    fn div(self, other: Self) -> Result<Self, EvalError> {
        Self::from_i128(
            i128::from(self.numer) * i128::from(other.denom),
            i128::from(self.denom) * i128::from(other.numer),
        )
    }

    fn neg(self) -> Result<Self, EvalError> {
        Self::from_i128(-i128::from(self.numer), i128::from(self.denom))
    }

    fn abs(self) -> Result<Self, EvalError> {
        Self::from_i128(i128::from(self.numer).abs(), i128::from(self.denom))
    }

    fn cmp(self, other: Self) -> Ordering {
        (i128::from(self.numer) * i128::from(other.denom))
            .cmp(&(i128::from(other.numer) * i128::from(self.denom)))
    }

    fn to_f64(self) -> f64 {
        self.numer as f64 / self.denom as f64
    }

    fn from_decimal_str(literal: &str) -> Result<Self, EvalError> {
        let (mantissa, exponent) = split_exponent(literal)?;
        let (negative, unsigned) = match mantissa.as_bytes().first().copied() {
            Some(b'+') => (false, &mantissa[1..]),
            Some(b'-') => (true, &mantissa[1..]),
            _ => (false, mantissa),
        };

        let (whole, fractional) = match unsigned.split_once('.') {
            Some((whole, fractional)) => (whole, fractional),
            None => (unsigned, ""),
        };

        if whole.is_empty() && fractional.is_empty() {
            return Err(EvalError::InvalidArgument("invalid inexact number".into()));
        }

        let digits = format!("{whole}{fractional}");
        if digits.chars().any(|ch| !ch.is_ascii_digit()) {
            return Err(EvalError::InvalidArgument("invalid inexact number".into()));
        }

        let mut numer = if digits.is_empty() {
            0_i128
        } else {
            digits
                .parse::<i128>()
                .map_err(|_| EvalError::IntegerOverflow)?
        };
        if negative {
            numer = -numer;
        }

        let mut denom = pow10_i128(fractional.len())?;
        if exponent >= 0 {
            numer = numer
                .checked_mul(pow10_i128(exponent as usize)?)
                .ok_or(EvalError::IntegerOverflow)?;
        } else {
            denom = denom
                .checked_mul(pow10_i128(exponent.unsigned_abs() as usize)?)
                .ok_or(EvalError::IntegerOverflow)?;
        }

        Self::from_i128(numer, denom)
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.denom == 1 {
            write!(f, "{}", self.numer)
        } else {
            write!(f, "{}/{}", self.numer, self.denom)
        }
    }
}

pub(crate) fn parse_number_literal(atom: &str) -> Option<Number> {
    if atom.contains('/') {
        let (numer, denom) = atom.split_once('/')?;
        if numer.is_empty() || denom.is_empty() {
            return None;
        }
        let numer = numer.parse::<i64>().ok()?;
        let denom = denom.parse::<i64>().ok()?;
        return Number::exact_rational(numer, denom).ok();
    }

    if atom.contains('.') || atom.contains('e') || atom.contains('E') {
        return atom.parse::<f64>().ok().map(Number::Inexact);
    }

    atom.parse::<i64>().ok().map(Number::exact_integer)
}

fn render_inexact(value: f64) -> String {
    let mut rendered = value.to_string();
    if value.is_finite()
        && !rendered.contains('.')
        && !rendered.contains('e')
        && !rendered.contains('E')
    {
        rendered.push_str(".0");
    }
    rendered
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    left = left.abs();
    right = right.abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    if left == 0 {
        1
    } else {
        left
    }
}

fn pow10_i128(exponent: usize) -> Result<i128, EvalError> {
    let mut result = 1_i128;
    for _ in 0..exponent {
        result = result.checked_mul(10).ok_or(EvalError::IntegerOverflow)?;
    }
    Ok(result)
}

fn split_exponent(literal: &str) -> Result<(&str, i32), EvalError> {
    match literal.find(['e', 'E']) {
        Some(index) => {
            let mantissa = &literal[..index];
            let exponent = literal[index + 1..]
                .parse::<i32>()
                .map_err(|_| EvalError::InvalidArgument("invalid inexact number".into()))?;
            Ok((mantissa, exponent))
        }
        None => Ok((literal, 0)),
    }
}
