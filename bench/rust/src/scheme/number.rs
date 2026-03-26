use std::cmp::Ordering;

use super::EvalError;

#[derive(Clone, Debug)]
pub(super) enum ParseNumberError {
    Integer(String),
    Rational(String),
    Inexact(String),
}

impl ParseNumberError {
    pub(super) fn message(&self) -> String {
        match self {
            Self::Integer(token) => format!("invalid integer literal: {token}"),
            Self::Rational(token) => format!("invalid rational literal: {token}"),
            Self::Inexact(token) => format!("invalid inexact literal: {token}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Number {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Number {
    pub(super) const fn zero() -> Self {
        Self::Integer(0)
    }

    pub(super) const fn one() -> Self {
        Self::Integer(1)
    }

    pub(super) fn from_rational(numerator: i64, denominator: i64) -> Result<Self, EvalError> {
        Ok(Self::from_normalized_rational(Rational::new(
            numerator,
            denominator,
        )?))
    }

    pub(super) fn from_inexact(value: f64) -> Result<Self, EvalError> {
        if value.is_finite() {
            Ok(Self::Inexact(value))
        } else {
            Err(EvalError::IntegerOverflow)
        }
    }

    pub(super) fn is_exact(self) -> bool {
        !matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_integer(self) -> bool {
        match self {
            Self::Integer(_) => true,
            Self::Rational(rational) => rational.denominator == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(super) fn is_rational(self) -> bool {
        match self {
            Self::Integer(_) | Self::Rational(_) => true,
            Self::Inexact(value) => value.is_finite(),
        }
    }

    pub(super) fn as_exact_integer(self) -> Option<i64> {
        match self {
            Self::Integer(value) => Some(value),
            Self::Rational(rational) if rational.denominator == 1 => Some(rational.numerator),
            Self::Rational(_) | Self::Inexact(_) => None,
        }
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Rational(rational) => {
                format!("{}/{}", rational.numerator, rational.denominator)
            }
            Self::Inexact(value) => render_inexact(value),
        }
    }

    pub(super) fn to_f64(self) -> f64 {
        match self {
            Self::Integer(value) => value as f64,
            Self::Rational(rational) => rational.numerator as f64 / rational.denominator as f64,
            Self::Inexact(value) => value,
        }
    }

    pub(super) fn compare(self, other: Self) -> Result<Ordering, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return self
                .to_f64()
                .partial_cmp(&other.to_f64())
                .ok_or(EvalError::IntegerOverflow);
        }

        let lhs = self.to_exact_rational();
        let rhs = other.to_exact_rational();
        Ok(lhs.compare(rhs))
    }

    pub(super) fn equals(self, other: Self) -> Result<bool, EvalError> {
        Ok(self.compare(other)? == Ordering::Equal)
    }

    pub(super) fn add(self, other: Self) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Self::from_inexact(self.to_f64() + other.to_f64());
        }

        let result = self.to_exact_rational().add(other.to_exact_rational())?;
        Ok(Self::from_normalized_rational(result))
    }

    pub(super) fn sub(self, other: Self) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Self::from_inexact(self.to_f64() - other.to_f64());
        }

        let result = self.to_exact_rational().sub(other.to_exact_rational())?;
        Ok(Self::from_normalized_rational(result))
    }

    pub(super) fn mul(self, other: Self) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Self::from_inexact(self.to_f64() * other.to_f64());
        }

        let result = self.to_exact_rational().mul(other.to_exact_rational())?;
        Ok(Self::from_normalized_rational(result))
    }

    pub(super) fn div(self, other: Self) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            let divisor = other.to_f64();
            if divisor == 0.0 {
                return Err(EvalError::DivisionByZero);
            }
            return Self::from_inexact(self.to_f64() / divisor);
        }

        let result = self.to_exact_rational().div(other.to_exact_rational())?;
        Ok(Self::from_normalized_rational(result))
    }

    pub(super) fn negate(self) -> Result<Self, EvalError> {
        match self {
            Self::Integer(value) => Ok(Self::Integer(
                value.checked_neg().ok_or(EvalError::IntegerOverflow)?,
            )),
            Self::Rational(rational) => {
                let numerator = rational
                    .numerator
                    .checked_neg()
                    .ok_or(EvalError::IntegerOverflow)?;
                Ok(Self::from_normalized_rational(Rational {
                    numerator,
                    denominator: rational.denominator,
                }))
            }
            Self::Inexact(value) => Self::from_inexact(-value),
        }
    }

    pub(super) fn abs(self) -> Result<Self, EvalError> {
        match self {
            Self::Integer(value) => Ok(Self::Integer(
                value.checked_abs().ok_or(EvalError::IntegerOverflow)?,
            )),
            Self::Rational(rational) => {
                let numerator = rational
                    .numerator
                    .checked_abs()
                    .ok_or(EvalError::IntegerOverflow)?;
                Ok(Self::from_normalized_rational(Rational {
                    numerator,
                    denominator: rational.denominator,
                }))
            }
            Self::Inexact(value) => Self::from_inexact(value.abs()),
        }
    }

    pub(super) fn numerator(self) -> Result<i64, EvalError> {
        Ok(self.to_exact_rational().numerator)
    }

    pub(super) fn denominator(self) -> Result<i64, EvalError> {
        Ok(self.to_exact_rational().denominator)
    }

    pub(super) fn to_exact(self) -> Result<Self, EvalError> {
        match self {
            Self::Integer(_) | Self::Rational(_) => Ok(self),
            Self::Inexact(value) => parse_inexact_as_exact(value),
        }
    }

    fn from_normalized_rational(rational: Rational) -> Self {
        if rational.denominator == 1 {
            Self::Integer(rational.numerator)
        } else {
            Self::Rational(rational)
        }
    }

    fn to_exact_rational(self) -> Rational {
        match self {
            Self::Integer(value) => Rational {
                numerator: value,
                denominator: 1,
            },
            Self::Rational(rational) => rational,
            Self::Inexact(_) => unreachable!("inexact numbers do not have an exact rational form"),
        }
    }
}

impl Rational {
    fn new(numerator: i64, denominator: i64) -> Result<Self, EvalError> {
        if denominator == 0 {
            return Err(EvalError::DivisionByZero);
        }

        let mut numerator = numerator as i128;
        let mut denominator = denominator as i128;
        if denominator < 0 {
            numerator = numerator.checked_neg().ok_or(EvalError::IntegerOverflow)?;
            denominator = denominator
                .checked_neg()
                .ok_or(EvalError::IntegerOverflow)?;
        }

        let gcd = gcd_i128(numerator, denominator);
        numerator /= gcd;
        denominator /= gcd;

        if numerator == 0 {
            denominator = 1;
        }

        let numerator = i64::try_from(numerator).map_err(|_| EvalError::IntegerOverflow)?;
        let denominator = i64::try_from(denominator).map_err(|_| EvalError::IntegerOverflow)?;

        Ok(Self {
            numerator,
            denominator,
        })
    }

    fn compare(self, other: Self) -> Ordering {
        let lhs = (self.numerator as i128) * (other.denominator as i128);
        let rhs = (other.numerator as i128) * (self.denominator as i128);
        lhs.cmp(&rhs)
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        let numerator = (self.numerator as i128)
            .checked_mul(other.denominator as i128)
            .and_then(|lhs| {
                (other.numerator as i128)
                    .checked_mul(self.denominator as i128)
                    .and_then(|rhs| lhs.checked_add(rhs))
            })
            .ok_or(EvalError::IntegerOverflow)?;
        let denominator = (self.denominator as i128)
            .checked_mul(other.denominator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        normalize_i128_rational(numerator, denominator)
    }

    fn sub(self, other: Self) -> Result<Self, EvalError> {
        let numerator = (self.numerator as i128)
            .checked_mul(other.denominator as i128)
            .and_then(|lhs| {
                (other.numerator as i128)
                    .checked_mul(self.denominator as i128)
                    .and_then(|rhs| lhs.checked_sub(rhs))
            })
            .ok_or(EvalError::IntegerOverflow)?;
        let denominator = (self.denominator as i128)
            .checked_mul(other.denominator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        normalize_i128_rational(numerator, denominator)
    }

    fn mul(self, other: Self) -> Result<Self, EvalError> {
        let numerator = (self.numerator as i128)
            .checked_mul(other.numerator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        let denominator = (self.denominator as i128)
            .checked_mul(other.denominator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        normalize_i128_rational(numerator, denominator)
    }

    fn div(self, other: Self) -> Result<Self, EvalError> {
        if other.numerator == 0 {
            return Err(EvalError::DivisionByZero);
        }

        let numerator = (self.numerator as i128)
            .checked_mul(other.denominator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        let denominator = (self.denominator as i128)
            .checked_mul(other.numerator as i128)
            .ok_or(EvalError::IntegerOverflow)?;
        normalize_i128_rational(numerator, denominator)
    }
}

pub(super) fn parse_number_token(token: &str) -> Option<Result<Number, ParseNumberError>> {
    parse_integer_token(token)
        .map(|result| result.map(Number::Integer))
        .or_else(|| parse_rational_token(token))
        .or_else(|| parse_decimal_token(token))
}

fn parse_integer_token(token: &str) -> Option<Result<i64, ParseNumberError>> {
    if !looks_like_integer(token) {
        return None;
    }

    Some(
        token
            .parse::<i64>()
            .map_err(|_| ParseNumberError::Integer(token.into())),
    )
}

fn parse_rational_token(token: &str) -> Option<Result<Number, ParseNumberError>> {
    let (numerator, denominator) = token.split_once('/')?;
    if denominator.contains('/')
        || !looks_like_integer(numerator)
        || !looks_like_integer(denominator)
    {
        return None;
    }

    Some(parse_rational_parts(token, numerator, denominator))
}

fn parse_rational_parts(
    token: &str,
    numerator: &str,
    denominator: &str,
) -> Result<Number, ParseNumberError> {
    let numerator = numerator
        .parse::<i64>()
        .map_err(|_| ParseNumberError::Rational(token.into()))?;
    let denominator = denominator
        .parse::<i64>()
        .map_err(|_| ParseNumberError::Rational(token.into()))?;
    Number::from_rational(numerator, denominator)
        .map_err(|_| ParseNumberError::Rational(token.into()))
}

fn parse_decimal_token(token: &str) -> Option<Result<Number, ParseNumberError>> {
    if !looks_like_decimal(token) {
        return None;
    }

    Some(
        token
            .parse::<f64>()
            .map_err(|_| ParseNumberError::Inexact(token.into()))
            .and_then(|value| {
                Number::from_inexact(value)
                    .map_err(|_| ParseNumberError::Inexact(token.into()))
            }),
    )
}

pub(super) fn parse_number_string(token: &str) -> Option<Number> {
    match parse_number_token(token) {
        Some(Ok(number)) => Some(number),
        Some(Err(_)) | None => None,
    }
}

fn parse_inexact_as_exact(value: f64) -> Result<Number, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::IntegerOverflow);
    }

    let token = value.to_string();
    if let Some(result) = parse_integer_token(&token) {
        return result
            .map(Number::Integer)
            .map_err(|_| EvalError::IntegerOverflow);
    }

    if looks_like_decimal(&token) {
        return exact_number_from_decimal(&token);
    }

    Err(EvalError::IntegerOverflow)
}

fn exact_number_from_decimal(token: &str) -> Result<Number, EvalError> {
    let (negative, digits) = match token.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, token),
    };

    let Some((whole, fractional)) = digits.split_once('.') else {
        return Err(EvalError::IntegerOverflow);
    };

    let numerator_digits = format!("{whole}{fractional}");
    let mut numerator = numerator_digits
        .parse::<i128>()
        .map_err(|_| EvalError::IntegerOverflow)?;
    if negative {
        numerator = numerator.checked_neg().ok_or(EvalError::IntegerOverflow)?;
    }

    let denominator = 10_i128
        .checked_pow(fractional.len() as u32)
        .ok_or(EvalError::IntegerOverflow)?;

    Ok(Number::from_normalized_rational(normalize_i128_rational(
        numerator,
        denominator,
    )?))
}

fn normalize_i128_rational(numerator: i128, denominator: i128) -> Result<Rational, EvalError> {
    if denominator == 0 {
        return Err(EvalError::DivisionByZero);
    }

    let mut numerator = numerator;
    let mut denominator = denominator;
    if denominator < 0 {
        numerator = numerator.checked_neg().ok_or(EvalError::IntegerOverflow)?;
        denominator = denominator
            .checked_neg()
            .ok_or(EvalError::IntegerOverflow)?;
    }

    let gcd = gcd_i128(numerator, denominator);
    numerator /= gcd;
    denominator /= gcd;

    if numerator == 0 {
        denominator = 1;
    }

    Ok(Rational {
        numerator: i64::try_from(numerator).map_err(|_| EvalError::IntegerOverflow)?,
        denominator: i64::try_from(denominator).map_err(|_| EvalError::IntegerOverflow)?,
    })
}

fn gcd_i128(mut lhs: i128, mut rhs: i128) -> i128 {
    lhs = lhs.abs();
    rhs = rhs.abs();

    if lhs == 0 {
        return rhs.max(1);
    }
    if rhs == 0 {
        return lhs.max(1);
    }

    while rhs != 0 {
        let remainder = lhs % rhs;
        lhs = rhs;
        rhs = remainder;
    }

    lhs
}

fn looks_like_integer(token: &str) -> bool {
    let digits = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);
    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

fn looks_like_decimal(token: &str) -> bool {
    let digits = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);
    let Some((whole, fractional)) = digits.split_once('.') else {
        return false;
    };

    !whole.is_empty()
        && !fractional.is_empty()
        && !fractional.contains('.')
        && whole.chars().all(|ch| ch.is_ascii_digit())
        && fractional.chars().all(|ch| ch.is_ascii_digit())
}

fn render_inexact(value: f64) -> String {
    let rendered = value.to_string();
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}
