use super::EvalError;
use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Number {
    Exact(Rational),
    Inexact(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Number {
    pub(crate) fn integer(value: i64) -> Self {
        Self::Exact(Rational::integer(value))
    }

    pub(crate) fn parse_literal(literal: &str) -> Result<Self, EvalError> {
        if literal.contains('/') {
            let Some((left, right)) = literal.split_once('/') else {
                return Err(EvalError::InvalidNumber {
                    literal: literal.to_string(),
                });
            };

            let numerator = left.parse::<i64>().map_err(|_| EvalError::InvalidNumber {
                literal: literal.to_string(),
            })?;
            let denominator = right.parse::<i64>().map_err(|_| EvalError::InvalidNumber {
                literal: literal.to_string(),
            })?;

            let rational =
                Rational::new(numerator, denominator).map_err(|_| EvalError::InvalidNumber {
                    literal: literal.to_string(),
                })?;
            return Ok(Self::Exact(rational));
        }

        if literal.contains('.') || literal.contains('e') || literal.contains('E') {
            return literal.parse::<f64>().map(Self::Inexact).map_err(|_| {
                EvalError::InvalidNumber {
                    literal: literal.to_string(),
                }
            });
        }

        literal
            .parse::<i64>()
            .map(Self::integer)
            .map_err(|_| EvalError::InvalidNumber {
                literal: literal.to_string(),
            })
    }

    pub(crate) fn render(&self) -> String {
        match self {
            Self::Exact(rational) => rational.render(),
            Self::Inexact(value) => render_inexact(*value),
        }
    }

    pub(crate) fn is_exact(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(crate) fn is_inexact(&self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(crate) fn is_integer(&self) -> bool {
        match self {
            Self::Exact(rational) => rational.denominator == 1,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(crate) fn is_rational(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(crate) fn exact_to_inexact(&self) -> Self {
        match self {
            Self::Exact(rational) => Self::Inexact(rational.to_f64()),
            Self::Inexact(value) => Self::Inexact(*value),
        }
    }

    pub(crate) fn inexact_to_exact(&self, name: &'static str) -> Result<Self, EvalError> {
        match self {
            Self::Exact(_) => Ok(self.clone()),
            Self::Inexact(value) => exact_from_decimal_string(&value.to_string(), name),
        }
    }

    pub(crate) fn numerator(&self) -> Option<i64> {
        match self {
            Self::Exact(rational) => Some(rational.numerator),
            Self::Inexact(_) => None,
        }
    }

    pub(crate) fn denominator(&self) -> Option<i64> {
        match self {
            Self::Exact(rational) => Some(rational.denominator),
            Self::Inexact(_) => None,
        }
    }

    pub(crate) fn as_exact_integer(&self) -> Option<i64> {
        match self {
            Self::Exact(rational) if rational.denominator == 1 => Some(rational.numerator),
            _ => None,
        }
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator == 0,
            Self::Inexact(value) => *value == 0.0,
        }
    }

    pub(crate) fn is_positive(&self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator > 0,
            Self::Inexact(value) => *value > 0.0,
        }
    }

    pub(crate) fn is_negative(&self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator < 0,
            Self::Inexact(value) => *value < 0.0,
        }
    }

    pub(crate) fn abs(&self, name: &'static str) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.abs(name)?)),
            Self::Inexact(value) => Ok(Self::Inexact(value.abs())),
        }
    }

    pub(crate) fn negate(&self, name: &'static str) -> Result<Self, EvalError> {
        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.negate(name)?)),
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
        }
    }

    pub(crate) fn add(&self, other: &Self, name: &'static str) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.add(*right, name)?)),
            _ => Ok(Self::Inexact(self.to_f64() + other.to_f64())),
        }
    }

    pub(crate) fn subtract(&self, other: &Self, name: &'static str) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                Ok(Self::Exact(left.subtract(*right, name)?))
            }
            _ => Ok(Self::Inexact(self.to_f64() - other.to_f64())),
        }
    }

    pub(crate) fn multiply(&self, other: &Self, name: &'static str) -> Result<Self, EvalError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                Ok(Self::Exact(left.multiply(*right, name)?))
            }
            _ => Ok(Self::Inexact(self.to_f64() * other.to_f64())),
        }
    }

    pub(crate) fn divide(&self, other: &Self, name: &'static str) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::DivisionByZero);
        }

        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.divide(*right, name)?)),
            _ => Ok(Self::Inexact(self.to_f64() / other.to_f64())),
        }
    }

    pub(crate) fn expt(&self, exponent: i64, name: &'static str) -> Result<Self, EvalError> {
        if exponent < 0 {
            return Err(EvalError::InvalidArgument {
                name,
                message: "expected a non-negative exponent",
            });
        }

        match self {
            Self::Exact(rational) => Ok(Self::Exact(rational.pow(exponent as u32, name)?)),
            Self::Inexact(value) => Ok(Self::Inexact(value.powi(exponent as i32))),
        }
    }

    pub(crate) fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Some(left.cmp(*right)),
            _ => self.to_f64().partial_cmp(&other.to_f64()),
        }
    }

    pub(crate) fn numeric_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            _ => self.to_f64() == other.to_f64(),
        }
    }

    pub(crate) fn to_f64(&self) -> f64 {
        match self {
            Self::Exact(rational) => rational.to_f64(),
            Self::Inexact(value) => *value,
        }
    }
}

impl Rational {
    fn integer(value: i64) -> Self {
        Self {
            numerator: value,
            denominator: 1,
        }
    }

    fn new(numerator: i64, denominator: i64) -> Result<Self, EvalError> {
        Self::from_i128(numerator as i128, denominator as i128, "number")
    }

    fn from_i128(
        numerator: i128,
        denominator: i128,
        name: &'static str,
    ) -> Result<Self, EvalError> {
        if denominator == 0 {
            return Err(EvalError::DivisionByZero);
        }

        if numerator == 0 {
            return Ok(Self::integer(0));
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let divisor = gcd_i128(numerator.unsigned_abs() as i128, denominator);
        numerator /= divisor;
        denominator /= divisor;

        let numerator = i64::try_from(numerator).map_err(|_| EvalError::InvalidArgument {
            name,
            message: "overflow",
        })?;
        let denominator = i64::try_from(denominator).map_err(|_| EvalError::InvalidArgument {
            name,
            message: "overflow",
        })?;

        Ok(Self {
            numerator,
            denominator,
        })
    }

    fn render(self) -> String {
        if self.denominator == 1 {
            self.numerator.to_string()
        } else {
            format!("{}/{}", self.numerator, self.denominator)
        }
    }

    fn abs(self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(self.numerator as i128, self.denominator as i128, name).map(|rational| {
            if rational.numerator < 0 {
                Self {
                    numerator: -rational.numerator,
                    denominator: rational.denominator,
                }
            } else {
                rational
            }
        })
    }

    fn negate(self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(-(self.numerator as i128), self.denominator as i128, name)
    }

    fn add(self, other: Self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128
                + other.numerator as i128 * self.denominator as i128,
            self.denominator as i128 * other.denominator as i128,
            name,
        )
    }

    fn subtract(self, other: Self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128
                - other.numerator as i128 * self.denominator as i128,
            self.denominator as i128 * other.denominator as i128,
            name,
        )
    }

    fn multiply(self, other: Self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(
            self.numerator as i128 * other.numerator as i128,
            self.denominator as i128 * other.denominator as i128,
            name,
        )
    }

    fn divide(self, other: Self, name: &'static str) -> Result<Self, EvalError> {
        Self::from_i128(
            self.numerator as i128 * other.denominator as i128,
            self.denominator as i128 * other.numerator as i128,
            name,
        )
    }

    fn pow(self, exponent: u32, name: &'static str) -> Result<Self, EvalError> {
        let numerator = checked_pow_i128(self.numerator as i128, exponent, name)?;
        let denominator = checked_pow_i128(self.denominator as i128, exponent, name)?;
        Self::from_i128(numerator, denominator, name)
    }

    fn cmp(self, other: Self) -> Ordering {
        (self.numerator as i128 * other.denominator as i128)
            .cmp(&(other.numerator as i128 * self.denominator as i128))
    }

    fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }
}

fn exact_from_decimal_string(token: &str, name: &'static str) -> Result<Number, EvalError> {
    let (sign, rest) = if let Some(rest) = token.strip_prefix('-') {
        (-1_i128, rest)
    } else if let Some(rest) = token.strip_prefix('+') {
        (1_i128, rest)
    } else {
        (1_i128, token)
    };

    let (mantissa, exponent) = if let Some(index) = rest.find(['e', 'E']) {
        let exponent =
            rest[index + 1..]
                .parse::<i32>()
                .map_err(|_| EvalError::InvalidArgument {
                    name,
                    message: "expected a finite inexact number",
                })?;
        (&rest[..index], exponent)
    } else {
        (rest, 0)
    };

    let (whole, fractional) = if let Some((whole, fractional)) = mantissa.split_once('.') {
        (whole, fractional)
    } else {
        (mantissa, "")
    };

    let digits = format!("{whole}{fractional}");
    if digits.is_empty() || !digits.chars().all(|ch| ch.is_ascii_digit()) {
        return Err(EvalError::InvalidArgument {
            name,
            message: "expected a finite inexact number",
        });
    }

    let mut numerator = digits
        .parse::<i128>()
        .map_err(|_| EvalError::InvalidArgument {
            name,
            message: "overflow",
        })?
        * sign;
    let scale = fractional.len() as i32 - exponent;

    let denominator = if scale > 0 {
        checked_pow10(scale as u32, name)?
    } else {
        numerator = numerator
            .checked_mul(checked_pow10((-scale) as u32, name)?)
            .ok_or(EvalError::InvalidArgument {
                name,
                message: "overflow",
            })?;
        1
    };

    Rational::from_i128(numerator, denominator, name).map(Number::Exact)
}

fn render_inexact(value: f64) -> String {
    let mut rendered = value.to_string();
    if !rendered.contains(['.', 'e', 'E']) {
        rendered.push_str(".0");
    }
    rendered
}

fn checked_pow10(exponent: u32, name: &'static str) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..exponent {
        value = value.checked_mul(10).ok_or(EvalError::InvalidArgument {
            name,
            message: "overflow",
        })?;
    }
    Ok(value)
}

fn checked_pow_i128(base: i128, exponent: u32, name: &'static str) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..exponent {
        value = value.checked_mul(base).ok_or(EvalError::InvalidArgument {
            name,
            message: "overflow",
        })?;
    }
    Ok(value)
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left.abs()
}
