use std::cmp::Ordering;

#[derive(Debug, Clone, Copy)]
pub(super) enum Number {
    Exact(Rational),
    Inexact(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Rational {
    numerator: i64,
    denominator: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NumberError {
    DivisionByZero,
    Overflow,
    NonFinite,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NumberParseError {
    Invalid,
    ZeroDenominator,
    Overflow,
}

impl Number {
    pub(super) fn exact_integer(value: i64) -> Self {
        Self::Exact(Rational::integer(value))
    }

    pub(super) fn is_exact(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_inexact(&self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_rational(&self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_integer(&self) -> bool {
        match self {
            Self::Exact(value) => value.is_integer(),
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(super) fn exact_integer_value(&self) -> Option<i64> {
        match self {
            Self::Exact(value) if value.is_integer() => Some(value.numerator()),
            _ => None,
        }
    }

    pub(super) fn numerator(&self) -> Option<i64> {
        match self {
            Self::Exact(value) => Some(value.numerator()),
            Self::Inexact(_) => None,
        }
    }

    pub(super) fn denominator(&self) -> Option<i64> {
        match self {
            Self::Exact(value) => Some(value.denominator()),
            Self::Inexact(_) => None,
        }
    }

    pub(super) fn to_inexact(self) -> Self {
        match self {
            Self::Exact(value) => Self::Inexact(value.to_f64()),
            Self::Inexact(_) => self,
        }
    }

    pub(super) fn to_exact(self) -> Result<Self, NumberError> {
        match self {
            Self::Exact(_) => Ok(self),
            Self::Inexact(value) => Ok(Self::Exact(inexact_to_exact(value)?)),
        }
    }

    pub(super) fn abs(self) -> Result<Self, NumberError> {
        match self {
            Self::Exact(value) => Ok(Self::Exact(value.abs()?)),
            Self::Inexact(value) => Ok(Self::Inexact(value.abs())),
        }
    }

    pub(super) fn add(self, other: Self) -> Result<Self, NumberError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.add(right)?)),
            _ => Ok(Self::Inexact(self.as_f64() + other.as_f64())),
        }
    }

    pub(super) fn sub(self, other: Self) -> Result<Self, NumberError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.sub(right)?)),
            _ => Ok(Self::Inexact(self.as_f64() - other.as_f64())),
        }
    }

    pub(super) fn mul(self, other: Self) -> Result<Self, NumberError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.mul(right)?)),
            _ => Ok(Self::Inexact(self.as_f64() * other.as_f64())),
        }
    }

    pub(super) fn div(self, other: Self) -> Result<Self, NumberError> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Ok(Self::Exact(left.div(right)?)),
            (_, other) if other.is_zero() => Err(NumberError::DivisionByZero),
            (Self::Inexact(_), _) | (Self::Exact(_), Self::Inexact(_)) => {
                Ok(Self::Inexact(self.as_f64() / other.as_f64()))
            }
        }
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::Exact(value) => value.render(),
            Self::Inexact(value) => render_inexact(value),
        }
    }

    fn as_f64(self) -> f64 {
        match self {
            Self::Exact(value) => value.to_f64(),
            Self::Inexact(value) => value,
        }
    }

    fn is_zero(self) -> bool {
        match self {
            Self::Exact(value) => value.numerator() == 0,
            Self::Inexact(value) => value == 0.0,
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

    fn numerator(self) -> i64 {
        self.numerator
    }

    fn denominator(self) -> i64 {
        self.denominator
    }

    fn is_integer(self) -> bool {
        self.denominator == 1
    }

    fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }

    fn abs(self) -> Result<Self, NumberError> {
        Self::from_parts_i128(
            i128::from(self.numerator).abs(),
            i128::from(self.denominator),
        )
    }

    fn add(self, other: Self) -> Result<Self, NumberError> {
        let numerator = i128::from(self.numerator) * i128::from(other.denominator)
            + i128::from(other.numerator) * i128::from(self.denominator);
        let denominator = i128::from(self.denominator) * i128::from(other.denominator);
        Self::from_parts_i128(numerator, denominator)
    }

    fn sub(self, other: Self) -> Result<Self, NumberError> {
        let numerator = i128::from(self.numerator) * i128::from(other.denominator)
            - i128::from(other.numerator) * i128::from(self.denominator);
        let denominator = i128::from(self.denominator) * i128::from(other.denominator);
        Self::from_parts_i128(numerator, denominator)
    }

    fn mul(self, other: Self) -> Result<Self, NumberError> {
        let numerator = i128::from(self.numerator) * i128::from(other.numerator);
        let denominator = i128::from(self.denominator) * i128::from(other.denominator);
        Self::from_parts_i128(numerator, denominator)
    }

    fn div(self, other: Self) -> Result<Self, NumberError> {
        if other.numerator == 0 {
            return Err(NumberError::DivisionByZero);
        }

        let numerator = i128::from(self.numerator) * i128::from(other.denominator);
        let denominator = i128::from(self.denominator) * i128::from(other.numerator);
        Self::from_parts_i128(numerator, denominator)
    }

    fn render(self) -> String {
        if self.denominator == 1 {
            self.numerator.to_string()
        } else {
            format!("{}/{}", self.numerator, self.denominator)
        }
    }

    fn from_parts_i64(numerator: i64, denominator: i64) -> Result<Self, NumberError> {
        Self::from_parts_i128(i128::from(numerator), i128::from(denominator))
    }

    fn from_parts_i128(mut numerator: i128, mut denominator: i128) -> Result<Self, NumberError> {
        if denominator == 0 {
            return Err(NumberError::DivisionByZero);
        }

        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        if numerator == 0 {
            return Ok(Self::integer(0));
        }

        let divisor = gcd_i128(numerator.abs(), denominator);
        numerator /= divisor;
        denominator /= divisor;

        let numerator = i64::try_from(numerator).map_err(|_| NumberError::Overflow)?;
        let denominator = i64::try_from(denominator).map_err(|_| NumberError::Overflow)?;

        Ok(Self {
            numerator,
            denominator,
        })
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (*self, *other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            _ => self.as_f64() == other.as_f64(),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (*self, *other) {
            (Self::Exact(left), Self::Exact(right)) => Some(left.cmp(&right)),
            _ => self.as_f64().partial_cmp(&other.as_f64()),
        }
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Self) -> Ordering {
        let left = i128::from(self.numerator) * i128::from(other.denominator);
        let right = i128::from(other.numerator) * i128::from(self.denominator);
        left.cmp(&right)
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub(super) fn parse_number_literal(token: &str) -> Result<Number, NumberParseError> {
    if token.contains('/') {
        let (numerator, denominator) = token.split_once('/').ok_or(NumberParseError::Invalid)?;
        if numerator.is_empty() || denominator.is_empty() {
            return Err(NumberParseError::Invalid);
        }

        let numerator = numerator
            .parse::<i64>()
            .map_err(|_| NumberParseError::Invalid)?;
        let denominator = denominator
            .parse::<i64>()
            .map_err(|_| NumberParseError::Invalid)?;
        let rational = Rational::from_parts_i64(numerator, denominator).map_err(map_parse_error)?;
        Ok(Number::Exact(rational))
    } else if token.contains('.') || token.contains('e') || token.contains('E') {
        let value = token
            .parse::<f64>()
            .map_err(|_| NumberParseError::Invalid)?;
        if !value.is_finite() {
            return Err(NumberParseError::Invalid);
        }
        Ok(Number::Inexact(value))
    } else {
        let value = token
            .parse::<i64>()
            .map_err(|_| NumberParseError::Invalid)?;
        Ok(Number::exact_integer(value))
    }
}

fn inexact_to_exact(value: f64) -> Result<Rational, NumberError> {
    if !value.is_finite() {
        return Err(NumberError::NonFinite);
    }

    let mut token = value.to_string();
    if !token.contains('.') && !token.contains('e') && !token.contains('E') {
        token.push_str(".0");
    }

    parse_decimal_to_rational(&token).map_err(map_number_parse_error)
}

fn parse_decimal_to_rational(token: &str) -> Result<Rational, NumberParseError> {
    let (mantissa, exponent) = match token.find(['e', 'E']) {
        Some(index) => {
            let exponent = token[index + 1..]
                .parse::<i32>()
                .map_err(|_| NumberParseError::Invalid)?;
            (&token[..index], exponent)
        }
        None => (token, 0),
    };

    let (sign, mantissa) = match mantissa.chars().next() {
        Some('+') => (1_i128, &mantissa[1..]),
        Some('-') => (-1_i128, &mantissa[1..]),
        Some(_) => (1_i128, mantissa),
        None => return Err(NumberParseError::Invalid),
    };

    let (whole, fractional) = match mantissa.split_once('.') {
        Some((whole, fractional)) => (whole, fractional),
        None => (mantissa, ""),
    };

    if whole.is_empty() && fractional.is_empty() {
        return Err(NumberParseError::Invalid);
    }

    if !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fractional.chars().all(|ch| ch.is_ascii_digit())
    {
        return Err(NumberParseError::Invalid);
    }

    let digits = if whole.is_empty() {
        format!("0{fractional}")
    } else {
        format!("{whole}{fractional}")
    };
    let numerator = digits
        .parse::<i128>()
        .map_err(|_| NumberParseError::Overflow)?
        * sign;
    let mut denominator = pow10_i128(fractional.len()).map_err(map_parse_error)?;
    let mut numerator = numerator;

    if exponent > 0 {
        numerator = numerator
            .checked_mul(pow10_i128(exponent as usize).map_err(map_parse_error)?)
            .ok_or(NumberParseError::Overflow)?;
    } else if exponent < 0 {
        denominator = denominator
            .checked_mul(pow10_i128((-exponent) as usize).map_err(map_parse_error)?)
            .ok_or(NumberParseError::Overflow)?;
    }

    Rational::from_parts_i128(numerator, denominator).map_err(map_parse_error)
}

fn render_inexact(value: f64) -> String {
    let mut rendered = value.to_string();
    if !rendered.contains('.') && !rendered.contains('e') && !rendered.contains('E') {
        rendered.push_str(".0");
    }
    rendered
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left.abs().max(1)
}

fn pow10_i128(exponent: usize) -> Result<i128, NumberError> {
    let mut value = 1_i128;
    for _ in 0..exponent {
        value = value.checked_mul(10).ok_or(NumberError::Overflow)?;
    }
    Ok(value)
}

fn map_parse_error(error: NumberError) -> NumberParseError {
    match error {
        NumberError::DivisionByZero => NumberParseError::ZeroDenominator,
        NumberError::Overflow | NumberError::NonFinite => NumberParseError::Overflow,
    }
}

fn map_number_parse_error(error: NumberParseError) -> NumberError {
    match error {
        NumberParseError::Invalid | NumberParseError::ZeroDenominator => NumberError::NonFinite,
        NumberParseError::Overflow => NumberError::Overflow,
    }
}
