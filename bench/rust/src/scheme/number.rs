use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum Number {
    Exact(Rational),
    Inexact(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Rational {
    numer: i64,
    denom: i64,
}

impl Rational {
    pub(super) fn from_int(value: i64) -> Self {
        Self {
            numer: value,
            denom: 1,
        }
    }

    fn from_parts(numer: i128, denom: i128) -> Option<Self> {
        if denom == 0 {
            return None;
        }

        if numer == 0 {
            return Some(Self::from_int(0));
        }

        let mut numer = numer;
        let mut denom = denom;
        if denom < 0 {
            numer = -numer;
            denom = -denom;
        }

        let divisor = gcd_i128(numer.abs(), denom.abs());
        numer /= divisor;
        denom /= divisor;

        Some(Self {
            numer: i64::try_from(numer).ok()?,
            denom: i64::try_from(denom).ok()?,
        })
    }

    pub(super) fn numerator(self) -> i64 {
        self.numer
    }

    pub(super) fn denominator(self) -> i64 {
        self.denom
    }

    pub(super) fn is_integer(self) -> bool {
        self.denom == 1
    }

    pub(super) fn as_i64(self) -> Option<i64> {
        self.is_integer().then_some(self.numer)
    }

    fn to_f64(self) -> f64 {
        self.numer as f64 / self.denom as f64
    }

    fn add(self, other: Self) -> Option<Self> {
        Self::from_parts(
            self.numer as i128 * other.denom as i128 + other.numer as i128 * self.denom as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn sub(self, other: Self) -> Option<Self> {
        Self::from_parts(
            self.numer as i128 * other.denom as i128 - other.numer as i128 * self.denom as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn mul(self, other: Self) -> Option<Self> {
        Self::from_parts(
            self.numer as i128 * other.numer as i128,
            self.denom as i128 * other.denom as i128,
        )
    }

    fn div(self, other: Self) -> Option<Self> {
        Self::from_parts(
            self.numer as i128 * other.denom as i128,
            self.denom as i128 * other.numer as i128,
        )
    }

    fn neg(self) -> Option<Self> {
        Self::from_parts(-(self.numer as i128), self.denom as i128)
    }

    fn abs(self) -> Option<Self> {
        Self::from_parts((self.numer as i128).abs(), self.denom as i128)
    }

    fn cmp(self, other: Self) -> Ordering {
        let left = self.numer as i128 * other.denom as i128;
        let right = other.numer as i128 * self.denom as i128;
        left.cmp(&right)
    }

    fn render(self) -> String {
        if self.denom == 1 {
            self.numer.to_string()
        } else {
            format!("{}/{}", self.numer, self.denom)
        }
    }
}

impl Number {
    pub(super) fn exact_int(value: i64) -> Self {
        Self::Exact(Rational::from_int(value))
    }

    pub(super) fn as_exact_i64(self) -> Option<i64> {
        match self {
            Self::Exact(rational) => rational.as_i64(),
            Self::Inexact(_) => None,
        }
    }

    pub(super) fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_integer(self) -> bool {
        match self {
            Self::Exact(rational) => rational.is_integer(),
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(super) fn is_rational(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    pub(super) fn is_zero(self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator() == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    pub(super) fn is_positive(self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator() > 0,
            Self::Inexact(value) => value > 0.0,
        }
    }

    pub(super) fn is_negative(self) -> bool {
        match self {
            Self::Exact(rational) => rational.numerator() < 0,
            Self::Inexact(value) => value < 0.0,
        }
    }

    pub(super) fn as_rational(self) -> Option<Rational> {
        match self {
            Self::Exact(rational) => Some(rational),
            Self::Inexact(_) => None,
        }
    }

    pub(super) fn to_f64(self) -> f64 {
        match self {
            Self::Exact(rational) => rational.to_f64(),
            Self::Inexact(value) => value,
        }
    }

    pub(super) fn to_inexact(self) -> Self {
        Self::Inexact(self.to_f64())
    }

    pub(super) fn to_exact(self) -> Option<Self> {
        match self {
            Self::Exact(rational) => Some(Self::Exact(rational)),
            Self::Inexact(value) => {
                parse_decimal_as_rational(&render_inexact(value)).map(Self::Exact)
            }
        }
    }

    pub(super) fn add(self, other: Self) -> Self {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                exact_or_inexact(left.add(right), left.to_f64() + right.to_f64())
            }
            _ => Self::Inexact(self.to_f64() + other.to_f64()),
        }
    }

    pub(super) fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                exact_or_inexact(left.sub(right), left.to_f64() - right.to_f64())
            }
            _ => Self::Inexact(self.to_f64() - other.to_f64()),
        }
    }

    pub(super) fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                exact_or_inexact(left.mul(right), left.to_f64() * right.to_f64())
            }
            _ => Self::Inexact(self.to_f64() * other.to_f64()),
        }
    }

    pub(super) fn div(self, other: Self) -> Option<Self> {
        if other.is_zero() {
            return None;
        }

        Some(match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => {
                exact_or_inexact(left.div(right), left.to_f64() / right.to_f64())
            }
            _ => Self::Inexact(self.to_f64() / other.to_f64()),
        })
    }

    pub(super) fn neg(self) -> Self {
        match self {
            Self::Exact(rational) => exact_or_inexact(rational.neg(), -(rational.to_f64())),
            Self::Inexact(value) => Self::Inexact(-value),
        }
    }

    pub(super) fn abs(self) -> Self {
        match self {
            Self::Exact(rational) => exact_or_inexact(rational.abs(), rational.to_f64().abs()),
            Self::Inexact(value) => Self::Inexact(value.abs()),
        }
    }

    pub(super) fn numeric_eq(self, other: Self) -> bool {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => left == right,
            _ => self.to_f64() == other.to_f64(),
        }
    }

    pub(super) fn compare(self, other: Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Exact(left), Self::Exact(right)) => Some(left.cmp(right)),
            _ => self.to_f64().partial_cmp(&other.to_f64()),
        }
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::Exact(rational) => rational.render(),
            Self::Inexact(value) => render_inexact(value),
        }
    }
}

pub(super) fn parse_number_token(token: &str) -> Option<Number> {
    if is_integer_token(token) {
        return token.parse::<i64>().ok().map(Number::exact_int);
    }

    if let Some(rational) = parse_rational_token(token) {
        return Some(Number::Exact(rational));
    }

    if looks_like_inexact_token(token) {
        return token.parse::<f64>().ok().map(Number::Inexact);
    }

    None
}

fn exact_or_inexact(value: Option<Rational>, fallback: f64) -> Number {
    value
        .map(Number::Exact)
        .unwrap_or(Number::Inexact(fallback))
}

fn render_inexact(value: f64) -> String {
    let rendered = format!("{value:?}");
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}

fn parse_rational_token(token: &str) -> Option<Rational> {
    let (numer, denom) = token.split_once('/')?;
    if denom.contains('/') || !is_integer_token(numer) || !is_integer_token(denom) {
        return None;
    }

    Rational::from_parts(
        numer.parse::<i64>().ok()? as i128,
        denom.parse::<i64>().ok()? as i128,
    )
}

fn looks_like_inexact_token(token: &str) -> bool {
    token.chars().any(|ch| ch.is_ascii_digit())
        && token.chars().any(|ch| matches!(ch, '.' | 'e' | 'E'))
}

fn is_integer_token(token: &str) -> bool {
    let digits = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);

    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
}

fn parse_decimal_as_rational(text: &str) -> Option<Rational> {
    let mut text = text.trim();
    if text.is_empty()
        || matches!(
            text.to_ascii_lowercase().as_str(),
            "nan"
                | "+nan"
                | "-nan"
                | "inf"
                | "+inf"
                | "-inf"
                | "infinity"
                | "+infinity"
                | "-infinity"
        )
    {
        return None;
    }

    let sign = if let Some(rest) = text.strip_prefix('+') {
        text = rest;
        1_i128
    } else if let Some(rest) = text.strip_prefix('-') {
        text = rest;
        -1_i128
    } else {
        1_i128
    };

    let (mantissa, exponent) = if let Some(index) = text.find(['e', 'E']) {
        let (mantissa, exponent) = text.split_at(index);
        let exponent = exponent[1..].parse::<i32>().ok()?;
        (mantissa, exponent)
    } else {
        (text, 0_i32)
    };

    let (whole, fraction) = if let Some((whole, fraction)) = mantissa.split_once('.') {
        (whole, fraction)
    } else {
        (mantissa, "")
    };

    if (whole.is_empty() && fraction.is_empty())
        || !whole.chars().all(|ch| ch.is_ascii_digit())
        || !fraction.chars().all(|ch| ch.is_ascii_digit())
    {
        return None;
    }

    let digits = format!("{whole}{fraction}");
    let magnitude = digits.trim_start_matches('0');
    let mut numer = if magnitude.is_empty() {
        0_i128
    } else {
        magnitude.parse::<i128>().ok()?
    };
    numer *= sign;

    let mut denom = pow10_i128(fraction.len() as u32)?;
    if exponent >= 0 {
        numer = numer.checked_mul(pow10_i128(exponent as u32)?)?;
    } else {
        denom = denom.checked_mul(pow10_i128((-exponent) as u32)?)?;
    }

    Rational::from_parts(numer, denom)
}

fn pow10_i128(power: u32) -> Option<i128> {
    let mut value = 1_i128;
    for _ in 0..power {
        value = value.checked_mul(10)?;
    }
    Some(value)
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left
}
