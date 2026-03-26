use std::cmp::Ordering;

use super::error::EvalError;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum Number {
    ExactInt(i64),
    ExactRational { numer: i64, denom: i64 },
    Inexact(f64),
}

impl Number {
    pub(super) fn parse(token: &str) -> Option<Self> {
        if let Some(number) = parse_rational(token) {
            return Some(number);
        }

        if let Ok(value) = token.parse::<i64>() {
            return Some(Self::ExactInt(value));
        }

        if token.contains('.') || token.contains('e') || token.contains('E') {
            if let Ok(value) = token.parse::<f64>() {
                if value.is_finite() {
                    return Some(Self::Inexact(value));
                }
            }
        }

        None
    }

    pub(super) const fn exact_int(value: i64) -> Self {
        Self::ExactInt(value)
    }

    pub(super) fn is_exact(self) -> bool {
        !matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub(super) fn is_integer(self) -> bool {
        match self {
            Self::ExactInt(_) => true,
            Self::ExactRational { .. } => false,
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    pub(super) fn is_rational(self) -> bool {
        match self {
            Self::ExactInt(_) | Self::ExactRational { .. } => true,
            Self::Inexact(value) => value.is_finite(),
        }
    }

    pub(super) fn is_zero(self) -> bool {
        match self {
            Self::ExactInt(value) => value == 0,
            Self::ExactRational { numer, .. } => numer == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    pub(super) fn as_f64(self) -> f64 {
        match self {
            Self::ExactInt(value) => value as f64,
            Self::ExactRational { numer, denom } => numer as f64 / denom as f64,
            Self::Inexact(value) => value,
        }
    }

    pub(super) fn render(self) -> String {
        match self {
            Self::ExactInt(value) => value.to_string(),
            Self::ExactRational { numer, denom } => format!("{numer}/{denom}"),
            Self::Inexact(value) => format!("{value:?}"),
        }
    }

    pub(super) fn exact_parts(self) -> Option<(i64, i64)> {
        match self {
            Self::ExactInt(value) => Some((value, 1)),
            Self::ExactRational { numer, denom } => Some((numer, denom)),
            Self::Inexact(_) => None,
        }
    }

    pub(super) fn exact_integer(self) -> Option<i64> {
        match self {
            Self::ExactInt(value) => Some(value),
            Self::ExactRational { numer, denom: 1 } => Some(numer),
            Self::ExactRational { .. } | Self::Inexact(_) => None,
        }
    }

    pub(super) fn numerator(self) -> Option<i64> {
        self.exact_parts().map(|(numer, _)| numer)
    }

    pub(super) fn denominator(self) -> Option<i64> {
        self.exact_parts().map(|(_, denom)| denom)
    }

    pub(super) fn to_inexact(self) -> Self {
        Self::Inexact(self.as_f64())
    }

    pub(super) fn to_exact(self, name: &str) -> Result<Self, EvalError> {
        match self {
            Self::ExactInt(_) | Self::ExactRational { .. } => Ok(self),
            Self::Inexact(value) => exact_from_inexact(value, name),
        }
    }

    pub(super) fn negate(self, name: &str) -> Result<Self, EvalError> {
        match self {
            Self::Inexact(value) => Ok(Self::Inexact(-value)),
            exact => {
                let (numer, denom) = exact
                    .exact_parts()
                    .expect("exact number always has exact parts");
                Self::normalize_exact(name, -(numer as i128), denom as i128)
            }
        }
    }

    pub(super) fn abs(self, name: &str) -> Result<Self, EvalError> {
        match self {
            Self::Inexact(value) => Ok(Self::Inexact(value.abs())),
            exact => {
                let (numer, denom) = exact
                    .exact_parts()
                    .expect("exact number always has exact parts");
                Self::normalize_exact(name, (numer as i128).abs(), denom as i128)
            }
        }
    }

    pub(super) fn add(self, other: Self, name: &str) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Ok(Self::Inexact(self.as_f64() + other.as_f64()));
        }

        let (left_num, left_den) = self
            .exact_parts()
            .expect("exact number always has exact parts");
        let (right_num, right_den) = other
            .exact_parts()
            .expect("exact number always has exact parts");

        let numer =
            (left_num as i128) * (right_den as i128) + (right_num as i128) * (left_den as i128);
        let denom = (left_den as i128) * (right_den as i128);
        Self::normalize_exact(name, numer, denom)
    }

    pub(super) fn sub(self, other: Self, name: &str) -> Result<Self, EvalError> {
        self.add(other.negate(name)?, name)
    }

    pub(super) fn mul(self, other: Self, name: &str) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Ok(Self::Inexact(self.as_f64() * other.as_f64()));
        }

        let (left_num, left_den) = self
            .exact_parts()
            .expect("exact number always has exact parts");
        let (right_num, right_den) = other
            .exact_parts()
            .expect("exact number always has exact parts");

        let numer = (left_num as i128) * (right_num as i128);
        let denom = (left_den as i128) * (right_den as i128);
        Self::normalize_exact(name, numer, denom)
    }

    pub(super) fn div(self, other: Self, name: &str) -> Result<Self, EvalError> {
        if self.is_inexact() || other.is_inexact() {
            return Ok(Self::Inexact(self.as_f64() / other.as_f64()));
        }

        let (left_num, left_den) = self
            .exact_parts()
            .expect("exact number always has exact parts");
        let (right_num, right_den) = other
            .exact_parts()
            .expect("exact number always has exact parts");

        let numer = (left_num as i128) * (right_den as i128);
        let denom = (left_den as i128) * (right_num as i128);
        Self::normalize_exact(name, numer, denom)
    }

    pub(super) fn compare(self, other: Self) -> Option<Ordering> {
        if self.is_inexact() || other.is_inexact() {
            return self.as_f64().partial_cmp(&other.as_f64());
        }

        let (left_num, left_den) = self
            .exact_parts()
            .expect("exact number always has exact parts");
        let (right_num, right_den) = other
            .exact_parts()
            .expect("exact number always has exact parts");

        ((left_num as i128) * (right_den as i128))
            .partial_cmp(&((right_num as i128) * (left_den as i128)))
    }

    pub(super) fn normalize_exact(
        name: &str,
        numer: i128,
        denom: i128,
    ) -> Result<Self, EvalError> {
        normalize_exact_parts(numer, denom).ok_or_else(|| EvalError::NumericOverflow {
            name: name.into(),
        })
    }
}

fn parse_rational(token: &str) -> Option<Number> {
    let (numer, denom) = token.split_once('/')?;
    if numer.contains('/') || denom.contains('/') {
        return None;
    }

    let numer = numer.parse::<i64>().ok()?;
    let denom = denom.parse::<i64>().ok()?;
    normalize_exact_parts(numer as i128, denom as i128)
}

fn normalize_exact_parts(mut numer: i128, mut denom: i128) -> Option<Number> {
    if denom == 0 {
        return None;
    }

    if numer == 0 {
        return Some(Number::ExactInt(0));
    }

    if denom < 0 {
        numer = -numer;
        denom = -denom;
    }

    let divisor = gcd_i128(numer.abs(), denom);
    numer /= divisor;
    denom /= divisor;

    if denom == 1 {
        return Some(Number::ExactInt(i64::try_from(numer).ok()?));
    }

    Some(Number::ExactRational {
        numer: i64::try_from(numer).ok()?,
        denom: i64::try_from(denom).ok()?,
    })
}

fn exact_from_inexact(value: f64, name: &str) -> Result<Number, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::NumericOverflow { name: name.into() });
    }

    let rendered = format!("{value:?}");
    let (mantissa, exponent) = match rendered.find(['e', 'E']) {
        Some(index) => {
            let exponent = rendered[index + 1..].parse::<i32>().map_err(|_| {
                EvalError::NumericOverflow {
                    name: name.into(),
                }
            })?;
            (&rendered[..index], exponent)
        }
        None => (rendered.as_str(), 0),
    };

    let (negative, unsigned) = match mantissa.as_bytes().first().copied() {
        Some(b'-') => (true, &mantissa[1..]),
        Some(b'+') => (false, &mantissa[1..]),
        _ => (false, mantissa),
    };

    let (whole, fractional) = match unsigned.split_once('.') {
        Some(parts) => parts,
        None => (unsigned, ""),
    };

    let digits = format!("{whole}{fractional}");
    let mut numer = if digits.is_empty() {
        0
    } else {
        digits.parse::<i128>().map_err(|_| EvalError::NumericOverflow {
            name: name.into(),
        })?
    };

    if negative {
        numer = -numer;
    }

    let scale = fractional.len() as i32 - exponent;
    if scale <= 0 {
        numer *= pow10_i128((-scale) as u32, name)?;
        Number::normalize_exact(name, numer, 1)
    } else {
        let denom = pow10_i128(scale as u32, name)?;
        Number::normalize_exact(name, numer, denom)
    }
}

fn pow10_i128(power: u32, name: &str) -> Result<i128, EvalError> {
    let mut value = 1_i128;
    for _ in 0..power {
        value = value
            .checked_mul(10)
            .ok_or_else(|| EvalError::NumericOverflow { name: name.into() })?;
    }
    Ok(value)
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }
    left.max(1)
}
