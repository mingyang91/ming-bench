use std::cmp::Ordering;

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Integer(i64),
    Rational(Rational),
    Inexact(f64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Rational {
    pub fn numerator(self) -> i64 {
        self.numerator
    }

    pub fn denominator(self) -> i64 {
        self.denominator
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.numeric_eq(*other)
    }
}

impl Number {
    pub const fn integer(value: i64) -> Self {
        Self::Integer(value)
    }

    pub fn rational(numerator: i64, denominator: i64) -> Option<Self> {
        number_from_fraction(i128::from(numerator), i128::from(denominator))
    }

    pub fn parse(token: &str) -> Option<Self> {
        parse_rational(token)
            .or_else(|| token.parse::<i64>().ok().map(Self::Integer))
            .or_else(|| parse_inexact(token))
    }

    pub fn render(self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Rational(value) => format!("{}/{}", value.numerator(), value.denominator()),
            Self::Inexact(value) => render_inexact(value),
        }
    }

    pub fn is_exact(self) -> bool {
        !matches!(self, Self::Inexact(_))
    }

    pub fn is_inexact(self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    pub fn is_integer(self) -> bool {
        self.integer_value().is_some()
    }

    pub fn is_rational(self) -> bool {
        self.is_exact()
    }

    pub fn is_zero(self) -> bool {
        match self {
            Self::Integer(value) => value == 0,
            Self::Rational(value) => value.numerator() == 0,
            Self::Inexact(value) => value == 0.0,
        }
    }

    pub fn to_inexact(self) -> f64 {
        match self {
            Self::Integer(value) => value as f64,
            Self::Rational(value) => value.numerator() as f64 / value.denominator() as f64,
            Self::Inexact(value) => value,
        }
    }

    pub fn to_exact(self) -> Option<Self> {
        match self {
            exact @ (Self::Integer(_) | Self::Rational(_)) => Some(exact),
            Self::Inexact(value) => parse_decimal_to_exact(&render_inexact(value)),
        }
    }

    pub fn numerator(self) -> Option<i64> {
        match self.to_exact()? {
            Self::Integer(value) => Some(value),
            Self::Rational(value) => Some(value.numerator()),
            Self::Inexact(_) => None,
        }
    }

    pub fn denominator(self) -> Option<i64> {
        match self.to_exact()? {
            Self::Integer(_) => Some(1),
            Self::Rational(value) => Some(value.denominator()),
            Self::Inexact(_) => None,
        }
    }

    pub fn integer_value(self) -> Option<i64> {
        match self.to_exact()? {
            Self::Integer(value) => Some(value),
            Self::Rational(_) | Self::Inexact(_) => None,
        }
    }

    pub fn eqv(self, other: Self) -> bool {
        match (self, other) {
            (Self::Integer(left), Self::Integer(right)) => left == right,
            (Self::Rational(left), Self::Rational(right)) => left == right,
            (Self::Inexact(left), Self::Inexact(right)) => left == right,
            _ => false,
        }
    }

    pub fn numeric_eq(self, other: Self) -> bool {
        self.cmp_numeric(other)
            .is_some_and(|ordering| ordering == Ordering::Equal)
    }

    pub fn cmp_numeric(self, other: Self) -> Option<Ordering> {
        if let (Some(left), Some(right)) = (self.exact_parts(), other.exact_parts()) {
            return Some(compare_exact_parts(left, right));
        }
        if let (Some(left), Some(right)) = (exact_value(self), exact_value(other)) {
            return Some(compare_exact_parts(left, right));
        }

        self.to_inexact().partial_cmp(&other.to_inexact())
    }

    pub fn checked_abs(self) -> Option<Self> {
        match self {
            Self::Integer(value) => value.checked_abs().map(Self::Integer),
            Self::Rational(value) => number_from_fraction(
                i128::from(value.numerator()).abs(),
                i128::from(value.denominator()),
            ),
            Self::Inexact(value) => Some(Self::Inexact(value.abs())),
        }
    }

    pub fn checked_neg(self) -> Option<Self> {
        match self {
            Self::Integer(value) => value.checked_neg().map(Self::Integer),
            Self::Rational(value) => number_from_fraction(
                i128::from(value.numerator()).checked_neg()?,
                i128::from(value.denominator()),
            ),
            Self::Inexact(value) => Some(Self::Inexact(-value)),
        }
    }

    pub fn checked_add(self, other: Self) -> Option<Self> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                let numerator = i128::from(left_num)
                    .checked_mul(i128::from(right_den))?
                    .checked_add(i128::from(right_num).checked_mul(i128::from(left_den))?)?;
                let denominator = i128::from(left_den).checked_mul(i128::from(right_den))?;
                number_from_fraction(numerator, denominator)
            }
            _ => inexact_binary_operation(self, other, |left, right| left + right),
        }
    }

    pub fn checked_sub(self, other: Self) -> Option<Self> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => {
                let numerator = i128::from(left_num)
                    .checked_mul(i128::from(right_den))?
                    .checked_sub(i128::from(right_num).checked_mul(i128::from(left_den))?)?;
                let denominator = i128::from(left_den).checked_mul(i128::from(right_den))?;
                number_from_fraction(numerator, denominator)
            }
            _ => inexact_binary_operation(self, other, |left, right| left - right),
        }
    }

    pub fn checked_mul(self, other: Self) -> Option<Self> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => number_from_fraction(
                i128::from(left_num).checked_mul(i128::from(right_num))?,
                i128::from(left_den).checked_mul(i128::from(right_den))?,
            ),
            _ => inexact_binary_operation(self, other, |left, right| left * right),
        }
    }

    pub fn checked_div(self, other: Self) -> Option<Self> {
        match (self.exact_parts(), other.exact_parts()) {
            (Some((left_num, left_den)), Some((right_num, right_den))) => number_from_fraction(
                i128::from(left_num).checked_mul(i128::from(right_den))?,
                i128::from(left_den).checked_mul(i128::from(right_num))?,
            ),
            _ => inexact_binary_operation(self, other, |left, right| left / right),
        }
    }

    pub fn checked_pow(self, exponent: u32) -> Option<Self> {
        checked_pow_recursive(Self::Integer(1), self, exponent)
    }

    fn exact_parts(self) -> Option<(i64, i64)> {
        match self {
            Self::Integer(value) => Some((value, 1)),
            Self::Rational(value) => Some((value.numerator(), value.denominator())),
            Self::Inexact(_) => None,
        }
    }
}

fn inexact_binary_operation(
    left: Number,
    right: Number,
    combine: impl FnOnce(f64, f64) -> f64,
) -> Option<Number> {
    let value = combine(left.to_inexact(), right.to_inexact());
    value.is_finite().then_some(Number::Inexact(value))
}

fn parse_rational(token: &str) -> Option<Number> {
    let (numerator, denominator) = token.split_once('/')?;
    if denominator.contains('/') {
        return None;
    }

    let numerator = numerator.parse::<i64>().ok()?;
    let denominator = denominator.parse::<i64>().ok()?;
    Number::rational(numerator, denominator)
}

fn parse_inexact(token: &str) -> Option<Number> {
    (token.contains('.') || token.contains('e') || token.contains('E'))
        .then(|| token.parse::<f64>().ok())
        .flatten()
        .filter(|value| value.is_finite())
        .map(Number::Inexact)
}

fn render_inexact(value: f64) -> String {
    let rendered = value.to_string();
    if rendered.contains('.') || rendered.contains('e') || rendered.contains('E') {
        rendered
    } else {
        format!("{rendered}.0")
    }
}

fn parse_decimal_to_exact(token: &str) -> Option<Number> {
    let (sign, rest) = split_sign(token);
    let (mantissa, exponent) = split_exponent(rest)?;
    let (whole_part, fractional_part) = mantissa.split_once('.').unwrap_or((mantissa, ""));

    if whole_part.is_empty() && fractional_part.is_empty() {
        return None;
    }
    if !whole_part.chars().all(|ch| ch.is_ascii_digit())
        || !fractional_part.chars().all(|ch| ch.is_ascii_digit())
    {
        return None;
    }

    let digits = format!("{whole_part}{fractional_part}");
    if digits.is_empty() {
        return None;
    }

    let mut numerator = sign.checked_mul(digits.parse::<i128>().ok()?)?;
    let fractional_scale = checked_pow10(u32::try_from(fractional_part.len()).ok()?)?;
    let mut denominator = fractional_scale;

    match exponent.cmp(&0) {
        Ordering::Greater => {
            numerator = numerator.checked_mul(checked_pow10(exponent.unsigned_abs())?)?;
        }
        Ordering::Less => {
            denominator = denominator.checked_mul(checked_pow10(exponent.unsigned_abs())?)?;
        }
        Ordering::Equal => {}
    }

    number_from_fraction(numerator, denominator)
}

fn split_sign(token: &str) -> (i128, &str) {
    match token.as_bytes().first() {
        Some(b'+') => (1, &token[1..]),
        Some(b'-') => (-1, &token[1..]),
        _ => (1, token),
    }
}

fn split_exponent(token: &str) -> Option<(&str, i32)> {
    let Some(index) = token.find(['e', 'E']) else {
        return Some((token, 0));
    };

    let mantissa = &token[..index];
    let exponent = token[index + 1..].parse::<i32>().ok()?;
    Some((mantissa, exponent))
}

fn checked_pow10(exponent: u32) -> Option<i128> {
    10_i128.checked_pow(exponent)
}

fn checked_pow_recursive(result: Number, base: Number, exponent: u32) -> Option<Number> {
    if exponent == 0 {
        return Some(result);
    }

    let result = if exponent % 2 == 1 {
        result.checked_mul(base)?
    } else {
        result
    };
    let exponent = exponent / 2;
    if exponent == 0 {
        return Some(result);
    }

    checked_pow_recursive(result, base.checked_mul(base)?, exponent)
}

fn number_from_fraction(numerator: i128, denominator: i128) -> Option<Number> {
    if denominator == 0 {
        return None;
    }

    let (numerator, denominator) = normalize_fraction(numerator, denominator);
    if denominator == 1 {
        return i64::try_from(numerator).ok().map(Number::Integer);
    }

    Some(Number::Rational(Rational {
        numerator: i64::try_from(numerator).ok()?,
        denominator: i64::try_from(denominator).ok()?,
    }))
}

fn normalize_fraction(numerator: i128, denominator: i128) -> (i128, i128) {
    let (mut numerator, mut denominator) = if denominator < 0 {
        (-numerator, -denominator)
    } else {
        (numerator, denominator)
    };
    let divisor = gcd_i128(numerator, denominator);
    numerator /= divisor;
    denominator /= divisor;
    (numerator, denominator)
}

fn gcd_i128(left: i128, right: i128) -> i128 {
    let mut left = left.abs();
    let mut right = right.abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    left.max(1)
}

fn exact_value(number: Number) -> Option<(i64, i64)> {
    number
        .to_exact()
        .and_then(|exact| exact.exact_parts())
        .filter(|_| number.is_inexact())
}

fn compare_exact_parts(left: (i64, i64), right: (i64, i64)) -> Ordering {
    let (left_num, left_den) = left;
    let (right_num, right_den) = right;
    (i128::from(left_num) * i128::from(right_den))
        .cmp(&(i128::from(right_num) * i128::from(left_den)))
}
