pub mod error;

pub use error::EvalError;

use std::cmp::Ordering;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let value = eval_program(input)?;
    Ok(format_value(&value))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let value = eval_program(input)?;
    Ok((format_value(&value), String::new()))
}

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Bool(bool),
    Number(Number),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Bool(bool),
    Number(Number),
    List(Vec<Value>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Number {
    Exact(Rational),
    Inexact(f64),
}

impl Number {
    fn is_exact(self) -> bool {
        matches!(self, Self::Exact(_))
    }

    fn is_integer(self) -> bool {
        match self {
            Self::Exact(rational) => rational.is_integer(),
            Self::Inexact(value) => value.is_finite() && value.fract() == 0.0,
        }
    }

    fn to_f64(self) -> f64 {
        match self {
            Self::Exact(rational) => rational.to_f64(),
            Self::Inexact(value) => value,
        }
    }

    fn into_exact(self) -> Result<Rational, EvalError> {
        match self {
            Self::Exact(rational) => Ok(rational),
            Self::Inexact(value) => inexact_to_exact(value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Rational {
    numerator: i64,
    denominator: i64,
}

impl Rational {
    fn new(numerator: i64, denominator: i64) -> Result<Self, EvalError> {
        if denominator == 0 {
            return Err(EvalError::message("division by zero"));
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let divisor = gcd(numerator, denominator);
        Ok(Self {
            numerator: numerator / divisor,
            denominator: denominator / divisor,
        })
    }

    fn from_integer(value: i64) -> Self {
        Self {
            numerator: value,
            denominator: 1,
        }
    }

    fn add(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.denominator + other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn sub(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.denominator - other.numerator * self.denominator,
            self.denominator * other.denominator,
        )
    }

    fn mul(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.numerator,
            self.denominator * other.denominator,
        )
    }

    fn div(self, other: Self) -> Result<Self, EvalError> {
        Self::new(
            self.numerator * other.denominator,
            self.denominator * other.numerator,
        )
    }

    fn negate(self) -> Self {
        Self {
            numerator: -self.numerator,
            denominator: self.denominator,
        }
    }

    fn is_integer(self) -> bool {
        self.denominator == 1
    }

    fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }

    fn cmp(self, other: Self) -> Ordering {
        let left = self.numerator as i128 * other.denominator as i128;
        let right = other.numerator as i128 * self.denominator as i128;
        left.cmp(&right)
    }

    fn format(self) -> String {
        if self.denominator == 1 {
            self.numerator.to_string()
        } else {
            format!("{}/{}", self.numerator, self.denominator)
        }
    }
}

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::message("empty input"));
    }

    let mut last = None;
    for expr in expressions {
        last = Some(eval_expr(&expr)?);
    }

    last.ok_or_else(|| EvalError::message("empty input"))
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Number(value) => Ok(Value::Number(*value)),
        Expr::Symbol(name) => Err(EvalError::message(format!("unbound variable: {name}"))),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::message("cannot evaluate empty list"));
    };

    let Expr::Symbol(name) = operator else {
        return Err(EvalError::message("operator must be a symbol"));
    };

    let evaluated_args = arguments
        .iter()
        .map(eval_expr)
        .collect::<Result<Vec<_>, _>>()?;

    match name.as_str() {
        "list" => Ok(Value::List(evaluated_args)),
        "+" => Ok(Value::Number(eval_numeric_fold(name, &evaluated_args)?)),
        "-" => Ok(Value::Number(eval_numeric_fold(name, &evaluated_args)?)),
        "*" => Ok(Value::Number(eval_numeric_fold(name, &evaluated_args)?)),
        "/" => Ok(Value::Number(eval_numeric_fold(name, &evaluated_args)?)),
        "=" => eval_compare(name, &evaluated_args),
        "<" => eval_compare(name, &evaluated_args),
        ">" => eval_compare(name, &evaluated_args),
        "exact?" => Ok(Value::Bool(predicate_exact(&evaluated_args, name)?)),
        "inexact?" => Ok(Value::Bool(predicate_inexact(&evaluated_args, name)?)),
        "number?" => Ok(Value::Bool(predicate_number(&evaluated_args, name)?)),
        "integer?" => Ok(Value::Bool(predicate_integer(&evaluated_args, name)?)),
        "rational?" => Ok(Value::Bool(predicate_rational(&evaluated_args, name)?)),
        "exact->inexact" => Ok(Value::Number(exact_to_inexact(&evaluated_args)?)),
        "inexact->exact" => Ok(Value::Number(inexact_or_exact_to_exact(&evaluated_args)?)),
        "numerator" => Ok(Value::Number(numerator(&evaluated_args)?)),
        "denominator" => Ok(Value::Number(denominator(&evaluated_args)?)),
        _ => Err(EvalError::message(format!("unknown procedure: {name}"))),
    }
}

fn eval_numeric_fold(name: &str, args: &[Value]) -> Result<Number, EvalError> {
    let numbers = expect_numbers(args)?;
    let all_exact = numbers.iter().all(|number| number.is_exact());

    match name {
        "+" => {
            if all_exact {
                let mut total = Rational::from_integer(0);
                for number in numbers {
                    total = total.add(as_exact(number))?;
                }
                Ok(Number::Exact(total))
            } else {
                Ok(Number::Inexact(numbers.into_iter().map(Number::to_f64).sum()))
            }
        }
        "*" => {
            if all_exact {
                let mut total = Rational::from_integer(1);
                for number in numbers {
                    total = total.mul(as_exact(number))?;
                }
                Ok(Number::Exact(total))
            } else {
                let mut total = 1.0;
                for number in numbers {
                    total *= number.to_f64();
                }
                Ok(Number::Inexact(total))
            }
        }
        "-" => {
            if numbers.is_empty() {
                return Err(EvalError::message("- expects at least 1 argument"));
            }

            if all_exact {
                let first = as_exact(numbers[0]);
                let total = if numbers.len() == 1 {
                    first.negate()
                } else {
                    let mut total = first;
                    for number in &numbers[1..] {
                        total = total.sub(as_exact(*number))?;
                    }
                    total
                };
                Ok(Number::Exact(total))
            } else {
                let mut total = numbers[0].to_f64();
                if numbers.len() == 1 {
                    total = -total;
                } else {
                    for number in &numbers[1..] {
                        total -= number.to_f64();
                    }
                }
                Ok(Number::Inexact(total))
            }
        }
        "/" => {
            if numbers.len() < 2 {
                return Err(EvalError::message("/ expects at least 2 arguments"));
            }

            if all_exact {
                let mut total = as_exact(numbers[0]);
                for number in &numbers[1..] {
                    total = total.div(as_exact(*number))?;
                }
                Ok(Number::Exact(total))
            } else {
                let mut total = numbers[0].to_f64();
                for number in &numbers[1..] {
                    let divisor = number.to_f64();
                    if divisor == 0.0 {
                        return Err(EvalError::message("division by zero"));
                    }
                    total /= divisor;
                }
                Ok(Number::Inexact(total))
            }
        }
        _ => Err(EvalError::message(format!("unknown numeric operator: {name}"))),
    }
}

fn eval_compare(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    if numbers.len() < 2 {
        return Err(EvalError::message(format!("{name} expects at least 2 arguments")));
    }

    let result = numbers
        .windows(2)
        .all(|pair| compare_numbers(pair[0], pair[1], name));
    Ok(Value::Bool(result))
}

fn compare_numbers(left: Number, right: Number, operator: &str) -> bool {
    if left.is_exact() && right.is_exact() {
        let ordering = as_exact(left).cmp(as_exact(right));
        match operator {
            "=" => ordering == Ordering::Equal,
            "<" => ordering == Ordering::Less,
            ">" => ordering == Ordering::Greater,
            _ => false,
        }
    } else {
        let left = left.to_f64();
        let right = right.to_f64();
        match operator {
            "=" => left == right,
            "<" => left < right,
            ">" => left > right,
            _ => false,
        }
    }
}

fn predicate_exact(args: &[Value], name: &str) -> Result<bool, EvalError> {
    let value = expect_arity(name, args, 1)?;
    Ok(matches!(value, Value::Number(number) if number.is_exact()))
}

fn predicate_inexact(args: &[Value], name: &str) -> Result<bool, EvalError> {
    let value = expect_arity(name, args, 1)?;
    Ok(matches!(value, Value::Number(Number::Inexact(_))))
}

fn predicate_number(args: &[Value], name: &str) -> Result<bool, EvalError> {
    let value = expect_arity(name, args, 1)?;
    Ok(matches!(value, Value::Number(_)))
}

fn predicate_integer(args: &[Value], name: &str) -> Result<bool, EvalError> {
    let value = expect_arity(name, args, 1)?;
    Ok(matches!(value, Value::Number(number) if number.is_integer()))
}

fn predicate_rational(args: &[Value], name: &str) -> Result<bool, EvalError> {
    let value = expect_arity(name, args, 1)?;
    Ok(match value {
        Value::Number(Number::Exact(_)) => true,
        Value::Number(Number::Inexact(value)) => value.is_finite(),
        _ => false,
    })
}

fn exact_to_inexact(args: &[Value]) -> Result<Number, EvalError> {
    let number = expect_number(expect_arity("exact->inexact", args, 1)?)?;
    Ok(Number::Inexact(number.to_f64()))
}

fn inexact_or_exact_to_exact(args: &[Value]) -> Result<Number, EvalError> {
    let number = expect_number(expect_arity("inexact->exact", args, 1)?)?;
    Ok(Number::Exact(number.into_exact()?))
}

fn numerator(args: &[Value]) -> Result<Number, EvalError> {
    let number = expect_number(expect_arity("numerator", args, 1)?)?;
    let rational = number.into_exact()?;
    Ok(Number::Exact(Rational::from_integer(rational.numerator)))
}

fn denominator(args: &[Value]) -> Result<Number, EvalError> {
    let number = expect_number(expect_arity("denominator", args, 1)?)?;
    let rational = number.into_exact()?;
    Ok(Number::Exact(Rational::from_integer(rational.denominator)))
}

fn expect_arity<'a>(name: &str, args: &'a [Value], expected: usize) -> Result<&'a Value, EvalError> {
    if args.len() != expected {
        return Err(EvalError::message(format!(
            "{name} expects exactly {expected} argument{}",
            if expected == 1 { "" } else { "s" }
        )));
    }
    Ok(&args[0])
}

fn expect_number(value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(number) => Ok(*number),
        _ => Err(EvalError::message("expected number")),
    }
}

fn expect_numbers(args: &[Value]) -> Result<Vec<Number>, EvalError> {
    args.iter().map(expect_number).collect()
}

fn as_exact(number: Number) -> Rational {
    match number {
        Number::Exact(rational) => rational,
        Number::Inexact(_) => unreachable!("inexact number passed to exact-only path"),
    }
}

fn inexact_to_exact(value: f64) -> Result<Rational, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::message("cannot convert non-finite number to exact"));
    }

    let repr = format!("{value:?}");
    decimal_to_rational(&repr)
}

fn decimal_to_rational(token: &str) -> Result<Rational, EvalError> {
    if let Some((whole, fractional)) = split_decimal(token) {
        let scale = 10_i64
            .checked_pow(fractional.len() as u32)
            .ok_or_else(|| EvalError::message(format!("decimal is too precise: {token}")))?;

        let whole_value = parse_signed_integer(whole)?;
        let fractional_value = if fractional.is_empty() {
            0
        } else {
            fractional
                .parse::<i64>()
                .map_err(|_| EvalError::message(format!("invalid decimal: {token}")))?
        };

        let sign = if whole.starts_with('-') { -1 } else { 1 };
        let whole_magnitude = whole_value.abs();
        let numerator = whole_magnitude
            .checked_mul(scale)
            .and_then(|base| base.checked_add(fractional_value))
            .ok_or_else(|| EvalError::message(format!("decimal is too large: {token}")))?;

        return Rational::new(sign * numerator, scale);
    }

    Ok(Rational::from_integer(parse_signed_integer(token)?))
}

fn split_decimal(token: &str) -> Option<(&str, &str)> {
    let (whole, fractional) = token.split_once('.')?;
    if !looks_like_decimal_parts(whole, fractional) {
        return None;
    }
    Some((whole, fractional))
}

fn looks_like_decimal_parts(whole: &str, fractional: &str) -> bool {
    let whole_digits = if let Some(rest) = whole.strip_prefix('-') {
        !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit())
    } else {
        !whole.is_empty() && whole.chars().all(|ch| ch.is_ascii_digit())
    };

    whole_digits && !fractional.is_empty() && fractional.chars().all(|ch| ch.is_ascii_digit())
}

fn parse_signed_integer(token: &str) -> Result<i64, EvalError> {
    token
        .parse::<i64>()
        .map_err(|_| EvalError::message(format!("invalid integer: {token}")))
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Bool(value) => {
            if *value {
                "#t".into()
            } else {
                "#f".into()
            }
        }
        Value::Number(number) => format_number(*number),
        Value::List(items) => {
            let parts = items.iter().map(format_value).collect::<Vec<_>>();
            format!("({})", parts.join(" "))
        }
    }
}

fn format_number(number: Number) -> String {
    match number {
        Number::Exact(rational) => rational.format(),
        Number::Inexact(value) => format!("{value:?}"),
    }
}

fn parse_number_literal(token: &str) -> Result<Option<Number>, EvalError> {
    if let Some((numerator, denominator)) = token.split_once('/') {
        if !denominator.contains('/') && is_signed_integer(numerator) && is_signed_integer(denominator) {
            let numerator = parse_signed_integer(numerator)?;
            let denominator = parse_signed_integer(denominator)?;
            return Ok(Some(Number::Exact(Rational::new(numerator, denominator)?)));
        }
        return Ok(None);
    }

    if let Some((whole, fractional)) = split_decimal(token) {
        let _ = (whole, fractional);
        let value = token
            .parse::<f64>()
            .map_err(|_| EvalError::message(format!("invalid inexact number: {token}")))?;
        return Ok(Some(Number::Inexact(value)));
    }

    if is_signed_integer(token) {
        return Ok(Some(Number::Exact(Rational::from_integer(
            parse_signed_integer(token)?,
        ))));
    }

    Ok(None)
}

fn is_signed_integer(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }

    let digits = if let Some(rest) = token.strip_prefix('-') {
        if rest.is_empty() {
            return false;
        }
        rest
    } else {
        token
    };

    digits.chars().all(|ch| ch.is_ascii_digit())
}

fn gcd(left: i64, right: i64) -> i64 {
    let mut left = left.abs();
    let mut right = right.abs();

    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    if left == 0 { 1 } else { left }
}

struct Parser<'a> {
    chars: Vec<char>,
    index: usize,
    _source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            _source: source,
        }
    }

    fn parse_program(mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        loop {
            self.skip_ignored();
            if self.is_eof() {
                return Ok(expressions);
            }
            expressions.push(self.parse_expr()?);
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::message("unexpected ')'")),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::message("unexpected end of input")),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.index += 1;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek() {
                Some(')') => {
                    self.index += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::message("unterminated list")),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.index;
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.index += 1;
        }

        let token = self.chars[start..self.index].iter().collect::<String>();
        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ => match parse_number_literal(&token)? {
                Some(number) => Ok(Expr::Number(number)),
                None => Ok(Expr::Symbol(token)),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.index += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.peek() {
                    self.index += 1;
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            return;
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn is_eof(&self) -> bool {
        self.index >= self.chars.len()
    }
}

#[cfg(test)]
mod tests;
