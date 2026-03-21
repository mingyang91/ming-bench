use crate::scheme::error::ParseError;
use crate::scheme::value::Value;

/// Parse all expressions from the input string.
pub fn parse(input: &str) -> Result<Vec<Value>, ParseError> {
    let mut chars = input.chars().peekable();
    let mut exprs = Vec::new();

    loop {
        skip_whitespace(&mut chars);
        if chars.peek().is_none() {
            break;
        }
        exprs.push(parse_expr(&mut chars)?);
    }

    Ok(exprs)
}

fn skip_whitespace(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
        } else {
            break;
        }
    }
}

fn parse_expr(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, ParseError> {
    skip_whitespace(chars);

    match chars.peek() {
        None => Err(ParseError::UnexpectedEof),
        Some('"') => parse_string(chars),
        Some('#') => parse_boolean(chars),
        Some(&c) if c == '-' || c.is_ascii_digit() => parse_number_or_symbol(chars),
        Some(&c) => Err(ParseError::UnexpectedChar { ch: c }),
    }
}

fn parse_string(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, ParseError> {
    chars.next(); // consume opening quote
    let mut s = String::new();

    loop {
        match chars.next() {
            None => return Err(ParseError::UnterminatedString),
            Some('"') => return Ok(Value::String(s)),
            Some('\\') => match chars.next() {
                Some('n') => s.push('\n'),
                Some('t') => s.push('\t'),
                Some('\\') => s.push('\\'),
                Some('"') => s.push('"'),
                Some(c) => {
                    s.push('\\');
                    s.push(c);
                }
                None => return Err(ParseError::UnterminatedString),
            },
            Some(c) => s.push(c),
        }
    }
}

fn parse_boolean(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, ParseError> {
    chars.next(); // consume '#'
    match chars.next() {
        Some('t') => Ok(Value::Boolean(true)),
        Some('f') => Ok(Value::Boolean(false)),
        Some(c) => Err(ParseError::UnexpectedChar { ch: c }),
        None => Err(ParseError::UnexpectedEof),
    }
}

fn parse_number_or_symbol(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, ParseError> {
    let mut token = String::new();

    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        token.push(c);
        chars.next();
    }

    token
        .parse::<i64>()
        .map(Value::Integer)
        .map_err(|_| ParseError::UnexpectedChar {
            ch: token.chars().next().unwrap_or('?'),
        })
}
