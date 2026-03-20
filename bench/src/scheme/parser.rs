use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Parse all expressions from the input string.
pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    let mut exprs = Vec::new();
    let mut chars = input.chars().peekable();

    while chars.peek().is_some() {
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
) -> Result<Value, EvalError> {
    skip_whitespace(chars);

    match chars.peek() {
        None => Err(EvalError::Parse {
            msg: "unexpected end of input".into(),
        }),
        Some('"') => parse_string(chars),
        Some('#') => parse_boolean(chars),
        _ => parse_number_or_symbol(chars),
    }
}

fn parse_string(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, EvalError> {
    chars.next(); // consume opening "
    let mut s = String::new();
    loop {
        match chars.next() {
            None => {
                return Err(EvalError::Parse {
                    msg: "unterminated string".into(),
                })
            }
            Some('"') => return Ok(Value::Str(s)),
            Some('\\') => match chars.next() {
                Some('n') => s.push('\n'),
                Some('t') => s.push('\t'),
                Some('\\') => s.push('\\'),
                Some('"') => s.push('"'),
                Some(c) => s.push(c),
                None => {
                    return Err(EvalError::Parse {
                        msg: "unterminated escape in string".into(),
                    })
                }
            },
            Some(c) => s.push(c),
        }
    }
}

fn parse_boolean(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, EvalError> {
    chars.next(); // consume #
    match chars.next() {
        Some('t') => Ok(Value::Boolean(true)),
        Some('f') => Ok(Value::Boolean(false)),
        _ => Err(EvalError::Parse {
            msg: "expected #t or #f".into(),
        }),
    }
}

fn parse_number_or_symbol(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Value, EvalError> {
    let mut token = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }
        token.push(c);
        chars.next();
    }

    if let Ok(n) = token.parse::<i64>() {
        return Ok(Value::Integer(n));
    }

    Err(EvalError::Parse {
        msg: format!("unexpected token: {token}"),
    })
}
