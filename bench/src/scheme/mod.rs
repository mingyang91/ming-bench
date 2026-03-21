pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
        }
    }
}

/// Parse a single token from the input as a self-evaluating atom.
fn parse_atom(input: &str) -> Result<Value, EvalError> {
    if let Ok(n) = input.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if input == "#t" {
        return Ok(Value::Boolean(true));
    }
    if input == "#f" {
        return Ok(Value::Boolean(false));
    }
    if input.starts_with('"') && input.ends_with('"') && input.len() >= 2 {
        let inner = &input[1..input.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }
    Err(EvalError::Parse {
        message: format!("unexpected token: {input}"),
    })
}

/// Read a string literal (opening `"` already consumed) from `chars`.
fn read_string_literal(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut s = String::from('"');
    loop {
        match chars.next() {
            Some('\\') => {
                s.push('\\');
                s.extend(chars.next());
            }
            Some('"') => {
                s.push('"');
                break;
            }
            Some(c) => s.push(c),
            None => break,
        }
    }
    s
}

/// Read a bare token (non-whitespace, non-delimiter) from `chars`.
fn read_bare_token(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut tok = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '(' || c == ')' || c == '"' {
            break;
        }
        tok.push(c);
        chars.next();
    }
    tok
}

/// Tokenize input, respecting string literals as single tokens.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
        } else if ch == '"' {
            chars.next();
            tokens.push(read_string_literal(&mut chars));
        } else {
            tokens.push(read_bare_token(&mut chars));
        }
    }
    tokens
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    if tokens.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let mut last = None;
    for token in &tokens {
        last = Some(parse_atom(token)?);
    }
    Ok(last.expect("tokens is non-empty").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
