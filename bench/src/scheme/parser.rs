use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Skip a line comment (starting at `;`).
fn skip_comment(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    while let Some(&c) = chars.peek() {
        chars.next();
        if c == '\n' {
            break;
        }
    }
}

/// Read a string literal (opening `"` already consumed).
fn read_string(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut s = String::from('"');
    while let Some(&c) = chars.peek() {
        chars.next();
        s.push(c);
        if c == '"' {
            break;
        }
    }
    s
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';')
}

/// Read an atom token.
fn read_atom(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut atom = String::new();
    while let Some(&c) = chars.peek() {
        if is_delimiter(c) {
            break;
        }
        atom.push(c);
        chars.next();
    }
    atom
}

/// Tokenize input into a flat list of tokens.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            ';' => skip_comment(&mut chars),
            '(' | ')' => {
                tokens.push(ch.to_string());
                chars.next();
            }
            '"' => {
                chars.next();
                tokens.push(read_string(&mut chars));
            }
            _ => tokens.push(read_atom(&mut chars)),
        }
    }

    tokens
}

/// Parse all expressions from input.
pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();

    while pos < tokens.len() {
        let (expr, next_pos) = parse_expr(&tokens, pos)?;
        exprs.push(expr);
        pos = next_pos;
    }

    Ok(exprs)
}

fn parse_expr(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    let token = tokens.get(pos).ok_or_else(|| EvalError::Parse {
        message: "unexpected end of input".into(),
    })?;

    match token.as_str() {
        "(" => parse_list(tokens, pos + 1),
        ")" => Err(EvalError::Parse {
            message: "unexpected ')'".into(),
        }),
        _ => Ok((parse_atom(token), pos + 1)),
    }
}

fn parse_list(tokens: &[String], mut pos: usize) -> Result<(Value, usize), EvalError> {
    let mut elems = Vec::new();

    loop {
        let token = tokens.get(pos).ok_or_else(|| EvalError::Parse {
            message: "unterminated list".into(),
        })?;

        if token == ")" {
            return Ok((Value::List(elems), pos + 1));
        }

        let (expr, next_pos) = parse_expr(tokens, pos)?;
        elems.push(expr);
        pos = next_pos;
    }
}

fn parse_atom(token: &str) -> Value {
    if let Ok(n) = token.parse::<i64>() {
        return Value::Integer(n);
    }

    match token {
        "#t" => Value::Boolean(true),
        "#f" => Value::Boolean(false),
        s if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 => {
            Value::String(s[1..s.len() - 1].to_string())
        }
        _ => Value::Symbol(token.to_string()),
    }
}
