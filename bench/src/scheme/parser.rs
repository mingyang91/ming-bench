use crate::scheme::EvalError;
use crate::scheme::value::Value;
use std::iter::Peekable;
use std::str::Chars;

/// Read a string literal (after the opening `"` has been consumed).
fn read_string(chars: &mut Peekable<Chars<'_>>) -> Result<String, EvalError> {
    let mut s = String::new();
    loop {
        match chars.next() {
            Some('\\') => match chars.next() {
                Some(esc) => s.push(esc),
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated string escape".into(),
                    })
                }
            },
            Some('"') => return Ok(s),
            Some(c) => s.push(c),
            None => {
                return Err(EvalError::Parse {
                    message: "unterminated string".into(),
                })
            }
        }
    }
}

/// Skip a line comment (starting from `;`).
fn skip_comment(chars: &mut Peekable<Chars<'_>>) {
    while let Some(&c) = chars.peek() {
        if c == '\n' {
            break;
        }
        chars.next();
    }
}

/// Read an atom token (symbol, number, boolean).
fn read_atom(chars: &mut Peekable<Chars<'_>>) -> String {
    let mut atom = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '(' || c == ')' || c == ';' {
            break;
        }
        atom.push(c);
        chars.next();
    }
    atom
}

/// Tokenize input into a list of tokens.
fn tokenize(input: &str) -> Result<Vec<String>, EvalError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            '"' => {
                chars.next();
                let s = read_string(&mut chars)?;
                tokens.push(format!("\"{s}\""));
            }
            ';' => skip_comment(&mut chars),
            '(' | ')' | '\'' => {
                tokens.push(ch.to_string());
                chars.next();
            }
            _ => tokens.push(read_atom(&mut chars)),
        }
    }
    Ok(tokens)
}

/// Parse a list from the token stream (after the opening `(` has been consumed).
fn parse_list(tokens: &[String], start: usize) -> Result<(Value, usize), EvalError> {
    let mut elements = Vec::new();
    let mut i = start;
    loop {
        if i >= tokens.len() {
            return Err(EvalError::Parse {
                message: "unmatched opening parenthesis".into(),
            });
        }
        if tokens[i] == ")" {
            return Ok((Value::List(elements), i + 1));
        }
        let (val, next) = parse_expr(tokens, i)?;
        elements.push(val);
        i = next;
    }
}

/// Parse a single expression from the token stream, returning the value
/// and the remaining token index.
fn parse_expr(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".into(),
        });
    }

    let token = &tokens[pos];

    if token == "(" {
        parse_list(tokens, pos + 1)
    } else if token == ")" {
        Err(EvalError::Parse {
            message: "unexpected closing parenthesis".into(),
        })
    } else if token == "'" {
        let (quoted, next) = parse_expr(tokens, pos + 1)?;
        Ok((Value::List(vec![Value::Symbol("quote".into()), quoted]), next))
    } else {
        Ok((parse_atom(token), pos + 1))
    }
}

fn parse_atom(token: &str) -> Value {
    if token == "#t" {
        return Value::Boolean(true);
    }
    if token == "#f" {
        return Value::Boolean(false);
    }
    if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        return Value::String(inner.to_string());
    }
    if let Ok(n) = token.parse::<i64>() {
        return Value::Integer(n);
    }
    Value::Symbol(token.to_string())
}

/// Parse all expressions from input string.
pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut expressions = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse_expr(&tokens, pos)?;
        expressions.push(expr);
        pos = next;
    }
    Ok(expressions)
}
