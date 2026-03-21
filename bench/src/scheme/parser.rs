//! Tokenizer and parser for S-expressions.

use super::{EvalError, Expr};

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

/// Read an atom token (non-delimiter chars) from `chars`.
fn read_atom(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
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

/// Tokenize input into a flat list of tokens (atoms, parens, strings).
pub(super) fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&ch) = chars.peek() {
        match ch {
            _ if ch.is_whitespace() => {
                chars.next();
            }
            '(' | ')' => {
                tokens.push(ch.to_string());
                chars.next();
            }
            '"' => {
                chars.next();
                tokens.push(read_string_literal(&mut chars));
            }
            _ => tokens.push(read_atom(&mut chars)),
        }
    }
    tokens
}

/// Parse a list body (after the opening `(`) until the matching `)`.
fn parse_list(tokens: &[String]) -> Result<(Vec<Expr>, &[String]), EvalError> {
    let mut items = Vec::new();
    let mut remaining = tokens;
    loop {
        if remaining.first().map(|s| s.as_str()) == Some(")") {
            return Ok((items, &remaining[1..]));
        }
        let (expr, rest) = parse(remaining)?;
        items.push(expr);
        remaining = rest;
    }
}

/// Parse tokens into S-expression ASTs.
fn parse(tokens: &[String]) -> Result<(Expr, &[String]), EvalError> {
    let [first, rest @ ..] = tokens else {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
        });
    };
    if first == "(" {
        let (items, remaining) = parse_list(rest)?;
        Ok((Expr::List(items), remaining))
    } else if first == ")" {
        Err(EvalError::Parse {
            message: "unexpected ')'".to_string(),
        })
    } else {
        Ok((Expr::Atom(first.clone()), rest))
    }
}

/// Parse all top-level expressions from token stream.
pub(super) fn parse_all(tokens: &[String]) -> Result<Vec<Expr>, EvalError> {
    let mut exprs = Vec::new();
    let mut remaining = tokens;
    while !remaining.is_empty() {
        let (expr, rest) = parse(remaining)?;
        exprs.push(expr);
        remaining = rest;
    }
    Ok(exprs)
}
