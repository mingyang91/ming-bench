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

/// Skip whitespace in the char stream.
fn skip_whitespace(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    while chars.peek().is_some_and(|c| c.is_whitespace()) {
        chars.next();
    }
}

/// Tokenize a single element from the char stream, returning the change in
/// paren depth (1 for `(`, -1 for `)`, 0 otherwise). Returns `None` at EOF.
fn tokenize_one(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    tokens: &mut Vec<String>,
) -> Option<i32> {
    skip_whitespace(chars);
    match chars.peek() {
        Some(&'(') => {
            tokens.push("(".to_string());
            chars.next();
            Some(1)
        }
        Some(&')') => {
            tokens.push(")".to_string());
            chars.next();
            Some(-1)
        }
        Some(&'\'') => {
            tokens.push("(".to_string());
            tokens.push("quote".to_string());
            chars.next();
            collect_one_expr(chars, tokens);
            tokens.push(")".to_string());
            Some(0)
        }
        Some(&'"') => {
            chars.next();
            tokens.push(read_string_literal(chars));
            Some(0)
        }
        Some(_) => {
            tokens.push(read_atom(chars));
            Some(0)
        }
        None => None,
    }
}

/// Collect tokens for exactly one S-expression from the char stream.
fn collect_one_expr(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    tokens: &mut Vec<String>,
) {
    skip_whitespace(chars);
    match chars.peek() {
        Some(&'(') => {
            tokens.push("(".to_string());
            chars.next();
            let mut depth = 1u32;
            while depth > 0 {
                match tokenize_one(chars, tokens) {
                    Some(delta) => depth = depth.wrapping_add_signed(delta),
                    None => break,
                }
            }
        }
        Some(&'\'') => {
            tokens.push("(".to_string());
            tokens.push("quote".to_string());
            chars.next();
            collect_one_expr(chars, tokens);
            tokens.push(")".to_string());
        }
        Some(&'"') => {
            chars.next();
            tokens.push(read_string_literal(chars));
        }
        Some(_) => tokens.push(read_atom(chars)),
        None => {}
    }
}

/// Tokenize input into a flat list of tokens (atoms, parens, strings).
pub(super) fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while chars.peek().is_some() {
        if tokenize_one(&mut chars, &mut tokens).is_none() {
            break;
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
