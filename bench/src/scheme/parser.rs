//! Tokenizer and parser for S-expressions with source position tracking.

use super::{EvalError, Expr, Span};

/// Character-level scanner that tracks line and column.
struct Scanner<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            line: 1,
            col: 1,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            col: self.col,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|c| c.is_whitespace()) {
            self.next_char();
        }
    }
}

/// Read a string literal (opening `"` already consumed).
fn read_string_literal(scanner: &mut Scanner<'_>) -> String {
    let mut s = String::from('"');
    loop {
        match scanner.next_char() {
            Some('\\') => {
                s.push('\\');
                s.extend(scanner.next_char());
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

/// Read an atom token (non-delimiter chars).
fn read_atom(scanner: &mut Scanner<'_>) -> String {
    let mut tok = String::new();
    while let Some(&c) = scanner.peek() {
        if c.is_whitespace() || c == '(' || c == ')' || c == '"' {
            break;
        }
        tok.push(c);
        scanner.next_char();
    }
    tok
}

pub(super) type Token = (String, Span);

/// Tokenize a single element, returning paren depth change.
fn tokenize_one(scanner: &mut Scanner<'_>, tokens: &mut Vec<Token>) -> Option<i32> {
    scanner.skip_whitespace();
    let span = scanner.span();
    match scanner.peek() {
        Some(&'(') => {
            scanner.next_char();
            tokens.push(("(".to_string(), span));
            Some(1)
        }
        Some(&')') => {
            scanner.next_char();
            tokens.push((")".to_string(), span));
            Some(-1)
        }
        Some(&'\'') => {
            scanner.next_char();
            tokens.push(("(".to_string(), span));
            tokens.push(("quote".to_string(), span));
            collect_one_expr(scanner, tokens);
            tokens.push((")".to_string(), span));
            Some(0)
        }
        Some(&'"') => {
            scanner.next_char();
            tokens.push((read_string_literal(scanner), span));
            Some(0)
        }
        Some(_) => {
            tokens.push((read_atom(scanner), span));
            Some(0)
        }
        None => None,
    }
}

/// Collect tokens for exactly one S-expression.
fn collect_one_expr(scanner: &mut Scanner<'_>, tokens: &mut Vec<Token>) {
    scanner.skip_whitespace();
    let span = scanner.span();
    match scanner.peek() {
        Some(&'(') => {
            scanner.next_char();
            tokens.push(("(".to_string(), span));
            let mut depth = 1u32;
            while depth > 0 {
                match tokenize_one(scanner, tokens) {
                    Some(delta) => depth = depth.wrapping_add_signed(delta),
                    None => break,
                }
            }
        }
        Some(&'\'') => {
            scanner.next_char();
            tokens.push(("(".to_string(), span));
            tokens.push(("quote".to_string(), span));
            collect_one_expr(scanner, tokens);
            tokens.push((")".to_string(), span));
        }
        Some(&'"') => {
            scanner.next_char();
            tokens.push((read_string_literal(scanner), span));
        }
        Some(_) => tokens.push((read_atom(scanner), span)),
        None => {}
    }
}

/// Tokenize input into a flat list of tokens with source positions.
pub(super) fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut scanner = Scanner::new(input);
    while scanner.peek().is_some() {
        if tokenize_one(&mut scanner, &mut tokens).is_none() {
            break;
        }
    }
    tokens
}

/// Parse a list body (after the opening `(`) until the matching `)`.
fn parse_list(tokens: &[Token]) -> Result<(Vec<Expr>, &[Token]), EvalError> {
    let mut items = Vec::new();
    let mut remaining = tokens;
    loop {
        if remaining.first().map(|(s, _)| s.as_str()) == Some(")") {
            return Ok((items, &remaining[1..]));
        }
        let (expr, rest) = parse(remaining)?;
        items.push(expr);
        remaining = rest;
    }
}

/// Parse tokens into S-expression ASTs.
fn parse(tokens: &[Token]) -> Result<(Expr, &[Token]), EvalError> {
    let [(first, span), rest @ ..] = tokens else {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
        });
    };
    if first == "(" {
        let (items, remaining) = parse_list(rest)?;
        Ok((Expr::List(items, *span), remaining))
    } else if first == ")" {
        Err(EvalError::Parse {
            message: "unexpected ')'".to_string(),
        })
    } else {
        Ok((Expr::Atom(first.clone(), *span), rest))
    }
}

/// Parse all top-level expressions from token stream.
pub(super) fn parse_all(tokens: &[Token]) -> Result<Vec<Expr>, EvalError> {
    let mut exprs = Vec::new();
    let mut remaining = tokens;
    while !remaining.is_empty() {
        let (expr, rest) = parse(remaining)?;
        exprs.push(expr);
        remaining = rest;
    }
    Ok(exprs)
}
