use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};

pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        let (expr, next) = parse_expr(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    HashLParen,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    Syntax,
    Symbol(String),
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' | '\x0c' => {
                i += 1;
                col += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token { kind: TokenKind::LParen, line, col });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(Token { kind: TokenKind::RParen, line, col });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_line = line;
                let start_col = col;
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            other => {
                                s.push('\\');
                                s.push(other);
                            }
                        }
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 0;
                        }
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse { msg: "unterminated string".into() });
                }
                i += 1;
                col += 1;
                tokens.push(Token { kind: TokenKind::Str(s), line: start_line, col: start_col });
            }
            '\'' => {
                tokens.push(Token { kind: TokenKind::Quote, line, col });
                i += 1;
                col += 1;
            }
            '`' => {
                tokens.push(Token { kind: TokenKind::Quasiquote, line, col });
                i += 1;
                col += 1;
            }
            ',' => {
                if i + 1 < chars.len() && chars[i + 1] == '@' {
                    tokens.push(Token { kind: TokenKind::UnquoteSplicing, line, col });
                    i += 2;
                    col += 2;
                } else {
                    tokens.push(Token { kind: TokenKind::Unquote, line, col });
                    i += 1;
                    col += 1;
                }
            }
            '#' => {
                let tok_line = line;
                let tok_col = col;
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        '\'' => {
                            tokens.push(Token { kind: TokenKind::Syntax, line: tok_line, col: tok_col });
                            i += 2;
                            col += 2;
                        }
                        't' => {
                            tokens.push(Token { kind: TokenKind::Bool(true), line: tok_line, col: tok_col });
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push(Token { kind: TokenKind::Bool(false), line: tok_line, col: tok_col });
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            i += 2;
                            col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse { msg: "unexpected end after #\\".into() });
                            }
                            // Read the character name or single char
                            let start = i;
                            while i < chars.len()
                                && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                            {
                                i += 1;
                                col += 1;
                            }
                            let name: String = chars[start..i].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.len() == 1 => s.chars().next().expect("single char"),
                                other => {
                                    return Err(EvalError::Parse {
                                        msg: format!("unknown character name: {other}"),
                                    });
                                }
                            };
                            tokens.push(Token { kind: TokenKind::Char(ch), line: tok_line, col: tok_col });
                        }
                        '(' => {
                            tokens.push(Token { kind: TokenKind::HashLParen, line: tok_line, col: tok_col });
                            i += 2;
                            col += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                msg: format!("unexpected character after #: {}", chars[i + 1]),
                            });
                        }
                    }
                } else {
                    return Err(EvalError::Parse { msg: "unexpected end after #".into() });
                }
            }
            _ => {
                let start_col = col;
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token { kind: TokenKind::Int(n), line, col: start_col });
                } else if let Some(kind) = try_parse_rational(&word) {
                    tokens.push(Token { kind, line, col: start_col });
                } else if let Some(kind) = try_parse_float(&word) {
                    tokens.push(Token { kind, line, col: start_col });
                } else {
                    tokens.push(Token { kind: TokenKind::Symbol(word), line, col: start_col });
                }
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse { msg: "unexpected end of input".into() });
    }
    let token = &tokens[pos];
    let span = Span { line: token.line, col: token.col };
    match &token.kind {
        TokenKind::Int(n) => Ok((Value::Int(*n), pos + 1)),
        TokenKind::Float(f) => Ok((Value::Float(*f), pos + 1)),
        TokenKind::Rational(n, d) => Ok((make_rational_value(*n, *d), pos + 1)),
        TokenKind::Bool(b) => Ok((Value::Bool(*b), pos + 1)),
        TokenKind::Str(s) => Ok((Value::String(s.clone()), pos + 1)),
        TokenKind::Char(c) => Ok((Value::Char(*c), pos + 1)),
        TokenKind::Symbol(s) => Ok((Value::Symbol(s.clone(), Some(span)), pos + 1)),
        TokenKind::LParen => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            while i < tokens.len() {
                if matches!(tokens[i].kind, TokenKind::RParen) {
                    return Ok((Value::List(elems, Some(span)), i + 1));
                }
                // Check for dotted pair: (a b . c)
                if matches!(&tokens[i].kind, TokenKind::Symbol(s) if s == ".") {
                    i += 1; // skip the dot
                    let (cdr_val, next) = parse_expr(tokens, i)?;
                    i = next;
                    if i >= tokens.len() || !matches!(tokens[i].kind, TokenKind::RParen) {
                        return Err(EvalError::Parse {
                            msg: "expected ')' after dotted pair cdr".into(),
                        });
                    }
                    // Build the improper list: (a b . c) => Pair(a, Pair(b, c))
                    let result = elems.into_iter().rev().fold(cdr_val, |acc, elem| {
                        Value::Pair(Rc::new(RefCell::new((elem, acc))))
                    });
                    return Ok((result, i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
            Err(EvalError::Parse { msg: "unclosed parenthesis".into() })
        }
        TokenKind::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("quote".into(), Some(span)), inner],
                    Some(span),
                ),
                next,
            ))
        }
        TokenKind::Syntax => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("syntax".into(), Some(span)), inner],
                    Some(span),
                ),
                next,
            ))
        }
        TokenKind::HashLParen => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            while i < tokens.len() {
                if matches!(tokens[i].kind, TokenKind::RParen) {
                    return Ok((Value::Vector(Rc::new(RefCell::new(elems))), i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
            Err(EvalError::Parse { msg: "unclosed vector literal".into() })
        }
        TokenKind::Quasiquote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("quasiquote".into(), Some(span)), inner],
                    Some(span),
                ),
                next,
            ))
        }
        TokenKind::Unquote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("unquote".into(), Some(span)), inner],
                    Some(span),
                ),
                next,
            ))
        }
        TokenKind::UnquoteSplicing => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("unquote-splicing".into(), Some(span)), inner],
                    Some(span),
                ),
                next,
            ))
        }
        TokenKind::RParen => Err(EvalError::Parse { msg: "unexpected ')'".into() }),
    }
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational_value(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d.abs();
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Value::Int(n) } else { Value::Rational(n, d) }
}

fn try_parse_rational(word: &str) -> Option<TokenKind> {
    let (neg, rest) = if let Some(stripped) = word.strip_prefix('-') {
        (true, stripped)
    } else if let Some(stripped) = word.strip_prefix('+') {
        (false, stripped)
    } else {
        (false, word)
    };
    let parts: Vec<&str> = rest.splitn(2, '/').collect();
    if parts.len() != 2 {
        return None;
    }
    let n = parts[0].parse::<i64>().ok()?;
    let d = parts[1].parse::<i64>().ok()?;
    if d == 0 {
        return None;
    }
    let n = if neg { -n } else { n };
    Some(TokenKind::Rational(n, d))
}

fn try_parse_float(word: &str) -> Option<TokenKind> {
    // Must contain a dot to be a float literal
    if !word.contains('.') {
        return None;
    }
    let f = word.parse::<f64>().ok()?;
    Some(TokenKind::Float(f))
}
