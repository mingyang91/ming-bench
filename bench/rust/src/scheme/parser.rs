use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::{make_rational, Value};

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
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Char(char),
}

fn tokenize(input: &str) -> Result<Vec<(Token, Span)>, EvalError> {
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
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push((Token::Quote, Span::new(line, col)));
                i += 1;
                col += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push((Token::LParen, Span::new(line, col)));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((Token::RParen, Span::new(line, col)));
                i += 1;
                col += 1;
            }
            '"' => {
                let start_span = Span::new(line, col);
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
                    } else if chars[i] == '\n' {
                        s.push(chars[i]);
                        line += 1;
                        col = 0; // will be incremented below
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse {
                        message: "unterminated string".to_string(),
                        span: start_span,
                    });
                }
                i += 1; // closing quote
                col += 1;
                tokens.push((Token::String(s), start_span));
            }
            '#' => {
                let start_span = Span::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push((Token::Boolean(true), start_span));
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push((Token::Boolean(false), start_span));
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            // Character literal: #\x or #\space, #\newline, etc.
                            if i + 2 >= chars.len() {
                                return Err(EvalError::Parse {
                                    message: "unexpected end of input in character literal".to_string(),
                                    span: start_span,
                                });
                            }
                            // Read the character name
                            let char_start = i + 2;
                            let mut char_end = char_start + 1;
                            // Check if it's a named character (alphabetic chars following)
                            while char_end < chars.len()
                                && chars[char_end].is_alphabetic()
                                && char_end > char_start
                            {
                                char_end += 1;
                            }
                            let char_name: String = chars[char_start..char_end].iter().collect();
                            let c = match char_name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.len() == 1 => s.chars().next().expect("single char"),
                                other => {
                                    return Err(EvalError::Parse {
                                        message: format!("unknown character name: {other}"),
                                        span: start_span,
                                    });
                                }
                            };
                            let consumed = char_end - i;
                            tokens.push((Token::Char(c), start_span));
                            i = char_end;
                            col += consumed;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                message: format!(
                                    "unexpected character after #: {}",
                                    chars[i + 1]
                                ),
                                span: start_span,
                            });
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        message: "unexpected end of input after #".to_string(),
                        span: start_span,
                    });
                }
            }
            _ => {
                let start_span = Span::new(line, col);
                let start = i;
                while i < chars.len()
                    && !matches!(
                        chars[i],
                        ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"'
                    )
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push((Token::Integer(n), start_span));
                } else if let Some(tok) = try_parse_rational(&word) {
                    tokens.push((tok, start_span));
                } else if let Some(tok) = try_parse_float(&word) {
                    tokens.push((tok, start_span));
                } else {
                    tokens.push((Token::Symbol(word), start_span));
                }
            }
        }
    }
    Ok(tokens)
}

fn try_parse_rational(word: &str) -> Option<Token> {
    let slash_pos = word.find('/')?;
    if slash_pos == 0 || slash_pos == word.len() - 1 {
        return None;
    }
    let numer = word[..slash_pos].parse::<i64>().ok()?;
    let denom = word[slash_pos + 1..].parse::<i64>().ok()?;
    if denom == 0 {
        return None;
    }
    Some(Token::Rational(numer, denom))
}

fn try_parse_float(word: &str) -> Option<Token> {
    if !word.contains('.') {
        return None;
    }
    let f = word.parse::<f64>().ok()?;
    Some(Token::Float(f))
}

fn parse_expr(tokens: &[(Token, Span)], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
            span: tokens
                .last()
                .map_or(Span::default(), |(_, s)| *s),
        });
    }

    let (token, span) = &tokens[pos];
    match token {
        Token::Integer(n) => Ok((Value::Integer(*n, *span), pos + 1)),
        Token::Rational(n, d) => Ok((make_rational(*n, *d, *span), pos + 1)),
        Token::Float(f) => Ok((Value::Float(*f, *span), pos + 1)),
        Token::Boolean(b) => Ok((Value::Boolean(*b, *span), pos + 1)),
        Token::String(s) => {
            Ok((Value::immutable_string(s.clone(), *span), pos + 1))
        }
        Token::Symbol(s) => Ok((Value::Symbol(s.clone(), *span), pos + 1)),
        Token::Char(c) => Ok((Value::Char(*c, *span), pos + 1)),
        Token::LParen => {
            let list_span = *span;
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse {
                        message: "unclosed parenthesis".to_string(),
                        span: list_span,
                    });
                }
                if matches!(tokens[i].0, Token::RParen) {
                    return Ok((Value::List(elems, list_span), i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
        }
        Token::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("quote".to_string(), *span), inner],
                    *span,
                ),
                next,
            ))
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected )".to_string(),
            span: *span,
        }),
    }
}
