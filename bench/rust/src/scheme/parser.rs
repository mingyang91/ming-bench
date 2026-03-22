use super::{EvalError, Value};

pub(super) type Span = (usize, usize);

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push((Token::LParen, (line, col)));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((Token::RParen, (line, col)));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push((Token::Quote, (line, col)));
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
                            c => {
                                s.push('\\');
                                s.push(c);
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
                        line: start_line,
                        col: start_col,
                    });
                }
                i += 1; // closing quote
                col += 1;
                tokens.push((Token::Str(s), (start_line, start_col)));
            }
            '#' => {
                let start_col = col;
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push((Token::Boolean(true), (line, start_col)));
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push((Token::Boolean(false), (line, start_col)));
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            // Character literal: #\a, #\space, #\newline
                            if i + 2 >= chars.len() {
                                return Err(EvalError::Parse {
                                    message: "unexpected end after #\\".to_string(),
                                    line,
                                    col: start_col,
                                });
                            }
                            let char_start = i + 2;
                            let mut char_end = char_start + 1;
                            // Read a full word for named characters
                            while char_end < chars.len()
                                && chars[char_end].is_alphanumeric()
                            {
                                char_end += 1;
                            }
                            let name: String = chars[char_start..char_end].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.len() == 1 => s.chars().next().expect("single char"),
                                other => {
                                    return Err(EvalError::Parse {
                                        message: format!("unknown character name: {other}"),
                                        line,
                                        col: start_col,
                                    });
                                }
                            };
                            tokens.push((Token::Char(ch), (line, start_col)));
                            let consumed = 2 + name.len(); // #\ + name
                            i += consumed;
                            col += consumed;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                message: format!(
                                    "unexpected character after #: {}",
                                    chars[i + 1]
                                ),
                                line,
                                col: start_col,
                            });
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        message: "unexpected end after #".to_string(),
                        line,
                        col: start_col,
                    });
                }
            }
            _ => {
                // Symbol or number
                let start = i;
                let start_col = col;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push((Token::Integer(n), (line, start_col)));
                } else {
                    tokens.push((Token::Symbol(word), (line, start_col)));
                }
            }
        }
    }

    Ok(tokens)
}

fn parse(tokens: &[(Token, Span)], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
            line: 0,
            col: 0,
        });
    }

    let (token, span) = &tokens[pos];

    match token {
        Token::Integer(n) => Ok((Value::Integer(*n), pos + 1)),
        Token::Boolean(b) => Ok((Value::Boolean(*b), pos + 1)),
        Token::Str(s) => Ok((Value::Str(s.clone()), pos + 1)),
        Token::Char(c) => Ok((Value::Char(*c), pos + 1)),
        Token::Symbol(s) => Ok((Value::Symbol(s.clone(), *span), pos + 1)),
        Token::LParen => {
            let list_span = *span;
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse {
                        message: "unclosed parenthesis".to_string(),
                        line: list_span.0,
                        col: list_span.1,
                    });
                }
                if tokens[i].0 == Token::RParen {
                    return Ok((Value::List(items, list_span), i + 1));
                }
                let (val, next) = parse(tokens, i)?;
                items.push(val);
                i = next;
            }
        }
        Token::Quote => {
            let quote_span = *span;
            let (val, next) = parse(tokens, pos + 1)?;
            Ok((
                Value::List(
                    vec![Value::Symbol("quote".to_string(), quote_span), val],
                    quote_span,
                ),
                next,
            ))
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected )".to_string(),
            line: span.0,
            col: span.1,
        }),
    }
}

pub fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}
