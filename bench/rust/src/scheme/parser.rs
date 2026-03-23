use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

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
    Boolean(bool),
    String(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '\'' => {
                tokens.push(Token::Quote);
                i += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
            '"' => {
                i += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
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
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse {
                        message: "unterminated string".to_string(),
                    });
                }
                i += 1; // closing quote
                tokens.push(Token::String(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                message: format!("unexpected character after #: {}", chars[i + 1]),
                            });
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        message: "unexpected end of input after #".to_string(),
                    });
                }
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Integer(n));
                } else {
                    tokens.push(Token::Symbol(word));
                }
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
        });
    }

    match &tokens[pos] {
        Token::Integer(n) => Ok((Value::Integer(*n), pos + 1)),
        Token::Boolean(b) => Ok((Value::Boolean(*b), pos + 1)),
        Token::String(s) => Ok((Value::String(s.clone()), pos + 1)),
        Token::Symbol(s) => Ok((Value::Symbol(s.clone()), pos + 1)),
        Token::LParen => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse {
                        message: "unclosed parenthesis".to_string(),
                    });
                }
                if matches!(tokens[i], Token::RParen) {
                    return Ok((Value::List(elems), i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
        }
        Token::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Value::List(vec![Value::Symbol("quote".to_string()), inner]),
                next,
            ))
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected )".to_string(),
        }),
    }
}
