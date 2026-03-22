use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Tokenize and parse Scheme source into a list of Value expressions.
pub fn parse(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        let (val, next) = parse_expr(&tokens, pos)?;
        exprs.push(val);
        pos = next;
    }
    Ok(exprs)
}

#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Atom(String),
    StringLit(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                // Line comment
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
                        message: "unterminated string".into(),
                    });
                }
                i += 1; // closing quote
                tokens.push(Token::StringLit(s));
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
                    i += 1;
                }
                let atom: String = chars[start..i].iter().collect();
                tokens.push(Token::Atom(atom));
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".into(),
        });
    }
    match &tokens[pos] {
        Token::LParen => {
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse {
                        message: "unmatched '('".into(),
                    });
                }
                if matches!(tokens[i], Token::RParen) {
                    return Ok((Value::List(items), i + 1));
                }
                let (val, next) = parse_expr(tokens, i)?;
                items.push(val);
                i = next;
            }
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected ')'".into(),
        }),
        Token::StringLit(s) => Ok((Value::Str(s.clone()), pos + 1)),
        Token::Atom(a) => Ok((parse_atom(a), pos + 1)),
    }
}

fn parse_atom(s: &str) -> Value {
    if s == "#t" {
        return Value::Boolean(true);
    }
    if s == "#f" {
        return Value::Boolean(false);
    }
    if let Ok(n) = s.parse::<i64>() {
        return Value::Integer(n);
    }
    Value::Symbol(s.to_string())
}
