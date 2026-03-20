use super::error::EvalError;
use super::Value;

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Atom(String),
}

fn skip_line_comment(chars: &mut std::iter::Peekable<std::str::Chars>) {
    for c in chars.by_ref() {
        if c == '\n' {
            break;
        }
    }
}

fn read_string_literal(chars: &mut std::iter::Peekable<std::str::Chars>) -> String {
    let mut s = String::from("\"");
    for c in chars.by_ref() {
        s.push(c);
        if c == '"' {
            break;
        }
    }
    s
}

fn read_atom(chars: &mut std::iter::Peekable<std::str::Chars>) -> String {
    let mut atom = String::new();
    while let Some(&c) = chars.peek() {
        if c == '(' || c == ')' || c.is_whitespace() || c == ';' {
            break;
        }
        atom.push(c);
        chars.next();
    }
    atom
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            ';' => skip_line_comment(&mut chars),
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '"' => {
                chars.next();
                tokens.push(Token::Atom(read_string_literal(&mut chars)));
            }
            _ => tokens.push(Token::Atom(read_atom(&mut chars))),
        }
    }
    tokens
}

// --- Parser ---

fn parse_list(tokens: &[Token], start: usize) -> Result<(Value, usize), EvalError> {
    let mut elements = Vec::new();
    let mut i = start;
    loop {
        if i >= tokens.len() {
            return Err(EvalError::UnexpectedToken {
                token: "unexpected EOF".to_string(),
            });
        }
        if tokens[i] == Token::RParen {
            i += 1;
            break;
        }
        let (val, next) = parse_tokens(tokens, i)?;
        elements.push(val);
        i = next;
    }
    let list = elements
        .into_iter()
        .rev()
        .fold(Value::Nil, |acc, v| Value::Pair(Box::new(v), Box::new(acc)));
    Ok((list, i))
}

fn parse_tokens(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::EmptyInput);
    }

    match &tokens[pos] {
        Token::LParen => parse_list(tokens, pos + 1),
        Token::RParen => Err(EvalError::UnexpectedToken {
            token: ")".to_string(),
        }),
        Token::Atom(s) => {
            let val = parse_atom(s)?;
            Ok((val, pos + 1))
        }
    }
}

fn parse_atom(s: &str) -> Result<Value, EvalError> {
    if s == "#t" {
        return Ok(Value::Boolean(true));
    }
    if s == "#f" {
        return Ok(Value::Boolean(false));
    }
    if let Ok(n) = s.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
        let inner = &s[1..s.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }
    Ok(Value::Symbol(s.to_string()))
}

pub fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input);
    if tokens.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (val, next) = parse_tokens(&tokens, pos)?;
        exprs.push(val);
        pos = next;
    }
    Ok(exprs)
}
