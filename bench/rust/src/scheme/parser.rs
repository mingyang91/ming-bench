use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

/// Source position: (line, column), both 1-based.
pub type Span = (usize, usize);

/// Token produced by the lexer.
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

/// Compute (line, col) for each char index.
fn compute_positions(input: &str) -> Vec<Span> {
    let mut positions = Vec::with_capacity(input.len());
    let mut line = 1;
    let mut col = 1;
    for ch in input.chars() {
        positions.push((line, col));
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    positions
}

/// Tokenize input into a sequence of tokens with source positions.
fn tokenize(input: &str) -> Result<Vec<(Token, Span)>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let positions = compute_positions(input);
    let mut i = 0;

    while i < chars.len() {
        let span = positions[i];
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => i = skip_line_comment(&chars, i),
            '(' => { tokens.push((Token::LParen, span)); i += 1; }
            ')' => { tokens.push((Token::RParen, span)); i += 1; }
            '\'' => { tokens.push((Token::Quote, span)); i += 1; }
            '#' => { let (tok, next) = tokenize_hash(&chars, i)?; tokens.push((tok, span)); i = next; }
            '"' => { let (tok, next) = tokenize_string(&chars, i)?; tokens.push((tok, span)); i = next; }
            _ => { let (tok, next) = tokenize_atom(&chars, i); tokens.push((tok, span)); i = next; }
        }
    }
    Ok(tokens)
}

fn skip_line_comment(chars: &[char], start: usize) -> usize {
    let mut i = start;
    while i < chars.len() && chars[i] != '\n' {
        i += 1;
    }
    i
}

fn tokenize_hash(chars: &[char], start: usize) -> Result<(Token, usize), EvalError> {
    let i = start + 1;
    if i < chars.len() && chars[i] == 't' {
        Ok((Token::Boolean(true), i + 1))
    } else if i < chars.len() && chars[i] == 'f' {
        Ok((Token::Boolean(false), i + 1))
    } else if i < chars.len() && chars[i] == '\\' {
        tokenize_char_literal(chars, i + 1)
    } else {
        Err(EvalError::Parse {
            message: "unexpected character after #".into(),
        })
    }
}

fn tokenize_char_literal(chars: &[char], start: usize) -> Result<(Token, usize), EvalError> {
    if start >= chars.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of character literal".into(),
        });
    }
    // Collect the name (could be "space", "newline", or a single char)
    let mut end = start;
    while end < chars.len() && !is_delimiter(chars[end]) {
        end += 1;
    }
    let name: String = chars[start..end].iter().collect();
    let ch = match name.as_str() {
        "space" => ' ',
        "newline" => '\n',
        "tab" => '\t',
        s if s.len() == 1 => s.chars().next().expect("single char"),
        _ => {
            return Err(EvalError::Parse {
                message: format!("unknown character name: {name}"),
            })
        }
    };
    Ok((Token::Char(ch), end))
}

fn push_escape(s: &mut String, c: char) {
    match c {
        'n' => s.push('\n'),
        't' => s.push('\t'),
        '\\' => s.push('\\'),
        '"' => s.push('"'),
        other => { s.push('\\'); s.push(other); }
    }
}

fn tokenize_string(chars: &[char], start: usize) -> Result<(Token, usize), EvalError> {
    let mut i = start + 1;
    let mut s = String::new();
    while i < chars.len() && chars[i] != '"' {
        if chars[i] == '\\' && i + 1 < chars.len() {
            i += 1;
            push_escape(&mut s, chars[i]);
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
    Ok((Token::Str(s), i + 1))
}

fn tokenize_atom(chars: &[char], start: usize) -> (Token, usize) {
    let mut i = start;
    while i < chars.len() && !is_delimiter(chars[i]) {
        i += 1;
    }
    let word: String = chars[start..i].iter().collect();
    let tok = match word.parse::<i64>() {
        Ok(n) => Token::Integer(n),
        Err(_) => Token::Symbol(word),
    };
    (tok, i)
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
}

/// Parse all expressions from input, returning each with its source position.
pub fn parse(input: &str) -> Result<Vec<(Value, Span)>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        let (val, span, next) = parse_expr(&tokens, pos)?;
        exprs.push((val, span));
        pos = next;
    }
    Ok(exprs)
}

fn parse_expr(tokens: &[(Token, Span)], pos: usize) -> Result<(Value, Span, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".into(),
        });
    }
    let (ref tok, span) = tokens[pos];
    match tok {
        Token::Integer(n) => Ok((Value::Integer(*n), span, pos + 1)),
        Token::Boolean(b) => Ok((Value::Boolean(*b), span, pos + 1)),
        Token::Str(s) => Ok((Value::Str(s.clone()), span, pos + 1)),
        Token::Char(c) => Ok((Value::Char(*c), span, pos + 1)),
        Token::Symbol(s) => Ok((Value::Symbol(s.clone()), span, pos + 1)),
        Token::Quote => {
            let (inner, _, next) = parse_expr(tokens, pos + 1)?;
            Ok((Value::List(vec![Value::Symbol("quote".into()), inner]), span, next))
        }
        Token::LParen => {
            let (items, next) = parse_list_items(tokens, pos + 1)?;
            Ok((Value::List(items), span, next))
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected closing parenthesis".into(),
        }),
    }
}

fn parse_list_items(tokens: &[(Token, Span)], start: usize) -> Result<(Vec<Value>, usize), EvalError> {
    let mut items = Vec::new();
    let mut i = start;
    while i < tokens.len() && !matches!(tokens[i].0, Token::RParen) {
        let (val, _, next) = parse_expr(tokens, i)?;
        items.push(val);
        i = next;
    }
    if i >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unmatched opening parenthesis".into(),
        });
    }
    Ok((items, i + 1))
}
