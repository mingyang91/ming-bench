use crate::scheme::value::{Value, ValueKind, Pos};
use crate::scheme::EvalError;

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
    pos: Pos,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Atom(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        let cur_pos = (line, col);
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(Token { kind: TokenKind::LParen, pos: cur_pos }); i += 1; col += 1; }
            ')' => { tokens.push(Token { kind: TokenKind::RParen, pos: cur_pos }); i += 1; col += 1; }
            '\'' => { tokens.push(Token { kind: TokenKind::Quote, pos: cur_pos }); i += 1; col += 1; }
            '"' => {
                let start_pos = cur_pos;
                let mut s = String::new();
                i += 1; col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else if chars[i] == '\n' {
                        s.push(chars[i]);
                        line += 1; col = 0;
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; col += 1;
                tokens.push(Token { kind: TokenKind::Atom(format!("\"{}\"", s)), pos: start_pos });
            }
            '#' if i + 1 < chars.len() && chars[i + 1] == '\\' => {
                let start_pos = cur_pos;
                // Character literal: #\x or #\space etc.
                i += 2; col += 2;
                if i < chars.len() {
                    // Check for named characters (e.g., #\space, #\newline)
                    let ch_start = i;
                    if chars[i].is_alphabetic() {
                        while i < chars.len() && chars[i].is_alphabetic() {
                            i += 1; col += 1;
                        }
                        let name: String = chars[ch_start..i].iter().collect();
                        tokens.push(Token { kind: TokenKind::Atom(format!("#\\{}", name)), pos: start_pos });
                    } else {
                        // Single character like #\( or #\)
                        let c = chars[i];
                        i += 1; col += 1;
                        tokens.push(Token { kind: TokenKind::Atom(format!("#\\{}", c)), pos: start_pos });
                    }
                } else {
                    tokens.push(Token { kind: TokenKind::Atom("#\\".to_string()), pos: start_pos });
                }
            }
            _ => {
                let start_pos = cur_pos;
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|';'|'"') {
                    i += 1; col += 1;
                }
                let atom: String = chars[start..i].iter().collect();
                tokens.push(Token { kind: TokenKind::Atom(atom), pos: start_pos });
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[pos];
    match &token.kind {
        TokenKind::LParen => {
            let list_pos = token.pos;
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse("unclosed parenthesis".into()));
                }
                if matches!(tokens[i].kind, TokenKind::RParen) {
                    return Ok((Value::new(ValueKind::List(elems), list_pos), i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
        }
        TokenKind::RParen => Err(EvalError::Parse("unexpected ')'".into())),
        TokenKind::Quote => {
            let quote_pos = token.pos;
            let (expr, next) = parse_expr(tokens, pos + 1)?;
            Ok((Value::new(ValueKind::List(vec![
                Value::new(ValueKind::Symbol("quote".into()), quote_pos),
                expr,
            ]), quote_pos), next))
        }
        TokenKind::Atom(s) => Ok((parse_atom(s, token.pos), pos + 1)),
    }
}

fn parse_atom(s: &str, pos: Pos) -> Value {
    let kind = if s == "#t" {
        ValueKind::Boolean(true)
    } else if s == "#f" {
        ValueKind::Boolean(false)
    } else if s.starts_with("#\\") {
        let rest = &s[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            c if c.len() == 1 => c.chars().next().unwrap(),
            _ => return Value::new(ValueKind::Symbol(s.to_string()), pos),
        };
        ValueKind::Char(ch)
    } else if s.starts_with('"') && s.ends_with('"') {
        ValueKind::Str(s[1..s.len()-1].to_string())
    } else if let Ok(n) = s.parse::<i64>() {
        ValueKind::Integer(n)
    } else {
        ValueKind::Symbol(s.to_string())
    };
    Value::new(kind, pos)
}
