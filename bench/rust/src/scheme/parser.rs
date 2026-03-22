use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::Value;

/// Tokenize and parse Scheme source into a list of Value expressions,
/// each paired with the source position of its first token.
pub fn parse(input: &str) -> Result<Vec<(Value, Span)>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        let span = tokens[pos].span;
        let (val, next) = parse_expr(&tokens, pos)?;
        exprs.push((val, span));
        pos = next;
    }
    Ok(exprs)
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Atom(String),
    StringLit(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        let span = Span::new(line, col);
        match chars[i] {
            '\n' => {
                line += 1;
                col = 1;
                i += 1;
            }
            ' ' | '\t' | '\r' => {
                col += 1;
                i += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(Token { kind: TokenKind::LParen, span });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(Token { kind: TokenKind::RParen, span });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token { kind: TokenKind::Quote, span });
                i += 1;
                col += 1;
            }
            '"' => {
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
                    return Err(EvalError::parse("unterminated string").at(span));
                }
                i += 1; // closing quote
                col += 1;
                tokens.push(Token { kind: TokenKind::StringLit(s), span });
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
                    i += 1;
                    col += 1;
                }
                let atom: String = chars[start..i].iter().collect();
                tokens.push(Token { kind: TokenKind::Atom(atom), span });
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::parse("unexpected end of input"));
    }
    match &tokens[pos].kind {
        TokenKind::LParen => {
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::parse("unmatched '('").at(tokens[pos].span));
                }
                if matches!(tokens[i].kind, TokenKind::RParen) {
                    return Ok((Value::List(items), i + 1));
                }
                let (val, next) = parse_expr(tokens, i)?;
                items.push(val);
                i = next;
            }
        }
        TokenKind::RParen => Err(EvalError::parse("unexpected ')'").at(tokens[pos].span)),
        TokenKind::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Value::List(vec![Value::Symbol("quote".into()), inner]), next))
        }
        TokenKind::StringLit(s) => Ok((Value::Str(s.clone()), pos + 1)),
        TokenKind::Atom(a) => Ok((parse_atom(a), pos + 1)),
    }
}

fn parse_atom(s: &str) -> Value {
    if s == "#t" {
        return Value::Boolean(true);
    }
    if s == "#f" {
        return Value::Boolean(false);
    }
    if let Some(rest) = s.strip_prefix("#\\") {
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            _ if rest.len() == 1 => rest.chars().next().expect("single char after #\\"),
            _ => return Value::Symbol(s.to_string()),
        };
        return Value::Char(ch);
    }
    if let Ok(n) = s.parse::<i64>() {
        return Value::Integer(n);
    }
    Value::Symbol(s.to_string())
}
