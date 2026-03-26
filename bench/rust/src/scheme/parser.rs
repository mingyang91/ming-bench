use super::{error::SourcePos, EvalError, Expr, ExprKind};

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    pos: SourcePos,
}

pub(super) fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    let (tokens, eof_pos) = tokenize(input)?;
    Parser::new(tokens, eof_pos).parse_program()
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_pos: SourcePos,
}

impl Parser {
    fn new(tokens: Vec<Token>, eof_pos: SourcePos) -> Self {
        Self {
            tokens,
            index: 0,
            eof_pos,
        }
    }

    fn parse_program(mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.index < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or_else(|| EvalError::UnexpectedEof.with_position(self.eof_pos))?;
        self.index += 1;

        match token.kind {
            TokenKind::LParen => self.parse_list(token.pos),
            TokenKind::RParen => Err(EvalError::UnexpectedToken {
                token: ")".to_string(),
            }
            .with_position(token.pos)),
            TokenKind::Integer(value) => Ok(Expr::new(ExprKind::Integer(value), token.pos)),
            TokenKind::Boolean(value) => Ok(Expr::new(ExprKind::Boolean(value), token.pos)),
            TokenKind::Char(value) => Ok(Expr::new(ExprKind::Char(value), token.pos)),
            TokenKind::String(value) => Ok(Expr::new(ExprKind::String(value), token.pos)),
            TokenKind::Symbol(value) => Ok(Expr::new(ExprKind::Symbol(value), token.pos)),
            TokenKind::Quote => self.parse_quote(token.pos),
        }
    }

    fn parse_list(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let mut items = Vec::new();
        while self.index < self.tokens.len() {
            if self
                .tokens
                .get(self.index)
                .is_some_and(|token| matches!(token.kind, TokenKind::RParen))
            {
                self.index += 1;
                return Ok(Expr::new(ExprKind::List(items), pos));
            }
            items.push(self.parse_expr()?);
        }
        Err(EvalError::UnexpectedEof.with_position(self.eof_pos))
    }

    fn parse_quote(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        Ok(Expr::new(
            ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".to_string()), pos),
                self.parse_expr()?,
            ]),
            pos,
        ))
    }
}

fn tokenize(input: &str) -> Result<(Vec<Token>, SourcePos), EvalError> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < bytes.len() {
        let pos = pos_from_index(input, index);
        match bytes[index] {
            b' ' | b'\n' | b'\r' | b'\t' => {
                index += 1;
            }
            b';' => {
                while index < bytes.len() && bytes[index] != b'\n' {
                    index += 1;
                }
            }
            b'(' => {
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    pos,
                });
                index += 1;
            }
            b')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    pos,
                });
                index += 1;
            }
            b'\'' => {
                tokens.push(Token {
                    kind: TokenKind::Quote,
                    pos,
                });
                index += 1;
            }
            b'"' => {
                let (value, next_index) =
                    parse_string(input, index + 1).map_err(|err| err.with_position(pos))?;
                tokens.push(Token {
                    kind: TokenKind::String(value),
                    pos,
                });
                index = next_index;
            }
            b'#' => {
                if let Some((kind, next_index)) = parse_hash_literal(input, index) {
                    tokens.push(Token { kind, pos });
                    index = next_index;
                } else {
                    return Err(EvalError::UnexpectedToken {
                        token: input[index..].to_string(),
                    }
                    .with_position(pos));
                }
            }
            _ => {
                let start = index;
                while index < bytes.len() && !is_token_boundary(bytes[index]) {
                    index += 1;
                }

                let atom = &input[start..index];
                let atom_pos = pos_from_index(input, start);
                if let Ok(value) = atom.parse::<i64>() {
                    tokens.push(Token {
                        kind: TokenKind::Integer(value),
                        pos: atom_pos,
                    });
                } else if atom.chars().next().is_some_and(|ch| ch == '+' || ch == '-')
                    && atom.len() > 1
                    && atom[1..].chars().all(|ch| ch.is_ascii_digit())
                {
                    return Err(EvalError::InvalidInteger {
                        value: atom.to_string(),
                    }
                    .with_position(atom_pos));
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Symbol(atom.to_string()),
                        pos: atom_pos,
                    });
                }
            }
        }
    }

    Ok((tokens, pos_from_index(input, input.len())))
}

fn parse_string(input: &str, mut index: usize) -> Result<(String, usize), EvalError> {
    let bytes = input.as_bytes();
    let mut value = String::new();

    while index < bytes.len() {
        match bytes[index] {
            b'"' => return Ok((value, index + 1)),
            b'\\' => {
                index += 1;
                let escaped = bytes.get(index).ok_or(EvalError::UnterminatedString)?;
                value.push(match escaped {
                    b'"' => '"',
                    b'\\' => '\\',
                    b'n' => '\n',
                    b't' => '\t',
                    other => *other as char,
                });
                index += 1;
            }
            other => {
                value.push(other as char);
                index += 1;
            }
        }
    }

    Err(EvalError::UnterminatedString)
}

fn parse_hash_literal(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    parse_boolean(input, index).or_else(|| parse_char_literal(input, index))
}

fn parse_boolean(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    let remainder = &input[index..];
    if remainder.starts_with("#t") && is_delimiter(input, index + 2) {
        Some((TokenKind::Boolean(true), index + 2))
    } else if remainder.starts_with("#f") && is_delimiter(input, index + 2) {
        Some((TokenKind::Boolean(false), index + 2))
    } else {
        None
    }
}

fn parse_char_literal(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    let remainder = input.get(index..)?;
    if !remainder.starts_with("#\\") {
        return None;
    }

    let bytes = input.as_bytes();
    let start = index + 2;
    let mut end = start;
    while end < bytes.len() && !is_token_boundary(bytes[end]) {
        end += 1;
    }

    let literal = input.get(start..end)?;
    let value = match literal {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = literal.chars();
            let value = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            value
        }
    };

    Some((TokenKind::Char(value), end))
}

fn pos_from_index(input: &str, index: usize) -> SourcePos {
    let mut line = 1;
    let mut col = 1;

    for byte in input.as_bytes().iter().take(index) {
        if *byte == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    SourcePos::new(line, col)
}

fn is_delimiter(input: &str, index: usize) -> bool {
    match input.as_bytes().get(index) {
        None => true,
        Some(byte) if is_token_boundary(*byte) => true,
        Some(_) => false,
    }
}

fn is_token_boundary(byte: u8) -> bool {
    matches!(
        byte,
        b' ' | b'\n' | b'\r' | b'\t' | b'(' | b')' | b'\'' | b';'
    )
}
