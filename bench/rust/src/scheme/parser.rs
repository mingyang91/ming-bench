use crate::scheme::error::{EvalError, SourcePos};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Number(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }

    fn symbol(name: impl Into<String>, pos: SourcePos) -> Self {
        Self::new(ExprKind::Symbol(name.into()), pos)
    }

    fn list(items: Vec<Expr>, pos: SourcePos) -> Self {
        Self::new(ExprKind::List(items), pos)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TokenKind {
    LeftParen,
    RightParen,
    Quote,
    Atom,
    String,
}

#[derive(Clone, Debug)]
struct Token {
    kind: TokenKind,
    text: String,
    pos: SourcePos,
}

pub fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    let (tokens, eof_pos) = tokenize(input)?;
    let mut parser = Parser {
        tokens,
        index: 0,
        eof_pos,
    };

    let mut exprs = Vec::new();
    while parser.has_next() {
        exprs.push(parser.parse_expr()?);
    }

    if exprs.is_empty() {
        return Err(EvalError::new("empty input", eof_pos));
    }

    Ok(exprs)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_pos: SourcePos,
}

impl Parser {
    fn has_next(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or_else(|| EvalError::new("unexpected end of input", self.eof_pos))?;
        self.index += 1;

        match token.kind {
            TokenKind::LeftParen => self.parse_list(token.pos),
            TokenKind::RightParen => Err(EvalError::new("unexpected ')'", token.pos)),
            TokenKind::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::list(
                    vec![Expr::symbol("quote", token.pos), quoted],
                    token.pos,
                ))
            }
            TokenKind::String => Ok(Expr::new(ExprKind::String(token.text), token.pos)),
            TokenKind::Atom => Ok(parse_atom(&token.text, token.pos)),
        }
    }

    fn parse_list(&mut self, start_pos: SourcePos) -> Result<Expr, EvalError> {
        let mut items = Vec::new();

        loop {
            let Some(token) = self.tokens.get(self.index) else {
                return Err(EvalError::new("unterminated list", start_pos));
            };

            if token.kind == TokenKind::RightParen {
                self.index += 1;
                return Ok(Expr::list(items, start_pos));
            }

            items.push(self.parse_expr()?);
        }
    }
}

fn parse_atom(raw: &str, pos: SourcePos) -> Expr {
    let kind = match raw {
        "#t" => ExprKind::Bool(true),
        "#f" => ExprKind::Bool(false),
        _ => match raw.parse::<i64>() {
            Ok(number) => ExprKind::Number(number),
            Err(_) => ExprKind::Symbol(raw.to_string()),
        },
    };

    Expr::new(kind, pos)
}

fn tokenize(input: &str) -> Result<(Vec<Token>, SourcePos), EvalError> {
    let mut scanner = Scanner::new(input);
    let mut tokens = Vec::new();

    while let Some(ch) = scanner.peek() {
        if ch.is_whitespace() {
            scanner.advance();
            continue;
        }

        if ch == ';' {
            while let Some(next) = scanner.peek() {
                scanner.advance();
                if next == '\n' {
                    break;
                }
            }
            continue;
        }

        let pos = scanner.pos();
        match ch {
            '(' => {
                scanner.advance();
                tokens.push(Token {
                    kind: TokenKind::LeftParen,
                    text: "(".to_string(),
                    pos,
                });
            }
            ')' => {
                scanner.advance();
                tokens.push(Token {
                    kind: TokenKind::RightParen,
                    text: ")".to_string(),
                    pos,
                });
            }
            '\'' => {
                scanner.advance();
                tokens.push(Token {
                    kind: TokenKind::Quote,
                    text: "'".to_string(),
                    pos,
                });
            }
            '"' => tokens.push(read_string_token(&mut scanner, pos)?),
            _ => tokens.push(read_atom_token(&mut scanner, pos)),
        }
    }

    Ok((tokens, scanner.pos()))
}

fn read_string_token(scanner: &mut Scanner, start_pos: SourcePos) -> Result<Token, EvalError> {
    scanner.advance();
    let mut text = String::new();

    loop {
        match scanner.peek() {
            Some('"') => {
                scanner.advance();
                return Ok(Token {
                    kind: TokenKind::String,
                    text,
                    pos: start_pos,
                });
            }
            Some('\\') => {
                scanner.advance();
                let escaped = scanner.peek().ok_or_else(|| {
                    EvalError::new("unterminated string literal", start_pos)
                })?;
                scanner.advance();
                match escaped {
                    '"' | '\\' => text.push(escaped),
                    'n' => text.push('\n'),
                    't' => text.push('\t'),
                    'r' => text.push('\r'),
                    other => text.push(other),
                }
            }
            Some(ch) => {
                scanner.advance();
                text.push(ch);
            }
            None => return Err(EvalError::new("unterminated string literal", start_pos)),
        }
    }
}

fn read_atom_token(scanner: &mut Scanner, start_pos: SourcePos) -> Token {
    let mut text = String::new();

    while let Some(ch) = scanner.peek() {
        if ch.is_whitespace() || matches!(ch, '(' | ')' | '\'') {
            break;
        }
        scanner.advance();
        text.push(ch);
    }

    Token {
        kind: TokenKind::Atom,
        text,
        pos: start_pos,
    }
}

struct Scanner {
    chars: Vec<char>,
    index: usize,
    line: usize,
    col: usize,
}

impl Scanner {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn pos(&self) -> SourcePos {
        SourcePos::new(self.line, self.col)
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }
}
