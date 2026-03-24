use crate::scheme::EvalError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    span: Span,
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(Token { kind: TokenKind::LParen, span: Span { line, col } });
                i += 1; col += 1;
            }
            ')' => {
                tokens.push(Token { kind: TokenKind::RParen, span: Span { line, col } });
                i += 1; col += 1;
            }
            '\'' => {
                tokens.push(Token { kind: TokenKind::Quote, span: Span { line, col } });
                i += 1; col += 1;
            }
            '"' => {
                let start_col = col;
                let start_line = line;
                i += 1; col += 1;
                let mut s = String::new();
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
                    } else {
                        if chars[i] == '\n' { line += 1; col = 0; }
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse(format!("unterminated string at {}:{}", start_line, start_col)));
                }
                i += 1; col += 1; // skip closing "
                tokens.push(Token { kind: TokenKind::Str(s), span: Span { line: start_line, col: start_col } });
            }
            '#' => {
                let start_col = col;
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token { kind: TokenKind::Boolean(true), span: Span { line, col: start_col } });
                            i += 2; col += 2;
                        }
                        'f' => {
                            tokens.push(Token { kind: TokenKind::Boolean(false), span: Span { line, col: start_col } });
                            i += 2; col += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{} at {}:{}", chars[i + 1], line, col))),
                    }
                } else {
                    return Err(EvalError::Parse(format!("unexpected # at {}:{}", line, col)));
                }
            }
            _ => {
                let start = i;
                let start_col = col;
                while i < chars.len() && !is_delimiter(chars[i]) {
                    i += 1; col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token { kind: TokenKind::Integer(n), span: Span { line, col: start_col } });
                } else {
                    tokens.push(Token { kind: TokenKind::Symbol(word), span: Span { line, col: start_col } });
                }
            }
        }
    }
    Ok(tokens)
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
}

pub fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    parse_all(&tokens)
}

fn parse_all(tokens: &[Token]) -> Result<Vec<Expr>, EvalError> {
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        let (expr, next) = parse_expr(tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let span = tokens[pos].span;
    match &tokens[pos].kind {
        TokenKind::Integer(n) => Ok((Expr { kind: ExprKind::Integer(*n), span }, pos + 1)),
        TokenKind::Boolean(b) => Ok((Expr { kind: ExprKind::Boolean(*b), span }, pos + 1)),
        TokenKind::Str(s) => Ok((Expr { kind: ExprKind::Str(s.clone()), span }, pos + 1)),
        TokenKind::Symbol(s) => Ok((Expr { kind: ExprKind::Symbol(s.clone()), span }, pos + 1)),
        TokenKind::LParen => {
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse(format!("unclosed parenthesis at {}:{}", span.line, span.col)));
                }
                if matches!(&tokens[i].kind, TokenKind::RParen) {
                    return Ok((Expr { kind: ExprKind::List(items), span }, i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                items.push(expr);
                i = next;
            }
        }
        TokenKind::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), span },
                    inner,
                ]),
                span,
            }, next))
        }
        TokenKind::RParen => Err(EvalError::Parse(format!("unexpected ) at {}:{}", span.line, span.col))),
    }
}
