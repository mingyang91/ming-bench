use crate::scheme::EvalError;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    pub fn pos_str(&self) -> String {
        format!("{}:{}", self.line, self.col)
    }
}

pub fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
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
    line: usize,
    col: usize,
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
                tokens.push(Token { kind: TokenKind::LParen, line, col });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(Token { kind: TokenKind::RParen, line, col });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token { kind: TokenKind::Quote, line, col });
                i += 1;
                col += 1;
            }
            '"' => {
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
                    return Err(EvalError::Parse(format!("unterminated string at {line}:{start_col}")));
                }
                i += 1;
                col += 1;
                tokens.push(Token { kind: TokenKind::Atom(format!("\"{}\"", s)), line, col: start_col });
            }
            '#' if i + 1 < chars.len() => {
                let start_col = col;
                match chars[i + 1] {
                    't' => {
                        tokens.push(Token { kind: TokenKind::Atom("#t".into()), line, col: start_col });
                        i += 2;
                        col += 2;
                    }
                    'f' => {
                        tokens.push(Token { kind: TokenKind::Atom("#f".into()), line, col: start_col });
                        i += 2;
                        col += 2;
                    }
                    _ => {
                        let start = i;
                        while i < chars.len()
                            && !chars[i].is_whitespace()
                            && chars[i] != '('
                            && chars[i] != ')'
                        {
                            i += 1;
                            col += 1;
                        }
                        let s: String = chars[start..i].iter().collect();
                        tokens.push(Token { kind: TokenKind::Atom(s), line, col: start_col });
                    }
                }
            }
            _ => {
                let start = i;
                let start_col = col;
                while i < chars.len()
                    && !chars[i].is_whitespace()
                    && chars[i] != '('
                    && chars[i] != ')'
                    && chars[i] != ';'
                {
                    i += 1;
                    col += 1;
                }
                let s: String = chars[start..i].iter().collect();
                tokens.push(Token { kind: TokenKind::Atom(s), line, col: start_col });
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[pos];
    match &tok.kind {
        TokenKind::LParen => {
            let line = tok.line;
            let col = tok.col;
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse(format!("unmatched opening parenthesis at {line}:{col}")));
                }
                if matches!(&tokens[i].kind, TokenKind::RParen) {
                    return Ok((Expr { kind: ExprKind::List(elems), line, col }, i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
        }
        TokenKind::RParen => Err(EvalError::Parse(format!("unexpected ')' at {}:{}", tok.line, tok.col))),
        TokenKind::Quote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), line: tok.line, col: tok.col },
                    inner,
                ]),
                line: tok.line,
                col: tok.col,
            }, next))
        }
        TokenKind::Atom(s) => Ok((Expr { kind: parse_atom(s), line: tok.line, col: tok.col }, pos + 1)),
    }
}

fn parse_atom(s: &str) -> ExprKind {
    if s == "#t" {
        ExprKind::Boolean(true)
    } else if s == "#f" {
        ExprKind::Boolean(false)
    } else if s.starts_with('"') && s.ends_with('"') {
        ExprKind::Str(s[1..s.len() - 1].to_string())
    } else if let Ok(n) = s.parse::<i64>() {
        ExprKind::Integer(n)
    } else {
        ExprKind::Symbol(s.to_string())
    }
}
