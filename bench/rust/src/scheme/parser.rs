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
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum TokenKind {
    LParen,
    RParen,
    VectorOpen, // #(
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplicing,
    SyntaxQuote,
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
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
            ' ' | '\t' | '\r' | '\x0C' => { col += 1; i += 1; }
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
            '`' => {
                tokens.push(Token { kind: TokenKind::Quasiquote, span: Span { line, col } });
                i += 1; col += 1;
            }
            ',' => {
                if i + 1 < chars.len() && chars[i + 1] == '@' {
                    tokens.push(Token { kind: TokenKind::UnquoteSplicing, span: Span { line, col } });
                    i += 2; col += 2;
                } else {
                    tokens.push(Token { kind: TokenKind::Unquote, span: Span { line, col } });
                    i += 1; col += 1;
                }
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
                        '\'' => {
                            // #'expr => (syntax expr)
                            tokens.push(Token { kind: TokenKind::SyntaxQuote, span: Span { line, col: start_col } });
                            i += 2; col += 2;
                        }
                        't' => {
                            tokens.push(Token { kind: TokenKind::Boolean(true), span: Span { line, col: start_col } });
                            i += 2; col += 2;
                        }
                        'f' => {
                            tokens.push(Token { kind: TokenKind::Boolean(false), span: Span { line, col: start_col } });
                            i += 2; col += 2;
                        }
                        '\\' => {
                            // Character literal: #\x, #\space, #\newline, #\tab
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse(format!("unexpected end of character literal at {}:{}", line, start_col)));
                            }
                            let start_char = i;
                            // Read a word (for named chars like space/newline/tab) or single char
                            while i < chars.len() && !is_delimiter(chars[i]) {
                                i += 1; col += 1;
                            }
                            let name: String = chars[start_char..i].iter().collect();
                            let ch = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                s if s.chars().count() == 1 => s.chars().next().unwrap(),
                                _ => return Err(EvalError::Parse(format!("unknown character name: #\\{} at {}:{}", name, line, start_col))),
                            };
                            tokens.push(Token { kind: TokenKind::Char(ch), span: Span { line, col: start_col } });
                        }
                        '(' => {
                            tokens.push(Token { kind: TokenKind::VectorOpen, span: Span { line, col: start_col } });
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
                } else if let Some((n, d)) = parse_rational_literal(&word) {
                    tokens.push(Token { kind: TokenKind::Rational(n, d), span: Span { line, col: start_col } });
                } else if word.contains('.') || word.contains('e') || word.contains('E') {
                    if let Ok(f) = word.parse::<f64>() {
                        tokens.push(Token { kind: TokenKind::Float(f), span: Span { line, col: start_col } });
                    } else {
                        tokens.push(Token { kind: TokenKind::Symbol(word), span: Span { line, col: start_col } });
                    }
                } else {
                    tokens.push(Token { kind: TokenKind::Symbol(word), span: Span { line, col: start_col } });
                }
            }
        }
    }
    Ok(tokens)
}

fn parse_rational_literal(s: &str) -> Option<(i64, i64)> {
    let parts: Vec<&str> = s.splitn(2, '/').collect();
    if parts.len() == 2 {
        if let (Ok(n), Ok(d)) = (parts[0].parse::<i64>(), parts[1].parse::<i64>()) {
            if d != 0 {
                return Some((n, d));
            }
        }
    }
    None
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
        TokenKind::Float(f) => Ok((Expr { kind: ExprKind::Float(*f), span }, pos + 1)),
        TokenKind::Rational(n, d) => Ok((Expr { kind: ExprKind::Rational(*n, *d), span }, pos + 1)),
        TokenKind::Boolean(b) => Ok((Expr { kind: ExprKind::Boolean(*b), span }, pos + 1)),
        TokenKind::Str(s) => Ok((Expr { kind: ExprKind::Str(s.clone()), span }, pos + 1)),
        TokenKind::Char(c) => Ok((Expr { kind: ExprKind::Char(*c), span }, pos + 1)),
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
        TokenKind::Quasiquote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quasiquote".into()), span },
                    inner,
                ]),
                span,
            }, next))
        }
        TokenKind::Unquote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("unquote".into()), span },
                    inner,
                ]),
                span,
            }, next))
        }
        TokenKind::UnquoteSplicing => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("unquote-splicing".into()), span },
                    inner,
                ]),
                span,
            }, next))
        }
        TokenKind::SyntaxQuote => {
            let (inner, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("syntax".into()), span },
                    inner,
                ]),
                span,
            }, next))
        }
        TokenKind::VectorOpen => {
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse(format!("unclosed vector literal at {}:{}", span.line, span.col)));
                }
                if matches!(&tokens[i].kind, TokenKind::RParen) {
                    // Desugar #(a b c) into (vector a b c)
                    let mut elems = vec![Expr { kind: ExprKind::Symbol("vector".into()), span }];
                    elems.extend(items);
                    return Ok((Expr { kind: ExprKind::List(elems), span }, i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                items.push(expr);
                i = next;
            }
        }
        TokenKind::RParen => Err(EvalError::Parse(format!("unexpected ) at {}:{}", span.line, span.col))),
    }
}
