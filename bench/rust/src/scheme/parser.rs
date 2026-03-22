use crate::scheme::error::{EvalError, EvalErrorKind, Span};

/// A parsed S-expression kind.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    List(Vec<Expr>),
}

/// A parsed S-expression with source position.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// A token with source position.
struct Token {
    text: String,
    span: Span,
}

/// Tokenize input into a list of tokens with position info.
fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            c if c.is_whitespace() => {
                if c == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                i += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token {
                    text: "(".into(),
                    span: Span { line, col },
                });
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push(Token {
                    text: ")".into(),
                    span: Span { line, col },
                });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token {
                    text: "'".into(),
                    span: Span { line, col },
                });
                i += 1;
                col += 1;
            }
            '"' => {
                let start_line = line;
                let start_col = col;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                        col += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token {
                    text: s,
                    span: Span {
                        line: start_line,
                        col: start_col,
                    },
                });
            }
            '#' => {
                let start_col = col;
                let mut tok = String::new();
                tok.push('#');
                i += 1;
                col += 1;
                while i < chars.len()
                    && !chars[i].is_whitespace()
                    && chars[i] != '('
                    && chars[i] != ')'
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token {
                    text: tok,
                    span: Span {
                        line,
                        col: start_col,
                    },
                });
            }
            _ => {
                let start_col = col;
                let mut tok = String::new();
                while i < chars.len()
                    && !chars[i].is_whitespace()
                    && chars[i] != '('
                    && chars[i] != ')'
                    && chars[i] != '"'
                    && chars[i] != ';'
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token {
                    text: tok,
                    span: Span {
                        line,
                        col: start_col,
                    },
                });
            }
        }
    }

    tokens
}

/// Parse a single expression from the token stream.
fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalErrorKind::Parse {
            message: "unexpected end of input".into(),
        }
        .into());
    }

    let token = &tokens[pos];

    match token.text.as_str() {
        "(" => {
            let span = token.span.clone();
            let mut elements = Vec::new();
            let mut i = pos + 1;
            while i < tokens.len() && tokens[i].text != ")" {
                let (expr, next) = parse_expr(tokens, i)?;
                elements.push(expr);
                i = next;
            }
            if i >= tokens.len() {
                return Err(EvalErrorKind::Parse {
                    message: "missing closing parenthesis".into(),
                }
                .at(&span));
            }
            Ok((
                Expr {
                    kind: ExprKind::List(elements),
                    span,
                },
                i + 1,
            ))
        }
        ")" => Err(EvalErrorKind::Parse {
            message: "unexpected closing parenthesis".into(),
        }
        .at(&token.span)),
        "'" => {
            let span = token.span.clone();
            let (expr, next) = parse_expr(tokens, pos + 1)?;
            Ok((
                Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".into()),
                            span: span.clone(),
                        },
                        expr,
                    ]),
                    span,
                },
                next,
            ))
        }
        _ => {
            let span = token.span.clone();
            let kind = parse_atom(&token.text)?;
            Ok((Expr { kind, span }, pos + 1))
        }
    }
}

/// Parse an atom token into an ExprKind.
fn parse_atom(token: &str) -> Result<ExprKind, EvalError> {
    // Booleans
    if token == "#t" {
        return Ok(ExprKind::Boolean(true));
    }
    if token == "#f" {
        return Ok(ExprKind::Boolean(false));
    }

    // String literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        let mut result = String::new();
        let chars: Vec<char> = inner.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '\\' && i + 1 < chars.len() {
                match chars[i + 1] {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    other => {
                        result.push('\\');
                        result.push(other);
                    }
                }
                i += 2;
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }
        return Ok(ExprKind::SchemeString(result));
    }

    // Integer
    if let Ok(n) = token.parse::<i64>() {
        return Ok(ExprKind::Integer(n));
    }

    // Symbol
    Ok(ExprKind::Symbol(token.to_string()))
}

/// Parse the full input into a list of top-level expressions.
pub fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;

    while pos < tokens.len() {
        let (expr, next) = parse_expr(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }

    Ok(exprs)
}
