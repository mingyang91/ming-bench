use crate::scheme::EvalError;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
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
enum Token {
    LParen,
    RParen,
    Atom(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                // Line comment
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
            '"' => {
                // String literal
                i += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
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
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; // skip closing quote
                tokens.push(Token::Atom(format!("\"{}\"", s)));
            }
            '#' if i + 1 < chars.len() => {
                match chars[i + 1] {
                    't' => {
                        tokens.push(Token::Atom("#t".into()));
                        i += 2;
                    }
                    'f' => {
                        tokens.push(Token::Atom("#f".into()));
                        i += 2;
                    }
                    _ => {
                        // Collect as atom
                        let start = i;
                        while i < chars.len()
                            && !chars[i].is_whitespace()
                            && chars[i] != '('
                            && chars[i] != ')'
                        {
                            i += 1;
                        }
                        let s: String = chars[start..i].iter().collect();
                        tokens.push(Token::Atom(s));
                    }
                }
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !chars[i].is_whitespace()
                    && chars[i] != '('
                    && chars[i] != ')'
                    && chars[i] != ';'
                {
                    i += 1;
                }
                let s: String = chars[start..i].iter().collect();
                tokens.push(Token::Atom(s));
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[pos] {
        Token::LParen => {
            let mut elems = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse("unmatched opening parenthesis".into()));
                }
                if matches!(tokens[i], Token::RParen) {
                    return Ok((Expr::List(elems), i + 1));
                }
                let (expr, next) = parse_expr(tokens, i)?;
                elems.push(expr);
                i = next;
            }
        }
        Token::RParen => Err(EvalError::Parse("unexpected ')'".into())),
        Token::Atom(s) => Ok((parse_atom(s), pos + 1)),
    }
}

fn parse_atom(s: &str) -> Expr {
    if s == "#t" {
        Expr::Boolean(true)
    } else if s == "#f" {
        Expr::Boolean(false)
    } else if s.starts_with('"') && s.ends_with('"') {
        Expr::Str(s[1..s.len() - 1].to_string())
    } else if let Ok(n) = s.parse::<i64>() {
        Expr::Integer(n)
    } else {
        Expr::Symbol(s.to_string())
    }
}
