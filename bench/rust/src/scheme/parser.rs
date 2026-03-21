use crate::scheme::error::EvalError;

/// A parsed S-expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

/// Read a string literal (after the opening `"`).
fn read_string(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut s = String::from('"');
    while let Some(c) = chars.next() {
        s.push(c);
        match c {
            '\\' => if let Some(escaped) = chars.next() { s.push(escaped); },
            '"' => break,
            _ => {}
        }
    }
    s
}

/// Skip a line comment (after the opening `;`).
fn skip_comment(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
    while let Some(&c) = chars.peek() {
        chars.next();
        if c == '\n' {
            break;
        }
    }
}

/// Read an atom token.
fn read_atom(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut atom = String::new();
    while let Some(&c) = chars.peek() {
        if matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')') {
            break;
        }
        atom.push(c);
        chars.next();
    }
    atom
}

/// Tokenize input into a list of token strings.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            '(' | ')' => {
                tokens.push(ch.to_string());
                chars.next();
            }
            '"' => {
                chars.next();
                tokens.push(read_string(&mut chars));
            }
            ';' => skip_comment(&mut chars),
            _ => tokens.push(read_atom(&mut chars)),
        }
    }
    tokens
}

/// Parse a sequence of tokens into an Expr.
fn parse_tokens(tokens: &[String], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }

    let token = &tokens[*pos];
    *pos += 1;

    match token.as_str() {
        "(" => {
            let mut list = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != ")" {
                list.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing parenthesis".into()));
            }
            *pos += 1; // consume ')'
            Ok(Expr::List(list))
        }
        ")" => Err(EvalError::Parse("unexpected ')'".into())),
        _ => Ok(parse_atom(token)),
    }
}

/// Parse an atom token into an Expr.
fn parse_atom(token: &str) -> Expr {
    if token == "#t" {
        return Expr::Boolean(true);
    }
    if token == "#f" {
        return Expr::Boolean(false);
    }
    if let Ok(n) = token.parse::<i64>() {
        return Expr::Integer(n);
    }
    if token.starts_with('"') && token.ends_with('"') {
        return Expr::String(token[1..token.len() - 1].to_string());
    }
    Expr::Symbol(token.to_string())
}

/// Parse input string into a list of expressions.
pub fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}
