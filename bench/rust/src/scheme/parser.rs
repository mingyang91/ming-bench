use crate::scheme::EvalError;

/// A parsed S-expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    List(Vec<Expr>),
}

/// Tokenize input into a list of token strings.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            // Skip whitespace
            c if c.is_whitespace() => {
                i += 1;
            }
            // Skip line comments
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push("(".into());
                i += 1;
            }
            ')' => {
                tokens.push(")".into());
                i += 1;
            }
            '\'' => {
                tokens.push("'".into());
                i += 1;
            }
            // String literal
            '"' => {
                let mut s = String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            // #t, #f, #\char
            '#' => {
                let mut tok = String::new();
                tok.push('#');
                i += 1;
                while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
            // Symbol or number
            _ => {
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
                }
                tokens.push(tok);
            }
        }
    }

    tokens
}

/// Parse a single expression from the token stream, returning the expr
/// and the number of tokens consumed.
fn parse_expr(tokens: &[String], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".into(),
        });
    }

    let token = &tokens[pos];

    match token.as_str() {
        "(" => {
            let mut elements = Vec::new();
            let mut i = pos + 1;
            while i < tokens.len() && tokens[i] != ")" {
                let (expr, next) = parse_expr(tokens, i)?;
                elements.push(expr);
                i = next;
            }
            if i >= tokens.len() {
                return Err(EvalError::Parse {
                    message: "missing closing parenthesis".into(),
                });
            }
            Ok((Expr::List(elements), i + 1))
        }
        ")" => Err(EvalError::Parse {
            message: "unexpected closing parenthesis".into(),
        }),
        "'" => {
            let (expr, next) = parse_expr(tokens, pos + 1)?;
            Ok((Expr::List(vec![Expr::Symbol("quote".into()), expr]), next))
        }
        _ => Ok((parse_atom(token)?, pos + 1)),
    }
}

/// Parse an atom token into an Expr.
fn parse_atom(token: &str) -> Result<Expr, EvalError> {
    // Booleans
    if token == "#t" {
        return Ok(Expr::Boolean(true));
    }
    if token == "#f" {
        return Ok(Expr::Boolean(false));
    }

    // String literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        // Handle escape sequences
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
        return Ok(Expr::SchemeString(result));
    }

    // Integer
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Expr::Integer(n));
    }

    // Symbol
    Ok(Expr::Symbol(token.to_string()))
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
