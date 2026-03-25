pub mod error;

pub use error::EvalError;

use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// --- Tokenizer ---

#[derive(Debug, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            '"' => {
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
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; // skip closing "
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            c if c == '-' || c == '+' => {
                // Check if it's a number: sign followed by digit
                if i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    // But only if not preceded by something that makes it a symbol context
                    // i.e., if we're at start or after ( or whitespace
                    let is_number = i == 0
                        || matches!(tokens.last(), Some(Token::LParen) | None);
                    if is_number {
                        let start = i;
                        i += 1;
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            i += 1;
                        }
                        let num_str: String = chars[start..i].iter().collect();
                        tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                            EvalError::Parse(format!("invalid number: {num_str}"))
                        })?));
                    } else {
                        // It's a symbol like + or -
                        let start = i;
                        i += 1;
                        while i < chars.len() && is_symbol_char(chars[i]) {
                            i += 1;
                        }
                        let sym: String = chars[start..i].iter().collect();
                        tokens.push(Token::Symbol(sym));
                    }
                } else {
                    // It's a symbol
                    let start = i;
                    i += 1;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1;
                    }
                    let sym: String = chars[start..i].iter().collect();
                    tokens.push(Token::Symbol(sym));
                }
            }
            c if c.is_ascii_digit() => {
                let start = i;
                while i < chars.len() && chars[i].is_ascii_digit() {
                    i += 1;
                }
                let num_str: String = chars[start..i].iter().collect();
                tokens.push(Token::Integer(num_str.parse().map_err(|_| {
                    EvalError::Parse(format!("invalid number: {num_str}"))
                })?));
            }
            c if is_symbol_start(c) => {
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1;
                }
                let sym: String = chars[start..i].iter().collect();
                tokens.push(Token::Symbol(sym));
            }
            c => return Err(EvalError::Parse(format!("unexpected character: {c}"))),
        }
    }
    Ok(tokens)
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || "!$%&*/<=>?^_~".contains(c)
}

fn is_symbol_char(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || "+-.:@#".contains(c)
}

// --- Parser ---

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::Integer(n)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::Boolean(b)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Str(s)) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Symbol(s)) }
        Token::LParen => {
            *pos += 1;
            let mut list = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos], Token::RParen) {
                list.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1; // skip )
            Ok(Expr::List(list))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Evaluator ---

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => Err(EvalError::Unbound(s.clone())),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Syntax("empty application".into()));
            }
            let head = match &elems[0] {
                Expr::Symbol(s) => s.as_str(),
                _ => return Err(EvalError::Syntax("expected operator".into())),
            };
            let args = &elems[1..];
            match head {
                "+" => {
                    let mut sum: i64 = 0;
                    for a in args {
                        sum += as_int(&eval(a)?)?;
                    }
                    Ok(Value::Integer(sum))
                }
                "-" => {
                    if args.is_empty() {
                        return Err(EvalError::Arity("- requires at least 1 argument".into()));
                    }
                    if args.len() == 1 {
                        Ok(Value::Integer(-as_int(&eval(&args[0])?)?))
                    } else {
                        let mut result = as_int(&eval(&args[0])?)?;
                        for a in &args[1..] {
                            result -= as_int(&eval(a)?)?;
                        }
                        Ok(Value::Integer(result))
                    }
                }
                "*" => {
                    let mut product: i64 = 1;
                    for a in args {
                        product *= as_int(&eval(a)?)?;
                    }
                    Ok(Value::Integer(product))
                }
                "/" => {
                    if args.is_empty() {
                        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
                    }
                    let mut result = as_int(&eval(&args[0])?)?;
                    for a in &args[1..] {
                        let divisor = as_int(&eval(a)?)?;
                        if divisor == 0 {
                            return Err(EvalError::DivisionByZero);
                        }
                        result /= divisor;
                    }
                    Ok(Value::Integer(result))
                }
                "<" => {
                    let vals = eval_int_args(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
                }
                ">" => {
                    let vals = eval_int_args(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
                }
                "=" => {
                    let vals = eval_int_args(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
                }
                "<=" => {
                    let vals = eval_int_args(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
                }
                ">=" => {
                    let vals = eval_int_args(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
                }
                "not" => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity("not requires 1 argument".into()));
                    }
                    Ok(Value::Boolean(!is_truthy(&eval(&args[0])?)))
                }
                "and" => {
                    if args.is_empty() {
                        return Ok(Value::Boolean(true));
                    }
                    let mut result = Value::Boolean(true);
                    for a in args {
                        result = eval(a)?;
                        if !is_truthy(&result) {
                            return Ok(result);
                        }
                    }
                    Ok(result)
                }
                "or" => {
                    if args.is_empty() {
                        return Ok(Value::Boolean(false));
                    }
                    let mut result = Value::Boolean(false);
                    for a in args {
                        result = eval(a)?;
                        if is_truthy(&result) {
                            return Ok(result);
                        }
                    }
                    Ok(result)
                }
                _ => Err(EvalError::Unbound(head.to_string())),
            }
        }
    }
}

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
    }
}

fn eval_int_args(args: &[Expr]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| eval(a).and_then(|v| as_int(&v))).collect()
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
