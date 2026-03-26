pub mod error;

pub use error::EvalError;

use std::fmt;

// ── Values ──────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Void,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Void => Ok(()),
        }
    }
}

// ── Tokenizer ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
    Quote,
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => { i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' { i += 1; }
            }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            '\'' => { tokens.push(Token::Quote); i += 1; }
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
                i += 1;
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(Token::Boolean(true));
                                i += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #t".into()));
                            }
                        }
                        'f' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(Token::Boolean(false));
                                i += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #f".into()));
                            }
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected character after #: {}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            c => {
                // Number or symbol
                let start = i;
                while i < chars.len() && !is_delimiter(chars[i]) {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Integer(n));
                } else {
                    tokens.push(Token::Symbol(word));
                }
            }
        }
    }
    Ok(tokens)
}

// ── Parser ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::Integer(n)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::Boolean(b)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Str(s)) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Symbol(s)) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::List(vec![Expr::Symbol("quote".into()), inner]))
        }
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                items.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing parenthesis".into()));
            }
            *pos += 1; // consume RParen
            Ok(Expr::List(items))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(tokens: &[Token]) -> Result<Vec<Expr>, EvalError> {
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => Err(EvalError::UnboundVariable(s.clone())),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &items[0] {
                match op.as_str() {
                    "+" => return eval_add(&items[1..]),
                    "-" => return eval_sub(&items[1..]),
                    "*" => return eval_mul(&items[1..]),
                    "/" => return eval_div(&items[1..]),
                    "<" => return eval_cmp(&items[1..], |a, b| a < b),
                    ">" => return eval_cmp(&items[1..], |a, b| a > b),
                    "=" => return eval_cmp(&items[1..], |a, b| a == b),
                    "<=" => return eval_cmp(&items[1..], |a, b| a <= b),
                    ">=" => return eval_cmp(&items[1..], |a, b| a >= b),
                    "not" => return eval_not(&items[1..]),
                    "and" => return eval_and(&items[1..]),
                    "or" => return eval_or(&items[1..]),
                    _ => {}
                }
            }
            Err(EvalError::Generic(format!("unknown procedure: {}", items[0].symbol_name())))
        }
    }
}

impl Expr {
    fn symbol_name(&self) -> String {
        match self {
            Expr::Symbol(s) => s.clone(),
            other => format!("{:?}", other),
        }
    }
}

fn require_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Type(format!("expected integer, got {}", other))),
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn eval_add(args: &[Expr]) -> Result<Value, EvalError> {
    let mut sum = 0i64;
    for a in args {
        sum += require_int(&eval(a)?)?;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Expr]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0])?)?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for a in &args[1..] {
        result -= require_int(&eval(a)?)?;
    }
    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Expr]) -> Result<Value, EvalError> {
    let mut product = 1i64;
    for a in args {
        product *= require_int(&eval(a)?)?;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Expr]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0])?)?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for a in &args[1..] {
        let d = require_int(&eval(a)?)?;
        if d == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn eval_cmp(args: &[Expr], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = require_int(&eval(&args[0])?)?;
    for a in &args[1..] {
        let curr = require_int(&eval(a)?)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
    }
    let v = eval(&args[0])?;
    Ok(Value::Boolean(!is_truthy(&v)))
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

// ── Public API ──────────────────────────────────────────────────────

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr)?;
    }
    Ok(last.to_string())
}

#[cfg(test)]
mod tests;
