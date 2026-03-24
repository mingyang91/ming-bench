pub mod error;

pub use error::EvalError;

use std::fmt;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
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
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Void => write!(f, ""),
        }
    }
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
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
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
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
                i += 1; // closing quote
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
                        _ => {
                            return Err(EvalError::Parse(format!(
                                "unexpected character after #: {}",
                                chars[i + 1]
                            )));
                        }
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
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

// ---------- Parser ----------

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
        Token::Integer(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Integer(n))
        }
        Token::Boolean(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Boolean(b))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Str(s))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::Symbol(s))
        }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1; // RParen
            Ok(Expr::List(elems))
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

// ---------- Evaluator ----------

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

const BUILTINS: &[&str] = &["+", "-", "*", "/", "<", ">", "=", "<=", ">="];

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            if BUILTINS.contains(&name.as_str()) {
                Ok(Value::Symbol(name.clone()))
            } else {
                Err(EvalError::UnboundVariable(name.clone()))
            }
        }
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "and" => return eval_and(&elems[1..]),
                    "or" => return eval_or(&elems[1..]),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("not expects 1 argument".into()));
                        }
                        let val = eval(&elems[1])?;
                        return Ok(Value::Boolean(!is_truthy(&val)));
                    }
                    _ => {}
                }
            }
            // Function call
            let func = eval(&elems[0])?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(eval)
                .collect::<Result<_, _>>()?;
            apply_builtin(&func, &args)
        }
    }
}

fn eval_and(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for e in exprs {
        result = eval(e)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for e in exprs {
        let result = eval(e)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn as_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
    }
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Symbol(s) => s.as_str(),
        _ => return Err(EvalError::Type(format!("not a procedure: {func}"))),
    };

    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-as_integer(&args[0])?));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                result -= as_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                let divisor = as_integer(a)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("< requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? < as_integer(&args[1])?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("> requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? > as_integer(&args[1])?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? == as_integer(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("<= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? <= as_integer(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(">= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? >= as_integer(&args[1])?))
        }
        _ => Err(EvalError::UnboundVariable(name.to_string())),
    }
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
