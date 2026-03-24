pub mod error;

pub use error::EvalError;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Nil,
    Builtin(String, fn(&[Value]) -> Result<Value, EvalError>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Nil => write!(f, "()"),
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
            Value::Builtin(name, _) => write!(f, "#<procedure {name}>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::List(a), Value::List(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
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
            _c => {
                // Number or symbol
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

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
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
                elems.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1; // consume RParen
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
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Environment ---

type Env = HashMap<String, Value>;

fn default_env() -> Env {
    let mut env = Env::new();

    fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
        let mut sum = 0i64;
        for a in args {
            match a {
                Value::Integer(n) => sum += n,
                _ => return Err(EvalError::Type("+ expects numbers".into())),
            }
        }
        Ok(Value::Integer(sum))
    }

    fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
        if args.is_empty() {
            return Err(EvalError::Arity("- requires at least 1 argument".into()));
        }
        match &args[0] {
            Value::Integer(first) => {
                if args.len() == 1 {
                    return Ok(Value::Integer(-first));
                }
                let mut result = *first;
                for a in &args[1..] {
                    match a {
                        Value::Integer(n) => result -= n,
                        _ => return Err(EvalError::Type("- expects numbers".into())),
                    }
                }
                Ok(Value::Integer(result))
            }
            _ => Err(EvalError::Type("- expects numbers".into())),
        }
    }

    fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
        let mut product = 1i64;
        for a in args {
            match a {
                Value::Integer(n) => product *= n,
                _ => return Err(EvalError::Type("* expects numbers".into())),
            }
        }
        Ok(Value::Integer(product))
    }

    fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
        }
        match &args[0] {
            Value::Integer(first) => {
                let mut result = *first;
                for a in &args[1..] {
                    match a {
                        Value::Integer(0) => return Err(EvalError::DivisionByZero),
                        Value::Integer(n) => result /= n,
                        _ => return Err(EvalError::Type("/ expects numbers".into())),
                    }
                }
                Ok(Value::Integer(result))
            }
            _ => Err(EvalError::Type("/ expects numbers".into())),
        }
    }

    fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity("< requires 2 arguments".into()));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a < b)),
            _ => Err(EvalError::Type("< expects numbers".into())),
        }
    }

    fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity("> requires 2 arguments".into()));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a > b)),
            _ => Err(EvalError::Type("> expects numbers".into())),
        }
    }

    fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity("= requires 2 arguments".into()));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a == b)),
            _ => Err(EvalError::Type("= expects numbers".into())),
        }
    }

    fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != 2 {
            return Err(EvalError::Arity("<= requires 2 arguments".into()));
        }
        match (&args[0], &args[1]) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Boolean(a <= b)),
            _ => Err(EvalError::Type("<= expects numbers".into())),
        }
    }

    fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != 1 {
            return Err(EvalError::Arity("not requires 1 argument".into()));
        }
        Ok(Value::Boolean(!args[0].is_truthy()))
    }

    env.insert("+".into(), Value::Builtin("+".into(), builtin_add));
    env.insert("-".into(), Value::Builtin("-".into(), builtin_sub));
    env.insert("*".into(), Value::Builtin("*".into(), builtin_mul));
    env.insert("/".into(), Value::Builtin("/".into(), builtin_div));
    env.insert("<".into(), Value::Builtin("<".into(), builtin_lt));
    env.insert(">".into(), Value::Builtin(">".into(), builtin_gt));
    env.insert("=".into(), Value::Builtin("=".into(), builtin_eq));
    env.insert("<=".into(), Value::Builtin("<=".into(), builtin_le));
    env.insert("not".into(), Value::Builtin("not".into(), builtin_not));

    env
}

// --- Evaluator ---

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| EvalError::UnboundVariable(name.clone())),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Value::Nil);
            }

            // Special forms: and, or
            if let Expr::Symbol(name) = &elems[0] {
                match name.as_str() {
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval(arg, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &elems[1..] {
                            result = eval(arg, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = eval(&elems[0], env)?;
            let mut args = Vec::new();
            for arg in &elems[1..] {
                args.push(eval(arg, env)?);
            }
            match func {
                Value::Builtin(_, f) => f(&args),
                _ => Err(EvalError::Type(format!(
                    "not a procedure: {func}"
                ))),
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut env = default_env();
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval(expr, &mut env)?;
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
