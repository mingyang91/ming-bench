pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Void,
}

impl Value {
    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".to_string(),
        }
    }

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
                        't' => { tokens.push(Token::Boolean(true)); i += 2; }
                        'f' => { tokens.push(Token::Boolean(false)); i += 2; }
                        _ => return Err(EvalError::Parse("unexpected #-literal".to_string()))
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                // Number or symbol
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|'"'|';') {
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
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                items.push(parse_tokens(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1; // skip RParen
            Ok(Expr::List(items))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr::Integer(n)) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr::Boolean(b)) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Str(s)) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr::Symbol(s)) }
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

// --- Evaluator ---

type Env = HashMap<String, Value>;

fn default_env() -> Env {
    Env::new()
}

fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => {
            env.get(name)
                .cloned()
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(items) => {
            if items.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &items[0] {
                match op.as_str() {
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    _ => {}
                }
            }
            // Function call
            let func = eval_expr(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|e| eval_expr(e, env)).collect();
            let args = args?;
            apply_builtin(&func, &args)
        }
    }
}

fn eval_and(exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env)?;
        if !val.is_truthy() {
            return Ok(val);
        }
        if i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn eval_or(exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env)?;
        if val.is_truthy() {
            return Ok(val);
        }
        if i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Symbol(s) => s.as_str(),
        _ => return Err(EvalError::Type(format!("not a procedure: {func:?}"))),
    };

    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_int(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_int(&args[0])?));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                result -= expect_int(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_int(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = expect_int(&args[0])?;
            for a in &args[1..] {
                let d = expect_int(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("< requires 2 arguments".into()));
            }
            Ok(Value::Boolean(expect_int(&args[0])? < expect_int(&args[1])?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("> requires 2 arguments".into()));
            }
            Ok(Value::Boolean(expect_int(&args[0])? > expect_int(&args[1])?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(expect_int(&args[0])? == expect_int(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("<= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(expect_int(&args[0])? <= expect_int(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(">= requires 2 arguments".into()));
            }
            Ok(Value::Boolean(expect_int(&args[0])? >= expect_int(&args[1])?))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(name.to_string())),
    }
}

fn expect_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {val:?}"))),
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
    // Pre-populate builtins as symbols
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not"] {
        env.insert(name.to_string(), Value::Symbol(name.to_string()));
    }
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval_expr(expr, &mut env)?;
    }
    Ok(result.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
