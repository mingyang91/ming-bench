pub mod error;

pub use error::EvalError;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    Void,
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Tokenizer ──

fn tokenize(input: &str) -> Vec<String> {
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
            '(' => { tokens.push("(".into()); i += 1; }
            ')' => { tokens.push(")".into()); i += 1; }
            '#' => {
                if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') {
                    let tok: String = chars[i..i+2].iter().collect();
                    tokens.push(tok);
                    i += 2;
                } else {
                    let mut tok = String::new();
                    while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                        tok.push(chars[i]);
                        i += 1;
                    }
                    tokens.push(tok);
                }
            }
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
            _ => {
                let mut tok = String::new();
                while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

// ── Parser ──

fn parse(tokens: &[String], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    if tok == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos] != ")" {
            list.push(parse(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1; // skip ')'
        Ok(Expr::List(list))
    } else if tok == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if tok == "#t" {
        *pos += 1;
        Ok(Expr::Bool(true))
    } else if tok == "#f" {
        *pos += 1;
        Ok(Expr::Bool(false))
    } else if tok.starts_with('"') {
        *pos += 1;
        // Strip outer quotes and process escapes
        let inner = &tok[1..tok.len()-1];
        let mut s = String::new();
        let cs: Vec<char> = inner.chars().collect();
        let mut j = 0;
        while j < cs.len() {
            if cs[j] == '\\' && j + 1 < cs.len() {
                match cs[j + 1] {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    c => { s.push('\\'); s.push(c); }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        Ok(Expr::Str(s))
    } else if let Ok(n) = tok.parse::<i64>() {
        *pos += 1;
        Ok(Expr::Int(n))
    } else {
        *pos += 1;
        Ok(Expr::Symbol(tok.clone()))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ──

type Env = HashMap<String, Val>;

fn default_env() -> Env {
    HashMap::new()
}

fn is_truthy(v: &Val) -> bool {
    !matches!(v, Val::Bool(false))
}

fn as_int(v: &Val, ctx: &str) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{ctx}: expected integer, got {v}"))),
    }
}

fn eval(expr: &Expr, env: &mut Env) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(name) => {
            env.get(name)
                .cloned()
                .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
        }
        Expr::List(list) => {
            if list.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &list[0] {
                match op.as_str() {
                    "and" => {
                        let mut result = Val::Bool(true);
                        for arg in &list[1..] {
                            result = eval(arg, env)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Val::Bool(false);
                        for arg in &list[1..] {
                            result = eval(arg, env)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    _ => {}
                }
            }

            // Function application
            let func = &list[0];
            let args: Result<Vec<Val>, _> = list[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;

            if let Expr::Symbol(op) = func {
                match op.as_str() {
                    "+" => {
                        let mut sum: i64 = 0;
                        for a in &args {
                            sum += as_int(a, "+")?;
                        }
                        Ok(Val::Int(sum))
                    }
                    "-" => {
                        if args.is_empty() {
                            return Err(EvalError::Arity("-: need at least 1 argument".into()));
                        }
                        if args.len() == 1 {
                            Ok(Val::Int(-as_int(&args[0], "-")?))
                        } else {
                            let mut result = as_int(&args[0], "-")?;
                            for a in &args[1..] {
                                result -= as_int(a, "-")?;
                            }
                            Ok(Val::Int(result))
                        }
                    }
                    "*" => {
                        let mut prod: i64 = 1;
                        for a in &args {
                            prod *= as_int(a, "*")?;
                        }
                        Ok(Val::Int(prod))
                    }
                    "/" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity("/: need at least 2 arguments".into()));
                        }
                        let mut result = as_int(&args[0], "/")?;
                        for a in &args[1..] {
                            let d = as_int(a, "/")?;
                            if d == 0 {
                                return Err(EvalError::Type("division by zero".into()));
                            }
                            result /= d;
                        }
                        Ok(Val::Int(result))
                    }
                    "=" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity("=: need at least 2 arguments".into()));
                        }
                        let first = as_int(&args[0], "=")?;
                        Ok(Val::Bool(args[1..].iter().all(|a| as_int(a, "=").map_or(false, |n| n == first))))
                    }
                    "<" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity("<: need at least 2 arguments".into()));
                        }
                        let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, "<")).collect();
                        let vals = vals?;
                        Ok(Val::Bool(vals.windows(2).all(|w| w[0] < w[1])))
                    }
                    ">" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity(">: need at least 2 arguments".into()));
                        }
                        let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, ">")).collect();
                        let vals = vals?;
                        Ok(Val::Bool(vals.windows(2).all(|w| w[0] > w[1])))
                    }
                    "<=" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity("<=: need at least 2 arguments".into()));
                        }
                        let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, "<=")).collect();
                        let vals = vals?;
                        Ok(Val::Bool(vals.windows(2).all(|w| w[0] <= w[1])))
                    }
                    ">=" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity(">=: need at least 2 arguments".into()));
                        }
                        let vals: Result<Vec<i64>, _> = args.iter().map(|a| as_int(a, ">=")).collect();
                        let vals = vals?;
                        Ok(Val::Bool(vals.windows(2).all(|w| w[0] >= w[1])))
                    }
                    "not" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity("not: need exactly 1 argument".into()));
                        }
                        Ok(Val::Bool(!is_truthy(&args[0])))
                    }
                    _ => Err(EvalError::UnboundVariable(op.clone())),
                }
            } else {
                Err(EvalError::Type("not a procedure".into()))
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let mut env = default_env();
    let mut result = Val::Void;
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
