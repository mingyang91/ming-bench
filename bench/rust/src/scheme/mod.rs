pub mod error;

pub use error::EvalError;

use std::fmt;

#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    List(Vec<Val>),
    Void,
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' | '\r' => { chars.next(); }
            ';' => {
                // line comment
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    if c2 == '\n' { break; }
                }
            }
            '(' => { tokens.push("(".into()); chars.next(); }
            ')' => { tokens.push(")".into()); chars.next(); }
            '"' => {
                chars.next();
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            match chars.next() {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('"') => s.push('"'),
                                Some('\\') => s.push('\\'),
                                Some(other) => { s.push('\\'); s.push(other); }
                                None => break,
                            }
                        }
                        Some('"') => break,
                        Some(c2) => s.push(c2),
                        None => break,
                    }
                }
                tokens.push(format!("\"{}\"", s));
            }
            _ => {
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == ';' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

fn parse(tokens: &[String]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    if tok == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i] != ")" {
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        Ok((Expr::List(elems), i + 1))
    } else if tok == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else if tok.starts_with('"') {
        let s = tok[1..tok.len()-1].to_string();
        Ok((Expr::Str(s), 1))
    } else if tok == "#t" {
        Ok((Expr::Bool(true), 1))
    } else if tok == "#f" {
        Ok((Expr::Bool(false), 1))
    } else if let Ok(n) = tok.parse::<i64>() {
        Ok((Expr::Int(n), 1))
    } else {
        Ok((Expr::Symbol(tok.clone()), 1))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}

// --- Evaluator ---

fn eval(expr: &Expr) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "and" => return eval_and(&elems[1..]),
                    "or" => return eval_or(&elems[1..]),
                    _ => {}
                }
            }
            // Function call
            let func = &elems[0];
            let op_name = match func {
                Expr::Symbol(s) => s.as_str(),
                _ => return Err(EvalError::Type("not a procedure".into())),
            };
            let args: Vec<Val> = elems[1..].iter().map(eval).collect::<Result<_, _>>()?;
            apply_builtin(op_name, &args)
        }
    }
}

fn eval_and(exprs: &[Expr]) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(true));
    }
    let mut result = Val::Bool(true);
    for expr in exprs {
        result = eval(expr)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr]) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(false));
    }
    for expr in exprs {
        let result = eval(expr)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Val::Bool(false))
}

fn require_ints(args: &[Val], op: &str) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| match a {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{op}: expected number"))),
    }).collect()
}

fn apply_builtin(op: &str, args: &[Val]) -> Result<Val, EvalError> {
    match op {
        "+" => {
            let nums = require_ints(args, "+")?;
            Ok(Val::Int(nums.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("-: need at least 1 argument".into()));
            }
            let nums = require_ints(args, "-")?;
            if nums.len() == 1 {
                Ok(Val::Int(-nums[0]))
            } else {
                Ok(Val::Int(nums[0] - nums[1..].iter().sum::<i64>()))
            }
        }
        "*" => {
            let nums = require_ints(args, "*")?;
            Ok(Val::Int(nums.iter().product()))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/: need at least 2 arguments".into()));
            }
            let nums = require_ints(args, "/")?;
            if nums[1..].contains(&0) {
                return Err(EvalError::Runtime("division by zero".into()));
            }
            let mut result = nums[0];
            for &n in &nums[1..] {
                result /= n;
            }
            Ok(Val::Int(result))
        }
        "<" => {
            let nums = require_ints(args, "<")?;
            Ok(Val::Bool(nums.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            let nums = require_ints(args, ">")?;
            Ok(Val::Bool(nums.windows(2).all(|w| w[0] > w[1])))
        }
        "=" => {
            let nums = require_ints(args, "=")?;
            Ok(Val::Bool(nums.windows(2).all(|w| w[0] == w[1])))
        }
        "<=" => {
            let nums = require_ints(args, "<=")?;
            Ok(Val::Bool(nums.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            let nums = require_ints(args, ">=")?;
            Ok(Val::Bool(nums.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not: expected 1 argument".into()));
            }
            Ok(Val::Bool(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
