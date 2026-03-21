pub mod error;

pub use error::EvalError;

use std::fmt;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
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
            Val::Int(n) => write!(f, "{}", n),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Symbol(s) => write!(f, "{}", s),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Val::Void => write!(f, ""),
        }
    }
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Int(i64),
    Bool(bool),
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
                            '"' => s.push('"'),
                            '\\' => s.push('\\'),
                            c => s.push(c),
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
                            tokens.push(Token::Bool(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Bool(false));
                            i += 2;
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            c => {
                // number or symbol
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"')
                {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Int(n));
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
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Int(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::Int(n))
        }
        Token::Bool(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::Bool(b))
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
                return Err(EvalError::Parse("unmatched (".into()));
            }
            *pos += 1; // skip )
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

fn eval(expr: &Expr) -> Result<Val, EvalError> {
    match expr {
        Expr::Int(n) => Ok(Val::Int(*n)),
        Expr::Bool(b) => Ok(Val::Bool(*b)),
        Expr::Str(s) => Ok(Val::Str(s.clone())),
        Expr::Symbol(name) => {
            if is_builtin(name) {
                Ok(Val::Symbol(name.clone()))
            } else {
                Err(EvalError::UnboundVariable {
                    name: name.clone(),
                })
            }
        }
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
            let func = eval(&elems[0])?;
            let args: Vec<Val> = elems[1..]
                .iter()
                .map(|e| eval(e))
                .collect::<Result<Vec<_>, _>>()?;

            match func {
                Val::Symbol(ref name) => apply_builtin(name, &args),
                _ => Err(EvalError::Type(format!("not a procedure: {}", func))),
            }
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

fn apply_builtin(name: &str, args: &[Val]) -> Result<Val, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(a)?;
            }
            Ok(Val::Int(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Val::Int(-as_int(&args[0])?));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                result -= as_int(a)?;
            }
            Ok(Val::Int(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_int(a)?;
            }
            Ok(Val::Int(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = as_int(&args[0])?;
            for a in &args[1..] {
                let d = as_int(a)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                result /= d;
            }
            Ok(Val::Int(result))
        }
        "<" => {
            let (a, b) = two_ints(args, "<")?;
            Ok(Val::Bool(a < b))
        }
        ">" => {
            let (a, b) = two_ints(args, ">")?;
            Ok(Val::Bool(a > b))
        }
        "=" => {
            let (a, b) = two_ints(args, "=")?;
            Ok(Val::Bool(a == b))
        }
        "<=" => {
            let (a, b) = two_ints(args, "<=")?;
            Ok(Val::Bool(a <= b))
        }
        ">=" => {
            let (a, b) = two_ints(args, ">=")?;
            Ok(Val::Bool(a >= b))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Val::Bool(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
    )
}

fn as_int(v: &Val) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected number, got {}", v))),
    }
}

fn two_ints(args: &[Val], op: &str) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{} requires 2 arguments", op)));
    }
    Ok((as_int(&args[0])?, as_int(&args[1])?))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let mut result = Val::Void;
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
