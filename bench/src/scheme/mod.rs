pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
}

type Env = HashMap<String, Value>;

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
        }
    }
}

/// Tokenize input into a list of tokens.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            _ => {
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                }
                tokens.push(s);
            }
        }
    }
    tokens
}

/// Parse a single expression from the token stream.
fn parse(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let token = &tokens[pos];

    // Boolean literals
    if token == "#t" || token == "#true" {
        return Ok((Value::Boolean(true), pos + 1));
    }
    if token == "#f" || token == "#false" {
        return Ok((Value::Boolean(false), pos + 1));
    }

    // String literal
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        // Process escape sequences
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
                    other => {
                        s.push('\\');
                        s.push(other);
                    }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        return Ok((Value::Str(s), pos + 1));
    }

    // Integer literal
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Value::Integer(n), pos + 1));
    }

    // List
    if token == "(" {
        let mut elems = Vec::new();
        let mut p = pos + 1;
        loop {
            if p >= tokens.len() {
                return Err(EvalError::Parse("unclosed parenthesis".to_string()));
            }
            if tokens[p] == ")" {
                return Ok((Value::List(elems), p + 1));
            }
            let (val, next) = parse(tokens, p)?;
            elems.push(val);
            p = next;
        }
    }

    // Quote shorthand
    if token == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        return Ok((Value::List(vec![Value::Symbol("quote".to_string()), inner]), next));
    }

    // Symbol
    Ok((Value::Symbol(token.clone()), pos + 1))
}

/// Parse all expressions from input.
fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let mut env = Env::new();
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

fn eval(expr: Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr),
        Value::Symbol(s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| EvalError::Runtime(format!("unbound symbol: {}", s))),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Runtime("empty application".to_string()));
            }
            let first = &elems[0];
            match first {
                Value::Symbol(op) => match op.as_str() {
                    "define" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Runtime(
                                "define requires 2 arguments".to_string(),
                            ));
                        }
                        let name = match &elems[1] {
                            Value::Symbol(s) => s.clone(),
                            _ => {
                                return Err(EvalError::Runtime(
                                    "define: first argument must be a symbol".to_string(),
                                ))
                            }
                        };
                        let val = eval(elems[2].clone(), env)?;
                        env.insert(name, val.clone());
                        Ok(val)
                    }
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Runtime(
                                "if requires 2 or 3 arguments".to_string(),
                            ));
                        }
                        let cond = eval(elems[1].clone(), env)?;
                        if cond != Value::Boolean(false) {
                            eval(elems[2].clone(), env)
                        } else if elems.len() == 4 {
                            eval(elems[3].clone(), env)
                        } else {
                            Ok(Value::Boolean(false))
                        }
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Runtime(
                                "quote requires 1 argument".to_string(),
                            ));
                        }
                        Ok(elems[1].clone())
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval(arg.clone(), env)?;
                            if result == Value::Boolean(false) {
                                return Ok(Value::Boolean(false));
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        for arg in &elems[1..] {
                            let result = eval(arg.clone(), env)?;
                            if result != Value::Boolean(false) {
                                return Ok(result);
                            }
                        }
                        Ok(Value::Boolean(false))
                    }
                    _ => apply_builtin(op, &elems[1..], env),
                },
                _ => Err(EvalError::Runtime("not a procedure".to_string())),
            }
        }
    }
}

fn apply_builtin(op: &str, args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    // Evaluate all arguments first
    let mut vals = Vec::with_capacity(args.len());
    for arg in args {
        vals.push(eval(arg.clone(), env)?);
    }

    match op {
        "+" => {
            let mut sum: i64 = 0;
            for v in &vals {
                sum += expect_int(v)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if vals.is_empty() {
                return Err(EvalError::Runtime("- requires at least 1 argument".to_string()));
            }
            if vals.len() == 1 {
                return Ok(Value::Integer(-expect_int(&vals[0])?));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                result -= expect_int(v)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for v in &vals {
                product *= expect_int(v)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if vals.is_empty() {
                return Err(EvalError::Runtime("/ requires at least 1 argument".to_string()));
            }
            let mut result = expect_int(&vals[0])?;
            for v in &vals[1..] {
                let d = expect_int(v)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".to_string()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("< requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? < expect_int(&vals[1])?))
        }
        ">" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("> requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? > expect_int(&vals[1])?))
        }
        "=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? == expect_int(&vals[1])?))
        }
        "<=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime("<= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? <= expect_int(&vals[1])?))
        }
        ">=" => {
            if vals.len() != 2 {
                return Err(EvalError::Runtime(">= requires 2 arguments".to_string()));
            }
            Ok(Value::Boolean(expect_int(&vals[0])? >= expect_int(&vals[1])?))
        }
        "not" => {
            if vals.len() != 1 {
                return Err(EvalError::Runtime("not requires 1 argument".to_string()));
            }
            Ok(Value::Boolean(vals[0] == Value::Boolean(false)))
        }
        _ => Err(EvalError::Runtime(format!("unknown procedure: {}", op))),
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Runtime("expected integer".to_string())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
