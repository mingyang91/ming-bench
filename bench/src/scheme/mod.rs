pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", inner.join(" "))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '(' => { tokens.push("(".to_string()); i += 1; }
            ')' => { tokens.push(")".to_string()); i += 1; }
            '"' => {
                let mut s = String::from('"');
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
            _ => {
                let mut s = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';') {
                    s.push(chars[i]);
                    i += 1;
                }
                tokens.push(s);
            }
        }
    }
    tokens
}

fn parse(tokens: &[String], pos: usize) -> Result<(Expr, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".to_string()));
    }
    let token = &tokens[pos];
    if token == "(" {
        let mut elems = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i] != ")" {
            let (expr, next) = parse(tokens, i)?;
            elems.push(expr);
            i = next;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".to_string()));
        }
        Ok((Expr::List(elems), i + 1))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected )".to_string()))
    } else {
        Ok((parse_atom(token)?, pos + 1))
    }
}

fn parse_atom(token: &str) -> Result<Expr, EvalError> {
    if token == "#t" {
        return Ok(Expr::Boolean(true));
    }
    if token == "#f" {
        return Ok(Expr::Boolean(false));
    }
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Expr::Integer(n));
    }
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Expr::Str(inner.to_string()));
    }
    Ok(Expr::Symbol(token.to_string()))
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
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

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(s) => Err(EvalError::Parse(format!("unbound variable: {}", s))),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".to_string()));
            }
            if let Expr::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "+" => {
                        let mut sum: i64 = 0;
                        for arg in &elems[1..] {
                            sum += require_int(&eval_expr(arg)?)?;
                        }
                        Ok(Value::Integer(sum))
                    }
                    "-" => {
                        if elems.len() < 2 {
                            return Err(EvalError::Parse("- requires at least one argument".to_string()));
                        }
                        let first = require_int(&eval_expr(&elems[1])?)?;
                        if elems.len() == 2 {
                            Ok(Value::Integer(-first))
                        } else {
                            let mut result = first;
                            for arg in &elems[2..] {
                                result -= require_int(&eval_expr(arg)?)?;
                            }
                            Ok(Value::Integer(result))
                        }
                    }
                    "*" => {
                        let mut product: i64 = 1;
                        for arg in &elems[1..] {
                            product *= require_int(&eval_expr(arg)?)?;
                        }
                        Ok(Value::Integer(product))
                    }
                    "/" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Parse("/ requires at least two arguments".to_string()));
                        }
                        let mut result = require_int(&eval_expr(&elems[1])?)?;
                        for arg in &elems[2..] {
                            let divisor = require_int(&eval_expr(arg)?)?;
                            if divisor == 0 {
                                return Err(EvalError::Parse("division by zero".to_string()));
                            }
                            result /= divisor;
                        }
                        Ok(Value::Integer(result))
                    }
                    "<" => {
                        let (a, b) = require_two_ints(&elems[1..], "<")?;
                        Ok(Value::Boolean(a < b))
                    }
                    ">" => {
                        let (a, b) = require_two_ints(&elems[1..], ">")?;
                        Ok(Value::Boolean(a > b))
                    }
                    "=" => {
                        let (a, b) = require_two_ints(&elems[1..], "=")?;
                        Ok(Value::Boolean(a == b))
                    }
                    "<=" => {
                        let (a, b) = require_two_ints(&elems[1..], "<=")?;
                        Ok(Value::Boolean(a <= b))
                    }
                    ">=" => {
                        let (a, b) = require_two_ints(&elems[1..], ">=")?;
                        Ok(Value::Boolean(a >= b))
                    }
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Parse("not requires exactly one argument".to_string()));
                        }
                        let val = eval_expr(&elems[1])?;
                        Ok(Value::Boolean(is_false(&val)))
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &elems[1..] {
                            result = eval_expr(arg)?;
                            if is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &elems[1..] {
                            result = eval_expr(arg)?;
                            if !is_false(&result) {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    _ => Err(EvalError::Parse(format!("unknown procedure: {}", op))),
                }
            } else {
                Err(EvalError::Parse("not a procedure".to_string()))
            }
        }
    }
}

fn is_false(val: &Value) -> bool {
    matches!(val, Value::Boolean(false))
}

fn require_two_ints(args: &[Expr], op: &str) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Parse(format!("{} requires exactly two arguments", op)));
    }
    let a = require_int(&eval_expr(&args[0])?)?;
    let b = require_int(&eval_expr(&args[1])?)?;
    Ok((a, b))
}

fn require_int(val: &Value) -> Result<i64, EvalError> {
    match val {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Parse("expected integer".to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("42"), Ok("42".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".to_string()));
    }
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval_expr(expr)?;
    }
    Ok(result.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
