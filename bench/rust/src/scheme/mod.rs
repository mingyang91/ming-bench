pub mod error;

pub use error::EvalError;

// ── Value ──

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(b) => if *b { "#t".into() } else { "#f".into() },
            Value::String(s) => format!("\"{}\"", s),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// ── Parser ──

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
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
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push("(".into()); i += 1; }
            ')' => { tokens.push(")".into()); i += 1; }
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
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';') {
                    i += 1;
                }
                tokens.push(chars[start..i].iter().collect());
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[String], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[*pos];
    if token == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos] != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1; // skip )
        Ok(Expr::List(list))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected )".into()))
    } else {
        *pos += 1;
        Ok(parse_atom(token))
    }
}

fn parse_atom(token: &str) -> Expr {
    if token == "#t" {
        Expr::Boolean(true)
    } else if token == "#f" {
        Expr::Boolean(false)
    } else if token.starts_with('"') && token.ends_with('"') {
        let inner = &token[1..token.len() - 1];
        Expr::String(inner.into())
    } else if let Ok(n) = token.parse::<i64>() {
        Expr::Integer(n)
    } else {
        Expr::Symbol(token.into())
    }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ──

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Type("empty application".into()));
            }
            if let Expr::Symbol(op) = &items[0] {
                match op.as_str() {
                    "+" => {
                        let mut sum: i64 = 0;
                        for arg in &items[1..] {
                            sum += as_int(&eval(arg)?)?;
                        }
                        Ok(Value::Integer(sum))
                    }
                    "-" => {
                        if items.len() < 2 {
                            return Err(EvalError::Arity("- requires at least 1 argument".into()));
                        }
                        if items.len() == 2 {
                            Ok(Value::Integer(-as_int(&eval(&items[1])?)?))
                        } else {
                            let mut result = as_int(&eval(&items[1])?)?;
                            for arg in &items[2..] {
                                result -= as_int(&eval(arg)?)?;
                            }
                            Ok(Value::Integer(result))
                        }
                    }
                    "*" => {
                        let mut product: i64 = 1;
                        for arg in &items[1..] {
                            product *= as_int(&eval(arg)?)?;
                        }
                        Ok(Value::Integer(product))
                    }
                    "/" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
                        }
                        let mut result = as_int(&eval(&items[1])?)?;
                        for arg in &items[2..] {
                            let d = as_int(&eval(arg)?)?;
                            if d == 0 {
                                return Err(EvalError::Type("division by zero".into()));
                            }
                            result /= d;
                        }
                        Ok(Value::Integer(result))
                    }
                    "<" => cmp_op(&items[1..], |a, b| a < b),
                    ">" => cmp_op(&items[1..], |a, b| a > b),
                    "=" => cmp_op(&items[1..], |a, b| a == b),
                    "<=" => cmp_op(&items[1..], |a, b| a <= b),
                    ">=" => cmp_op(&items[1..], |a, b| a >= b),
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into()));
                        }
                        let v = eval(&items[1])?;
                        Ok(Value::Boolean(!v.is_truthy()))
                    }
                    "and" => {
                        let mut result = Value::Boolean(true);
                        for arg in &items[1..] {
                            result = eval(arg)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    "or" => {
                        let mut result = Value::Boolean(false);
                        for arg in &items[1..] {
                            result = eval(arg)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        Ok(result)
                    }
                    _ => Err(EvalError::UnboundVariable(op.clone())),
                }
            } else {
                Err(EvalError::Type("not a procedure".into()))
            }
        }
    }
}

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn cmp_op(args: &[Expr], f: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_int(&eval(&args[0])?)?;
    for arg in &args[1..] {
        let curr = as_int(&eval(arg)?)?;
        if !f(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
