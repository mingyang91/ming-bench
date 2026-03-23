pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Void,
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Void => "".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ---------- Parser ----------

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
                        let next = chars[i + 1];
                        match next {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            _ => { s.push('\\'); s.push(next); }
                        }
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                s.push('"');
                if i < chars.len() { i += 1; } // skip closing "
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
            return Err(EvalError::Parse("missing closing parenthesis".into()));
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
        Expr::Str(token[1..token.len()-1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        Expr::Integer(n)
    } else {
        Expr::Symbol(token.to_string())
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

// ---------- Evaluator ----------

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            match &elems[0] {
                Expr::Symbol(op) => eval_builtin(op, &elems[1..]),
                _ => Err(EvalError::Type("not a procedure".into())),
            }
        }
    }
}

fn eval_builtin(op: &str, args: &[Expr]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_int(&eval(a)?)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                Ok(Value::Integer(-as_int(&eval(&args[0])?)?))
            } else {
                let mut result = as_int(&eval(&args[0])?)?;
                for a in &args[1..] {
                    result -= as_int(&eval(a)?)?;
                }
                Ok(Value::Integer(result))
            }
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= as_int(&eval(a)?)?;
            }
            Ok(Value::Integer(prod))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut result = as_int(&eval(&args[0])?)?;
            for a in &args[1..] {
                let d = as_int(&eval(a)?)?;
                if d == 0 {
                    return Err(EvalError::Runtime("division by zero".into()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => cmp_op(args, |a, b| a < b),
        ">" => cmp_op(args, |a, b| a > b),
        "=" => cmp_op(args, |a, b| a == b),
        "<=" => cmp_op(args, |a, b| a <= b),
        ">=" => cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            let v = eval(&args[0])?;
            Ok(Value::Boolean(!v.is_truthy()))
        }
        "and" => {
            let mut result = Value::Boolean(true);
            for a in args {
                result = eval(a)?;
                if !result.is_truthy() {
                    return Ok(result);
                }
            }
            Ok(result)
        }
        "or" => {
            let mut result = Value::Boolean(false);
            for a in args {
                result = eval(a)?;
                if result.is_truthy() {
                    return Ok(result);
                }
            }
            Ok(result)
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn as_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn cmp_op(args: &[Expr], f: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_int(&eval(&args[0])?)?;
    for a in &args[1..] {
        let curr = as_int(&eval(a)?)?;
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
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr)?;
    }
    Ok(last.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
