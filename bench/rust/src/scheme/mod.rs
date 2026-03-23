pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!("expected integer, got {}", other.display()))),
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

// --- Parser ---

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
            '(' | ')' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            '"' => {
                let mut s = String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        s.push(chars[i]);
                        i += 1;
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
            '#' if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') => {
                let mut tok = String::from('#');
                i += 1;
                tok.push(chars[i]);
                i += 1;
                tokens.push(tok);
            }
            _ => {
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';')
                {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
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
        *pos += 1; // skip ')'
        Ok(Expr::List(list))
    } else if token == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if token == "#t" {
        *pos += 1;
        Ok(Expr::Boolean(true))
    } else if token == "#f" {
        *pos += 1;
        Ok(Expr::Boolean(false))
    } else if token.starts_with('"') {
        *pos += 1;
        let inner = &token[1..token.len() - 1];
        Ok(Expr::Str(inner.to_string()))
    } else if let Ok(n) = token.parse::<i64>() {
        *pos += 1;
        Ok(Expr::Integer(n))
    } else {
        *pos += 1;
        Ok(Expr::Symbol(token.clone()))
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

// --- Evaluator ---

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::BadSyntax("empty application".into()));
            }
            match &items[0] {
                Expr::Symbol(op) => eval_builtin(op, &items[1..]),
                _ => Err(EvalError::BadSyntax("not a procedure".into())),
            }
        }
    }
}

fn eval_builtin(op: &str, args: &[Expr]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| eval(a)?.as_integer()).collect();
            Ok(Value::Integer(vals?.iter().sum()))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = eval(&args[0])?.as_integer()?;
            if args.len() == 1 {
                Ok(Value::Integer(-first))
            } else {
                let rest: Result<Vec<i64>, _> =
                    args[1..].iter().map(|a| eval(a)?.as_integer()).collect();
                Ok(Value::Integer(rest?.iter().fold(first, |acc, &x| acc - x)))
            }
        }
        "*" => {
            let vals: Result<Vec<i64>, _> = args.iter().map(|a| eval(a)?.as_integer()).collect();
            Ok(Value::Integer(vals?.iter().product()))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = eval(&args[0])?.as_integer()?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                Ok(Value::Integer(1 / first))
            } else {
                let mut result = first;
                for a in &args[1..] {
                    let v = eval(a)?.as_integer()?;
                    if v == 0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    result /= v;
                }
                Ok(Value::Integer(result))
            }
        }
        "<" => compare_op(args, |a, b| a < b),
        ">" => compare_op(args, |a, b| a > b),
        "=" => compare_op(args, |a, b| a == b),
        "<=" => compare_op(args, |a, b| a <= b),
        ">=" => compare_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires exactly 1 argument".into()));
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

fn compare_op(args: &[Expr], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let vals: Result<Vec<i64>, _> = args.iter().map(|a| eval(a)?.as_integer()).collect();
    let vals = vals?;
    for w in vals.windows(2) {
        if !cmp(w[0], w[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse(input)?;
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr)?;
    }
    Ok(result.display())
}

pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(_input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
