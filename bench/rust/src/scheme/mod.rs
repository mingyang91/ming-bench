pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Void,
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "#<void>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Parser ---

fn skip_whitespace(input: &[u8], pos: usize) -> usize {
    let mut i = pos;
    while i < input.len() {
        if input[i].is_ascii_whitespace() {
            i += 1;
        } else if i + 1 < input.len() && input[i] == b';' {
            // line comment
            while i < input.len() && input[i] != b'\n' {
                i += 1;
            }
        } else {
            break;
        }
    }
    i
}

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

fn parse_expr(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let i = skip_whitespace(input, pos);
    if i >= input.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }

    match input[i] {
        b'(' => parse_list(input, i),
        b'"' => parse_string(input, i),
        b'#' => {
            if i + 1 < input.len() {
                match input[i + 1] {
                    b't' => Ok((Expr::Boolean(true), i + 2)),
                    b'f' => Ok((Expr::Boolean(false), i + 2)),
                    _ => Err(EvalError::Parse(format!("unexpected character after #"))),
                }
            } else {
                Err(EvalError::Parse("unexpected end after #".into()))
            }
        }
        _ => parse_atom(input, i),
    }
}

fn parse_string(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let mut i = pos + 1; // skip opening quote
    let mut s = String::new();
    while i < input.len() && input[i] != b'"' {
        if input[i] == b'\\' && i + 1 < input.len() {
            i += 1;
            match input[i] {
                b'n' => s.push('\n'),
                b't' => s.push('\t'),
                b'\\' => s.push('\\'),
                b'"' => s.push('"'),
                c => {
                    s.push('\\');
                    s.push(c as char);
                }
            }
        } else {
            s.push(input[i] as char);
        }
        i += 1;
    }
    if i >= input.len() {
        return Err(EvalError::Parse("unterminated string".into()));
    }
    Ok((Expr::Str(s), i + 1)) // skip closing quote
}

fn parse_list(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let mut i = pos + 1; // skip '('
    let mut items = Vec::new();
    loop {
        i = skip_whitespace(input, i);
        if i >= input.len() {
            return Err(EvalError::Parse("unterminated list".into()));
        }
        if input[i] == b')' {
            return Ok((Expr::List(items), i + 1));
        }
        let (expr, next) = parse_expr(input, i)?;
        items.push(expr);
        i = next;
    }
}

fn is_symbol_char(b: u8) -> bool {
    !b.is_ascii_whitespace() && b != b'(' && b != b')' && b != b'"' && b != b';'
}

fn parse_atom(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let mut i = pos;
    while i < input.len() && is_symbol_char(input[i]) {
        i += 1;
    }
    if i == pos {
        return Err(EvalError::Parse(format!(
            "unexpected character: {}",
            input[pos] as char
        )));
    }
    let token = std::str::from_utf8(&input[pos..i]).unwrap();
    // Try integer
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Expr::Integer(n), i));
    }
    Ok((Expr::Symbol(token.to_string()), i))
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let bytes = input.as_bytes();
    let mut pos = 0;
    let mut exprs = Vec::new();
    loop {
        pos = skip_whitespace(bytes, pos);
        if pos >= bytes.len() {
            break;
        }
        let (expr, next) = parse_expr(bytes, pos)?;
        exprs.push(expr);
        pos = next;
    }
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
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
                return Ok(Value::List(vec![]));
            }
            // The first element should be a symbol (operator)
            if let Expr::Symbol(op) = &items[0] {
                eval_builtin(op, &items[1..])
            } else {
                Err(EvalError::Type("not a procedure".into()))
            }
        }
    }
}

fn eval_builtin(op: &str, args: &[Expr]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(eval(a)?)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = as_integer(eval(&args[0])?)?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= as_integer(eval(a)?)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(eval(a)?)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = as_integer(eval(&args[0])?)?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = as_integer(eval(a)?)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
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
            let val = eval(&args[0])?;
            Ok(Value::Boolean(!val.is_truthy()))
        }
        "and" => {
            // Returns last truthy value or first falsy
            if args.is_empty() {
                return Ok(Value::Boolean(true));
            }
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
            // Returns first truthy value or last falsy
            if args.is_empty() {
                return Ok(Value::Boolean(false));
            }
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

fn as_integer(v: Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn cmp_op(args: &[Expr], f: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = as_integer(eval(&args[0])?)?;
    for a in &args[1..] {
        let cur = as_integer(eval(a)?)?;
        if !f(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let mut result = Value::Void;
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
