mod builtins;
pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
        }
    }
}

/// An S-expression AST node.
#[derive(Debug, Clone)]
enum Expr {
    Atom(String),
    List(Vec<Expr>),
}

/// Read a string literal (opening `"` already consumed) from `chars`.
fn read_string_literal(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut s = String::from('"');
    loop {
        match chars.next() {
            Some('\\') => {
                s.push('\\');
                s.extend(chars.next());
            }
            Some('"') => {
                s.push('"');
                break;
            }
            Some(c) => s.push(c),
            None => break,
        }
    }
    s
}

/// Read an atom token (non-delimiter chars) from `chars`.
fn read_atom(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut tok = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '(' || c == ')' || c == '"' {
            break;
        }
        tok.push(c);
        chars.next();
    }
    tok
}

/// Tokenize input into a flat list of tokens (atoms, parens, strings).
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&ch) = chars.peek() {
        match ch {
            _ if ch.is_whitespace() => {
                chars.next();
            }
            '(' | ')' => {
                tokens.push(ch.to_string());
                chars.next();
            }
            '"' => {
                chars.next();
                tokens.push(read_string_literal(&mut chars));
            }
            _ => tokens.push(read_atom(&mut chars)),
        }
    }
    tokens
}

/// Parse a list body (after the opening `(`) until the matching `)`.
fn parse_list(tokens: &[String]) -> Result<(Vec<Expr>, &[String]), EvalError> {
    let mut items = Vec::new();
    let mut remaining = tokens;
    loop {
        if remaining.first().map(|s| s.as_str()) == Some(")") {
            return Ok((items, &remaining[1..]));
        }
        let (expr, rest) = parse(remaining)?;
        items.push(expr);
        remaining = rest;
    }
}

/// Parse tokens into S-expression ASTs.
fn parse(tokens: &[String]) -> Result<(Expr, &[String]), EvalError> {
    let [first, rest @ ..] = tokens else {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
        });
    };
    if first == "(" {
        let (items, remaining) = parse_list(rest)?;
        Ok((Expr::List(items), remaining))
    } else if first == ")" {
        Err(EvalError::Parse {
            message: "unexpected ')'".to_string(),
        })
    } else {
        Ok((Expr::Atom(first.clone()), rest))
    }
}

/// Parse all top-level expressions from token stream.
fn parse_all(tokens: &[String]) -> Result<Vec<Expr>, EvalError> {
    let mut exprs = Vec::new();
    let mut remaining = tokens;
    while !remaining.is_empty() {
        let (expr, rest) = parse(remaining)?;
        exprs.push(expr);
        remaining = rest;
    }
    Ok(exprs)
}

/// Parse an atom token into a Value.
fn atom_to_value(token: &str) -> Result<Value, EvalError> {
    if let Ok(n) = token.parse::<i64>() {
        return Ok(Value::Integer(n));
    }
    if token == "#t" {
        return Ok(Value::Boolean(true));
    }
    if token == "#f" {
        return Ok(Value::Boolean(false));
    }
    if token.starts_with('"') && token.ends_with('"') && token.len() >= 2 {
        let inner = &token[1..token.len() - 1];
        return Ok(Value::String(inner.to_string()));
    }
    Ok(Value::Symbol(token.to_string()))
}

/// Evaluate an expression.
fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Atom(token) => atom_to_value(token),
        Expr::List(items) => eval_list(items),
    }
}

/// Evaluate a list expression (function application or special form).
fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let [operator, args @ ..] = items else {
        return Err(EvalError::Parse {
            message: "empty list".to_string(),
        });
    };
    let Expr::Atom(op) = operator else {
        return Err(EvalError::Parse {
            message: "expected operator".to_string(),
        });
    };
    match op.as_str() {
        "and" => eval_and(args),
        "or" => eval_or(args),
        _ => {
            let evaluated: Vec<Value> = args.iter().map(eval).collect::<Result<_, _>>()?;
            apply_builtin(op, &evaluated)
        }
    }
}

/// Apply a built-in operator.
fn apply_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => builtins::apply_add(args),
        "-" => builtins::apply_sub(args),
        "*" => builtins::apply_mul(args),
        "/" => builtins::apply_div(args),
        "<" => builtins::apply_compare(args, |a, b| a < b),
        ">" => builtins::apply_compare(args, |a, b| a > b),
        "=" => builtins::apply_compare(args, |a, b| a == b),
        "<=" => builtins::apply_compare(args, |a, b| a <= b),
        ">=" => builtins::apply_compare(args, |a, b| a >= b),
        "not" => builtins::apply_not(args),
        _ => Err(EvalError::UnboundVariable {
            name: op.to_string(),
        }),
    }
}

/// Short-circuit `and`: returns last truthy value, or first falsy value.
fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg)?;
        if result == Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Short-circuit `or`: returns first truthy value, or last falsy value.
fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg)?;
        if result != Value::Boolean(false) {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    if tokens.is_empty() {
        return Err(EvalError::Parse {
            message: "empty input".to_string(),
        });
    }
    let exprs = parse_all(&tokens)?;
    let mut last = None;
    for expr in &exprs {
        last = Some(eval(expr)?);
    }
    Ok(last.expect("exprs is non-empty").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
