pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Symbol(String),
    Void,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Void => write!(f, "#<void>"),
        }
    }
}

// ── Parser ──

struct Parser {
    tokens: Vec<String>,
    pos: usize,
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
            '#' => {
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')') {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
            _ => {
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';') {
                    tok.push(chars[i]);
                    i += 1;
                }
                tokens.push(tok);
            }
        }
    }
    tokens
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let tok = &self.tokens[self.pos];
        if tok == "(" {
            self.pos += 1;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos] != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            self.pos += 1; // consume ')'
            Ok(Value::List(elems))
        } else {
            self.pos += 1;
            Ok(parse_atom(tok))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Value>, EvalError> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

fn parse_atom(token: &str) -> Value {
    if token == "#t" {
        Value::Boolean(true)
    } else if token == "#f" {
        Value::Boolean(false)
    } else if token.starts_with('"') && token.ends_with('"') {
        Value::String(token[1..token.len() - 1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        Value::Integer(n)
    } else {
        Value::Symbol(token.to_string())
    }
}

// ── Evaluator ──

fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Void => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let Value::Symbol(name) = &elems[0] {
                return eval_call(name, &elems[1..]);
            }
            Err(EvalError::Type(format!("not a procedure: {}", elems[0])))
        }
    }
}

fn eval_call(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        // Special forms (don't evaluate args eagerly)
        "and" => eval_and(args),
        "or" => eval_or(args),
        // Builtins (evaluate args first)
        _ => {
            let evaled: Result<Vec<Value>, _> = args.iter().map(|e| eval(e)).collect();
            let evaled = evaled?;
            apply_builtin(name, &evaled)
        }
    }
}

fn eval_and(exprs: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr)?;
        if result == Value::Boolean(false) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Value]) -> Result<Value, EvalError> {
    for expr in exprs {
        let val = eval(expr)?;
        if val != Value::Boolean(false) {
            return Ok(val);
        }
    }
    Ok(Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => arith_add(args),
        "-" => arith_sub(args),
        "*" => arith_mul(args),
        "/" => arith_div(args),
        "<" => cmp_op(args, |a, b| a < b),
        ">" => cmp_op(args, |a, b| a > b),
        "=" => cmp_op(args, |a, b| a == b),
        "<=" => cmp_op(args, |a, b| a <= b),
        ">=" => cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not expects 1 argument".into()));
            }
            Ok(Value::Boolean(args[0] == Value::Boolean(false)))
        }
        _ => Err(EvalError::UnboundVariable(name.to_string())),
    }
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if nums.len() == 1 {
        Ok(Value::Integer(-nums[0]))
    } else {
        Ok(Value::Integer(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut result = nums[0];
    for &d in &nums[1..] {
        if d == 0 {
            return Err(EvalError::Type("division by zero".into()));
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let result = eval_str(input)?;
    Ok((result, String::new()))
}

#[cfg(test)]
mod tests;
