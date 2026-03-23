pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::SchemeString(s) => format!("\"{}\"", s),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self, context: &str) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type {
                message: format!("{context}: expected number, got {}", other.display()),
            }),
        }
    }
}

/// A parsed S-expression.
#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    List(Vec<Expr>),
}

// --- Parser ---

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            let ch = self.input[self.pos];
            if ch.is_ascii_whitespace() {
                self.pos += 1;
            } else if ch == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        match self.peek() {
            None => Err(EvalError::Parse {
                message: "unexpected end of input".to_string(),
            }),
            Some(b'(') => self.parse_list(),
            Some(b'"') => self.parse_string(),
            Some(b'#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => {
                    return Err(EvalError::Parse {
                        message: "unterminated list".to_string(),
                    })
                }
                Some(b')') => {
                    self.pos += 1;
                    return Ok(Expr::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening '"'
        let mut s = String::new();
        loop {
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse {
                    message: "unterminated string".to_string(),
                });
            }
            let ch = self.input[self.pos];
            if ch == b'"' {
                self.pos += 1;
                return Ok(Expr::SchemeString(s));
            }
            if ch == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse {
                        message: "unterminated escape in string".to_string(),
                    });
                }
                let escaped = self.input[self.pos];
                match escaped {
                    b'n' => s.push('\n'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'"' => s.push('"'),
                    _ => {
                        s.push('\\');
                        s.push(escaped as char);
                    }
                }
            } else {
                s.push(ch as char);
            }
            self.pos += 1;
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some(b't') => {
                self.pos += 1;
                Ok(Expr::Boolean(true))
            }
            Some(b'f') => {
                self.pos += 1;
                Ok(Expr::Boolean(false))
            }
            _ => Err(EvalError::Parse {
                message: "unexpected character after #".to_string(),
            }),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let ch = self.input[self.pos];
            if ch.is_ascii_whitespace() || ch == b'(' || ch == b')' || ch == b'"' || ch == b';' {
                break;
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(EvalError::Parse {
                message: "unexpected character".to_string(),
            });
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("valid utf8 slice");
        if let Ok(n) = token.parse::<i64>() {
            Ok(Expr::Integer(n))
        } else {
            Ok(Expr::Symbol(token.to_string()))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// --- Evaluator ---

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".to_string(),
                });
            }
            eval_application(items)
        }
    }
}

fn eval_application(items: &[Expr]) -> Result<Value, EvalError> {
    let head = &items[0];
    let args_exprs = &items[1..];

    // Special forms: and, or
    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(args_exprs),
            "or" => return eval_or(args_exprs),
            _ => {}
        }
    }

    // Evaluate head
    if let Expr::Symbol(name) = head {
        let args: Vec<Value> = args_exprs
            .iter()
            .map(eval)
            .collect::<Result<_, _>>()?;

        match name.as_str() {
            "+" => builtin_add(&args),
            "-" => builtin_sub(&args),
            "*" => builtin_mul(&args),
            "/" => builtin_div(&args),
            "<" => builtin_cmp(&args, "<", |a, b| a < b),
            ">" => builtin_cmp(&args, ">", |a, b| a > b),
            "=" => builtin_cmp(&args, "=", |a, b| a == b),
            "<=" => builtin_cmp(&args, "<=", |a, b| a <= b),
            ">=" => builtin_cmp(&args, ">=", |a, b| a >= b),
            "not" => builtin_not(&args),
            _ => Err(EvalError::UnboundVariable {
                name: name.clone(),
            }),
        }
    } else {
        Err(EvalError::Type {
            message: "not a procedure".to_string(),
        })
    }
}

fn eval_and(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for arg in args {
        sum += arg.as_integer("+")?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "- requires at least one argument".to_string(),
        });
    }
    if args.len() == 1 {
        return Ok(Value::Integer(-args[0].as_integer("-")?));
    }
    let mut result = args[0].as_integer("-")?;
    for arg in &args[1..] {
        result -= arg.as_integer("-")?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for arg in args {
        product *= arg.as_integer("*")?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "/ requires at least one argument".to_string(),
        });
    }
    if args.len() == 1 {
        let divisor = args[0].as_integer("/")?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / divisor));
    }
    let mut result = args[0].as_integer("/")?;
    for arg in &args[1..] {
        let divisor = arg.as_integer("/")?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Integer(result))
}

fn builtin_cmp(args: &[Value], name: &str, op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            message: format!("{name} requires at least two arguments"),
        });
    }
    let mut prev = args[0].as_integer(name)?;
    for arg in &args[1..] {
        let curr = arg.as_integer(name)?;
        if !op(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "not requires exactly one argument".to_string(),
        });
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
        });
    }
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
