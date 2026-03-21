pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone)]
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

/// A parsed S-expression.
#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Parser ──────────────────────────────────────────────────────────

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.pos += 1;
            } else if self.chars[self.pos] == ';' {
                // skip line comments
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Expr::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume opening "
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unterminated string".into())),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => return Err(EvalError::Parse("unterminated escape".into())),
                },
                Some('"') => return Ok(Expr::Str(s)),
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            _ => Err(EvalError::Parse("invalid # literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            self.pos += 1;
        }
        let token: String = self.chars[start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse("empty token".into()));
        }
        // Try parsing as integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr::Integer(n));
        }
        Ok(Expr::Symbol(token))
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::Str(s) => Ok(Value::Str(s.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            match &items[0] {
                Expr::Symbol(op) => eval_special(op, &items[1..]),
                _ => Err(EvalError::Type("not a procedure".into())),
            }
        }
    }
}

fn eval_special(op: &str, args: &[Expr]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += eval(a)?.as_integer()?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-eval(&args[0])?.as_integer()?));
            }
            let mut result = eval(&args[0])?.as_integer()?;
            for a in &args[1..] {
                result -= eval(a)?.as_integer()?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= eval(a)?.as_integer()?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let mut result = eval(&args[0])?.as_integer()?;
            for a in &args[1..] {
                let divisor = eval(a)?.as_integer()?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_op(args, |a, b| a < b),
        ">" => compare_op(args, |a, b| a > b),
        "=" => compare_op(args, |a, b| a == b),
        "<=" => compare_op(args, |a, b| a <= b),
        ">=" => compare_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!eval(&args[0])?.is_truthy()))
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

fn compare_op(args: &[Expr], cmp: impl Fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = eval(&args[0])?.as_integer()?;
    for a in &args[1..] {
        let cur = eval(a)?.as_integer()?;
        if !cmp(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
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
