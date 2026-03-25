pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    List(Vec<Value>),
    Symbol(String),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".into(),
        }
    }
}

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
                // Skip line comments
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
        let ch = self.chars.get(self.pos).copied();
        if ch.is_some() {
            self.pos += 1;
        }
        ch
    }

    fn parse_expr(&mut self) -> Result<Value, EvalError> {
        self.skip_whitespace();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(EvalError::Parse("unclosed parenthesis".into())),
                Some(')') => {
                    self.next_char();
                    return Ok(Value::List(items));
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume opening '"'
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => return Err(EvalError::Parse("unclosed string".into())),
                Some('"') => return Ok(Value::Str(s)),
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => s.push(c),
                    None => return Err(EvalError::Parse("unclosed string escape".into())),
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Value, EvalError> {
        self.next_char(); // consume '#'
        match self.next_char() {
            Some('t') => {
                // Check it's not part of a longer identifier
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Value::Boolean(true))
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            Some('f') => {
                if self.peek().is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-' && c != '!' && c != '?') {
                    Ok(Value::Boolean(false))
                } else {
                    Err(EvalError::Parse("invalid boolean literal".into()))
                }
            }
            _ => Err(EvalError::Parse("invalid hash literal".into())),
        }
    }

    fn parse_atom(&mut self) -> Result<Value, EvalError> {
        let mut token = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            token.push(c);
            self.next_char();
        }
        if token.is_empty() {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        // Try integer
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Value::Integer(n));
        }
        Ok(Value::Symbol(token))
    }

    fn parse_all(&mut self) -> Result<Vec<Value>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Void => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            let op = match &items[0] {
                Value::Symbol(s) => s.as_str(),
                _ => return Err(EvalError::Type("not a procedure".into())),
            };
            let args = &items[1..];
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
                        Ok(Value::Integer(-eval(&args[0])?.as_integer()?))
                    } else {
                        let mut result = eval(&args[0])?.as_integer()?;
                        for a in &args[1..] {
                            result -= eval(a)?.as_integer()?;
                        }
                        Ok(Value::Integer(result))
                    }
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
                "<" => {
                    let vals = eval_args_to_ints(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] < w[1])))
                }
                ">" => {
                    let vals = eval_args_to_ints(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] > w[1])))
                }
                "=" => {
                    let vals = eval_args_to_ints(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] == w[1])))
                }
                "<=" => {
                    let vals = eval_args_to_ints(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] <= w[1])))
                }
                ">=" => {
                    let vals = eval_args_to_ints(args)?;
                    Ok(Value::Boolean(vals.windows(2).all(|w| w[0] >= w[1])))
                }
                "not" => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
                    }
                    Ok(Value::Boolean(!eval(&args[0])?.is_truthy()))
                }
                "and" => {
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
    }
}

fn eval_args_to_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|a| eval(a)?.as_integer()).collect()
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
