pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => render_string(value),
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::Syntax {
                message: "empty input".into(),
            })
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_boolean(),
            Some('+') | Some('-')
                if self
                    .peek_second_char()
                    .is_some_and(|ch| ch.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            Some(ch) if ch.is_ascii_digit() => self.parse_number(),
            Some(_) => self.parse_symbol(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump();
        let mut value = String::new();

        loop {
            match self.bump() {
                Some('"') => return Ok(Expr::String(value)),
                Some('\\') => {
                    let escaped = match self.bump() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(ch) => ch,
                        None => return Err(EvalError::UnexpectedEof),
                    };
                    value.push(escaped);
                }
                Some(ch) => value.push(ch),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_boolean(&mut self) -> Result<Expr, EvalError> {
        if self.consume_literal("#t") {
            return Ok(Expr::Boolean(true));
        }

        if self.consume_literal("#f") {
            return Ok(Expr::Boolean(false));
        }

        Err(EvalError::Syntax {
            message: "invalid boolean literal".into(),
        })
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        if matches!(self.peek_char(), Some('+') | Some('-')) {
            self.bump();
        }

        let mut saw_digit = false;
        while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump();
        }

        if !saw_digit {
            return Err(EvalError::Syntax {
                message: "invalid number literal".into(),
            });
        }

        let token = &self.input[start..self.offset];
        let value = token.parse::<i64>().map_err(|_| EvalError::Syntax {
            message: format!("invalid number literal: {token}"),
        })?;

        Ok(Expr::Integer(value))
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while matches!(self.peek_char(), Some(ch) if !is_delimiter(ch)) {
            self.bump();
        }

        if start == self.offset {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        Ok(Expr::Symbol(self.input[start..self.offset].to_string()))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek_char() {
                Some(ch) if ch.is_whitespace() => {
                    self.bump();
                }
                Some(';') => {
                    while let Some(ch) = self.bump() {
                        if ch == '\n' {
                            break;
                        }
                    }
                }
                _ => return,
            }
        }
    }

    fn consume_literal(&mut self, literal: &str) -> bool {
        if !self.remaining().starts_with(literal) {
            return false;
        }

        let end = self.offset + literal.len();
        if self
            .input
            .get(end..)
            .and_then(|rest| rest.chars().next())
            .is_some_and(|ch| !is_delimiter(ch))
        {
            return false;
        }

        self.offset = end;
        true
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.offset..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.remaining().chars();
        chars.next()?;
        chars.next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let mut last = None;

    for expr in &exprs {
        last = Some(eval_expr(expr)?);
    }

    last.map(|value| value.render())
        .ok_or_else(|| EvalError::Syntax {
            message: "empty input".into(),
        })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    }

    if let Expr::Symbol(name) = &items[0] {
        match name.as_str() {
            "and" => return eval_and(&items[1..]),
            "or" => return eval_or(&items[1..]),
            _ => {
                let args = items[1..]
                    .iter()
                    .map(eval_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                return apply_builtin(name, &args);
            }
        }
    }

    let operator = eval_expr(&items[0])?;
    Err(EvalError::NotAProcedure {
        found: operator.render(),
    })
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => add(args),
        "-" => subtract(args),
        "*" => multiply(args),
        "/" => divide(args),
        "<" => compare(name, args, |left, right| left < right),
        ">" => compare(name, args, |left, right| left > right),
        "=" => compare(name, args, |left, right| left == right),
        "<=" => compare(name, args, |left, right| left <= right),
        "not" => builtin_not(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn add(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("+", args)?;
    Ok(Value::Integer(values.into_iter().sum()))
}

fn subtract(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("-", args)?;
    match values.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn multiply(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("*", args)?;
    Ok(Value::Integer(values.into_iter().product()))
}

fn divide(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("/", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 1,
        });
    }

    let mut total = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= value;
    }

    Ok(Value::Integer(total))
}

fn compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = expect_numbers(name, args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2 arguments".into(),
            got: values.len(),
        });
    }

    let result = values.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn expect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                name: name.into(),
                expected: "number".into(),
                found: other.type_name().into(),
            }),
        })
        .collect()
}

fn render_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');

    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }

    out.push('"');
    out
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

#[cfg(test)]
mod tests;
