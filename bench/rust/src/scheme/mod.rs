pub mod error;

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn render(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();

        loop {
            self.skip_ws_and_comments();
            if self.peek_char().is_none() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ws_and_comments();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::Syntax {
                message: "unexpected ')'".into(),
            }),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ws_and_comments();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut value = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(value)),
                Some('\\') => match self.bump_char() {
                    Some('"') => value.push('"'),
                    Some('\\') => value.push('\\'),
                    Some('n') => value.push('\n'),
                    Some('r') => value.push('\r'),
                    Some('t') => value.push('\t'),
                    Some(other) => value.push(other),
                    None => return Err(EvalError::UnexpectedEof),
                },
                Some(ch) => value.push(ch),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Integer(value)),
                Err(_) => Ok(Expr::Symbol(token.into())),
            },
        }
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.bump_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.bump_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
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
    let value = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;

    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr)?;
    }

    Ok(result)
}

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name) = head {
        return match name.as_str() {
            "and" => eval_and(tail),
            "or" => eval_or(tail),
            "not" => {
                let args = eval_args(tail)?;
                builtin_not(&args)
            }
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" => {
                let args = eval_args(tail)?;
                apply_builtin(name, &args)
            }
            _ => Err(EvalError::UnboundVariable { name: name.clone() }),
        };
    }

    let value = eval(head)?;
    Err(EvalError::NotAProcedure {
        got: value.type_name().into(),
    })
}

fn eval_args(args: &[Expr]) -> Result<Vec<Value>, EvalError> {
    args.iter().map(eval).collect()
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => builtin_add(args),
        "-" => builtin_sub(args),
        "*" => builtin_mul(args),
        "/" => builtin_div(args),
        "<" => compare_numbers("<", args, |left, right| left < right),
        ">" => compare_numbers(">", args, |left, right| left > right),
        "=" => compare_numbers("=", args, |left, right| left == right),
        "<=" => compare_numbers("<=", args, |left, right| left <= right),
        _ => Err(EvalError::UnboundVariable { name: name.into() }),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0;
    for arg in args {
        total += expect_number("+", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(wrong_arg_count("-", "at least 1", 0)),
        [arg] => Ok(Value::Integer(-expect_number("-", arg)?)),
        [first, rest @ ..] => {
            let mut total = expect_number("-", first)?;
            for arg in rest {
                total -= expect_number("-", arg)?;
            }
            Ok(Value::Integer(total))
        }
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1;
    for arg in args {
        total *= expect_number("*", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(wrong_arg_count("/", "at least 2", 0));
    };

    if rest.is_empty() {
        return Err(wrong_arg_count("/", "at least 2", 1));
    }

    let mut total = expect_number("/", first)?;
    for arg in rest {
        let divisor = expect_number("/", arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }

    Ok(Value::Integer(total))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count("not", "1", args.len())),
    }
}

fn compare_numbers<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_number(name, iter.next().expect("len checked"))?;

    for arg in iter {
        let right = expect_number(name, arg)?;
        if !predicate(left, right) {
            return Ok(Value::Boolean(false));
        }
        left = right;
    }

    Ok(Value::Boolean(true))
}

fn expect_number(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "number".into(),
            got: value.type_name().into(),
        }),
    }
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();

    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }

    escaped
}

#[cfg(test)]
mod tests;
