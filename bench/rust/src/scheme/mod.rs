pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Bool(bool),
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Int(i64),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Int(_) => "number",
            Self::String(_) => "string",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::Int(value) => value.to_string(),
            Self::String(value) => render_string(value),
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, cursor: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyInput)
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::SyntaxError {
                message: "unexpected ')'".into(),
            }),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::SyntaxError {
                message: "unexpected end of input".into(),
            }),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::SyntaxError {
                        message: "unterminated list".into(),
                    });
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
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
                    Some(other) => {
                        return Err(EvalError::SyntaxError {
                            message: format!("unsupported string escape: \\{other}"),
                        });
                    }
                    None => {
                        return Err(EvalError::SyntaxError {
                            message: "unterminated string escape".into(),
                        });
                    }
                },
                Some(ch) => value.push(ch),
                None => {
                    return Err(EvalError::SyntaxError {
                        message: "unterminated string".into(),
                    });
                }
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let mut token = String::new();

        while let Some(ch) = self.peek_char() {
            if is_delimiter(ch) {
                break;
            }

            token.push(ch);
            self.bump_char();
        }

        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected expression".into(),
            });
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value)),
                Err(_) => Ok(Expr::Symbol(token)),
            },
        }
    }

    fn skip_ignored(&mut self) {
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

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found '{actual}'"),
            }),
            None => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found end of input"),
            }),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.cursor..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.cursor += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.cursor >= self.input.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    let mut last = None;

    for expression in expressions {
        last = Some(eval_expr(expression)?);
    }

    last.ok_or(EvalError::EmptyInput)
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundSymbol { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or_else(|| EvalError::SyntaxError {
        message: "cannot evaluate an empty list".into(),
    })?;

    let operator = match head {
        Expr::Symbol(name) => name.as_str(),
        _ => {
            return Err(EvalError::NotAProcedure {
                found: "non-symbol".into(),
            });
        }
    };

    match operator {
        "+" => eval_add(args),
        "-" => eval_sub(args),
        "*" => eval_mul(args),
        "/" => eval_div(args),
        "<" => eval_compare(operator, args, |lhs, rhs| lhs < rhs),
        ">" => eval_compare(operator, args, |lhs, rhs| lhs > rhs),
        "=" => eval_compare(operator, args, |lhs, rhs| lhs == rhs),
        "<=" => eval_compare(operator, args, |lhs, rhs| lhs <= rhs),
        "not" => eval_not(args),
        "and" => eval_and(args),
        "or" => eval_or(args),
        _ => Err(EvalError::UnboundSymbol {
            name: operator.to_string(),
        }),
    }
}

fn eval_add(args: &[Expr]) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total += expect_number(eval_expr(arg)?)?;
    }

    Ok(Value::Int(total))
}

fn eval_sub(args: &[Expr]) -> Result<Value, EvalError> {
    let values = eval_number_args("-", args, 1)?;

    let result = if values.len() == 1 {
        -values[0]
    } else {
        let (first, rest) = values.split_first().expect("length checked");
        rest.iter().fold(*first, |acc, value| acc - value)
    };

    Ok(Value::Int(result))
}

fn eval_mul(args: &[Expr]) -> Result<Value, EvalError> {
    let mut product = 1_i64;

    for arg in args {
        product *= expect_number(eval_expr(arg)?)?;
    }

    Ok(Value::Int(product))
}

fn eval_div(args: &[Expr]) -> Result<Value, EvalError> {
    let values = eval_number_args("/", args, 2)?;
    let (first, rest) = values.split_first().expect("length checked");
    let mut result = *first;

    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        if result % value != 0 {
            return Err(EvalError::NonIntegerDivision);
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn eval_compare<F>(name: &str, args: &[Expr], compare: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_number_args(name, args, 2)?;

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    }

    let value = eval_expr(&args[0])?;
    Ok(Value::Bool(!value.is_truthy()))
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for arg in args {
        let value = eval_expr(arg)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);

    for arg in args {
        let value = eval_expr(arg)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_number_args(name: &str, args: &[Expr], min: usize) -> Result<Vec<i64>, EvalError> {
    if args.len() < min {
        return Err(EvalError::WrongArgCount {
            name: name.to_string(),
            expected: format!("at least {min}"),
            actual: args.len(),
        });
    }

    args.iter()
        .map(|arg| eval_expr(arg).and_then(expect_number))
        .collect()
}

fn expect_number(value: Value) -> Result<i64, EvalError> {
    match value {
        Value::Int(number) => Ok(number),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.type_name().into(),
        }),
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
    let expressions = parser.parse_program()?;
    let result = eval_program(&expressions)?;
    Ok(result.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|result| (result, String::new()))
}

#[cfg(test)]
mod tests;
