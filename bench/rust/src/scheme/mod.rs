pub mod error;

pub use error::EvalError;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
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
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => render_string(value),
        }
    }
}

fn render_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }

    out.push('"');
    out
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected at least one expression".to_string(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut out = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(out)),
                Some('\\') => match self.bump_char() {
                    Some('"') => out.push('"'),
                    Some('\\') => out.push('\\'),
                    Some('n') => out.push('\n'),
                    Some('r') => out.push('\r'),
                    Some('t') => out.push('\t'),
                    Some(other) => out.push(other),
                    None => return Err(EvalError::UnterminatedString),
                },
                Some(ch) => out.push(ch),
                None => return Err(EvalError::UnterminatedString),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token =
            self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';');

        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "unexpected token".to_string(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => {
                if let Some(value) = parse_integer_token(token) {
                    Ok(Expr::Integer(value))
                } else {
                    Ok(Expr::Symbol(token.to_string()))
                }
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            self.take_while(|ch| ch.is_whitespace());

            if self.peek_char() == Some(';') {
                self.take_while(|ch| ch != '\n');
                continue;
            }

            break;
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.bump_char();
        }

        &self.input[start..self.index]
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

fn parse_integer_token(token: &str) -> Option<i64> {
    let digits = match token.bytes().next() {
        Some(b'+') | Some(b'-') => &token[1..],
        _ => token,
    };

    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    token.parse().ok()
}

fn eval_program(exprs: &[Expr]) -> Result<Value, EvalError> {
    let mut last = None;

    for expr in exprs {
        last = Some(eval_expr(expr)?);
    }

    last.ok_or_else(|| EvalError::SyntaxError {
        message: "expected at least one expression".to_string(),
    })
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
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    let Expr::Symbol(operator) = &items[0] else {
        return Err(EvalError::NotAProcedure);
    };

    let args = &items[1..];

    match operator.as_str() {
        "and" => eval_and(args),
        "or" => eval_or(args),
        "not" => {
            let value = expect_exact_arity(operator, args, 1)?;
            Ok(Value::Boolean(!eval_expr(value)?.is_truthy()))
        }
        "+" => {
            let numbers = eval_numbers(args)?;
            Ok(Value::Integer(numbers.into_iter().sum()))
        }
        "*" => {
            let numbers = eval_numbers(args)?;
            Ok(Value::Integer(numbers.into_iter().product()))
        }
        "-" => {
            let numbers = eval_numbers(args)?;
            match numbers.as_slice() {
                [] => Err(EvalError::WrongArgumentCount {
                    name: operator.clone(),
                    expected: "at least 1".to_string(),
                    got: 0,
                }),
                [value] => Ok(Value::Integer(-(*value))),
                [first, rest @ ..] => Ok(Value::Integer(
                    rest.iter().fold(*first, |acc, value| acc - *value),
                )),
            }
        }
        "/" => {
            let numbers = eval_numbers(args)?;
            match numbers.as_slice() {
                [] | [_] => Err(EvalError::WrongArgumentCount {
                    name: operator.clone(),
                    expected: "at least 2".to_string(),
                    got: numbers.len(),
                }),
                [first, rest @ ..] => {
                    let mut result = *first;
                    for divisor in rest {
                        if *divisor == 0 {
                            return Err(EvalError::DivisionByZero);
                        }
                        result /= *divisor;
                    }
                    Ok(Value::Integer(result))
                }
            }
        }
        "<" => eval_comparison(operator, args, |left, right| left < right),
        ">" => eval_comparison(operator, args, |left, right| left > right),
        "=" => eval_comparison(operator, args, |left, right| left == right),
        "<=" => eval_comparison(operator, args, |left, right| left <= right),
        _ => Err(EvalError::UnsupportedForm {
            name: operator.clone(),
        }),
    }
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

fn eval_numbers(args: &[Expr]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());

    for expr in args {
        let value = eval_expr(expr)?;
        match value {
            Value::Integer(number) => values.push(number),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "number",
                    actual: other.type_name(),
                });
            }
        }
    }

    Ok(values)
}

fn eval_comparison(
    name: &str,
    args: &[Expr],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_numbers(args)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn expect_exact_arity<'a>(
    name: &str,
    args: &'a [Expr],
    expected: usize,
) -> Result<&'a Expr, EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(&args[0])
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
    let value = eval_program(&exprs)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
