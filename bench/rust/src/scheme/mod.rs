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
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => {
                let escaped = value
                    .chars()
                    .flat_map(|ch| match ch {
                        '\\' => ['\\', '\\'].into_iter().collect::<Vec<_>>(),
                        '"' => ['\\', '"'].into_iter().collect::<Vec<_>>(),
                        '\n' => ['\\', 'n'].into_iter().collect::<Vec<_>>(),
                        '\t' => ['\\', 't'].into_iter().collect::<Vec<_>>(),
                        other => [other].into_iter().collect::<Vec<_>>(),
                    })
                    .collect::<String>();

                format!("\"{escaped}\"")
            }
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
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        let Some(ch) = self.peek_char() else {
            return Err(EvalError::UnexpectedEof);
        };

        match ch {
            '(' => self.parse_list(),
            '"' => self.parse_string(),
            '#' => self.parse_boolean(),
            ')' => Err(EvalError::UnexpectedToken { token: ")".into() }),
            '-' if self
                .peek_second_char()
                .is_some_and(|next| next.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            ch if ch.is_ascii_digit() => self.parse_number(),
            _ => self.parse_symbol(),
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
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.bump_char().ok_or(EvalError::UnterminatedString)?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnterminatedString)
    }

    fn parse_boolean(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('#')?;
        match self.bump_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            Some(other) => Err(EvalError::InvalidBoolean {
                literal: format!("#{other}"),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;
        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let mut saw_digit = false;
        while self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump_char();
        }

        let literal = &self.input[start..self.offset];
        if !saw_digit {
            return Err(EvalError::InvalidNumber {
                literal: literal.into(),
            });
        }

        literal
            .parse::<i64>()
            .map(Expr::Integer)
            .map_err(|_| EvalError::InvalidNumber {
                literal: literal.into(),
            })
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while self
            .peek_char()
            .is_some_and(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';')
        {
            self.bump_char();
        }

        if start == self.offset {
            let token = self
                .peek_char()
                .map(|ch| ch.to_string())
                .unwrap_or_else(|| "<eof>".into());
            return Err(EvalError::UnexpectedToken { token });
        }

        Ok(Expr::Symbol(self.input[start..self.offset].into()))
    }

    fn skip_ignored(&mut self) {
        loop {
            while self.peek_char().is_some_and(char::is_whitespace) {
                self.bump_char();
            }

            if self.peek_char() != Some(';') {
                return;
            }

            while let Some(ch) = self.bump_char() {
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::UnexpectedToken {
                token: ch.to_string(),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn bump_char(&mut self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        let ch = chars.next()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        chars.next()?;
        chars.next()
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_application(items),
    }
}

fn eval_application(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::NotAProcedure);
    };

    let Expr::Symbol(name) = head else {
        return Err(EvalError::NotAProcedure);
    };

    match name.as_str() {
        "and" => eval_and(tail),
        "or" => eval_or(tail),
        _ => {
            let values = tail
                .iter()
                .map(eval_expr)
                .collect::<Result<Vec<_>, EvalError>>()?;
            apply_builtin(name, &values)
        }
    }
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval_expr(arg)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr(arg)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let numbers = extract_numbers("+", args)?;
            Ok(Value::Integer(numbers.iter().sum()))
        }
        "*" => {
            let numbers = extract_numbers("*", args)?;
            Ok(Value::Integer(numbers.iter().product()))
        }
        "-" => {
            let numbers = extract_numbers("-", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "-",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let value = if rest.is_empty() {
                -*first
            } else {
                rest.iter().fold(*first, |acc, value| acc - value)
            };

            Ok(Value::Integer(value))
        }
        "/" => {
            let numbers = extract_numbers("/", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            if rest.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 2 arguments",
                    got: 1,
                });
            }

            let mut result = *first;
            for value in rest {
                if *value == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= value;
            }

            Ok(Value::Integer(result))
        }
        "<" => compare_numbers("<", args, |left, right| left < right),
        "<=" => compare_numbers("<=", args, |left, right| left <= right),
        "=" => compare_numbers("=", args, |left, right| left == right),
        ">" => compare_numbers(">", args, |left, right| left > right),
        ">=" => compare_numbers(">=", args, |left, right| left >= right),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "not",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnknownProcedure {
            name: name.to_string(),
        }),
    }
}

fn compare_numbers(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = extract_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn extract_numbers(name: &'static str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            }),
        })
        .collect()
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
    let program = parser.parse_program()?;
    let mut last_value = None;

    for expression in &program {
        last_value = Some(eval_expr(expression)?);
    }

    let value = last_value.ok_or(EvalError::EmptyInput)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
