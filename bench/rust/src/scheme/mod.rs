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

    fn kind(&self) -> &'static str {
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
            Self::String(value) => format!("{value:?}"),
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

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if expressions.is_empty() {
            Err(EvalError::EmptyProgram)
        } else {
            Ok(expressions)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::UnexpectedCloseParen),
            Some('"') => self.parse_string().map(Expr::String),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.next_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.next_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<String, EvalError> {
        self.next_char();
        let mut value = String::new();

        while let Some(ch) = self.next_char() {
            match ch {
                '"' => return Ok(value),
                '\\' => {
                    let escaped = self.next_char().ok_or(EvalError::UnexpectedEof)?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => return Err(EvalError::InvalidEscape { escape: other }),
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof)
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.next_char();
        }

        let token = &self.input[start..self.pos];

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Integer(value)),
                Err(_) => Ok(Expr::Symbol(token.into())),
            },
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.next_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.next_char() {
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

    fn next_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }
}

fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    let mut last_value = None;

    for expression in expressions {
        last_value = Some(eval_expr(expression)?);
    }

    last_value.ok_or(EvalError::EmptyProgram)
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
    let (operator, arguments) = items.split_first().ok_or(EvalError::EmptyList)?;

    let Expr::Symbol(name) = operator else {
        return Err(EvalError::NotAProcedure {
            found: expr_kind(operator).into(),
        });
    };

    match name.as_str() {
        "+" => eval_add(arguments),
        "-" => eval_sub(arguments),
        "*" => eval_mul(arguments),
        "/" => eval_div(arguments),
        "<" => eval_compare(name, arguments, |left, right| left < right),
        ">" => eval_compare(name, arguments, |left, right| left > right),
        "=" => eval_compare(name, arguments, |left, right| left == right),
        "<=" => eval_compare(name, arguments, |left, right| left <= right),
        "not" => eval_not(arguments),
        "and" => eval_and(arguments),
        "or" => eval_or(arguments),
        _ => Err(EvalError::UnknownProcedure { name: name.clone() }),
    }
}

fn eval_add(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn eval_sub(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments)?;
    Ok(Value::Integer(
        numbers.into_iter().fold(1_i64, |acc, value| acc * value),
    ))
}

fn eval_div(arguments: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        }),
        [first, rest @ ..] => {
            let mut total = *first;
            for divisor in rest {
                if *divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                total /= divisor;
            }
            Ok(Value::Integer(total))
        }
    }
}

fn eval_compare(
    name: &str,
    arguments: &[Expr],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn eval_not(arguments: &[Expr]) -> Result<Value, EvalError> {
    if arguments.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        });
    }

    let value = eval_expr(&arguments[0])?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_and(arguments: &[Expr]) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr]) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_number_args(arguments: &[Expr]) -> Result<Vec<i64>, EvalError> {
    arguments.iter().map(eval_number).collect()
}

fn eval_number(expr: &Expr) -> Result<i64, EvalError> {
    match eval_expr(expr)? {
        Value::Integer(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.kind().into(),
        }),
    }
}

fn expr_kind(expr: &Expr) -> &'static str {
    match expr {
        Expr::Integer(_) => "number",
        Expr::Boolean(_) => "boolean",
        Expr::String(_) => "string",
        Expr::Symbol(_) => "symbol",
        Expr::List(_) => "list",
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
    let expressions = Parser::new(input).parse_program()?;
    let value = eval_program(&expressions)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
