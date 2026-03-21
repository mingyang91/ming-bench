pub mod error;

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Int(i64),
    Bool(bool),
    String(String),
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn render(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
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
        self.skip_whitespace();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected expression".into(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();

        let next = self.peek_char().ok_or(EvalError::UnexpectedEof)?;
        match next {
            '(' => self.parse_list(),
            ')' => Err(EvalError::SyntaxError {
                message: "unexpected ')'".into(),
            }),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.advance_char().ok_or(EvalError::UnexpectedEof)?;
                    let resolved = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => {
                            return Err(EvalError::SyntaxError {
                                message: format!("unsupported string escape: \\{other}"),
                            });
                        }
                    };
                    value.push(resolved);
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof)
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')') {
                break;
            }
            self.advance_char();
        }

        let token = &self.input[start..self.offset];
        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected token".into(),
            });
        }

        match token {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value)),
                Err(_) => Ok(Expr::Symbol(token.into())),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
            self.advance_char();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.advance_char() {
            Some(found) if found == expected => Ok(()),
            Some(found) => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found '{found}'"),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
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
    let mut last_value = None;

    for expr in exprs {
        last_value = Some(eval_expr(&expr)?);
    }

    last_value
        .map(|value| value.render())
        .ok_or(EvalError::SyntaxError {
            message: "expected expression".into(),
        })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|value| (value, String::new()))
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    let Expr::Symbol(name) = head else {
        return Err(EvalError::NotCallable {
            found: expr_type_name(head),
        });
    };

    match name.as_str() {
        "+" => eval_add(&items[1..]),
        "-" => eval_sub(&items[1..]),
        "*" => eval_mul(&items[1..]),
        "/" => eval_div(&items[1..]),
        "<" => eval_compare(&items[1..], name, |left, right| left < right),
        ">" => eval_compare(&items[1..], name, |left, right| left > right),
        "=" => eval_compare(&items[1..], name, |left, right| left == right),
        "<=" => eval_compare(&items[1..], name, |left, right| left <= right),
        "not" => eval_not(&items[1..]),
        "and" => eval_and(&items[1..]),
        "or" => eval_or(&items[1..]),
        _ => Err(EvalError::UnknownOperator { name: name.clone() }),
    }
}

fn eval_add(args: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    Ok(Value::Int(numbers.into_iter().sum()))
}

fn eval_sub(args: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    match numbers.as_slice() {
        [] => Err(EvalError::WrongArity {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }),
        [value] => Ok(Value::Int(-value)),
        [first, rest @ ..] => Ok(Value::Int(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(args: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    Ok(Value::Int(numbers.into_iter().product()))
}

fn eval_div(args: &[Expr]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArity {
            name: "/".into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArity {
            name: "/".into(),
            expected: "at least 2".into(),
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

    Ok(Value::Int(result))
}

fn eval_compare<F>(args: &[Expr], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_number_args(args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArity {
            name: name.into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        });
    }

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArity {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(!eval_expr(&args[0])?.is_truthy()))
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

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

    Ok(Value::Bool(false))
}

fn eval_number_args(args: &[Expr]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|expr| match eval_expr(expr)? {
            Value::Int(value) => Ok(value),
            other => Err(EvalError::TypeError {
                expected: "number",
                found: other.type_name(),
            }),
        })
        .collect()
}

fn expr_type_name(expr: &Expr) -> &'static str {
    match expr {
        Expr::Int(_) => "number",
        Expr::Bool(_) => "boolean",
        Expr::String(_) => "string",
        Expr::Symbol(_) => "symbol",
        Expr::List(_) => "list",
    }
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

#[cfg(test)]
mod tests;
