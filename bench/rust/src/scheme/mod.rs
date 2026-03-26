pub mod error;

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
}

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
    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let escaped = value
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t");
                format!("\"{escaped}\"")
            }
        }
    }

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

    fn as_number(&self, name: &'static str) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name().to_string(),
            }),
        }
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.index < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or(EvalError::UnexpectedEof)?;
        self.index += 1;

        match token {
            Token::LParen => {
                let mut items = Vec::new();
                while self.index < self.tokens.len() {
                    if matches!(self.tokens.get(self.index), Some(Token::RParen)) {
                        self.index += 1;
                        return Ok(Expr::List(items));
                    }
                    items.push(self.parse_expr()?);
                }
                Err(EvalError::UnexpectedEof)
            }
            Token::RParen => Err(EvalError::UnexpectedToken {
                token: ")".to_string(),
            }),
            Token::Integer(value) => Ok(Expr::Integer(value)),
            Token::Boolean(value) => Ok(Expr::Boolean(value)),
            Token::String(value) => Ok(Expr::String(value)),
            Token::Symbol(value) => Ok(Expr::Symbol(value)),
        }
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
    let (result, output) = eval_str_with_output(input)?;
    debug_assert!(output.is_empty());
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let exprs = parser.parse_program()?;

    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let mut last = Value::Boolean(false);
    for expr in &exprs {
        last = eval(expr)?;
    }

    Ok((last.render(), String::new()))
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < bytes.len() {
        match bytes[index] {
            b' ' | b'\n' | b'\r' | b'\t' => {
                index += 1;
            }
            b'(' => {
                tokens.push(Token::LParen);
                index += 1;
            }
            b')' => {
                tokens.push(Token::RParen);
                index += 1;
            }
            b'"' => {
                let (value, next_index) = parse_string(input, index + 1)?;
                tokens.push(Token::String(value));
                index = next_index;
            }
            b'#' => {
                if let Some((token, next_index)) = parse_boolean(input, index) {
                    tokens.push(token);
                    index = next_index;
                } else {
                    return Err(EvalError::UnexpectedToken {
                        token: input[index..].to_string(),
                    });
                }
            }
            _ => {
                let start = index;
                while index < bytes.len()
                    && !matches!(bytes[index], b' ' | b'\n' | b'\r' | b'\t' | b'(' | b')')
                {
                    index += 1;
                }

                let atom = &input[start..index];
                if let Ok(value) = atom.parse::<i64>() {
                    tokens.push(Token::Integer(value));
                } else if atom.chars().next().is_some_and(|ch| ch == '+' || ch == '-')
                    && atom.len() > 1
                    && atom[1..].chars().all(|ch| ch.is_ascii_digit())
                {
                    return Err(EvalError::InvalidInteger {
                        value: atom.to_string(),
                    });
                } else {
                    tokens.push(Token::Symbol(atom.to_string()));
                }
            }
        }
    }

    Ok(tokens)
}

fn parse_string(input: &str, mut index: usize) -> Result<(String, usize), EvalError> {
    let bytes = input.as_bytes();
    let mut value = String::new();

    while index < bytes.len() {
        match bytes[index] {
            b'"' => return Ok((value, index + 1)),
            b'\\' => {
                index += 1;
                let escaped = bytes.get(index).ok_or(EvalError::UnterminatedString)?;
                value.push(match escaped {
                    b'"' => '"',
                    b'\\' => '\\',
                    b'n' => '\n',
                    b't' => '\t',
                    other => *other as char,
                });
                index += 1;
            }
            other => {
                value.push(other as char);
                index += 1;
            }
        }
    }

    Err(EvalError::UnterminatedString)
}

fn parse_boolean(input: &str, index: usize) -> Option<(Token, usize)> {
    let remainder = &input[index..];
    if remainder.starts_with("#t") && is_delimiter(input, index + 2) {
        Some((Token::Boolean(true), index + 2))
    } else if remainder.starts_with("#f") && is_delimiter(input, index + 2) {
        Some((Token::Boolean(false), index + 2))
    } else {
        None
    }
}

fn is_delimiter(input: &str, index: usize) -> bool {
    match input.as_bytes().get(index) {
        None => true,
        Some(b' ' | b'\n' | b'\r' | b'\t' | b'(' | b')') => true,
        Some(_) => false,
    }
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
    let (head, tail) = items
        .split_first()
        .ok_or_else(|| EvalError::NotAProcedure {
            found: "()".to_string(),
        })?;

    let Expr::Symbol(name) = head else {
        let found = eval(head)?.type_name().to_string();
        return Err(EvalError::NotAProcedure { found });
    };

    match name.as_str() {
        "and" => eval_and(tail),
        "or" => eval_or(tail),
        "not" => eval_not(tail),
        "+" => eval_add(tail),
        "-" => eval_sub(tail),
        "*" => eval_mul(tail),
        "/" => eval_div(tail),
        "<" => eval_compare(tail, "<", |left, right| left < right),
        ">" => eval_compare(tail, ">", |left, right| left > right),
        "=" => eval_compare(tail, "=", |left, right| left == right),
        "<=" => eval_compare(tail, "<=", |left, right| left <= right),
        _ => Err(EvalError::UnboundVariable { name: name.clone() }),
    }
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);
    for expr in args {
        let value = eval(expr)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_not(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let value = eval(&args[0])?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_add(args: &[Expr]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for value in eval_numbers("+", args)? {
        sum += value;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Expr]) -> Result<Value, EvalError> {
    let values = eval_numbers("-", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "-",
        expected: "at least 1",
        got: 0,
    })?;

    let result = if rest.is_empty() {
        -*first
    } else {
        rest.iter().fold(*first, |acc, value| acc - value)
    };

    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Expr]) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for value in eval_numbers("*", args)? {
        product *= value;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Expr]) -> Result<Value, EvalError> {
    let values = eval_numbers("/", args)?;
    let (first, rest) = values.split_first().ok_or(EvalError::WrongArgCount {
        name: "/",
        expected: "at least 1",
        got: 0,
    })?;

    if rest.is_empty() {
        if *first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
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

fn eval_compare<F>(args: &[Expr], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_numbers(name, args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: values.len(),
        });
    }

    for pair in values.windows(2) {
        if !cmp(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn eval_numbers(name: &'static str, args: &[Expr]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr)?.as_number(name)?);
    }
    Ok(values)
}

#[cfg(test)]
mod tests;
