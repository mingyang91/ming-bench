pub mod error;

pub use error::EvalError;

use std::fmt;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    if program.is_empty() {
        return Err(EvalError::Syntax("expected at least one expression".into()));
    }

    let mut last = None;
    for expr in &program {
        last = Some(eval_expr(expr)?);
    }

    Ok(last.expect("program is not empty").to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|result| (result, String::new()))
}

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Number(i64),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Bool(_) => "boolean",
            Self::Number(_) => "number",
            Self::String(_) => "string",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(true) => f.write_str("#t"),
            Self::Bool(false) => f.write_str("#f"),
            Self::Number(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{}\"", escape_string(value)),
        }
    }
}

fn eval_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Number(value) => Ok(Value::Number(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundSymbol(name.clone())),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()));
    };

    match head {
        Expr::Symbol(name) if name == "and" => eval_and(args),
        Expr::Symbol(name) if name == "or" => eval_or(args),
        Expr::Symbol(name) => {
            let evaluated = args
                .iter()
                .map(eval_expr)
                .collect::<Result<Vec<_>, EvalError>>()?;
            apply_builtin(name, &evaluated)
        }
        _ => {
            let value = eval_expr(head)?;
            Err(EvalError::NotAProcedure(value.to_string()))
        }
    }
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for arg in args {
        last = eval_expr(arg)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr(arg)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let numbers = expect_numbers(name, args)?;
            let sum = numbers
                .iter()
                .try_fold(0_i64, |acc, value| acc.checked_add(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(sum))
        }
        "-" => {
            let numbers = expect_numbers(name, args)?;
            match numbers.split_first() {
                None => Err(wrong_arg_count(name, "at least 1", args.len())),
                Some((first, [])) => first
                    .checked_neg()
                    .map(Value::Number)
                    .ok_or(EvalError::IntegerOverflow),
                Some((first, rest)) => {
                    let result = rest
                        .iter()
                        .try_fold(*first, |acc, value| acc.checked_sub(*value))
                        .ok_or(EvalError::IntegerOverflow)?;
                    Ok(Value::Number(result))
                }
            }
        }
        "*" => {
            let numbers = expect_numbers(name, args)?;
            let product = numbers
                .iter()
                .try_fold(1_i64, |acc, value| acc.checked_mul(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(product))
        }
        "/" => {
            let numbers = expect_numbers(name, args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            };
            if rest.is_empty() {
                return Err(wrong_arg_count(name, "at least 2", args.len()));
            }

            let result = rest.iter().try_fold(*first, |acc, value| {
                if *value == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                acc.checked_div(*value).ok_or(EvalError::IntegerOverflow)
            })?;
            Ok(Value::Number(result))
        }
        "<" => compare_numbers(name, args, |left, right| left < right),
        ">" => compare_numbers(name, args, |left, right| left > right),
        "=" => compare_numbers(name, args, |left, right| left == right),
        "<=" => compare_numbers(name, args, |left, right| left <= right),
        "not" => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(!args[0].is_truthy()))
        }
        _ => Err(EvalError::UnknownProcedure(name.to_string())),
    }
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let is_true = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_true))
}

fn expect_numbers<'a>(name: &str, args: &'a [Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Number(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                expected: format!("number for {name}"),
                found: other.type_name().to_string(),
            }),
        })
        .collect()
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgumentCount {
        name: name.to_string(),
        expected: expected.to_string(),
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
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");

        match ch {
            c if c.is_whitespace() => {
                index += ch.len_utf8();
            }
            ';' => {
                index += ch.len_utf8();
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    index += next.len_utf8();
                    if next == '\n' {
                        break;
                    }
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                index += ch.len_utf8();
            }
            ')' => {
                tokens.push(Token::RParen);
                index += ch.len_utf8();
            }
            '"' => {
                let (string, next_index) = parse_string(input, index)?;
                tokens.push(Token::String(string));
                index = next_index;
            }
            _ => {
                let start = index;
                while index < input.len() {
                    let next = input[index..]
                        .chars()
                        .next()
                        .expect("index always points to a valid character boundary");
                    if next.is_whitespace() || next == '(' || next == ')' || next == ';' {
                        break;
                    }
                    index += next.len_utf8();
                }

                let atom = &input[start..index];
                tokens.push(parse_atom(atom));
            }
        }
    }

    Ok(tokens)
}

fn parse_string(input: &str, start: usize) -> Result<(String, usize), EvalError> {
    let mut result = String::new();
    let mut index = start + 1;

    while index < input.len() {
        let ch = input[index..]
            .chars()
            .next()
            .expect("index always points to a valid character boundary");
        index += ch.len_utf8();

        match ch {
            '"' => return Ok((result, index)),
            '\\' => {
                let escaped = input[index..]
                    .chars()
                    .next()
                    .ok_or_else(|| EvalError::Syntax("unterminated string literal".into()))?;
                index += escaped.len_utf8();
                match escaped {
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    _ => {
                        return Err(EvalError::Syntax(format!(
                            "unsupported escape sequence: \\{escaped}"
                        )))
                    }
                }
            }
            _ => result.push(ch),
        }
    }

    Err(EvalError::Syntax("unterminated string literal".into()))
}

fn parse_atom(atom: &str) -> Token {
    match atom {
        "#t" => Token::Bool(true),
        "#f" => Token::Bool(false),
        _ => match atom.parse::<i64>() {
            Ok(value) => Token::Number(value),
            Err(_) => Token::Symbol(atom.to_string()),
        },
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
        let mut expressions = Vec::new();
        while self.index < self.tokens.len() {
            expressions.push(self.parse_expr()?);
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .tokens
            .get(self.index)
            .cloned()
            .ok_or_else(|| EvalError::Syntax("unexpected end of input".into()))?;
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
                Err(EvalError::Syntax("missing ')'".into()))
            }
            Token::RParen => Err(EvalError::Syntax("unexpected ')'".into())),
            Token::Bool(value) => Ok(Expr::Bool(value)),
            Token::Number(value) => Ok(Expr::Number(value)),
            Token::String(value) => Ok(Expr::String(value)),
            Token::Symbol(name) => Ok(Expr::Symbol(name)),
        }
    }
}
