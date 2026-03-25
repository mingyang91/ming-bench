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
    let exprs = Parser::new(input).parse_program()?;
    let mut last = None;

    for expr in &exprs {
        last = Some(eval(expr)?);
    }

    let value = last.ok_or(EvalError::EmptyInput)?;
    Ok(value.to_scheme_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Integer(i64),
    Bool(bool),
    String(String),
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Bool(_) => "boolean",
            Value::String(_) => "string",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false))
    }

    fn as_number(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "number".into(),
                got: other.type_name().into(),
            }),
        }
    }

    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Bool(true) => "#t".into(),
            Value::Bool(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.type_name())
    }
}

fn escape_string(input: &str) -> String {
    let mut escaped = String::new();

    for ch in input.chars() {
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

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
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

    match head {
        Expr::Symbol(name) => match name.as_str() {
            "and" => eval_and(tail),
            "or" => eval_or(tail),
            "+" | "-" | "*" | "/" | "<" | "<=" | "=" | ">" | ">=" | "not" => {
                let args = eval_all(tail)?;
                apply_builtin(name, &args)
            }
            _ => Err(EvalError::UnboundVariable { name: name.clone() }),
        },
        other => {
            let value = eval(other)?;
            Err(EvalError::NotCallable {
                found: value.type_name().into(),
            })
        }
    }
}

fn eval_all(exprs: &[Expr]) -> Result<Vec<Value>, EvalError> {
    exprs.iter().map(eval).collect()
}

fn eval_and(exprs: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for expr in exprs {
        let value = eval(expr)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(exprs: &[Expr]) -> Result<Value, EvalError> {
    for expr in exprs {
        let value = eval(expr)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Bool(false))
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => apply_add(args),
        "-" => apply_sub(args),
        "*" => apply_mul(args),
        "/" => apply_div(args),
        "<" => apply_compare(name, args, |left, right| left < right),
        "<=" => apply_compare(name, args, |left, right| left <= right),
        "=" => apply_compare(name, args, |left, right| left == right),
        ">" => apply_compare(name, args, |left, right| left > right),
        ">=" => apply_compare(name, args, |left, right| left >= right),
        "not" => apply_not(args),
        _ => Err(EvalError::UnboundVariable {
            name: name.to_string(),
        }),
    }
}

fn apply_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;

    for arg in args {
        total = total
            .checked_add(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_sub(args: &[Value]) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    };

    let first = first.as_number()?;

    if rest.is_empty() {
        return Ok(Value::Integer(
            first.checked_neg().ok_or(EvalError::IntegerOverflow)?,
        ));
    }

    let mut total = first;
    for arg in rest {
        total = total
            .checked_sub(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;

    for arg in args {
        total = total
            .checked_mul(arg.as_number()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_div(args: &[Value]) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 1".into(),
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        });
    }

    let mut total = first.as_number()?;
    for arg in rest {
        let divisor = arg.as_number()?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total = total
            .checked_div(divisor)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn apply_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2".into(),
            got: args.len(),
        });
    }

    let mut iter = args.iter();
    let mut left = iter.next().expect("comparison arity checked").as_number()?;

    for arg in iter {
        let right = arg.as_number()?;
        if !predicate(left, right) {
            return Ok(Value::Bool(false));
        }
        left = right;
    }

    Ok(Value::Bool(true))
}

fn apply_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

struct Parser<'a> {
    chars: Vec<char>,
    index: usize,
    _source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            _source: source,
        }
    }

    fn parse_program(mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while self.peek().is_some() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::Syntax {
                message: "unexpected ')'".into(),
            }),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_token_expr(),
            None => Err(EvalError::Syntax {
                message: "unexpected end of input".into(),
            }),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek() {
                Some(')') => {
                    self.index += 1;
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => {
                    return Err(EvalError::Syntax {
                        message: "unterminated list".into(),
                    });
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect('"')?;
        let mut value = String::new();

        while let Some(ch) = self.peek() {
            self.index += 1;
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.peek().ok_or(EvalError::Syntax {
                        message: "unterminated string escape".into(),
                    })?;
                    self.index += 1;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => value.push(other),
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::Syntax {
            message: "unterminated string".into(),
        })
    }

    fn parse_token_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self.take_token();

        if token.is_empty() {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        match token.as_str() {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ if is_integer_token(&token) => {
                let value = token.parse().map_err(|_| EvalError::Syntax {
                    message: format!("invalid integer literal: {token}"),
                })?;
                Ok(Expr::Integer(value))
            }
            _ => Ok(Expr::Symbol(token)),
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
                self.index += 1;
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.peek() {
                    self.index += 1;
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn take_token(&mut self) -> String {
        let start = self.index;

        while matches!(self.peek(), Some(ch) if !is_token_delimiter(ch)) {
            self.index += 1;
        }

        self.chars[start..self.index].iter().collect()
    }

    fn expect(&mut self, expected: char) -> Result<(), EvalError> {
        match self.peek() {
            Some(ch) if ch == expected => {
                self.index += 1;
                Ok(())
            }
            Some(found) => Err(EvalError::Syntax {
                message: format!("expected '{expected}', found '{found}'"),
            }),
            None => Err(EvalError::Syntax {
                message: format!("expected '{expected}', found end of input"),
            }),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }
}

fn is_integer_token(token: &str) -> bool {
    if token == "+" || token == "-" {
        return false;
    }

    token.parse::<i64>().is_ok()
}

fn is_token_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | '"' | ';')
}
