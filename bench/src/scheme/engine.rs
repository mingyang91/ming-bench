use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Void,
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(formatter, "{value}"),
            Self::Boolean(true) => formatter.write_str("#t"),
            Self::Boolean(false) => formatter.write_str("#f"),
            Self::String(value) => write!(formatter, "\"{value}\""),
            Self::Symbol(value) => formatter.write_str(value),
            Self::Pair(car, cdr) => {
                formatter.write_str("(")?;
                fmt_pair(car, cdr, formatter)?;
                formatter.write_str(")")
            }
            Self::Nil => formatter.write_str("()"),
            Self::Void => Ok(()),
        }
    }
}

fn fmt_pair(car: &Value, cdr: &Value, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(formatter, "{car}")?;

    match cdr {
        Value::Nil => Ok(()),
        Value::Pair(next_car, next_cdr) => {
            formatter.write_str(" ")?;
            fmt_pair(next_car, next_cdr, formatter)
        }
        value => write!(formatter, " . {value}"),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Literal(Value),
    Symbol(String),
    Application(Vec<Expr>),
}

type Environment = HashMap<String, Value>;

pub(super) fn eval_program(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    let mut environment = Environment::new();

    let mut last_value = Value::Void;
    for expr in &program {
        last_value = eval_expr(expr, &mut environment)?;
    }

    Ok(last_value)
}

fn eval_expr(expr: &Expr, environment: &mut Environment) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(name) => environment
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unbound symbol: {name}")),
        Expr::Application(parts) => eval_application(parts, environment),
    }
}

fn eval_application(parts: &[Expr], environment: &mut Environment) -> Result<Value, String> {
    let (operator, arguments) = parts
        .split_first()
        .ok_or_else(|| "cannot evaluate empty application".to_string())?;
    let operator = match operator {
        Expr::Symbol(name) => name.as_str(),
        _ => return Err("operator must be a symbol".into()),
    };

    match operator {
        "and" => return eval_and(arguments, environment),
        "or" => return eval_or(arguments, environment),
        "if" => return eval_if(arguments, environment),
        "define" => return eval_define(arguments, environment),
        "quote" => return eval_quote(arguments),
        _ => {}
    }

    let mut values = Vec::with_capacity(arguments.len());
    for argument in arguments {
        values.push(eval_expr(argument, environment)?);
    }

    match operator {
        "+" => eval_add(&values),
        "-" => eval_subtract(&values),
        "*" => eval_multiply(&values),
        "/" => eval_divide(&values),
        "<" => eval_less_than(&values),
        ">" => eval_greater_than(&values),
        "=" => eval_equal(&values),
        "<=" => eval_less_equal(&values),
        "not" => eval_not(&values),
        _ => Err(format!("unknown procedure: {operator}")),
    }
}

fn eval_define(arguments: &[Expr], environment: &mut Environment) -> Result<Value, String> {
    let [name, value] = arguments else {
        return Err("`define` expects exactly 2 arguments".into());
    };

    let Expr::Symbol(name) = name else {
        return Err("`define` expects a symbol name".into());
    };

    let value = eval_expr(value, environment)?;
    environment.insert(name.clone(), value);

    Ok(Value::Void)
}

fn eval_if(arguments: &[Expr], environment: &mut Environment) -> Result<Value, String> {
    let [condition, consequent, alternative] = arguments else {
        return Err("`if` expects exactly 3 arguments".into());
    };

    let condition = eval_expr(condition, environment)?;
    if is_truthy(&condition) {
        eval_expr(consequent, environment)
    } else {
        eval_expr(alternative, environment)
    }
}

fn eval_quote(arguments: &[Expr]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`quote` expects exactly 1 argument".into());
    };

    quote_expr(value)
}

fn quote_expr(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::Application(values) => {
            let mut list = Value::Nil;
            for value in values.iter().rev() {
                list = Value::Pair(Box::new(quote_expr(value)?), Box::new(list));
            }
            Ok(list)
        }
    }
}

fn eval_add(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(0_i64, |total, value| {
            let value = expect_integer(value, "+")?;
            total
                .checked_add(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_subtract(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`-` expects at least 1 argument".to_string())?;
    let first = expect_integer(first, "-")?;

    if rest.is_empty() {
        return first
            .checked_neg()
            .map(Value::Integer)
            .ok_or_else(|| "integer overflow".to_string());
    }

    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "-")?;
            total
                .checked_sub(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_multiply(arguments: &[Value]) -> Result<Value, String> {
    arguments
        .iter()
        .try_fold(1_i64, |total, value| {
            let value = expect_integer(value, "*")?;
            total
                .checked_mul(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_divide(arguments: &[Value]) -> Result<Value, String> {
    let (first, rest) = arguments
        .split_first()
        .ok_or_else(|| "`/` expects at least 2 arguments".to_string())?;
    if rest.is_empty() {
        return Err("`/` expects at least 2 arguments".into());
    }

    let first = expect_integer(first, "/")?;
    rest.iter()
        .try_fold(first, |total, value| {
            let value = expect_integer(value, "/")?;
            if value == 0 {
                return Err("division by zero".into());
            }
            total
                .checked_div(value)
                .ok_or_else(|| "integer overflow".to_string())
        })
        .map(Value::Integer)
}

fn eval_less_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<", |left, right| left < right)
}

fn eval_greater_than(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, ">", |left, right| left > right)
}

fn eval_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "=", |left, right| left == right)
}

fn eval_less_equal(arguments: &[Value]) -> Result<Value, String> {
    eval_numeric_comparison(arguments, "<=", |left, right| left <= right)
}

fn eval_numeric_comparison<F>(
    arguments: &[Value],
    operator: &str,
    compare: F,
) -> Result<Value, String>
where
    F: Fn(i64, i64) -> bool,
{
    let mut numbers = arguments.iter();
    let first = numbers
        .next()
        .ok_or_else(|| format!("`{operator}` expects at least 2 arguments"))?;
    let mut previous = expect_integer(first, operator)?;
    let mut saw_pair = false;

    for value in numbers {
        saw_pair = true;
        let current = expect_integer(value, operator)?;
        if !compare(previous, current) {
            return Ok(Value::Boolean(false));
        }
        previous = current;
    }

    if !saw_pair {
        return Err(format!("`{operator}` expects at least 2 arguments"));
    }

    Ok(Value::Boolean(true))
}

fn eval_not(arguments: &[Value]) -> Result<Value, String> {
    let [value] = arguments else {
        return Err("`not` expects exactly 1 argument".into());
    };

    Ok(Value::Boolean(!is_truthy(value)))
}

fn eval_and(arguments: &[Expr], environment: &mut Environment) -> Result<Value, String> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if !is_truthy(&value) {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], environment: &mut Environment) -> Result<Value, String> {
    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if is_truthy(&value) {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn expect_integer(value: &Value, operator: &str) -> Result<i64, String> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(format!("`{operator}` expects integer arguments")),
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

struct Parser<'a> {
    input: &'a str,
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, cursor: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, String> {
        let mut expressions = Vec::new();
        self.skip_whitespace();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if expressions.is_empty() {
            return Err("empty program".into());
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.peek_char() {
            Some('(') => self.parse_application(),
            Some(')') => Err("unexpected `)`".into()),
            Some('"') => self.parse_string().map(Expr::Literal),
            Some('#') => self.parse_boolean().map(Expr::Literal),
            Some('-') if self.peek_next_is_digit() => self.parse_integer().map(Expr::Literal),
            Some('0'..='9') => self.parse_integer().map(Expr::Literal),
            Some(_) => self.parse_symbol().map(Expr::Symbol),
            None => Err("unexpected end of input".into()),
        }
    }

    fn parse_application(&mut self) -> Result<Expr, String> {
        self.bump_char();
        self.skip_whitespace();

        let mut expressions = Vec::new();
        while matches!(self.peek_char(), Some(character) if character != ')') {
            expressions.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if self.peek_char().is_none() {
            return Err("unterminated list".into());
        }

        self.bump_char();
        Ok(Expr::Application(expressions))
    }

    fn parse_symbol(&mut self) -> Result<String, String> {
        let start = self.cursor;
        while matches!(self.peek_char(), Some(character) if !Self::is_delimiter(character)) {
            self.bump_char();
        }

        if start == self.cursor {
            return Err("invalid symbol".into());
        }

        Ok(self.input[start..self.cursor].to_string())
    }

    fn parse_boolean(&mut self) -> Result<Value, String> {
        if self.remaining().starts_with("#t") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(true));
        }

        if self.remaining().starts_with("#f") {
            self.cursor += 2;
            self.ensure_token_boundary("boolean")?;
            return Ok(Value::Boolean(false));
        }

        Err("invalid boolean literal".into())
    }

    fn parse_integer(&mut self) -> Result<Value, String> {
        let start = self.cursor;

        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let digit_start = self.cursor;
        while matches!(self.peek_char(), Some('0'..='9')) {
            self.bump_char();
        }

        if digit_start == self.cursor {
            return Err("invalid integer literal".into());
        }

        let literal = &self.input[start..self.cursor];
        self.ensure_token_boundary("integer")?;

        literal
            .parse::<i64>()
            .map(Value::Integer)
            .map_err(|_| format!("integer literal out of range: {literal}"))
    }

    fn parse_string(&mut self) -> Result<Value, String> {
        self.bump_char();

        let mut contents = String::new();
        loop {
            match self.bump_char() {
                Some('"') => return Ok(Value::String(contents)),
                Some('\\') => contents.push(self.parse_escape_sequence()?),
                Some(character) => contents.push(character),
                None => return Err("unterminated string literal".into()),
            }
        }
    }

    fn parse_escape_sequence(&mut self) -> Result<char, String> {
        match self.bump_char() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('n') => Ok('\n'),
            Some('t') => Ok('\t'),
            Some(character) => Err(format!("unsupported string escape: \\{character}")),
            None => Err("unterminated string literal".into()),
        }
    }

    fn ensure_token_boundary(&self, kind: &str) -> Result<(), String> {
        match self.peek_char() {
            None => Ok(()),
            Some(character) if Self::is_delimiter(character) => Ok(()),
            Some(character) => Err(format!("unexpected `{character}` after {kind} literal")),
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(character) if character.is_whitespace()) {
            self.bump_char();
        }
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.cursor..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_next_is_digit(&self) -> bool {
        self.remaining()
            .chars()
            .nth(1)
            .is_some_and(|character| character.is_ascii_digit())
    }

    fn bump_char(&mut self) -> Option<char> {
        let character = self.peek_char()?;
        self.cursor += character.len_utf8();
        Some(character)
    }

    fn is_eof(&self) -> bool {
        self.cursor == self.input.len()
    }

    fn is_delimiter(character: char) -> bool {
        character.is_whitespace() || matches!(character, '(' | ')')
    }
}
