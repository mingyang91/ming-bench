#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(value) => write!(formatter, "{value}"),
            Self::Boolean(true) => formatter.write_str("#t"),
            Self::Boolean(false) => formatter.write_str("#f"),
            Self::String(value) => write!(formatter, "\"{value}\""),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Literal(Value),
    Symbol(String),
    Application(Vec<Expr>),
}

pub(super) fn eval_program(input: &str) -> Result<Value, String> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;

    let mut last_value = None;
    for expr in &program {
        last_value = Some(eval_expr(expr)?);
    }

    last_value.ok_or_else(|| "empty program".into())
}

fn eval_expr(expr: &Expr) -> Result<Value, String> {
    match expr {
        Expr::Literal(value) => Ok(value.clone()),
        Expr::Symbol(name) => Err(format!("unbound symbol: {name}")),
        Expr::Application(parts) => eval_application(parts),
    }
}

fn eval_application(parts: &[Expr]) -> Result<Value, String> {
    let (operator, arguments) = parts
        .split_first()
        .ok_or_else(|| "cannot evaluate empty application".to_string())?;
    let operator = match operator {
        Expr::Symbol(name) => name.as_str(),
        _ => return Err("operator must be a symbol".into()),
    };

    let values: Result<Vec<_>, _> = arguments.iter().map(eval_expr).collect();
    let values = values?;

    match operator {
        "+" => eval_add(&values),
        "-" => eval_subtract(&values),
        "*" => eval_multiply(&values),
        "/" => eval_divide(&values),
        _ => Err(format!("unknown procedure: {operator}")),
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

fn expect_integer(value: &Value, operator: &str) -> Result<i64, String> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(format!("`{operator}` expects integer arguments")),
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
