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

    fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            Self::Int(value) => Ok(*value),
            _ => Err(EvalError::TypeMismatch {
                expected: "number",
                found: self.render(),
            }),
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Bool(true) => "#t".to_string(),
            Self::Bool(false) => "#f".to_string(),
            Self::Int(value) => value.to_string(),
            Self::String(value) => format!("\"{}\"", escape_string(value)),
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
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::EmptyProgram)
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::ParseError {
                message: "unexpected ')'".to_string(),
            }),
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
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.bump_char().ok_or(EvalError::UnexpectedEof)?;
                    let ch = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    value.push(ch);
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof)
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ';' {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(EvalError::ParseError {
                message: "expected expression".to_string(),
            });
        }

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if is_integer_token(token) {
            return token
                .parse::<i64>()
                .map(Expr::Int)
                .map_err(|_| EvalError::ParseError {
                    message: format!("invalid integer literal: {token}"),
                });
        }

        Ok(Expr::Symbol(token.to_string()))
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

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
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

fn eval_program(exprs: &[Expr]) -> Result<Value, EvalError> {
    let mut last = None;
    for expr in exprs {
        last = Some(eval(expr)?);
    }
    last.ok_or(EvalError::EmptyProgram)
}

fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Err(EvalError::UnboundSymbol { name: name.clone() }),
        Expr::List(items) => eval_list(items),
    }
}

fn eval_list(items: &[Expr]) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        });
    };

    match head {
        Expr::Symbol(name) if name == "and" => eval_and(args),
        Expr::Symbol(name) if name == "or" => eval_or(args),
        Expr::Symbol(name) => {
            let values = args.iter().map(eval).collect::<Result<Vec<_>, _>>()?;
            apply_builtin(name, &values)
        }
        _ => Err(EvalError::NotAProcedure {
            found: render_expr(head),
        }),
    }
}

fn eval_and(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expr in args {
        last = eval(expr)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr]) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);
    for expr in args {
        let value = eval(expr)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => apply_add(args),
        "-" => apply_sub(args),
        "*" => apply_mul(args),
        "/" => apply_div(args),
        "<" => apply_comparison("<", args, |left, right| left < right),
        ">" => apply_comparison(">", args, |left, right| left > right),
        "=" => apply_comparison("=", args, |left, right| left == right),
        "<=" => apply_comparison("<=", args, |left, right| left <= right),
        "not" => apply_not(args),
        _ => Err(EvalError::UnboundSymbol {
            name: name.to_string(),
        }),
    }
}

fn apply_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for arg in args {
        sum += arg.as_int()?;
    }
    Ok(Value::Int(sum))
}

fn apply_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(EvalError::WrongArgCount {
            name: "-",
            expected: "at least 1",
            got: 0,
        }),
        [value] => Ok(Value::Int(-value.as_int()?)),
        [first, rest @ ..] => {
            let mut result = first.as_int()?;
            for arg in rest {
                result -= arg.as_int()?;
            }
            Ok(Value::Int(result))
        }
    }
}

fn apply_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for arg in args {
        product *= arg.as_int()?;
    }
    Ok(Value::Int(product))
}

fn apply_div(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "/",
            expected: "at least 2",
            got: 0,
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/",
            expected: "at least 2",
            got: 1,
        });
    }

    let mut result = first.as_int()?;
    for arg in rest {
        let divisor = arg.as_int()?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Int(result))
}

fn apply_comparison(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2",
            got: args.len(),
        });
    }

    let numbers = args
        .iter()
        .map(Value::as_int)
        .collect::<Result<Vec<_>, _>>()?;
    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_match))
}

fn apply_not(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(!value.is_truthy()))
}

fn is_integer_token(token: &str) -> bool {
    let digits = token
        .strip_prefix('+')
        .or_else(|| token.strip_prefix('-'))
        .unwrap_or(token);

    !digits.is_empty() && digits.chars().all(|ch| ch.is_ascii_digit())
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

fn render_expr(expr: &Expr) -> String {
    match expr {
        Expr::Bool(true) => "#t".to_string(),
        Expr::Bool(false) => "#f".to_string(),
        Expr::Int(value) => value.to_string(),
        Expr::String(value) => format!("\"{}\"", escape_string(value)),
        Expr::Symbol(value) => value.clone(),
        Expr::List(items) => {
            let rendered = items.iter().map(render_expr).collect::<Vec<_>>().join(" ");
            format!("({rendered})")
        }
    }
}
