pub mod error;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn as_integer(&self, name: &str) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            _ => Err(EvalError::ExpectedNumber {
                name: name.to_owned(),
            }),
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_owned(),
            Self::Boolean(false) => "#f".to_owned(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::List(values) => render_list(values),
            Self::Procedure(_) => "#<procedure>".to_owned(),
            Self::Void => "#<void>".to_owned(),
        }
    }
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    Not,
}

#[derive(Clone)]
enum Procedure {
    Builtin(Builtin),
    Lambda(Lambda),
}

#[derive(Clone)]
struct Lambda {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<Environment>;

struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.bindings.borrow_mut().insert(name.into(), value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.lookup(name))
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            _ => rendered.push(ch),
        }
    }

    rendered.push('"');
    rendered
}

fn render_list(values: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, value) in values.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&value.render());
    }

    rendered.push(')');
    rendered
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

        while self.peek_char().is_some() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::EmptyInput)
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::Parse("unexpected ')'".to_owned())),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::Parse("unexpected end of input".to_owned())),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::Parse("unterminated list".to_owned())),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| EvalError::Parse("unterminated string".to_owned()))?;
                    let decoded = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => other,
                    };
                    value.push(decoded);
                }
                other => value.push(other),
            }
        }

        Err(EvalError::Parse("unterminated string".to_owned()))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance_char();
        }

        let atom = &self.input[start..self.pos];

        if atom.is_empty() {
            return Err(EvalError::Parse("expected expression".to_owned()));
        }

        if atom == "#t" {
            return Ok(Expr::Boolean(true));
        }

        if atom == "#f" {
            return Ok(Expr::Boolean(false));
        }

        if let Ok(value) = atom.parse::<i64>() {
            return Ok(Expr::Integer(value));
        }

        Ok(Expr::Symbol(atom.to_owned()))
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.advance_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.advance_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.advance_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::Parse(format!(
                "expected '{expected}', found '{ch}'"
            ))),
            None => Err(EvalError::Parse(format!("expected '{expected}'"))),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }
}

fn root_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::Equal),
        ("<=", Builtin::LessThanOrEqual),
        ("not", Builtin::Not),
    ] {
        env.define(
            name,
            Value::Procedure(Rc::new(Procedure::Builtin(builtin))),
        );
    }

    env
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundSymbol(name.clone())),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or(EvalError::InvalidApplication)?;

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return eval_define(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "lambda" => return eval_lambda(args, env),
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            _ => {}
        }
    }

    let operator = eval_expr(head, env)?;
    apply(operator, args, env)
}

fn apply(operator: Value, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => apply_builtin(*builtin, args, env),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args, env),
        },
        _ => Err(EvalError::InvalidApplication),
    }
}

fn apply_builtin(builtin: Builtin, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let values = eval_args(args, env)?;

    match builtin {
        Builtin::Add => eval_add(&values),
        Builtin::Sub => eval_sub(&values),
        Builtin::Mul => eval_mul(&values),
        Builtin::Div => eval_div(&values),
        Builtin::LessThan => eval_compare("<", &values, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(">", &values, |left, right| left > right),
        Builtin::Equal => eval_compare("=", &values, |left, right| left == right),
        Builtin::LessThanOrEqual => eval_compare("<=", &values, |left, right| left <= right),
        Builtin::Not => eval_not(&values),
    }
}

fn apply_lambda(lambda: &Lambda, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".to_owned()),
            expected: format!("exactly {} arguments", lambda.params.len()),
            got: args.len(),
        });
    }

    let values = eval_args(args, env)?;
    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().cloned().zip(values) {
        call_env.define(param, value);
    }

    eval_sequence(&lambda.body, &call_env)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (target, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("define requires a target".to_owned()))?;

    match target {
        Expr::Symbol(name) => {
            if body.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define".to_owned(),
                    expected: "exactly 2 forms".to_owned(),
                    got: args.len(),
                });
            }

            let value = eval_expr(&body[0], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let (name_expr, params_exprs) = signature
                .split_first()
                .ok_or_else(|| EvalError::Parse("define requires a function name".to_owned()))?;
            let name = expect_symbol(name_expr, "define function name")?;

            if body.is_empty() {
                return Err(EvalError::Parse(
                    "define requires a function body".to_owned(),
                ));
            }

            let params = parse_params(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
                name: Some(name.clone()),
                params,
                body: body.to_vec(),
                env: env.clone(),
            })));

            env.define(name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define target must be a symbol".to_owned())),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if".to_owned(),
            expected: "exactly 3 arguments".to_owned(),
            got: args.len(),
        });
    }

    let condition = eval_expr(&args[0], env)?;

    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| EvalError::Parse("lambda requires parameters".to_owned()))?;

    if body.is_empty() {
        return Err(EvalError::Parse("lambda requires a body".to_owned()));
    }

    let params = match params_expr {
        Expr::List(items) => parse_params(items, "lambda")?,
        _ => {
            return Err(EvalError::Parse(
                "lambda parameters must be a list".to_owned(),
            ));
        }
    };

    Ok(Value::Procedure(Rc::new(Procedure::Lambda(Lambda {
        name: None,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))))
}

fn parse_params(params: &[Expr], form: &str) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| expect_symbol(expr, &format!("{form} parameter")))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::Parse(format!("{context} must be a symbol"))),
    }
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_args(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;

    for value in eval_integer_args("+", args)? {
        sum += value;
    }

    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("-", args)?;

    match values.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".to_owned(),
            expected: "at least 1 argument".to_owned(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-*value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - *value),
        )),
    }
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1_i64;

    for value in eval_integer_args("*", args)? {
        product *= value;
    }

    Ok(Value::Integer(product))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = eval_integer_args("/", args)?;

    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".to_owned(),
            expected: "at least 2 arguments".to_owned(),
            got: 1,
        });
    }

    let mut quotient = *first;

    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        quotient /= *value;
    }

    Ok(Value::Integer(quotient))
}

fn eval_compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = eval_integer_args(name, args)?;
    let result = values
        .windows(2)
        .all(|window| predicate(window[0], window[1]));

    Ok(Value::Boolean(result))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".to_owned(),
            expected: "exactly 1 argument".to_owned(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_integer_args(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(|value| value.as_integer(name)).collect()
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
    let env = root_env();
    let last = eval_sequence(&exprs, &env)?;
    Ok(last.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
