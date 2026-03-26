use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<Closure>),
    Void,
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
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => format!("{value:?}"),
            Self::Symbol(name) => name.clone(),
            Self::List(items) => render_list(items),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

fn render_list(items: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render());
    }

    rendered.push(')');
    rendered
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
    LessEqual,
    Not,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessEqual => "<=",
            Self::Not => "not",
        }
    }
}

struct Closure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<Env>;

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

impl Env {
    fn new() -> EnvRef {
        let env = Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        });

        for (name, builtin) in [
            ("+", Builtin::Add),
            ("-", Builtin::Sub),
            ("*", Builtin::Mul),
            ("/", Builtin::Div),
            ("<", Builtin::LessThan),
            (">", Builtin::GreaterThan),
            ("=", Builtin::Equal),
            ("<=", Builtin::LessEqual),
            ("not", Builtin::Not),
        ] {
            env.define(name.into(), Value::Builtin(builtin));
        }

        env
    }

    fn child(parent: &EnvRef) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent.clone()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
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
    let env = Env::new();
    eval_sequence(expressions, &env)
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Void;

    for expression in expressions {
        last_value = eval_expr(expression, env)?;
    }

    Ok(last_value)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_application(items, env),
    }
}

fn eval_application(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (operator, arguments) = items.split_first().ok_or(EvalError::EmptyList)?;

    if let Expr::Symbol(name) = operator {
        match name.as_str() {
            "define" => return eval_define(arguments, env),
            "if" => return eval_if(arguments, env),
            "quote" => return eval_quote(arguments),
            "lambda" => return eval_lambda(arguments, env),
            "and" => return eval_and(arguments, env),
            "or" => return eval_or(arguments, env),
            _ => {}
        }
    }

    let procedure = eval_expr(operator, env)?;
    apply_value(procedure, arguments, env)
}

fn apply_value(value: Value, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match value {
        Value::Builtin(builtin) => eval_builtin(builtin, arguments, env),
        Value::Procedure(closure) => apply_closure(closure, arguments, env),
        other => Err(EvalError::NotAProcedure {
            found: other.kind().into(),
        }),
    }
}

fn eval_builtin(builtin: Builtin, arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(arguments, env),
        Builtin::Sub => eval_sub(arguments, env),
        Builtin::Mul => eval_mul(arguments, env),
        Builtin::Div => eval_div(arguments, env),
        Builtin::LessThan => {
            eval_compare(builtin.name(), arguments, env, |left, right| left < right)
        }
        Builtin::GreaterThan => {
            eval_compare(builtin.name(), arguments, env, |left, right| left > right)
        }
        Builtin::Equal => eval_compare(builtin.name(), arguments, env, |left, right| left == right),
        Builtin::LessEqual => {
            eval_compare(builtin.name(), arguments, env, |left, right| left <= right)
        }
        Builtin::Not => eval_not(arguments, env),
    }
}

fn apply_closure(
    closure: Rc<Closure>,
    arguments: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    if arguments.len() != closure.params.len() {
        return Err(EvalError::WrongArgCount {
            name: "procedure".into(),
            expected: format!("exactly {}", closure.params.len()),
            got: arguments.len(),
        });
    }

    let argument_values = eval_args(arguments, env)?;
    let call_env = Env::child(&closure.env);

    for (param, value) in closure.params.iter().cloned().zip(argument_values) {
        call_env.define(param, value);
    }

    eval_sequence(&closure.body, &call_env)
}

fn eval_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_expr(argument, env))
        .collect()
}

fn eval_define(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if arguments.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: "define".into(),
            expected: "at least 2".into(),
            got: arguments.len(),
        });
    }

    match &arguments[0] {
        Expr::Symbol(name) => {
            if arguments.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define".into(),
                    expected: "exactly 2".into(),
                    got: arguments.len(),
                });
            }

            let value = eval_expr(&arguments[1], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let (name_expr, params) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define: expected function name".into(),
                    })?;

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::InvalidSyntax {
                    message: "define: expected function name".into(),
                });
            };

            let closure = Value::Procedure(Rc::new(Closure {
                params: parse_params(params)?,
                body: arguments[1..].to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define: expected symbol or function signature".into(),
        }),
    }
}

fn eval_if(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "exactly 3".into(),
            got: arguments.len(),
        });
    };

    if eval_expr(condition, env)?.is_truthy() {
        eval_expr(consequent, env)
    } else {
        eval_expr(alternate, env)
    }
}

fn eval_quote(arguments: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = arguments else {
        return Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        });
    };

    Ok(quote_expr(quoted))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_lambda(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (param_list, body) = arguments.split_first().ok_or(EvalError::WrongArgCount {
        name: "lambda".into(),
        expected: "at least 2".into(),
        got: 0,
    })?;

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: 1,
        });
    }

    Ok(Value::Procedure(Rc::new(Closure {
        params: parse_param_list(param_list)?,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::InvalidSyntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_params(params)
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| match param {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::InvalidSyntax {
                message: "lambda: parameters must be symbols".into(),
            }),
        })
        .collect()
}

fn eval_add(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn eval_sub(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

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

fn eval_mul(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;
    Ok(Value::Integer(
        numbers.into_iter().fold(1_i64, |acc, value| acc * value),
    ))
}

fn eval_div(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

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
    env: &EnvRef,
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = eval_number_args(arguments, env)?;

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

fn eval_not(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if arguments.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: arguments.len(),
        });
    }

    let value = eval_expr(&arguments[0], env)?;
    Ok(Value::Boolean(!value.is_truthy()))
}

fn eval_and(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_number_args(arguments: &[Expr], env: &EnvRef) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_number(argument, env))
        .collect()
}

fn eval_number(expr: &Expr, env: &EnvRef) -> Result<i64, EvalError> {
    match eval_expr(expr, env)? {
        Value::Integer(value) => Ok(value),
        other => Err(EvalError::TypeMismatch {
            expected: "number".into(),
            found: other.kind().into(),
        }),
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
