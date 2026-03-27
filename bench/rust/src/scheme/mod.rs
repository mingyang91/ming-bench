pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Bool(bool),
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

type EnvRef = Rc<Environment>;
type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

#[derive(Clone)]
enum Value {
    Bool(bool),
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.render())
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
        func: BuiltinFn,
    },
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Builtin { name, .. } => write!(f, "#<builtin:{name}>"),
            Self::Lambda { .. } => f.write_str("#<lambda>"),
        }
    }
}

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

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
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
            Self::Symbol(value) => value.clone(),
            Self::List(items) => {
                let rendered = items
                    .iter()
                    .map(Value::render)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({rendered})")
            }
            Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
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
    let value = eval_program(&exprs, default_env())?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;

fn default_env() -> EnvRef {
    let env = Environment::new(None);
    for (name, func) in [
        ("+", apply_add as BuiltinFn),
        ("-", apply_sub as BuiltinFn),
        ("*", apply_mul as BuiltinFn),
        ("/", apply_div as BuiltinFn),
        ("<", apply_lt as BuiltinFn),
        (">", apply_gt as BuiltinFn),
        ("=", apply_eq as BuiltinFn),
        ("<=", apply_lte as BuiltinFn),
        ("not", apply_not as BuiltinFn),
    ] {
        env.define(
            name,
            Value::Procedure(Rc::new(Procedure::Builtin { name, func })),
        );
    }
    env
}

fn eval_program(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval(expr, env.clone())?;
    }
    Ok(last)
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundSymbol { name: name.clone() }),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        });
    };

    match head {
        Expr::Symbol(name) if name == "and" => eval_and(args, env),
        Expr::Symbol(name) if name == "or" => eval_or(args, env),
        Expr::Symbol(name) if name == "if" => eval_if(args, env),
        Expr::Symbol(name) if name == "quote" => eval_quote(args),
        Expr::Symbol(name) if name == "lambda" => eval_lambda(args, env),
        Expr::Symbol(name) if name == "define" => eval_define(args, env),
        _ => {
            let procedure = eval(head, env.clone())?;
            let values = args
                .iter()
                .map(|expr| eval(expr, env.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            apply_procedure(procedure, &values)
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expr in args {
        last = eval(expr, env.clone())?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);
    for expr in args {
        let value = eval(expr, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = args else {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3",
            got: args.len(),
        });
    };

    if eval(condition, env.clone())?.is_truthy() {
        eval(consequent, env)
    } else {
        eval(alternate, env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = args else {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(quote_expr(quoted))
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 1,
        });
    }

    let params = parse_param_list(params_expr)?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env,
    })))
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = eval(value_expr, env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::ParseError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Some((Expr::Symbol(name), params)) = signature.split_first() else {
                return Err(EvalError::ParseError {
                    message: "define requires a function name".to_string(),
                });
            };

            let params = parse_param_names(params)?;
            env.define(
                name.clone(),
                Value::Procedure(Rc::new(Procedure::Lambda {
                    params,
                    body: body.to_vec(),
                    env: env.clone(),
                })),
            );
            Ok(Value::Void)
        }
        _ => Err(EvalError::ParseError {
            message: "invalid define form".to_string(),
        }),
    }
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    match expr {
        Expr::List(items) => parse_param_names(items),
        _ => Err(EvalError::ParseError {
            message: "lambda parameters must be a list".to_string(),
        }),
    }
}

fn parse_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|expr| match expr {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::ParseError {
                message: "parameter names must be symbols".to_string(),
            }),
        })
        .collect()
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool(value) => Value::Bool(*value),
        Expr::Int(value) => Value::Int(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn apply_procedure(value: Value, args: &[Value]) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = value else {
        return Err(EvalError::NotAProcedure {
            found: value.render(),
        });
    };

    match procedure.as_ref() {
        Procedure::Builtin { func, .. } => func(args),
        Procedure::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::WrongArgCount {
                    name: "lambda",
                    expected: "exact parameter count",
                    got: args.len(),
                });
            }

            let call_env = Environment::new(Some(env.clone()));
            for (name, value) in params.iter().zip(args.iter()) {
                call_env.define(name.clone(), value.clone());
            }

            eval_program(body, call_env)
        }
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

fn apply_lt(args: &[Value]) -> Result<Value, EvalError> {
    apply_comparison("<", args, |left, right| left < right)
}

fn apply_gt(args: &[Value]) -> Result<Value, EvalError> {
    apply_comparison(">", args, |left, right| left > right)
}

fn apply_eq(args: &[Value]) -> Result<Value, EvalError> {
    apply_comparison("=", args, |left, right| left == right)
}

fn apply_lte(args: &[Value]) -> Result<Value, EvalError> {
    apply_comparison("<=", args, |left, right| left <= right)
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
