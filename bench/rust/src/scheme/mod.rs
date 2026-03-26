use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

type NativeFunc = fn(&[Value]) -> Result<Value, EvalError>;
type EnvRef = Rc<Env>;

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    NativeProc {
        name: &'static str,
        func: NativeFunc,
    },
    Closure(Rc<Closure>),
    Void,
}

struct Closure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
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
            Self::Symbol(value) => value.clone(),
            Self::List(items) => {
                let rendered = items
                    .iter()
                    .map(Value::render)
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("({rendered})")
            }
            Self::NativeProc { name, .. } => format!("#<procedure:{name}>"),
            Self::Closure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
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
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::NativeProc { .. } | Self::Closure(_) => "procedure",
            Self::Void => "void",
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

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            bindings: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }
}

impl Closure {
    fn call(&self, args: &[Value]) -> Result<Value, EvalError> {
        if args.len() != self.params.len() {
            return Err(EvalError::WrongArgCount {
                name: "lambda",
                expected: "the declared arity",
                got: args.len(),
            });
        }

        let frame = Env::new(Some(self.env.clone()));
        for (name, value) in self.params.iter().zip(args.iter()) {
            frame.define(name.clone(), value.clone());
        }

        eval_sequence(&self.body, frame)
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

    let env = default_env();
    let last = eval_sequence(&exprs, env)?;
    Ok((last.render(), String::new()))
}

fn default_env() -> EnvRef {
    let env = Env::new(None);
    for (name, func) in [
        ("+", native_add as NativeFunc),
        ("-", native_sub as NativeFunc),
        ("*", native_mul as NativeFunc),
        ("/", native_div as NativeFunc),
        ("<", native_lt as NativeFunc),
        (">", native_gt as NativeFunc),
        ("=", native_num_eq as NativeFunc),
        ("<=", native_lte as NativeFunc),
        ("not", native_not as NativeFunc),
    ] {
        env.define(name.to_string(), Value::NativeProc { name, func });
    }
    env
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

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval(expr, env.clone())?;
    }
    Ok(last)
}

fn eval(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let (head, tail) = items
        .split_first()
        .ok_or_else(|| EvalError::NotAProcedure {
            found: "()".to_string(),
        })?;

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env),
            "if" => return eval_if(tail, env),
            "quote" => return eval_quote(tail),
            "lambda" => return eval_lambda(tail, env),
            "and" => return eval_and(tail, env),
            "or" => return eval_or(tail, env),
            _ => {}
        }
    }

    let procedure = eval(head, env.clone())?;
    let args = eval_args(tail, env)?;
    apply(procedure, &args)
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some(target) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define requires a target".to_string(),
        });
    };

    match target {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2",
                    got: args.len(),
                });
            }

            let value = eval(&args[1], env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let (name_expr, params_exprs) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define requires a binding name".to_string(),
                    })?;

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::InvalidSyntax {
                    message: "function name must be a symbol".to_string(),
                });
            };

            if args.len() < 2 {
                return Err(EvalError::InvalidSyntax {
                    message: "function definition requires a body".to_string(),
                });
            }

            let params = parse_param_slice(params_exprs)?;
            let closure = Value::Closure(Rc::new(Closure {
                params,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidSyntax {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    if eval(&args[0], env.clone())?.is_truthy() {
        eval(&args[1], env)
    } else {
        eval(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    quote_expr(&args[0])
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let params = parse_param_list(&args[0])?;
    Ok(Value::Closure(Rc::new(Closure {
        params,
        body: args[1..].to_vec(),
        env,
    })))
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);
    for expr in args {
        let value = eval(expr, env.clone())?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_args(args: &[Expr], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env.clone())?);
    }
    Ok(values)
}

fn apply(procedure: Value, args: &[Value]) -> Result<Value, EvalError> {
    match procedure {
        Value::NativeProc { func, .. } => func(args),
        Value::Closure(closure) => closure.call(args),
        other => Err(EvalError::NotAProcedure {
            found: other.render(),
        }),
    }
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    match expr {
        Expr::List(items) => parse_param_slice(items),
        _ => Err(EvalError::InvalidSyntax {
            message: "lambda parameters must be a list".to_string(),
        }),
    }
}

fn parse_param_slice(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());
    for item in items {
        let Expr::Symbol(name) = item else {
            return Err(EvalError::InvalidSyntax {
                message: "parameter names must be symbols".to_string(),
            });
        };
        params.push(name.clone());
    }
    Ok(params)
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::List(items) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_expr(item)?);
            }
            Ok(Value::List(values))
        }
    }
}

fn native_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn native_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for value in values_as_numbers("+", args)? {
        sum += value;
    }
    Ok(Value::Integer(sum))
}

fn native_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = values_as_numbers("-", args)?;
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

fn native_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for value in values_as_numbers("*", args)? {
        product *= value;
    }
    Ok(Value::Integer(product))
}

fn native_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = values_as_numbers("/", args)?;
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

fn native_lt(args: &[Value]) -> Result<Value, EvalError> {
    native_compare(args, "<", |left, right| left < right)
}

fn native_gt(args: &[Value]) -> Result<Value, EvalError> {
    native_compare(args, ">", |left, right| left > right)
}

fn native_num_eq(args: &[Value]) -> Result<Value, EvalError> {
    native_compare(args, "=", |left, right| left == right)
}

fn native_lte(args: &[Value]) -> Result<Value, EvalError> {
    native_compare(args, "<=", |left, right| left <= right)
}

fn native_compare<F>(args: &[Value], name: &'static str, cmp: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = values_as_numbers(name, args)?;
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

fn values_as_numbers(name: &'static str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for value in args {
        values.push(value.as_number(name)?);
    }
    Ok(values)
}

#[cfg(test)]
mod tests;
