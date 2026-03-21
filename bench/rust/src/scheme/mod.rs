pub mod error;

pub use error::EvalError;

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::Rc,
};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    Ok(eval_program(input)?.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_program(input)?.to_string(), String::new()))
}

#[cfg(test)]
mod tests;

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()?;

    if program.is_empty() {
        return Err(EvalError::Syntax("expected at least one expression".into()));
    }

    let env = Env::global();
    eval_sequence(&program, env)
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
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

#[derive(Debug, Clone)]
enum Value {
    Bool(bool),
    Number(i64),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<LambdaProcedure>),
    Void,
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
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
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
            Self::Symbol(value) => f.write_str(value),
            Self::List(items) => {
                f.write_str("(")?;
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{item}")?;
                }
                f.write_str(")")
            }
            Self::Builtin(_) | Self::Procedure(_) => f.write_str("#<procedure>"),
            Self::Void => f.write_str("#<void>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
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
    const ALL: [Self; 9] = [
        Self::Add,
        Self::Sub,
        Self::Mul,
        Self::Div,
        Self::LessThan,
        Self::GreaterThan,
        Self::Equal,
        Self::LessEqual,
        Self::Not,
    ];

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

type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Env {
    fn global() -> EnvRef {
        let env = Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }));

        {
            let mut bindings = env.borrow_mut();
            for builtin in Builtin::ALL {
                bindings
                    .bindings
                    .insert(builtin.name().to_string(), Value::Builtin(builtin));
            }
        }

        env
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: String, value: Value) {
        env.borrow_mut().bindings.insert(name, value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Option<Value> {
        let (value, parent) = {
            let env_ref = env.borrow();
            (env_ref.bindings.get(name).cloned(), env_ref.parent.clone())
        };

        value.or_else(|| parent.and_then(|parent| Self::lookup(&parent, name)))
    }
}

#[derive(Debug)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

fn eval_sequence(exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval_expr(expr, env.clone())?;
    }
    Ok(last)
}

fn eval_expr(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::Number(value) => Ok(Value::Number(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => {
            Env::lookup(&env, name).ok_or_else(|| EvalError::UnboundSymbol(name.clone()))
        }
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::Syntax("cannot evaluate an empty list".into()));
    };

    match head {
        Expr::Symbol(name) if name == "and" => eval_and(args, env),
        Expr::Symbol(name) if name == "or" => eval_or(args, env),
        Expr::Symbol(name) if name == "if" => eval_if(args, env),
        Expr::Symbol(name) if name == "quote" => eval_quote(args),
        Expr::Symbol(name) if name == "define" => eval_define(args, env),
        Expr::Symbol(name) if name == "lambda" => eval_lambda(args, env),
        _ => {
            let procedure = eval_expr(head, env.clone())?;
            let evaluated = args
                .iter()
                .map(|arg| eval_expr(arg, env.clone()))
                .collect::<Result<Vec<_>, EvalError>>()?;
            apply(procedure, &evaluated)
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for arg in args {
        last = eval_expr(arg, env.clone())?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr(arg, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(wrong_arg_count("if", "exactly 3", args.len()));
    }

    let condition = eval_expr(&args[0], env.clone())?;
    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    }

    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool(value) => Value::Bool(*value),
        Expr::Number(value) => Value::Number(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(wrong_arg_count("define", "at least 2", args.len()));
    }

    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(wrong_arg_count("define", "exactly 2", args.len()));
            }

            let value = eval_expr(&args[1], env.clone())?;
            Env::define(&env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name, params)) = signature.split_first() else {
                return Err(EvalError::Syntax(
                    "define requires a function name".into(),
                ));
            };
            let name = expect_symbol(name, "function name")?;
            let params = parse_parameters(params)?;
            let procedure = Value::Procedure(Rc::new(LambdaProcedure {
                name: Some(name.clone()),
                params,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));

            Env::define(&env, name, procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax(
            "define requires a symbol or function signature".into(),
        )),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    };
    if body.is_empty() {
        return Err(wrong_arg_count("lambda", "at least 2", args.len()));
    }

    let params = parse_parameter_list(params_expr)?;
    Ok(Value::Procedure(Rc::new(LambdaProcedure {
        name: None,
        params,
        body: body.to_vec(),
        env,
    })))
}

fn parse_parameter_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    match expr {
        Expr::List(items) => parse_parameters(items),
        _ => Err(EvalError::Syntax(
            "lambda parameter list must be a list".into(),
        )),
    }
}

fn parse_parameters(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|expr| expect_symbol(expr, "parameter"))
        .collect()
}

fn expect_symbol(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::Syntax(format!("{context} must be a symbol"))),
    }
}

fn apply(function: Value, args: &[Value]) -> Result<Value, EvalError> {
    match function {
        Value::Builtin(builtin) => apply_builtin(builtin, args),
        Value::Procedure(procedure) => apply_lambda(&procedure, args),
        other => Err(EvalError::NotAProcedure(other.to_string())),
    }
}

fn apply_lambda(procedure: &Rc<LambdaProcedure>, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        let expected = format!("exactly {}", procedure.params.len());
        let name = procedure.name.as_deref().unwrap_or("lambda");
        return Err(wrong_arg_count(name, &expected, args.len()));
    }

    let local_env = Env::child(procedure.env.clone());
    for (param, arg) in procedure.params.iter().zip(args.iter()) {
        Env::define(&local_env, param.clone(), arg.clone());
    }

    eval_sequence(&procedure.body, local_env)
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    let name = builtin.name();
    match builtin {
        Builtin::Add => {
            let numbers = expect_numbers(name, args)?;
            let sum = numbers
                .iter()
                .try_fold(0_i64, |acc, value| acc.checked_add(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(sum))
        }
        Builtin::Sub => {
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
        Builtin::Mul => {
            let numbers = expect_numbers(name, args)?;
            let product = numbers
                .iter()
                .try_fold(1_i64, |acc, value| acc.checked_mul(*value))
                .ok_or(EvalError::IntegerOverflow)?;
            Ok(Value::Number(product))
        }
        Builtin::Div => {
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
        Builtin::LessThan => compare_numbers(name, args, |left, right| left < right),
        Builtin::GreaterThan => compare_numbers(name, args, |left, right| left > right),
        Builtin::Equal => compare_numbers(name, args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers(name, args, |left, right| left <= right),
        Builtin::Not => {
            if args.len() != 1 {
                return Err(wrong_arg_count(name, "exactly 1", args.len()));
            }
            Ok(Value::Bool(!args[0].is_truthy()))
        }
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

fn expect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
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
            '\'' => {
                tokens.push(Token::Quote);
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
                    if next.is_whitespace()
                        || next == '('
                        || next == ')'
                        || next == '\''
                        || next == ';'
                    {
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
                        )));
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
            Token::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![
                    Expr::Symbol("quote".to_string()),
                    quoted,
                ]))
            }
            Token::Bool(value) => Ok(Expr::Bool(value)),
            Token::Number(value) => Ok(Expr::Number(value)),
            Token::String(value) => Ok(Expr::String(value)),
            Token::Symbol(name) => Ok(Expr::Symbol(name)),
        }
    }
}
