pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let expressions = Parser::new(input).parse_program()?;
    if expressions.is_empty() {
        return Err(EvalError::msg("input is empty"));
    }

    let env = default_env();
    let mut last = Value::Void;
    for expression in &expressions {
        last = eval(expression, &env)?;
    }

    Ok((last, String::new()))
}

type EnvRef = Rc<RefCell<Environment>>;

#[derive(Clone, Debug)]
enum Expr {
    Number(i128),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(i128),
    Bool(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
}

#[derive(Clone, Copy)]
struct BuiltinProcedure {
    name: &'static str,
    func: fn(&[Value]) -> Result<Value, EvalError>,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
    }

    fn define(env: &EnvRef, name: impl Into<String>, value: Value) {
        env.borrow_mut().bindings.insert(name.into(), value);
    }

    fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
        let (value, parent) = {
            let borrowed = env.borrow();
            (borrowed.bindings.get(name).cloned(), borrowed.parent.clone())
        };

        if let Some(value) = value {
            return Ok(value);
        }

        if let Some(parent) = parent {
            return Self::lookup(&parent, name);
        }

        Err(EvalError::msg(format!("unbound variable: {name}")))
    }
}

impl Value {
    fn render(&self) -> String {
        match self {
            Self::Number(number) => number.to_string(),
            Self::Bool(true) => "#t".to_string(),
            Self::Bool(false) => "#f".to_string(),
            Self::String(value) => render_string(value),
            Self::Symbol(name) => name.clone(),
            Self::List(values) => render_list(values),
            Self::Procedure(procedure) => procedure.render(),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn as_number(&self, procedure_name: &str) -> Result<i128, EvalError> {
        match self {
            Self::Number(number) => Ok(*number),
            _ => Err(EvalError::msg(format!(
                "{procedure_name} expects numeric arguments"
            ))),
        }
    }
}

impl Procedure {
    fn render(&self) -> String {
        match self {
            Self::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
            Self::Lambda(lambda) => match &lambda.name {
                Some(name) => format!("#<procedure:{name}>"),
                None => "#<procedure>".to_string(),
            },
        }
    }
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);
    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_subtract);
    define_builtin(&env, "*", builtin_multiply);
    define_builtin(&env, "/", builtin_divide);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_numeric_equals);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);
    env
}

fn define_builtin(
    env: &EnvRef,
    name: &'static str,
    func: fn(&[Value]) -> Result<Value, EvalError>,
) {
    Environment::define(
        env,
        name,
        Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure { name, func }))),
    );
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(number) => Ok(Value::Number(*number)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => Environment::lookup(env, name),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::msg("cannot evaluate empty list"));
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(tail, env),
            "or" => return eval_or(tail, env),
            "define" => return eval_define(tail, env),
            "if" => return eval_if(tail, env),
            "quote" => return eval_quote(tail),
            "lambda" => return eval_lambda(tail, env),
            _ => {}
        }
    }

    let procedure = eval(head, env)?;
    let mut arguments = Vec::with_capacity(tail.len());
    for expression in tail {
        arguments.push(eval(expression, env)?);
    }
    apply(procedure, arguments)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("define requires a target and value"));
    }

    match &args[0] {
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::msg(
                    "define variable form requires exactly one value",
                ));
            }

            let value = eval(&args[1], env)?;
            Environment::define(env, name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name_expr, params)) = signature.split_first() else {
                return Err(EvalError::msg("define function form requires a name"));
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::msg("function name must be a symbol"));
            };

            let lambda = build_lambda(
                params,
                &args[1..],
                env,
                Some(name.clone()),
            )?;
            Environment::define(env, name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::msg(
            "define target must be a symbol or parameter list",
        )),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::msg(
            "if requires condition, then branch, and else branch",
        ));
    }

    if eval(&args[0], env)?.is_truthy() {
        eval(&args[1], env)
    } else {
        eval(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::msg("quote requires exactly one argument"));
    }

    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::msg("lambda requires a parameter list and body"));
    }

    let Expr::List(params) = &args[0] else {
        return Err(EvalError::msg("lambda parameters must be a list"));
    };

    build_lambda(params, &args[1..], env, None)
}

fn build_lambda(
    params: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    name: Option<String>,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::msg("lambda requires at least one body expression"));
    }

    let mut param_names = Vec::with_capacity(params.len());
    for param in params {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::msg("lambda parameters must be symbols"));
        };
        param_names.push(name.clone());
    }

    Ok(Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params: param_names,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))))
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expression in expressions {
        last = eval(expression, env)?;
    }
    Ok(last)
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Number(number) => Value::Number(*number),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_and(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expression in expressions {
        let value = eval(expression, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for expression in expressions {
        let value = eval(expression, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Bool(false))
}

fn apply(procedure: Value, arguments: Vec<Value>) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = procedure else {
        return Err(EvalError::msg("attempted to call a non-procedure"));
    };

    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.func)(&arguments),
        Procedure::Lambda(lambda) => apply_lambda(lambda, arguments),
    }
}

fn apply_lambda(lambda: &LambdaProcedure, arguments: Vec<Value>) -> Result<Value, EvalError> {
    ensure_exactly(
        lambda.name.as_deref().unwrap_or("lambda"),
        arguments.len(),
        lambda.params.len(),
    )?;

    let call_env = Environment::new(Some(Rc::clone(&lambda.env)));
    for (name, value) in lambda.params.iter().zip(arguments.into_iter()) {
        Environment::define(&call_env, name.clone(), value);
    }

    eval_sequence(&lambda.body, &call_env)
}

fn builtin_add(arguments: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i128;
    for argument in arguments {
        total += argument.as_number("+")?;
    }
    Ok(Value::Number(total))
}

fn builtin_subtract(arguments: &[Value]) -> Result<Value, EvalError> {
    ensure_at_least("-", arguments.len(), 1)?;
    let first = arguments[0].as_number("-")?;

    if arguments.len() == 1 {
        return Ok(Value::Number(-first));
    }

    let mut total = first;
    for argument in &arguments[1..] {
        total -= argument.as_number("-")?;
    }
    Ok(Value::Number(total))
}

fn builtin_multiply(arguments: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i128;
    for argument in arguments {
        total *= argument.as_number("*")?;
    }
    Ok(Value::Number(total))
}

fn builtin_divide(arguments: &[Value]) -> Result<Value, EvalError> {
    ensure_at_least("/", arguments.len(), 1)?;

    let mut total = if arguments.len() == 1 {
        1_i128
    } else {
        arguments[0].as_number("/")?
    };

    let divisors = if arguments.len() == 1 {
        arguments
    } else {
        &arguments[1..]
    };

    for argument in divisors {
        let divisor = argument.as_number("/")?;
        if divisor == 0 {
            return Err(EvalError::msg("division by zero"));
        }
        total /= divisor;
    }

    Ok(Value::Number(total))
}

fn builtin_less_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<", |left, right| left < right)
}

fn builtin_greater_than(arguments: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(arguments, ">", |left, right| left > right)
}

fn builtin_numeric_equals(arguments: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(arguments, "=", |left, right| left == right)
}

fn builtin_less_equal(arguments: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(arguments, "<=", |left, right| left <= right)
}

fn builtin_not(arguments: &[Value]) -> Result<Value, EvalError> {
    ensure_exactly("not", arguments.len(), 1)?;
    Ok(Value::Bool(!arguments[0].is_truthy()))
}

fn compare_numbers(
    arguments: &[Value],
    name: &str,
    predicate: fn(i128, i128) -> bool,
) -> Result<Value, EvalError> {
    ensure_at_least(name, arguments.len(), 2)?;
    let mut previous = arguments[0].as_number(name)?;

    for argument in &arguments[1..] {
        let current = argument.as_number(name)?;
        if !predicate(previous, current) {
            return Ok(Value::Bool(false));
        }
        previous = current;
    }

    Ok(Value::Bool(true))
}

fn ensure_exactly(name: &str, actual: usize, expected: usize) -> Result<(), EvalError> {
    if actual == expected {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected {expected} arguments but got {actual}"
        )))
    }
}

fn ensure_at_least(name: &str, actual: usize, minimum: usize) -> Result<(), EvalError> {
    if actual >= minimum {
        Ok(())
    } else {
        Err(EvalError::msg(format!(
            "{name} expected at least {minimum} arguments but got {actual}"
        )))
    }
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

fn render_string(value: &str) -> String {
    let mut rendered = String::from("\"");
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

struct Parser<'a> {
    chars: Vec<char>,
    pos: usize,
    _input: &'a str,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            _input: input,
        }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut expressions = Vec::new();
        self.skip_ignored();
        while !self.is_at_end() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }
        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        match self.peek() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::msg("unexpected ')'")),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::msg("unexpected end of input")),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut values = Vec::new();
        self.skip_ignored();

        while let Some(ch) = self.peek() {
            if ch == ')' {
                self.advance();
                return Ok(Expr::List(values));
            }

            values.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Err(EvalError::msg("unterminated list"))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.advance();
        let mut value = String::new();

        while let Some(ch) = self.advance() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let Some(escaped) = self.advance() else {
                        return Err(EvalError::msg("unterminated string literal"));
                    };
                    value.push(match escaped {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(EvalError::msg("unterminated string literal"))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let mut token = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            token.push(ch);
            self.advance();
        }

        if token == "#t" {
            return Ok(Expr::Bool(true));
        }
        if token == "#f" {
            return Ok(Expr::Bool(false));
        }
        if let Ok(number) = token.parse::<i128>() {
            return Ok(Expr::Number(number));
        }

        Ok(Expr::Symbol(token))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek() {
                Some(ch) if ch.is_whitespace() => {
                    self.advance();
                }
                Some(';') => {
                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

#[cfg(test)]
mod tests;
