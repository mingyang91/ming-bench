pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

type EnvRef = Rc<Environment>;
type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

#[derive(Clone)]
enum Token {
    LParen,
    RParen,
    Quote,
    Atom(String),
    String(String),
}

#[derive(Clone)]
enum Expr {
    Number(Number),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone)]
enum Value {
    Number(Number),
    Boolean(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(Rc<Pair>),
    Builtin(Builtin),
    Closure(Rc<Closure>),
    CaseClosure(Rc<CaseClosure>),
    Void,
}

#[derive(Clone, PartialEq)]
enum Number {
    Exact { numerator: i128, denominator: i128 },
    Inexact(f64),
}

impl Number {
    fn exact_integer(value: i128) -> Self {
        Self::Exact {
            numerator: value,
            denominator: 1,
        }
    }

    fn exact_rational(numerator: i128, denominator: i128) -> Result<Self, EvalError> {
        if denominator == 0 {
            return Err(EvalError::message("invalid number"));
        }

        if numerator == 0 {
            return Ok(Self::exact_integer(0));
        }

        let mut numerator = numerator;
        let mut denominator = denominator;
        if denominator < 0 {
            numerator = -numerator;
            denominator = -denominator;
        }

        let divisor = gcd_i128(numerator.abs(), denominator);
        Ok(Self::Exact {
            numerator: numerator / divisor,
            denominator: denominator / divisor,
        })
    }

    fn inexact(value: f64) -> Result<Self, EvalError> {
        if value.is_finite() {
            Ok(Self::Inexact(value))
        } else {
            Err(EvalError::message("invalid number"))
        }
    }

    fn parse(atom: &str) -> Option<Self> {
        if let Some((numerator, denominator)) = parse_rational_atom(atom) {
            return Self::exact_rational(numerator, denominator).ok();
        }

        if let Ok(value) = atom.parse::<i128>() {
            return Some(Self::exact_integer(value));
        }

        if atom.contains('.') {
            return atom
                .parse::<f64>()
                .ok()
                .and_then(|value| Self::inexact(value).ok());
        }

        None
    }

    fn is_exact(&self) -> bool {
        matches!(self, Self::Exact { .. })
    }

    fn is_inexact(&self) -> bool {
        matches!(self, Self::Inexact(_))
    }

    fn is_integer(&self) -> bool {
        match self {
            Self::Exact { denominator, .. } => *denominator == 1,
            Self::Inexact(value) => value.fract() == 0.0,
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            Self::Exact { numerator, .. } => *numerator == 0,
            Self::Inexact(value) => *value == 0.0,
        }
    }

    fn to_f64(&self) -> f64 {
        match self {
            Self::Exact {
                numerator,
                denominator,
            } => *numerator as f64 / *denominator as f64,
            Self::Inexact(value) => *value,
        }
    }

    fn to_exact(&self) -> Result<Self, EvalError> {
        match self {
            Self::Exact { .. } => Ok(self.clone()),
            Self::Inexact(value) => exact_from_inexact(*value),
        }
    }

    fn to_inexact(&self) -> Result<Self, EvalError> {
        Self::inexact(self.to_f64())
    }

    fn numerator(&self) -> Result<Self, EvalError> {
        match self.to_exact()? {
            Self::Exact { numerator, .. } => Ok(Self::exact_integer(numerator)),
            Self::Inexact(_) => unreachable!(),
        }
    }

    fn denominator(&self) -> Result<Self, EvalError> {
        match self.to_exact()? {
            Self::Exact { denominator, .. } => Ok(Self::exact_integer(denominator)),
            Self::Inexact(_) => unreachable!(),
        }
    }

    fn add(&self, other: &Self) -> Result<Self, EvalError> {
        match (self, other) {
            (
                Self::Exact {
                    numerator: left_num,
                    denominator: left_den,
                },
                Self::Exact {
                    numerator: right_num,
                    denominator: right_den,
                },
            ) => Self::exact_rational(
                left_num * right_den + right_num * left_den,
                left_den * right_den,
            ),
            _ => Self::inexact(self.to_f64() + other.to_f64()),
        }
    }

    fn sub(&self, other: &Self) -> Result<Self, EvalError> {
        match (self, other) {
            (
                Self::Exact {
                    numerator: left_num,
                    denominator: left_den,
                },
                Self::Exact {
                    numerator: right_num,
                    denominator: right_den,
                },
            ) => Self::exact_rational(
                left_num * right_den - right_num * left_den,
                left_den * right_den,
            ),
            _ => Self::inexact(self.to_f64() - other.to_f64()),
        }
    }

    fn mul(&self, other: &Self) -> Result<Self, EvalError> {
        match (self, other) {
            (
                Self::Exact {
                    numerator: left_num,
                    denominator: left_den,
                },
                Self::Exact {
                    numerator: right_num,
                    denominator: right_den,
                },
            ) => Self::exact_rational(left_num * right_num, left_den * right_den),
            _ => Self::inexact(self.to_f64() * other.to_f64()),
        }
    }

    fn div(&self, other: &Self) -> Result<Self, EvalError> {
        if other.is_zero() {
            return Err(EvalError::message("division by zero"));
        }

        match (self, other) {
            (
                Self::Exact {
                    numerator: left_num,
                    denominator: left_den,
                },
                Self::Exact {
                    numerator: right_num,
                    denominator: right_den,
                },
            ) => Self::exact_rational(left_num * right_den, left_den * right_num),
            _ => Self::inexact(self.to_f64() / other.to_f64()),
        }
    }

    fn compare(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                Self::Exact {
                    numerator: left_num,
                    denominator: left_den,
                },
                Self::Exact {
                    numerator: right_num,
                    denominator: right_den,
                },
            ) => (left_num * right_den).cmp(&(right_num * left_den)),
            _ => self
                .to_f64()
                .partial_cmp(&other.to_f64())
                .unwrap_or(Ordering::Equal),
        }
    }
}

fn parse_rational_atom(atom: &str) -> Option<(i128, i128)> {
    let (numerator, denominator) = atom.split_once('/')?;
    if numerator.is_empty() || denominator.is_empty() {
        return None;
    }

    Some((numerator.parse().ok()?, denominator.parse().ok()?))
}

fn exact_from_inexact(value: f64) -> Result<Number, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::message("invalid number"));
    }

    let formatted = value.to_string();
    let lower = formatted.to_ascii_lowercase();
    let (mantissa, exponent_part) = lower.split_once('e').unwrap_or((lower.as_str(), "0"));
    let exponent = exponent_part
        .parse::<i32>()
        .map_err(|_| EvalError::message("invalid number"))?;
    let negative = mantissa.starts_with('-');
    let unsigned = mantissa.trim_start_matches(|ch| ch == '+' || ch == '-');
    let (whole, fractional) = unsigned.split_once('.').unwrap_or((unsigned, ""));
    let digits = format!("{whole}{fractional}");
    let mut numerator = digits
        .parse::<i128>()
        .map_err(|_| EvalError::message("invalid number"))?;
    if negative {
        numerator = -numerator;
    }

    let scale = fractional.len() as i32 - exponent;
    if scale <= 0 {
        Number::exact_rational(numerator * 10i128.pow((-scale) as u32), 1)
    } else {
        Number::exact_rational(numerator, 10i128.pow(scale as u32))
    }
}

fn gcd_i128(mut left: i128, mut right: i128) -> i128 {
    while right != 0 {
        let remainder = left % right;
        left = right;
        right = remainder;
    }

    if left == 0 {
        1
    } else {
        left.abs()
    }
}

#[derive(Clone)]
struct Pair {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Copy)]
struct Builtin {
    name: &'static str,
    func: BuiltinFn,
}

#[derive(Clone)]
struct Closure {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct CaseClosure {
    clauses: Vec<Closure>,
}

struct ParameterSpec {
    params: Vec<String>,
    rest_param: Option<String>,
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

    fn set(&self, name: &str, value: Value) -> Result<(), EvalError> {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            return Ok(());
        }

        match &self.parent {
            Some(parent) => parent.set(name, value),
            None => Err(EvalError::message(format!("unbound variable: {name}"))),
        }
    }

    fn lookup(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Ok(value);
        }

        match &self.parent {
            Some(parent) => parent.lookup(name),
            None => Err(EvalError::message(format!("unbound variable: {name}"))),
        }
    }
}

impl Value {
    fn pair(car: Value, cdr: Value) -> Self {
        Self::Pair(Rc::new(Pair { car, cdr }))
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
    let value = evaluate_program(input)?;
    Ok(format_value(&value))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let value = evaluate_program(input)?;
    Ok((format_value(&value), String::new()))
}

fn evaluate_program(input: &str) -> Result<Value, EvalError> {
    let expressions = parse_program(input)?;
    if expressions.is_empty() {
        return Err(EvalError::message("empty input"));
    }

    let env = create_global_env();
    let mut result = Value::Void;

    for expression in &expressions {
        result = evaluate(expression, env.clone())?;
    }

    Ok(result)
}

fn parse_program(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let mut expressions = Vec::new();

    while parser.has_more() {
        expressions.push(parser.parse_expr()?);
    }

    Ok(expressions)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn has_more(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        let token = self
            .next()
            .ok_or_else(|| EvalError::message("unexpected end of input"))?;

        match token {
            Token::LParen => {
                let mut elements = Vec::new();
                loop {
                    match self.peek() {
                        Some(Token::RParen) => {
                            self.index += 1;
                            return Ok(Expr::List(elements));
                        }
                        Some(_) => elements.push(self.parse_expr()?),
                        None => return Err(EvalError::message("missing ')'")),
                    }
                }
            }
            Token::RParen => Err(EvalError::message("unexpected ')'")),
            Token::Quote => Ok(Expr::List(vec![
                Expr::Symbol("quote".into()),
                self.parse_expr()?,
            ])),
            Token::String(value) => Ok(Expr::String(value)),
            Token::Atom(atom) => Ok(parse_atom(atom)),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }
        token
    }
}

fn parse_atom(atom: String) -> Expr {
    match atom.as_str() {
        "#t" => Expr::Boolean(true),
        "#f" => Expr::Boolean(false),
        _ => match Number::parse(&atom) {
            Some(value) => Expr::Number(value),
            None => Expr::Symbol(atom),
        },
    }
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < chars.len() {
        let ch = chars[index];

        if ch.is_whitespace() {
            index += 1;
            continue;
        }

        if ch == ';' {
            while index < chars.len() && chars[index] != '\n' {
                index += 1;
            }
            continue;
        }

        match ch {
            '(' => {
                tokens.push(Token::LParen);
                index += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                index += 1;
            }
            '\'' => {
                tokens.push(Token::Quote);
                index += 1;
            }
            '"' => {
                index += 1;
                let mut value = String::new();
                let mut terminated = false;

                while index < chars.len() {
                    let current = chars[index];
                    if current == '"' {
                        terminated = true;
                        index += 1;
                        break;
                    }

                    if current == '\\' {
                        index += 1;
                        if index >= chars.len() {
                            return Err(EvalError::message("unterminated string literal"));
                        }

                        let escaped = chars[index];
                        value.push(match escaped {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '"' => '"',
                            '\\' => '\\',
                            other => other,
                        });
                        index += 1;
                        continue;
                    }

                    value.push(current);
                    index += 1;
                }

                if !terminated {
                    return Err(EvalError::message("unterminated string literal"));
                }

                tokens.push(Token::String(value));
            }
            _ => {
                let mut atom = String::new();
                while index < chars.len() {
                    let current = chars[index];
                    if current.is_whitespace() || matches!(current, '(' | ')' | '\'' | ';') {
                        break;
                    }

                    atom.push(current);
                    index += 1;
                }

                tokens.push(Token::Atom(atom));
            }
        }
    }

    Ok(tokens)
}

fn evaluate(expr: &Expr, env: EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Number(value) => Ok(Value::Number(value.clone())),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env.lookup(name),
        Expr::List(elements) => evaluate_list(elements, env),
    }
}

fn evaluate_list(elements: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if elements.is_empty() {
        return Err(EvalError::message("cannot evaluate an empty list"));
    }

    if let Expr::Symbol(name) = &elements[0] {
        match name.as_str() {
            "define" => return evaluate_define(&elements[1..], env),
            "set!" => return evaluate_set(&elements[1..], env),
            "if" => return evaluate_if(&elements[1..], env),
            "quote" => return evaluate_quote(&elements[1..]),
            "lambda" => return evaluate_lambda(&elements[1..], env),
            "case-lambda" => return evaluate_case_lambda(&elements[1..], env),
            "and" => return evaluate_and(&elements[1..], env),
            "or" => return evaluate_or(&elements[1..], env),
            "begin" => return evaluate_begin(&elements[1..], env),
            "let" => return evaluate_let(&elements[1..], env),
            "cond" => return evaluate_cond(&elements[1..], env),
            _ => {}
        }
    }

    let procedure = evaluate(&elements[0], env.clone())?;
    let arguments = elements[1..]
        .iter()
        .map(|expr| evaluate(expr, env.clone()))
        .collect::<Result<Vec<_>, _>>()?;
    apply_procedure(procedure, &arguments)
}

fn evaluate_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_at_least("define", args.len(), 2)?;

    match &args[0] {
        Expr::Symbol(name) => {
            require_exact("define", args.len(), 2)?;
            let value = evaluate(&args[1], env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) if !signature.is_empty() => {
            let name = match &signature[0] {
                Expr::Symbol(name) => name.clone(),
                _ => return Err(EvalError::message("define: invalid function name")),
            };

            let ParameterSpec { params, rest_param } =
                parse_parameter_list(&signature[1..], "lambda")?;
            let closure = Value::Closure(Rc::new(Closure {
                params,
                rest_param,
                body: args[1..].to_vec(),
                env: env.clone(),
            }));
            env.define(name, closure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::message("define: invalid binding target")),
    }
}

fn evaluate_set(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_exact("set!", args.len(), 2)?;

    let name = match &args[0] {
        Expr::Symbol(name) => name,
        _ => return Err(EvalError::message("set!: expected a symbol")),
    };

    let value = evaluate(&args[1], env.clone())?;
    env.set(name, value)?;
    Ok(Value::Void)
}

fn evaluate_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_exact("if", args.len(), 3)?;
    if is_truthy(&evaluate(&args[0], env.clone())?) {
        evaluate(&args[1], env)
    } else {
        evaluate(&args[2], env)
    }
}

fn evaluate_quote(args: &[Expr]) -> Result<Value, EvalError> {
    require_exact("quote", args.len(), 1)?;
    Ok(datum_to_value(&args[0]))
}

fn evaluate_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_at_least("lambda", args.len(), 2)?;

    let ParameterSpec { params, rest_param } = parse_formals(&args[0], "lambda")?;

    Ok(Value::Closure(Rc::new(Closure {
        params,
        rest_param,
        body: args[1..].to_vec(),
        env,
    })))
}

fn evaluate_case_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_at_least("case-lambda", args.len(), 1)?;

    let mut clauses = Vec::with_capacity(args.len());
    for clause in args {
        let items = match clause {
            Expr::List(items) if items.len() >= 2 => items,
            _ => return Err(EvalError::message("case-lambda: invalid clause")),
        };

        let ParameterSpec { params, rest_param } = parse_formals(&items[0], "case-lambda")?;
        clauses.push(Closure {
            params,
            rest_param,
            body: items[1..].to_vec(),
            env: env.clone(),
        });
    }

    Ok(Value::CaseClosure(Rc::new(CaseClosure { clauses })))
}

fn evaluate_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = evaluate(arg, env.clone())?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn evaluate_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = evaluate(arg, env.clone())?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn evaluate_begin(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    evaluate_sequence(args, env)
}

fn evaluate_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    require_at_least("let", args.len(), 2)?;

    match &args[0] {
        Expr::List(bindings) => evaluate_ordinary_let(bindings, &args[1..], env),
        Expr::Symbol(name) => {
            require_at_least("let", args.len(), 3)?;
            let bindings = match &args[1] {
                Expr::List(bindings) => bindings,
                _ => return Err(EvalError::message("let: expected a binding list")),
            };
            evaluate_named_let(name, bindings, &args[2..], env)
        }
        _ => Err(EvalError::message("let: invalid form")),
    }
}

fn evaluate_ordinary_let(
    bindings: &[Expr],
    body: &[Expr],
    env: EnvRef,
) -> Result<Value, EvalError> {
    require_at_least("let", body.len(), 1)?;
    let parsed = parse_bindings(bindings, env.clone())?;
    let child = Environment::new(Some(env));

    for (name, value) in parsed {
        child.define(name, value);
    }

    evaluate_sequence(body, child)
}

fn evaluate_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: EnvRef,
) -> Result<Value, EvalError> {
    require_at_least("let", body.len(), 1)?;
    let parsed = parse_bindings(bindings, env.clone())?;
    let params = parsed
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let arguments = parsed
        .into_iter()
        .map(|(_, value)| value)
        .collect::<Vec<_>>();

    let let_env = Environment::new(Some(env));
    let closure = Value::Closure(Rc::new(Closure {
        params,
        rest_param: None,
        body: body.to_vec(),
        env: let_env.clone(),
    }));
    let_env.define(name.to_string(), closure.clone());

    apply_procedure(closure, &arguments)
}

fn evaluate_cond(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let items = match clause {
            Expr::List(items) if !items.is_empty() => items,
            _ => return Err(EvalError::message("cond: invalid clause")),
        };

        if matches!(&items[0], Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::message("cond: else must be last"));
            }
            if items.len() == 1 {
                return Err(EvalError::message("cond: else clause requires a body"));
            }
            return evaluate_sequence(&items[1..], env.clone());
        }

        let test = evaluate(&items[0], env.clone())?;
        if is_truthy(&test) {
            if items.len() == 1 {
                return Ok(test);
            }
            return evaluate_sequence(&items[1..], env);
        }
    }

    Ok(Value::Void)
}

fn parse_bindings(bindings: &[Expr], env: EnvRef) -> Result<Vec<(String, Value)>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let items = match binding {
            Expr::List(items) if items.len() == 2 => items,
            _ => return Err(EvalError::message("let: invalid binding")),
        };

        let name = match &items[0] {
            Expr::Symbol(name) => name.clone(),
            _ => return Err(EvalError::message("let: binding name must be a symbol")),
        };
        let value = evaluate(&items[1], env.clone())?;
        parsed.push((name, value));
    }

    Ok(parsed)
}

fn parse_formals(expr: &Expr, context: &str) -> Result<ParameterSpec, EvalError> {
    match expr {
        Expr::Symbol(name) => Ok(ParameterSpec {
            params: Vec::new(),
            rest_param: Some(name.clone()),
        }),
        Expr::List(params) => parse_parameter_list(params, context),
        _ => Err(EvalError::message(format!(
            "{context}: expected a parameter list"
        ))),
    }
}

fn parse_parameter_list(params: &[Expr], context: &str) -> Result<ParameterSpec, EvalError> {
    let mut names = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name) if name == "." => {
                let rest_param = match params.get(index + 1) {
                    Some(Expr::Symbol(name)) if name != "." && index + 2 == params.len() => {
                        name.clone()
                    }
                    _ => {
                        return Err(EvalError::message(format!(
                            "{context}: invalid rest parameter list"
                        )))
                    }
                };

                return Ok(ParameterSpec {
                    params: names,
                    rest_param: Some(rest_param),
                });
            }
            Expr::Symbol(name) => names.push(name.clone()),
            _ => {
                return Err(EvalError::message(format!(
                    "{context}: parameter names must be symbols"
                )))
            }
        }

        index += 1;
    }

    Ok(ParameterSpec {
        params: names,
        rest_param: None,
    })
}

fn datum_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Number(value) => Value::Number(value.clone()),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => list_value(items.iter().map(datum_to_value)),
    }
}

fn apply_procedure(procedure: Value, args: &[Value]) -> Result<Value, EvalError> {
    match procedure {
        Value::Builtin(builtin) => (builtin.func)(args),
        Value::Closure(closure) => apply_closure(closure.as_ref(), args, "lambda"),
        Value::CaseClosure(case_closure) => apply_case_closure(case_closure.as_ref(), args),
        _ => Err(EvalError::message("attempted to call a non-procedure")),
    }
}

fn apply_case_closure(case_closure: &CaseClosure, args: &[Value]) -> Result<Value, EvalError> {
    for clause in &case_closure.clauses {
        if closure_accepts_arity(clause, args.len()) {
            return apply_closure(clause, args, "case-lambda");
        }
    }

    Err(EvalError::message(format!(
        "case-lambda: no matching clause for {} argument(s)",
        args.len()
    )))
}

fn apply_closure(closure: &Closure, args: &[Value], name: &str) -> Result<Value, EvalError> {
    if closure.rest_param.is_some() {
        require_at_least(name, args.len(), closure.params.len())?;
    } else {
        require_exact(name, args.len(), closure.params.len())?;
    }

    let call_env = Environment::new(Some(closure.env.clone()));
    for (name, value) in closure.params.iter().zip(args.iter()) {
        call_env.define(name.clone(), value.clone());
    }

    if let Some(rest_param) = &closure.rest_param {
        call_env.define(
            rest_param.clone(),
            list_value(args[closure.params.len()..].iter().cloned()),
        );
    }

    evaluate_sequence(&closure.body, call_env)
}

fn closure_accepts_arity(closure: &Closure, arity: usize) -> bool {
    match closure.rest_param {
        Some(_) => arity >= closure.params.len(),
        None => arity == closure.params.len(),
    }
}

fn evaluate_sequence(expressions: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in expressions {
        result = evaluate(expr, env.clone())?;
    }
    Ok(result)
}

fn create_global_env() -> EnvRef {
    let env = Environment::new(None);

    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_sub);
    define_builtin(&env, "*", builtin_mul);
    define_builtin(&env, "/", builtin_div);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_number_equal);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);
    define_builtin(&env, "cons", builtin_cons);
    define_builtin(&env, "car", builtin_car);
    define_builtin(&env, "cdr", builtin_cdr);
    define_builtin(&env, "null?", builtin_is_null);
    define_builtin(&env, "list", builtin_list);
    define_builtin(&env, "length", builtin_length);
    define_builtin(&env, "append", builtin_append);
    define_builtin(&env, "apply", builtin_apply);
    define_builtin(&env, "string?", builtin_is_string);
    define_builtin(&env, "number?", builtin_is_number);
    define_builtin(&env, "integer?", builtin_is_integer);
    define_builtin(&env, "rational?", builtin_is_rational);
    define_builtin(&env, "exact?", builtin_is_exact);
    define_builtin(&env, "inexact?", builtin_is_inexact);
    define_builtin(&env, "exact->inexact", builtin_exact_to_inexact);
    define_builtin(&env, "inexact->exact", builtin_inexact_to_exact);
    define_builtin(&env, "numerator", builtin_numerator);
    define_builtin(&env, "denominator", builtin_denominator);
    define_builtin(&env, "boolean?", builtin_is_boolean);
    define_builtin(&env, "pair?", builtin_is_pair);
    define_builtin(&env, "symbol?", builtin_is_symbol);
    define_builtin(&env, "procedure?", builtin_is_procedure);
    define_builtin(&env, "equal?", builtin_equal);

    env
}

fn define_builtin(env: &EnvRef, name: &'static str, func: BuiltinFn) {
    env.define(name, Value::Builtin(Builtin { name, func }));
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let sum = args
        .iter()
        .try_fold(Number::exact_integer(0), |total, arg| {
            total.add(&expect_number(arg)?)
        })?;
    number_value(sum)
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    require_at_least("-", args.len(), 1)?;
    let first = expect_number(&args[0])?;
    if args.len() == 1 {
        return number_value(Number::exact_integer(0).sub(&first)?);
    }

    let result = args[1..]
        .iter()
        .try_fold(first, |total, arg| total.sub(&expect_number(arg)?))?;
    number_value(result)
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let product = args
        .iter()
        .try_fold(Number::exact_integer(1), |total, arg| {
            total.mul(&expect_number(arg)?)
        })?;
    number_value(product)
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    require_at_least("/", args.len(), 1)?;
    let first = expect_number(&args[0])?;

    if args.len() == 1 {
        return number_value(Number::exact_integer(1).div(&first)?);
    }

    let result = args[1..]
        .iter()
        .try_fold(first, |total, arg| total.div(&expect_number(arg)?))?;
    number_value(result)
}

fn builtin_less_than(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<", args, |ordering| ordering == Ordering::Less)
}

fn builtin_greater_than(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers(">", args, |ordering| ordering == Ordering::Greater)
}

fn builtin_number_equal(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("=", args, |ordering| ordering == Ordering::Equal)
}

fn builtin_less_equal(args: &[Value]) -> Result<Value, EvalError> {
    compare_numbers("<=", args, |ordering| ordering != Ordering::Greater)
}

fn compare_numbers(
    name: &str,
    args: &[Value],
    predicate: impl Fn(Ordering) -> bool,
) -> Result<Value, EvalError> {
    require_at_least(name, args.len(), 1)?;
    let values = args
        .iter()
        .map(expect_number)
        .collect::<Result<Vec<_>, _>>()?;

    for window in values.windows(2) {
        if !predicate(window[0].compare(&window[1])) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("not", args.len(), 1)?;
    Ok(Value::Boolean(!is_truthy(&args[0])))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("cons", args.len(), 2)?;
    Ok(Value::pair(args[0].clone(), args[1].clone()))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("car", args.len(), 1)?;
    let pair = expect_pair(&args[0])?;
    Ok(pair.car.clone())
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("cdr", args.len(), 1)?;
    let pair = expect_pair(&args[0])?;
    Ok(pair.cdr.clone())
}

fn builtin_is_null(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("null?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_value(args.iter().cloned()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("length", args.len(), 1)?;
    let values = list_to_vec(&args[0])?;
    number_value(Number::exact_integer(values.len() as i128))
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Nil);
    }

    let mut prefix = Vec::new();
    for arg in &args[..args.len() - 1] {
        prefix.extend(list_to_vec(arg)?);
    }

    let mut result = args.last().cloned().unwrap_or(Value::Nil);
    for value in prefix.into_iter().rev() {
        result = Value::pair(value, result);
    }
    Ok(result)
}

fn builtin_apply(args: &[Value]) -> Result<Value, EvalError> {
    require_at_least("apply", args.len(), 2)?;

    let procedure = args[0].clone();
    let mut applied_args = args[1..args.len() - 1].to_vec();
    applied_args.extend(list_to_vec(&args[args.len() - 1])?);
    apply_procedure(procedure, &applied_args)
}

fn builtin_is_string(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("string?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::String(_))))
}

fn builtin_is_number(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("number?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Number(_))))
}

fn builtin_is_integer(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("integer?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(
        &args[0],
        Value::Number(number) if number.is_integer()
    )))
}

fn builtin_is_rational(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("rational?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Number(_))))
}

fn builtin_is_exact(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("exact?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(
        &args[0],
        Value::Number(number) if number.is_exact()
    )))
}

fn builtin_is_inexact(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("inexact?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(
        &args[0],
        Value::Number(number) if number.is_inexact()
    )))
}

fn builtin_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("exact->inexact", args.len(), 1)?;
    number_value(expect_number(&args[0])?.to_inexact()?)
}

fn builtin_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("inexact->exact", args.len(), 1)?;
    number_value(expect_number(&args[0])?.to_exact()?)
}

fn builtin_numerator(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("numerator", args.len(), 1)?;
    number_value(expect_number(&args[0])?.numerator()?)
}

fn builtin_denominator(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("denominator", args.len(), 1)?;
    number_value(expect_number(&args[0])?.denominator()?)
}

fn builtin_is_boolean(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("boolean?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn builtin_is_pair(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("pair?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_))))
}

fn builtin_is_symbol(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("symbol?", args.len(), 1)?;
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn builtin_is_procedure(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("procedure?", args.len(), 1)?;
    Ok(Value::Boolean(is_procedure(&args[0])))
}

fn builtin_equal(args: &[Value]) -> Result<Value, EvalError> {
    require_exact("equal?", args.len(), 2)?;
    Ok(Value::Boolean(equal_values(&args[0], &args[1])))
}

fn is_procedure(value: &Value) -> bool {
    matches!(
        value,
        Value::Builtin(_) | Value::Closure(_) | Value::CaseClosure(_)
    )
}

fn equal_values(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => left.compare(right) == Ordering::Equal,
        (Value::Boolean(left), Value::Boolean(right)) => left == right,
        (Value::String(left), Value::String(right)) => left == right,
        (Value::Symbol(left), Value::Symbol(right)) => left == right,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(left), Value::Pair(right)) => {
            equal_values(&left.car, &right.car) && equal_values(&left.cdr, &right.cdr)
        }
        (Value::Builtin(left), Value::Builtin(right)) => left.name == right.name,
        (Value::Closure(left), Value::Closure(right)) => Rc::ptr_eq(left, right),
        (Value::CaseClosure(left), Value::CaseClosure(right)) => Rc::ptr_eq(left, right),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn expect_number(value: &Value) -> Result<Number, EvalError> {
    match value {
        Value::Number(value) => Ok(value.clone()),
        _ => Err(EvalError::message(format!(
            "expected number, got {}",
            type_name(value)
        ))),
    }
}

fn expect_pair(value: &Value) -> Result<Rc<Pair>, EvalError> {
    match value {
        Value::Pair(pair) => Ok(pair.clone()),
        _ => Err(EvalError::message(format!(
            "expected pair, got {}",
            type_name(value)
        ))),
    }
}

fn list_to_vec(value: &Value) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Nil => return Ok(values),
            Value::Pair(pair) => {
                values.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            other => {
                return Err(EvalError::message(format!(
                    "expected list, got {}",
                    type_name(&other)
                )))
            }
        }
    }
}

fn list_value(values: impl IntoIterator<Item = Value>) -> Value {
    let collected = values.into_iter().collect::<Vec<_>>();
    let mut result = Value::Nil;
    for value in collected.into_iter().rev() {
        result = Value::pair(value, result);
    }
    result
}

fn number_value(value: Number) -> Result<Value, EvalError> {
    match value {
        Number::Inexact(value) if !value.is_finite() => Err(EvalError::message("invalid number")),
        other => Ok(Value::Number(other)),
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Boolean(false))
}

fn require_exact(name: &str, actual: usize, expected: usize) -> Result<(), EvalError> {
    if actual == expected {
        Ok(())
    } else {
        Err(EvalError::message(format!(
            "{name}: expected {expected} argument(s), got {actual}"
        )))
    }
}

fn require_at_least(name: &str, actual: usize, minimum: usize) -> Result<(), EvalError> {
    if actual >= minimum {
        Ok(())
    } else {
        Err(EvalError::message(format!(
            "{name}: expected at least {minimum} argument(s), got {actual}"
        )))
    }
}

fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Number(_) => "number",
        Value::Boolean(_) => "boolean",
        Value::String(_) => "string",
        Value::Symbol(_) => "symbol",
        Value::Nil => "null",
        Value::Pair(_) => "pair",
        Value::Builtin(_) | Value::Closure(_) | Value::CaseClosure(_) => "procedure",
        Value::Void => "void",
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Number(number) => format_number(number),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(value) => format_string(value),
        Value::Symbol(name) => name.clone(),
        Value::Nil => "()".into(),
        Value::Pair(_) => format_pair(value),
        Value::Builtin(builtin) => format!("#<procedure:{}>", builtin.name),
        Value::Closure(_) | Value::CaseClosure(_) => "#<procedure>".into(),
        Value::Void => "#<void>".into(),
    }
}

fn format_number(value: &Number) -> String {
    match value {
        Number::Exact {
            numerator,
            denominator,
        } if *denominator == 1 => numerator.to_string(),
        Number::Exact {
            numerator,
            denominator,
        } => format!("{numerator}/{denominator}"),
        Number::Inexact(value) if value.fract() == 0.0 => format!("{value:.1}"),
        Number::Inexact(value) => value.to_string(),
    }
}

fn format_string(value: &str) -> String {
    let mut result = String::with_capacity(value.len() + 2);
    result.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            other => result.push(other),
        }
    }
    result.push('"');
    result
}

fn format_pair(value: &Value) -> String {
    let mut current = value.clone();
    let mut parts = Vec::new();

    loop {
        match current {
            Value::Pair(pair) => {
                parts.push(format_value(&pair.car));
                current = pair.cdr.clone();
            }
            Value::Nil => return format!("({})", parts.join(" ")),
            other => return format!("({} . {})", parts.join(" "), format_value(&other)),
        }
    }
}

#[cfg(test)]
mod tests;
