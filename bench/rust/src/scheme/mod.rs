use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
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
type PairRef = Rc<RefCell<PairCell>>;

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    Nil,
    Pair(PairRef),
    NativeProc {
        name: &'static str,
        func: NativeFunc,
    },
    Closure(Rc<Closure>),
    Void,
}

struct PairCell {
    car: Value,
    cdr: Value,
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
            Self::Nil => "()".to_string(),
            Self::Pair(pair) => render_pair(pair.clone()),
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
            Self::Nil => "null",
            Self::Pair(_) => "pair",
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

    fn as_pair(&self, name: &'static str) -> Result<PairRef, EvalError> {
        match self {
            Self::Pair(pair) => Ok(pair.clone()),
            other => Err(EvalError::ExpectedPair {
                name,
                found: other.render(),
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
            Token::Quote => Ok(Expr::List(vec![
                Expr::Symbol("quote".to_string()),
                self.parse_expr()?,
            ])),
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
        ("cons", native_cons as NativeFunc),
        ("car", native_car as NativeFunc),
        ("cdr", native_cdr as NativeFunc),
        ("null?", native_null_pred as NativeFunc),
        ("list", native_list as NativeFunc),
        ("length", native_length as NativeFunc),
        ("append", native_append as NativeFunc),
        ("string?", native_string_pred as NativeFunc),
        ("number?", native_number_pred as NativeFunc),
        ("boolean?", native_boolean_pred as NativeFunc),
        ("pair?", native_pair_pred as NativeFunc),
        ("symbol?", native_symbol_pred as NativeFunc),
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
            b';' => {
                while index < bytes.len() && bytes[index] != b'\n' {
                    index += 1;
                }
            }
            b'(' => {
                tokens.push(Token::LParen);
                index += 1;
            }
            b')' => {
                tokens.push(Token::RParen);
                index += 1;
            }
            b'\'' => {
                tokens.push(Token::Quote);
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
                while index < bytes.len() && !is_token_boundary(bytes[index]) {
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
        Some(byte) if is_token_boundary(*byte) => true,
        Some(_) => false,
    }
}

fn is_token_boundary(byte: u8) -> bool {
    matches!(
        byte,
        b' ' | b'\n' | b'\r' | b'\t' | b'(' | b')' | b'\'' | b';'
    )
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
            "begin" => return eval_begin(tail, env),
            "cond" => return eval_cond(tail, env),
            "let" => return eval_let(tail, env),
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

fn eval_begin(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidSyntax {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "cond clauses cannot be empty".to_string(),
            })?;

        if matches!(test, Expr::Symbol(name) if name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, env);
        }

        let test_value = eval(test, env.clone())?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match first {
        Expr::Symbol(name) => eval_named_let(name, &args[1..], env),
        bindings => eval_plain_let(bindings, &args[1..], env),
    }
}

fn eval_plain_let(bindings_expr: &Expr, body: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone())?;
    let frame = Env::new(Some(env));

    for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
        frame.define(name, value);
    }

    eval_sequence(body, frame)
}

fn eval_named_let(name: &str, args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_bindings(&args[0])?;
    let values = eval_binding_values(&bindings, env.clone())?;
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let frame = Env::new(Some(env));
    let closure = Value::Closure(Rc::new(Closure {
        params,
        body: args[1..].to_vec(),
        env: frame.clone(),
    }));

    frame.define(name.to_string(), closure.clone());
    apply(closure, &values)
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let Expr::List(items) = binding else {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must be pairs".to_string(),
            });
        };

        if items.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must contain exactly 2 items".to_string(),
            });
        }

        let Expr::Symbol(name) = &items[0] else {
            return Err(EvalError::InvalidSyntax {
                message: "let binding names must be symbols".to_string(),
            });
        };

        parsed.push((name.clone(), items[1].clone()));
    }

    Ok(parsed)
}

fn eval_binding_values(bindings: &[(String, Expr)], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(bindings.len());
    for (_, expr) in bindings {
        values.push(eval(expr, env.clone())?);
    }
    Ok(values)
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
            Ok(list_from_values(values))
        }
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new(PairCell { car, cdr })))
}

fn list_from_values(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |tail, head| make_pair(head, tail))
}

fn list_to_vec(value: &Value, name: &'static str) -> Result<Vec<Value>, EvalError> {
    let mut items = Vec::new();
    let mut cursor = value.clone();

    loop {
        match cursor {
            Value::Nil => return Ok(items),
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                items.push(car);
                cursor = cdr;
            }
            other => {
                return Err(EvalError::ExpectedList {
                    name,
                    found: other.render(),
                });
            }
        }
    }
}

fn render_pair(pair: PairRef) -> String {
    let mut rendered = Vec::new();
    let mut cursor = Value::Pair(pair);

    loop {
        match cursor {
            Value::Pair(pair) => {
                let (car, cdr) = {
                    let borrowed = pair.borrow();
                    (borrowed.car.clone(), borrowed.cdr.clone())
                };
                rendered.push(car.render());
                cursor = cdr;
            }
            Value::Nil => return format!("({})", rendered.join(" ")),
            other => {
                let prefix = rendered.join(" ");
                return format!("({prefix} . {})", other.render());
            }
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

fn native_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "cons",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    Ok(make_pair(args[0].clone(), args[1].clone()))
}

fn native_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "car",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("car")?;
    let car = pair.borrow().car.clone();
    Ok(car)
}

fn native_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "cdr",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let pair = args[0].as_pair("cdr")?;
    let cdr = pair.borrow().cdr.clone();
    Ok(cdr)
}

fn native_null_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("null?", args, |value| matches!(value, Value::Nil))
}

fn native_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(list_from_values(args.to_vec()))
}

fn native_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Integer(list_to_vec(&args[0], "length")?.len() as i64))
}

fn native_append(args: &[Value]) -> Result<Value, EvalError> {
    let Some(last) = args.last().cloned() else {
        return Ok(Value::Nil);
    };

    let mut result = last;
    for list in args[..args.len() - 1].iter().rev() {
        let mut items = list_to_vec(list, "append")?;
        while let Some(item) = items.pop() {
            result = make_pair(item, result);
        }
    }

    Ok(result)
}

fn native_string_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("string?", args, |value| matches!(value, Value::String(_)))
}

fn native_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("number?", args, |value| matches!(value, Value::Integer(_)))
}

fn native_boolean_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_)))
}

fn native_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("pair?", args, |value| matches!(value, Value::Pair(_)))
}

fn native_symbol_pred(args: &[Value]) -> Result<Value, EvalError> {
    native_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_)))
}

fn native_predicate<F>(name: &'static str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
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
