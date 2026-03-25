pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Procedure),
    Void,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin(&'static str),
    Lambda(Rc<Lambda>),
}

#[derive(Debug)]
struct Lambda {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type EnvRef = Rc<RefCell<Env>>;

#[derive(Debug, Default)]
struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(items) if items.is_empty() => "null",
            Self::List(_) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::List(items) => render_list(items),
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

impl Env {
    fn new_root() -> EnvRef {
        Rc::new(RefCell::new(Self::default()))
    }

    fn new_child(parent: &EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(Rc::clone(parent)),
            bindings: HashMap::new(),
        }))
    }
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            Err(EvalError::Syntax {
                message: "empty input".into(),
            })
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => self.parse_quote_shorthand(),
            Some('"') => self.parse_string(),
            Some('#') => self.parse_boolean(),
            Some('+') | Some('-')
                if self
                    .peek_second_char()
                    .is_some_and(|ch| ch.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            Some(ch) if ch.is_ascii_digit() => self.parse_number(),
            Some(_) => self.parse_symbol(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.bump();
        let expr = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), expr]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::List(items));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump();
        let mut value = String::new();

        loop {
            match self.bump() {
                Some('"') => return Ok(Expr::String(value)),
                Some('\\') => {
                    let escaped = match self.bump() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(ch) => ch,
                        None => return Err(EvalError::UnexpectedEof),
                    };
                    value.push(escaped);
                }
                Some(ch) => value.push(ch),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_boolean(&mut self) -> Result<Expr, EvalError> {
        if self.consume_literal("#t") {
            return Ok(Expr::Boolean(true));
        }

        if self.consume_literal("#f") {
            return Ok(Expr::Boolean(false));
        }

        Err(EvalError::Syntax {
            message: "invalid boolean literal".into(),
        })
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        if matches!(self.peek_char(), Some('+') | Some('-')) {
            self.bump();
        }

        let mut saw_digit = false;
        while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump();
        }

        if !saw_digit {
            return Err(EvalError::Syntax {
                message: "invalid number literal".into(),
            });
        }

        let token = &self.input[start..self.offset];
        let value = token.parse::<i64>().map_err(|_| EvalError::Syntax {
            message: format!("invalid number literal: {token}"),
        })?;

        Ok(Expr::Integer(value))
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while matches!(self.peek_char(), Some(ch) if !is_delimiter(ch)) {
            self.bump();
        }

        if start == self.offset {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        Ok(Expr::Symbol(self.input[start..self.offset].to_string()))
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek_char() {
                Some(ch) if ch.is_whitespace() => {
                    self.bump();
                }
                Some(';') => {
                    while let Some(ch) = self.bump() {
                        if ch == '\n' {
                            break;
                        }
                    }
                }
                _ => return,
            }
        }
    }

    fn consume_literal(&mut self, literal: &str) -> bool {
        if !self.remaining().starts_with(literal) {
            return false;
        }

        let end = self.offset + literal.len();
        if self
            .input
            .get(end..)
            .and_then(|rest| rest.chars().next())
            .is_some_and(|ch| !is_delimiter(ch))
        {
            return false;
        }

        self.offset = end;
        true
    }

    fn remaining(&self) -> &'a str {
        &self.input[self.offset..]
    }

    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.remaining().chars();
        chars.next()?;
        chars.next()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
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
    let env = Env::new_root();
    let result = eval_sequence(&exprs, &env)?;
    Ok(result.render())
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = None;

    for expr in exprs {
        last = Some(eval_expr(expr, env)?);
    }

    last.ok_or_else(|| EvalError::Syntax {
        message: "empty input".into(),
    })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => lookup_symbol(env, name),
        Expr::List(items) => eval_list(items, env),
    }
}

fn lookup_symbol(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
    if let Some(value) = env_lookup(env, name) {
        return Ok(value);
    }

    if let Some(name) = builtin_name(name) {
        return Ok(Value::Procedure(Procedure::Builtin(name)));
    }

    Err(EvalError::UnboundVariable {
        name: name.to_string(),
    })
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    let mut current = Some(Rc::clone(env));

    while let Some(scope) = current {
        let (value, parent) = {
            let scope = scope.borrow();
            (
                scope.bindings.get(name).cloned(),
                scope.parent.as_ref().map(Rc::clone),
            )
        };

        if value.is_some() {
            return value;
        }

        current = parent;
    }

    None
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut().bindings.insert(name, value);
}

fn builtin_name(name: &str) -> Option<&'static str> {
    match name {
        "+" => Some("+"),
        "-" => Some("-"),
        "*" => Some("*"),
        "/" => Some("/"),
        "<" => Some("<"),
        ">" => Some(">"),
        "=" => Some("="),
        "<=" => Some("<="),
        "append" => Some("append"),
        "boolean?" => Some("boolean?"),
        "car" => Some("car"),
        "cdr" => Some("cdr"),
        "cons" => Some("cons"),
        "length" => Some("length"),
        "list" => Some("list"),
        "not" => Some("not"),
        "null?" => Some("null?"),
        "number?" => Some("number?"),
        "pair?" => Some("pair?"),
        "string?" => Some("string?"),
        "symbol?" => Some("symbol?"),
        _ => None,
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    }

    if let Expr::Symbol(name) = &items[0] {
        match name.as_str() {
            "begin" => return eval_begin(&items[1..], env),
            "cond" => return eval_cond(&items[1..], env),
            "define" => return eval_define(&items[1..], env),
            "if" => return eval_if(&items[1..], env),
            "let" => return eval_let(&items[1..], env),
            "quote" => return eval_quote(&items[1..]),
            "lambda" => return eval_lambda(None, &items[1..], env),
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            _ => {}
        }
    }

    let operator = eval_expr(&items[0], env)?;
    let args = items[1..]
        .iter()
        .map(|expr| eval_expr(expr, env))
        .collect::<Result<Vec<_>, _>>()?;

    apply(operator, &args)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = eval_expr(value_expr, env)?;
            env_define(env, name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => {
            let (name, params) = parse_define_signature(signature)?;
            let lambda = make_lambda(Some(name.clone()), params, body, env)?;
            env_define(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "invalid define form".into(),
        }),
    }
}

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn parse_define_signature(signature: &[Expr]) -> Result<(String, Vec<String>), EvalError> {
    let (name, params) = signature.split_first().ok_or_else(|| EvalError::Syntax {
        message: "invalid define form".into(),
    })?;

    let name = match name {
        Expr::Symbol(name) => name.clone(),
        _ => {
            return Err(EvalError::Syntax {
                message: "function name must be a symbol".into(),
            });
        }
    };

    let params = parse_params(params)?;
    Ok((name, params))
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| match expr {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::Syntax {
                message: "parameter name must be a symbol".into(),
            }),
        })
        .collect()
}

fn eval_cond(clauses: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::Syntax {
                message: "cond clauses must be lists".into(),
            });
        };

        let (test, body) = items.split_first().ok_or_else(|| EvalError::Syntax {
            message: "cond clause cannot be empty".into(),
        })?;

        if matches!(test, Expr::Symbol(name) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "else clause must be last".into(),
                });
            }
            return eval_sequence(body, env);
        }

        let test_value = eval_expr(test, env)?;
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

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, when_true, when_false] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(when_true, env)
            } else {
                eval_expr(when_false, env)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "exactly 3 arguments".into(),
            got: args.len(),
        }),
    }
}

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (head, rest) = args.split_first().ok_or_else(|| EvalError::Syntax {
        message: "let requires bindings and a body".into(),
    })?;

    match head {
        Expr::List(bindings) => eval_plain_let(bindings, rest, env),
        Expr::Symbol(name) => eval_named_let(name, rest, env),
        _ => Err(EvalError::Syntax {
            message: "invalid let form".into(),
        }),
    }
}

fn eval_plain_let(bindings: &[Expr], body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let requires a body".into(),
        });
    }

    let bindings = eval_bindings(bindings, env)?;
    let let_env = Env::new_child(env);

    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    eval_sequence(body, &let_env)
}

fn eval_named_let(name: &str, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args.split_first().ok_or_else(|| EvalError::Syntax {
        message: "named let requires bindings and a body".into(),
    })?;

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "named let requires a body".into(),
        });
    }

    let bindings = match bindings_expr {
        Expr::List(bindings) => eval_bindings(bindings, env)?,
        _ => {
            return Err(EvalError::Syntax {
                message: "let bindings must be a list".into(),
            });
        }
    };

    let (params, values): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let let_env = Env::new_child(env);
    let lambda = make_lambda(Some(name.to_string()), params, body, &let_env)?;
    env_define(&let_env, name.to_string(), lambda.clone());
    apply(lambda, &values)
}

fn eval_bindings(bindings: &[Expr], env: &EnvRef) -> Result<Vec<(String, Value)>, EvalError> {
    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) => match items.as_slice() {
                [Expr::Symbol(name), value_expr] => Ok((name.clone(), eval_expr(value_expr, env)?)),
                _ => Err(EvalError::Syntax {
                    message: "each let binding must contain a name and value".into(),
                }),
            },
            _ => Err(EvalError::Syntax {
                message: "let bindings must be lists".into(),
            }),
        })
        .collect()
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => quote_expr(expr),
        _ => Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(value) => Ok(Value::Symbol(value.clone())),
        Expr::List(items) => items
            .iter()
            .map(quote_expr)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::List),
    }
}

fn eval_lambda(name: Option<String>, args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let (params_expr, body) = args.split_first().ok_or_else(|| EvalError::Syntax {
        message: "lambda requires parameters and a body".into(),
    })?;

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda requires a body".into(),
        });
    }

    let params = match params_expr {
        Expr::List(params) => parse_params(params)?,
        _ => {
            return Err(EvalError::Syntax {
                message: "lambda parameters must be a list".into(),
            });
        }
    };

    make_lambda(name, params, body, env)
}

fn make_lambda(
    name: Option<String>,
    params: Vec<String>,
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda requires a body".into(),
        });
    }

    Ok(Value::Procedure(Procedure::Lambda(Rc::new(Lambda {
        name,
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    }))))
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

fn apply(operator: Value, args: &[Value]) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(Procedure::Builtin(name)) => apply_builtin(name, args),
        Value::Procedure(Procedure::Lambda(lambda)) => apply_lambda(lambda, args),
        other => Err(EvalError::NotAProcedure {
            found: other.render(),
        }),
    }
}

fn apply_lambda(lambda: Rc<Lambda>, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected: format!("exactly {} arguments", lambda.params.len()),
            got: args.len(),
        });
    }

    let call_env = Env::new_child(&lambda.env);
    for (param, value) in lambda.params.iter().zip(args.iter()) {
        env_define(&call_env, param.clone(), value.clone());
    }

    eval_sequence(&lambda.body, &call_env)
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => add(args),
        "-" => subtract(args),
        "*" => multiply(args),
        "/" => divide(args),
        "<" => compare(name, args, |left, right| left < right),
        ">" => compare(name, args, |left, right| left > right),
        "=" => compare(name, args, |left, right| left == right),
        "<=" => compare(name, args, |left, right| left <= right),
        "append" => append(args),
        "boolean?" => predicate(args, "boolean?", |value| matches!(value, Value::Boolean(_))),
        "car" => car(args),
        "cdr" => cdr(args),
        "cons" => cons(args),
        "length" => length(args),
        "list" => Ok(Value::List(args.to_vec())),
        "not" => builtin_not(args),
        "null?" => predicate(
            args,
            "null?",
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate(args, "number?", |value| matches!(value, Value::Integer(_))),
        "pair?" => predicate(
            args,
            "pair?",
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        "string?" => predicate(args, "string?", |value| matches!(value, Value::String(_))),
        "symbol?" => predicate(args, "symbol?", |value| matches!(value, Value::Symbol(_))),
        _ => unreachable!("unsupported builtin: {name}"),
    }
}

fn add(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("+", args)?;
    Ok(Value::Integer(values.into_iter().sum()))
}

fn subtract(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("-", args)?;
    match values.as_slice() {
        [] => Err(EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn multiply(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("*", args)?;
    Ok(Value::Integer(values.into_iter().product()))
}

fn divide(args: &[Value]) -> Result<Value, EvalError> {
    let values = expect_numbers("/", args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 0,
        })?;

    if rest.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "/".into(),
            expected: "at least 2 arguments".into(),
            got: 1,
        });
    }

    let mut total = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= value;
    }

    Ok(Value::Integer(total))
}

fn compare<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = expect_numbers(name, args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "at least 2 arguments".into(),
            got: values.len(),
        });
    }

    let result = values.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn cons(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [head, Value::List(tail)] => {
            let mut items = Vec::with_capacity(tail.len() + 1);
            items.push(head.clone());
            items.extend(tail.iter().cloned());
            Ok(Value::List(items))
        }
        [_, other] => Err(EvalError::TypeMismatch {
            name: "cons".into(),
            expected: "list".into(),
            found: other.type_name().into(),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }),
    }
}

fn car(args: &[Value]) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("car", args)?;
    Ok(items[0].clone())
}

fn cdr(args: &[Value]) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("cdr", args)?;
    Ok(Value::List(items[1..].to_vec()))
}

fn append(args: &[Value]) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for value in args {
        let list = expect_list("append", value)?;
        items.extend(list.iter().cloned());
    }

    Ok(Value::List(items))
}

fn length(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(expect_list("length", value)?.len() as i64)),
        _ => Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn predicate<F>(args: &[Value], name: &str, test: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(test(value))),
        _ => Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn expect_numbers(name: &str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(EvalError::TypeMismatch {
                name: name.into(),
                expected: "number".into(),
                found: other.type_name().into(),
            }),
        })
        .collect()
}

fn expect_list<'a>(name: &str, value: &'a Value) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "list".into(),
            found: other.type_name().into(),
        }),
    }
}

fn expect_non_empty_list<'a>(name: &str, args: &'a [Value]) -> Result<&'a [Value], EvalError> {
    match args {
        [value] => match value {
            Value::List(items) if !items.is_empty() => Ok(items),
            other => Err(EvalError::TypeMismatch {
                name: name.into(),
                expected: "pair".into(),
                found: other.type_name().into(),
            }),
        },
        _ => Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn render_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');

    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }

    out.push('"');
    out
}

fn render_list(items: &[Value]) -> String {
    let mut out = String::from("(");

    for (index, value) in items.iter().enumerate() {
        if index > 0 {
            out.push(' ');
        }
        out.push_str(&value.render());
    }

    out.push(')');
    out
}

fn is_delimiter(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '(' | ')' | ';')
}

#[cfg(test)]
mod tests;
