pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    EmptyList,
    Pair(Box<Value>, Box<Value>),
    Procedure(Rc<Procedure>),
    Void,
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
            Self::EmptyList | Self::Pair(_, _) => "pair",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::EmptyList => "()".to_string(),
            Self::Pair(_, _) => render_pair(self),
            Self::Procedure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }
}

fn render_pair(value: &Value) -> String {
    let mut out = String::from("(");
    let mut cursor = value;

    loop {
        let Value::Pair(car, cdr) = cursor else {
            break;
        };

        out.push_str(&car.render());

        match cdr.as_ref() {
            Value::EmptyList => break,
            Value::Pair(_, _) => {
                out.push(' ');
                cursor = cdr.as_ref();
            }
            other => {
                out.push_str(" . ");
                out.push_str(&other.render());
                break;
            }
        }
    }

    out.push(')');
    out
}

fn render_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');

    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }

    out.push('"');
    out
}

type EnvRef = Rc<RefCell<Env>>;
type BuiltinFn = fn(&[Value]) -> Result<Value, EvalError>;

struct Env {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Env {
    fn new() -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }))
    }

    fn child(parent: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }
}

#[derive(Clone)]
enum Procedure {
    Builtin(BuiltinProcedure),
    Lambda(LambdaProcedure),
}

#[derive(Clone)]
struct BuiltinProcedure {
    implementation: BuiltinFn,
}

#[derive(Clone)]
struct LambdaProcedure {
    name: String,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Parser<'a> {
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, index: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_ignored();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected at least one expression".to_string(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('"') => self.parse_string(),
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
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut out = String::new();

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(out)),
                Some('\\') => match self.bump_char() {
                    Some('"') => out.push('"'),
                    Some('\\') => out.push('\\'),
                    Some('n') => out.push('\n'),
                    Some('r') => out.push('\r'),
                    Some('t') => out.push('\t'),
                    Some(other) => out.push(other),
                    None => return Err(EvalError::UnterminatedString),
                },
                Some(ch) => out.push(ch),
                None => return Err(EvalError::UnterminatedString),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let token =
            self.take_while(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';');

        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "unexpected token".to_string(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => {
                if let Some(value) = parse_integer_token(token) {
                    Ok(Expr::Integer(value))
                } else {
                    Ok(Expr::Symbol(token.to_string()))
                }
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            self.take_while(|ch| ch.is_whitespace());

            if self.peek_char() == Some(';') {
                self.take_while(|ch| ch != '\n');
                continue;
            }

            break;
        }
    }

    fn take_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
        let start = self.index;

        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.bump_char();
        }

        &self.input[start..self.index]
    }

    fn is_eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.index..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

fn parse_integer_token(token: &str) -> Option<i64> {
    let digits = match token.bytes().next() {
        Some(b'+') | Some(b'-') => &token[1..],
        _ => token,
    };

    if digits.is_empty() || !digits.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }

    token.parse().ok()
}

fn default_env() -> EnvRef {
    let env = Env::new();

    define_builtin(&env, "+", builtin_add);
    define_builtin(&env, "-", builtin_sub);
    define_builtin(&env, "*", builtin_mul);
    define_builtin(&env, "/", builtin_div);
    define_builtin(&env, "<", builtin_less_than);
    define_builtin(&env, ">", builtin_greater_than);
    define_builtin(&env, "=", builtin_equal_numbers);
    define_builtin(&env, "<=", builtin_less_equal);
    define_builtin(&env, "not", builtin_not);

    env
}

fn define_builtin(env: &EnvRef, name: &'static str, implementation: BuiltinFn) {
    let value = Value::Procedure(Rc::new(Procedure::Builtin(BuiltinProcedure {
        implementation,
    })));
    env.borrow_mut().bindings.insert(name.to_string(), value);
}

fn lookup(env: &EnvRef, name: &str) -> Result<Value, EvalError> {
    let (value, parent) = {
        let scope = env.borrow();
        (scope.bindings.get(name).cloned(), scope.parent.clone())
    };

    if let Some(value) = value {
        return Ok(value);
    }

    if let Some(parent) = parent {
        return lookup(&parent, name);
    }

    Err(EvalError::UnboundVariable {
        name: name.to_string(),
    })
}

fn eval_program(exprs: &[Expr]) -> Result<Value, EvalError> {
    let env = default_env();
    eval_sequence(exprs, env)
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
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => lookup(&env, name),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate an empty list".to_string(),
        });
    }

    if let Expr::Symbol(operator) = &items[0] {
        let args = &items[1..];

        match operator.as_str() {
            "and" => return eval_and(args, env),
            "or" => return eval_or(args, env),
            "if" => return eval_if(args, env),
            "quote" => return eval_quote(args),
            "define" => return eval_define(args, env),
            "lambda" => return eval_lambda(args, env),
            _ => {}
        }
    }

    let operator = eval_expr(&items[0], env.clone())?;
    apply(operator, &items[1..], env)
}

fn eval_and(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for expr in args {
        let value = eval_expr(expr, env.clone())?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval_expr(expr, env.clone())?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_if(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    expect_expr_arity("if", args, 3)?;

    let condition = eval_expr(&args[0], env.clone())?;
    if condition.is_truthy() {
        eval_expr(&args[1], env)
    } else {
        eval_expr(&args[2], env)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    expect_expr_arity("quote", args, 1)?;
    Ok(quote_expr(&args[0]))
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(name) => Value::Symbol(name.clone()),
        Expr::List(items) => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::Pair(Box::new(quote_expr(item)), Box::new(tail))
        }),
    }
}

fn eval_define(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "define requires a binding target".to_string(),
        });
    }

    match &args[0] {
        Expr::Symbol(name) => {
            expect_expr_arity("define", args, 2)?;
            let value = eval_expr(&args[1], env.clone())?;
            env.borrow_mut().bindings.insert(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            if signature.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function name".to_string(),
                });
            }

            if args.len() < 2 {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Expr::Symbol(name) = &signature[0] else {
                return Err(EvalError::SyntaxError {
                    message: "define function name must be a symbol".to_string(),
                });
            };

            let params = parse_param_slice(&signature[1..])?;
            let lambda = make_lambda(name.clone(), params, args[1..].to_vec(), env.clone());
            env.borrow_mut().bindings.insert(name.clone(), lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::SyntaxError {
            message: "define requires a symbol or function signature".to_string(),
        }),
    }
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::SyntaxError {
            message: "lambda requires parameters and a body".to_string(),
        });
    }

    let params = parse_param_list(&args[0])?;
    Ok(make_lambda(
        "lambda".to_string(),
        params,
        args[1..].to_vec(),
        env,
    ))
}

fn parse_param_list(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(params) = expr else {
        return Err(EvalError::SyntaxError {
            message: "lambda parameters must be a list".to_string(),
        });
    };

    parse_param_slice(params)
}

fn parse_param_slice(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut names = Vec::with_capacity(params.len());

    for param in params {
        let Expr::Symbol(name) = param else {
            return Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".to_string(),
            });
        };
        names.push(name.clone());
    }

    Ok(names)
}

fn make_lambda(name: String, params: Vec<String>, body: Vec<Expr>, env: EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure::Lambda(LambdaProcedure {
        name,
        params,
        body,
        env,
    })))
}

fn apply(operator: Value, arg_exprs: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Value::Procedure(procedure) = operator else {
        return Err(EvalError::NotAProcedure);
    };

    let args = eval_args(arg_exprs, env)?;

    match procedure.as_ref() {
        Procedure::Builtin(builtin) => (builtin.implementation)(&args),
        Procedure::Lambda(lambda) => apply_lambda(lambda, args),
    }
}

fn eval_args(arg_exprs: &[Expr], env: EnvRef) -> Result<Vec<Value>, EvalError> {
    let mut args = Vec::with_capacity(arg_exprs.len());

    for expr in arg_exprs {
        args.push(eval_expr(expr, env.clone())?);
    }

    Ok(args)
}

fn apply_lambda(lambda: &LambdaProcedure, args: Vec<Value>) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgumentCount {
            name: lambda.name.clone(),
            expected: lambda.params.len().to_string(),
            got: args.len(),
        });
    }

    let call_env = Env::child(lambda.env.clone());
    {
        let mut scope = call_env.borrow_mut();
        for (param, arg) in lambda.params.iter().cloned().zip(args) {
            scope.bindings.insert(param, arg);
        }
    }

    eval_sequence(&lambda.body, call_env)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    expect_value_arity("not", args, 1)?;
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    Ok(Value::Integer(numbers.into_iter().sum()))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;
    Ok(Value::Integer(numbers.into_iter().product()))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] => Err(EvalError::WrongArgumentCount {
            name: "-".to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        }),
        [value] => Ok(Value::Integer(-(*value))),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - *value),
        )),
    }
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::WrongArgumentCount {
            name: "/".to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        }),
        [first, rest @ ..] => {
            let mut result = *first;
            for divisor in rest {
                if *divisor == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= *divisor;
            }
            Ok(Value::Integer(result))
        }
    }
}

fn builtin_less_than(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("<", args, |left, right| left < right)
}

fn builtin_greater_than(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare(">", args, |left, right| left > right)
}

fn builtin_equal_numbers(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("=", args, |left, right| left == right)
}

fn builtin_less_equal(args: &[Value]) -> Result<Value, EvalError> {
    builtin_compare("<=", args, |left, right| left <= right)
}

fn builtin_compare(
    name: &str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(args)?;

    if numbers.len() < 2 {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: "at least 2".to_string(),
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(is_match))
}

fn expect_numbers(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    let mut values = Vec::with_capacity(args.len());

    for value in args {
        match value {
            Value::Integer(number) => values.push(*number),
            other => {
                return Err(EvalError::TypeMismatch {
                    expected: "number",
                    actual: other.type_name(),
                });
            }
        }
    }

    Ok(values)
}

fn expect_expr_arity(name: &str, args: &[Expr], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
}

fn expect_value_arity(name: &str, args: &[Value], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::WrongArgumentCount {
            name: name.to_string(),
            expected: expected.to_string(),
            got: args.len(),
        });
    }

    Ok(())
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
