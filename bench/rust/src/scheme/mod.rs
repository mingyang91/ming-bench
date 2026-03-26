use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod error;

pub use error::EvalError;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Copy, Debug)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Less,
    Greater,
    Equal,
    LessEqual,
    Not,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Mul => "*",
            Builtin::Div => "/",
            Builtin::Less => "<",
            Builtin::Greater => ">",
            Builtin::Equal => "=",
            Builtin::LessEqual => "<=",
            Builtin::Not => "not",
        }
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
            Value::Builtin(_) | Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn render(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(value) => format!("\"{}\"", escape_string(value)),
            Value::Symbol(value) => value.clone(),
            Value::List(items) => render_list(items),
            Value::Builtin(_) | Value::Procedure(_) => "#<procedure>".into(),
            Value::Void => String::new(),
        }
    }
}

type EnvRef = Rc<Env>;

struct Env {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
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

struct Procedure {
    name: Option<String>,
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();

        loop {
            self.skip_ws_and_comments();
            if self.peek_char().is_none() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }

        if exprs.is_empty() {
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ws_and_comments();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some(')') => Err(EvalError::Syntax {
                message: "unexpected ')'".into(),
            }),
            Some('"') => self.parse_string(),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ws_and_comments();
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

        loop {
            match self.bump_char() {
                Some('"') => return Ok(Expr::String(value)),
                Some('\\') => match self.bump_char() {
                    Some('"') => value.push('"'),
                    Some('\\') => value.push('\\'),
                    Some('n') => value.push('\n'),
                    Some('r') => value.push('\r'),
                    Some('t') => value.push('\t'),
                    Some(other) => value.push(other),
                    None => return Err(EvalError::UnexpectedEof),
                },
                Some(ch) => value.push(ch),
                None => return Err(EvalError::UnexpectedEof),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(EvalError::Syntax {
                message: "expected expression".into(),
            });
        }

        match token {
            "#t" => Ok(Expr::Boolean(true)),
            "#f" => Ok(Expr::Boolean(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Integer(value)),
                Err(_) => Ok(Expr::Symbol(token.into())),
            },
        }
    }

    fn skip_ws_and_comments(&mut self) {
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
    let value = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let value = eval_program(input)?;
    Ok((value.render(), String::new()))
}

fn eval_program(input: &str) -> Result<Value, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = initial_env();
    eval_sequence(&exprs, &env)
}

fn initial_env() -> EnvRef {
    let env = Env::new(None);

    for builtin in [
        Builtin::Add,
        Builtin::Sub,
        Builtin::Mul,
        Builtin::Div,
        Builtin::Less,
        Builtin::Greater,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::Not,
    ] {
        env.define(builtin.name().into(), Value::Builtin(builtin));
    }

    env
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval(expr, env)?;
    }

    Ok(result)
}

fn eval(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
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

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env),
            "if" => return eval_if(tail, env),
            "quote" => return eval_quote(tail),
            "lambda" => return build_lambda(tail, env, None),
            "and" => return eval_and(tail, env),
            "or" => return eval_or(tail, env),
            _ => {}
        }
    }

    let callable = eval(head, env)?;
    let args = eval_args(tail, env)?;
    apply(callable, &args)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name), value_expr] => {
            let value = if let Some(parts) = lambda_parts(value_expr) {
                build_lambda(parts, env, Some(name.clone()))?
            } else {
                eval(value_expr, env)?
            };
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => {
            let Some((Expr::Symbol(name), params)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define: expected function name".into(),
                });
            };
            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "define: expected function body".into(),
                });
            }

            let value = new_procedure(
                Some(name.clone()),
                parse_param_list(params)?,
                body,
                env,
            );
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch, else_branch] => {
            if eval(condition, env)?.is_truthy() {
                eval(then_branch, env)
            } else {
                eval(else_branch, env)
            }
        }
        _ => Err(wrong_arg_count("if", "3", args.len())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote_expr(expr)),
        _ => Err(wrong_arg_count("quote", "1", args.len())),
    }
}

fn eval_args(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval(expr, env)).collect()
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg, env)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn build_lambda(parts: &[Expr], env: &EnvRef, name: Option<String>) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = parts else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameters and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda: expected body".into(),
        });
    }

    Ok(new_procedure(
        name,
        parse_params_expr(params_expr)?,
        body,
        env,
    ))
}

fn new_procedure(name: Option<String>, params: Vec<String>, body: &[Expr], env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        name,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))
}

fn parse_params_expr(params: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(items) = params else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_param_list(items)
}

fn parse_param_list(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut names = Vec::with_capacity(params.len());

    for param in params {
        match param {
            Expr::Symbol(name) => names.push(name.clone()),
            _ => {
                return Err(EvalError::Syntax {
                    message: "lambda: expected parameter name".into(),
                });
            }
        }
    }

    Ok(names)
}

fn lambda_parts(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items) = expr else {
        return None;
    };
    let (Expr::Symbol(name), tail) = items.split_first()? else {
        return None;
    };

    if name == "lambda" {
        Some(tail)
    } else {
        None
    }
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

fn apply(callable: Value, args: &[Value]) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args),
        Value::Procedure(procedure) => apply_procedure(&procedure, args),
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_procedure(procedure: &Procedure, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        let name = procedure.name.as_deref().unwrap_or("lambda");
        return Err(wrong_arg_count(
            name,
            &procedure.params.len().to_string(),
            args.len(),
        ));
    }

    let call_env = Env::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }

    eval_sequence(&procedure.body, &call_env)
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => builtin_add(args),
        Builtin::Sub => builtin_sub(args),
        Builtin::Mul => builtin_mul(args),
        Builtin::Div => builtin_div(args),
        Builtin::Less => compare_numbers("<", args, |left, right| left < right),
        Builtin::Greater => compare_numbers(">", args, |left, right| left > right),
        Builtin::Equal => compare_numbers("=", args, |left, right| left == right),
        Builtin::LessEqual => compare_numbers("<=", args, |left, right| left <= right),
        Builtin::Not => builtin_not(args),
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0;
    for arg in args {
        total += expect_number("+", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [] => Err(wrong_arg_count("-", "at least 1", 0)),
        [arg] => Ok(Value::Integer(-expect_number("-", arg)?)),
        [first, rest @ ..] => {
            let mut total = expect_number("-", first)?;
            for arg in rest {
                total -= expect_number("-", arg)?;
            }
            Ok(Value::Integer(total))
        }
    }
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1;
    for arg in args {
        total *= expect_number("*", arg)?;
    }
    Ok(Value::Integer(total))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest @ ..] = args else {
        return Err(wrong_arg_count("/", "at least 2", 0));
    };

    if rest.is_empty() {
        return Err(wrong_arg_count("/", "at least 2", 1));
    }

    let mut total = expect_number("/", first)?;
    for arg in rest {
        let divisor = expect_number("/", arg)?;
        if divisor == 0 {
            return Err(EvalError::DivisionByZero);
        }
        total /= divisor;
    }

    Ok(Value::Integer(total))
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count("not", "1", args.len())),
    }
}

fn compare_numbers<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    if args.len() < 2 {
        return Err(wrong_arg_count(name, "at least 2", args.len()));
    }

    let mut iter = args.iter();
    let mut left = expect_number(name, iter.next().expect("len checked"))?;

    for arg in iter {
        let right = expect_number(name, arg)?;
        if !predicate(left, right) {
            return Ok(Value::Boolean(false));
        }
        left = right;
    }

    Ok(Value::Boolean(true))
}

fn expect_number(name: &str, value: &Value) -> Result<i64, EvalError> {
    match value {
        Value::Integer(number) => Ok(*number),
        _ => Err(EvalError::TypeMismatch {
            name: name.into(),
            expected: "number".into(),
            got: value.type_name().into(),
        }),
    }
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn render_list(items: &[Value]) -> String {
    let parts: Vec<String> = items.iter().map(Value::render).collect();
    format!("({})", parts.join(" "))
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
            other => escaped.push(other),
        }
    }

    escaped
}

#[cfg(test)]
mod tests;
