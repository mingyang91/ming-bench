pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Position {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Bool { value: bool, pos: Position },
    Int { value: i64, pos: Position },
    String { value: String, pos: Position },
    Symbol { name: String, pos: Position },
    List { items: Vec<Expr>, pos: Position },
}

impl Expr {
    fn pos(&self) -> Position {
        match self {
            Self::Bool { pos, .. }
            | Self::Int { pos, .. }
            | Self::String { pos, .. }
            | Self::Symbol { pos, .. }
            | Self::List { pos, .. } => *pos,
        }
    }
}

type EnvRef = Rc<Environment>;
type BuiltinFn = fn(&[EvaluatedArg]) -> Result<Value, EvalError>;

#[derive(Clone)]
struct EvaluatedArg {
    value: Value,
    pos: Position,
}

impl EvaluatedArg {
    fn as_int(&self) -> Result<i64, EvalError> {
        self.value
            .as_int()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }

    fn as_list(&self) -> Result<&[Value], EvalError> {
        self.value
            .as_list()
            .map_err(|error| error.with_position(self.pos.line, self.pos.col))
    }
}

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

    fn as_list(&self) -> Result<&[Value], EvalError> {
        match self {
            Self::List(items) => Ok(items),
            _ => Err(EvalError::TypeMismatch {
                expected: "list",
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
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
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
        let pos = self.current_position();

        match self.peek_char() {
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.bump_char();
                let quoted = self.parse_expr()?;
                Ok(Expr::List {
                    items: vec![
                        Expr::Symbol {
                            name: "quote".to_string(),
                            pos,
                        },
                        quoted,
                    ],
                    pos,
                })
            }
            Some('"') => self.parse_string(),
            Some(')') => Err(EvalError::ParseError {
                message: "unexpected ')'".to_string(),
            }
            .with_position(pos.line, pos.col)),
            Some(_) => self.parse_atom(),
            None => Err(EvalError::UnexpectedEof.with_position(pos.line, pos.col)),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_position();
        self.bump_char();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump_char();
                    return Ok(Expr::List { items, pos });
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(self.error_here(EvalError::UnexpectedEof)),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let pos = self.current_position();
        self.bump_char();
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String { value, pos }),
                '\\' => {
                    let Some(escaped) = self.bump_char() else {
                        return Err(self.error_here(EvalError::UnexpectedEof));
                    };
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

        Err(self.error_here(EvalError::UnexpectedEof))
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.pos;
        let pos = self.current_position();
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || ch == '(' || ch == ')' || ch == ';' {
                break;
            }
            self.bump_char();
        }

        let token = &self.input[start..self.pos];
        if token.is_empty() {
            return Err(self.error_at(
                pos,
                EvalError::ParseError {
                    message: "expected expression".to_string(),
                },
            ));
        }

        if token == "#t" {
            return Ok(Expr::Bool { value: true, pos });
        }
        if token == "#f" {
            return Ok(Expr::Bool { value: false, pos });
        }
        if is_integer_token(token) {
            return token
                .parse::<i64>()
                .map(|value| Expr::Int { value, pos })
                .map_err(|_| {
                    self.error_at(
                        pos,
                        EvalError::ParseError {
                            message: format!("invalid integer literal: {token}"),
                        },
                    )
                });
        }

        Ok(Expr::Symbol {
            name: token.to_string(),
            pos,
        })
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
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    fn error_here(&self, error: EvalError) -> EvalError {
        self.error_at(self.current_position(), error)
    }

    fn error_at(&self, pos: Position, error: EvalError) -> EvalError {
        error.with_position(pos.line, pos.col)
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

fn with_position<T>(result: Result<T, EvalError>, pos: Position) -> Result<T, EvalError> {
    result.map_err(|error| error.with_position(pos.line, pos.col))
}

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
        ("cons", apply_cons as BuiltinFn),
        ("car", apply_car as BuiltinFn),
        ("cdr", apply_cdr as BuiltinFn),
        ("null?", apply_null as BuiltinFn),
        ("list", apply_list as BuiltinFn),
        ("length", apply_length as BuiltinFn),
        ("append", apply_append as BuiltinFn),
        ("string?", apply_string_pred as BuiltinFn),
        ("number?", apply_number_pred as BuiltinFn),
        ("boolean?", apply_boolean_pred as BuiltinFn),
        ("pair?", apply_pair_pred as BuiltinFn),
        ("symbol?", apply_symbol_pred as BuiltinFn),
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
        Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
        Expr::Int { value, .. } => Ok(Value::Int(*value)),
        Expr::String { value, .. } => Ok(Value::String(value.clone())),
        Expr::Symbol { name, pos } => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundSymbol { name: name.clone() })
            .map_err(|error| error.with_position(pos.line, pos.col)),
        Expr::List { items, pos } => with_position(eval_list(items, env), *pos),
    }
}

fn eval_list(items: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        });
    };

    let head_pos = head.pos();

    match head {
        Expr::Symbol { name, .. } if name == "and" => with_position(eval_and(args, env), head_pos),
        Expr::Symbol { name, .. } if name == "or" => with_position(eval_or(args, env), head_pos),
        Expr::Symbol { name, .. } if name == "if" => with_position(eval_if(args, env), head_pos),
        Expr::Symbol { name, .. } if name == "quote" => {
            with_position(eval_quote(args), head_pos)
        }
        Expr::Symbol { name, .. } if name == "begin" => {
            with_position(eval_begin(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "cond" => {
            with_position(eval_cond(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "let" => with_position(eval_let(args, env), head_pos),
        Expr::Symbol { name, .. } if name == "lambda" => {
            with_position(eval_lambda(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define" => {
            with_position(eval_define(args, env), head_pos)
        }
        _ => {
            let procedure = eval(head, env.clone())?;
            let values = args
                .iter()
                .map(|expr| {
                    eval(expr, env.clone()).map(|value| EvaluatedArg {
                        value,
                        pos: expr.pos(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            with_position(apply_procedure(procedure, &values), head_pos)
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

fn eval_begin(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    eval_program(args, env)
}

fn eval_cond(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    for clause in args {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "cond clauses cannot be empty".to_string(),
            });
        };

        if matches!(test, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone())
            };
        }

        let test_value = eval(test, env.clone())?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_program(body, env.clone())
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 3",
                    got: 2,
                });
            }

            let bindings = parse_let_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = bindings
                .iter()
                .map(|(_, expr)| {
                    eval(expr, env.clone()).map(|value| EvaluatedArg {
                        value,
                        pos: expr.pos(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let closure_env = Environment::new(Some(env));
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body: body.to_vec(),
                env: closure_env.clone(),
            }));
            closure_env.define(name.clone(), procedure.clone());
            apply_procedure(procedure, &values)
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 2",
                    got: 1,
                });
            }

            let bindings = parse_let_bindings(bindings_expr)?;
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, env.clone()))
                .collect::<Result<Vec<_>, _>>()?;

            let let_env = Environment::new(Some(env));
            for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
                let_env.define(name.clone(), value);
            }

            eval_program(body, let_env)
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }),
    }
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
        [Expr::Symbol { name, .. }, value_expr] => {
            let value = eval(value_expr, env.clone())?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List {
            items: signature, ..
        }, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::ParseError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Some((
                Expr::Symbol { name, .. },
                params,
            )) = signature.split_first()
            else {
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
        Expr::List { items, .. } => parse_param_names(items),
        _ => Err(EvalError::ParseError {
            message: "lambda parameters must be a list".to_string(),
        }),
    }
}

fn parse_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|expr| match expr {
            Expr::Symbol { name, .. } => Ok(name.clone()),
            _ => Err(EvalError::ParseError {
                message: "parameter names must be symbols".to_string(),
            }),
        })
        .collect()
}

fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List { items: bindings, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "let bindings must be a list".to_string(),
        });
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List { items: parts, .. } => match parts.as_slice() {
                [Expr::Symbol { name, .. }, value] => Ok((name.clone(), value.clone())),
                _ => Err(EvalError::ParseError {
                    message: "let bindings must be (name value) pairs".to_string(),
                }),
            },
            _ => Err(EvalError::ParseError {
                message: "let bindings must be (name value) pairs".to_string(),
            }),
        })
        .collect()
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Bool { value, .. } => Value::Bool(*value),
        Expr::Int { value, .. } => Value::Int(*value),
        Expr::String { value, .. } => Value::String(value.clone()),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn apply_procedure(value: Value, args: &[EvaluatedArg]) -> Result<Value, EvalError> {
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
            for (name, arg) in params.iter().zip(args.iter()) {
                call_env.define(name.clone(), arg.value.clone());
            }

            eval_program(body, call_env)
        }
    }
}

fn apply_add(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let mut sum = 0_i64;
    for arg in args {
        sum += arg.as_int()?;
    }
    Ok(Value::Int(sum))
}

fn apply_sub(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
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

fn apply_mul(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let mut product = 1_i64;
    for arg in args {
        product *= arg.as_int()?;
    }
    Ok(Value::Int(product))
}

fn apply_div(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
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
            return Err(EvalError::DivisionByZero.with_position(arg.pos.line, arg.pos.col));
        }
        result /= divisor;
    }
    Ok(Value::Int(result))
}

fn apply_lt(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    apply_comparison("<", args, |left, right| left < right)
}

fn apply_gt(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    apply_comparison(">", args, |left, right| left > right)
}

fn apply_eq(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    apply_comparison("=", args, |left, right| left == right)
}

fn apply_lte(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    apply_comparison("<=", args, |left, right| left <= right)
}

fn apply_comparison(
    name: &'static str,
    args: &[EvaluatedArg],
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
        .map(EvaluatedArg::as_int)
        .collect::<Result<Vec<_>, _>>()?;
    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Bool(is_match))
}

fn apply_not(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "not",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(!value.value.is_truthy()))
}

fn apply_cons(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cons",
            expected: "exactly 2",
            got: args.len(),
        });
    };

    let tail_items = tail.as_list()?;
    let mut items = Vec::with_capacity(tail_items.len() + 1);
    items.push(head.value.clone());
    items.extend(tail_items.iter().cloned());
    Ok(Value::List(items))
}

fn apply_car(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    items.first().cloned().ok_or_else(|| EvalError::TypeMismatch {
        expected: "non-empty list",
        found: value.value.render(),
    }
    .with_position(value.pos.line, value.pos.col))
}

fn apply_cdr(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    let items = value.as_list()?;
    if items.is_empty() {
        return Err(
            EvalError::TypeMismatch {
                expected: "non-empty list",
                found: value.value.render(),
            }
            .with_position(value.pos.line, value.pos.col),
        );
    }

    Ok(Value::List(items[1..].to_vec()))
}

fn apply_null(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(
        matches!(&value.value, Value::List(items) if items.is_empty()),
    ))
}

fn apply_list(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    Ok(Value::List(
        args.iter().map(|arg| arg.value.clone()).collect(),
    ))
}

fn apply_length(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Int(value.as_list()?.len() as i64))
}

fn apply_append(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let mut items = Vec::new();
    for value in args {
        items.extend(value.as_list()?.iter().cloned());
    }
    Ok(Value::List(items))
}

fn apply_string_pred(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "string?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::String(_))))
}

fn apply_number_pred(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "number?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Int(_))))
}

fn apply_boolean_pred(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "boolean?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Bool(_))))
}

fn apply_pair_pred(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "pair?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(
        matches!(&value.value, Value::List(items) if !items.is_empty()),
    ))
}

fn apply_symbol_pred(args: &[EvaluatedArg]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "symbol?",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(&value.value, Value::Symbol(_))))
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
