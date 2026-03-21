pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Int(i64),
    Bool(bool),
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
    LessThan,
    GreaterThan,
    Equal,
    LessEqual,
    Not,
    Cons,
    Car,
    Cdr,
    IsNull,
    List,
    Length,
    IsString,
    IsNumber,
    IsBoolean,
    IsPair,
    IsSymbol,
}

type EnvRef = Rc<Environment>;

#[derive(Debug)]
struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

#[derive(Clone, Debug)]
struct Procedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Debug)]
enum Value {
    Int(i64),
    Bool(bool),
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
            Self::Int(_) => "number",
            Self::Bool(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Builtin(_) | Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Bool(false))
    }

    fn render(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(true) => "#t".into(),
            Self::Bool(false) => "#f".into(),
            Self::String(value) => render_string(value),
            Self::Symbol(value) => value.clone(),
            Self::List(items) => render_list(items),
            Self::Builtin(_) | Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
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
        self.skip_whitespace();

        while !self.is_eof() {
            exprs.push(self.parse_expr()?);
            self.skip_whitespace();
        }

        if exprs.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected expression".into(),
            });
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();

        let next = self.peek_char().ok_or(EvalError::UnexpectedEof)?;
        match next {
            '(' => self.parse_list(),
            ')' => Err(EvalError::SyntaxError {
                message: "unexpected ')'".into(),
            }),
            '\'' => self.parse_quote_sugar(),
            '"' => self.parse_string(),
            _ => self.parse_atom(),
        }
    }

    fn parse_quote_sugar(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        Ok(Expr::List(vec![
            Expr::Symbol("quote".into()),
            self.parse_expr()?,
        ]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();

            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    break;
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof),
            }
        }

        Ok(Expr::List(items))
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.advance_char().ok_or(EvalError::UnexpectedEof)?;
                    let resolved = match escaped {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        other => {
                            return Err(EvalError::SyntaxError {
                                message: format!("unsupported string escape: \\{other}"),
                            });
                        }
                    };
                    value.push(resolved);
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof)
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')') {
                break;
            }
            self.advance_char();
        }

        let token = &self.input[start..self.offset];
        if token.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "expected token".into(),
            });
        }

        match token {
            "#t" => Ok(Expr::Bool(true)),
            "#f" => Ok(Expr::Bool(false)),
            _ => match token.parse::<i64>() {
                Ok(value) => Ok(Expr::Int(value)),
                Err(_) => Ok(Expr::Symbol(token.into())),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
            self.advance_char();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.advance_char() {
            Some(found) if found == expected => Ok(()),
            Some(found) => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found '{found}'"),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
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
    let env = default_env();
    let mut last_value = None;

    for expr in exprs {
        last_value = Some(eval_expr(&expr, &env)?);
    }

    last_value
        .map(|value| value.render())
        .ok_or(EvalError::SyntaxError {
            message: "expected expression".into(),
        })
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    eval_str(input).map(|value| (value, String::new()))
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for (name, builtin) in [
        ("+", Builtin::Add),
        ("-", Builtin::Sub),
        ("*", Builtin::Mul),
        ("/", Builtin::Div),
        ("<", Builtin::LessThan),
        (">", Builtin::GreaterThan),
        ("=", Builtin::Equal),
        ("<=", Builtin::LessEqual),
        ("not", Builtin::Not),
        ("cons", Builtin::Cons),
        ("car", Builtin::Car),
        ("cdr", Builtin::Cdr),
        ("null?", Builtin::IsNull),
        ("list", Builtin::List),
        ("length", Builtin::Length),
        ("string?", Builtin::IsString),
        ("number?", Builtin::IsNumber),
        ("boolean?", Builtin::IsBoolean),
        ("pair?", Builtin::IsPair),
        ("symbol?", Builtin::IsSymbol),
    ] {
        env.define(name, Value::Builtin(builtin));
    }

    env
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .lookup(name)
            .ok_or(EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items, env),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return eval_define(&items[1..], env),
            "if" => return eval_if(&items[1..], env),
            "quote" => return eval_quote(&items[1..]),
            "lambda" => return eval_lambda(&items[1..], env),
            "and" => return eval_and(&items[1..], env),
            "or" => return eval_or(&items[1..], env),
            "let" => return eval_let(&items[1..], env),
            "begin" => return eval_begin(&items[1..], env),
            "cond" => return eval_cond(&items[1..], env),
            _ => {}
        }
    }

    let operator = eval_expr(head, env)?;
    let args = eval_arg_values(&items[1..], env)?;
    apply_value(operator, &args)
}

fn eval_define(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match parts {
        [Expr::Symbol(name), value_expr] => {
            let value = eval_expr(value_expr, env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function body".into(),
                });
            }

            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::SyntaxError {
                    message: "define requires a function name".into(),
                });
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::SyntaxError {
                    message: "define function name must be a symbol".into(),
                });
            };

            let params = parse_param_names(params_exprs)?;
            let procedure = Value::Procedure(Rc::new(Procedure {
                params,
                body: body.to_vec(),
                env: env.clone(),
            }));
            env.define(name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::SyntaxError {
            message: "malformed define".into(),
        }),
    }
}

fn eval_if(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = parts else {
        return Err(EvalError::WrongArity {
            name: "if".into(),
            expected: "exactly 3".into(),
            got: parts.len(),
        });
    };

    if eval_expr(condition, env)?.is_truthy() {
        eval_expr(consequent, env)
    } else {
        eval_expr(alternate, env)
    }
}

fn eval_quote(parts: &[Expr]) -> Result<Value, EvalError> {
    let [datum] = parts else {
        return Err(EvalError::WrongArity {
            name: "quote".into(),
            expected: "exactly 1".into(),
            got: parts.len(),
        });
    };

    Ok(quote_expr(datum))
}

fn eval_lambda(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = parts.split_first() else {
        return Err(EvalError::WrongArity {
            name: "lambda".into(),
            expected: "at least 2".into(),
            got: parts.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a body".into(),
        });
    }

    let Expr::List(params_exprs) = params_expr else {
        return Err(EvalError::SyntaxError {
            message: "lambda parameters must be a list".into(),
        });
    };

    let params = parse_param_names(params_exprs)?;
    Ok(Value::Procedure(Rc::new(Procedure {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_let(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = parts.split_first() else {
        return Err(EvalError::WrongArity {
            name: "let".into(),
            expected: "at least 2".into(),
            got: parts.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        });
    }

    let Expr::List(bindings) = bindings_expr else {
        return Err(EvalError::SyntaxError {
            message: "let bindings must be a list".into(),
        });
    };

    let evaluated_bindings = bindings
        .iter()
        .map(|binding| {
            let Expr::List(parts) = binding else {
                return Err(EvalError::SyntaxError {
                    message: "let binding must be a list".into(),
                });
            };

            let [Expr::Symbol(name), value_expr] = parts.as_slice() else {
                return Err(EvalError::SyntaxError {
                    message: "let binding must contain a name and value".into(),
                });
            };

            Ok((name.clone(), eval_expr(value_expr, env)?))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let local_env = Environment::new(Some(env.clone()));
    for (name, value) in evaluated_bindings {
        local_env.define(name, value);
    }

    eval_body(body, &local_env)
}

fn eval_begin(parts: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_body(parts, env)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clause must be a list".into(),
            });
        };

        let Some((test_expr, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "cond clause cannot be empty".into(),
            });
        };

        if let Expr::Symbol(name) = test_expr {
            if name == "else" {
                if index + 1 != clauses.len() {
                    return Err(EvalError::SyntaxError {
                        message: "cond else clause must be last".into(),
                    });
                }

                if body.is_empty() {
                    return Err(EvalError::SyntaxError {
                        message: "cond else clause requires a body".into(),
                    });
                }

                return eval_body(body, env);
            }
        }

        let test_value = eval_expr(test_expr, env)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_body(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn parse_param_names(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| match param {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".into(),
            }),
        })
        .collect()
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn eval_arg_values(args: &[Expr], env: &EnvRef) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|expr| eval_expr(expr, env)).collect()
}

fn apply_value(operator: Value, args: &[Value]) -> Result<Value, EvalError> {
    match operator {
        Value::Builtin(builtin) => apply_builtin(builtin, args),
        Value::Procedure(procedure) => apply_procedure(&procedure, args),
        other => Err(EvalError::NotCallable {
            found: other.type_name(),
        }),
    }
}

fn apply_builtin(builtin: Builtin, args: &[Value]) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => eval_add(args),
        Builtin::Sub => eval_sub(args),
        Builtin::Mul => eval_mul(args),
        Builtin::Div => eval_div(args),
        Builtin::LessThan => eval_compare(args, "<", |left, right| left < right),
        Builtin::GreaterThan => eval_compare(args, ">", |left, right| left > right),
        Builtin::Equal => eval_compare(args, "=", |left, right| left == right),
        Builtin::LessEqual => eval_compare(args, "<=", |left, right| left <= right),
        Builtin::Not => eval_not(args),
        Builtin::Cons => eval_cons(args),
        Builtin::Car => eval_car(args),
        Builtin::Cdr => eval_cdr(args),
        Builtin::IsNull => eval_null(args),
        Builtin::List => eval_list_builtin(args),
        Builtin::Length => eval_length(args),
        Builtin::IsString => eval_type_predicate(args, "string?", |value| {
            matches!(value, Value::String(_))
        }),
        Builtin::IsNumber => eval_type_predicate(args, "number?", |value| {
            matches!(value, Value::Int(_))
        }),
        Builtin::IsBoolean => eval_type_predicate(args, "boolean?", |value| {
            matches!(value, Value::Bool(_))
        }),
        Builtin::IsPair => eval_type_predicate(args, "pair?", |value| {
            matches!(value, Value::List(items) if !items.is_empty())
        }),
        Builtin::IsSymbol => eval_type_predicate(args, "symbol?", |value| {
            matches!(value, Value::Symbol(_))
        }),
    }
}

fn apply_procedure(procedure: &Procedure, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        return Err(EvalError::WrongArity {
            name: "procedure".into(),
            expected: procedure.params.len().to_string(),
            got: args.len(),
        });
    }

    let local_env = Environment::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.iter().zip(args) {
        local_env.define(param.clone(), arg.clone());
    }

    eval_body(&procedure.body, &local_env)
}

fn eval_body(body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in body {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    Ok(Value::Int(numbers.into_iter().sum()))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    match numbers.as_slice() {
        [] => Err(EvalError::WrongArity {
            name: "-".into(),
            expected: "at least 1".into(),
            got: 0,
        }),
        [value] => Ok(Value::Int(-value)),
        [first, rest @ ..] => Ok(Value::Int(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    Ok(Value::Int(numbers.into_iter().product()))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let numbers = eval_number_args(args)?;
    let [first, rest @ ..] = numbers.as_slice() else {
        return Err(EvalError::WrongArity {
            name: "/".into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        });
    };

    if rest.is_empty() {
        return Err(EvalError::WrongArity {
            name: "/".into(),
            expected: "at least 2".into(),
            got: 1,
        });
    }

    let mut result = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= value;
    }

    Ok(Value::Int(result))
}

fn eval_compare<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_number_args(args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArity {
            name: name.into(),
            expected: "at least 2".into(),
            got: numbers.len(),
        });
    }

    for pair in numbers.windows(2) {
        if !predicate(pair[0], pair[1]) {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArity {
            name: "not".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    }

    Ok(Value::Bool(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value]) -> Result<Value, EvalError> {
    let [head, tail] = args else {
        return Err(EvalError::WrongArity {
            name: "cons".into(),
            expected: "exactly 2".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = tail else {
        return Err(EvalError::TypeError {
            expected: "list",
            found: tail.type_name(),
        });
    };

    let mut result = Vec::with_capacity(items.len() + 1);
    result.push(head.clone());
    result.extend(items.iter().cloned());
    Ok(Value::List(result))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArity {
            name: "car".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        other => Err(EvalError::TypeError {
            expected: "pair",
            found: other.type_name(),
        }),
    }
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArity {
            name: "cdr".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    };

    match value {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        other => Err(EvalError::TypeError {
            expected: "pair",
            found: other.type_name(),
        }),
    }
}

fn eval_null(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArity {
            name: "null?".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    };

    Ok(Value::Bool(matches!(value, Value::List(items) if items.is_empty())))
}

fn eval_list_builtin(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArity {
            name: "length".into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    };

    match value {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        other => Err(EvalError::TypeError {
            expected: "list",
            found: other.type_name(),
        }),
    }
}

fn eval_type_predicate<F>(args: &[Value], name: &str, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArity {
            name: name.into(),
            expected: "exactly 1".into(),
            got: args.len(),
        });
    };

    Ok(Value::Bool(predicate(value)))
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

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

    Ok(Value::Bool(false))
}

fn eval_number_args(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Int(number) => Ok(*number),
            other => Err(EvalError::TypeError {
                expected: "number",
                found: other.type_name(),
            }),
        })
        .collect()
}

fn render_list(items: &[Value]) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&item.render());
    }

    rendered.push(')');
    rendered
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');

    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }

    rendered.push('"');
    rendered
}

#[cfg(test)]
mod tests;
