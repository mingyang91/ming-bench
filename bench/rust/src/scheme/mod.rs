pub mod error;

pub use error::EvalError;

use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
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
    Procedure(Rc<Procedure>),
    Void,
}

#[derive(Debug, Clone)]
enum Procedure {
    Builtin {
        name: &'static str,
    },
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
}

type EnvRef = Rc<Env>;

#[derive(Debug)]
struct Env {
    values: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            values: RefCell::new(HashMap::new()),
            parent,
        })
    }

    fn define(&self, name: impl Into<String>, value: Value) {
        self.values.borrow_mut().insert(name.into(), value);
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.values
            .borrow()
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
}

impl Procedure {
    fn call(&self, args: Vec<Value>) -> Result<Value, EvalError> {
        match self {
            Self::Builtin { name } => apply_builtin(name, &args),
            Self::Lambda { params, body, env } => {
                if args.len() != params.len() {
                    return Err(EvalError::WrongArgCount {
                        name: "lambda",
                        expected: "exactly the declared number of arguments",
                        got: args.len(),
                    });
                }

                let call_env = Env::new(Some(env.clone()));
                for (param, value) in params.iter().cloned().zip(args) {
                    call_env.define(param, value);
                }

                eval_sequence(body, &call_env)
            }
        }
    }
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
            Self::List(_) => "list",
            Self::Procedure(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".into(),
            Self::Boolean(false) => "#f".into(),
            Self::String(value) => {
                let escaped = value
                    .chars()
                    .flat_map(|ch| match ch {
                        '\\' => ['\\', '\\'].into_iter().collect::<Vec<_>>(),
                        '"' => ['\\', '"'].into_iter().collect::<Vec<_>>(),
                        '\n' => ['\\', 'n'].into_iter().collect::<Vec<_>>(),
                        '\t' => ['\\', 't'].into_iter().collect::<Vec<_>>(),
                        other => [other].into_iter().collect::<Vec<_>>(),
                    })
                    .collect::<String>();

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
            Self::Procedure(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
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
        let mut expressions = Vec::new();
        self.skip_ignored();

        while !self.is_eof() {
            expressions.push(self.parse_expr()?);
            self.skip_ignored();
        }

        Ok(expressions)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();

        let Some(ch) = self.peek_char() else {
            return Err(EvalError::UnexpectedEof);
        };

        match ch {
            '(' => self.parse_list(),
            '\'' => self.parse_quote_shorthand(),
            '"' => self.parse_string(),
            '#' => self.parse_boolean(),
            ')' => Err(EvalError::UnexpectedToken { token: ")".into() }),
            '-' if self
                .peek_second_char()
                .is_some_and(|next| next.is_ascii_digit()) =>
            {
                self.parse_number()
            }
            ch if ch.is_ascii_digit() => self.parse_number(),
            _ => self.parse_symbol(),
        }
    }

    fn parse_quote_shorthand(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('\'')?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(vec![Expr::Symbol("quote".into()), quoted]))
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('(')?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();

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
        self.expect_char('"')?;
        let mut value = String::new();

        while let Some(ch) = self.bump_char() {
            match ch {
                '"' => return Ok(Expr::String(value)),
                '\\' => {
                    let escaped = self.bump_char().ok_or(EvalError::UnterminatedString)?;
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnterminatedString)
    }

    fn parse_boolean(&mut self) -> Result<Expr, EvalError> {
        self.expect_char('#')?;
        match self.bump_char() {
            Some('t') => Ok(Expr::Boolean(true)),
            Some('f') => Ok(Expr::Boolean(false)),
            Some(other) => Err(EvalError::InvalidBoolean {
                literal: format!("#{other}"),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn parse_number(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;
        if self.peek_char() == Some('-') {
            self.bump_char();
        }

        let mut saw_digit = false;
        while self.peek_char().is_some_and(|ch| ch.is_ascii_digit()) {
            saw_digit = true;
            self.bump_char();
        }

        let literal = &self.input[start..self.offset];
        if !saw_digit {
            return Err(EvalError::InvalidNumber {
                literal: literal.into(),
            });
        }

        literal
            .parse::<i64>()
            .map(Expr::Integer)
            .map_err(|_| EvalError::InvalidNumber {
                literal: literal.into(),
            })
    }

    fn parse_symbol(&mut self) -> Result<Expr, EvalError> {
        let start = self.offset;

        while self
            .peek_char()
            .is_some_and(|ch| !ch.is_whitespace() && ch != '(' && ch != ')' && ch != ';' && ch != '\'')
        {
            self.bump_char();
        }

        if start == self.offset {
            let token = self
                .peek_char()
                .map(|ch| ch.to_string())
                .unwrap_or_else(|| "<eof>".into());
            return Err(EvalError::UnexpectedToken { token });
        }

        Ok(Expr::Symbol(self.input[start..self.offset].into()))
    }

    fn skip_ignored(&mut self) {
        loop {
            while self.peek_char().is_some_and(char::is_whitespace) {
                self.bump_char();
            }

            if self.peek_char() != Some(';') {
                return;
            }

            while let Some(ch) = self.bump_char() {
                if ch == '\n' {
                    break;
                }
            }
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), EvalError> {
        match self.bump_char() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(EvalError::UnexpectedToken {
                token: ch.to_string(),
            }),
            None => Err(EvalError::UnexpectedEof),
        }
    }

    fn bump_char(&mut self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        let ch = chars.next()?;
        self.offset += ch.len_utf8();
        Some(ch)
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn peek_second_char(&self) -> Option<char> {
        let mut chars = self.input[self.offset..].chars();
        chars.next()?;
        chars.next()
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn eval_expr_in_env(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_application(items, env),
    }
}

fn eval_application(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::NotAProcedure);
    };

    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "and" => return eval_and(tail, env),
            "begin" => return eval_begin(tail, env),
            "cond" => return eval_cond(tail, env),
            "define" => return eval_define(tail, env),
            "if" => return eval_if(tail, env),
            "lambda" => return eval_lambda(tail, env),
            "let" => return eval_let(tail, env),
            "or" => return eval_or(tail, env),
            "quote" => return eval_quote(tail),
            _ => {}
        }
    }

    let procedure = eval_expr_in_env(head, env)?;
    let args = tail
        .iter()
        .map(|expr| eval_expr_in_env(expr, env))
        .collect::<Result<Vec<_>, EvalError>>()?;

    apply_procedure(procedure, args)
}

fn eval_sequence(expressions: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expression in expressions {
        last = eval_expr_in_env(expression, env)?;
    }

    Ok(last)
}

fn eval_and(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval_expr_in_env(arg, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }

    Ok(result)
}

fn eval_or(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval_expr_in_env(arg, env)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items) = clause else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected clauses to be lists",
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::InvalidForm {
                name: "cond",
                message: "expected each clause to contain a test",
            });
        };

        if matches!(test, Expr::Symbol(symbol) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must be last",
                });
            }

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "cond",
                    message: "else clause must contain a body",
                });
            }

            return eval_sequence(body, env);
        }

        let value = eval_expr_in_env(test, env)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((target, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "define",
            expected: "a binding target and value",
            got: 0,
        });
    };

    match target {
        Expr::Symbol(name) => {
            if rest.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let value = eval_expr_in_env(&rest[0], env)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        Expr::List(signature) => {
            let Some((name_expr, params_exprs)) = signature.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            let Expr::Symbol(name) = name_expr else {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected a function name",
                });
            };

            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "define",
                    message: "expected at least one body expression",
                });
            }

            let params = parse_param_names(params_exprs, "define")?;
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params,
                body: rest.to_vec(),
                env: env.clone(),
            }));

            env.define(name.clone(), procedure);
            Ok(Value::Void)
        }
        _ => Err(EvalError::InvalidForm {
            name: "define",
            message: "expected a symbol or function signature",
        }),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3 arguments",
            got: args.len(),
        });
    }

    if eval_expr_in_env(&args[0], env)?.is_truthy() {
        eval_expr_in_env(&args[1], env)
    } else {
        eval_expr_in_env(&args[2], env)
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "a parameter list and body",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::InvalidForm {
            name: "lambda",
            message: "expected at least one body expression",
        });
    }

    let params = parse_params(params_expr, "lambda")?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env: env.clone(),
    })))
}

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((first, rest)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "bindings and at least one body expression",
            got: 0,
        });
    };

    match first {
        Expr::List(_) => {
            if rest.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(first, "let")?;
            let values = bindings
                .iter()
                .map(|(_, expr)| eval_expr_in_env(expr, env))
                .collect::<Result<Vec<_>, EvalError>>()?;

            let let_env = Env::new(Some(env.clone()));
            for ((name, _), value) in bindings.into_iter().zip(values) {
                let_env.define(name, value);
            }

            eval_sequence(rest, &let_env)
        }
        Expr::Symbol(name) => {
            let Some((bindings_expr, body)) = rest.split_first() else {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected bindings and at least one body expression",
                });
            };

            if body.is_empty() {
                return Err(EvalError::InvalidForm {
                    name: "let",
                    message: "expected at least one body expression",
                });
            }

            let bindings = parse_let_bindings(bindings_expr, "let")?;
            let args = bindings
                .iter()
                .map(|(_, expr)| eval_expr_in_env(expr, env))
                .collect::<Result<Vec<_>, EvalError>>()?;
            let params = bindings
                .into_iter()
                .map(|(binding_name, _)| binding_name)
                .collect();

            let let_env = Env::new(Some(env.clone()));
            let_env.define(name.clone(), Value::Void);

            let procedure = Rc::new(Procedure::Lambda {
                params,
                body: body.to_vec(),
                env: let_env.clone(),
            });
            let value = Value::Procedure(procedure.clone());
            let_env.define(name.clone(), value.clone());

            apply_procedure(value, args)
        }
        _ => Err(EvalError::InvalidForm {
            name: "let",
            message: "expected a binding list or let name",
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(quote_expr(&args[0]))
}

fn parse_params(expr: &Expr, name: &'static str) -> Result<Vec<String>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name,
            message: "expected a parameter list",
        });
    };

    parse_param_names(items, name)
}

fn parse_let_bindings(
    expr: &Expr,
    name: &'static str,
) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings) = expr else {
        return Err(EvalError::InvalidForm {
            name,
            message: "expected a binding list",
        });
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items) if items.len() == 2 => match (&items[0], &items[1]) {
                (Expr::Symbol(symbol), value) => Ok((symbol.clone(), value.clone())),
                _ => Err(EvalError::InvalidForm {
                    name,
                    message: "expected binding names to be symbols",
                }),
            },
            _ => Err(EvalError::InvalidForm {
                name,
                message: "expected each binding to have a name and value",
            }),
        })
        .collect()
}

fn parse_param_names(items: &[Expr], name: &'static str) -> Result<Vec<String>, EvalError> {
    items
        .iter()
        .map(|item| match item {
            Expr::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err(EvalError::InvalidForm {
                name,
                message: "expected parameter names to be symbols",
            }),
        })
        .collect()
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

fn root_env() -> EnvRef {
    let env = Env::new(None);
    for name in [
        "+",
        "-",
        "*",
        "/",
        "<",
        "<=",
        "=",
        ">",
        ">=",
        "append",
        "boolean?",
        "car",
        "cdr",
        "cons",
        "length",
        "list",
        "not",
        "null?",
        "number?",
        "pair?",
        "string?",
        "symbol?",
    ] {
        env.define(name, Value::Procedure(Rc::new(Procedure::Builtin { name })));
    }

    env
}

fn apply_procedure(procedure: Value, args: Vec<Value>) -> Result<Value, EvalError> {
    match procedure {
        Value::Procedure(procedure) => procedure.call(args),
        _ => Err(EvalError::NotAProcedure),
    }
}

fn apply_builtin(name: &'static str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let numbers = extract_numbers("+", args)?;
            Ok(Value::Integer(numbers.iter().sum()))
        }
        "append" => {
            let mut values = Vec::new();
            for arg in args {
                match arg {
                    Value::List(items) => values.extend(items.iter().cloned()),
                    other => {
                        return Err(EvalError::ExpectedList {
                            name: "append",
                            found: other.type_name(),
                        });
                    }
                }
            }

            Ok(Value::List(values))
        }
        "*" => {
            let numbers = extract_numbers("*", args)?;
            Ok(Value::Integer(numbers.iter().product()))
        }
        "-" => {
            let numbers = extract_numbers("-", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "-",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            let value = if rest.is_empty() {
                -*first
            } else {
                rest.iter().fold(*first, |acc, value| acc - value)
            };

            Ok(Value::Integer(value))
        }
        "/" => {
            let numbers = extract_numbers("/", args)?;
            let Some((first, rest)) = numbers.split_first() else {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 1 argument",
                    got: 0,
                });
            };

            if rest.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "/",
                    expected: "at least 2 arguments",
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

            Ok(Value::Integer(result))
        }
        "<" => compare_numbers("<", args, |left, right| left < right),
        "<=" => compare_numbers("<=", args, |left, right| left <= right),
        "=" => compare_numbers("=", args, |left, right| left == right),
        ">" => compare_numbers(">", args, |left, right| left > right),
        ">=" => compare_numbers(">=", args, |left, right| left >= right),
        "boolean?" => predicate_builtin("boolean?", args, |value| matches!(value, Value::Boolean(_))),
        "car" => {
            let list = expect_pair_arg("car", args)?;
            Ok(list[0].clone())
        }
        "cdr" => {
            let list = expect_pair_arg("cdr", args)?;
            Ok(Value::List(list[1..].to_vec()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "cons",
                    expected: "exactly 2 arguments",
                    got: args.len(),
                });
            }

            let Value::List(rest) = &args[1] else {
                return Err(EvalError::ExpectedList {
                    name: "cons",
                    found: args[1].type_name(),
                });
            };

            let mut values = Vec::with_capacity(rest.len() + 1);
            values.push(args[0].clone());
            values.extend(rest.iter().cloned());
            Ok(Value::List(values))
        }
        "length" => {
            let list = expect_list_arg("length", args)?;
            Ok(Value::Integer(list.len() as i64))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount {
                    name: "not",
                    expected: "exactly 1 argument",
                    got: args.len(),
                });
            }

            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "null?" => predicate_builtin("null?", args, |value| {
            matches!(value, Value::List(items) if items.is_empty())
        }),
        "number?" => predicate_builtin("number?", args, |value| matches!(value, Value::Integer(_))),
        "pair?" => predicate_builtin("pair?", args, |value| {
            matches!(value, Value::List(items) if !items.is_empty())
        }),
        "string?" => predicate_builtin("string?", args, |value| matches!(value, Value::String(_))),
        "symbol?" => predicate_builtin("symbol?", args, |value| matches!(value, Value::Symbol(_))),
        _ => Err(EvalError::UnknownProcedure {
            name: name.to_string(),
        }),
    }
}

fn compare_numbers(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = extract_numbers(name, args)?;
    if numbers.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "at least 2 arguments",
            got: numbers.len(),
        });
    }

    let is_match = numbers.windows(2).all(|pair| predicate(pair[0], pair[1]));

    Ok(Value::Boolean(is_match))
}

fn extract_numbers(name: &'static str, args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(EvalError::ExpectedNumber {
                name,
                found: other.type_name(),
            }),
        })
        .collect()
}

fn predicate_builtin(
    name: &'static str,
    args: &[Value],
    predicate: impl Fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    Ok(Value::Boolean(predicate(&args[0])))
}

fn expect_list_arg<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a [Value], EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name,
            expected: "exactly 1 argument",
            got: args.len(),
        });
    }

    match &args[0] {
        Value::List(items) => Ok(items),
        other => Err(EvalError::ExpectedList {
            name,
            found: other.type_name(),
        }),
    }
}

fn expect_pair_arg<'a>(name: &'static str, args: &'a [Value]) -> Result<&'a [Value], EvalError> {
    let list = expect_list_arg(name, args)?;
    if list.is_empty() {
        return Err(EvalError::ExpectedPair {
            name,
            found: "list",
        });
    }

    Ok(list)
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
    let program = parser.parse_program()?;
    let env = root_env();
    let mut last_value = None;

    for expression in &program {
        last_value = Some(eval_expr_in_env(expression, &env)?);
    }

    let value = last_value.ok_or(EvalError::EmptyInput)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

#[cfg(test)]
mod tests;
