pub mod error;

pub use error::{EvalError, SourcePos};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn position(&self) -> SourcePos {
        match self {
            Self::Integer(_, position)
            | Self::Boolean(_, position)
            | Self::String(_, position)
            | Self::Symbol(_, position)
            | Self::List(_, position) => *position,
        }
    }
}

type EnvRef = Rc<RefCell<Environment>>;

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Value>),
    Procedure(Rc<Procedure>),
    Builtin(Builtin),
    Void,
}

struct Procedure {
    params: Vec<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equal,
    LessThanOrEqual,
    Not,
    Cons,
    Car,
    Cdr,
    NullPred,
    List,
    Length,
    Append,
    StringPred,
    NumberPred,
    BooleanPred,
    PairPred,
    SymbolPred,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, Value>,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "integer",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Procedure(_) | Self::Builtin(_) => "procedure",
            Self::Void => "void",
        }
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Self::Integer(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "integer",
                found: other.type_name().into(),
            }),
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
            Self::Procedure(_) | Self::Builtin(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::Equal => "=",
            Self::LessThanOrEqual => "<=",
            Self::Not => "not",
            Self::Cons => "cons",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::NullPred => "null?",
            Self::List => "list",
            Self::Length => "length",
            Self::Append => "append",
            Self::StringPred => "string?",
            Self::NumberPred => "number?",
            Self::BooleanPred => "boolean?",
            Self::PairPred => "pair?",
            Self::SymbolPred => "symbol?",
        }
    }

    fn apply(self, args: &[Value]) -> Result<Value, EvalError> {
        apply_builtin(self.name(), args)
    }
}

impl Environment {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
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
            return Err(EvalError::EmptyInput);
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let position = self.current_position();
        match self.peek_char() {
            Some('(') => self.parse_list(position),
            Some('\'') => self.parse_quote(position),
            Some('"') => self.parse_string(position),
            Some(')') => Err(EvalError::SyntaxError {
                message: "unexpected ')'".into(),
            }
            .with_position(position)),
            Some(_) => self.parse_atom(position),
            None => Err(EvalError::UnexpectedEof.with_position(position)),
        }
    }

    fn parse_list(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('(', position)?;
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.advance_char();
                    return Ok(Expr::List(items, position));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(EvalError::UnexpectedEof.with_position(self.current_position())),
            }
        }
    }

    fn parse_quote(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('\'', position)?;
        let quoted = self.parse_expr()?;
        Ok(Expr::List(
            vec![Expr::Symbol("quote".into(), position), quoted],
            position,
        ))
    }

    fn parse_string(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        self.expect_char('"', position)?;
        let mut value = String::new();

        while let Some(ch) = self.advance_char() {
            match ch {
                '"' => return Ok(Expr::String(value, position)),
                '\\' => {
                    let escaped = self
                        .advance_char()
                        .ok_or_else(|| EvalError::UnexpectedEof.with_position(position))?;
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        other => value.push(other),
                    }
                }
                other => value.push(other),
            }
        }

        Err(EvalError::UnexpectedEof.with_position(position))
    }

    fn parse_atom(&mut self, position: SourcePos) -> Result<Expr, EvalError> {
        let token = self.read_token();
        if token.is_empty() {
            return Err(EvalError::UnexpectedEof.with_position(position));
        }

        match token {
            "#t" => Ok(Expr::Boolean(true, position)),
            "#f" => Ok(Expr::Boolean(false, position)),
            _ if is_integer_token(token) => {
                let value = token.parse().map_err(|_| {
                    EvalError::SyntaxError {
                        message: format!("invalid integer literal: {token}"),
                    }
                    .with_position(position)
                })?;
                Ok(Expr::Integer(value, position))
            }
            _ => Ok(Expr::Symbol(token.to_string(), position)),
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            while matches!(self.peek_char(), Some(ch) if ch.is_whitespace()) {
                self.advance_char();
            }

            if self.peek_char() == Some(';') {
                while let Some(ch) = self.advance_char() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn read_token(&mut self) -> &'a str {
        let start = self.pos;

        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() || matches!(ch, '(' | ')' | ';') {
                break;
            }
            self.advance_char();
        }

        &self.input[start..self.pos]
    }

    fn expect_char(&mut self, expected: char, position: SourcePos) -> Result<(), EvalError> {
        match self.advance_char() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(EvalError::SyntaxError {
                message: format!("expected '{expected}', found '{actual}'"),
            }
            .with_position(position)),
            None => Err(EvalError::UnexpectedEof.with_position(position)),
        }
    }

    fn current_position(&self) -> SourcePos {
        SourcePos::new(self.line, self.col)
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
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
}

fn is_integer_token(token: &str) -> bool {
    if token.chars().all(|ch| ch.is_ascii_digit()) {
        return true;
    }

    let mut chars = token.chars();
    matches!(chars.next(), Some('-'))
        && chars.clone().next().is_some()
        && chars.all(|ch| ch.is_ascii_digit())
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

fn render_list(items: &[Value]) -> String {
    if items.is_empty() {
        return "()".into();
    }

    let rendered_items = items
        .iter()
        .map(Value::render)
        .collect::<Vec<_>>()
        .join(" ");
    format!("({rendered_items})")
}

fn default_env() -> EnvRef {
    let env = Environment::new(None);

    for builtin in [
        Builtin::Add,
        Builtin::Sub,
        Builtin::Mul,
        Builtin::Div,
        Builtin::LessThan,
        Builtin::GreaterThan,
        Builtin::Equal,
        Builtin::LessThanOrEqual,
        Builtin::Not,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::NullPred,
        Builtin::List,
        Builtin::Length,
        Builtin::Append,
        Builtin::StringPred,
        Builtin::NumberPred,
        Builtin::BooleanPred,
        Builtin::PairPred,
        Builtin::SymbolPred,
    ] {
        env_define(&env, builtin.name().into(), Value::Builtin(builtin));
    }

    env
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut().bindings.insert(name, value);
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    let (value, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    value.or_else(|| parent.and_then(|parent| env_lookup(&parent, name)))
}

fn eval_program(exprs: &[Expr]) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    eval_sequence(exprs, &env)
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expr in exprs {
        last = eval_expr(expr, env)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value, _) => Ok(Value::Integer(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnboundVariable { name: name.clone() }.with_position(*position)
        }),
        Expr::List(items, position) => {
            eval_list(items, env).map_err(|err| err.with_position(*position))
        }
    }
}

fn eval_list(items: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some(head) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "cannot evaluate empty list".into(),
        });
    };

    let head_position = head.position();

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => {
                return eval_define(&items[1..], env)
                    .map_err(|err| err.with_position(head_position))
            }
            "if" => {
                return eval_if(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            "quote" => {
                return eval_quote(&items[1..]).map_err(|err| err.with_position(head_position))
            }
            "lambda" => {
                return eval_lambda(&items[1..], env)
                    .map_err(|err| err.with_position(head_position))
            }
            "and" => {
                return eval_and(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            "or" => {
                return eval_or(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            "begin" => {
                return eval_begin(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            "cond" => {
                return eval_cond(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            "let" => {
                return eval_let(&items[1..], env).map_err(|err| err.with_position(head_position))
            }
            _ => {}
        }
    }

    let callable = match head {
        Expr::Symbol(name, position) => env_lookup(env, name).ok_or_else(|| {
            EvalError::UnknownOperator { name: name.clone() }.with_position(*position)
        })?,
        _ => eval_expr(head, env)?,
    };

    let mut args = Vec::with_capacity(items.len().saturating_sub(1));
    for expr in &items[1..] {
        args.push(eval_expr(expr, env)?);
    }

    apply_callable(callable, &args).map_err(|err| err.with_position(head_position))
}

fn eval_define(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if let [Expr::Symbol(name, _), value_expr] = args {
        let value = eval_expr(value_expr, env)?;
        env_define(env, name.clone(), value);
        return Ok(Value::Void);
    }

    if let Some((Expr::List(signature, _), body)) = args.split_first() {
        let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "invalid function definition".into(),
            });
        };

        if body.is_empty() {
            return Err(EvalError::SyntaxError {
                message: "function definition requires a body".into(),
            });
        }

        let params = parse_param_names(params)?;
        let procedure = Value::Procedure(Rc::new(Procedure {
            params,
            body: body.to_vec(),
            env: Rc::clone(env),
        }));

        env_define(env, name.clone(), procedure);
        return Ok(Value::Void);
    }

    Err(EvalError::SyntaxError {
        message: "invalid define".into(),
    })
}

fn eval_if(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [condition, consequent] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(consequent, env)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, consequent, alternate] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(consequent, env)
            } else {
                eval_expr(alternate, env)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if".into(),
            expected: "2 or 3 arguments".into(),
            got: args.len(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [datum] => quote_to_value(datum),
        _ => Err(EvalError::WrongArgCount {
            name: "quote".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        }),
    }
}

fn quote_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(value, _) => Ok(Value::Integer(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(value.clone())),
        Expr::Symbol(name, _) => Ok(Value::Symbol(name.clone())),
        Expr::List(items, _) => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(quote_to_value(item)?);
            }
            Ok(Value::List(values))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a parameter list and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "lambda requires a body".into(),
        });
    }

    let params = parse_param_list(params_expr)?;
    Ok(Value::Procedure(Rc::new(Procedure {
        params,
        body: body.to_vec(),
        env: Rc::clone(env),
    })))
}

fn parse_param_list(params_expr: &Expr) -> Result<Vec<String>, EvalError> {
    match params_expr {
        Expr::List(items, _) => parse_param_names(items),
        _ => Err(EvalError::SyntaxError {
            message: "parameter list must be a list of symbols".into(),
        }),
    }
}

fn parse_param_names(items: &[Expr]) -> Result<Vec<String>, EvalError> {
    let mut params = Vec::with_capacity(items.len());

    for item in items {
        let Expr::Symbol(name, _) = item else {
            return Err(EvalError::SyntaxError {
                message: "parameter names must be symbols".into(),
            });
        };
        params.push(name.clone());
    }

    Ok(params)
}

fn apply_callable(callable: Value, args: &[Value]) -> Result<Value, EvalError> {
    match callable {
        Value::Procedure(procedure) => apply_procedure(&procedure, args),
        Value::Builtin(builtin) => builtin.apply(args),
        other => Err(EvalError::NotCallable {
            found: other.type_name().into(),
        }),
    }
}

fn apply_procedure(procedure: &Procedure, args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != procedure.params.len() {
        return Err(EvalError::WrongArgCount {
            name: "lambda".into(),
            expected: format!("exactly {} argument(s)", procedure.params.len()),
            got: args.len(),
        });
    }

    let call_env = Environment::new(Some(Rc::clone(&procedure.env)));
    for (param, arg) in procedure.params.iter().zip(args) {
        env_define(&call_env, param.clone(), arg.clone());
    }

    eval_sequence(&procedure.body, &call_env)
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

fn eval_begin(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    eval_sequence(args, env)
}

fn eval_cond(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::SyntaxError {
                message: "cond clauses must be lists".into(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::SyntaxError {
                message: "cond clause cannot be empty".into(),
            });
        };

        if matches!(test, Expr::Symbol(symbol, _) if symbol == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::SyntaxError {
                    message: "else clause must be last".into(),
                });
            }

            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_sequence(body, env)
            };
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

fn eval_let(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::List(bindings, _), body @ ..] => eval_plain_let(bindings, body, env),
        [Expr::Symbol(name, _), Expr::List(bindings, _), body @ ..] => {
            eval_named_let(name, bindings, body, env)
        }
        _ => Err(EvalError::SyntaxError {
            message: "invalid let".into(),
        }),
    }
}

fn eval_plain_let(bindings: &[Expr], body: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings, env)?;
    let let_env = Environment::new(Some(Rc::clone(env)));
    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    eval_sequence(body, &let_env)
}

fn eval_named_let(
    name: &str,
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "let requires a body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings, env)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let args = bindings
        .into_iter()
        .map(|(_, value)| value)
        .collect::<Vec<_>>();

    let let_env = Environment::new(Some(Rc::clone(env)));
    let procedure = Value::Procedure(Rc::new(Procedure {
        params,
        body: body.to_vec(),
        env: Rc::clone(&let_env),
    }));
    env_define(&let_env, name.to_string(), procedure.clone());

    apply_callable(procedure, &args)
}

fn parse_let_bindings(bindings: &[Expr], env: &EnvRef) -> Result<Vec<(String, Value)>, EvalError> {
    let mut parsed = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(items, _) = binding else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be lists".into(),
            });
        };

        let [Expr::Symbol(name, _), value_expr] = items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: "let bindings must be (name value) pairs".into(),
            });
        };

        parsed.push((name.clone(), eval_expr(value_expr, env)?));
    }

    Ok(parsed)
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => eval_add(args),
        "-" => eval_sub(args),
        "*" => eval_mul(args),
        "/" => eval_div(args),
        "<" => eval_compare(name, args, |lhs, rhs| lhs < rhs),
        ">" => eval_compare(name, args, |lhs, rhs| lhs > rhs),
        "=" => eval_compare(name, args, |lhs, rhs| lhs == rhs),
        "<=" => eval_compare(name, args, |lhs, rhs| lhs <= rhs),
        "not" => eval_not(args),
        "cons" => eval_cons(args),
        "car" => eval_car(args),
        "cdr" => eval_cdr(args),
        "null?" => eval_null(args),
        "list" => Ok(Value::List(args.to_vec())),
        "length" => eval_length(args),
        "append" => eval_append(args),
        "string?" => eval_predicate("string?", args, |value| matches!(value, Value::String(_))),
        "number?" => eval_predicate("number?", args, |value| matches!(value, Value::Integer(_))),
        "boolean?" => eval_predicate("boolean?", args, |value| matches!(value, Value::Boolean(_))),
        "pair?" => eval_pair_pred(args),
        "symbol?" => eval_predicate("symbol?", args, |value| matches!(value, Value::Symbol(_))),
        _ => Err(EvalError::UnknownOperator {
            name: name.to_string(),
        }),
    }
}

fn eval_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 0_i64;
    for arg in args {
        total = total
            .checked_add(arg.as_integer()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }
    Ok(Value::Integer(total))
}

fn eval_sub(args: &[Value]) -> Result<Value, EvalError> {
    let values = numeric_args(args)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| EvalError::WrongArgCount {
            name: "-".into(),
            expected: "at least 1 argument".into(),
            got: 0,
        })?;

    let result = if rest.is_empty() {
        first.checked_neg().ok_or(EvalError::IntegerOverflow)?
    } else {
        let mut total = *first;
        for value in rest {
            total = total
                .checked_sub(*value)
                .ok_or(EvalError::IntegerOverflow)?;
        }
        total
    };

    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut total = 1_i64;
    for arg in args {
        total = total
            .checked_mul(arg.as_integer()?)
            .ok_or(EvalError::IntegerOverflow)?;
    }
    Ok(Value::Integer(total))
}

fn eval_div(args: &[Value]) -> Result<Value, EvalError> {
    let values = numeric_args(args)?;
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
        total = total
            .checked_div(*value)
            .ok_or(EvalError::IntegerOverflow)?;
    }

    Ok(Value::Integer(total))
}

fn eval_compare<F>(name: &str, args: &[Value], compare: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = numeric_args(args)?;
    if values.len() < 2 {
        return Err(EvalError::WrongArgCount {
            name: name.to_string(),
            expected: "at least 2 arguments".into(),
            got: values.len(),
        });
    }

    for pair in values.windows(2) {
        if !compare(pair[0], pair[1]) {
            return Ok(Value::Boolean(false));
        }
    }

    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "not".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    }

    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn eval_cons(args: &[Value]) -> Result<Value, EvalError> {
    let [first, rest] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cons".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = rest else {
        return Err(EvalError::TypeMismatch {
            expected: "list",
            found: rest.type_name().into(),
        });
    };

    let mut result = Vec::with_capacity(items.len() + 1);
    result.push(first.clone());
    result.extend(items.iter().cloned());
    Ok(Value::List(result))
}

fn eval_car(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "car".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    };

    items
        .first()
        .cloned()
        .ok_or_else(|| EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        })
}

fn eval_cdr(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "cdr".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    };

    if items.is_empty() {
        return Err(EvalError::TypeMismatch {
            expected: "pair",
            found: value.type_name().into(),
        });
    }

    Ok(Value::List(items[1..].to_vec()))
}

fn eval_null(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "null?".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(
        matches!(value, Value::List(items) if items.is_empty()),
    ))
}

fn eval_length(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "length".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    let Value::List(items) = value else {
        return Err(EvalError::TypeMismatch {
            expected: "list",
            found: value.type_name().into(),
        });
    };

    Ok(Value::Integer(items.len() as i64))
}

fn eval_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();

    for value in args {
        let Value::List(items) = value else {
            return Err(EvalError::TypeMismatch {
                expected: "list",
                found: value.type_name().into(),
            });
        };
        result.extend(items.iter().cloned());
    }

    Ok(Value::List(result))
}

fn eval_predicate<F>(name: &str, args: &[Value], predicate: F) -> Result<Value, EvalError>
where
    F: FnOnce(&Value) -> bool,
{
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: name.into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(predicate(value)))
}

fn eval_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: "pair?".into(),
            expected: "exactly 1 argument".into(),
            got: args.len(),
        });
    };

    Ok(Value::Boolean(
        matches!(value, Value::List(items) if !items.is_empty()),
    ))
}

fn numeric_args(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter().map(Value::as_integer).collect()
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
