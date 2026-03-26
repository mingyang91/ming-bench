mod builtins;
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
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn position(&self) -> SourcePos {
        match self {
            Self::Integer(_, position)
            | Self::Boolean(_, position)
            | Self::String(_, position)
            | Self::Char(_, position)
            | Self::Symbol(_, position)
            | Self::List(_, position) => *position,
        }
    }
}

type EnvRef = Rc<RefCell<Environment>>;
type OutputRef = Rc<RefCell<String>>;
type BindingRef = Rc<RefCell<Value>>;

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    MutableString(Rc<RefCell<Vec<char>>>),
    Symbol(String),
    Char(char),
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

#[derive(Clone)]
struct Builtin {
    kind: BuiltinKind,
    output: OutputRef,
}

#[derive(Clone, Copy)]
enum BuiltinKind {
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
    Display,
    Write,
    Newline,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringCopy,
    StringSet,
    CharPred,
}

struct Environment {
    parent: Option<EnvRef>,
    bindings: HashMap<String, BindingRef>,
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Integer(_) => "integer",
            Self::Boolean(_) => "boolean",
            Self::String(_) | Self::MutableString(_) => "string",
            Self::Symbol(_) => "symbol",
            Self::Char(_) => "character",
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

    fn as_string(&self) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            Self::MutableString(value) => Ok(value.borrow().iter().collect()),
            other => Err(EvalError::TypeMismatch {
                expected: "string",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_symbol(&self) -> Result<&str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::TypeMismatch {
                expected: "symbol",
                found: other.type_name().into(),
            }),
        }
    }

    fn as_char(&self) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::TypeMismatch {
                expected: "character",
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
            Self::MutableString(value) => render_string(&value.borrow().iter().collect::<String>()),
            Self::Symbol(value) => value.clone(),
            Self::Char(value) => render_char(*value),
            Self::List(items) => render_list(items),
            Self::Procedure(_) | Self::Builtin(_) => "#<procedure>".into(),
            Self::Void => "#<void>".into(),
        }
    }

    fn render_display(&self) -> String {
        match self {
            Self::String(value) => value.clone(),
            Self::MutableString(value) => value.borrow().iter().collect(),
            Self::Char(value) => value.to_string(),
            Self::List(items) => render_display_list(items),
            _ => self.render(),
        }
    }
}

impl Builtin {
    fn new(kind: BuiltinKind, output: OutputRef) -> Self {
        Self { kind, output }
    }

    fn apply(&self, args: &[Value]) -> Result<Value, EvalError> {
        builtins::apply_builtin(self.kind, args, &self.output)
    }
}

impl BuiltinKind {
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
            Self::Display => "display",
            Self::Write => "write",
            Self::Newline => "newline",
            Self::StringAppend => "string-append",
            Self::StringLength => "string-length",
            Self::Substring => "substring",
            Self::StringToNumber => "string->number",
            Self::NumberToString => "number->string",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::StringRef => "string-ref",
            Self::StringCopy => "string-copy",
            Self::StringSet => "string-set!",
            Self::CharPred => "char?",
        }
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
            _ if token.starts_with("#\\") => {
                let value = parse_char_literal(token).ok_or_else(|| {
                    EvalError::SyntaxError {
                        message: format!("invalid character literal: {token}"),
                    }
                    .with_position(position)
                })?;
                Ok(Expr::Char(value, position))
            }
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

fn parse_char_literal(token: &str) -> Option<char> {
    let suffix = token.strip_prefix("#\\")?;
    match suffix {
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => {
            let mut chars = suffix.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
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

fn render_display_list(items: &[Value]) -> String {
    if items.is_empty() {
        return "()".into();
    }

    let rendered_items = items
        .iter()
        .map(Value::render_display)
        .collect::<Vec<_>>()
        .join(" ");
    format!("({rendered_items})")
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

fn env_define(env: &EnvRef, name: String, value: Value) {
    env.borrow_mut()
        .bindings
        .insert(name, Rc::new(RefCell::new(value)));
}

fn env_lookup_binding(env: &EnvRef, name: &str) -> Option<BindingRef> {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    binding.or_else(|| parent.and_then(|parent| env_lookup_binding(&parent, name)))
}

fn env_lookup(env: &EnvRef, name: &str) -> Option<Value> {
    env_lookup_binding(env, name).map(|binding| binding.borrow().clone())
}

fn env_set(env: &EnvRef, name: &str, value: Value) -> bool {
    let (binding, parent) = {
        let borrowed = env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    if let Some(binding) = binding {
        *binding.borrow_mut() = value;
        true
    } else if let Some(parent) = parent {
        env_set(&parent, name, value)
    } else {
        false
    }
}

fn eval_program(exprs: &[Expr], output: OutputRef) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = builtins::default_env(output);
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
        Expr::Char(value, _) => Ok(Value::Char(*value)),
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
            "set!" => {
                return eval_set(&items[1..], env).map_err(|err| err.with_position(head_position))
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
        Expr::Char(value, _) => Ok(Value::Char(*value)),
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

fn eval_set(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, position), value_expr] => {
            let value = eval_expr(value_expr, env)?;
            if env_set(env, name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() }.with_position(*position))
            }
        }
        [_, _] => Err(EvalError::SyntaxError {
            message: "set! target must be a symbol".into(),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "set!".into(),
            expected: "exactly 2 arguments".into(),
            got: args.len(),
        }),
    }
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

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_input(input)?;
    Ok(value.render())
}

fn eval_input(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_program()?;
    let output = Rc::new(RefCell::new(String::new()));
    let value = eval_program(&exprs, Rc::clone(&output))?;
    let rendered_output = output.borrow().clone();
    Ok((value, rendered_output))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_input(input)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;
