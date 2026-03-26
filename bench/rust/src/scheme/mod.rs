use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builtins;
pub mod error;

use builtins::default_env;
pub use error::EvalError;
use error::SourcePos;

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    pos: SourcePos,
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

impl Expr {
    fn new(kind: ExprKind, pos: SourcePos) -> Self {
        Self { kind, pos }
    }
}

type NativeFunc = fn(&[Value], &EvalContext) -> Result<Value, EvalError>;
type EnvRef = Rc<Env>;
type PairRef = Rc<RefCell<PairCell>>;
type StringRef = Rc<RefCell<Vec<char>>>;

struct EvalContext {
    output: RefCell<String>,
}

impl EvalContext {
    fn new() -> Self {
        Self {
            output: RefCell::new(String::new()),
        }
    }

    fn push_output(&self, value: &str) {
        self.output.borrow_mut().push_str(value);
    }

    fn into_output(self) -> String {
        self.output.into_inner()
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(StringRef),
    Char(char),
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
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

struct Env {
    bindings: RefCell<HashMap<String, Value>>,
    parent: Option<EnvRef>,
}

fn make_string(value: impl AsRef<str>) -> Value {
    Value::String(Rc::new(RefCell::new(value.as_ref().chars().collect())))
}

fn render_string(value: &StringRef) -> String {
    value.borrow().iter().collect()
}

impl Value {
    fn render(&self) -> String {
        match self {
            Self::Integer(value) => value.to_string(),
            Self::Boolean(true) => "#t".to_string(),
            Self::Boolean(false) => "#f".to_string(),
            Self::String(value) => {
                let escaped = render_string(value)
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t");
                format!("\"{escaped}\"")
            }
            Self::Char(value) => render_char(*value),
            Self::Symbol(value) => value.clone(),
            Self::Nil => "()".to_string(),
            Self::Pair(pair) => render_pair(pair.clone()),
            Self::NativeProc { name, .. } => format!("#<procedure:{name}>"),
            Self::Closure(_) => "#<procedure>".to_string(),
            Self::Void => "#<void>".to_string(),
        }
    }

    fn display_render(&self) -> String {
        match self {
            Self::String(value) => render_string(value),
            Self::Char(value) => value.to_string(),
            _ => self.render(),
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
            Self::Char(_) => "char",
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

    fn as_string(&self, name: &'static str) -> Result<String, EvalError> {
        match self {
            Self::String(value) => Ok(render_string(value)),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_string_ref(&self, name: &'static str) -> Result<StringRef, EvalError> {
        match self {
            Self::String(value) => Ok(value.clone()),
            other => Err(EvalError::ExpectedString {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_char(&self, name: &'static str) -> Result<char, EvalError> {
        match self {
            Self::Char(value) => Ok(*value),
            other => Err(EvalError::ExpectedChar {
                name,
                found: other.render(),
            }),
        }
    }

    fn as_symbol<'a>(&'a self, name: &'static str) -> Result<&'a str, EvalError> {
        match self {
            Self::Symbol(value) => Ok(value),
            other => Err(EvalError::ExpectedSymbol {
                name,
                found: other.render(),
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

    fn set(&self, name: &str, value: Value) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            true
        } else {
            self.parent
                .as_ref()
                .is_some_and(|parent| parent.set(name, value))
        }
    }
}

impl Closure {
    fn call(&self, args: &[Value], ctx: &EvalContext) -> Result<Value, EvalError> {
        if args.len() < self.params.len()
            || (self.rest_param.is_none() && args.len() != self.params.len())
        {
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

        if let Some(name) = &self.rest_param {
            frame.define(
                name.clone(),
                list_from_values(args[self.params.len()..].to_vec()),
            );
        }

        eval_sequence(&self.body, frame, ctx)
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_pos: SourcePos,
}

impl Parser {
    fn new(tokens: Vec<Token>, eof_pos: SourcePos) -> Self {
        Self {
            tokens,
            index: 0,
            eof_pos,
        }
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
            .ok_or_else(|| EvalError::UnexpectedEof.with_position(self.eof_pos))?;
        self.index += 1;

        match token.kind {
            TokenKind::LParen => {
                let mut items = Vec::new();
                while self.index < self.tokens.len() {
                    if self
                        .tokens
                        .get(self.index)
                        .is_some_and(|token| matches!(token.kind, TokenKind::RParen))
                    {
                        self.index += 1;
                        return Ok(Expr::new(ExprKind::List(items), token.pos));
                    }
                    items.push(self.parse_expr()?);
                }
                Err(EvalError::UnexpectedEof.with_position(self.eof_pos))
            }
            TokenKind::RParen => Err(EvalError::UnexpectedToken {
                token: ")".to_string(),
            }
            .with_position(token.pos)),
            TokenKind::Integer(value) => Ok(Expr::new(ExprKind::Integer(value), token.pos)),
            TokenKind::Boolean(value) => Ok(Expr::new(ExprKind::Boolean(value), token.pos)),
            TokenKind::Char(value) => Ok(Expr::new(ExprKind::Char(value), token.pos)),
            TokenKind::String(value) => Ok(Expr::new(ExprKind::String(value), token.pos)),
            TokenKind::Symbol(value) => Ok(Expr::new(ExprKind::Symbol(value), token.pos)),
            TokenKind::Quote => Ok(Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".to_string()), token.pos),
                    self.parse_expr()?,
                ]),
                token.pos,
            )),
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
    let (result, _) = eval_str_with_output(input)?;
    Ok(result)
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (tokens, eof_pos) = tokenize(input)?;
    let mut parser = Parser::new(tokens, eof_pos);
    let exprs = parser.parse_program()?;

    if exprs.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    let ctx = EvalContext::new();
    let last = eval_sequence(&exprs, env, &ctx)?;
    Ok((last.render(), ctx.into_output()))
}

fn tokenize(input: &str) -> Result<(Vec<Token>, SourcePos), EvalError> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < bytes.len() {
        let pos = pos_from_index(input, index);
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
                tokens.push(Token {
                    kind: TokenKind::LParen,
                    pos,
                });
                index += 1;
            }
            b')' => {
                tokens.push(Token {
                    kind: TokenKind::RParen,
                    pos,
                });
                index += 1;
            }
            b'\'' => {
                tokens.push(Token {
                    kind: TokenKind::Quote,
                    pos,
                });
                index += 1;
            }
            b'"' => {
                let (value, next_index) =
                    parse_string(input, index + 1).map_err(|err| err.with_position(pos))?;
                tokens.push(Token {
                    kind: TokenKind::String(value),
                    pos,
                });
                index = next_index;
            }
            b'#' => {
                if let Some((kind, next_index)) = parse_hash_literal(input, index) {
                    tokens.push(Token { kind, pos });
                    index = next_index;
                } else {
                    return Err(EvalError::UnexpectedToken {
                        token: input[index..].to_string(),
                    }
                    .with_position(pos));
                }
            }
            _ => {
                let start = index;
                while index < bytes.len() && !is_token_boundary(bytes[index]) {
                    index += 1;
                }

                let atom = &input[start..index];
                let atom_pos = pos_from_index(input, start);
                if let Ok(value) = atom.parse::<i64>() {
                    tokens.push(Token {
                        kind: TokenKind::Integer(value),
                        pos: atom_pos,
                    });
                } else if atom.chars().next().is_some_and(|ch| ch == '+' || ch == '-')
                    && atom.len() > 1
                    && atom[1..].chars().all(|ch| ch.is_ascii_digit())
                {
                    return Err(EvalError::InvalidInteger {
                        value: atom.to_string(),
                    }
                    .with_position(atom_pos));
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Symbol(atom.to_string()),
                        pos: atom_pos,
                    });
                }
            }
        }
    }

    Ok((tokens, pos_from_index(input, input.len())))
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

fn parse_hash_literal(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    parse_boolean(input, index).or_else(|| parse_char_literal(input, index))
}

fn parse_boolean(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    let remainder = &input[index..];
    if remainder.starts_with("#t") && is_delimiter(input, index + 2) {
        Some((TokenKind::Boolean(true), index + 2))
    } else if remainder.starts_with("#f") && is_delimiter(input, index + 2) {
        Some((TokenKind::Boolean(false), index + 2))
    } else {
        None
    }
}

fn parse_char_literal(input: &str, index: usize) -> Option<(TokenKind, usize)> {
    let remainder = input.get(index..)?;
    if !remainder.starts_with("#\\") {
        return None;
    }

    let bytes = input.as_bytes();
    let start = index + 2;
    let mut end = start;
    while end < bytes.len() && !is_token_boundary(bytes[end]) {
        end += 1;
    }

    let literal = input.get(start..end)?;
    let value = match literal {
        "space" => ' ',
        "newline" => '\n',
        _ => {
            let mut chars = literal.chars();
            let value = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            value
        }
    };

    Some((TokenKind::Char(value), end))
}

fn pos_from_index(input: &str, index: usize) -> SourcePos {
    let mut line = 1;
    let mut col = 1;

    for byte in input.as_bytes().iter().take(index) {
        if *byte == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    SourcePos::new(line, col)
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

fn eval_sequence(exprs: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for expr in exprs {
        last = eval(expr, env.clone(), ctx)?;
    }
    Ok(last)
}

fn eval(expr: &Expr, env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(name) => env.lookup(name).ok_or_else(|| {
            EvalError::UnboundVariable { name: name.clone() }.with_position(expr.pos)
        }),
        ExprKind::List(items) => eval_list(expr.pos, items, env, ctx),
    }
}

fn eval_list(
    pos: SourcePos,
    items: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let (head, tail) = items.split_first().ok_or_else(|| {
        EvalError::NotAProcedure {
            found: "()".to_string(),
        }
        .with_position(pos)
    })?;

    if let ExprKind::Symbol(name) = &head.kind {
        match name.as_str() {
            "define" => {
                return eval_define(tail, env, ctx).map_err(|err| err.with_position(head.pos))
            }
            "set!" => return eval_set(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "if" => return eval_if(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "quote" => return eval_quote(tail).map_err(|err| err.with_position(head.pos)),
            "lambda" => return eval_lambda(tail, env).map_err(|err| err.with_position(head.pos)),
            "and" => return eval_and(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "or" => return eval_or(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "begin" => {
                return eval_begin(tail, env, ctx).map_err(|err| err.with_position(head.pos))
            }
            "cond" => return eval_cond(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            "let" => return eval_let(tail, env, ctx).map_err(|err| err.with_position(head.pos)),
            _ => {}
        }
    }

    let procedure = eval(head, env.clone(), ctx)?;
    let args = eval_args(tail, env, ctx)?;
    apply_procedure(procedure, &args, ctx).map_err(|err| err.with_position(head.pos))
}

fn eval_define(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(target) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define requires a target".to_string(),
        });
    };

    match &target.kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::WrongArgCount {
                    name: "define",
                    expected: "exactly 2",
                    got: args.len(),
                });
            }

            let value = eval(&args[1], env.clone(), ctx)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        ExprKind::List(signature) => {
            let (name_expr, params_exprs) =
                signature
                    .split_first()
                    .ok_or_else(|| EvalError::InvalidSyntax {
                        message: "define requires a binding name".to_string(),
                    })?;

            let ExprKind::Symbol(name) = &name_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "function name must be a symbol".to_string(),
                });
            };

            if args.len() < 2 {
                return Err(EvalError::InvalidSyntax {
                    message: "function definition requires a body".to_string(),
                });
            }

            let (params, rest_param) = parse_param_slice(params_exprs)?;
            let closure = Value::Closure(Rc::new(Closure {
                params,
                rest_param,
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

fn eval_set(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        });
    }

    let ExprKind::Symbol(name) = &args[0].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "set! requires a symbol target".to_string(),
        });
    };

    let value = eval(&args[1], env.clone(), ctx)?;
    if env.set(name, value) {
        Ok(Value::Void)
    } else {
        Err(EvalError::UnboundVariable { name: name.clone() }.with_position(args[0].pos))
    }
}

fn eval_if(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::WrongArgCount {
            name: "if",
            expected: "exactly 3",
            got: args.len(),
        });
    }

    if eval(&args[0], env.clone(), ctx)?.is_truthy() {
        eval(&args[1], env, ctx)
    } else {
        eval(&args[2], env, ctx)
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

    let (params, rest_param) = parse_param_list(&args[0])?;
    Ok(Value::Closure(Rc::new(Closure {
        params,
        rest_param,
        body: args[1..].to_vec(),
        env,
    })))
}

fn eval_and(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for expr in args {
        let value = eval(expr, env.clone(), ctx)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    eval_sequence(args, env, ctx)
}

fn eval_cond(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    for (index, clause) in args.iter().enumerate() {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let (test, body) = items
            .split_first()
            .ok_or_else(|| EvalError::InvalidSyntax {
                message: "cond clauses cannot be empty".to_string(),
            })?;

        if matches!(&test.kind, ExprKind::Symbol(name) if name == "else") {
            if index + 1 != args.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "cond else clause must be last".to_string(),
                });
            }
            return eval_sequence(body, env, ctx);
        }

        let test_value = eval(test, env.clone(), ctx)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, ctx)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Value, EvalError> {
    let Some(first) = args.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "let requires bindings".to_string(),
        });
    };

    match &first.kind {
        ExprKind::Symbol(name) => eval_named_let(name, &args[1..], env, ctx),
        _ => eval_plain_let(first, &args[1..], env, ctx),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "let requires a body".to_string(),
        });
    }

    let bindings = parse_bindings(bindings_expr)?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let frame = Env::new(Some(env));

    for ((name, _), value) in bindings.into_iter().zip(values.into_iter()) {
        frame.define(name, value);
    }

    eval_sequence(body, frame, ctx)
}

fn eval_named_let(
    name: &str,
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::InvalidSyntax {
            message: "named let requires bindings and a body".to_string(),
        });
    }

    let bindings = parse_bindings(&args[0])?;
    let values = eval_binding_values(&bindings, env.clone(), ctx)?;
    let params = bindings
        .iter()
        .map(|(binding, _)| binding.clone())
        .collect();
    let frame = Env::new(Some(env));
    let closure = Value::Closure(Rc::new(Closure {
        params,
        rest_param: None,
        body: args[1..].to_vec(),
        env: frame.clone(),
    }));

    frame.define(name.to_string(), closure.clone());
    apply_procedure(closure, &values, ctx)
}

fn parse_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let ExprKind::List(bindings) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let bindings must be a list".to_string(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        let ExprKind::List(items) = &binding.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must be pairs".to_string(),
            });
        };

        if items.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "let bindings must contain exactly 2 items".to_string(),
            });
        }

        let ExprKind::Symbol(name) = &items[0].kind else {
            return Err(EvalError::InvalidSyntax {
                message: "let binding names must be symbols".to_string(),
            });
        };

        parsed.push((name.clone(), items[1].clone()));
    }

    Ok(parsed)
}

fn eval_binding_values(
    bindings: &[(String, Expr)],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(bindings.len());
    for (_, expr) in bindings {
        values.push(eval(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn eval_args(args: &[Expr], env: EnvRef, ctx: &EvalContext) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env.clone(), ctx)?);
    }
    Ok(values)
}

fn apply_procedure(
    procedure: Value,
    args: &[Value],
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    match procedure {
        Value::NativeProc { func, .. } => func(args, ctx),
        Value::Closure(closure) => closure.call(args, ctx),
        other => Err(EvalError::NotAProcedure {
            found: other.render(),
        }),
    }
}

fn parse_param_list(expr: &Expr) -> Result<(Vec<String>, Option<String>), EvalError> {
    match &expr.kind {
        ExprKind::List(items) => parse_param_slice(items),
        ExprKind::Symbol(name) => Ok((Vec::new(), Some(name.clone()))),
        _ => Err(EvalError::InvalidSyntax {
            message: "lambda parameters must be a list or symbol".to_string(),
        }),
    }
}

fn parse_param_slice(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::with_capacity(items.len());
    let mut index = 0;
    while let Some(item) = items.get(index) {
        let ExprKind::Symbol(name) = &item.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "parameter names must be symbols".to_string(),
            });
        };

        if name == "." {
            let Some(rest_expr) = items.get(index + 1) else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter dot must be followed by a name".to_string(),
                });
            };
            let ExprKind::Symbol(rest_name) = &rest_expr.kind else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter name must be a symbol".to_string(),
                });
            };
            if index + 2 != items.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter must be the final parameter".to_string(),
                });
            }
            return Ok((params, Some(rest_name.clone())));
        }

        params.push(name.clone());
        index += 1;
    }
    Ok((params, None))
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::Char(value) => Ok(Value::Char(*value)),
        ExprKind::String(value) => Ok(make_string(value)),
        ExprKind::Symbol(value) => Ok(Value::Symbol(value.clone())),
        ExprKind::List(items) => {
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

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        _ => format!("#\\{value}"),
    }
}

#[cfg(test)]
mod tests;
