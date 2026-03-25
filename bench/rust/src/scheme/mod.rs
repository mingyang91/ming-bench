pub mod error;

pub use error::{EvalError, SourcePos};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const START_POS: SourcePos = SourcePos::new(1, 1);

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: SourcePos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(pos: SourcePos, kind: ExprKind) -> Self {
        Self { kind, pos }
    }

    fn list_items(&self) -> Option<&[Expr]> {
        match &self.kind {
            ExprKind::List(items) => Some(items),
            _ => None,
        }
    }

    fn symbol_name(&self) -> Option<&str> {
        match &self.kind {
            ExprKind::Symbol(name) => Some(name.as_str()),
            _ => None,
        }
    }
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
    line: usize,
    column: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            column: 1,
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
            Err(syntax_error(self.current_pos(), "empty input"))
        } else {
            Ok(exprs)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_ignored();
        let pos = self.current_pos();

        match self.peek_char() {
            Some('(') => self.parse_list(pos),
            Some('\'') => self.parse_quote_shorthand(pos),
            Some('"') => self.parse_string(pos),
            Some('#') => self.parse_boolean(pos),
            Some('+') | Some('-')
                if self
                    .peek_second_char()
                    .is_some_and(|ch| ch.is_ascii_digit()) =>
            {
                self.parse_number(pos)
            }
            Some(ch) if ch.is_ascii_digit() => self.parse_number(pos),
            Some(_) => self.parse_symbol(pos),
            None => Err(unexpected_eof(pos)),
        }
    }

    fn parse_quote_shorthand(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let expr = self.parse_expr()?;
        Ok(Expr::new(
            pos,
            ExprKind::List(vec![
                Expr::new(pos, ExprKind::Symbol("quote".into())),
                expr,
            ]),
        ))
    }

    fn parse_list(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let mut items = Vec::new();

        loop {
            self.skip_ignored();
            match self.peek_char() {
                Some(')') => {
                    self.bump();
                    return Ok(Expr::new(pos, ExprKind::List(items)));
                }
                Some(_) => items.push(self.parse_expr()?),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }
    }

    fn parse_string(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        self.bump();
        let mut value = String::new();

        loop {
            match self.bump() {
                Some('"') => return Ok(Expr::new(pos, ExprKind::String(value))),
                Some('\\') => {
                    let escaped = match self.bump() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some(ch) => ch,
                        None => return Err(unexpected_eof(self.current_pos())),
                    };
                    value.push(escaped);
                }
                Some(ch) => value.push(ch),
                None => return Err(unexpected_eof(self.current_pos())),
            }
        }
    }

    fn parse_boolean(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        if self.consume_literal("#t") {
            return Ok(Expr::new(pos, ExprKind::Boolean(true)));
        }

        if self.consume_literal("#f") {
            return Ok(Expr::new(pos, ExprKind::Boolean(false)));
        }

        Err(syntax_error(pos, "invalid boolean literal"))
    }

    fn parse_number(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
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
            return Err(syntax_error(pos, "invalid number literal"));
        }

        let token = &self.input[start..self.offset];
        let value = token
            .parse::<i64>()
            .map_err(|_| syntax_error(pos, format!("invalid number literal: {token}")))?;

        Ok(Expr::new(pos, ExprKind::Integer(value)))
    }

    fn parse_symbol(&mut self, pos: SourcePos) -> Result<Expr, EvalError> {
        let start = self.offset;

        while matches!(self.peek_char(), Some(ch) if !is_delimiter(ch)) {
            self.bump();
        }

        if start == self.offset {
            return Err(syntax_error(pos, "expected expression"));
        }

        Ok(Expr::new(
            pos,
            ExprKind::Symbol(self.input[start..self.offset].to_string()),
        ))
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

        for _ in literal.chars() {
            self.bump();
        }

        true
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos::new(self.line, self.column)
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

        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.input.len()
    }
}

fn syntax_error(pos: SourcePos, message: impl Into<String>) -> EvalError {
    EvalError::Syntax {
        pos,
        message: message.into(),
    }
}

fn unexpected_eof(pos: SourcePos) -> EvalError {
    EvalError::UnexpectedEof { pos }
}

fn wrong_arg_count(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    got: usize,
) -> EvalError {
    EvalError::WrongArgCount {
        pos,
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn type_mismatch(
    pos: SourcePos,
    name: impl Into<String>,
    expected: impl Into<String>,
    found: impl Into<String>,
) -> EvalError {
    EvalError::TypeMismatch {
        pos,
        name: name.into(),
        expected: expected.into(),
        found: found.into(),
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
    let result = eval_sequence(&exprs, &env, START_POS)?;
    Ok(result.render())
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef, empty_pos: SourcePos) -> Result<Value, EvalError> {
    let mut last = None;

    for expr in exprs {
        last = Some(eval_expr(expr, env)?);
    }

    last.ok_or_else(|| syntax_error(empty_pos, "empty input"))
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    Ok((eval_str(input)?, String::new()))
}

fn eval_expr(expr: &Expr, env: &EnvRef) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(name) => lookup_symbol(env, name, expr.pos),
        ExprKind::List(items) => eval_list(items, env, expr.pos),
    }
}

fn lookup_symbol(env: &EnvRef, name: &str, pos: SourcePos) -> Result<Value, EvalError> {
    if let Some(value) = env_lookup(env, name) {
        return Ok(value);
    }

    if let Some(name) = builtin_name(name) {
        return Ok(Value::Procedure(Procedure::Builtin(name)));
    }

    Err(EvalError::UnboundVariable {
        pos,
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

fn eval_list(items: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    if items.is_empty() {
        return Err(syntax_error(pos, "cannot evaluate empty list"));
    }

    if let Some(name) = items[0].symbol_name() {
        let form_pos = items[0].pos;
        match name {
            "begin" => return eval_begin(&items[1..], env, form_pos),
            "cond" => return eval_cond(&items[1..], env),
            "define" => return eval_define(&items[1..], env, form_pos),
            "if" => return eval_if(&items[1..], env, form_pos),
            "let" => return eval_let(&items[1..], env, form_pos),
            "quote" => return eval_quote(&items[1..], form_pos),
            "lambda" => return eval_lambda(None, &items[1..], env, form_pos),
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

    apply(operator, &args, items[0].pos)
}

fn eval_define(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [signature, body @ ..] if signature.list_items().is_some() => {
            let (name, params) = parse_define_signature(signature)?;
            let lambda = make_lambda(Some(name.clone()), params, body, env, pos)?;
            env_define(env, name, lambda);
            Ok(Value::Void)
        }
        [name_expr, value_expr] => {
            if let Some(name) = name_expr.symbol_name() {
                let value = eval_expr(value_expr, env)?;
                env_define(env, name.to_string(), value);
                Ok(Value::Void)
            } else {
                Err(syntax_error(pos, "invalid define form"))
            }
        }
        _ => Err(syntax_error(pos, "invalid define form")),
    }
}

fn eval_begin(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    eval_sequence(args, env, pos)
}

fn parse_define_signature(signature: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let items = signature
        .list_items()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let (name_expr, params) = items
        .split_first()
        .ok_or_else(|| syntax_error(signature.pos, "invalid define form"))?;

    let name = name_expr
        .symbol_name()
        .ok_or_else(|| syntax_error(name_expr.pos, "function name must be a symbol"))?
        .to_string();

    let params = parse_params(params)?;
    Ok((name, params))
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|expr| {
            expr.symbol_name()
                .map(str::to_string)
                .ok_or_else(|| syntax_error(expr.pos, "parameter name must be a symbol"))
        })
        .collect()
}

fn eval_cond(clauses: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "cond clauses must be lists"))?;

        let (test, body) = items
            .split_first()
            .ok_or_else(|| syntax_error(clause.pos, "cond clause cannot be empty"))?;

        if test.symbol_name() == Some("else") {
            if index + 1 != clauses.len() {
                return Err(syntax_error(test.pos, "else clause must be last"));
            }
            return eval_sequence(body, env, clause.pos);
        }

        let test_value = eval_expr(test, env)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_sequence(body, env, clause.pos)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_if(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [condition, when_true, when_false] => {
            if eval_expr(condition, env)?.is_truthy() {
                eval_expr(when_true, env)
            } else {
                eval_expr(when_false, env)
            }
        }
        _ => Err(wrong_arg_count(pos, "if", "exactly 3 arguments", args.len())),
    }
}

fn eval_let(args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    let (head, rest) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "let requires bindings and a body"))?;

    if let Some(bindings) = head.list_items() {
        eval_plain_let(bindings, rest, env, pos)
    } else if let Some(name) = head.symbol_name() {
        eval_named_let(name, rest, env, head.pos)
    } else {
        Err(syntax_error(head.pos, "invalid let form"))
    }
}

fn eval_plain_let(
    bindings: &[Expr],
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "let requires a body"));
    }

    let bindings = eval_bindings(bindings, env)?;
    let let_env = Env::new_child(env);

    for (name, value) in bindings {
        env_define(&let_env, name, value);
    }

    eval_sequence(body, &let_env, pos)
}

fn eval_named_let(name: &str, args: &[Expr], env: &EnvRef, pos: SourcePos) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "named let requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "named let requires a body"));
    }

    let bindings = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "let bindings must be a list"))?;
    let bindings = eval_bindings(bindings, env)?;

    let (params, values): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    let let_env = Env::new_child(env);
    let lambda = make_lambda(Some(name.to_string()), params, body, &let_env, pos)?;
    env_define(&let_env, name.to_string(), lambda.clone());
    apply(lambda, &values, pos)
}

fn eval_bindings(bindings: &[Expr], env: &EnvRef) -> Result<Vec<(String, Value)>, EvalError> {
    bindings
        .iter()
        .map(|binding| {
            let items = binding
                .list_items()
                .ok_or_else(|| syntax_error(binding.pos, "let bindings must be lists"))?;

            match items {
                [name_expr, value_expr] => {
                    let name = name_expr
                        .symbol_name()
                        .ok_or_else(|| {
                            syntax_error(
                                binding.pos,
                                "each let binding must contain a name and value",
                            )
                        })?
                        .to_string();
                    let value = eval_expr(value_expr, env)?;
                    Ok((name, value))
                }
                _ => Err(syntax_error(
                    binding.pos,
                    "each let binding must contain a name and value",
                )),
            }
        })
        .collect()
}

fn eval_quote(args: &[Expr], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [expr] => quote_expr(expr),
        _ => Err(wrong_arg_count(pos, "quote", "exactly 1 argument", args.len())),
    }
}

fn quote_expr(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(value) => Ok(Value::Integer(*value)),
        ExprKind::Boolean(value) => Ok(Value::Boolean(*value)),
        ExprKind::String(value) => Ok(Value::String(value.clone())),
        ExprKind::Symbol(value) => Ok(Value::Symbol(value.clone())),
        ExprKind::List(items) => items
            .iter()
            .map(quote_expr)
            .collect::<Result<Vec<_>, _>>()
            .map(Value::List),
    }
}

fn eval_lambda(
    name: Option<String>,
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    let (params_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "lambda requires parameters and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
    }

    let params = params_expr
        .list_items()
        .ok_or_else(|| syntax_error(params_expr.pos, "lambda parameters must be a list"))?;
    let params = parse_params(params)?;

    make_lambda(name, params, body, env, pos)
}

fn make_lambda(
    name: Option<String>,
    params: Vec<String>,
    body: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(syntax_error(pos, "lambda requires a body"));
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

fn apply(operator: Value, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(Procedure::Builtin(name)) => apply_builtin(name, args, pos),
        Value::Procedure(Procedure::Lambda(lambda)) => apply_lambda(lambda, args, pos),
        other => Err(EvalError::NotAProcedure {
            pos,
            found: other.render(),
        }),
    }
}

fn apply_lambda(lambda: Rc<Lambda>, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(wrong_arg_count(
            pos,
            lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            format!("exactly {} arguments", lambda.params.len()),
            args.len(),
        ));
    }

    let call_env = Env::new_child(&lambda.env);
    for (param, value) in lambda.params.iter().zip(args.iter()) {
        env_define(&call_env, param.clone(), value.clone());
    }

    eval_sequence(&lambda.body, &call_env, pos)
}

fn apply_builtin(name: &str, args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match name {
        "+" => add(args, pos),
        "-" => subtract(args, pos),
        "*" => multiply(args, pos),
        "/" => divide(args, pos),
        "<" => compare(name, args, pos, |left, right| left < right),
        ">" => compare(name, args, pos, |left, right| left > right),
        "=" => compare(name, args, pos, |left, right| left == right),
        "<=" => compare(name, args, pos, |left, right| left <= right),
        "append" => append(args, pos),
        "boolean?" => predicate(args, "boolean?", pos, |value| matches!(value, Value::Boolean(_))),
        "car" => car(args, pos),
        "cdr" => cdr(args, pos),
        "cons" => cons(args, pos),
        "length" => length(args, pos),
        "list" => Ok(Value::List(args.to_vec())),
        "not" => builtin_not(args, pos),
        "null?" => predicate(
            args,
            "null?",
            pos,
            |value| matches!(value, Value::List(items) if items.is_empty()),
        ),
        "number?" => predicate(args, "number?", pos, |value| matches!(value, Value::Integer(_))),
        "pair?" => predicate(
            args,
            "pair?",
            pos,
            |value| matches!(value, Value::List(items) if !items.is_empty()),
        ),
        "string?" => predicate(args, "string?", pos, |value| matches!(value, Value::String(_))),
        "symbol?" => predicate(args, "symbol?", pos, |value| matches!(value, Value::Symbol(_))),
        _ => unreachable!("unsupported builtin: {name}"),
    }
}

fn add(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("+", args, pos)?;
    Ok(Value::Integer(values.into_iter().sum()))
}

fn subtract(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("-", args, pos)?;
    match values.as_slice() {
        [] => Err(wrong_arg_count(pos, "-", "at least 1 argument", 0)),
        [value] => Ok(Value::Integer(-value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn multiply(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("*", args, pos)?;
    Ok(Value::Integer(values.into_iter().product()))
}

fn divide(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let values = expect_numbers("/", args, pos)?;
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| wrong_arg_count(pos, "/", "at least 2 arguments", 0))?;

    if rest.is_empty() {
        return Err(wrong_arg_count(pos, "/", "at least 2 arguments", 1));
    }

    let mut total = *first;
    for value in rest {
        if *value == 0 {
            return Err(EvalError::DivisionByZero { pos });
        }
        total /= value;
    }

    Ok(Value::Integer(total))
}

fn compare<F>(name: &str, args: &[Value], pos: SourcePos, predicate: F) -> Result<Value, EvalError>
where
    F: Fn(i64, i64) -> bool,
{
    let values = expect_numbers(name, args, pos)?;
    if values.len() < 2 {
        return Err(wrong_arg_count(
            pos,
            name,
            "at least 2 arguments",
            values.len(),
        ));
    }

    let result = values.windows(2).all(|pair| predicate(pair[0], pair[1]));
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(wrong_arg_count(pos, "not", "exactly 1 argument", args.len())),
    }
}

fn cons(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [head, Value::List(tail)] => {
            let mut items = Vec::with_capacity(tail.len() + 1);
            items.push(head.clone());
            items.extend(tail.iter().cloned());
            Ok(Value::List(items))
        }
        [_, other] => Err(type_mismatch(pos, "cons", "list", other.type_name())),
        _ => Err(wrong_arg_count(pos, "cons", "exactly 2 arguments", args.len())),
    }
}

fn car(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("car", args, pos)?;
    Ok(items[0].clone())
}

fn cdr(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let items = expect_non_empty_list("cdr", args, pos)?;
    Ok(Value::List(items[1..].to_vec()))
}

fn append(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    let mut items = Vec::new();

    for value in args {
        let list = expect_list("append", value, pos)?;
        items.extend(list.iter().cloned());
    }

    Ok(Value::List(items))
}

fn length(args: &[Value], pos: SourcePos) -> Result<Value, EvalError> {
    match args {
        [value] => Ok(Value::Integer(expect_list("length", value, pos)?.len() as i64)),
        _ => Err(wrong_arg_count(pos, "length", "exactly 1 argument", args.len())),
    }
}

fn predicate<F>(args: &[Value], name: &str, pos: SourcePos, test: F) -> Result<Value, EvalError>
where
    F: Fn(&Value) -> bool,
{
    match args {
        [value] => Ok(Value::Boolean(test(value))),
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
    }
}

fn expect_numbers(name: &str, args: &[Value], pos: SourcePos) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|value| match value {
            Value::Integer(number) => Ok(*number),
            other => Err(type_mismatch(pos, name, "number", other.type_name())),
        })
        .collect()
}

fn expect_list<'a>(name: &str, value: &'a Value, pos: SourcePos) -> Result<&'a [Value], EvalError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(type_mismatch(pos, name, "list", other.type_name())),
    }
}

fn expect_non_empty_list<'a>(
    name: &str,
    args: &'a [Value],
    pos: SourcePos,
) -> Result<&'a [Value], EvalError> {
    match args {
        [value] => match value {
            Value::List(items) if !items.is_empty() => Ok(items),
            other => Err(type_mismatch(pos, name, "pair", other.type_name())),
        },
        _ => Err(wrong_arg_count(pos, name, "exactly 1 argument", args.len())),
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
