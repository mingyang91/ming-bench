pub mod error;
mod builtins;
mod macros;
mod numeric;

pub use error::EvalError;

use builtins::eval_builtin;
use macros::{eval_define_syntax, expand_and_eval_macro};
use numeric::{f64_to_exact, is_number, make_rational, nums_equal, nums_less, value_to_f64, values_equal};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(crate) fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("#{}#{}", prefix, n)
}

pub(crate) type Frame = Rc<RefCell<HashMap<String, Value>>>;
pub(crate) type Env = Vec<Frame>;
type CaseClause = (Vec<String>, Option<String>, Vec<Expr>, Env);

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct Span {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, den > 0)
    Float(f64),
    Boolean(bool),
    Char(char),
    Str(Rc<RefCell<String>>, bool), // (data, mutable)
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Procedure(Vec<String>, Option<String>, Vec<Expr>, Env),
    Builtin(String),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Vec<Expr>, Expr)>,
        def_env: Env,
    },
    Record {
        type_tag: String,
        fields: Vec<(String, Value)>,
    },
    CaseLambda(Vec<CaseClause>),
    Vector(Rc<RefCell<Vec<Value>>>),
}


fn make_str(s: String) -> Value {
    Value::Str(Rc::new(RefCell::new(s)), true)
}

fn make_immutable_str(s: String) -> Value {
    Value::Str(Rc::new(RefCell::new(s)), false)
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Float(n) => {
                let s = format!("{n}");
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    write!(f, "{s}")
                } else {
                    write!(f, "{s}.0")
                }
            }
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::Str(s, _) => write!(f, "\"{}\"", s.borrow()),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Value::Pair(a, b) => write!(f, "({a} . {b})"),
            Value::Procedure(..) => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<procedure:{name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Record { type_tag, .. } => write!(f, "#<record:{type_tag}>"),
            Value::CaseLambda(..) => write!(f, "#<procedure>"),
            Value::Vector(v) => {
                write!(f, "#(")?;
                let items = v.borrow();
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
}

// ── Parser ──

struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input: input.as_bytes(), pos: 0 }
    }

    fn current_span(&self) -> Span {
        let mut line = 1;
        let mut col = 1;
        for &b in &self.input[..self.pos] {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Span { line, col }
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() {
                self.pos += 1;
            } else if b == b';' {
                while self.pos < self.input.len() && self.input[self.pos] != b'\n' {
                    self.pos += 1;
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() { Some(self.input[self.pos]) } else { None }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace_and_comments();
        let span = self.current_span();
        match self.peek() {
            None => Err(EvalError::Parse("unexpected end of input".into())),
            Some(b'\'') => {
                self.pos += 1;
                let inner = self.parse_expr()?;
                Ok(Expr { kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), span },
                    inner,
                ]), span })
            }
            Some(b'(') => self.parse_list(span),
            Some(b'"') => self.parse_string(span),
            Some(b'#') => self.parse_hash(span),
            _ => self.parse_atom(span),
        }
    }

    fn parse_list(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            match self.peek() {
                None => return Err(EvalError::Parse("unterminated list".into())),
                Some(b')') => { self.pos += 1; return Ok(Expr { kind: ExprKind::List(items), span }); }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip opening "
        let mut s = String::new();
        loop {
            if self.pos >= self.input.len() {
                return Err(EvalError::Parse("unterminated string".into()));
            }
            let b = self.input[self.pos];
            if b == b'"' { self.pos += 1; return Ok(Expr { kind: ExprKind::Str(s), span }); }
            if b == b'\\' {
                self.pos += 1;
                if self.pos >= self.input.len() {
                    return Err(EvalError::Parse("unterminated escape".into()));
                }
                match self.input[self.pos] {
                    b'n' => s.push('\n'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'"' => s.push('"'),
                    c => { s.push('\\'); s.push(c as char); }
                }
            } else {
                s.push(b as char);
            }
            self.pos += 1;
        }
    }

    fn parse_hash(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '#'
        match self.peek() {
            Some(b't') => { self.pos += 1; Ok(Expr { kind: ExprKind::Boolean(true), span }) }
            Some(b'f') => { self.pos += 1; Ok(Expr { kind: ExprKind::Boolean(false), span }) }
            Some(b'\\') => self.parse_char(span),
            Some(b'(') => {
                // Vector literal #(...)
                self.pos += 1; // skip '('
                let mut items = Vec::new();
                loop {
                    self.skip_whitespace_and_comments();
                    match self.peek() {
                        None => return Err(EvalError::Parse("unterminated vector literal".into())),
                        Some(b')') => { self.pos += 1; break; }
                        _ => items.push(self.parse_expr()?),
                    }
                }
                // Desugar #(a b c) into (vector a b c)
                let mut list_items = vec![Expr { kind: ExprKind::Symbol("vector".into()), span }];
                list_items.extend(items);
                Ok(Expr { kind: ExprKind::List(list_items), span })
            }
            _ => Err(EvalError::Parse("unexpected # literal".into())),
        }
    }

    fn parse_char(&mut self, span: Span) -> Result<Expr, EvalError> {
        self.pos += 1; // skip '\'
        if self.pos >= self.input.len() {
            return Err(EvalError::Parse("unexpected end of character literal".into()));
        }
        // Check for named characters like #\space, #\newline
        let start = self.pos;
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() || b == b'(' || b == b')' || b == b'"' || b == b';' {
                break;
            }
            self.pos += 1;
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("valid UTF-8");
        let c = match token {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string has a first char"),
            _ => return Err(EvalError::Parse(format!("unknown character name: {token}"))),
        };
        Ok(Expr { kind: ExprKind::Char(c), span })
    }

    fn parse_atom(&mut self, span: Span) -> Result<Expr, EvalError> {
        let start = self.pos;
        while self.pos < self.input.len() {
            let b = self.input[self.pos];
            if b.is_ascii_whitespace() || b == b'(' || b == b')' || b == b'"' || b == b';' {
                break;
            }
            self.pos += 1;
        }
        if self.pos == start {
            return Err(EvalError::Parse("unexpected character".into()));
        }
        let token = std::str::from_utf8(&self.input[start..self.pos]).expect("input is valid UTF-8");
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr { kind: ExprKind::Integer(n), span });
        }
        // Rational literal: digits/digits (possibly with leading -)
        if let Some(slash) = token.find('/') {
            if slash > 0 || (token.starts_with('-') && slash > 1) {
                let (num_s, den_s) = (&token[..slash], &token[slash + 1..]);
                if let (Ok(num), Ok(den)) = (num_s.parse::<i64>(), den_s.parse::<i64>()) {
                    if den != 0 {
                        return Ok(Expr { kind: ExprKind::Rational(num, den), span });
                    }
                }
            }
        }
        // Float literal
        if token.contains('.') || token.contains('e') || token.contains('E') {
            if let Ok(f) = token.parse::<f64>() {
                return Ok(Expr { kind: ExprKind::Float(f), span });
            }
        }
        Ok(Expr { kind: ExprKind::Symbol(token.to_string()), span })
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace_and_comments();
            if self.pos >= self.input.len() { break; }
            exprs.push(self.parse_expr()?);
        }
        if exprs.is_empty() {
            return Err(EvalError::Parse("empty input".into()));
        }
        Ok(exprs)
    }
}

// ── Evaluator ──

pub(crate) fn new_frame() -> Frame {
    Rc::new(RefCell::new(HashMap::new()))
}

pub(crate) fn env_lookup(env: &Env, name: &str) -> Result<Value, EvalError> {
    for frame in env.iter().rev() {
        if let Some(v) = frame.borrow().get(name).cloned() {
            return Ok(v);
        }
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

pub(crate) fn env_define(env: &Env, name: String, val: Value) {
    env.last().expect("env must have at least one frame").borrow_mut().insert(name, val);
}

fn env_set(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    for frame in env.iter().rev() {
        let mut f = frame.borrow_mut();
        if f.contains_key(name) {
            f.insert(name.to_string(), val);
            return Ok(());
        }
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn with_span(span: Span, err: EvalError) -> EvalError {
    let msg = err.to_string();
    if msg.as_bytes().windows(2).any(|w| w[0].is_ascii_digit() && w[1] == b':') {
        return err;
    }
    EvalError::Generic(format!("{}:{}: {}", span.line, span.col, msg))
}

pub(crate) fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let span = expr.span;
    eval_inner(expr, env, output).map_err(|e| with_span(span, e))
}

fn eval_inner(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Rational(n, d) => Ok(make_rational(*n, *d)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Str(s) => Ok(make_immutable_str(s.clone())),
        ExprKind::Symbol(name) => {
            match env_lookup(env, name) {
                Ok(v) => Ok(v),
                Err(_) if is_builtin(name) => Ok(Value::Builtin(name.clone())),
                Err(e) => Err(e),
            }
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env, output),
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into()));
                        }
                        if let ExprKind::Symbol(name) = &items[1].kind {
                            let val = eval(&items[2], env, output)?;
                            env_set(env, name, val)?;
                            return Ok(Value::Boolean(false));
                        } else {
                            return Err(EvalError::Type("set! requires a symbol".into()));
                        }
                    }
                    "if" => return eval_if(&items[1..], env, output),
                    "quote" => return eval_quote(&items[1..]),
                    "lambda" => return eval_lambda(&items[1..], env),
                    "case-lambda" => return eval_case_lambda(&items[1..], env),
                    "and" => return eval_and(&items[1..], env, output),
                    "or" => return eval_or(&items[1..], env, output),
                    "let" => return eval_let(&items[1..], env, output),
                    "begin" => return eval_begin(&items[1..], env, output),
                    "cond" => return eval_cond(&items[1..], env, output),
                    "define-syntax" => return eval_define_syntax(&items[1..], env),
                    "define-record-type" => return eval_define_record_type(&items[1..], env),
                    "letrec" => return eval_letrec(&items[1..], env, output),
                    "letrec*" => return eval_letrec_star(&items[1..], env, output),
                    "case" => return eval_case(&items[1..], env, output),
                    "do" => return eval_do(&items[1..], env, output),
                    _ => {}
                }
                // Check for macro invocation
                if let Ok(macro_val @ Value::Macro { .. }) = env_lookup(env, op) {
                    return expand_and_eval_macro(&macro_val, items, env, output);
                }
                if is_builtin(op) {
                    let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env, output)).collect::<Result<_, _>>()?;
                    return eval_builtin(op, &args, output);
                }
            }
            let func = eval(&items[0], env, output)?;
            let args: Vec<Value> = items[1..].iter().map(|a| eval(a, env, output)).collect::<Result<_, _>>()?;
            apply_proc(&func, &args, output)
        }
    }
}

// ── Tail Call Optimization (Trampoline) ──

enum TailResult {
    Done(Value),
    TailCall(Value, Vec<Value>),
}

fn eval_tail(expr: &Expr, env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    let span = expr.span;
    eval_tail_inner(expr, env, output).map_err(|e| with_span(span, e))
}

fn eval_tail_inner(expr: &Expr, env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "if" => return eval_if_tail(&items[1..], env, output),
                    "begin" => return eval_begin_tail(&items[1..], env, output),
                    "cond" => return eval_cond_tail(&items[1..], env, output),
                    "and" => return eval_and_tail(&items[1..], env, output),
                    "or" => return eval_or_tail(&items[1..], env, output),
                    "let" => return eval_let_tail(&items[1..], env, output),
                    "letrec" => return eval_letrec_tail(&items[1..], env, output),
                    "letrec*" => return eval_letrec_star_tail(&items[1..], env, output),
                    "define" | "set!" | "quote" | "lambda" | "case-lambda" |
                    "define-syntax" | "define-record-type" | "case" | "do" => {
                        return Ok(TailResult::Done(eval(expr, env, output)?));
                    }
                    _ => {}
                }
                if let Ok(macro_val @ Value::Macro { .. }) = env_lookup(env, op) {
                    return Ok(TailResult::Done(expand_and_eval_macro(&macro_val, items, env, output)?));
                }
                if is_builtin(op) {
                    let args: Vec<Value> = items[1..].iter()
                        .map(|a| eval(a, env, output))
                        .collect::<Result<_, _>>()?;
                    return Ok(TailResult::Done(eval_builtin(op, &args, output)?));
                }
            }
            // Function call in tail position
            let func = eval(&items[0], env, output)?;
            let args: Vec<Value> = items[1..].iter()
                .map(|a| eval(a, env, output))
                .collect::<Result<_, _>>()?;
            match &func {
                Value::Procedure(..) | Value::CaseLambda(..) => {
                    Ok(TailResult::TailCall(func, args))
                }
                _ => Ok(TailResult::Done(apply_proc(&func, &args, output)?)),
            }
        }
        _ => Ok(TailResult::Done(eval(expr, env, output)?)),
    }
}

fn eval_if_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval_tail(&args[1], env, output)
    } else if args.len() == 3 {
        eval_tail(&args[2], env, output)
    } else {
        Ok(TailResult::Done(Value::Boolean(false)))
    }
}

fn eval_body_tail(body: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if body.is_empty() {
        return Ok(TailResult::Done(Value::Boolean(false)));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, env, output)?;
    }
    eval_tail(&body[body.len() - 1], env, output)
}

fn eval_begin_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    eval_body_tail(args, env, output)
}

fn eval_cond_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    for clause in args {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        return eval_body_tail(&items[1..], env, output);
                    }
                }
                let test = eval(&items[0], env, output)?;
                if is_truthy(&test) {
                    if items.len() == 1 {
                        return Ok(TailResult::Done(test));
                    }
                    return eval_body_tail(&items[1..], env, output);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(TailResult::Done(Value::Boolean(false)))
}

fn eval_and_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.is_empty() {
        return Ok(TailResult::Done(Value::Boolean(true)));
    }
    for a in &args[..args.len() - 1] {
        let result = eval(a, env, output)?;
        if !is_truthy(&result) {
            return Ok(TailResult::Done(result));
        }
    }
    eval_tail(&args[args.len() - 1], env, output)
}

fn eval_or_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.is_empty() {
        return Ok(TailResult::Done(Value::Boolean(false)));
    }
    for a in &args[..args.len() - 1] {
        let result = eval(a, env, output)?;
        if is_truthy(&result) {
            return Ok(TailResult::Done(result));
        }
    }
    eval_tail(&args[args.len() - 1], env, output)
}

fn eval_let_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Type("let: binding name must be symbol".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let mut let_env = env.clone();
        let frame = new_frame();
        let_env.push(frame.clone());
        let proc = Value::Procedure(params, None, body, let_env.clone());
        frame.borrow_mut().insert(name.clone(), proc.clone());
        return Ok(TailResult::TailCall(proc, inits));
    }
    // Regular let
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let frame = new_frame();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    frame.borrow_mut().insert(s.clone(), val);
                } else {
                    return Err(EvalError::Type("let: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    env.push(frame);
    let result = eval_body_tail(&args[1..], env, output);
    env.pop();
    result
}

fn eval_letrec_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    env_define(env, s.clone(), Value::Boolean(false));
                } else {
                    return Err(EvalError::Type("letrec: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    for b in bindings {
        if let ExprKind::List(pair) = &b.kind {
            if let ExprKind::Symbol(s) = &pair[0].kind {
                let val = eval(&pair[1], env, output)?;
                env_set(env, s, val)?;
            }
        }
    }
    let result = eval_body_tail(&args[1..], env, output);
    env.pop();
    result
}

fn eval_letrec_star_tail(args: &[Expr], env: &mut Env, output: &mut String) -> Result<TailResult, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    env_define(env, s.clone(), val);
                } else {
                    return Err(EvalError::Type("letrec*: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let result = eval_body_tail(&args[1..], env, output);
    env.pop();
    result
}

fn apply_proc(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    let mut cur_func = func.clone();
    let mut cur_args = args.to_vec();

    loop {
        match cur_func {
            Value::Procedure(ref params, ref rest, ref body, ref closure_env) => {
                if rest.is_some() {
                    if cur_args.len() < params.len() {
                        return Err(EvalError::Arity(format!(
                            "expected at least {} arguments, got {}", params.len(), cur_args.len()
                        )));
                    }
                } else if cur_args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), cur_args.len()
                    )));
                }
                let mut new_env = closure_env.clone();
                let frame = new_frame();
                for (p, a) in params.iter().zip(cur_args.iter()) {
                    frame.borrow_mut().insert(p.clone(), a.clone());
                }
                if let Some(rest_name) = rest {
                    let rest_args = cur_args[params.len()..].to_vec();
                    frame.borrow_mut().insert(rest_name.clone(), Value::List(rest_args));
                }
                new_env.push(frame);

                if body.is_empty() {
                    return Ok(Value::Boolean(false));
                }
                // Evaluate all but last body expression
                for expr in &body[..body.len() - 1] {
                    eval(expr, &mut new_env, output)?;
                }
                // Last body expression in tail position
                match eval_tail(&body[body.len() - 1], &mut new_env, output)? {
                    TailResult::Done(v) => return Ok(v),
                    TailResult::TailCall(f, a) => {
                        cur_func = f;
                        cur_args = a;
                    }
                }
            }
            Value::CaseLambda(ref clauses) => {
                let mut found = false;
                for (params, rest, body, closure_env) in clauses {
                    let matches = if rest.is_some() {
                        cur_args.len() >= params.len()
                    } else {
                        cur_args.len() == params.len()
                    };
                    if matches {
                        cur_func = Value::Procedure(params.clone(), rest.clone(), body.clone(), closure_env.clone());
                        found = true;
                        break;
                    }
                }
                if !found {
                    return Err(EvalError::Arity(format!(
                        "case-lambda: no matching clause for {} arguments", cur_args.len()
                    )));
                }
            }
            Value::Builtin(ref name) => return eval_builtin(name, &cur_args, output),
            _ => return Err(EvalError::Type("not a procedure".into())),
        }
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn expect_integer(v: &Value, context: &str) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{context}: expected integer, got {v}"))),
    }
}

fn parse_params(items: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest = None;
    let mut i = 0;
    while i < items.len() {
        match &items[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= items.len() {
                    return Err(EvalError::Parse("expected rest parameter after dot".into()));
                }
                match &items[i + 1].kind {
                    ExprKind::Symbol(r) => rest = Some(r.clone()),
                    _ => return Err(EvalError::Type("rest parameter must be a symbol".into())),
                }
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(EvalError::Type("expected symbol as parameter".into())),
        }
    }
    Ok((params, rest))
}

fn eval_define(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env, output)?;
            env_define(env, name.clone(), val);
            Ok(Value::Boolean(false))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let (params, rest) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let proc = Value::Procedure(params, rest, body, env.clone());
            env_define(env, name, proc);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn eval_if(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("quote requires 1 argument".into()));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Str(s) => make_immutable_str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let (params, rest) = match &args[0].kind {
        ExprKind::List(items) => parse_params(items)?,
        ExprKind::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Procedure(params, rest, body, env.clone()))
}

fn eval_case_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            ExprKind::List(items) if items.len() >= 2 => {
                let (params, rest) = match &items[0].kind {
                    ExprKind::List(param_items) => parse_params(param_items)?,
                    ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Type("case-lambda: expected parameter list".into())),
                };
                let body = items[1..].to_vec();
                clauses.push((params, rest, body, env.clone()));
            }
            _ => return Err(EvalError::Type("case-lambda: invalid clause".into())),
        }
    }
    Ok(Value::CaseLambda(clauses))
}

fn eval_and(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

pub(crate) fn is_builtin(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
        | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
        | "integer?" | "rational?" | "exact?" | "inexact?"
        | "exact->inexact" | "inexact->exact"
        | "numerator" | "denominator"
        | "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string"
        | "symbol->string" | "string->symbol"
        | "string-ref" | "string-copy" | "string-set!" | "string->list" | "list->string"
        | "apply"
        | "eq?" | "equal?"
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "list-ref" | "list-tail" | "list?" | "assoc" | "map"
        | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?" | "char->integer" | "integer->char"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase"
        | "procedure?"
        | "eqv?"
        | "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector?" | "vector->list" | "list->vector")
}

fn display_value(v: &Value) -> String {
    match v {
        Value::Str(s, _) => s.borrow().clone(),
        Value::Char(c) => c.to_string(),
        Value::List(items) => {
            let mut s = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(item));
            }
            s.push(')');
            s
        }
        Value::Pair(a, b) => format!("({} . {})", display_value(a), display_value(b)),
        Value::Builtin(name) => format!("#<procedure:{name}>"),
        Value::Record { type_tag, .. } => format!("#<record:{type_tag}>"),
        Value::Vector(v) => {
            let items = v.borrow();
            let mut s = String::from("#(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(item));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

fn eval_let(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Type("let: binding name must be symbol".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let mut let_env = env.clone();
        let frame = new_frame();
        let_env.push(frame.clone());
        let proc = Value::Procedure(params.clone(), None, body, let_env.clone());
        frame.borrow_mut().insert(name.clone(), proc);
        let func = env_lookup(&let_env, name)?;
        return apply_proc(&func, &inits, output);
    }
    // Regular let: (let ((var init) ...) body ...)
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let frame = new_frame();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    frame.borrow_mut().insert(s.clone(), val);
                } else {
                    return Err(EvalError::Type("let: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    env.push(frame);
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

fn eval_letrec(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    // First pass: bind all names to undefined (we use #f as placeholder)
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    env_define(env, s.clone(), Value::Boolean(false));
                } else {
                    return Err(EvalError::Type("letrec: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    // Second pass: evaluate inits and set!
    for b in bindings {
        if let ExprKind::List(pair) = &b.kind {
            if let ExprKind::Symbol(s) = &pair[0].kind {
                let val = eval(&pair[1], env, output)?;
                env_set(env, s, val)?;
            }
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

fn eval_letrec_star(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let frame = new_frame();
    env.push(frame);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], env, output)?;
                    env_define(env, s.clone(), val);
                } else {
                    return Err(EvalError::Type("letrec*: binding name must be symbol".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, env, output)?;
    }
    env.pop();
    Ok(result)
}

fn eqv_match(val: &Value, datum: &Value) -> bool {
    match (val, datum) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn eval_body(body: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in body {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_case(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires at least 1 argument".into()));
    }
    let key = eval(&args[0], env, output)?;
    for clause in &args[1..] {
        let ExprKind::List(items) = &clause.kind else {
            return Err(EvalError::Type("case: invalid clause".into()));
        };
        if items.is_empty() {
            return Err(EvalError::Type("case: invalid clause".into()));
        }
        // Check for else clause
        if let ExprKind::Symbol(s) = &items[0].kind {
            if s == "else" {
                return eval_body(&items[1..], env, output);
            }
        }
        // Regular clause: ((datum ...) body ...)
        let ExprKind::List(datums) = &items[0].kind else { continue };
        let matched = datums.iter().any(|d| eqv_match(&key, &expr_to_value(d)));
        if matched {
            return eval_body(&items[1..], env, output);
        }
    }
    // No match, no else
    Ok(Value::Boolean(false))
}

fn eval_do(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable specs and test clause".into()));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("do: expected variable spec list".into())),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type("do: expected test clause".into())),
    };
    let body = &args[2..];

    // Parse variable specs: (var init [step])
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Expr>> = Vec::new();

    let frame = new_frame();
    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("do: variable name must be symbol".into())),
                };
                let init = eval(&parts[1], env, output)?;
                let step = if parts.len() >= 3 { Some(parts[2].clone()) } else { None };
                frame.borrow_mut().insert(name.clone(), init);
                var_names.push(name);
                step_exprs.push(step);
            }
            _ => return Err(EvalError::Type("do: invalid variable spec".into())),
        }
    }
    env.push(frame);

    loop {
        // Evaluate test
        let test = eval(&test_clause[0], env, output)?;
        if is_truthy(&test) {
            // Test is true: evaluate result expressions
            let mut result = Value::Boolean(false);
            for expr in &test_clause[1..] {
                result = eval(expr, env, output)?;
            }
            env.pop();
            return Ok(result);
        }
        // Execute body
        for expr in body {
            eval(expr, env, output)?;
        }
        // Evaluate step expressions using current values (parallel)
        let mut new_vals = Vec::new();
        for (i, step) in step_exprs.iter().enumerate() {
            if let Some(step_expr) = step {
                new_vals.push((var_names[i].clone(), eval(step_expr, env, output)?));
            }
        }
        // Update all at once
        for (name, val) in new_vals {
            env_set(env, &name, val)?;
        }
    }
}

fn eval_begin(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(args: &[Expr], env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in args {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &items[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&items[0], env, output)?;
                if is_truthy(&test) {
                    if items.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &items[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        }
    }
    Ok(Value::Boolean(false))
}

// ── Records (define-record-type) ──

fn eval_define_record_type(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    // Type name (used for unique tag)
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected type name symbol".into())),
    };
    let type_tag = gensym(&type_name);

    // Constructor: (make-foo field1 field2 ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define-record-type: constructor name must be symbol".into())),
            };
            let fields: Vec<String> = items[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define-record-type: field name must be symbol".into())),
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())),
    };

    // Predicate name
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())),
    };

    // Field accessors: (field accessor) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(items) if items.len() == 2 => {
                let field = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: field name must be symbol".into())),
                };
                let accessor = match &items[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: accessor name must be symbol".into())),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Type("define-record-type: invalid field spec".into())),
        }
    }

    // Define constructor as a builtin-like procedure
    let tag = type_tag.clone();
    let fields = constructor_fields.clone();
    let constructor_tag = tag.clone();
    let constructor_fields_clone = fields.clone();

    // We'll use special procedure values that carry record metadata.
    // Constructor: a procedure that creates Record values
    // We encode these as special Builtin names that we handle in apply.

    // Register constructor
    let ctor_id = format!("##record-ctor##{}", constructor_tag);
    // Store metadata: type_tag and field names in the environment
    let meta_key = format!("##record-meta##{}", constructor_tag);
    env_define(env, meta_key, Value::List(
        constructor_fields_clone.iter().map(|f| Value::Symbol(f.clone())).collect()
    ));
    env_define(env, constructor_name, Value::Builtin(ctor_id));

    // Register predicate
    let pred_id = format!("##record-pred##{}", tag);
    env_define(env, predicate_name, Value::Builtin(pred_id));

    // Register accessors
    for (field_name, accessor_name) in &field_accessors {
        // Find the index of this field in the constructor fields
        let idx = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Generic(format!(
                "define-record-type: field {} not in constructor", field_name
            )))?;
        let acc_id = format!("##record-acc##{}##{}", tag, idx);
        env_define(env, accessor_name.clone(), Value::Builtin(acc_id));
    }

    Ok(Value::Boolean(false))
}


/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut env = vec![new_frame()];
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let mut env = vec![new_frame()];
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
