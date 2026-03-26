pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

fn output_write(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

// ── Environment ─────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

#[derive(Debug, Clone)]
struct Env(Rc<RefCell<EnvInner>>);

impl Env {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: None,
        })))
    }

    fn with_parent(parent: &Env) -> Self {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.clone()),
        })))
    }

    fn get(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        if let Some(v) = inner.bindings.get(name) {
            Some(v.clone())
        } else if let Some(ref parent) = inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    fn set_existing(&self, name: &str, val: Value) -> bool {
        let mut inner = self.0.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), val);
            return true;
        }
        if let Some(ref parent) = inner.parent {
            parent.set_existing(name, val)
        } else {
            false
        }
    }
}

// ── Source Position ──────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span {
    line: usize,
    col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ── Values ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Lambda(Vec<String>, Option<String>, Vec<Expr>, Env),
    Builtin(String),
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Void, Value::Void) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                _ => write!(f, "#\\{}", c),
            },
            Value::Pair(a, b) => write!(f, "({} . {})", a, b),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Lambda(..) => write!(f, "#<procedure>"),
            Value::Builtin(..) => write!(f, "#<procedure>"),
            Value::Void => Ok(()),
        }
    }
}

// ── Tokenizer ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Quote,
}

#[derive(Debug, Clone)]
struct SpannedToken {
    token: Token,
    span: Span,
}

fn is_delimiter(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
}

fn tokenize(input: &str) -> Result<Vec<SpannedToken>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' { i += 1; col += 1; }
            }
            '(' => { tokens.push(SpannedToken { token: Token::LParen, span: Span { line, col } }); i += 1; col += 1; }
            ')' => { tokens.push(SpannedToken { token: Token::RParen, span: Span { line, col } }); i += 1; col += 1; }
            '\'' => { tokens.push(SpannedToken { token: Token::Quote, span: Span { line, col } }); i += 1; col += 1; }
            '"' => {
                let start_span = Span { line, col };
                i += 1; col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1; col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        if chars[i] == '\n' { line += 1; col = 0; }
                        s.push(chars[i]);
                    }
                    i += 1; col += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; col += 1;
                tokens.push(SpannedToken { token: Token::Str(s), span: start_span });
            }
            '#' => {
                let start_span = Span { line, col };
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(SpannedToken { token: Token::Boolean(true), span: start_span });
                                i += 2; col += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #t".into()));
                            }
                        }
                        'f' => {
                            if i + 2 >= chars.len() || is_delimiter(chars[i + 2]) {
                                tokens.push(SpannedToken { token: Token::Boolean(false), span: start_span });
                                i += 2; col += 2;
                            } else {
                                return Err(EvalError::Parse("unexpected character after #f".into()));
                            }
                        }
                        '\\' => {
                            // Character literal: #\x, #\space, #\newline
                            i += 2; col += 2;
                            if i >= chars.len() {
                                return Err(EvalError::Parse("unexpected end of character literal".into()));
                            }
                            let start_ch = i;
                            // Read the character name (could be multi-char like "space", "newline")
                            if chars[i].is_alphabetic() && i + 1 < chars.len() && chars[i + 1].is_alphabetic() {
                                // Multi-character name
                                while i < chars.len() && !is_delimiter(chars[i]) && chars[i] != ')' {
                                    i += 1; col += 1;
                                }
                                let name: String = chars[start_ch..i].iter().collect();
                                let c = match name.as_str() {
                                    "space" => ' ',
                                    "newline" => '\n',
                                    "tab" => '\t',
                                    _ => return Err(EvalError::Parse(format!("unknown character name: {}", name))),
                                };
                                tokens.push(SpannedToken { token: Token::Char(c), span: start_span });
                            } else {
                                // Single character
                                let c = chars[i];
                                i += 1; col += 1;
                                tokens.push(SpannedToken { token: Token::Char(c), span: start_span });
                            }
                        }
                        _ => return Err(EvalError::Parse(format!("unexpected character after #: {}", chars[i + 1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                let start_span = Span { line, col };
                let start = i;
                while i < chars.len() && !is_delimiter(chars[i]) {
                    i += 1; col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(SpannedToken { token: Token::Integer(n), span: start_span });
                } else {
                    tokens.push(SpannedToken { token: Token::Symbol(word), span: start_span });
                }
            }
        }
    }
    Ok(tokens)
}

// ── Parser ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Expr>),
}

fn parse(tokens: &[SpannedToken], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let st = &tokens[*pos];
    let span = st.span;
    match &st.token {
        Token::Integer(n) => { let n = *n; *pos += 1; Ok(Expr { kind: ExprKind::Integer(n), span }) }
        Token::Boolean(b) => { let b = *b; *pos += 1; Ok(Expr { kind: ExprKind::Boolean(b), span }) }
        Token::Str(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Str(s), span }) }
        Token::Symbol(s) => { let s = s.clone(); *pos += 1; Ok(Expr { kind: ExprKind::Symbol(s), span }) }
        Token::Char(c) => { let c = *c; *pos += 1; Ok(Expr { kind: ExprKind::Char(c), span }) }
        Token::Quote => {
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), span },
                    inner,
                ]),
                span,
            })
        }
        Token::LParen => {
            *pos += 1;
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos].token != Token::RParen {
                items.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing parenthesis".into()));
            }
            *pos += 1; // consume RParen
            Ok(Expr { kind: ExprKind::List(items), span })
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(tokens: &[SpannedToken]) -> Result<Vec<Expr>, EvalError> {
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Evaluator ───────────────────────────────────────────────────────

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

/// Wrap an error with span info if it doesn't already have position info.
fn with_span(err: EvalError, span: Span) -> EvalError {
    let msg = err.to_string();
    // Don't double-annotate — check for pattern like "at N:N"
    if msg.contains(&format!("at {}", span)) {
        return err;
    }
    EvalError::Generic(format!("{} at {}", msg, span))
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    eval_inner(expr, env).map_err(|e| with_span(e, expr.span))
}

fn eval_inner(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => {
            env.get(s).ok_or_else(|| EvalError::UnboundVariable(s.clone()))
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&items[1..], env),
                    "if" => return eval_if(&items[1..], env),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires params and body".into()));
                        }
                        let (params, rest) = match &items[1].kind {
                            ExprKind::List(ps) => parse_params(ps)?,
                            ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                            _ => return Err(EvalError::Parse("lambda params must be a list or symbol".into())),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, rest, body, env.clone()));
                    }
                    "+" => return eval_add(&items[1..], env),
                    "-" => return eval_sub(&items[1..], env),
                    "*" => return eval_mul(&items[1..], env),
                    "/" => return eval_div(&items[1..], env),
                    "<" => return eval_cmp(&items[1..], env, |a, b| a < b),
                    ">" => return eval_cmp(&items[1..], env, |a, b| a > b),
                    "=" => return eval_cmp(&items[1..], env, |a, b| a == b),
                    "<=" => return eval_cmp(&items[1..], env, |a, b| a <= b),
                    ">=" => return eval_cmp(&items[1..], env, |a, b| a >= b),
                    "not" => return eval_not(&items[1..], env),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    "let" => return eval_let(&items[1..], env),
                    "cons" => return eval_cons(&items[1..], env),
                    "car" => return eval_car(&items[1..], env),
                    "cdr" => return eval_cdr(&items[1..], env),
                    "null?" => return eval_null(&items[1..], env),
                    "list" => return eval_list(&items[1..], env),
                    "length" => return eval_length(&items[1..], env),
                    "append" => return eval_append(&items[1..], env),
                    "number?" => return eval_type_pred(&items[1..], env, "number"),
                    "string?" => return eval_type_pred(&items[1..], env, "string"),
                    "boolean?" => return eval_type_pred(&items[1..], env, "boolean"),
                    "pair?" => return eval_type_pred(&items[1..], env, "pair"),
                    "symbol?" => return eval_type_pred(&items[1..], env, "symbol"),
                    "char?" => return eval_type_pred(&items[1..], env, "char"),
                    "display" => return eval_display(&items[1..], env),
                    "write" => return eval_write(&items[1..], env),
                    "newline" => return eval_newline(&items[1..]),
                    "string-append" => return eval_string_append(&items[1..], env),
                    "string-length" => return eval_string_length(&items[1..], env),
                    "substring" => return eval_substring(&items[1..], env),
                    "string->number" => return eval_string_to_number(&items[1..], env),
                    "number->string" => return eval_number_to_string(&items[1..], env),
                    "symbol->string" => return eval_symbol_to_string(&items[1..], env),
                    "string->symbol" => return eval_string_to_symbol(&items[1..], env),
                    "string-ref" => return eval_string_ref(&items[1..], env),
                    "string-copy" => return eval_string_copy(&items[1..], env),
                    "string-set!" => return eval_string_set(&items[1..], env),
                    "set!" => return eval_set(&items[1..], env),
                    "eq?" | "equal?" | "abs" | "modulo" | "remainder" | "quotient"
                    | "min" | "max" | "expt" | "zero?" | "positive?" | "negative?"
                    | "odd?" | "even?" | "list-ref" | "list-tail" | "list?" | "assoc"
                    | "map" | "for-each"
                    | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
                    | "char=?" | "char<?"
                    | "string=?" | "string<?" | "string-ci=?" | "string-upcase" | "string-downcase" => {
                        // Handled via apply_builtin
                        let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
                        return apply_builtin(op, &args?);
                    }
                    _ => {}
                }
            }
            // Function application
            let func = eval(&items[0], env)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env)).collect();
            let args = args?;
            apply(&func, &args)
        }
    }
}

fn parse_params(exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let dot_pos = exprs.iter().position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "."));
    if let Some(pos) = dot_pos {
        let fixed: Vec<String> = exprs[..pos].iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse("param must be symbol".into())),
        }).collect::<Result<_, _>>()?;
        if pos + 2 != exprs.len() {
            return Err(EvalError::Parse("expected exactly one symbol after dot".into()));
        }
        let rest = match &exprs[pos + 1].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Parse("rest param must be symbol".into())),
        };
        Ok((fixed, Some(rest)))
    } else {
        let params: Vec<String> = exprs.iter().map(|e| match &e.kind {
            ExprKind::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse("param must be symbol".into())),
        }).collect::<Result<_, _>>()?;
        Ok((params, None))
    }
}

fn apply(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            if let Some(rest_name) = rest {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} args, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} args, got {}", params.len(), args.len()
                )));
            }
            let call_env = Env::with_parent(closure_env);
            for (p, a) in params.iter().zip(args) {
                call_env.set(p.clone(), a.clone());
            }
            if let Some(rest_name) = rest {
                let rest_args = args[params.len()..].to_vec();
                call_env.set(rest_name.clone(), Value::List(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &call_env)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => apply_builtin(name, args),
        other => Err(EvalError::Type(format!("not a procedure: {}", other))),
    }
}

fn apply_builtin(name: &str, args: &[Value]) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum = 0i64;
            for a in args { sum += require_int(a)?; }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() { return Err(EvalError::Arity("- requires at least 1 argument".into())); }
            let first = require_int(&args[0])?;
            if args.len() == 1 { return Ok(Value::Integer(-first)); }
            let mut result = first;
            for a in &args[1..] { result -= require_int(a)?; }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product = 1i64;
            for a in args { product *= require_int(a)?; }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() { return Err(EvalError::Arity("/ requires at least 1 argument".into())); }
            let first = require_int(&args[0])?;
            if args.len() == 1 {
                if first == 0 { return Err(EvalError::DivisionByZero); }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = require_int(a)?;
                if d == 0 { return Err(EvalError::DivisionByZero); }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "=" => {
            if args.len() < 2 { return Err(EvalError::Arity("= requires at least 2 arguments".into())); }
            let mut prev = require_int(&args[0])?;
            for a in &args[1..] { let c = require_int(a)?; if prev != c { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "<" => {
            if args.len() < 2 { return Err(EvalError::Arity("< requires at least 2 arguments".into())); }
            let mut prev = require_int(&args[0])?;
            for a in &args[1..] { let c = require_int(a)?; if !(prev < c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        ">" => {
            if args.len() < 2 { return Err(EvalError::Arity("> requires at least 2 arguments".into())); }
            let mut prev = require_int(&args[0])?;
            for a in &args[1..] { let c = require_int(a)?; if !(prev > c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "<=" => {
            if args.len() < 2 { return Err(EvalError::Arity("<= requires at least 2 arguments".into())); }
            let mut prev = require_int(&args[0])?;
            for a in &args[1..] { let c = require_int(a)?; if !(prev <= c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        ">=" => {
            if args.len() < 2 { return Err(EvalError::Arity(">= requires at least 2 arguments".into())); }
            let mut prev = require_int(&args[0])?;
            for a in &args[1..] { let c = require_int(a)?; if !(prev >= c) { return Ok(Value::Boolean(false)); } prev = c; }
            Ok(Value::Boolean(true))
        }
        "not" => {
            if args.len() != 1 { return Err(EvalError::Arity("not requires exactly 1 argument".into())); }
            Ok(Value::Boolean(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 { return Err(EvalError::Arity("cons requires exactly 2 arguments".into())); }
            match &args[1] {
                Value::List(items) => {
                    let mut new = vec![args[0].clone()];
                    new.extend(items.iter().cloned());
                    Ok(Value::List(new))
                }
                _ => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
            }
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity("car requires exactly 1 argument".into())); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(a, _) => Ok(*a.clone()),
                Value::List(_) => Err(EvalError::Type("car: empty list".into())),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdr requires exactly 1 argument".into())); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::Pair(_, b) => Ok(*b.clone()),
                Value::List(_) => Err(EvalError::Type("cdr: empty list".into())),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity("null? requires exactly 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity("length requires exactly 1 argument".into())); }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::Type("length: not a list".into())),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for a in args {
                match a {
                    Value::List(items) => result.extend(items.iter().cloned()),
                    _ => return Err(EvalError::Type("append: not a list".into())),
                }
            }
            Ok(Value::List(result))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires exactly 1 argument".into())); }
            output_write(&display_value(&args[0]));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires exactly 1 argument".into())); }
            output_write(&args[0].to_string());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline takes no arguments".into())); }
            output_write("\n");
            Ok(Value::Void)
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
            }
            let proc = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(items) => items.clone(),
                _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail);
            apply(proc, &all_args)
        }
        "number?" => { if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Integer(_)))) }
        "string?" => { if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Str(_)))) }
        "boolean?" => { if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Boolean(_)))) }
        "pair?" => { if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); } Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty()) || matches!(&args[0], Value::Pair(_, _)))) }
        "symbol?" => { if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Symbol(_)))) }
        "char?" => { if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); } Ok(Value::Boolean(matches!(args[0], Value::Char(_)))) }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a { Value::Str(s) => result.push_str(s), other => return Err(EvalError::Type(format!("string-append: expected string, got {}", other))) }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Integer(s.len() as i64)), other => Err(EvalError::Type(format!("string-length: expected string, got {}", other))) }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
            Ok(Value::Str(require_int(&args[0])?.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
            match &args[0] { Value::Symbol(s) => Ok(Value::Str(s.clone())), other => Err(EvalError::Type(format!("symbol->string: expected symbol, got {}", other))) }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Symbol(s.clone())), other => Err(EvalError::Type(format!("string->symbol: expected string, got {}", other))) }
        }
        // ── L09: eq? / equal? ──
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
            let r = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
                _ => false,
            };
            Ok(Value::Boolean(r))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        // ── L09: numeric utilities ──
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
            Ok(Value::Integer(require_int(&args[0])?.abs()))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a / b)) // truncates toward zero in Rust
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a % b)) // Rust % has sign of dividend = remainder semantics
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
            let a = require_int(&args[0])?;
            let b = require_int(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(((a % b) + b) % b)) // sign of divisor
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
            let mut m = require_int(&args[0])?;
            for a in &args[1..] { let v = require_int(a)?; if v < m { m = v; } }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
            let mut m = require_int(&args[0])?;
            for a in &args[1..] { let v = require_int(a)?; if v > m { m = v; } }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
            let base = require_int(&args[0])?;
            let exp = require_int(&args[1])?;
            if exp < 0 { return Ok(Value::Integer(0)); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
            Ok(Value::Boolean(require_int(&args[0])? % 2 == 0))
        }
        // ── L09: list utilities ──
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
            let items = require_list(&args[0])?;
            let idx = require_int(&args[1])? as usize;
            items.get(idx).cloned().ok_or_else(|| EvalError::Generic(format!("list-ref: index {} out of range", idx)))
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
            let items = require_list(&args[0])?;
            let idx = require_int(&args[1])? as usize;
            if idx > items.len() { return Err(EvalError::Generic("list-tail: index out of range".into())); }
            Ok(Value::List(items[idx..].to_vec()))
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
            let key = &args[0];
            let alist = require_list(&args[1])?;
            for item in &alist {
                if let Value::List(pair) = item {
                    if !pair.is_empty() && values_equal(&pair[0], key) {
                        return Ok(item.clone());
                    }
                }
            }
            Ok(Value::Boolean(false))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
            let func = &args[0];
            let lists: Result<Vec<Vec<Value>>, _> = args[1..].iter().map(|a| require_list(a)).collect();
            let lists = lists?;
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len { return Err(EvalError::Generic("map: lists must have same length".into())); }
            }
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply(func, &call_args)?);
            }
            Ok(Value::List(result))
        }
        "for-each" => {
            if args.len() < 2 { return Err(EvalError::Arity("for-each requires at least 2 arguments".into())); }
            let func = &args[0];
            let lists: Result<Vec<Vec<Value>>, _> = args[1..].iter().map(|a| require_list(a)).collect();
            let lists = lists?;
            let len = lists[0].len();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                apply(func, &call_args)?;
            }
            Ok(Value::Void)
        }
        // ── L09: char operations ──
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
            Ok(Value::Boolean(require_char(&args[0])?.is_alphabetic()))
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
            Ok(Value::Boolean(require_char(&args[0])?.is_ascii_digit()))
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
            Ok(Value::Char(require_char(&args[0])?.to_ascii_uppercase()))
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
            Ok(Value::Char(require_char(&args[0])?.to_ascii_lowercase()))
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_char(&args[0])? == require_char(&args[1])?))
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_char(&args[0])? < require_char(&args[1])?))
        }
        // ── L09: string operations ──
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? == require_str(&args[1])?))
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])? < require_str(&args[1])?))
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
            Ok(Value::Boolean(require_str(&args[0])?.to_lowercase() == require_str(&args[1])?.to_lowercase()))
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
            Ok(Value::Str(require_str(&args[0])?.to_uppercase()))
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
            Ok(Value::Str(require_str(&args[0])?.to_lowercase()))
        }
        _ => Err(EvalError::Generic(format!("unknown builtin: {}", name))),
    }
}

fn eval_define(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env)?;
            env.set(name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(parts) => {
            // (define (f x y) body...) => (define f (lambda (x y) body...))
            // (define (f x . rest) body...) => (define f (lambda (x . rest) body...))
            if parts.is_empty() {
                return Err(EvalError::Parse("define: empty name list".into()));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse("define: name must be symbol".into())),
            };
            let (params, rest) = parse_params(&parts[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda(params, rest, body, env.clone());
            env.set(name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse("define: first argument must be symbol or list".into())),
    }
}

fn eval_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("set! requires exactly 2 arguments".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse("set!: first argument must be a symbol".into())),
    };
    let val = eval(&args[1], env)?;
    if !env.set_existing(&name, val) {
        return Err(EvalError::UnboundVariable(name));
    }
    Ok(Value::Void)
}

fn eval_if(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
    }
    let cond = eval(&args[0], env)?;
    if is_truthy(&cond) {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn require_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        other => Err(EvalError::Type(format!("expected integer, got {}", other))),
    }
}

fn require_char(v: &Value) -> Result<char, EvalError> {
    match v {
        Value::Char(c) => Ok(*c),
        other => Err(EvalError::Type(format!("expected char, got {}", other))),
    }
}

fn require_str(v: &Value) -> Result<String, EvalError> {
    match v {
        Value::Str(s) => Ok(s.clone()),
        other => Err(EvalError::Type(format!("expected string, got {}", other))),
    }
}

fn require_list(v: &Value) -> Result<Vec<Value>, EvalError> {
    match v {
        Value::List(items) => Ok(items.clone()),
        other => Err(EvalError::Type(format!("expected list, got {}", other))),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.len() == y.len() && x.iter().zip(y).all(|(a, b)| values_equal(a, b)),
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => values_equal(a1, b1) && values_equal(a2, b2),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

fn eval_add(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut sum = 0i64;
    for a in args {
        sum += require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(sum))
}

fn eval_sub(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for a in &args[1..] {
        result -= require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(result))
}

fn eval_mul(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut product = 1i64;
    for a in args {
        product *= require_int(&eval(a, env)?)?;
    }
    Ok(Value::Integer(product))
}

fn eval_div(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = require_int(&eval(&args[0], env)?)?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for a in &args[1..] {
        let d = require_int(&eval(a, env)?)?;
        if d == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn eval_cmp(args: &[Expr], env: &Env, cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = require_int(&eval(&args[0], env)?)?;
    for a in &args[1..] {
        let curr = require_int(&eval(a, env)?)?;
        if !cmp(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn eval_not(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
    }
    let v = eval(&args[0], env)?;
    Ok(Value::Boolean(!is_truthy(&v)))
}

fn eval_and(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a, env)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a, env)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for a in args {
        result = eval(a, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if is_truthy(&test) {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse("let: bindings must be a list".into())),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    match &pair[0].kind {
                        ExprKind::Symbol(s) => params.push(s.clone()),
                        _ => return Err(EvalError::Parse("let: binding name must be symbol".into())),
                    }
                    inits.push(&pair[1]);
                }
                _ => return Err(EvalError::Parse("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let lambda = Value::Lambda(params.clone(), None, body, env.clone());
        let let_env = Env::with_parent(env);
        let_env.set(name.clone(), lambda.clone());
        // Re-create lambda with let_env so it can see itself
        let body2 = match &lambda { Value::Lambda(_, _, b, _) => b.clone(), _ => unreachable!() };
        let lambda2 = Value::Lambda(params, None, body2, let_env.clone());
        let_env.set(name.clone(), lambda2.clone());
        let init_vals: Result<Vec<Value>, _> = inits.iter().map(|e| eval(e, env)).collect();
        return apply(&lambda2, &init_vals?);
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse("let: bindings must be a list".into())),
    };
    let let_env = Env::with_parent(env);
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse("let: binding name must be symbol".into())),
                };
                let val = eval(&pair[1], env)?;
                let_env.set(name, val);
            }
            _ => return Err(EvalError::Parse("let: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &let_env)?;
    }
    Ok(result)
}

fn eval_cons(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
    }
    let head = eval(&args[0], env)?;
    let tail = eval(&args[1], env)?;
    match tail {
        Value::List(mut items) => {
            items.insert(0, head);
            Ok(Value::List(items))
        }
        _ => Ok(Value::Pair(Box::new(head), Box::new(tail))),
    }
}

fn eval_car(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
        Value::Pair(a, _) => Ok(*a),
        Value::List(_) => Err(EvalError::Type("car: empty list".into())),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn eval_cdr(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
        Value::Pair(_, b) => Ok(*b),
        Value::List(_) => Err(EvalError::Type("cdr: empty list".into())),
        _ => Err(EvalError::Type("cdr: not a pair".into())),
    }
}

fn eval_null(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) => Ok(Value::Boolean(items.is_empty())),
        _ => Ok(Value::Boolean(false)),
    }
}

fn eval_list(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let items: Result<Vec<Value>, _> = args.iter().map(|a| eval(a, env)).collect();
    Ok(Value::List(items?))
}

fn eval_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::List(items) => Ok(Value::Integer(items.len() as i64)),
        _ => Err(EvalError::Type("length: not a list".into())),
    }
}

fn eval_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for a in args {
        match eval(a, env)? {
            Value::List(items) => result.extend(items),
            _ => return Err(EvalError::Type("append: not a list".into())),
        }
    }
    Ok(Value::List(result))
}

fn eval_type_pred(args: &[Expr], env: &Env, kind: &str) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{}? requires exactly 1 argument", kind)));
    }
    let val = eval(&args[0], env)?;
    let result = match kind {
        "number" => matches!(val, Value::Integer(_)),
        "string" => matches!(val, Value::Str(_)),
        "boolean" => matches!(val, Value::Boolean(_)),
        "pair" => matches!(val, Value::List(ref items) if !items.is_empty()) || matches!(val, Value::Pair(_, _)),
        "symbol" => matches!(val, Value::Symbol(_)),
        "char" => matches!(val, Value::Char(_)),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

// ── Display / Write / Newline ────────────────────────────────────────

fn display_value(val: &Value) -> String {
    match val {
        Value::Str(s) => s.clone(), // no quotes
        Value::Char(c) => c.to_string(),
        Value::Pair(a, b) => format!("({} . {})", display_value(a), display_value(b)),
        other => other.to_string(),
    }
}

fn eval_display(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    output_write(&display_value(&val));
    Ok(Value::Void)
}

fn eval_write(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write requires exactly 1 argument".into()));
    }
    let val = eval(&args[0], env)?;
    output_write(&val.to_string());
    Ok(Value::Void)
}

fn eval_newline(args: &[Expr]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline takes no arguments".into()));
    }
    output_write("\n");
    Ok(Value::Void)
}

// ── String / Symbol / Char operations ───────────────────────────────

fn eval_string_append(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match eval(a, env)? {
            Value::Str(s) => result.push_str(&s),
            other => return Err(EvalError::Type(format!("string-append: expected string, got {}", other))),
        }
    }
    Ok(Value::Str(result))
}

fn eval_string_length(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        other => Err(EvalError::Type(format!("string-length: expected string, got {}", other))),
    }
}

fn eval_substring(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring requires exactly 3 arguments".into()));
    }
    let s = match eval(&args[0], env)? {
        Value::Str(s) => s,
        other => return Err(EvalError::Type(format!("substring: expected string, got {}", other))),
    };
    let start = require_int(&eval(&args[1], env)?)? as usize;
    let end = require_int(&eval(&args[2], env)?)? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::Generic(format!("substring: index out of range")));
    }
    Ok(Value::Str(s[start..end].to_string()))
}

fn eval_string_to_number(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => {
            match s.parse::<i64>() {
                Ok(n) => Ok(Value::Integer(n)),
                Err(_) => Ok(Value::Boolean(false)),
            }
        }
        other => Err(EvalError::Type(format!("string->number: expected string, got {}", other))),
    }
}

fn eval_number_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string requires exactly 1 argument".into()));
    }
    let n = require_int(&eval(&args[0], env)?)?;
    Ok(Value::Str(n.to_string()))
}

fn eval_symbol_to_string(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Symbol(s) => Ok(Value::Str(s)),
        other => Err(EvalError::Type(format!("symbol->string: expected symbol, got {}", other))),
    }
}

fn eval_string_to_symbol(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Symbol(s)),
        other => Err(EvalError::Type(format!("string->symbol: expected string, got {}", other))),
    }
}

fn eval_string_ref(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref requires exactly 2 arguments".into()));
    }
    let s = match eval(&args[0], env)? {
        Value::Str(s) => s,
        other => return Err(EvalError::Type(format!("string-ref: expected string, got {}", other))),
    };
    let idx = require_int(&eval(&args[1], env)?)? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Generic(format!("string-ref: index {} out of range", idx))),
    }
}

fn eval_string_copy(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy requires exactly 1 argument".into()));
    }
    match eval(&args[0], env)? {
        Value::Str(s) => Ok(Value::Str(s)),
        other => Err(EvalError::Type(format!("string-copy: expected string, got {}", other))),
    }
}

fn eval_string_set(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires exactly 3 arguments".into()));
    }
    // First arg must be a variable name
    let var_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: first argument must be a variable".into())),
    };
    let idx = require_int(&eval(&args[1], env)?)? as usize;
    let ch = match eval(&args[2], env)? {
        Value::Char(c) => c,
        other => return Err(EvalError::Type(format!("string-set!: expected char, got {}", other))),
    };
    let s = match env.get(&var_name) {
        Some(Value::Str(s)) => s,
        Some(other) => return Err(EvalError::Type(format!("string-set!: expected string, got {}", other))),
        None => return Err(EvalError::UnboundVariable(var_name)),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Generic(format!("string-set!: index {} out of range", idx)));
    }
    chars[idx] = ch;
    let new_s: String = chars.into_iter().collect();
    if !env.set_existing(&var_name, Value::Str(new_s.clone())) {
        env.set(var_name, Value::Str(new_s));
    }
    Ok(Value::Void)
}

fn seed_builtins(env: &Env) {
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
        "not", "cons", "car", "cdr", "null?", "list", "length", "append",
        "number?", "string?", "boolean?", "pair?", "symbol?", "char?",
        "display", "write", "newline", "apply",
        "string-append", "string-length", "number->string",
        "symbol->string", "string->symbol",
        // L09
        "eq?", "equal?",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "map", "for-each",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
    ] {
        env.set(name.to_string(), Value::Builtin(name.to_string()));
    }
}

// ── Public API ──────────────────────────────────────────────────────

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    seed_builtins(&env);
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((last.to_string(), output))
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input)?;
    let exprs = parse_all(&tokens)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    seed_builtins(&env);
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

#[cfg(test)]
mod tests;
