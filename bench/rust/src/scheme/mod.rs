pub mod error;
mod builtins;

pub use error::EvalError;

use builtins::eval_builtin;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("#{}#{}", prefix, n)
}

type Frame = Rc<RefCell<HashMap<String, Value>>>;
type Env = Vec<Frame>;

#[derive(Debug, Clone, Copy, Default)]
struct Span {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, den > 0)
    Float(f64),
    Boolean(bool),
    Char(char),
    Str(Rc<RefCell<String>>),
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
}

fn num_gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational(num: i64, den: i64) -> Value {
    assert!(den != 0, "division by zero in make_rational");
    let sign = if den < 0 { -1 } else { 1 };
    let num = num * sign;
    let den = den.abs();
    let g = num_gcd(num.abs(), den);
    let num = num / g;
    let den = den / g;
    if den == 1 {
        Value::Integer(num)
    } else {
        Value::Rational(num, den)
    }
}

fn make_str(s: String) -> Value {
    Value::Str(Rc::new(RefCell::new(s)))
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
            Value::Str(s) => write!(f, "\"{}\"", s.borrow()),
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
        }
    }
}

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
enum ExprKind {
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

fn new_frame() -> Frame {
    Rc::new(RefCell::new(HashMap::new()))
}

fn env_lookup(env: &Env, name: &str) -> Result<Value, EvalError> {
    for frame in env.iter().rev() {
        if let Some(v) = frame.borrow().get(name).cloned() {
            return Ok(v);
        }
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn env_define(env: &Env, name: String, val: Value) {
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

fn eval(expr: &Expr, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
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
        ExprKind::Str(s) => Ok(make_str(s.clone())),
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
                    "and" => return eval_and(&items[1..], env, output),
                    "or" => return eval_or(&items[1..], env, output),
                    "let" => return eval_let(&items[1..], env, output),
                    "begin" => return eval_begin(&items[1..], env, output),
                    "cond" => return eval_cond(&items[1..], env, output),
                    "define-syntax" => return eval_define_syntax(&items[1..], env),
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

fn apply_proc(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Procedure(params, rest, body, closure_env) => {
            if let Some(_rest_name) = rest {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let mut new_env = closure_env.clone();
            let frame = new_frame();
            for (p, a) in params.iter().zip(args.iter()) {
                frame.borrow_mut().insert(p.clone(), a.clone());
            }
            if let Some(rest_name) = rest {
                let rest_args = args[params.len()..].to_vec();
                frame.borrow_mut().insert(rest_name.clone(), Value::List(rest_args));
            }
            new_env.push(frame);
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut new_env, output)?;
            }
            Ok(result)
        }
        Value::Builtin(name) => eval_builtin(name, args, output),
        _ => Err(EvalError::Type("not a procedure".into())),
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
        ExprKind::Str(s) => make_str(s.clone()),
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

fn is_builtin(op: &str) -> bool {
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
        | "string-ref" | "string-copy" | "string-set!"
        | "apply"
        | "eq?" | "equal?"
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "list-ref" | "list-tail" | "list?" | "assoc" | "map"
        | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase")
}

fn display_value(v: &Value) -> String {
    match v {
        Value::Str(s) => s.borrow().clone(),
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
        other => other.to_string(),
    }
}

fn value_to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        Value::Float(f) => Ok(*f),
        _ => Err(EvalError::Type(format!("expected number, got {v}"))),
    }
}

fn f64_to_exact(f: f64) -> Value {
    if f == f.floor() && f.abs() < i64::MAX as f64 {
        return Value::Integer(f as i64);
    }
    // Decompose IEEE 754 double to exact rational
    let bits = f.to_bits();
    let sign: i64 = if bits >> 63 == 1 { -1 } else { 1 };
    let raw_exp = ((bits >> 52) & 0x7FF) as i64;
    let mantissa = if raw_exp == 0 {
        (bits & 0x000F_FFFF_FFFF_FFFF) as i64
    } else {
        (bits & 0x000F_FFFF_FFFF_FFFF | 0x0010_0000_0000_0000) as i64
    };
    let exp = raw_exp - 1023 - 52;
    if exp >= 0 {
        Value::Integer(sign * mantissa * (1i64 << exp as u32))
    } else {
        let den = 1i64 << ((-exp) as u32);
        make_rational(sign * mantissa, den)
    }
}

fn is_number(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    if is_number(a) && is_number(b) {
        return nums_equal(a, b);
    }
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => *a.borrow() == *b.borrow(),
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        _ => false,
    }
}

fn nums_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Float(a), Value::Float(b)) => a == b,
        _ => {
            // Cross-tower: convert to f64
            if let (Ok(a), Ok(b)) = (value_to_f64(a), value_to_f64(b)) {
                a == b
            } else {
                false
            }
        }
    }
}

fn nums_less(a: &Value, b: &Value) -> Result<bool, EvalError> {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => Ok(a < b),
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => Ok(n1 * d2 < n2 * d1),
        (Value::Integer(a), Value::Rational(n, d)) => Ok(*a * d < *n),
        (Value::Rational(n, d), Value::Integer(b)) => Ok(*n < *b * d),
        _ => {
            let a = value_to_f64(a)?;
            let b = value_to_f64(b)?;
            Ok(a < b)
        }
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

// ── Macros (syntax-rules) ──

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

fn eval_define_syntax(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("define-syntax requires 2 arguments".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
    };
    let macro_val = parse_syntax_rules(&args[1], env)?;
    env_define(env, name, macro_val);
    Ok(Value::Boolean(false))
}

fn parse_syntax_rules(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let items = match &expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type("syntax-rules: expected list".into())),
    };
    if items.is_empty() {
        return Err(EvalError::Parse("syntax-rules: empty".into()));
    }
    if !matches!(&items[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Type("expected syntax-rules".into()));
    }
    if items.len() < 2 {
        return Err(EvalError::Arity("syntax-rules: need literals and rules".into()));
    }
    let literals = match &items[1].kind {
        ExprKind::List(lits) => {
            lits.iter()
                .map(|l| match &l.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type("syntax-rules: literal must be symbol".into())),
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => return Err(EvalError::Type("syntax-rules: expected literals list".into())),
    };
    let mut rules = Vec::new();
    for clause in &items[2..] {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                let pattern = match &parts[0].kind {
                    ExprKind::List(p) => p.clone(),
                    _ => return Err(EvalError::Type("syntax-rules: pattern must be list".into())),
                };
                rules.push((pattern, parts[1].clone()));
            }
            _ => return Err(EvalError::Type("syntax-rules: invalid rule".into())),
        }
    }
    Ok(Value::Macro {
        literals,
        rules,
        def_env: env.clone(),
    })
}

fn match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    bindings: &mut HashMap<String, MacroBinding>,
    literals: &[String],
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        // Check if next pattern element is ellipsis
        if pi + 1 < pattern.len() {
            if let ExprKind::Symbol(s) = &pattern[pi + 1].kind {
                if s == "..." {
                    // Current pattern matches zero or more remaining elements
                    if let ExprKind::Symbol(var) = &pattern[pi].kind {
                        let remaining: Vec<Expr> = input[ii..].to_vec();
                        bindings.insert(var.clone(), MacroBinding::Repeated(remaining));
                        return true; // ellipsis consumes the rest
                    }
                    return false;
                }
            }
        }
        if ii >= input.len() {
            return false;
        }
        match &pattern[pi].kind {
            ExprKind::Symbol(s) if literals.contains(s) => {
                if !matches!(&input[ii].kind, ExprKind::Symbol(is) if is == s) {
                    return false;
                }
            }
            ExprKind::Symbol(s) if s == "_" => {}
            ExprKind::Symbol(s) => {
                bindings.insert(s.clone(), MacroBinding::Single(input[ii].clone()));
            }
            ExprKind::List(sub_pat) => {
                if let ExprKind::List(sub_input) = &input[ii].kind {
                    if !match_pattern(sub_pat, sub_input, bindings, literals) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            ExprKind::Integer(n) => {
                if !matches!(&input[ii].kind, ExprKind::Integer(m) if m == n) {
                    return false;
                }
            }
            ExprKind::Boolean(b) => {
                if !matches!(&input[ii].kind, ExprKind::Boolean(b2) if b2 == b) {
                    return false;
                }
            }
            ExprKind::Rational(n, d) => {
                if !matches!(&input[ii].kind, ExprKind::Rational(n2, d2) if n2 == n && d2 == d) {
                    return false;
                }
            }
            ExprKind::Float(_) | ExprKind::Str(_) | ExprKind::Char(_) => return false,
        }
        pi += 1;
        ii += 1;
    }
    ii == input.len()
}

fn find_ellipsis_var(template: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Repeated(_))) {
                Some(s.clone())
            } else {
                None
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if let Some(v) = find_ellipsis_var(item, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn is_special_form(s: &str) -> bool {
    matches!(
        s,
        "define" | "set!" | "if" | "quote" | "lambda" | "and" | "or" | "let" | "begin" | "cond"
            | "define-syntax" | "syntax-rules"
    )
}

fn expand_ellipsis(
    pattern: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) -> Vec<Expr> {
    let var = match find_ellipsis_var(pattern, bindings) {
        Some(v) => v,
        None => return Vec::new(),
    };
    let elems = match bindings.get(&var) {
        Some(MacroBinding::Repeated(elems)) => elems,
        _ => return Vec::new(),
    };
    elems
        .iter()
        .map(|elem| {
            let mut single_bindings = bindings.clone();
            single_bindings.insert(var.clone(), MacroBinding::Single(elem.clone()));
            expand_template(pattern, &single_bindings, pattern_vars, renames)
        })
        .collect()
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    pattern_vars: &HashSet<String>,
    renames: &mut HashMap<String, String>,
) -> Expr {
    let span = template.span;
    match &template.kind {
        ExprKind::Symbol(s) if s == "..." => template.clone(),
        ExprKind::Symbol(s) => {
            if pattern_vars.contains(s) {
                if let Some(MacroBinding::Single(expr)) = bindings.get(s) {
                    return expr.clone();
                }
            }
            if is_special_form(s) || is_builtin(s) {
                return template.clone();
            }
            let gensym_name = renames
                .entry(s.clone())
                .or_insert_with(|| gensym(s))
                .clone();
            Expr {
                kind: ExprKind::Symbol(gensym_name),
                span,
            }
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() {
                    if let ExprKind::Symbol(s) = &items[i + 1].kind {
                        if s == "..." {
                            expanded.extend(expand_ellipsis(
                                &items[i], bindings, pattern_vars, renames,
                            ));
                            i += 2;
                            continue;
                        }
                    }
                }
                expanded.push(expand_template(&items[i], bindings, pattern_vars, renames));
                i += 1;
            }
            Expr {
                kind: ExprKind::List(expanded),
                span,
            }
        }
        _ => template.clone(),
    }
}

fn expand_and_eval_macro(
    macro_val: &Value,
    items: &[Expr],
    env: &mut Env,
    output: &mut String,
) -> Result<Value, EvalError> {
    let (literals, rules, def_env) = match macro_val {
        Value::Macro {
            literals,
            rules,
            def_env,
        } => (literals, rules, def_env),
        _ => unreachable!(),
    };
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if match_pattern(&pattern[1..], &items[1..], &mut bindings, literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let mut renames = HashMap::new();
            let expanded = expand_template(template, &bindings, &pattern_vars, &mut renames);

            // Inject definition-site bindings for hygiene
            let frame = new_frame();
            for (original, gensym_name) in &renames {
                if let Ok(val) = env_lookup(def_env, original) {
                    frame.borrow_mut().insert(gensym_name.clone(), val);
                }
            }
            env.push(frame);
            let result = eval(&expanded, env, output);
            env.pop();
            return result;
        }
    }
    Err(EvalError::Generic("no matching syntax-rules pattern".into()))
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
