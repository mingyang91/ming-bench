use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::scheme::EvalError;
use crate::scheme::parser::{Expr, ExprKind, Span};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

thread_local! {
    pub static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

pub fn with_output_capture<F, T>(f: F) -> (T, String)
where
    F: FnOnce() -> T,
{
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let result = f();
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    (result, output)
}

fn emit_output(s: &str) {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(s));
}

fn fmt_span(span: Span) -> String {
    format!("{}:{}", span.line, span.col)
}

fn with_span(err: EvalError, span: Span) -> EvalError {
    let pos = fmt_span(span);
    match err {
        EvalError::Parse(msg) if !has_pos(&msg) => EvalError::Parse(format!("{} at {}", msg, pos)),
        EvalError::UnboundVariable(msg) if !has_pos(&msg) => EvalError::UnboundVariable(format!("{} at {}", msg, pos)),
        EvalError::Type(msg) if !has_pos(&msg) => EvalError::Type(format!("{} at {}", msg, pos)),
        EvalError::Arity(msg) if !has_pos(&msg) => EvalError::Arity(format!("{} at {}", msg, pos)),
        EvalError::Runtime(msg) if !has_pos(&msg) => EvalError::Runtime(format!("{} at {}", msg, pos)),
        other => other,
    }
}

fn has_pos(msg: &str) -> bool {
    msg.as_bytes().windows(2).any(|w| w[0].is_ascii_digit() && w[1] == b':')
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
    Nil,
    Void,
    Builtin(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Rc<RefCell<EnvInner>>,
    },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Rc<RefCell<EnvInner>>,
    },
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 { let t = b; b = a % b; a = t; }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d.abs();
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

#[derive(Debug, Clone, Copy)]
enum Num {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

fn to_num(v: &Value) -> Result<Num, EvalError> {
    match v {
        Value::Integer(n) => Ok(Num::Int(*n)),
        Value::Float(f) => Ok(Num::Flt(*f)),
        Value::Rational(n, d) => Ok(Num::Rat(*n, *d)),
        _ => Err(EvalError::Type(format!("expected number, got {}", v.to_display()))),
    }
}

fn num_to_f64(n: Num) -> f64 {
    match n {
        Num::Int(i) => i as f64,
        Num::Rat(n, d) => n as f64 / d as f64,
        Num::Flt(f) => f,
    }
}

fn exact_parts(n: Num) -> Option<(i64, i64)> {
    match n {
        Num::Int(x) => Some((x, 1)),
        Num::Rat(n, d) => Some((n, d)),
        Num::Flt(_) => None,
    }
}

fn num_add(a: Num, b: Num) -> Value {
    if let (Some((n1, d1)), Some((n2, d2))) = (exact_parts(a), exact_parts(b)) {
        make_rational(n1 * d2 + n2 * d1, d1 * d2)
    } else {
        Value::Float(num_to_f64(a) + num_to_f64(b))
    }
}

fn num_sub(a: Num, b: Num) -> Value {
    if let (Some((n1, d1)), Some((n2, d2))) = (exact_parts(a), exact_parts(b)) {
        make_rational(n1 * d2 - n2 * d1, d1 * d2)
    } else {
        Value::Float(num_to_f64(a) - num_to_f64(b))
    }
}

fn num_mul(a: Num, b: Num) -> Value {
    if let (Some((n1, d1)), Some((n2, d2))) = (exact_parts(a), exact_parts(b)) {
        make_rational(n1 * n2, d1 * d2)
    } else {
        Value::Float(num_to_f64(a) * num_to_f64(b))
    }
}

fn num_div(a: Num, b: Num) -> Result<Value, EvalError> {
    if let (Some((n1, d1)), Some((n2, d2))) = (exact_parts(a), exact_parts(b)) {
        if n2 == 0 { return Err(EvalError::Runtime("division by zero".into())); }
        Ok(make_rational(n1 * d2, d1 * n2))
    } else {
        let denom = num_to_f64(b);
        if denom == 0.0 { return Err(EvalError::Runtime("division by zero".into())); }
        Ok(Value::Float(num_to_f64(a) / denom))
    }
}

fn num_neg(a: Num) -> Value {
    match a {
        Num::Int(x) => Value::Integer(-x),
        Num::Rat(n, d) => Value::Rational(-n, d),
        Num::Flt(f) => Value::Float(-f),
    }
}

fn num_cmp(a: Num, b: Num) -> f64 {
    // Returns a-b as f64 for comparison
    num_to_f64(a) - num_to_f64(b)
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Void, Value::Void) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Record { type_id: t1, fields: f1, .. }, Value::Record { type_id: t2, fields: f2, .. }) => {
                t1 == t2 && f1.iter().zip(f2.iter()).all(|((_, v1), (_, v2))| v1 == v2)
            }
            _ => false,
        }
    }
}

impl Value {
    /// write-style display (strings get quotes) — used for eval_str return values
    pub fn to_display(&self) -> String {
        self.fmt_value(true)
    }

    /// display-style output (strings without quotes) — used for `display` builtin
    pub fn to_display_output(&self) -> String {
        self.fmt_value(false)
    }

    fn fmt_value(&self, write_mode: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{}", f);
                if s.contains('.') || s.contains('e') || s.contains('E') { s } else { format!("{}.0", s) }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => {
                if write_mode {
                    format!("\"{}\"", s)
                } else {
                    s.clone()
                }
            }
            Value::Char(c) => {
                if write_mode {
                    match c {
                        ' ' => "#\\space".into(),
                        '\n' => "#\\newline".into(),
                        '\t' => "#\\tab".into(),
                        _ => format!("#\\{}", c),
                    }
                } else {
                    c.to_string()
                }
            }
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".into(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first { out.push(' '); }
                            first = false;
                            out.push_str(&car.fmt_value(write_mode));
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.fmt_value(write_mode));
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Void => "".into(),
            Value::Builtin(name) => format!("#<procedure:{}>", name),
            Value::Lambda { .. } => "#<procedure>".into(),
            Value::Macro { .. } => "#<macro>".into(),
            Value::Record { type_name, .. } => format!("#<record:{}>", type_name),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

#[derive(Debug)]
pub struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<RefCell<EnvInner>>>,
}

#[derive(Debug, Clone)]
pub struct Env(pub Rc<RefCell<EnvInner>>);

impl Env {
    pub fn default_env() -> Self {
        let mut bindings = HashMap::new();
        for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                     "cons", "car", "cdr", "null?", "list", "length", "append",
                     "pair?", "number?", "string?", "boolean?", "symbol?", "char?",
                     "integer?", "rational?", "exact?", "inexact?",
                     "exact->inexact", "inexact->exact",
                     "numerator", "denominator",
                     "display", "write", "newline",
                     "string-append", "string-length", "substring",
                     "string->number", "number->string",
                     "symbol->string", "string->symbol", "string-ref",
                     "string-copy", "make-string", "char->integer", "integer->char",
                     "apply", "eq?", "equal?", "map",
                     "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                     "zero?", "positive?", "negative?", "odd?", "even?",
                     "list-ref", "list-tail", "list?", "assoc",
                     "char-alphabetic?", "char-numeric?",
                     "char-upcase", "char-downcase", "char=?", "char<?",
                     "string=?", "string<?", "string-ci=?",
                     "string-upcase", "string-downcase"] {
            bindings.insert(name.to_string(), Value::Builtin(name.to_string()));
        }
        Env(Rc::new(RefCell::new(EnvInner {
            bindings,
            parent: None,
        })))
    }

    fn get(&self, name: &str) -> Option<Value> {
        let inner = self.0.borrow();
        if let Some(v) = inner.bindings.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &inner.parent {
            Env(parent.clone()).get(name)
        } else {
            None
        }
    }

    fn define(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    fn set(&self, name: &str, val: Value) -> Result<(), EvalError> {
        let has_key = self.0.borrow().bindings.contains_key(name);
        if has_key {
            self.0.borrow_mut().bindings.insert(name.to_string(), val);
            Ok(())
        } else {
            let parent = self.0.borrow().parent.clone();
            if let Some(p) = parent {
                Env(p).set(name, val)
            } else {
                Err(EvalError::UnboundVariable(format!("{}", name)))
            }
        }
    }

    fn child(parent: &Env) -> Env {
        Env(Rc::new(RefCell::new(EnvInner {
            bindings: HashMap::new(),
            parent: Some(parent.0.clone()),
        })))
    }
}

pub fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(n, d) => Ok(make_rational(*n, *d)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(name) => {
            env.get(name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{} at {}", name, fmt_span(span))))
        }
        ExprKind::List(items) if items.is_empty() => {
            Err(EvalError::Runtime(format!("empty application at {}", fmt_span(span))))
        }
        ExprKind::List(items) => {
            let head = &items[0];
            if let ExprKind::Symbol(name) = &head.kind {
                match name.as_str() {
                    "define" => return eval_define(&items[1..], env, span),
                    "if" => return eval_if(&items[1..], env, span),
                    "quote" => return eval_quote(&items[1..], span),
                    "lambda" => return eval_lambda(&items[1..], env, span),
                    "and" => return eval_and(&items[1..], env),
                    "or" => return eval_or(&items[1..], env),
                    "let" => return eval_let(&items[1..], env, span),
                    "begin" => return eval_begin(&items[1..], env),
                    "cond" => return eval_cond(&items[1..], env),
                    "set!" => return eval_set(&items[1..], env, span),
                    "string-set!" => return eval_string_set(&items[1..], env, span),
                    "define-syntax" => return eval_define_syntax(&items[1..], env, span),
                    "define-record-type" => return eval_define_record_type(&items[1..], env, span),
                    _ => {}
                }
                // Check for macro application
                if let Some(Value::Macro { literals, rules, def_env }) = env.get(name) {
                    let expanded = expand_macro(items, &literals, &rules, &Env(def_env), env, span)?;
                    return eval(&expanded, env);
                }
            }

            let func = eval(head, env)?;
            let args: Vec<Value> = items[1..].iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;

            apply_func(&func, &args).map_err(|e| with_span(e, span))
        }
    }
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("define requires at least 2 arguments at {}", fmt_span(span))));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(parts) if !parts.is_empty() => {
            if let ExprKind::Symbol(name) = &parts[0].kind {
                let (params, rest_param) = parse_params(&parts[1..], span)?;
                let body = args[1..].to_vec();
                let lambda = Value::Lambda {
                    params,
                    rest_param,
                    body,
                    env: env.0.clone(),
                };
                env.define(name.clone(), lambda);
                Ok(Value::Void)
            } else {
                Err(EvalError::Runtime(format!("define: expected function name at {}", fmt_span(span))))
            }
        }
        _ => Err(EvalError::Runtime(format!("define: bad syntax at {}", fmt_span(span)))),
    }
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Runtime(format!("if requires 2 or 3 arguments at {}", fmt_span(span))));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Runtime(format!("quote requires exactly 1 argument at {}", fmt_span(span))));
    }
    Ok(expr_to_value(&args[0]))
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => make_rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = Value::Pair(Box::new(expr_to_value(item)), Box::new(result));
            }
            result
        }
    }
}

fn parse_params(parts: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < parts.len() {
        if let ExprKind::Symbol(s) = &parts[i].kind {
            if s == "." {
                if i + 1 < parts.len() && i + 2 == parts.len() {
                    if let ExprKind::Symbol(rest) = &parts[i + 1].kind {
                        rest_param = Some(rest.clone());
                        break;
                    }
                }
                return Err(EvalError::Runtime(format!("bad dot syntax in params at {}", fmt_span(span))));
            }
            params.push(s.clone());
        } else {
            return Err(EvalError::Runtime(format!("parameter must be a symbol at {}", fmt_span(parts[i].span))));
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("lambda requires params and body at {}", fmt_span(span))));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(parts) => parse_params(parts, span)?,
        ExprKind::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Runtime(format!("lambda: expected parameter list at {}", fmt_span(span)))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.0.clone(),
    })
}

fn apply_func(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(_) => apply_builtin(func, args),
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(_) = rest_param {
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
            let parent_env = Env(env.clone());
            let local_env = Env::child(&parent_env);
            for (name, val) in params.iter().zip(args.iter()) {
                local_env.define(name.clone(), val.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = &args[params.len()..];
                let mut list = Value::Nil;
                for v in rest_args.iter().rev() {
                    list = Value::Pair(Box::new(v.clone()), Box::new(list));
                }
                local_env.define(rest.clone(), list);
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type(format!("{} is not a procedure", func.to_display()))),
    }
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Runtime(format!("let requires bindings and body at {}", fmt_span(span))));
    }
    // Named let: (let name ((var val) ...) body...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Runtime(format!("let: expected bindings list at {}", fmt_span(span)))),
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(p) = &pair[0].kind {
                        params.push(p.clone());
                        init_vals.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Runtime(format!("let: binding name must be symbol at {}", fmt_span(b.span))));
                    }
                }
                _ => return Err(EvalError::Runtime(format!("let: bad binding at {}", fmt_span(b.span)))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = Env::child(env);
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: local_env.0.clone(),
        };
        local_env.define(name.clone(), lambda.clone());
        for (p, v) in params.iter().zip(init_vals.iter()) {
            local_env.define(p.clone(), v.clone());
        }
        match &lambda {
            Value::Lambda { body, .. } => {
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env)?;
                }
                Ok(result)
            }
            _ => unreachable!(),
        }
    } else {
        // Regular let: (let ((var val) ...) body...)
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Runtime(format!("let: expected bindings list at {}", fmt_span(span)))),
        };
        let local_env = Env::child(env);
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(name) = &pair[0].kind {
                        let val = eval(&pair[1], env)?;
                        local_env.define(name.clone(), val);
                    } else {
                        return Err(EvalError::Runtime(format!("let: binding name must be symbol at {}", fmt_span(b.span))));
                    }
                }
                _ => return Err(EvalError::Runtime(format!("let: bad binding at {}", fmt_span(b.span)))),
            }
        }
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, &local_env)?;
        }
        Ok(result)
    }
}

fn eval_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Runtime(format!("set! requires exactly 2 arguments at {}", fmt_span(span))));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Runtime(format!("set!: first argument must be a symbol at {}", fmt_span(span)))),
    };
    if env.get(&name).is_none() {
        return Err(EvalError::UnboundVariable(format!("{} at {}", name, fmt_span(span))));
    }
    let val = eval(&args[1], env)?;
    env.set(&name, val)?;
    Ok(Value::Void)
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set! requires 3 arguments at {}", fmt_span(span))));
    }
    // Evaluate all arguments
    let target = eval(&args[0], env)?;
    let idx_val = eval(&args[1], env)?;
    let ch_val = eval(&args[2], env)?;

    let mut s = match target {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let idx = expect_int(&idx_val)? as usize;
    let ch = match ch_val {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };

    if idx >= s.len() {
        return Err(EvalError::Runtime("string-set!: index out of range".into()));
    }
    // SAFETY: we checked idx < len, and we're replacing a single byte with a single ASCII-range char
    unsafe { s.as_bytes_mut()[idx] = ch as u8; }

    // If the first arg was a variable, update it in the environment
    if let ExprKind::Symbol(name) = &args[0].kind {
        env.set(name, Value::Str(s)).ok();
    }
    Ok(Value::Void)
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
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
                if test.is_truthy() {
                    let mut result = Value::Void;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Runtime(format!("cond: bad clause at {}", fmt_span(clause.span)))),
        }
    }
    Ok(Value::Void)
}

fn apply_builtin(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    let name = match func {
        Value::Builtin(name) => name.as_str(),
        _ => return Err(EvalError::Type(format!("{} is not a procedure", func.to_display()))),
    };

    match name {
        "+" => {
            let mut acc = to_num(&Value::Integer(0))?;
            for a in args {
                let n = to_num(a)?;
                acc = to_num(&num_add(acc, n))?;
            }
            Ok(match acc { Num::Int(i) => Value::Integer(i), Num::Rat(n, d) => Value::Rational(n, d), Num::Flt(f) => Value::Float(f) })
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return Ok(num_neg(to_num(&args[0])?));
            }
            let mut acc = to_num(&args[0])?;
            for a in &args[1..] {
                let n = to_num(a)?;
                acc = to_num(&num_sub(acc, n))?;
            }
            Ok(match acc { Num::Int(i) => Value::Integer(i), Num::Rat(n, d) => Value::Rational(n, d), Num::Flt(f) => Value::Float(f) })
        }
        "*" => {
            let mut acc = to_num(&Value::Integer(1))?;
            for a in args {
                let n = to_num(a)?;
                acc = to_num(&num_mul(acc, n))?;
            }
            Ok(match acc { Num::Int(i) => Value::Integer(i), Num::Rat(n, d) => Value::Rational(n, d), Num::Flt(f) => Value::Float(f) })
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
            }
            let mut acc = to_num(&args[0])?;
            for a in &args[1..] {
                let n = to_num(a)?;
                acc = to_num(&num_div(acc, n)?)?;
            }
            Ok(match acc { Num::Int(i) => Value::Integer(i), Num::Rat(n, d) => Value::Rational(n, d), Num::Flt(f) => Value::Float(f) })
        }
        "<" => num_cmp_op(args, |a, b| a < b),
        ">" => num_cmp_op(args, |a, b| a > b),
        "=" => num_cmp_op(args, |a, b| a == b),
        "<=" => num_cmp_op(args, |a, b| a <= b),
        ">=" => num_cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
            }
            Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires exactly 1 argument".into()));
            }
            match &args[0] {
                Value::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
            }
            match &args[0] {
                Value::Pair(_, cdr) => Ok(*cdr.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(args[0], Value::Nil)))
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = Value::Pair(Box::new(a.clone()), Box::new(result));
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires exactly 1 argument".into()));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(_, cdr) => { count += 1; cur = cdr; }
                    _ => return Err(EvalError::Type("length: not a proper list".into())),
                }
            }
            Ok(Value::Integer(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            let mut result = args.last().unwrap().clone();
            for a in args[..args.len()-1].iter().rev() {
                let mut elems = Vec::new();
                let mut cur = a;
                loop {
                    match cur {
                        Value::Nil => break,
                        Value::Pair(car, cdr) => { elems.push(*car.clone()); cur = cdr; }
                        _ => return Err(EvalError::Type("append: not a proper list".into())),
                    }
                }
                for e in elems.into_iter().rev() {
                    result = Value::Pair(Box::new(e), Box::new(result));
                }
            }
            Ok(result)
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer? requires 1 argument".into())); }
            let result = match &args[0] {
                Value::Integer(_) => true,
                Value::Float(f) => *f == f.floor() && f.is_finite(),
                Value::Rational(_, _) => false, // already simplified, so denom != 1 means not integer
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity("rational? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires 1 argument".into())); }
            let n = to_num(&args[0])?;
            Ok(Value::Float(num_to_f64(n)))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(_) | Value::Rational(_, _) => Ok(args[0].clone()),
                Value::Float(f) => {
                    // Convert float to exact rational via continued fraction approximation
                    Ok(float_to_exact(*f))
                }
                _ => Err(EvalError::Type("inexact->exact: expected number".into())),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity("numerator requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type("numerator: expected rational".into())),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity("denominator requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type("denominator: expected rational".into())),
            }
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char? requires 1 argument".into())); }
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity("display requires 1 argument".into())); }
            emit_output(&args[0].to_display_output());
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity("write requires 1 argument".into())); }
            emit_output(&args[0].to_display());
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity("newline requires 0 arguments".into())); }
            emit_output("\n");
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring requires 3 arguments".into())); }
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("substring: expected string".into())) };
            let start = expect_int(&args[1])? as usize;
            let end = expect_int(&args[2])? as usize;
            if end > s.len() || start > end {
                return Err(EvalError::Runtime("substring: index out of range".into()));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string requires 1 argument".into())); }
            Ok(Value::Str(args[0].to_display()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string requires 1 argument".into())); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref requires 2 arguments".into())); }
            let s = match &args[0] { Value::Str(s) => s, _ => return Err(EvalError::Type("string-ref: expected string".into())) };
            let idx = expect_int(&args[1])? as usize;
            if idx >= s.len() {
                return Err(EvalError::Runtime("string-ref: index out of range".into()));
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy requires 1 argument".into())); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into())); }
            let len = expect_int(&args[0])? as usize;
            let ch = if args.len() == 2 {
                match &args[1] { Value::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into())) }
            } else { '\0' };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity("char->integer requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Integer(*c as i64)), _ => Err(EvalError::Type("char->integer: expected char".into())) }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer->char requires 1 argument".into())); }
            let n = expect_int(&args[0])?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\u{FFFD}')))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            // Collect the last argument (must be a list) into a Vec
            let mut tail_args = Vec::new();
            let mut cur = last;
            loop {
                match cur {
                    Value::Pair(car, cdr) => {
                        tail_args.push((**car).clone());
                        cur = cdr;
                    }
                    Value::Nil => break,
                    _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
                }
            }
            // Combine prefix args (between func and last) with tail args
            let mut all_args: Vec<Value> = args[1..args.len()-1].to_vec();
            all_args.extend(tail_args);
            apply_func(func, &all_args)
        }
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eq? requires 2 arguments".into())); }
            let result = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Nil, Value::Nil) => true,
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal? requires 2 arguments".into())); }
            Ok(Value::Boolean(args[0] == args[1]))
        }
        "map" => {
            if args.len() < 2 { return Err(EvalError::Arity("map requires at least 2 arguments".into())); }
            let func = &args[0];
            let mut lists: Vec<Vec<Value>> = Vec::new();
            for a in &args[1..] {
                let mut elems = Vec::new();
                let mut cur = a;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => { elems.push((**car).clone()); cur = cdr; }
                        Value::Nil => break,
                        _ => return Err(EvalError::Type("map: expected list".into())),
                    }
                }
                lists.push(elems);
            }
            let len = lists[0].len();
            let mut result = Value::Nil;
            let mut results = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                results.push(apply_func(func, &call_args)?);
            }
            for v in results.into_iter().rev() {
                result = Value::Pair(Box::new(v), Box::new(result));
            }
            Ok(result)
        }
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(n.abs())),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                Value::Rational(n, d) => Ok(make_rational(n.abs(), *d)),
                _ => Err(EvalError::Type("abs: expected number".into())),
            }
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity("modulo requires 2 arguments".into())); }
            let a = expect_int(&args[0])?;
            let b = expect_int(&args[1])?;
            if b == 0 { return Err(EvalError::Runtime("modulo: division by zero".into())); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity("remainder requires 2 arguments".into())); }
            let a = expect_int(&args[0])?;
            let b = expect_int(&args[1])?;
            if b == 0 { return Err(EvalError::Runtime("remainder: division by zero".into())); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient requires 2 arguments".into())); }
            let a = expect_int(&args[0])?;
            let b = expect_int(&args[1])?;
            if b == 0 { return Err(EvalError::Runtime("quotient: division by zero".into())); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
            let mut m = expect_int(&args[0])?;
            for a in &args[1..] { let v = expect_int(a)?; if v < m { m = v; } }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
            let mut m = expect_int(&args[0])?;
            for a in &args[1..] { let v = expect_int(a)?; if v > m { m = v; } }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt requires 2 arguments".into())); }
            let base = expect_int(&args[0])?;
            let exp = expect_int(&args[1])?;
            if exp < 0 { return Ok(Value::Integer(0)); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero? requires 1 argument".into())); }
            Ok(Value::Boolean(num_to_f64(to_num(&args[0])?) == 0.0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive? requires 1 argument".into())); }
            Ok(Value::Boolean(num_to_f64(to_num(&args[0])?) > 0.0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative? requires 1 argument".into())); }
            Ok(Value::Boolean(num_to_f64(to_num(&args[0])?) < 0.0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&args[0])? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even? requires 1 argument".into())); }
            Ok(Value::Boolean(expect_int(&args[0])? % 2 == 0))
        }
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref requires 2 arguments".into())); }
            let idx = expect_int(&args[1])? as usize;
            let mut cur = &args[0];
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => return Err(EvalError::Runtime("list-ref: index out of range".into())),
                }
            }
            match cur {
                Value::Pair(car, _) => Ok((**car).clone()),
                _ => Err(EvalError::Runtime("list-ref: index out of range".into())),
            }
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail requires 2 arguments".into())); }
            let idx = expect_int(&args[1])? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match cur {
                    Value::Pair(_, cdr) => cur = *cdr,
                    _ => return Err(EvalError::Runtime("list-tail: index out of range".into())),
                }
            }
            Ok(cur)
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list? requires 1 argument".into())); }
            let mut cur = &args[0];
            let result = loop {
                match cur {
                    Value::Nil => break true,
                    Value::Pair(_, cdr) => cur = cdr,
                    _ => break false,
                }
            };
            Ok(Value::Boolean(result))
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc requires 2 arguments".into())); }
            let key = &args[0];
            let mut cur = &args[1];
            loop {
                match cur {
                    Value::Pair(car, cdr) => {
                        if let Value::Pair(k, _) = &**car {
                            if **k == *key {
                                return Ok((**car).clone());
                            }
                        }
                        cur = cdr;
                    }
                    Value::Nil => return Ok(Value::Boolean(false)),
                    _ => return Err(EvalError::Type("assoc: expected list".into())),
                }
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())), _ => Err(EvalError::Type("char-alphabetic?: expected char".into())) }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())), _ => Err(EvalError::Type("char-numeric?: expected char".into())) }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())), _ => Err(EvalError::Type("char-upcase: expected char".into())) }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires 1 argument".into())); }
            match &args[0] { Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())), _ => Err(EvalError::Type("char-downcase: expected char".into())) }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into())),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into())),
            }
        }
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into())),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into())),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Str(s.to_uppercase())), _ => Err(EvalError::Type("string-upcase: expected string".into())) }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires 1 argument".into())); }
            match &args[0] { Value::Str(s) => Ok(Value::Str(s.to_lowercase())), _ => Err(EvalError::Type("string-downcase: expected string".into())) }
        }
        _ => {
            // Dynamic record type builtins
            if let Some(rest) = name.strip_prefix("__record_ctor_") {
                // Format: __record_ctor_{type_id}_{ctor_name}
                let type_id: u64 = rest.split('_').next().unwrap().parse().unwrap();
                return RECORD_REGISTRY.with(|reg| {
                    let reg = reg.borrow();
                    let info = reg.get(&type_id).ok_or_else(|| EvalError::Runtime("unknown record type".into()))?;
                    if args.len() != info.ctor_fields.len() {
                        return Err(EvalError::Arity(format!(
                            "record constructor expects {} arguments, got {}", info.ctor_fields.len(), args.len()
                        )));
                    }
                    let fields = info.ctor_fields.iter().cloned().zip(args.iter().cloned()).collect();
                    Ok(Value::Record { type_id, type_name: info.type_name.clone(), fields })
                });
            }
            if let Some(rest) = name.strip_prefix("__record_pred_") {
                let type_id: u64 = rest.parse().unwrap();
                if args.len() != 1 {
                    return Err(EvalError::Arity("record predicate expects 1 argument".into()));
                }
                return Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id)));
            }
            if let Some(rest) = name.strip_prefix("__record_acc_") {
                // Format: __record_acc_{type_id}_{field_name}_{accessor_name}
                let parts: Vec<&str> = rest.splitn(3, '_').collect();
                let type_id: u64 = parts[0].parse().unwrap();
                let field_name = parts[1];
                if args.len() != 1 {
                    return Err(EvalError::Arity("record accessor expects 1 argument".into()));
                }
                match &args[0] {
                    Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                        for (fname, val) in fields {
                            if fname == field_name {
                                return Ok(val.clone());
                            }
                        }
                        Err(EvalError::Runtime(format!("record has no field {}", field_name)))
                    }
                    _ => Err(EvalError::Type("record accessor: wrong type".into())),
                }
            } else {
                Err(EvalError::Runtime(format!("unknown builtin: {}", name)))
            }
        }
    }
}

fn expect_int(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("expected integer, got {}", v.to_display()))),
    }
}

fn num_cmp_op(args: &[Value], op: impl Fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let mut prev = num_to_f64(to_num(&args[0])?);
    for a in &args[1..] {
        let curr = num_to_f64(to_num(a)?);
        if !op(prev, curr) {
            return Ok(Value::Boolean(false));
        }
        prev = curr;
    }
    Ok(Value::Boolean(true))
}

fn float_to_exact(f: f64) -> Value {
    // Simple approach: represent as p/q by finding best rational
    if f == f.floor() {
        return Value::Integer(f as i64);
    }
    // Use the fraction representation: multiply by power of 2 then simplify
    let (mut n, mut d): (i64, i64) = {
        // Scale to remove decimal: f = n/d
        let bits = f.to_bits();
        let sign: i64 = if (bits >> 63) == 0 { 1 } else { -1 };
        let exponent = ((bits >> 52) & 0x7FF) as i64 - 1023;
        let mantissa = if exponent == -1023 {
            (bits & 0x000F_FFFF_FFFF_FFFF) << 1
        } else {
            (bits & 0x000F_FFFF_FFFF_FFFF) | 0x0010_0000_0000_0000
        } as i64;
        // f = sign * mantissa * 2^(exponent - 52)
        let e = exponent - 52;
        if e >= 0 {
            (sign * mantissa * (1i64 << e.min(62)), 1)
        } else {
            let neg_e = (-e).min(62) as u32;
            (sign * mantissa, 1i64 << neg_e)
        }
    };
    let g = gcd(n.abs(), d);
    n /= g;
    d /= g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

// ---- Hygienic Macros (syntax-rules) ----

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__m{}", base, n)
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "let", "begin",
    "cond", "set!", "string-set!", "define-syntax", "syntax-rules",
    "define-record-type",
    "let*", "letrec", "do", "case", "when", "unless", "else",
];

fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Runtime(format!("define-syntax requires 2 arguments at {}", fmt_span(span))));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Runtime(format!("define-syntax: expected symbol at {}", fmt_span(span)))),
    };
    let (literals, rules) = parse_syntax_rules(&args[1], span)?;
    env.define(name, Value::Macro {
        literals,
        rules,
        def_env: env.0.clone(),
    });
    Ok(Value::Void)
}

fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field-name ...) predicate (field-name accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Runtime(format!("define-record-type requires at least 3 arguments at {}", fmt_span(span))));
    }
    let type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Runtime(format!("define-record-type: expected type name at {}", fmt_span(span)))),
    };
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Parse constructor: (constructor-name field-name ...)
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Runtime("define-record-type: expected constructor name".into())),
            };
            let fields: Vec<String> = parts[1..].iter().map(|p| match &p.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Runtime("define-record-type: expected field name".into())),
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Runtime("define-record-type: expected constructor".into())),
    };

    // Parse predicate name
    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Runtime("define-record-type: expected predicate name".into())),
    };

    // Parse field accessors: (field-name accessor-name) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let field = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Runtime("define-record-type: expected field name".into())),
                };
                let accessor = match &parts[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Runtime("define-record-type: expected accessor name".into())),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Runtime("define-record-type: bad field spec".into())),
        }
    }

    // Define constructor
    let ctor_fields_clone = ctor_fields.clone();
    let type_name_clone = type_name.clone();
    let ctor_tag = format!("__record_ctor_{}_{}", type_id, ctor_name);
    env.define(ctor_name.clone(), Value::Builtin(ctor_tag.clone()));

    // Define predicate
    let pred_tag = format!("__record_pred_{}", type_id);
    env.define(pred_name.clone(), Value::Builtin(pred_tag.clone()));

    // Define accessors
    for (field, accessor) in &field_accessors {
        let acc_tag = format!("__record_acc_{}_{}_{}", type_id, field, accessor);
        env.define(accessor.clone(), Value::Builtin(acc_tag));
    }

    // Store record type info in a thread-local registry
    RECORD_REGISTRY.with(|reg| {
        reg.borrow_mut().insert(type_id, RecordTypeInfo {
            type_name: type_name_clone,
            ctor_fields: ctor_fields_clone,
            field_accessors: field_accessors,
        });
    });

    Ok(Value::Void)
}

#[derive(Debug, Clone)]
struct RecordTypeInfo {
    type_name: String,
    ctor_fields: Vec<String>,
    field_accessors: Vec<(String, String)>,
}

thread_local! {
    static RECORD_REGISTRY: RefCell<HashMap<u64, RecordTypeInfo>> = RefCell::new(HashMap::new());
}

fn parse_syntax_rules(expr: &Expr, span: Span) -> Result<(Vec<String>, Vec<(Expr, Expr)>), EvalError> {
    match &expr.kind {
        ExprKind::List(items) if items.len() >= 2 => {
            if !matches!(&items[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
                return Err(EvalError::Runtime(format!("define-syntax: expected syntax-rules at {}", fmt_span(span))));
            }
            let literals = match &items[1].kind {
                ExprKind::List(lits) => {
                    let mut result = Vec::new();
                    for lit in lits {
                        if let ExprKind::Symbol(s) = &lit.kind {
                            result.push(s.clone());
                        } else {
                            return Err(EvalError::Runtime("syntax-rules: literals must be symbols".into()));
                        }
                    }
                    result
                }
                _ => return Err(EvalError::Runtime("syntax-rules: expected literals list".into())),
            };
            let mut rules = Vec::new();
            for item in &items[2..] {
                match &item.kind {
                    ExprKind::List(parts) if parts.len() == 2 => {
                        rules.push((parts[0].clone(), parts[1].clone()));
                    }
                    _ => return Err(EvalError::Runtime("syntax-rules: bad rule".into())),
                }
            }
            Ok((literals, rules))
        }
        _ => Err(EvalError::Runtime(format!("define-syntax: expected syntax-rules at {}", fmt_span(span)))),
    }
}

#[derive(Debug, Clone)]
enum PatBinding {
    Single(Expr),
    Ellipsis(Vec<Expr>),
}

fn match_pattern(
    input: &[Expr],
    pattern: &Expr,
    literals: &[String],
) -> Option<HashMap<String, PatBinding>> {
    let pat_items = match &pattern.kind {
        ExprKind::List(items) => items,
        _ => return None,
    };
    if pat_items.is_empty() { return None; }
    // Skip first element (macro name) in both
    let pat = &pat_items[1..];
    let inp = &input[1..];
    let mut bindings = HashMap::new();
    match_list(inp, pat, literals, &mut bindings)?;
    Some(bindings)
}

fn match_list(
    input: &[Expr],
    pattern: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> Option<()> {
    let mut i = 0;
    let mut p = 0;
    while p < pattern.len() {
        let has_ellipsis = p + 1 < pattern.len()
            && matches!(&pattern[p + 1].kind, ExprKind::Symbol(s) if s == "...");
        if has_ellipsis {
            let name = match &pattern[p].kind {
                ExprKind::Symbol(s) if !literals.contains(s) => s.clone(),
                _ => return None,
            };
            let remaining_pats = pattern.len() - p - 2;
            let available = input.len().checked_sub(i + remaining_pats).unwrap_or(0);
            let matched: Vec<Expr> = input[i..i + available].to_vec();
            bindings.insert(name, PatBinding::Ellipsis(matched));
            i += available;
            p += 2;
        } else {
            if i >= input.len() { return None; }
            match_single(&input[i], &pattern[p], literals, bindings)?;
            i += 1;
            p += 1;
        }
    }
    if i != input.len() { return None; }
    Some(())
}

fn match_single(
    input: &Expr,
    pattern: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> Option<()> {
    match &pattern.kind {
        ExprKind::Symbol(name) if name == "_" => Some(()),
        ExprKind::Symbol(name) if literals.contains(name) => {
            if let ExprKind::Symbol(s) = &input.kind {
                if s == name { Some(()) } else { None }
            } else {
                None
            }
        }
        ExprKind::Symbol(name) => {
            bindings.insert(name.clone(), PatBinding::Single(input.clone()));
            Some(())
        }
        ExprKind::List(pat_items) => {
            if let ExprKind::List(inp_items) = &input.kind {
                match_list(inp_items, pat_items, literals, bindings)
            } else {
                None
            }
        }
        ExprKind::Integer(n) => {
            if let ExprKind::Integer(m) = &input.kind { if n == m { Some(()) } else { None } } else { None }
        }
        ExprKind::Boolean(b) => {
            if let ExprKind::Boolean(c) = &input.kind { if b == c { Some(()) } else { None } } else { None }
        }
        _ => None,
    }
}

fn expand_macro(
    input: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    span: Span,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        if let Some(bindings) = match_pattern(input, pattern, literals) {
            let mut rename_map = HashMap::new();
            return expand_template(template, &bindings, def_env, use_env, &mut rename_map, span);
        }
    }
    Err(EvalError::Runtime(format!("no matching syntax-rules pattern at {}", fmt_span(span))))
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    def_env: &Env,
    use_env: &Env,
    rename_map: &mut HashMap<String, String>,
    span: Span,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatBinding::Single(expr) => Ok(expr.clone()),
                    PatBinding::Ellipsis(_) => Ok(template.clone()),
                }
            } else if SPECIAL_FORMS.contains(&name.as_str()) {
                Ok(template.clone())
            } else {
                let renamed = rename_map.entry(name.clone()).or_insert_with(|| {
                    let g = gensym(name);
                    if let Some(val) = def_env.get(name) {
                        use_env.define(g.clone(), val);
                    }
                    g
                });
                Ok(Expr {
                    kind: ExprKind::Symbol(renamed.clone()),
                    span: template.span,
                })
            }
        }
        ExprKind::List(items) => {
            // Don't descend into quoted forms
            if !items.is_empty() {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "quote" {
                        return Ok(template.clone());
                    }
                }
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                let has_ellipsis = i + 1 < items.len()
                    && matches!(&items[i + 1].kind, ExprKind::Symbol(s) if s == "...");
                if has_ellipsis {
                    let ellipsis_vars = find_ellipsis_vars(&items[i], bindings);
                    if let Some(var_name) = ellipsis_vars.first() {
                        if let Some(PatBinding::Ellipsis(exprs)) = bindings.get(var_name) {
                            let count = exprs.len();
                            for j in 0..count {
                                let mut iter_bindings = bindings.clone();
                                for ev in &ellipsis_vars {
                                    if let Some(PatBinding::Ellipsis(vals)) = bindings.get(ev) {
                                        if j < vals.len() {
                                            iter_bindings.insert(ev.clone(), PatBinding::Single(vals[j].clone()));
                                        }
                                    }
                                }
                                let expanded = expand_template(&items[i], &iter_bindings, def_env, use_env, rename_map, span)?;
                                result.push(expanded);
                            }
                        }
                    }
                    i += 2;
                } else {
                    let expanded = expand_template(&items[i], bindings, def_env, use_env, rename_map, span)?;
                    result.push(expanded);
                    i += 1;
                }
            }
            Ok(Expr {
                kind: ExprKind::List(result),
                span: template.span,
            })
        }
        _ => Ok(template.clone()),
    }
}

fn find_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    find_ellipsis_vars_inner(template, bindings, &mut vars);
    vars
}

fn find_ellipsis_vars_inner(template: &Expr, bindings: &HashMap<String, PatBinding>, vars: &mut Vec<String>) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatBinding::Ellipsis(_))) {
                if !vars.contains(name) {
                    vars.push(name.clone());
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                find_ellipsis_vars_inner(item, bindings, vars);
            }
        }
        _ => {}
    }
}
