pub mod error;
mod builtins;
mod macros;

pub use error::EvalError;
use builtins::*;
use macros::eval_macro_call;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("#{}#{}", prefix, n)
}

pub(crate) type BuiltinFn = fn(&[Val], &Env) -> Result<Val, EvalError>;

#[derive(Clone)]
pub(crate) enum Val {
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Val>),
    Pair(Box<Val>, Box<Val>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Val>>>),
    Builtin(BuiltinFn),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Void,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub(crate) fn make_rational(n: i64, d: i64) -> Val {
    if d == 0 {
        return Val::Int(0); // shouldn't happen
    }
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Val::Int(n) } else { Val::Rational(n, d) }
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Float(x) => {
                if x.fract() == 0.0 && x.is_finite() {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Val::Rational(n, d) => write!(f, "{}/{}", n, d),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Pair(a, b) => {
                write!(f, "({a}")?;
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Val::Pair(ca, cb) => {
                            write!(f, " {ca}")?;
                            cur = cb.as_ref();
                        }
                        Val::List(v) if v.is_empty() => break,
                        other => { write!(f, " . {other}")?; break; }
                    }
                }
                write!(f, ")")
            }
            Val::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(..) | Val::Macro { .. } => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Environment ---

type Frame = Rc<RefCell<HashMap<String, Val>>>;

#[derive(Clone)]
pub(crate) struct Env {
    frames: Vec<Frame>,
    pub(crate) output: Rc<RefCell<String>>,
}

impl Env {
    fn new() -> Self {
        let frame = Rc::new(RefCell::new(HashMap::new()));
        let builtins: &[(&str, BuiltinFn)] = &[
            ("+", builtin_add as BuiltinFn),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_lt),
            (">", builtin_gt),
            ("=", builtin_eq),
            ("<=", builtin_le),
            (">=", builtin_ge),
            ("not", builtin_not),
            ("cons", builtin_cons as BuiltinFn),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("list", builtin_list),
            ("null?", builtin_null),
            ("length", builtin_length),
            ("append", builtin_append),
            ("boolean?", builtin_is_boolean),
            ("number?", builtin_is_number),
            ("string?", builtin_is_string),
            ("symbol?", builtin_is_symbol),
            ("pair?", builtin_is_pair),
            ("char?", builtin_is_char),
            ("display", builtin_display),
            ("write", builtin_write),
            ("newline", builtin_newline),
            ("string-append", builtin_string_append),
            ("string-length", builtin_string_length),
            ("substring", builtin_substring),
            ("string->number", builtin_string_to_number),
            ("number->string", builtin_number_to_string),
            ("symbol->string", builtin_symbol_to_string),
            ("string->symbol", builtin_string_to_symbol),
            ("string-ref", builtin_string_ref),
            ("string-copy", builtin_string_copy),
            ("string->list", builtin_string_to_list),
            ("list->string", builtin_list_to_string),
            ("char->integer", builtin_char_to_integer),
            ("integer->char", builtin_integer_to_char),
            ("apply", builtin_apply),
            ("equal?", builtin_equal),
            ("eq?", builtin_eq_pred),
            ("abs", builtin_abs),
            ("modulo", builtin_modulo),
            ("remainder", builtin_remainder),
            ("quotient", builtin_quotient),
            ("min", builtin_min),
            ("max", builtin_max),
            ("expt", builtin_expt),
            ("zero?", builtin_zero),
            ("positive?", builtin_positive),
            ("negative?", builtin_negative),
            ("odd?", builtin_odd),
            ("even?", builtin_even),
            ("list-ref", builtin_list_ref),
            ("list-tail", builtin_list_tail),
            ("list?", builtin_is_list),
            ("assoc", builtin_assoc),
            ("map", builtin_map),
            ("char-alphabetic?", builtin_char_alphabetic),
            ("char-numeric?", builtin_char_numeric),
            ("char-upcase", builtin_char_upcase),
            ("char-downcase", builtin_char_downcase),
            ("char=?", builtin_char_eq),
            ("char<?", builtin_char_lt),
            ("string=?", builtin_string_eq),
            ("string<?", builtin_string_lt),
            ("string-ci=?", builtin_string_ci_eq),
            ("string-upcase", builtin_string_upcase),
            ("string-downcase", builtin_string_downcase),
            ("integer?", builtin_is_integer),
            ("rational?", builtin_is_rational),
            ("exact?", builtin_is_exact),
            ("inexact?", builtin_is_inexact),
            ("exact->inexact", builtin_exact_to_inexact),
            ("inexact->exact", builtin_inexact_to_exact),
            ("numerator", builtin_numerator),
            ("denominator", builtin_denominator),
            ("procedure?", builtin_is_procedure),
            ("eqv?", builtin_eqv),
            ("vector", builtin_vector),
            ("make-vector", builtin_make_vector),
            ("vector-ref", builtin_vector_ref),
            ("vector-set!", builtin_vector_set),
            ("vector-length", builtin_vector_length),
            ("vector?", builtin_is_vector),
            ("vector->list", builtin_vector_to_list),
            ("list->vector", builtin_list_to_vector),
        ];
        for &(name, f) in builtins {
            frame.borrow_mut().insert(name.to_string(), Val::Builtin(f));
        }
        Env { frames: vec![frame], output: Rc::new(RefCell::new(String::new())) }
    }

    pub(crate) fn get(&self, name: &str) -> Option<Val> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    pub(crate) fn define(&self, name: String, val: Val) {
        self.frames.last().expect("env has no frames").borrow_mut().insert(name, val);
    }

    fn set(&self, name: &str, val: Val) -> Result<(), EvalError> {
        for frame in self.frames.iter().rev() {
            let mut f = frame.borrow_mut();
            if f.contains_key(name) {
                f.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(EvalError::UnboundVariable(name.to_string()))
    }

    pub(crate) fn push(&self) -> Env {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(RefCell::new(HashMap::new())));
        Env { frames, output: Rc::clone(&self.output) }
    }
}

// --- Source Positions ---

#[derive(Debug, Clone, Copy)]
pub(crate) struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut col = 1usize;
    while let Some(&c) = chars.peek() {
        match c {
            '\n' => { chars.next(); line += 1; col = 1; }
            ' ' | '\t' | '\r' => { chars.next(); col += 1; }
            ';' => {
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    col += 1;
                    if c2 == '\n' { line += 1; col = 1; break; }
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '"' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            col += 1;
                            match chars.next() {
                                Some('n') => { s.push('\n'); col += 1; }
                                Some('t') => { s.push('\t'); col += 1; }
                                Some('"') => { s.push('"'); col += 1; }
                                Some('\\') => { s.push('\\'); col += 1; }
                                Some(other) => { s.push('\\'); s.push(other); col += 1; }
                                None => break,
                            }
                        }
                        Some('"') => { col += 1; break; }
                        Some('\n') => { s.push('\n'); line += 1; col = 1; }
                        Some(c2) => { s.push(c2); col += 1; }
                        None => break,
                    }
                }
                tokens.push(Token { text: format!("\"{}\"", s), span: start_span });
            }
            _ => {
                let start_span = Span::new(line, col);
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == ';' || c2 == '\'' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                    col += 1;
                }
                tokens.push(Token { text: tok, span: start_span });
            }
        }
    }
    tokens
}

fn parse(tokens: &[Token]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    let span = tok.span;
    if tok.text == "'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse(format!("missing closing paren at {span}")));
        }
        Ok((Expr::new(ExprKind::List(elems), span), i + 1))
    } else if tok.text == ")" {
        Err(EvalError::Parse(format!("unexpected ) at {span}")))
    } else if tok.text.starts_with('"') {
        let s = tok.text[1..tok.text.len()-1].to_string();
        Ok((Expr::new(ExprKind::Str(s), span), 1))
    } else if tok.text == "#t" {
        Ok((Expr::new(ExprKind::Bool(true), span), 1))
    } else if tok.text == "#f" {
        Ok((Expr::new(ExprKind::Bool(false), span), 1))
    } else if tok.text.starts_with("#\\") {
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string is non-empty"),
            _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {span}", tok.text))),
        };
        Ok((Expr::new(ExprKind::Char(ch), span), 1))
    } else if let Ok(n) = tok.text.parse::<i64>() {
        Ok((Expr::new(ExprKind::Int(n), span), 1))
    } else if let Some(pos) = tok.text.find('/') {
        // Try rational literal n/d
        let num_part = &tok.text[..pos];
        let den_part = &tok.text[pos+1..];
        if let (Ok(n), Ok(d)) = (num_part.parse::<i64>(), den_part.parse::<i64>()) {
            if d != 0 {
                // Simplify the rational
                let sign = if d < 0 { -1 } else { 1 };
                let nn = n * sign;
                let dd = d * sign;
                let g = gcd(nn.abs(), dd);
                let nn = nn / g;
                let dd = dd / g;
                if dd == 1 {
                    Ok((Expr::new(ExprKind::Int(nn), span), 1))
                } else {
                    Ok((Expr::new(ExprKind::Rational(nn, dd), span), 1))
                }
            } else {
                Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
            }
        } else {
            Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
        }
    } else if let Ok(x) = tok.text.parse::<f64>() {
        Ok((Expr::new(ExprKind::Float(x), span), 1))
    } else {
        Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}

// --- Evaluator ---

fn span_err(span: Span, err: EvalError) -> EvalError {
    // If the error message already contains position info, return as-is
    let msg = err.to_string();
    if msg.contains(':') && msg.bytes().any(|b| b.is_ascii_digit()) {
        // Check more carefully: look for digit:digit pattern
        let bytes = msg.as_bytes();
        let has_pos = bytes.windows(3).any(|w| {
            w[0].is_ascii_digit() && w[1] == b':' && w[2].is_ascii_digit()
        });
        if has_pos {
            return err;
        }
    }
    match err {
        EvalError::Parse(m) => EvalError::Parse(format!("{m} at {span}")),
        EvalError::Type(m) => EvalError::Type(format!("{m} at {span}")),
        EvalError::UnboundVariable(m) => EvalError::UnboundVariable(format!("{m} at {span}")),
        EvalError::Arity(m) => EvalError::Arity(format!("{m} at {span}")),
        EvalError::Runtime(m) => EvalError::Runtime(format!("{m} at {span}")),
    }
}

pub(crate) fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Float(x) => Ok(Val::Float(*x)),
        ExprKind::Rational(n, d) => Ok(Val::Rational(*n, *d)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, span),
                    "if" => return eval_if(&elems[1..], env, span),
                    "quote" => return eval_quote(&elems[1..], span),
                    "lambda" => return eval_lambda(&elems[1..], env, span),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, span),
                    "cond" => return eval_cond(&elems[1..], env),
                    "set!" => return eval_set_bang(&elems[1..], env, span),
                    "string-set!" => return eval_string_set(&elems[1..], env, span),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, span),
                    "define-record-type" => return eval_define_record_type(&elems[1..], env, span),
                    "case-lambda" => return eval_case_lambda(&elems[1..], env, span),
                    "letrec" => return eval_letrec(&elems[1..], env, span),
                    "letrec*" => return eval_letrec_star(&elems[1..], env, span),
                    "case" => return eval_case(&elems[1..], env, span),
                    "do" => return eval_do(&elems[1..], env, span),
                    "let*" => return eval_let_star(&elems[1..], env, span),
                    _ => {
                        if let Some(Val::Macro { literals, rules, def_env }) = env.get(op) {
                            return eval_macro_call(elems, &literals, &rules, &def_env, env, span);
                        }
                    }
                }
            }
            // Evaluate function position
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..].iter().map(|e| eval(e, env)).collect::<Result<_, _>>()?;
            apply_val(&func, &args, env).map_err(|e| span_err(span, e))
        }
    }
}

pub(crate) fn apply_val(func: &Val, args: &[Val], caller_env: &Env) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env } => {
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                new_env.define(rest.clone(), Val::List(args[params.len()..].to_vec()));
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            }
        }
        Val::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if let Some(_rest) = rest_param {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let new_env = env.push();
                    for (p, a) in params.iter().zip(args.iter()) {
                        new_env.define(p.clone(), a.clone());
                    }
                    if let Some(rest) = rest_param {
                        new_env.define(rest.clone(), Val::List(args[params.len()..].to_vec()));
                    }
                    let mut result = Val::Void;
                    for expr in body {
                        result = eval(expr, &new_env)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "no matching clause for {} arguments", args.len()
            )))
        }
        Val::Builtin(f) => f(args, caller_env),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse(format!("missing rest parameter after . at {span}")));
                }
                rest_param = Some(match &exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("expected rest parameter name at {span}"))),
                });
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse(format!("expected parameter name at {span}"))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

// --- Macros (syntax-rules) --- see macros.rs

fn eval_define_record_type(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Parse(format!("define-record-type: expected at least 3 arguments at {span}")));
    }
    // args[0] = type name (ignored, but parsed)
    // args[1] = (constructor-name field-name ...)
    // args[2] = predicate-name
    // args[3..] = (field-name accessor-name) ...

    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(elems) if !elems.is_empty() => {
            let cname = match &elems[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor name at {span}"))),
            };
            let fields: Vec<String> = elems[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!("define-record-type: expected field name at {span}"))),
            }).collect::<Result<_, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Parse(format!("define-record-type: expected constructor at {span}"))),
    };

    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse(format!("define-record-type: expected predicate name at {span}"))),
    };

    // Parse field accessors
    let mut accessors: Vec<(String, String)> = Vec::new(); // (field_name, accessor_name)
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(elems) if elems.len() >= 2 => {
                let field = match &elems[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define-record-type: expected field name at {span}"))),
                };
                let accessor = match &elems[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("define-record-type: expected accessor name at {span}"))),
                };
                accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Parse(format!("define-record-type: expected field spec at {span}"))),
        }
    }

    // Use gensym tags and regular lists/lambdas to represent records
    let tag = gensym("record");

    // Constructor: (lambda (f1 f2 ...) (list '<tag> f1 f2 ...))
    // We define it directly as a Lambda with a body that creates a tagged list
    {
        let params = constructor_fields.clone();
        let tag_sym = tag.clone();
        // Build body: (list (quote <tag>) f1 f2 ...)
        let span0 = Span::new(0, 0);
        let mut list_args = vec![
            Expr::new(ExprKind::Symbol("list".into()), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("quote".into()), span0),
                Expr::new(ExprKind::Symbol(tag_sym), span0),
            ]), span0),
        ];
        for p in &params {
            list_args.push(Expr::new(ExprKind::Symbol(p.clone()), span0));
        }
        let body = vec![Expr::new(ExprKind::List(list_args), span0)];

        env.define(constructor_name, Val::Lambda {
            params,
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    // Predicate: checks if value is a list whose car is the tag
    {
        let tag_sym = tag.clone();
        let param = "__rec_v".to_string();
        let span0 = Span::new(0, 0);
        // Body: (and (pair? __rec_v) (equal? (car __rec_v) (quote <tag>)))
        let body = vec![Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("and".into()), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("pair?".into()), span0),
                Expr::new(ExprKind::Symbol(param.clone()), span0),
            ]), span0),
            Expr::new(ExprKind::List(vec![
                Expr::new(ExprKind::Symbol("equal?".into()), span0),
                Expr::new(ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("car".into()), span0),
                    Expr::new(ExprKind::Symbol(param.clone()), span0),
                ]), span0),
                Expr::new(ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".into()), span0),
                    Expr::new(ExprKind::Symbol(tag_sym), span0),
                ]), span0),
            ]), span0),
        ]), span0)];

        env.define(pred_name, Val::Lambda {
            params: vec![param],
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    // Accessors: (lambda (v) (list-ref v <index>))
    for (field_name, accessor_name) in &accessors {
        // Find field index in constructor_fields
        let idx = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Parse(format!(
                "define-record-type: field '{}' not in constructor at {span}", field_name
            )))?;
        let span0 = Span::new(0, 0);
        let param = "__rec_v".to_string();
        // Body: (list-ref __rec_v <idx+1>)  (+1 because index 0 is the tag)
        let body = vec![Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("list-ref".into()), span0),
            Expr::new(ExprKind::Symbol(param.clone()), span0),
            Expr::new(ExprKind::Int((idx + 1) as i64), span0),
        ]), span0)];

        env.define(accessor_name.clone(), Val::Lambda {
            params: vec![param],
            rest_param: None,
            body,
            env: env.clone(),
        });
    }

    Ok(Val::Void)
}

fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("define-syntax: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse(format!("define-syntax: expected symbol at {span}"))),
    };
    let transformer = match &args[1].kind {
        ExprKind::List(elems) => elems,
        _ => return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {span}"))),
    };
    if transformer.is_empty()
        || !matches!(&transformer[0].kind, ExprKind::Symbol(s) if s == "syntax-rules")
    {
        return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {span}")));
    }
    if transformer.len() < 2 {
        return Err(EvalError::Parse(format!("syntax-rules: missing literals at {span}")));
    }
    let literals = match &transformer[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!(
                    "syntax-rules: expected literal symbol at {span}"
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Parse(format!(
                "syntax-rules: expected literals list at {span}"
            )))
        }
    };
    let mut rules = Vec::new();
    for rule_expr in &transformer[2..] {
        match &rule_expr.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "syntax-rules: invalid rule at {span}"
                )))
            }
        }
    }
    env.define(
        name,
        Val::Macro {
            literals,
            rules,
            def_env: env.clone(),
        },
    );
    Ok(Val::Void)
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("define: missing arguments at {span}")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("define: expected 2 arguments at {span}")));
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Val::Void)
        }
        ExprKind::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("define: empty signature at {span}")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define: expected symbol at {span}"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], span)?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse(format!("define: expected symbol or list at {span}"))),
    }
}

fn eval_set_bang(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("set!: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s,
        _ => return Err(EvalError::Parse(format!("set!: expected symbol at {span}"))),
    };
    let val = eval(&args[1], env)?;
    env.set(name, val).map_err(|e| span_err(span, e))?;
    Ok(Val::Void)
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {span}")));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Val::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("quote: expected 1 argument at {span}")));
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Float(x) => Ok(Val::Float(*x)),
        ExprKind::Rational(n, d) => Ok(Val::Rational(*n, *d)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems.iter().map(expr_to_val).collect::<Result<_, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("lambda: missing parameters at {span}")));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
        ExprKind::Symbol(s) => {
            // (lambda rest body...) — single rest param
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {span}"))),
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_case_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Err(EvalError::Parse(format!("case-lambda: empty clause at {span}")));
                }
                let (params, rest_param) = match &elems[0].kind {
                    ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
                    ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Parse(format!("case-lambda: expected parameter list at {span}"))),
                };
                let body = elems[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => return Err(EvalError::Parse(format!("case-lambda: expected clause at {span}"))),
        }
    }
    Ok(Val::CaseLambda { clauses, env: env.clone() })
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(true));
    }
    let mut result = Val::Bool(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Val::Bool(false))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("let: missing arguments at {span}")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::Parse(format!("let: missing bindings at {span}")));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let body = args[2..].to_vec();
        let new_env = env.push();
        let lambda = Val::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: new_env.clone(),
        };
        new_env.define(name.clone(), lambda.clone());
        apply_val(&lambda, &inits, &new_env)
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let new_env = env.push();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        let val = eval(&pair[1], env)?;
                        new_env.define(s.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let mut result = Val::Void;
        for expr in &args[1..] {
            result = eval(expr, &new_env)?;
        }
        Ok(result)
    }
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Val, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Val::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse(format!("cond: invalid clause at {}", clause.span))),
        }
    }
    Ok(Val::Void)
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {span}")));
    }
    // String literals are immutable (L15)
    if matches!(&args[0].kind, ExprKind::Str(_)) {
        return Err(EvalError::Runtime("string-set!: strings are immutable".into()));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("string-set!: expected variable at {span}"))),
    };
    let idx_val = eval(&args[1], env)?;
    let idx = match idx_val {
        Val::Int(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into())),
    };
    let char_val = eval(&args[2], env)?;
    let ch = match char_val {
        Val::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected character".into())),
    };
    let current = env.get(&name).ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))?;
    match current {
        Val::Str(s) => {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Runtime("string-set!: index out of range".into()));
            }
            chars[idx] = ch;
            let new_str: String = chars.into_iter().collect();
            env.set(&name, Val::Str(new_str))?;
            Ok(Val::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string".into())),
    }
}

fn eval_letrec(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("letrec: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("letrec: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    // First pass: define all variables as Void
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    names.push(s.clone());
                    init_exprs.push(&pair[1]);
                    new_env.define(s.clone(), Val::Void);
                } else {
                    return Err(EvalError::Parse(format!("letrec: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("letrec: invalid binding at {span}"))),
        }
    }
    // Second pass: evaluate inits in the new env and set them
    for (name, init_expr) in names.iter().zip(init_exprs.iter()) {
        let val = eval(init_expr, &new_env)?;
        new_env.set(name, val)?;
    }
    let mut result = Val::Void;
    for expr in &args[1..] {
        result = eval(expr, &new_env)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("letrec*: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("letrec*: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], &new_env)?;
                    new_env.define(s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("letrec*: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("letrec*: invalid binding at {span}"))),
        }
    }
    let mut result = Val::Void;
    for expr in &args[1..] {
        result = eval(expr, &new_env)?;
    }
    Ok(result)
}

fn eval_let_star(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("let*: missing arguments at {span}")));
    }
    let bindings = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Parse(format!("let*: expected bindings list at {span}"))),
    };
    let new_env = env.push();
    for b in bindings {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval(&pair[1], &new_env)?;
                    new_env.define(s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("let*: expected variable name at {span}")));
                }
            }
            _ => return Err(EvalError::Parse(format!("let*: invalid binding at {span}"))),
        }
    }
    let mut result = Val::Void;
    for expr in &args[1..] {
        result = eval(expr, &new_env)?;
    }
    Ok(result)
}

fn vals_eqv(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::List(a), Val::List(b)) if a.is_empty() && b.is_empty() => true,
        (Val::Void, Val::Void) => true,
        _ => std::ptr::eq(a as *const Val, b as *const Val),
    }
}

fn eval_case(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("case: missing arguments at {span}")));
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                // Check for else
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Val::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                // Normal clause: ((datum ...) body ...)
                let datums = match &parts[0].kind {
                    ExprKind::List(d) => d,
                    _ => return Err(EvalError::Parse(format!("case: expected datum list at {span}"))),
                };
                for datum in datums {
                    let datum_val = expr_to_val(datum)?;
                    if vals_eqv(&key, &datum_val) {
                        let mut result = Val::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
            }
            _ => return Err(EvalError::Parse(format!("case: invalid clause at {span}"))),
        }
    }
    Ok(Val::Void)
}

fn eval_do(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Parse(format!("do: expected at least 2 arguments at {span}")));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(v) => v,
        _ => return Err(EvalError::Parse(format!("do: expected variable list at {span}"))),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(t) => t,
        _ => return Err(EvalError::Parse(format!("do: expected test clause at {span}"))),
    };
    if test_clause.is_empty() {
        return Err(EvalError::Parse(format!("do: empty test clause at {span}")));
    }

    // Parse variable specs
    struct DoVar<'a> {
        name: String,
        step: Option<&'a Expr>,
    }
    let mut vars = Vec::new();
    let do_env = env.push();
    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("do: expected variable name at {span}"))),
                };
                let init = eval(&parts[1], env)?;
                let step = if parts.len() >= 3 { Some(&parts[2]) } else { None };
                do_env.define(name.clone(), init);
                vars.push(DoVar { name, step });
            }
            _ => return Err(EvalError::Parse(format!("do: invalid variable spec at {span}"))),
        }
    }

    // Iterate
    loop {
        // Test
        let test_result = eval(&test_clause[0], &do_env)?;
        if test_result.is_truthy() {
            // Evaluate result expressions
            if test_clause.len() > 1 {
                let mut result = Val::Void;
                for expr in &test_clause[1..] {
                    result = eval(expr, &do_env)?;
                }
                return Ok(result);
            }
            return Ok(Val::Void);
        }

        // Evaluate body
        for expr in &args[2..] {
            eval(expr, &do_env)?;
        }

        // Step: evaluate ALL step expressions using current values, then update
        let new_vals: Vec<Option<Val>> = vars.iter().map(|v| {
            match v.step {
                Some(step_expr) => Ok(Some(eval(step_expr, &do_env)?)),
                None => Ok(None),
            }
        }).collect::<Result<_, EvalError>>()?;

        for (v, new_val) in vars.iter().zip(new_vals.into_iter()) {
            if let Some(val) = new_val {
                do_env.set(&v.name, val)?;
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    let output = env.output.borrow().clone();
    Ok((last.to_string(), output))
}

#[cfg(test)]
mod tests;
