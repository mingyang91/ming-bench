pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

type VecRef = Rc<RefCell<Vec<Value>>>;
type PairRef = Rc<RefCell<(Value, Value)>>;

// Continuation support
static CC_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn next_cc_id() -> usize {
    CC_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

// A continuation value: a function from Value to Result<Value, EvalError>
// Wrapped in Rc so it's Clone.
struct ContFn(Rc<dyn Fn(Value) -> Result<Value, EvalError>>);

impl ContFn {
    fn new(f: impl Fn(Value) -> Result<Value, EvalError> + 'static) -> Self {
        ContFn(Rc::new(f))
    }
    fn call(&self, v: Value) -> Result<Value, EvalError> {
        (self.0)(v)
    }
}

impl Clone for ContFn {
    fn clone(&self) -> Self {
        ContFn(self.0.clone())
    }
}

impl fmt::Debug for ContFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<continuation>")
    }
}

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}##{}", base, n)
}

#[derive(Debug, Clone, Copy, Default)]
struct Pos {
    line: usize,
    col: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator; always simplified, denom > 0
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    Nil,
    Pair(PairRef),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Record {
        type_id: usize,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    RecordConstructor {
        type_id: usize,
        type_name: String,
        field_names: Vec<String>,
    },
    RecordPredicate {
        type_id: usize,
    },
    RecordAccessor {
        type_id: usize,
        field_name: String,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>,
        env: Env,
    },
    Vector(VecRef),
    TailCall {
        expr: Box<Expr>,
        env: Env,
    },
    // A captured continuation (call/cc)
    Continuation {
        id: usize,
        func: ContFn,
    },
    // Multiple return values (from `values`)
    Values(Vec<Value>),
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

static WIND_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn next_wind_id() -> usize {
    WIND_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

#[derive(Clone)]
struct WindEntry {
    id: usize,
    in_thunk: Value,
    out_thunk: Value,
}

// Restart context: the body context captured when a call/cc continuation is created.
// Used by reentrant continuations to replay from the correct position.
struct RestartCtx {
    body_exprs: Vec<Expr>,
    body_idx: usize,  // index of the expression CONTAINING the call/cc
    body_env: Env,
    outer_k: ContFn,
}

thread_local! {
    static OUTPUT: RefCell<String> = RefCell::new(String::new());
    // Stack of active call/cc IDs (for escape detection)
    static ACTIVE_CC: RefCell<Vec<usize>> = RefCell::new(Vec::new());
    // Override stack: when a reentrant continuation is invoked, it pushes the value here.
    // The next call/cc evaluation that's part of the replay pops and returns this value.
    static CC_OVERRIDE_STACK: RefCell<Vec<Value>> = RefCell::new(Vec::new());
    // Current body context: set by eval_body_cps before evaluating each expression.
    // read by apply_cps_callcc to build the restart function.
    static BODY_CTX: RefCell<Option<RestartCtx>> = RefCell::new(None);
    // Dynamic-wind stack: tracks active dynamic-wind in/out thunks.
    static WIND_STACK: RefCell<Vec<WindEntry>> = RefCell::new(Vec::new());
}

fn write_output(s: &str) {
    OUTPUT.with(|out| out.borrow_mut().push_str(s));
}

fn display_value(val: &Value) -> String {
    match val {
        Value::Str(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        Value::Float(x) => {
            if x.fract() == 0.0 && x.is_finite() {
                format!("{:.1}", x)
            } else {
                format!("{}", x)
            }
        }
        Value::Vector(v) => {
            let elems = v.borrow();
            let mut s = String::from("#(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                s.push_str(&display_value(e));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Some(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_set_existing(env: &Env, name: &str, val: Value) {
    {
        let inner = env.borrow();
        if !inner.bindings.contains_key(name) {
            if let Some(ref parent) = inner.parent {
                let parent = parent.clone();
                drop(inner);
                env_set_existing(&parent, name, val);
                return;
            }
            return;
        }
    }
    env.borrow_mut().bindings.insert(name.to_string(), val);
}

fn default_env() -> Env {
    let env = new_env(None);
    env
}

fn fmt_pair(pr: &PairRef, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    let mut seen = Vec::new();
    let mut cur_ref = pr.clone();
    let mut first = true;
    loop {
        let ptr = Rc::as_ptr(&cur_ref) as usize;
        if seen.contains(&ptr) {
            write!(f, " ...")?;
            break;
        }
        seen.push(ptr);
        let car;
        let cdr;
        {
            let p = cur_ref.borrow();
            car = p.0.clone();
            cdr = p.1.clone();
        }
        if !first {
            write!(f, " ")?;
        }
        first = false;
        write!(f, "{}", car)?;
        match cdr {
            Value::Pair(ref next) => {
                cur_ref = next.clone();
            }
            Value::Nil => break,
            other => {
                write!(f, " . {}", other)?;
                break;
            }
        }
    }
    write!(f, ")")
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(x) => {
                if x.fract() == 0.0 && x.is_finite() {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
            Value::Rational(n, d) => write!(f, "{}/{}", n, d),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                c => write!(f, "#\\{}", c),
            },
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Nil => write!(f, "()"),
            Value::Pair(pr) => fmt_pair(pr, f),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::CaseLambda { .. } => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Record { type_name, .. } => write!(f, "#<record:{}>", type_name),
            Value::RecordConstructor { .. } => write!(f, "#<procedure>"),
            Value::RecordPredicate { .. } => write!(f, "#<procedure>"),
            Value::RecordAccessor { .. } => write!(f, "#<procedure>"),
            Value::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Value::TailCall { .. } => write!(f, "#<tailcall>"),
            Value::Continuation { .. } => write!(f, "#<continuation>"),
            Value::Values(vals) => {
                // Display the last value (or void for empty)
                if let Some(v) = vals.last() {
                    write!(f, "{}", v)
                } else {
                    write!(f, "")
                }
            }
        }
    }
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_string_at(&self, pos: Pos) -> Result<&str, EvalError> {
        match self {
            Value::Str(s) => Ok(s.as_str()),
            _ => Err(EvalError::Type(format!(
                "expected string, got {} at {}",
                self, pos
            ))),
        }
    }

    fn as_integer_at(&self, pos: Pos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!(
                "expected number, got {} at {}",
                self, pos
            ))),
        }
    }
}

/// Convert a Rust Vec of Values into a proper Scheme list (cons chain ending in Nil).
fn vec_to_list(vals: Vec<Value>) -> Value {
    let mut result = Value::Nil;
    for v in vals.into_iter().rev() {
        result = make_pair(v, result);
    }
    result
}

fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn make_rational(n: i64, d: i64) -> Value {
    if d == 0 {
        panic!("division by zero in make_rational");
    }
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

// Numeric coercion helpers
#[derive(Debug, Clone, Copy)]
enum NumVal {
    Int(i64),
    Rat(i64, i64),
    Flt(f64),
}

fn to_num(v: &Value, p: Pos) -> Result<NumVal, EvalError> {
    match v {
        Value::Integer(n) => Ok(NumVal::Int(*n)),
        Value::Rational(n, d) => Ok(NumVal::Rat(*n, *d)),
        Value::Float(f) => Ok(NumVal::Flt(*f)),
        _ => Err(EvalError::Type(format!("expected number, got {} at {}", v, p))),
    }
}

fn num_to_f64(n: &NumVal) -> f64 {
    match n {
        NumVal::Int(i) => *i as f64,
        NumVal::Rat(n, d) => *n as f64 / *d as f64,
        NumVal::Flt(f) => *f,
    }
}

fn is_inexact(n: &NumVal) -> bool {
    matches!(n, NumVal::Flt(_))
}

fn num_add(a: NumVal, b: NumVal) -> Value {
    if is_inexact(&a) || is_inexact(&b) {
        return Value::Float(num_to_f64(&a) + num_to_f64(&b));
    }
    let (an, ad) = match a { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    let (bn, bd) = match b { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    make_rational(an * bd + bn * ad, ad * bd)
}

fn num_sub(a: NumVal, b: NumVal) -> Value {
    if is_inexact(&a) || is_inexact(&b) {
        return Value::Float(num_to_f64(&a) - num_to_f64(&b));
    }
    let (an, ad) = match a { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    let (bn, bd) = match b { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    make_rational(an * bd - bn * ad, ad * bd)
}

fn num_mul(a: NumVal, b: NumVal) -> Value {
    if is_inexact(&a) || is_inexact(&b) {
        return Value::Float(num_to_f64(&a) * num_to_f64(&b));
    }
    let (an, ad) = match a { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    let (bn, bd) = match b { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    make_rational(an * bn, ad * bd)
}

fn num_div(a: NumVal, b: NumVal, p: Pos) -> Result<Value, EvalError> {
    if is_inexact(&a) || is_inexact(&b) {
        let bd = num_to_f64(&b);
        if bd == 0.0 {
            return Err(EvalError::DivisionByZero(format!("{}", p)));
        }
        return Ok(Value::Float(num_to_f64(&a) / bd));
    }
    let (an, ad) = match a { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    let (bn, bd) = match b { NumVal::Int(i) => (i, 1i64), NumVal::Rat(n, d) => (n, d), _ => unreachable!() };
    if bn == 0 {
        return Err(EvalError::DivisionByZero(format!("{}", p)));
    }
    Ok(make_rational(an * bd, ad * bn))
}

fn num_to_value(n: NumVal) -> Value {
    match n {
        NumVal::Int(i) => Value::Integer(i),
        NumVal::Rat(n, d) => make_rational(n, d),
        NumVal::Flt(f) => Value::Float(f),
    }
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 {
        return (0, 1);
    }
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    let mut p0: i64 = 0;
    let mut q0: i64 = 1;
    let mut p1: i64 = 1;
    let mut q1: i64 = 0;
    let mut x = f;
    for _ in 0..64 {
        let a = x.floor() as i64;
        let p2 = a * p1 + p0;
        let q2 = a * q1 + q0;
        p0 = p1;
        q0 = q1;
        p1 = p2;
        q1 = q2;
        let approx = p1 as f64 / q1 as f64;
        if (approx - f).abs() < 1e-12 {
            break;
        }
        let frac = x - a as f64;
        if frac < 1e-12 {
            break;
        }
        x = 1.0 / frac;
    }
    (sign * p1, q1)
}

fn is_numeric_value(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

// --- Parser ---

#[derive(Debug, Clone)]
enum Expr {
    Integer(i64, Pos),
    Float(f64, Pos),
    Rational(i64, i64, Pos),
    Boolean(bool, Pos),
    Str(String, Pos),
    Char(char, Pos),
    Symbol(String, Pos),
    List(Vec<Expr>, Pos),
}

impl Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Integer(_, p) => *p,
            Expr::Float(_, p) => *p,
            Expr::Rational(_, _, p) => *p,
            Expr::Boolean(_, p) => *p,
            Expr::Str(_, p) => *p,
            Expr::Char(_, p) => *p,
            Expr::Symbol(_, p) => *p,
            Expr::List(_, p) => *p,
        }
    }
}

struct Parser {
    tokens: Vec<(String, Pos)>,
    pos: usize,
}

fn tokenize(input: &str) -> Vec<(String, Pos)> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1usize;
    let mut col = 1usize;
    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push(("(".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((")".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(("'".into(), Pos { line, col }));
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos { line, col };
                let mut s = String::from("\"");
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        if chars[i + 1] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 2;
                        }
                        i += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push((s, start_pos));
            }
            _ => {
                let start_pos = Pos { line, col };
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(
                        chars[i],
                        ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' | '\''
                    )
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push((tok, start_pos));
            }
        }
    }
    tokens
}

impl Parser {
    fn new(input: &str) -> Self {
        Parser {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let (tok, tpos) = self.tokens[self.pos].clone();
        self.pos += 1;

        if tok == "'" {
            let inner = self.parse_expr()?;
            Ok(Expr::List(
                vec![Expr::Symbol("quote".into(), tpos), inner],
                tpos,
            ))
        } else if tok == "(" {
            let list_pos = tpos;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos].0 != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse(format!(
                    "missing closing paren at {}",
                    list_pos
                )));
            }
            self.pos += 1; // skip ')'
            Ok(Expr::List(elems, list_pos))
        } else if tok == ")" {
            Err(EvalError::Parse(format!("unexpected ')' at {}", tpos)))
        } else if tok == "#t" {
            Ok(Expr::Boolean(true, tpos))
        } else if tok == "#f" {
            Ok(Expr::Boolean(false, tpos))
        } else if tok.starts_with('"') && tok.ends_with('"') {
            let inner = &tok[1..tok.len() - 1];
            let mut result = String::new();
            let chars: Vec<char> = inner.chars().collect();
            let mut j = 0;
            while j < chars.len() {
                if chars[j] == '\\' && j + 1 < chars.len() {
                    match chars[j + 1] {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        c => {
                            result.push('\\');
                            result.push(c);
                        }
                    }
                    j += 2;
                } else {
                    result.push(chars[j]);
                    j += 1;
                }
            }
            Ok(Expr::Str(result, tpos))
        } else if tok.starts_with("#\\") {
            let ch = match &tok[2..] {
                "space" => ' ',
                "newline" => '\n',
                "tab" => '\t',
                s if s.len() == 1 => s.chars().next().unwrap(),
                _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {}", tok, tpos))),
            };
            Ok(Expr::Char(ch, tpos))
        } else if let Ok(n) = tok.parse::<i64>() {
            Ok(Expr::Integer(n, tpos))
        } else if let Some(idx) = tok.find('/') {
            let num_str = &tok[..idx];
            let den_str = &tok[idx+1..];
            if let (Ok(n), Ok(d)) = (num_str.parse::<i64>(), den_str.parse::<i64>()) {
                if d != 0 {
                    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
                    let na = n.abs();
                    let da = d.abs();
                    let g = gcd(na, da);
                    let sn = sign * (na / g);
                    let sd = da / g;
                    if sd == 1 {
                        Ok(Expr::Integer(sn, tpos))
                    } else {
                        Ok(Expr::Rational(sn, sd, tpos))
                    }
                } else {
                    Ok(Expr::Symbol(tok, tpos))
                }
            } else {
                Ok(Expr::Symbol(tok, tpos))
            }
        } else if let Ok(f) = tok.parse::<f64>() {
            Ok(Expr::Float(f, tpos))
        } else {
            Ok(Expr::Symbol(tok, tpos))
        }
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        while self.pos < self.tokens.len() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// --- Evaluator ---

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not"
            | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
            | "number?" | "string?" | "boolean?" | "pair?" | "symbol?" | "char?"
            | "display" | "write" | "newline"
            | "string-append" | "string-length" | "substring"
            | "string->number" | "number->string"
            | "symbol->string" | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "string->list" | "list->string"
            | "char->integer" | "integer->char"
            | "apply"
            | "abs" | "modulo" | "remainder" | "quotient"
            | "min" | "max" | "expt"
            | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
            | "list-ref" | "list-tail" | "list?" | "assoc"
            | "map" | "eq?" | "equal?"
            | "char-alphabetic?" | "char-numeric?"
            | "char-upcase" | "char-downcase"
            | "char=?" | "char<?"
            | "string=?" | "string<?" | "string-ci=?"
            | "string-upcase" | "string-downcase"
            | "exact?" | "inexact?" | "rational?" | "integer?"
            | "exact->inexact" | "inexact->exact"
            | "numerator" | "denominator"
            | "procedure?"
            | "eqv?"
            | "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
            | "vector?" | "vector->list" | "list->vector"
            | "assq" | "memq" | "for-each"
            | "set-car!" | "set-cdr!"
            | "cddr" | "cadr" | "cdar" | "caar" | "caddr" | "cdddr" | "cdadr"
            | "cadar" | "caddar"
            | "reverse" | "member" | "memv" | "assv" | "sort" | "error"
            | "gcd" | "lcm" | "truncate" | "round" | "floor" | "ceiling"
            | "make-string" | "string" | "string>?" | "string<=?" | "string>=?"
            | "call/cc" | "call-with-current-continuation"
            | "dynamic-wind"
            | "raise" | "with-exception-handler"
            | "values" | "call-with-values"
    )
}

fn force(mut result: Value) -> Result<Value, EvalError> {
    loop {
        match result {
            Value::TailCall { expr, env } => {
                result = eval_step(&expr, &env)?;
            }
            v => return Ok(v),
        }
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    force(eval_step(expr, env)?)
}

fn eval_step(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Float(f, _) => Ok(Value::Float(*f)),
        Expr::Rational(n, d, _) => Ok(Value::Rational(*n, *d)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(name, _) => {
            if let Some(val) = env_get(env, name) {
                Ok(val)
            } else if is_builtin(name) {
                Ok(Value::Symbol(name.clone()))
            } else {
                Err(EvalError::UnboundVariable(format!("{} at {}", name, p)))
            }
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(Value::Nil);
            }
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, p),
                    "if" => return eval_if(&elems[1..], env, p),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!(
                                "quote expects 1 argument at {}",
                                p
                            )));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], env, p),
                    "case-lambda" => return eval_case_lambda(&elems[1..], env, p),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, p),
                    "begin" => return eval_begin(&elems[1..], env),
                    "cond" => return eval_cond(&elems[1..], env),
                    "string-set!" => return eval_string_set(&elems[1..], env, p),
                    "set!" => return eval_set(&elems[1..], env, p),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, p),
                    "define-record-type" => return eval_define_record_type(&elems[1..], env, p),
                    "letrec" => return eval_letrec(&elems[1..], env, p),
                    "letrec*" => return eval_letrec_star(&elems[1..], env, p),
                    "case" => return eval_case(&elems[1..], env, p),
                    "do" => return eval_do(&elems[1..], env, p),
                    "let*" => return eval_let_star(&elems[1..], env, p),
                    "when" => return eval_when(&elems[1..], env, p),
                    "dynamic-wind" => return eval_dynamic_wind(&elems[1..], env, p),
                    "guard" => return eval_guard(&elems[1..], env, p),
                    _ => {
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            return eval_macro(&literals, &rules, &def_env, elems, env, p);
                        }
                    }
                }
            }
            // Function call
            let func = eval(&elems[0], env)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env))
                .collect::<Result<_, _>>()?;
            apply_value(&func, &args, p)
        }
    }
}

fn eval_dynamic_wind(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("dynamic-wind expects 3 arguments at {}", p)));
    }
    let in_thunk = eval(&args[0], env)?;
    let body_thunk = eval(&args[1], env)?;
    let out_thunk = eval(&args[2], env)?;

    force(apply_value(&in_thunk, &[], p)?)?;

    let wind_id = next_wind_id();
    WIND_STACK.with(|ws| ws.borrow_mut().push(WindEntry {
        id: wind_id,
        in_thunk: in_thunk.clone(),
        out_thunk: out_thunk.clone(),
    }));

    let body_result = (|| -> Result<Value, EvalError> {
        force(apply_value(&body_thunk, &[], p)?)
    })();

    WIND_STACK.with(|ws| ws.borrow_mut().pop());
    force(apply_value(&out_thunk, &[], p)?)?;

    body_result
}

/// (guard (var clause ...) body ...)
/// Evaluate body. If it raises, bind var to the raised value and evaluate clauses like cond.
fn eval_guard(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("guard requires at least 2 arguments at {}", p)));
    }
    // First arg is (var clause1 clause2 ...)
    let header = match &args[0] {
        Expr::List(elems, _) if elems.len() >= 2 => elems,
        _ => return Err(EvalError::Type(format!("guard: bad syntax at {}", p))),
    };
    let var_name = match &header[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!("guard: expected variable name at {}", p))),
    };
    let clauses = &header[1..];
    let body = &args[1..];

    // Evaluate body
    let body_result = (|| -> Result<Value, EvalError> {
        let mut result = Value::Boolean(false);
        for expr in body {
            result = eval(expr, env)?;
        }
        Ok(result)
    })();

    match body_result {
        Ok(v) => Ok(v),
        Err(EvalError::RaisedValue(val)) => {
            // Bind var to raised value, evaluate clauses
            let guard_env = new_env(Some(env.clone()));
            env_set(&guard_env, var_name, *val.clone());
            eval_guard_clauses(clauses, &guard_env, *val)
        }
        Err(e) => Err(e),
    }
}

fn eval_guard_clauses(clauses: &[Expr], env: &Env, raised_val: Value) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                // Check for else clause
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        // Evaluate the else body
                        let mut result = Value::Boolean(false);
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                // Evaluate the test
                let test_val = eval(&parts[0], env)?;
                if !matches!(test_val, Value::Boolean(false)) {
                    // Test passed, evaluate body (or return test value if no body)
                    if parts.len() == 1 {
                        return Ok(test_val);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => {}
        }
    }
    // No clause matched, re-raise
    Err(EvalError::RaisedValue(Box::new(raised_val)))
}

fn apply_value(func: &Value, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match func {
        Value::Symbol(op) => apply_builtin(op, args, call_pos),
        Value::Lambda { params, rest_param, body, env } => {
            if let Some(ref rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        call_pos
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (p, a) in params.iter().zip(args.iter()) {
                    env_set(&local_env, p.clone(), a.clone());
                }
                let rest_list = vec_to_list(args[params.len()..].to_vec());
                env_set(&local_env, rest.clone(), rest_list);
                if body.is_empty() {
                    return Ok(Value::Boolean(false));
                }
                for expr in &body[..body.len() - 1] {
                    eval(expr, &local_env)?;
                }
                return Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env });
            }
            if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {} at {}",
                    params.len(),
                    args.len(),
                    call_pos
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&local_env, p.clone(), a.clone());
            }
            if body.is_empty() {
                return Ok(Value::Boolean(false));
            }
            for expr in &body[..body.len() - 1] {
                eval(expr, &local_env)?;
            }
            Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env })
        }
        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{} constructor expects {} arguments, got {} at {}",
                    type_name, field_names.len(), args.len(), call_pos
                )));
            }
            let fields: Vec<(String, Value)> = field_names.iter()
                .zip(args.iter())
                .map(|(n, v)| (n.clone(), v.clone()))
                .collect();
            Ok(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields,
            })
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record predicate expects 1 argument, got {} at {}",
                    args.len(), call_pos
                )));
            }
            match &args[0] {
                Value::Record { type_id: tid, .. } => Ok(Value::Boolean(*tid == *type_id)),
                _ => Ok(Value::Boolean(false)),
            }
        }
        Value::RecordAccessor { type_id, field_name } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record accessor expects 1 argument, got {} at {}",
                    args.len(), call_pos
                )));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if *tid == *type_id => {
                    for (name, val) in fields {
                        if name == field_name {
                            return Ok(val.clone());
                        }
                    }
                    Err(EvalError::Type(format!("no field {} at {}", field_name, call_pos)))
                }
                _ => Err(EvalError::Type(format!(
                    "record accessor: wrong type at {}", call_pos
                ))),
            }
        }
        Value::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if let Some(_) = rest_param {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = new_env(Some(env.clone()));
                    for (p, a) in params.iter().zip(args.iter()) {
                        env_set(&local_env, p.clone(), a.clone());
                    }
                    if let Some(ref rest) = rest_param {
                        let rest_list = vec_to_list(args[params.len()..].to_vec());
                        env_set(&local_env, rest.clone(), rest_list);
                    }
                    if body.is_empty() {
                        return Ok(Value::Boolean(false));
                    }
                    for expr in &body[..body.len() - 1] {
                        eval(expr, &local_env)?;
                    }
                    return Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env });
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments at {}",
                args.len(), call_pos
            )))
        }
        Value::Continuation { id, func } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation expects 1 argument, got {} at {}",
                    args.len(), call_pos
                )));
            }
            let val = args[0].clone();
            // Check if this continuation is currently active (escape path)
            let is_active = ACTIVE_CC.with(|ac| ac.borrow().contains(id));
            if is_active {
                // Escape: propagate as an error through the call stack
                Err(EvalError::ContinuationEscape(*id, Box::new(val)))
            } else {
                // Reentrant: call the stored restart function
                func.call(val)
            }
        }
        _ => Err(EvalError::Type(format!(
            "not a procedure: {} at {}",
            func, call_pos
        ))),
    }
}

/// Parse a parameter list, detecting dot notation for rest params.
fn parse_params(param_exprs: &[Expr], p: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i] {
            Expr::Symbol(s, _) if s == "." => {
                if i + 1 >= param_exprs.len() || i + 2 != param_exprs.len() {
                    return Err(EvalError::Parse(format!(
                        "invalid dot notation in parameter list at {}", p
                    )));
                }
                match &param_exprs[i + 1] {
                    Expr::Symbol(rest, _) => rest_param = Some(rest.clone()),
                    _ => return Err(EvalError::Type(format!(
                        "expected symbol after dot in parameter list at {}", p
                    ))),
                }
                break;
            }
            Expr::Symbol(s, _) => params.push(s.clone()),
            _ => return Err(EvalError::Type(format!(
                "expected symbol as parameter at {}", p
            ))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_define(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "define requires at least 2 arguments at {}",
            p
        )));
    }
    match &args[0] {
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "define requires exactly 2 arguments at {}",
                    p
                )));
            }
            let val = eval(&args[1], env)?;
            env_set(env, name.clone(), val);
            Ok(Value::Symbol(name.clone()))
        }
        Expr::List(name_and_params, _) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse(format!("define: empty name list at {}", p)));
            }
            let name = match &name_and_params[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "define: expected symbol as function name at {}",
                        p
                    )))
                }
            };
            let (params, rest_param) = parse_params(&name_and_params[1..], p)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name.clone(), lambda);
            Ok(Value::Symbol(name))
        }
        _ => Err(EvalError::Type(format!(
            "define: expected symbol or list at {}",
            p
        ))),
    }
}

fn eval_if(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!(
            "if requires 2 or 3 arguments at {}",
            p
        )));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        Ok(Value::TailCall { expr: Box::new(args[1].clone()), env: env.clone() })
    } else if args.len() == 3 {
        Ok(Value::TailCall { expr: Box::new(args[2].clone()), env: env.clone() })
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_lambda(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "lambda requires params and body at {}",
            p
        )));
    }
    let (params, rest_param) = match &args[0] {
        Expr::List(param_exprs, _) => parse_params(param_exprs, p)?,
        Expr::Symbol(s, _) => {
            (vec![], Some(s.clone()))
        }
        _ => {
            return Err(EvalError::Type(format!(
                "lambda: expected parameter list at {}",
                p
            )))
        }
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_case_lambda(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "case-lambda requires at least one clause at {}", p
        )));
    }
    let mut clauses = Vec::new();
    for clause in args {
        match clause {
            Expr::List(elems, cp) => {
                if elems.len() < 2 {
                    return Err(EvalError::Type(format!(
                        "case-lambda clause needs params and body at {}", cp
                    )));
                }
                let (params, rest_param) = match &elems[0] {
                    Expr::List(param_exprs, _) => parse_params(param_exprs, *cp)?,
                    Expr::Symbol(s, _) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Type(format!(
                        "case-lambda: expected parameter list at {}", cp
                    ))),
                };
                let body = elems[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => return Err(EvalError::Type(format!(
                "case-lambda: expected clause at {}", p
            ))),
        }
    }
    Ok(Value::CaseLambda {
        clauses,
        env: env.clone(),
    })
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Float(f, _) => Value::Float(*f),
        Expr::Rational(n, d, _) => Value::Rational(*n, *d),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::List(elems, _) => {
            if elems.is_empty() {
                Value::Nil
            } else {
                vec_to_list(elems.iter().map(expr_to_value).collect())
            }
        }
    }
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for (i, expr) in exprs.iter().enumerate() {
        if i == exprs.len() - 1 {
            return Ok(Value::TailCall { expr: Box::new(expr.clone()), env: env.clone() });
        }
        let result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    unreachable!()
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for (i, expr) in exprs.iter().enumerate() {
        if i == exprs.len() - 1 {
            return Ok(Value::TailCall { expr: Box::new(expr.clone()), env: env.clone() });
        }
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    unreachable!()
}

fn eval_let(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "let requires bindings and body at {}",
            p
        )));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!(
                "named let requires bindings and body at {}",
                p
            )));
        }
        let bindings_expr = match &args[1] {
            Expr::List(b, _) => b,
            _ => {
                return Err(EvalError::Type(format!(
                    "let: expected bindings list at {}",
                    p
                )))
            }
        };
        let mut param_names = Vec::new();
        let mut init_exprs = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    match &pair[0] {
                        Expr::Symbol(s, _) => param_names.push(s.clone()),
                        _ => {
                            return Err(EvalError::Type(format!(
                                "let: expected symbol in binding at {}",
                                p
                            )))
                        }
                    }
                    init_exprs.push(pair[1].clone());
                }
                _ => {
                    return Err(EvalError::Type(format!(
                        "let: invalid binding at {}",
                        p
                    )))
                }
            }
        }
        let body = args[2..].to_vec();
        let loop_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: param_names,
            rest_param: None,
            body,
            env: loop_env.clone(),
        };
        env_set(&loop_env, name.clone(), lambda);
        let init_vals: Vec<Value> = init_exprs
            .iter()
            .map(|e| eval(e, env))
            .collect::<Result<_, _>>()?;
        let func = env_get(&loop_env, name).unwrap();
        return apply_value(&func, &init_vals, p);
    }
    // Regular let
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => {
            return Err(EvalError::Type(format!(
                "let: expected bindings list at {}",
                p
            )))
        }
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "let: expected symbol in binding at {}",
                            p
                        )))
                    }
                };
                let val = eval(&pair[1], env)?;
                env_set(&local_env, name, val);
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "let: invalid binding at {}",
                    p
                )))
            }
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env)?;
    }
    Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env })
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &args[..args.len() - 1] {
        eval(expr, env)?;
    }
    Ok(Value::TailCall { expr: Box::new(args.last().unwrap().clone()), env: env.clone() })
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        let body = &parts[1..];
                        if body.is_empty() {
                            return Ok(Value::Boolean(false));
                        }
                        for expr in &body[..body.len() - 1] {
                            eval(expr, env)?;
                        }
                        return Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: env.clone() });
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let body = &parts[1..];
                    if body.is_empty() {
                        return Ok(test);
                    }
                    for expr in &body[..body.len() - 1] {
                        eval(expr, env)?;
                    }
                    return Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: env.clone() });
                }
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "cond: invalid clause at {}",
                    clause.pos()
                )))
            }
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_string_set(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!(
            "string-set! requires 3 arguments at {}", p
        )));
    }
    if matches!(&args[0], Expr::Str(_, _)) {
        return Err(EvalError::Type(format!(
            "string-set!: strings are immutable at {}", p
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected string variable at {}", p
        ))),
    };
    let val = env_get(env, &name).ok_or_else(|| {
        EvalError::UnboundVariable(format!("{} at {}", name, p))
    })?;
    let mut s = match val {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected string at {}", p
        ))),
    };
    let idx = match eval(&args[1], env)? {
        Value::Integer(i) => i as usize,
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected integer index at {}", p
        ))),
    };
    let ch = match eval(&args[2], env)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(format!(
            "string-set!: expected char at {}", p
        ))),
    };
    if idx >= s.len() {
        return Err(EvalError::Type(format!(
            "string-set!: index out of range at {}", p
        )));
    }
    let bytes = unsafe { s.as_bytes_mut() };
    bytes[idx] = ch as u8;
    env_set_existing(env, &name, Value::Str(s));
    Ok(Value::Nil)
}

fn eval_set(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "set! requires 2 arguments at {}", p
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "set!: expected symbol at {}", p
        ))),
    };
    if env_get(env, &name).is_none() {
        return Err(EvalError::UnboundVariable(format!("{} at {}", name, p)));
    }
    let val = eval(&args[1], env)?;
    env_set_existing(env, &name, val);
    Ok(Value::Nil)
}

// --- Records (define-record-type) ---

fn eval_define_record_type(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Arity(format!(
            "define-record-type requires at least 3 arguments at {}", p
        )));
    }

    let _type_name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-record-type: expected type name symbol at {}", p
        ))),
    };

    let (ctor_name, ctor_fields) = match &args[1] {
        Expr::List(elems, _) if !elems.is_empty() => {
            let name = match &elems[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => return Err(EvalError::Type(format!(
                    "define-record-type: expected constructor name at {}", p
                ))),
            };
            let fields: Result<Vec<String>, EvalError> = elems[1..].iter().map(|e| match e {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!(
                    "define-record-type: expected field name at {}", p
                ))),
            }).collect();
            (name, fields?)
        }
        _ => return Err(EvalError::Type(format!(
            "define-record-type: expected constructor spec at {}", p
        ))),
    };

    let pred_name = match &args[2] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-record-type: expected predicate name at {}", p
        ))),
    };

    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    for field_spec in &args[3..] {
        match field_spec {
            Expr::List(elems, _) if elems.len() >= 2 => {
                let field_name = match &elems[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!(
                        "define-record-type: expected field name at {}", p
                    ))),
                };
                let accessor_name = match &elems[1] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!(
                        "define-record-type: expected accessor name at {}", p
                    ))),
                };
                env_set(env, accessor_name, Value::RecordAccessor {
                    type_id,
                    field_name,
                });
            }
            _ => return Err(EvalError::Type(format!(
                "define-record-type: expected field spec at {}", p
            ))),
        }
    }

    env_set(env, ctor_name, Value::RecordConstructor {
        type_id,
        type_name: _type_name.clone(),
        field_names: ctor_fields,
    });

    env_set(env, pred_name, Value::RecordPredicate { type_id });

    Ok(Value::Nil)
}

// --- Macros (syntax-rules) ---

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define" | "if" | "quote" | "lambda" | "case-lambda" | "and" | "or" | "let" | "begin"
            | "cond" | "string-set!" | "set!" | "define-syntax" | "define-record-type"
            | "letrec" | "letrec*" | "case" | "do" | "let*" | "when"
    )
}

fn eval_define_syntax(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "define-syntax requires 2 arguments at {}", p
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected symbol at {}", p
        ))),
    };
    let sr = match &args[1] {
        Expr::List(elems, _) => elems,
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected syntax-rules at {}", p
        ))),
    };
    if sr.len() < 2 {
        return Err(EvalError::Parse(format!(
            "define-syntax: invalid syntax-rules at {}", p
        )));
    }
    match &sr[0] {
        Expr::Symbol(s, _) if s == "syntax-rules" => {}
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected syntax-rules at {}", p
        ))),
    }
    let literals = match &sr[1] {
        Expr::List(lits, _) => lits
            .iter()
            .map(|l| match l {
                Expr::Symbol(s, _) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!(
                    "define-syntax: expected symbol in literals at {}", p
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => return Err(EvalError::Type(format!(
            "define-syntax: expected literals list at {}", p
        ))),
    };
    let mut rules = Vec::new();
    for rule in &sr[2..] {
        match rule {
            Expr::List(parts, _) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => return Err(EvalError::Type(format!(
                "define-syntax: invalid rule at {}", p
            ))),
        }
    }
    let mac = Value::Macro {
        literals,
        rules,
        def_env: env.clone(),
    };
    env_set(env, name.clone(), mac);
    Ok(Value::Symbol(name))
}

#[derive(Debug, Clone)]
enum PatBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn match_one(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name, _) if name == "_" => true,
        Expr::Symbol(name, _) if name == "..." => false,
        Expr::Symbol(name, _) if literals.contains(name) => {
            matches!(input, Expr::Symbol(s, _) if s == name)
        }
        Expr::Symbol(name, _) => {
            bindings.insert(name.clone(), PatBinding::Single(input.clone()));
            true
        }
        Expr::List(pelems, _) => match input {
            Expr::List(ielems, _) => match_list(pelems, ielems, literals, bindings),
            _ => false,
        },
        Expr::Integer(n, _) => matches!(input, Expr::Integer(m, _) if m == n),
        Expr::Float(f, _) => matches!(input, Expr::Float(g, _) if (g - f).abs() < f64::EPSILON),
        Expr::Rational(n, d, _) => matches!(input, Expr::Rational(n2, d2, _) if n2 == n && d2 == d),
        Expr::Boolean(b, _) => matches!(input, Expr::Boolean(c, _) if c == b),
        Expr::Str(s, _) => matches!(input, Expr::Str(t, _) if t == s),
        Expr::Char(c, _) => matches!(input, Expr::Char(d, _) if d == c),
    }
}

fn match_list(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        if pi + 1 < pattern.len() {
            if let Expr::Symbol(s, _) = &pattern[pi + 1] {
                if s == "..." {
                    let sub_pattern = &pattern[pi];
                    let var_names = collect_pattern_vars(sub_pattern, literals);
                    let remaining_pat = pattern.len() - pi - 2;
                    let max_match = if input.len() >= ii + remaining_pat {
                        input.len() - ii - remaining_pat
                    } else {
                        return false;
                    };
                    let mut list_bindings: HashMap<String, Vec<Expr>> = HashMap::new();
                    for v in &var_names {
                        list_bindings.insert(v.clone(), Vec::new());
                    }
                    for j in 0..max_match {
                        let mut sub_bindings = HashMap::new();
                        if !match_one(sub_pattern, &input[ii + j], literals, &mut sub_bindings) {
                            return false;
                        }
                        for v in &var_names {
                            if let Some(PatBinding::Single(e)) = sub_bindings.get(v) {
                                list_bindings.get_mut(v).unwrap().push(e.clone());
                            }
                        }
                    }
                    for (v, exprs) in list_bindings {
                        bindings.insert(v, PatBinding::List(exprs));
                    }
                    ii += max_match;
                    pi += 2;
                    continue;
                }
            }
        }
        if ii >= input.len() {
            return false;
        }
        if !match_one(&pattern[pi], &input[ii], literals, bindings) {
            return false;
        }
        pi += 1;
        ii += 1;
    }
    pi == pattern.len() && ii == input.len()
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String]) -> Vec<String> {
    let mut vars = Vec::new();
    collect_pvars_inner(pattern, literals, &mut vars);
    vars
}

fn collect_pvars_inner(pattern: &Expr, literals: &[String], vars: &mut Vec<String>) {
    match pattern {
        Expr::Symbol(name, _)
            if name != "..." && name != "_" && !literals.contains(name) =>
        {
            vars.push(name.clone());
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_pvars_inner(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    gensym_map: &HashMap<String, String>,
) -> Expr {
    match template {
        Expr::Symbol(name, pos) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatBinding::Single(e) => e.clone(),
                    PatBinding::List(_) => template.clone(),
                }
            } else if let Some(gname) = gensym_map.get(name) {
                Expr::Symbol(gname.clone(), *pos)
            } else {
                template.clone()
            }
        }
        Expr::List(elems, pos) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() {
                    if let Expr::Symbol(s, _) = &elems[i + 1] {
                        if s == "..." {
                            let sub = &elems[i];
                            let evars = find_ellipsis_vars(sub, bindings);
                            if let Some(first_var) = evars.first() {
                                if let Some(PatBinding::List(items)) = bindings.get(first_var) {
                                    let count = items.len();
                                    for j in 0..count {
                                        let mut sub_bindings = bindings.clone();
                                        for v in &evars {
                                            if let Some(PatBinding::List(vitems)) = bindings.get(v)
                                            {
                                                sub_bindings.insert(
                                                    v.clone(),
                                                    PatBinding::Single(vitems[j].clone()),
                                                );
                                            }
                                        }
                                        result.push(expand_template(sub, &sub_bindings, gensym_map));
                                    }
                                }
                            }
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(expand_template(&elems[i], bindings, gensym_map));
                i += 1;
            }
            Expr::List(result, *pos)
        }
        _ => template.clone(),
    }
}

fn find_ellipsis_vars(template: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    find_evars_inner(template, bindings, &mut vars);
    vars
}

fn find_evars_inner(
    template: &Expr,
    bindings: &HashMap<String, PatBinding>,
    vars: &mut Vec<String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(PatBinding::List(_))) && !vars.contains(name) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                find_evars_inner(e, bindings, vars);
            }
        }
        _ => {}
    }
}

fn collect_template_symbols(
    template: &Expr,
    pat_vars: &[String],
    gensym_map: &mut HashMap<String, String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if !pat_vars.contains(name)
                && !is_special_form(name)
                && !is_builtin(name)
                && name != "..."
                && !gensym_map.contains_key(name)
            {
                gensym_map.insert(name.clone(), gensym(name));
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_template_symbols(e, pat_vars, gensym_map);
            }
        }
        _ => {}
    }
}

fn eval_macro(
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    input: &[Expr],
    env: &Env,
    p: Pos,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if let Expr::List(pat_elems, _) = pattern {
            if match_list(&pat_elems[1..], &input[1..], literals, &mut bindings) {
                let pat_vars: Vec<String> = bindings.keys().cloned().collect();
                let mut gensym_map = HashMap::new();
                collect_template_symbols(template, &pat_vars, &mut gensym_map);

                let expanded = expand_template(template, &bindings, &gensym_map);

                let wrapper_env = new_env(Some(env.clone()));
                for (orig, gsym) in &gensym_map {
                    if let Some(val) = env_get(def_env, orig) {
                        env_set(&wrapper_env, gsym.clone(), val);
                    }
                }

                return eval(&expanded, &wrapper_env);
            }
        }
    }
    Err(EvalError::Type(format!(
        "no matching macro pattern at {}", p
    )))
}

fn eval_letrec(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("letrec requires bindings and body at {}", p)));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("letrec: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("letrec: expected symbol at {}", p))),
                };
                env_set(&local_env, name.clone(), Value::Boolean(false));
                names.push(name);
                init_exprs.push(&pair[1]);
            }
            _ => return Err(EvalError::Type(format!("letrec: invalid binding at {}", p))),
        }
    }
    for (name, init) in names.iter().zip(init_exprs.iter()) {
        let val = eval(init, &local_env)?;
        env_set(&local_env, name.clone(), val);
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env)?;
    }
    Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env })
}

fn eval_letrec_star(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("letrec* requires bindings and body at {}", p)));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("letrec*: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("letrec*: expected symbol at {}", p))),
                };
                let val = eval(&pair[1], &local_env)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Type(format!("letrec*: invalid binding at {}", p))),
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env)?;
    }
    Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env })
}

fn eval_let_star(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("let* requires bindings and body at {}", p)));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("let*: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("let*: expected symbol at {}", p))),
                };
                let val = eval(&pair[1], &local_env)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Type(format!("let*: invalid binding at {}", p))),
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env)?;
    }
    Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: local_env })
}

fn eval_when(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("when requires test and body at {}", p)));
    }
    let test = eval(&args[0], env)?;
    if test.is_truthy() {
        let body = &args[1..];
        if body.is_empty() {
            return Ok(Value::Boolean(false));
        }
        for expr in &body[..body.len() - 1] {
            eval(expr, env)?;
        }
        Ok(Value::TailCall { expr: Box::new(body.last().unwrap().clone()), env: env.clone() })
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eqv_values(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn eval_case(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("case requires key and clauses at {}", p)));
    }
    let key = eval(&args[0], env)?;
    for clause in &args[1..] {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                if let Expr::List(datums, _) = &parts[0] {
                    for datum in datums {
                        let dval = expr_to_value(datum);
                        if eqv_values(&key, &dval) {
                            let mut result = Value::Boolean(false);
                            for expr in &parts[1..] {
                                result = eval(expr, env)?;
                            }
                            return Ok(result);
                        }
                    }
                }
            }
            _ => return Err(EvalError::Type(format!("case: invalid clause at {}", p))),
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_do(args: &[Expr], env: &Env, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("do requires vars and test at {}", p)));
    }
    let var_specs = match &args[0] {
        Expr::List(v, _) => v,
        _ => return Err(EvalError::Type(format!("do: expected variable list at {}", p))),
    };
    let test_clause = match &args[1] {
        Expr::List(t, _) => t,
        _ => return Err(EvalError::Type(format!("do: expected test clause at {}", p))),
    };
    if test_clause.is_empty() {
        return Err(EvalError::Type(format!("do: empty test clause at {}", p)));
    }

    struct DoVar {
        name: String,
        step: Option<Expr>,
    }
    let mut var_defs = Vec::new();
    let mut init_vals = Vec::new();
    for spec in var_specs {
        match spec {
            Expr::List(parts, _) if parts.len() >= 2 => {
                let name = match &parts[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("do: expected symbol at {}", p))),
                };
                let init = eval(&parts[1], env)?;
                let step = if parts.len() >= 3 { Some(parts[2].clone()) } else { None };
                init_vals.push(init);
                var_defs.push(DoVar { name, step });
            }
            _ => return Err(EvalError::Type(format!("do: invalid var spec at {}", p))),
        }
    }

    let body = &args[2..];

    let loop_env = new_env(Some(env.clone()));
    for (def, val) in var_defs.iter().zip(init_vals.into_iter()) {
        env_set(&loop_env, def.name.clone(), val);
    }

    loop {
        let test_result = eval(&test_clause[0], &loop_env)?;
        if test_result.is_truthy() {
            if test_clause.len() > 1 {
                let mut result = Value::Boolean(false);
                for expr in &test_clause[1..] {
                    result = eval(expr, &loop_env)?;
                }
                return Ok(result);
            }
            return Ok(test_result);
        }
        for expr in body {
            eval(expr, &loop_env)?;
        }
        let new_vals: Vec<Option<Result<Value, EvalError>>> = var_defs.iter().map(|def| {
            if let Some(ref step) = def.step {
                Some(eval(step, &loop_env))
            } else {
                None
            }
        }).collect();
        for (def, new_val) in var_defs.iter().zip(new_vals.into_iter()) {
            if let Some(val_result) = new_val {
                env_set(&loop_env, def.name.clone(), val_result?);
            }
        }
    }
}

// Helper: extract car/cdr from a pair value, cloning them out
fn pair_car(v: &Value, p: Pos) -> Result<Value, EvalError> {
    match v {
        Value::Pair(pr) => Ok(pr.borrow().0.clone()),
        _ => Err(EvalError::Type(format!("car: expected pair, got {} at {}", v, p))),
    }
}

fn pair_cdr(v: &Value, p: Pos) -> Result<Value, EvalError> {
    match v {
        Value::Pair(pr) => Ok(pr.borrow().1.clone()),
        _ => Err(EvalError::Type(format!("cdr: expected pair, got {} at {}", v, p))),
    }
}

fn apply_builtin(op: &str, args: &[Value], p: Pos) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut acc = NumVal::Int(0);
            for a in args {
                let n = to_num(a, p)?;
                acc = match num_add(acc, n) {
                    Value::Integer(i) => NumVal::Int(i),
                    Value::Rational(n, d) => NumVal::Rat(n, d),
                    Value::Float(f) => NumVal::Flt(f),
                    _ => unreachable!(),
                };
            }
            Ok(num_to_value(acc))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("- requires at least 1 argument at {}", p)));
            }
            let first = to_num(&args[0], p)?;
            if args.len() == 1 {
                return Ok(match first {
                    NumVal::Int(i) => Value::Integer(-i),
                    NumVal::Rat(n, d) => Value::Rational(-n, d),
                    NumVal::Flt(f) => Value::Float(-f),
                });
            }
            let mut acc = first;
            for a in &args[1..] {
                let n = to_num(a, p)?;
                acc = match num_sub(acc, n) {
                    Value::Integer(i) => NumVal::Int(i),
                    Value::Rational(n, d) => NumVal::Rat(n, d),
                    Value::Float(f) => NumVal::Flt(f),
                    _ => unreachable!(),
                };
            }
            Ok(num_to_value(acc))
        }
        "*" => {
            let mut acc = NumVal::Int(1);
            for a in args {
                let n = to_num(a, p)?;
                acc = match num_mul(acc, n) {
                    Value::Integer(i) => NumVal::Int(i),
                    Value::Rational(n, d) => NumVal::Rat(n, d),
                    Value::Float(f) => NumVal::Flt(f),
                    _ => unreachable!(),
                };
            }
            Ok(num_to_value(acc))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("/ requires at least 1 argument at {}", p)));
            }
            if args.len() == 1 {
                let d = to_num(&args[0], p)?;
                return num_div(NumVal::Int(1), d, p);
            }
            let mut acc = to_num(&args[0], p)?;
            for a in &args[1..] {
                let n = to_num(a, p)?;
                acc = match num_div(acc, n, p)? {
                    Value::Integer(i) => NumVal::Int(i),
                    Value::Rational(n, d) => NumVal::Rat(n, d),
                    Value::Float(f) => NumVal::Flt(f),
                    _ => unreachable!(),
                };
            }
            Ok(num_to_value(acc))
        }
        "<" => {
            ensure_args(op, args, 2, p)?;
            let a = num_to_f64(&to_num(&args[0], p)?);
            let b = num_to_f64(&to_num(&args[1], p)?);
            Ok(Value::Boolean(a < b))
        }
        ">" => {
            ensure_args(op, args, 2, p)?;
            let a = num_to_f64(&to_num(&args[0], p)?);
            let b = num_to_f64(&to_num(&args[1], p)?);
            Ok(Value::Boolean(a > b))
        }
        "=" => {
            ensure_args(op, args, 2, p)?;
            let a = num_to_f64(&to_num(&args[0], p)?);
            let b = num_to_f64(&to_num(&args[1], p)?);
            Ok(Value::Boolean((a - b).abs() < f64::EPSILON))
        }
        "<=" => {
            ensure_args(op, args, 2, p)?;
            let a = num_to_f64(&to_num(&args[0], p)?);
            let b = num_to_f64(&to_num(&args[1], p)?);
            Ok(Value::Boolean(a <= b))
        }
        ">=" => {
            ensure_args(op, args, 2, p)?;
            let a = num_to_f64(&to_num(&args[0], p)?);
            let b = num_to_f64(&to_num(&args[1], p)?);
            Ok(Value::Boolean(a >= b))
        }
        "not" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            ensure_args(op, args, 2, p)?;
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&args[0], p)
        }
        "cdr" => {
            ensure_args(op, args, 1, p)?;
            pair_cdr(&args[0], p)
        }
        "null?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Nil)))
        }
        "list" => Ok(vec_to_list(args.to_vec())),
        "length" => {
            ensure_args(op, args, 1, p)?;
            let mut count = 0i64;
            let mut cur = args[0].clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(pr) => {
                        count += 1;
                        cur = pr.borrow().1.clone();
                    }
                    _ => {
                        return Err(EvalError::Type(format!(
                            "length: expected list at {}",
                            p
                        )))
                    }
                }
            }
            Ok(Value::Integer(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::Nil);
            }
            let mut result = args.last().unwrap().clone();
            for arg in args[..args.len() - 1].iter().rev() {
                let mut elems = Vec::new();
                let mut cur = arg.clone();
                loop {
                    match cur {
                        Value::Nil => break,
                        Value::Pair(pr) => {
                            let p = pr.borrow();
                            let car = p.0.clone();
                            let cdr = p.1.clone();
                            drop(p);
                            elems.push(car);
                            cur = cdr;
                        }
                        _ => {
                            return Err(EvalError::Type(format!(
                                "append: expected list at {}",
                                p
                            )))
                        }
                    }
                }
                for e in elems.into_iter().rev() {
                    result = make_pair(e, result);
                }
            }
            Ok(result)
        }
        "number?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(is_numeric_value(&args[0])))
        }
        "string?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
        }
        "boolean?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
        }
        "pair?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Pair(_))))
        }
        "symbol?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
        }
        "char?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
        }
        "display" => {
            ensure_args(op, args, 1, p)?;
            write_output(&display_value(&args[0]));
            Ok(Value::Nil)
        }
        "write" => {
            ensure_args(op, args, 1, p)?;
            write_output(&args[0].to_string());
            Ok(Value::Nil)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "newline expects 0 arguments, got {} at {}",
                    args.len(), p
                )));
            }
            write_output("\n");
            Ok(Value::Nil)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                result.push_str(a.as_string_at(p)?);
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Integer(s.len() as i64))
        }
        "substring" => {
            ensure_args(op, args, 3, p)?;
            let s = args[0].as_string_at(p)?;
            let start = args[1].as_integer_at(p)? as usize;
            let end = args[2].as_integer_at(p)? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Type(format!(
                    "substring: index out of range at {}", p
                )));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            if let Ok(n) = s.parse::<i64>() {
                Ok(Value::Integer(n))
            } else if let Ok(f) = s.parse::<f64>() {
                Ok(Value::Float(f))
            } else {
                Ok(Value::Boolean(false))
            }
        }
        "number->string" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Str(format!("{}", args[0])))
        }
        "symbol->string" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!(
                    "symbol->string: expected symbol, got {} at {}", args[0], p
                ))),
            }
        }
        "string->symbol" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Symbol(s.to_string()))
        }
        "string-ref" => {
            ensure_args(op, args, 2, p)?;
            let s = args[0].as_string_at(p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            if idx >= s.len() {
                return Err(EvalError::Type(format!(
                    "string-ref: index out of range at {}", p
                )));
            }
            Ok(Value::Char(s.chars().nth(idx).unwrap()))
        }
        "string-copy" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            Ok(Value::Str(s.to_string()))
        }
        "string->list" => {
            ensure_args(op, args, 1, p)?;
            let s = args[0].as_string_at(p)?;
            let list = s.chars().rev().fold(Value::Nil, |acc, c| {
                make_pair(Value::Char(c), acc)
            });
            Ok(list)
        }
        "list->string" => {
            ensure_args(op, args, 1, p)?;
            let mut chars = Vec::new();
            let mut cur = args[0].clone();
            loop {
                match cur {
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        match &pair.0 {
                            Value::Char(c) => chars.push(*c),
                            other => return Err(EvalError::Type(format!(
                                "list->string: expected char, got {} at {}", other, p
                            ))),
                        }
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    Value::Nil => break,
                    _ => return Err(EvalError::Type(format!(
                        "list->string: expected proper list at {}", p
                    ))),
                }
            }
            Ok(Value::Str(chars.into_iter().collect()))
        }
        "char->integer" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                other => Err(EvalError::Type(format!(
                    "char->integer: expected char, got {} at {}", other, p
                ))),
            }
        }
        "integer->char" => {
            ensure_args(op, args, 1, p)?;
            let n = args[0].as_integer_at(p)?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\u{FFFD}')))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!(
                    "apply requires at least 2 arguments at {}", p
                )));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            let mut cur = last.clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        call_args.push(pair.0.clone());
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!(
                        "apply: last argument must be a list at {}", p
                    ))),
                }
            }
            apply_value(func, &call_args, p)
        }
        "abs" => {
            ensure_args(op, args, 1, p)?;
            match to_num(&args[0], p)? {
                NumVal::Int(i) => Ok(Value::Integer(i.abs())),
                NumVal::Rat(n, d) => Ok(make_rational(n.abs(), d)),
                NumVal::Flt(f) => Ok(Value::Float(f.abs())),
            }
        }
        "modulo" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            ensure_args(op, args, 2, p)?;
            let a = args[0].as_integer_at(p)?;
            let b = args[1].as_integer_at(p)?;
            if b == 0 { return Err(EvalError::DivisionByZero(format!("{}", p))); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("min requires at least 1 argument at {}", p)));
            }
            let mut m = num_to_f64(&to_num(&args[0], p)?);
            let mut mi = 0;
            for (i, a) in args[1..].iter().enumerate() {
                let f = num_to_f64(&to_num(a, p)?);
                if f < m { m = f; mi = i + 1; }
            }
            Ok(args[mi].clone())
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("max requires at least 1 argument at {}", p)));
            }
            let mut m = num_to_f64(&to_num(&args[0], p)?);
            let mut mi = 0;
            for (i, a) in args[1..].iter().enumerate() {
                let f = num_to_f64(&to_num(a, p)?);
                if f > m { m = f; mi = i + 1; }
            }
            Ok(args[mi].clone())
        }
        "expt" => {
            ensure_args(op, args, 2, p)?;
            let base = args[0].as_integer_at(p)?;
            let exp = args[1].as_integer_at(p)?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            ensure_args(op, args, 1, p)?;
            let f = num_to_f64(&to_num(&args[0], p)?);
            Ok(Value::Boolean(f == 0.0))
        }
        "positive?" => {
            ensure_args(op, args, 1, p)?;
            let f = num_to_f64(&to_num(&args[0], p)?);
            Ok(Value::Boolean(f > 0.0))
        }
        "negative?" => {
            ensure_args(op, args, 1, p)?;
            let f = num_to_f64(&to_num(&args[0], p)?);
            Ok(Value::Boolean(f < 0.0))
        }
        "odd?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? % 2 != 0))
        }
        "even?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(args[0].as_integer_at(p)? % 2 == 0))
        }
        "list-ref" => {
            ensure_args(op, args, 2, p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match cur {
                    Value::Pair(pr) => cur = pr.borrow().1.clone(),
                    _ => return Err(EvalError::Type(format!("list-ref: index out of range at {}", p))),
                }
            }
            match cur {
                Value::Pair(pr) => Ok(pr.borrow().0.clone()),
                _ => Err(EvalError::Type(format!("list-ref: index out of range at {}", p))),
            }
        }
        "list-tail" => {
            ensure_args(op, args, 2, p)?;
            let idx = args[1].as_integer_at(p)? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match cur {
                    Value::Pair(pr) => cur = pr.borrow().1.clone(),
                    _ => return Err(EvalError::Type(format!("list-tail: index out of range at {}", p))),
                }
            }
            Ok(cur)
        }
        "list?" => {
            ensure_args(op, args, 1, p)?;
            // Floyd's tortoise-and-hare cycle detection
            let mut slow = args[0].clone();
            let mut fast = args[0].clone();
            let result = loop {
                // Advance fast by 2
                for _ in 0..2 {
                    match fast {
                        Value::Nil => { fast = Value::Nil; break; }
                        Value::Pair(pr) => fast = pr.borrow().1.clone(),
                        _ => { return Ok(Value::Boolean(false)); }
                    }
                }
                if matches!(fast, Value::Nil) {
                    break true;
                }
                // Advance slow by 1
                match slow {
                    Value::Pair(pr) => slow = pr.borrow().1.clone(),
                    _ => { break false; }
                }
                // Check if they point to the same pair (cycle)
                if let (Value::Pair(ref s), Value::Pair(ref f)) = (&slow, &fast) {
                    if Rc::ptr_eq(s, f) {
                        break false; // cycle detected
                    }
                }
            };
            Ok(Value::Boolean(result))
        }
        "assoc" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Value::Pair(ref inner) = car {
                            let k = inner.borrow().0.clone();
                            if values_equal(&k, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("assoc: expected list at {}", p))),
                }
            }
        }
        "eq?" => {
            ensure_args(op, args, 2, p)?;
            let result = match (&args[0], &args[1]) {
                (Value::Symbol(a), Value::Symbol(b)) => a == b,
                (Value::Integer(a), Value::Integer(b)) => a == b,
                (Value::Float(a), Value::Float(b)) => a == b,
                (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
                (Value::Boolean(a), Value::Boolean(b)) => a == b,
                (Value::Char(a), Value::Char(b)) => a == b,
                (Value::Nil, Value::Nil) => true,
                (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
                (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "equal?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("map requires at least 2 arguments at {}", p)));
            }
            let func = &args[0];
            let mut current_lists: Vec<Value> = args[1..].to_vec();
            let mut results = Vec::new();
            loop {
                let all_pairs = current_lists.iter().all(|l| matches!(l, Value::Pair(_)));
                if !all_pairs { break; }
                let mut call_args = Vec::new();
                let mut next_lists = Vec::new();
                for list in &current_lists {
                    match list {
                        Value::Pair(pr) => {
                            let pair = pr.borrow();
                            call_args.push(pair.0.clone());
                            next_lists.push(pair.1.clone());
                        }
                        _ => unreachable!(),
                    }
                }
                results.push(force(apply_value(func, &call_args, p)?)?);
                current_lists = next_lists;
            }
            Ok(vec_to_list(results))
        }
        "char-alphabetic?" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!("char-alphabetic?: expected char at {}", p))),
            }
        }
        "char-numeric?" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!("char-numeric?: expected char at {}", p))),
            }
        }
        "char-upcase" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(format!("char-upcase: expected char at {}", p))),
            }
        }
        "char-downcase" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(format!("char-downcase: expected char at {}", p))),
            }
        }
        "char=?" => {
            ensure_args(op, args, 2, p)?;
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("char=?: expected chars at {}", p))),
            }
        }
        "char<?" => {
            ensure_args(op, args, 2, p)?;
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("char<?: expected chars at {}", p))),
            }
        }
        "string=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? == args[1].as_string_at(p)?))
        }
        "string<?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? < args[1].as_string_at(p)?))
        }
        "string-ci=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(
                args[0].as_string_at(p)?.to_lowercase() == args[1].as_string_at(p)?.to_lowercase()
            ))
        }
        "string-upcase" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Str(args[0].as_string_at(p)?.to_uppercase()))
        }
        "string-downcase" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Str(args[0].as_string_at(p)?.to_lowercase()))
        }
        "exact?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "inexact?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
        }
        "rational?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
        }
        "integer?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
        }
        "exact->inexact" => {
            ensure_args(op, args, 1, p)?;
            let n = to_num(&args[0], p)?;
            Ok(Value::Float(num_to_f64(&n)))
        }
        "inexact->exact" => {
            ensure_args(op, args, 1, p)?;
            match to_num(&args[0], p)? {
                NumVal::Int(i) => Ok(Value::Integer(i)),
                NumVal::Rat(n, d) => Ok(make_rational(n, d)),
                NumVal::Flt(f) => {
                    let (n, d) = float_to_rational(f);
                    Ok(make_rational(n, d))
                }
            }
        }
        "numerator" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type(format!("numerator: expected rational at {}", p))),
            }
        }
        "denominator" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type(format!("denominator: expected rational at {}", p))),
            }
        }
        "procedure?" => {
            ensure_args(op, args, 1, p)?;
            let result = match &args[0] {
                Value::Lambda { .. }
                | Value::CaseLambda { .. }
                | Value::RecordConstructor { .. }
                | Value::RecordPredicate { .. }
                | Value::RecordAccessor { .. }
                | Value::Continuation { .. } => true,
                Value::Symbol(s) => is_builtin(s),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "eqv?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(eqv_values(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!("make-vector expects 1-2 arguments at {}", p)));
            }
            let len = args[0].as_integer_at(p)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            ensure_args(op, args, 2, p)?;
            match &args[0] {
                Value::Vector(v) => {
                    let idx = args[1].as_integer_at(p)? as usize;
                    let elems = v.borrow();
                    if idx >= elems.len() {
                        return Err(EvalError::Type(format!("vector-ref: index out of range at {}", p)));
                    }
                    Ok(elems[idx].clone())
                }
                _ => Err(EvalError::Type(format!("vector-ref: expected vector at {}", p))),
            }
        }
        "vector-set!" => {
            ensure_args(op, args, 3, p)?;
            match &args[0] {
                Value::Vector(v) => {
                    let idx = args[1].as_integer_at(p)? as usize;
                    let mut elems = v.borrow_mut();
                    if idx >= elems.len() {
                        return Err(EvalError::Type(format!("vector-set!: index out of range at {}", p)));
                    }
                    elems[idx] = args[2].clone();
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::Type(format!("vector-set!: expected vector at {}", p))),
            }
        }
        "vector-length" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type(format!("vector-length: expected vector at {}", p))),
            }
        }
        "vector?" => {
            ensure_args(op, args, 1, p)?;
            Ok(Value::Boolean(matches!(args[0], Value::Vector(_))))
        }
        "vector->list" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Vector(v) => Ok(vec_to_list(v.borrow().clone())),
                _ => Err(EvalError::Type(format!("vector->list: expected vector at {}", p))),
            }
        }
        "list->vector" => {
            ensure_args(op, args, 1, p)?;
            let mut elems = Vec::new();
            let mut cur = args[0].clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        elems.push(pair.0.clone());
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!("list->vector: expected list at {}", p))),
                }
            }
            Ok(Value::Vector(Rc::new(RefCell::new(elems))))
        }
        "assq" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Value::Pair(ref inner) = car {
                            let k = inner.borrow().0.clone();
                            if eqv_values(&k, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("assq: expected list at {}", p))),
                }
            }
        }
        "memq" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(ref pr) => {
                        let car = pr.borrow().0.clone();
                        if eqv_values(&car, key) {
                            return Ok(cur);
                        }
                        let next = pr.borrow().1.clone();
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!("memq: expected list at {}", p))),
                }
            }
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("for-each requires at least 2 arguments at {}", p)));
            }
            let func = &args[0];
            let mut current_lists: Vec<Value> = args[1..].to_vec();
            loop {
                let all_pairs = current_lists.iter().all(|l| matches!(l, Value::Pair(_)));
                if !all_pairs { break; }
                let mut call_args = Vec::new();
                let mut next_lists = Vec::new();
                for list in &current_lists {
                    match list {
                        Value::Pair(pr) => {
                            let pair = pr.borrow();
                            call_args.push(pair.0.clone());
                            next_lists.push(pair.1.clone());
                        }
                        _ => unreachable!(),
                    }
                }
                force(apply_value(func, &call_args, p)?)?;
                current_lists = next_lists;
            }
            Ok(Value::Nil)
        }
        "set-car!" => {
            ensure_args(op, args, 2, p)?;
            match &args[0] {
                Value::Pair(pr) => {
                    pr.borrow_mut().0 = args[1].clone();
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::Type(format!("set-car!: expected pair, got {} at {}", args[0], p))),
            }
        }
        "set-cdr!" => {
            ensure_args(op, args, 2, p)?;
            match &args[0] {
                Value::Pair(pr) => {
                    pr.borrow_mut().1 = args[1].clone();
                    Ok(Value::Nil)
                }
                _ => Err(EvalError::Type(format!("set-cdr!: expected pair, got {} at {}", args[0], p))),
            }
        }
        "caar" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&pair_car(&args[0], p)?, p)
        }
        "cadr" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&pair_cdr(&args[0], p)?, p)
        }
        "cdar" => {
            ensure_args(op, args, 1, p)?;
            pair_cdr(&pair_car(&args[0], p)?, p)
        }
        "cddr" => {
            ensure_args(op, args, 1, p)?;
            pair_cdr(&pair_cdr(&args[0], p)?, p)
        }
        "caddr" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&pair_cdr(&pair_cdr(&args[0], p)?, p)?, p)
        }
        "cdddr" => {
            ensure_args(op, args, 1, p)?;
            pair_cdr(&pair_cdr(&pair_cdr(&args[0], p)?, p)?, p)
        }
        "cdadr" => {
            ensure_args(op, args, 1, p)?;
            pair_cdr(&pair_car(&pair_cdr(&args[0], p)?, p)?, p)
        }
        "cadar" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&pair_cdr(&pair_car(&args[0], p)?, p)?, p)
        }
        "caddar" => {
            ensure_args(op, args, 1, p)?;
            pair_car(&pair_cdr(&pair_cdr(&pair_car(&args[0], p)?, p)?, p)?, p)
        }
        "reverse" => {
            ensure_args(op, args, 1, p)?;
            let mut result = Value::Nil;
            let mut cur = args[0].clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        result = make_pair(car, result);
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("reverse: expected list at {}", p))),
                }
            }
            Ok(result)
        }
        "member" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(ref pr) => {
                        let car = pr.borrow().0.clone();
                        if values_equal(&car, key) {
                            return Ok(cur);
                        }
                        let next = pr.borrow().1.clone();
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!("member: expected list at {}", p))),
                }
            }
        }
        "sort" => {
            ensure_args(op, args, 2, p)?;
            let cmp_func = &args[0];
            let mut elems = Vec::new();
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        elems.push(pair.0.clone());
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!("sort: expected list at {}", p))),
                }
            }
            // Simple insertion sort using the comparison function
            for i in 1..elems.len() {
                let mut j = i;
                while j > 0 {
                    let cmp_result = force(apply_value(cmp_func, &[elems[j].clone(), elems[j-1].clone()], p)?)?;
                    if cmp_result.is_truthy() {
                        elems.swap(j, j - 1);
                        j -= 1;
                    } else {
                        break;
                    }
                }
            }
            Ok(vec_to_list(elems))
        }
        "memv" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(ref pr) => {
                        let car = pr.borrow().0.clone();
                        if eqv_values(&car, key) {
                            return Ok(cur);
                        }
                        let next = pr.borrow().1.clone();
                        cur = next;
                    }
                    _ => return Err(EvalError::Type(format!("memv: expected list at {}", p))),
                }
            }
        }
        "assv" => {
            ensure_args(op, args, 2, p)?;
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match cur {
                    Value::Nil => return Ok(Value::Boolean(false)),
                    Value::Pair(pr) => {
                        let pair = pr.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Value::Pair(ref inner) = car {
                            let k = inner.borrow().0.clone();
                            if eqv_values(&k, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("assv: expected list at {}", p))),
                }
            }
        }
        "gcd" => {
            if args.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut result = args[0].as_integer_at(p)?.abs();
            for a in &args[1..] {
                result = gcd(result, a.as_integer_at(p)?.abs());
            }
            Ok(Value::Integer(result))
        }
        "lcm" => {
            if args.is_empty() {
                return Ok(Value::Integer(1));
            }
            let mut result = args[0].as_integer_at(p)?.abs();
            for a in &args[1..] {
                let b = a.as_integer_at(p)?.abs();
                if result == 0 && b == 0 {
                    result = 0;
                } else {
                    result = result / gcd(result, b) * b;
                }
            }
            Ok(Value::Integer(result))
        }
        "truncate" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                _ => Err(EvalError::Type(format!("truncate: expected number at {}", p))),
            }
        }
        "round" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                _ => Err(EvalError::Type(format!("round: expected number at {}", p))),
            }
        }
        "floor" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.floor() as i64)),
                _ => Err(EvalError::Type(format!("floor: expected number at {}", p))),
            }
        }
        "ceiling" => {
            ensure_args(op, args, 1, p)?;
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.ceil() as i64)),
                _ => Err(EvalError::Type(format!("ceiling: expected number at {}", p))),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!("make-string expects 1-2 arguments at {}", p)));
            }
            let len = args[0].as_integer_at(p)? as usize;
            let ch = if args.len() == 2 {
                match &args[1] {
                    Value::Char(c) => *c,
                    _ => return Err(EvalError::Type(format!("make-string: expected char at {}", p))),
                }
            } else {
                '\0'
            };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type(format!("string: expected char at {}", p))),
                }
            }
            Ok(Value::Str(s))
        }
        "string>?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? > args[1].as_string_at(p)?))
        }
        "string<=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? <= args[1].as_string_at(p)?))
        }
        "string>=?" => {
            ensure_args(op, args, 2, p)?;
            Ok(Value::Boolean(args[0].as_string_at(p)? >= args[1].as_string_at(p)?))
        }
        "error" => {
            if args.is_empty() {
                return Err(EvalError::Type("error".to_string()));
            }
            let msg = format!("{}", args[0]);
            Err(EvalError::Type(format!("error: {}", msg)))
        }
        "raise" => {
            ensure_args("raise", args, 1, p)?;
            Err(EvalError::RaisedValue(Box::new(args[0].clone())))
        }
        "with-exception-handler" => {
            ensure_args("with-exception-handler", args, 2, p)?;
            let handler = args[0].clone();
            let thunk = args[1].clone();
            match force(apply_value(&thunk, &[], p)?) {
                Ok(v) => Ok(v),
                Err(EvalError::RaisedValue(val)) => {
                    force(apply_value(&handler, &[*val], p)?)
                }
                Err(e) => Err(e),
            }
        }
        "values" => {
            if args.len() == 1 {
                // Single value is transparent
                Ok(args[0].clone())
            } else {
                Ok(Value::Values(args.to_vec()))
            }
        }
        "call-with-values" => {
            ensure_args("call-with-values", args, 2, p)?;
            let producer = args[0].clone();
            let consumer = args[1].clone();
            let produced = force(apply_value(&producer, &[], p)?)?;
            let call_args = match produced {
                Value::Values(vals) => vals,
                single => vec![single],
            };
            force(apply_value(&consumer, &call_args, p)?)
        }
        _ => Err(EvalError::UnboundVariable(format!("{} at {}", op, p))),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    values_equal_depth(a, b, 0)
}

fn values_equal_depth(a: &Value, b: &Value, depth: usize) -> bool {
    if depth > 10000 {
        return false; // prevent infinite recursion on circular structures
    }
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a), Value::Pair(b)) => {
            if Rc::ptr_eq(a, b) {
                return true;
            }
            let (a_car, a_cdr) = {
                let p = a.borrow();
                (p.0.clone(), p.1.clone())
            };
            let (b_car, b_cdr) = {
                let p = b.borrow();
                (p.0.clone(), p.1.clone())
            };
            values_equal_depth(&a_car, &b_car, depth + 1) && values_equal_depth(&a_cdr, &b_cdr, depth + 1)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal_depth(x, y, depth + 1))
        }
        _ => false,
    }
}

fn ensure_args(op: &str, args: &[Value], expected: usize, p: Pos) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(EvalError::Arity(format!(
            "{} expects {} arguments, got {} at {}",
            op, expected,
            args.len(),
            p
        )));
    }
    Ok(())
}

// ── CPS evaluator for call/cc support ──────────────────────────────────────

/// Check if an expression (or any sub-expression) contains an actual call/cc invocation.
/// Used to decide whether to use CPS evaluation.
/// Quoted forms are skipped to avoid false positives on `'call-with-current-continuation`.
fn contains_callcc(expr: &Expr) -> bool {
    match expr {
        Expr::List(elems, _) => {
            if let Some(Expr::Symbol(s, _)) = elems.first() {
                // Skip quoted forms — 'call-with-current-continuation is data, not a call
                if s == "quote" {
                    return false;
                }
                // This IS a call/cc invocation
                if s == "call/cc" || s == "call-with-current-continuation" {
                    return true;
                }
            }
            elems.iter().any(contains_callcc)
        }
        // A bare call/cc symbol reference (e.g., passed as first-class value)
        Expr::Symbol(s, _) => s == "call/cc" || s == "call-with-current-continuation",
        _ => false,
    }
}

/// CPS body evaluator: evaluate a sequence of expressions. k receives the last value.
/// Always uses CPS evaluation to properly thread continuations.
fn eval_body_cps(exprs: &[Expr], env: &Env, k: ContFn) -> Result<Value, EvalError> {
    eval_body_cps_from(exprs, 0, env, k)
}

fn eval_body_cps_from(exprs: &[Expr], start: usize, env: &Env, k: ContFn) -> Result<Value, EvalError> {
    if start >= exprs.len() {
        return k.call(Value::Boolean(false));
    }
    // Set BODY_CTX: captures the restart context for any call/cc in this expression.
    // The outer_k is `k` (the continuation of the WHOLE body from here), NOT k_rest.
    // The restart function will call eval_body_cps_from(exprs, start, env, k) again.
    let exprs_rc = exprs.to_vec();
    let env2 = env.clone();
    let k2 = k.clone();
    BODY_CTX.with(|ctx| {
        *ctx.borrow_mut() = Some(RestartCtx {
            body_exprs: exprs_rc,
            body_idx: start,
            body_env: env2,
            outer_k: k2,
        });
    });

    let result = if start == exprs.len() - 1 {
        // Last expression: continuation is k
        eval_cps(&exprs[start], env, k)
    } else {
        // Not last: continuation discards result and continues with rest
        let exprs3 = exprs.to_vec();
        let env3 = env.clone();
        let k_rest = ContFn::new(move |_v| {
            eval_body_cps_from(&exprs3, start + 1, &env3, k.clone())
        });
        eval_cps(&exprs[start], env, k_rest)
    };

    // Clear BODY_CTX after evaluation (it may have been updated by nested evals)
    // Only clear if it's still "ours" — actually, just leave it; it'll be overwritten by next call
    // We don't need to clear because BODY_CTX is always set before any call/cc is reached.
    result
}

/// CPS evaluator: evaluate expr in env, then call k with the result.
fn eval_cps(expr: &Expr, env: &Env, k: ContFn) -> Result<Value, EvalError> {
    match expr {
        Expr::List(elems, p) if !elems.is_empty() => {
            let pos = *p;
            // Check for special forms first
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "call/cc" | "call-with-current-continuation" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!(
                                "call/cc expects 1 argument at {}", pos
                            )));
                        }
                        let f = eval(&elems[1], env)?;
                        // apply_cps_callcc checks CC_OVERRIDE_STACK internally
                        return apply_cps_callcc(f, pos, k);
                    }
                    "define" => {
                        // (define name value) — use CPS for value evaluation to capture continuations
                        if elems.len() >= 3 {
                            if let Expr::Symbol(name, _) = &elems[1] {
                                let name2 = name.clone();
                                let env2 = env.clone();
                                let k_define = ContFn::new(move |v| {
                                    env_set(&env2, name2.clone(), v);
                                    k.call(Value::Symbol(name2.clone()))
                                });
                                return eval_cps(&elems[2], env, k_define);
                            }
                        }
                        // (define (f ...) body) or other forms — evaluate eagerly (creates lambda)
                        let v = eval(expr, env)?;
                        let v = force(v)?;
                        return k.call(v);
                    }
                    "set!" => {
                        // (set! name value) — CPS for value if it contains call/cc
                        if elems.len() == 3 {
                            if let Expr::Symbol(name, _) = &elems[1] {
                                let name2 = name.clone();
                                let env2 = env.clone();
                                let k_set = ContFn::new(move |v| {
                                    env_set_existing(&env2, &name2, v);
                                    k.call(Value::Nil)
                                });
                                return eval_cps(&elems[2], env, k_set);
                            }
                        }
                        // Fall through to eager eval
                        let v = eval(expr, env)?;
                        let v = force(v)?;
                        return k.call(v);
                    }
                    "let" => {
                        // (let ...) — CPS body evaluation for captured continuations
                        return eval_cps_let(&elems[1..], env, pos, k);
                    }
                    "begin" => {
                        // (begin e1 e2 ...) — CPS body
                        if elems[1..].is_empty() {
                            return k.call(Value::Boolean(false));
                        }
                        return eval_body_cps(&elems[1..], env, k);
                    }
                    "letrec" => {
                        return eval_cps_letrec(&elems[1..], env, pos, k, false);
                    }
                    "letrec*" => {
                        return eval_cps_letrec(&elems[1..], env, pos, k, true);
                    }
                    "let*" => {
                        return eval_cps_let_star(&elems[1..], env, pos, k);
                    }
                    "if" => {
                        // (if cond then [else]) — evaluate condition eagerly, then CPS branch
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Arity(format!("if: bad syntax at {}", pos)));
                        }
                        let cond_val = force(eval(&elems[1], env)?)?;
                        let branch = if !matches!(cond_val, Value::Boolean(false)) {
                            elems[2].clone()
                        } else if elems.len() == 4 {
                            elems[3].clone()
                        } else {
                            return k.call(Value::Boolean(false));
                        };
                        return eval_cps(&branch, env, k);
                    }
                    "dynamic-wind" => {
                        if elems.len() != 4 {
                            return Err(EvalError::Arity(format!(
                                "dynamic-wind expects 3 arguments at {}", pos
                            )));
                        }
                        let in_thunk = force(eval(&elems[1], env)?)?;
                        let body_thunk = force(eval(&elems[2], env)?)?;
                        let out_thunk = force(eval(&elems[3], env)?)?;

                        // Call in-thunk
                        force(apply_value(&in_thunk, &[], pos)?)?;

                        // Push to wind stack
                        let wind_id = next_wind_id();
                        WIND_STACK.with(|ws| ws.borrow_mut().push(WindEntry {
                            id: wind_id,
                            in_thunk: in_thunk.clone(),
                            out_thunk: out_thunk.clone(),
                        }));

                        // Call body-thunk via CPS; body_k handles normal completion
                        let out_thunk2 = out_thunk.clone();
                        let cleaned_up = Rc::new(RefCell::new(false));
                        let cleaned_up2 = cleaned_up.clone();
                        let body_k = ContFn::new(move |body_val| {
                            *cleaned_up2.borrow_mut() = true;
                            WIND_STACK.with(|ws| ws.borrow_mut().pop());
                            force(apply_value(&out_thunk2, &[], pos)?)?;
                            k.call(body_val)
                        });

                        let result = apply_cps_value(body_thunk, vec![], pos, body_k);

                        match result {
                            Ok(v) => return Ok(v),
                            Err(EvalError::ContinuationEscape(id, val)) => {
                                if !*cleaned_up.borrow() {
                                    WIND_STACK.with(|ws| ws.borrow_mut().pop());
                                    force(apply_value(&out_thunk, &[], pos)?)?;
                                }
                                return Err(EvalError::ContinuationEscape(id, val));
                            }
                            Err(e) => {
                                if !*cleaned_up.borrow() {
                                    WIND_STACK.with(|ws| ws.borrow_mut().pop());
                                    force(apply_value(&out_thunk, &[], pos)?)?;
                                }
                                return Err(e);
                            }
                        }
                    }
                    // Other recognized special forms that produce values without calling
                    // user lambdas: evaluate eagerly and pass result to k.
                    // ContinuationEscape propagates via ? if any nested call invokes a continuation.
                    "lambda" | "case-lambda" | "quote" | "define-syntax"
                    | "define-record-type" | "string-set!" => {
                        let v = eval(expr, env)?;
                        let v = force(v)?;
                        return k.call(v);
                    }
                    "guard" => {
                        let v = eval_guard(&elems[1..], env, pos)?;
                        let v = force(v)?;
                        return k.call(v);
                    }
                    // cond/and/or/when/unless/case/do may call user functions — handle via CPS.
                    // Desugar them: use eager eval to get the value but note that if any
                    // continuation is invoked inside, ContinuationEscape propagates via ? correctly.
                    "cond" | "and" | "or" | "when" | "unless" | "case" | "do" => {
                        let v = eval(expr, env)?;
                        let v = force(v)?;
                        return k.call(v);
                    }
                    _ => {
                        // Check if this is a macro
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            // Expand macro and re-evaluate via CPS
                            let expanded = eval_macro(&literals, &rules, &def_env, elems, env, pos)?;
                            return k.call(force(expanded)?);
                        }
                        // Otherwise: user function call or builtin — fall through to general application
                    }
                }
            }

            // General function application: evaluate function expression via CPS,
            // then evaluate all args via CPS (so lambda bodies are always CPS-evaluated),
            // then apply via apply_cps_value.
            let func_expr = elems[0].clone();
            let arg_exprs = elems[1..].to_vec();
            let env2 = env.clone();
            let k_func = ContFn::new(move |func_val| {
                eval_args_cps(func_val, &arg_exprs, &env2, pos, 0, Vec::new(), k.clone())
            });
            eval_cps(&func_expr, env, k_func)
        }
        _ => {
            // Atoms and other non-list expressions: evaluate eagerly
            let v = eval(expr, env)?;
            let v = force(v)?;
            k.call(v)
        }
    }
}

/// CPS variant of let evaluation.
fn eval_cps_let(args: &[Expr], env: &Env, p: Pos, k: ContFn) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("let requires bindings and body at {}", p)));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(_name, _) = &args[0] {
        // Use the existing eval_let for named let (it creates a loop lambda)
        // and wrap result with k
        let v = eval_let(args, env, p)?;
        let v = force(v)?;
        return k.call(v);
    }
    // Regular let
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("let: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("let: expected symbol in binding at {}", p))),
                };
                let val = eval(&pair[1], env)?;
                let val = force(val)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Type(format!("let: invalid binding at {}", p))),
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return k.call(Value::Boolean(false));
    }
    eval_body_cps(body, &local_env, k)
}

/// CPS variant of letrec/letrec* evaluation.
fn eval_cps_letrec(args: &[Expr], env: &Env, p: Pos, k: ContFn, star: bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("letrec requires bindings and body at {}", p)));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("letrec: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    if !star {
        // Pre-bind all names to #f
        let mut names = Vec::new();
        let mut init_exprs = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    let name = match &pair[0] {
                        Expr::Symbol(s, _) => s.clone(),
                        _ => return Err(EvalError::Type(format!("letrec: expected symbol at {}", p))),
                    };
                    env_set(&local_env, name.clone(), Value::Boolean(false));
                    names.push(name);
                    init_exprs.push(pair[1].clone());
                }
                _ => return Err(EvalError::Type(format!("letrec: invalid binding at {}", p))),
            }
        }
        for (name, init) in names.iter().zip(init_exprs.iter()) {
            let val = force(eval(init, &local_env)?)?;
            env_set(&local_env, name.clone(), val);
        }
    } else {
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    let name = match &pair[0] {
                        Expr::Symbol(s, _) => s.clone(),
                        _ => return Err(EvalError::Type(format!("letrec*: expected symbol at {}", p))),
                    };
                    let val = force(eval(&pair[1], &local_env)?)?;
                    env_set(&local_env, name, val);
                }
                _ => return Err(EvalError::Type(format!("letrec*: invalid binding at {}", p))),
            }
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return k.call(Value::Boolean(false));
    }
    eval_body_cps(body, &local_env, k)
}

/// CPS variant of let* evaluation.
fn eval_cps_let_star(args: &[Expr], env: &Env, p: Pos, k: ContFn) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("let* requires bindings and body at {}", p)));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Type(format!("let*: expected bindings list at {}", p))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Type(format!("let*: expected symbol at {}", p))),
                };
                let val = force(eval(&pair[1], &local_env)?)?;
                env_set(&local_env, name, val);
            }
            _ => return Err(EvalError::Type(format!("let*: invalid binding at {}", p))),
        }
    }
    let body = &args[1..];
    if body.is_empty() {
        return k.call(Value::Boolean(false));
    }
    eval_body_cps(body, &local_env, k)
}

/// Evaluate argument expressions one by one using CPS, accumulating results.
/// When all args are evaluated, apply func to them and call k.
fn eval_args_cps(
    func_val: Value,
    arg_exprs: &[Expr],
    env: &Env,
    pos: Pos,
    idx: usize,
    evaluated: Vec<Value>,
    k: ContFn,
) -> Result<Value, EvalError> {
    if idx >= arg_exprs.len() {
        // All args evaluated; apply function
        return apply_cps_value(func_val, evaluated, pos, k);
    }

    // Always evaluate args via CPS so lambda bodies are always CPS-evaluated
    let func_val2 = func_val;
    let remaining: Vec<Expr> = arg_exprs[idx + 1..].to_vec();
    let env2 = env.clone();
    let arg_expr = arg_exprs[idx].clone();
    let k_arg = ContFn::new(move |v| {
        let mut evaled = evaluated.clone();
        evaled.push(v);
        eval_args_cps(func_val2.clone(), &remaining, &env2, pos, 0, evaled, k.clone())
    });
    eval_cps(&arg_expr, env, k_arg)
}

/// Apply a captured continuation (call/cc handler).
fn apply_cps_callcc(f: Value, pos: Pos, outer_k: ContFn) -> Result<Value, EvalError> {
    // Check if we're replaying (an override is waiting for the next call/cc)
    if let Some(v) = CC_OVERRIDE_STACK.with(|s| {
        if !s.borrow().is_empty() { Some(s.borrow_mut().pop().unwrap()) } else { None }
    }) {
        // Replay: return the override value directly via outer_k
        return outer_k.call(v);
    }

    let id = next_cc_id();

    // Build the restart function for reentrant invocations.
    // Take the current BODY_CTX (if available) to build a proper restart.
    // Also save the wind stack for rewinding on re-entry.
    let saved_wind: Vec<WindEntry> = WIND_STACK.with(|ws| ws.borrow().clone());
    let restart_fn: ContFn = BODY_CTX.with(|ctx| {
        if let Some(rc) = ctx.borrow().as_ref() {
            // Clone the restart context
            let r_exprs = rc.body_exprs.clone();
            let r_idx = rc.body_idx;
            let r_env = rc.body_env.clone();
            let r_outer_k = rc.outer_k.clone();
            let sw = saved_wind.clone();
            ContFn::new(move |v| {
                // Rewind wind stack: unwind current, rewind to saved
                let current_wind = WIND_STACK.with(|ws| ws.borrow().clone());
                let common_len = current_wind.iter().zip(sw.iter())
                    .take_while(|(c, s)| c.id == s.id)
                    .count();
                // Unwind current entries beyond common prefix (innermost first)
                for entry in current_wind[common_len..].iter().rev() {
                    force(apply_value(&entry.out_thunk, &[], Pos::default())?)?;
                }
                // Rewind to saved entries beyond common prefix (outermost first)
                for entry in &sw[common_len..] {
                    force(apply_value(&entry.in_thunk, &[], Pos::default())?)?;
                }
                // Restore wind stack to saved state
                WIND_STACK.with(|ws| *ws.borrow_mut() = sw.clone());

                // Push override so the replayed call/cc returns v
                CC_OVERRIDE_STACK.with(|s| s.borrow_mut().push(v));
                // Re-evaluate the body from the start position
                eval_body_cps_from(&r_exprs, r_idx, &r_env, r_outer_k.clone())
            })
        } else {
            // No body context: fall back to outer_k (for escape-only continuations)
            outer_k.clone()
        }
    });

    // Build the continuation value
    let restart_fn2 = restart_fn.clone();
    let cont_val = Value::Continuation {
        id,
        func: ContFn::new(move |v| {
            let is_active = ACTIVE_CC.with(|ac| ac.borrow().contains(&id));
            if is_active {
                // Escape: throw to unwind the stack (prevents stack overflow)
                Err(EvalError::ContinuationEscape(id, Box::new(v)))
            } else {
                // Reentrant: use restart function
                restart_fn2.call(v)
            }
        }),
    };

    // Register this call/cc as active
    ACTIVE_CC.with(|ac| ac.borrow_mut().push(id));

    // Track whether the lambda passed to call/cc has "returned" (i.e., called outer_k).
    // If it has, any subsequent ContinuationEscape is from a logically external call
    // (the CPS chain extended past the lambda return). In that case, use restart_fn
    // for correct dynamic-wind handling. If the lambda hasn't returned, it's a true
    // escape and outer_k is used for stack efficiency (important for ctak-style code).
    let lambda_returned = Rc::new(RefCell::new(false));
    let lambda_returned2 = lambda_returned.clone();
    let outer_k_for_escape = outer_k.clone();
    let tracked_outer_k = ContFn::new(move |v| {
        *lambda_returned2.borrow_mut() = true;
        outer_k.call(v)
    });

    // Apply f to cont_val, using tracked_outer_k as the continuation
    let result = apply_cps_value(f, vec![cont_val], pos, tracked_outer_k);

    // Remove from active
    ACTIVE_CC.with(|ac| {
        let mut v = ac.borrow_mut();
        if let Some(i) = v.iter().rposition(|x| *x == id) {
            v.remove(i);
        }
    });

    match result {
        Ok(v) => Ok(v),
        Err(EvalError::ContinuationEscape(eid, val)) if eid == id => {
            if *lambda_returned.borrow() {
                // Lambda already returned; this escape is from outside the call/cc body
                // (via CPS chain). Use restart_fn for correct dynamic-wind handling.
                restart_fn.call(*val)
            } else {
                // True escape from within the lambda body. Use outer_k for stack efficiency.
                outer_k_for_escape.call(*val)
            }
        }
        Err(e) => Err(e),
    }
}

/// CPS apply: apply a value as a function to args, then call k.
fn apply_cps_value(func: Value, args: Vec<Value>, pos: Pos, k: ContFn) -> Result<Value, EvalError> {
    match &func {
        Value::Symbol(op) if op == "call/cc" || op == "call-with-current-continuation" => {
            // call/cc called as a value (e.g., ((lambda (cc) ...) call/cc))
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "call/cc expects 1 argument at {}", pos
                )));
            }
            apply_cps_callcc(args.into_iter().next().unwrap(), pos, k)
        }
        Value::Lambda { params, rest_param, body, env } => {
            // Apply lambda with CPS body evaluation
            let local_env = new_env(Some(env.clone()));
            if let Some(ref rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(), args.len(), pos
                    )));
                }
                for (p, a) in params.iter().zip(args.iter()) {
                    env_set(&local_env, p.clone(), a.clone());
                }
                let rest_list = vec_to_list(args[params.len()..].to_vec());
                env_set(&local_env, rest.clone(), rest_list);
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {} at {}",
                        params.len(), args.len(), pos
                    )));
                }
                for (p, a) in params.iter().zip(args.iter()) {
                    env_set(&local_env, p.clone(), a.clone());
                }
            }
            if body.is_empty() {
                return k.call(Value::Boolean(false));
            }
            eval_body_cps(body, &local_env, k)
        }
        Value::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = new_env(Some(env.clone()));
                    for (p, a) in params.iter().zip(args.iter()) {
                        env_set(&local_env, p.clone(), a.clone());
                    }
                    if let Some(ref rest) = rest_param {
                        let rest_list = vec_to_list(args[params.len()..].to_vec());
                        env_set(&local_env, rest.clone(), rest_list);
                    }
                    if body.is_empty() {
                        return k.call(Value::Boolean(false));
                    }
                    return eval_body_cps(body, &local_env, k);
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments at {}",
                args.len(), pos
            )))
        }
        Value::Continuation { id, func } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation expects 1 argument, got {} at {}", args.len(), pos
                )));
            }
            let val = args.into_iter().next().unwrap();
            let is_active = ACTIVE_CC.with(|ac| ac.borrow().contains(id));
            if is_active {
                Err(EvalError::ContinuationEscape(*id, Box::new(val)))
            } else {
                func.call(val)
            }
        }
        _ => {
            // For all other callable values, use the existing apply_value mechanism
            // and wrap the result with k
            let result = force(apply_value(&func, &args, pos)?)?;
            k.call(result)
        }
    }
}

// ── End CPS evaluator ───────────────────────────────────────────────────────

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    // Check if any expression contains call/cc; if so, use the CPS evaluator.
    if exprs.iter().any(contains_callcc) {
        let k = ContFn::new(|v| Ok(v));
        let result = eval_body_cps(&exprs, &env, k)?;
        return Ok(result.to_string());
    }
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &env)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT.with(|out| out.borrow_mut().clear());
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    // Check if any expression contains call/cc; if so, use the CPS evaluator.
    let result = if exprs.iter().any(contains_callcc) {
        let k = ContFn::new(|v| Ok(v));
        eval_body_cps(&exprs, &env, k)?
    } else {
        let mut result = Value::Boolean(false);
        for expr in &exprs {
            result = eval(expr, &env)?;
        }
        result
    };
    let output = OUTPUT.with(|out| out.borrow().clone());
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
