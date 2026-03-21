pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

thread_local! {
    static CALLCC_PENDING: RefCell<Option<(Pos, Value)>> = RefCell::new(None);
    static CALLCC_TOP_IDX: Cell<usize> = Cell::new(0);
    static CONT_RETURN: RefCell<Option<(Pos, usize, Value)>> = RefCell::new(None);
    static RAISED_VALUE: RefCell<Option<Value>> = RefCell::new(None);
}

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}##{}", base, n)
}

/// Source position (1-indexed line, 0-indexed column).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Pos {
    line: usize,
    col: usize,
}

impl Pos {
    fn fmt(&self) -> String {
        format!("{}:{}", self.line, self.col)
    }
}

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    Pair(Rc<RefCell<(Value, Value)>>),
    Nil,
    Lambda {
        params: Rc<Vec<String>>,
        rest_param: Option<String>,
        body: Rc<Vec<Expr>>,
        env: Env,
    },
    Builtin(String),
    Continuation(Pos, usize),
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Rc<Vec<String>>,
        rules: Rc<Vec<(Expr, Expr)>>,
        def_env: Env,
    },
    Values(Vec<Value>),
    Record {
        type_id: usize,
        type_name: String,
        fields: Rc<RefCell<Vec<Value>>>,
    },
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

fn make_rational_value(n: i64, d: i64) -> Value {
    assert!(d != 0);
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

fn value_to_f64(v: &Value, pos: Pos) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(format!("expected number, got {} at {}", v.display(), pos.fmt()))),
    }
}

fn value_to_exact(v: &Value, pos: Pos) -> Result<(i64, i64), EvalError> {
    match v {
        Value::Integer(n) => Ok((*n, 1)),
        Value::Rational(n, d) => Ok((*n, *d)),
        _ => Err(EvalError::Type(format!("expected exact number, got {} at {}", v.display(), pos.fmt()))),
    }
}

fn is_numeric(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn any_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

fn exact_add(args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    let mut n: i64 = 0;
    let mut d: i64 = 1;
    for a in args {
        let (an, ad) = value_to_exact(a, pos)?;
        n = n * ad + an * d;
        d = d * ad;
        let g = gcd(n.abs(), d.abs());
        if g != 0 { n /= g; d /= g; }
    }
    Ok(make_rational_value(n, d))
}

fn exact_sub(args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    if args.len() == 1 {
        let (n, d) = value_to_exact(&args[0], pos)?;
        return Ok(make_rational_value(-n, d));
    }
    let (mut n, mut d) = value_to_exact(&args[0], pos)?;
    for a in &args[1..] {
        let (an, ad) = value_to_exact(a, pos)?;
        n = n * ad - an * d;
        d = d * ad;
        let g = gcd(n.abs(), d.abs());
        if g != 0 { n /= g; d /= g; }
    }
    Ok(make_rational_value(n, d))
}

fn exact_mul(args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    let mut n: i64 = 1;
    let mut d: i64 = 1;
    for a in args {
        let (an, ad) = value_to_exact(a, pos)?;
        n *= an;
        d *= ad;
        let g = gcd(n.abs(), d.abs());
        if g != 0 { n /= g; d /= g; }
    }
    Ok(make_rational_value(n, d))
}

fn exact_div(args: &[Value], pos: Pos) -> Result<Value, EvalError> {
    let (mut n, mut d) = value_to_exact(&args[0], pos)?;
    for a in &args[1..] {
        let (an, ad) = value_to_exact(a, pos)?;
        if an == 0 {
            return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
        }
        n *= ad;
        d *= an;
        let g = gcd(n.abs(), d.abs());
        if g != 0 { n /= g; d /= g; }
    }
    Ok(make_rational_value(n, d))
}

impl Value {
    /// Format for `write` and return values (strings get quotes).
    fn display(&self) -> String {
        self.fmt_value(true)
    }

    /// Format for `display` output (strings without quotes).
    fn display_unquoted(&self) -> String {
        self.fmt_value(false)
    }

    fn fmt_value(&self, quote_strings: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                if f.is_infinite() || f.is_nan() {
                    format!("{}", f)
                } else if *f == f.trunc() && f.abs() < 1e15 {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => {
                if quote_strings {
                    format!("\"{}\"", s)
                } else {
                    s.clone()
                }
            }
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => match c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{}", c),
            },
            Value::Nil => "()".to_string(),
            Value::Pair(cell) => {
                let mut out = String::from("(");
                let mut cur_cell = Rc::clone(cell);
                let mut first = true;
                let mut seen = HashSet::new();
                loop {
                    let ptr = Rc::as_ptr(&cur_cell) as usize;
                    if !seen.insert(ptr) {
                        out.push_str("...");
                        break;
                    }
                    let (car_val, cdr_val) = {
                        let inner = cur_cell.borrow();
                        (inner.0.clone(), inner.1.clone())
                    };
                    if !first {
                        out.push(' ');
                    }
                    first = false;
                    out.push_str(&car_val.fmt_value(quote_strings));
                    match cdr_val {
                        Value::Pair(next) => cur_cell = next,
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.fmt_value(quote_strings));
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Vector(v) => {
                let elems = v.borrow();
                let mut out = String::from("#(");
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { out.push(' '); }
                    out.push_str(&e.fmt_value(quote_strings));
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } => "<procedure>".to_string(),
            Value::Builtin(name) => format!("<builtin:{}>", name),
            Value::Continuation(_, _) => "<continuation>".to_string(),
            Value::Macro { .. } => "<macro>".to_string(),
            Value::Values(vals) => {
                if vals.is_empty() {
                    "".to_string()
                } else {
                    vals[0].fmt_value(quote_strings)
                }
            }
            Value::Record { type_name, .. } => format!("<record:{}>", type_name),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn is_builtin_name(name: &str) -> bool {
        matches!(
            name,
            "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
                | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length"
                | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
                | "display" | "write" | "newline"
                | "string-append" | "string-length" | "substring"
                | "string->number" | "number->string" | "symbol->string" | "string->symbol"
                | "string-copy" | "string-ref" | "string-set!"
                | "string->list" | "list->string" | "char->integer" | "integer->char"
                | "apply" | "call/cc"
                | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
                | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
                | "list-ref" | "list-tail" | "list?" | "reverse" | "assoc" | "map"
                | "set-car!" | "set-cdr!" | "cddr" | "cadr" | "caar" | "cdar"
                | "eq?" | "eqv?" | "equal?"
                | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
                | "char=?" | "char<?"
                | "string=?" | "string<?" | "string-ci=?"
                | "string-upcase" | "string-downcase"
                | "vector" | "make-vector" | "vector-ref" | "vector-set!"
                | "vector-length" | "vector?" | "vector->list" | "list->vector"
                | "values" | "call-with-values"
                | "exact?" | "inexact?" | "exact->inexact" | "inexact->exact"
                | "numerator" | "denominator" | "rational?" | "integer?"
        )
    }

    fn as_integer(&self, pos: Pos) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            Value::Rational(n, d) if *d == 1 => Ok(*n),
            Value::Float(f) if *f == f.trunc() => Ok(*f as i64),
            other => Err(EvalError::Type(format!(
                "expected integer, got {} at {}",
                other.display(),
                pos.fmt()
            ))),
        }
    }
}

/// A parsed S-expression with source position.
#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Expr>),
}

// ── Environment ────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Env(Rc<RefCell<EnvInner>>);

#[derive(Debug)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

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
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = inner.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn set(&self, name: String, val: Value) {
        self.0.borrow_mut().bindings.insert(name, val);
    }

    /// Mutate an existing binding in the nearest scope that contains it.
    fn set_existing(&self, name: &str, val: Value) -> bool {
        let has_key = self.0.borrow().bindings.contains_key(name);
        if has_key {
            self.0.borrow_mut().bindings.insert(name.to_string(), val);
            true
        } else {
            let parent = self.0.borrow().parent.clone();
            if let Some(parent) = parent {
                parent.set_existing(name, val)
            } else {
                false
            }
        }
    }

    /// Copy all bindings from this env chain into target, skipping any
    /// that already exist in target's chain (preserves lexical scoping).
    fn copy_all_into_if_absent(&self, target: &Env) {
        let inner = self.0.borrow();
        for (k, v) in &inner.bindings {
            if target.get(k).is_none() {
                target.set(k.clone(), v.clone());
            }
        }
        if let Some(ref parent) = inner.parent {
            parent.copy_all_into_if_absent(target);
        }
    }
}

// ── Parser ──────────────────────────────────────────────────────────

struct Parser {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn current_pos(&self) -> Pos {
        Pos {
            line: self.line,
            col: self.col,
        }
    }

    fn advance(&mut self) {
        if self.pos < self.chars.len() {
            if self.chars[self.pos] == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.chars.len() {
            if self.chars[self.pos].is_whitespace() {
                self.advance();
            } else if self.chars[self.pos] == ';' {
                while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.advance();
        }
        c
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        self.skip_whitespace();
        let start = self.current_pos();
        match self.peek() {
            None => Err(EvalError::Parse(format!(
                "unexpected end of input at {}",
                start.fmt()
            ))),
            Some('(') => self.parse_list(),
            Some('\'') => {
                self.next_char();
                let inner = self.parse_expr()?;
                Ok(Expr {
                    kind: ExprKind::List(vec![
                        Expr {
                            kind: ExprKind::Symbol("quote".into()),
                            pos: start,
                        },
                        inner,
                    ]),
                    pos: start,
                })
            }
            Some('"') => self.parse_string(),
            Some('#') => self.parse_hash(),
            _ => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '('
        let mut items = Vec::new();
        loop {
            self.skip_whitespace();
            match self.peek() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated list at {}",
                        start.fmt()
                    )))
                }
                Some(')') => {
                    self.next_char();
                    return Ok(Expr {
                        kind: ExprKind::List(items),
                        pos: start,
                    });
                }
                _ => items.push(self.parse_expr()?),
            }
        }
    }

    fn parse_string(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume opening "
        let mut s = String::new();
        loop {
            match self.next_char() {
                None => {
                    return Err(EvalError::Parse(format!(
                        "unterminated string at {}",
                        start.fmt()
                    )))
                }
                Some('\\') => match self.next_char() {
                    Some('n') => s.push('\n'),
                    Some('t') => s.push('\t'),
                    Some('\\') => s.push('\\'),
                    Some('"') => s.push('"'),
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => {
                        return Err(EvalError::Parse(format!(
                            "unterminated escape at {}",
                            start.fmt()
                        )))
                    }
                },
                Some('"') => {
                    return Ok(Expr {
                        kind: ExprKind::Str(s),
                        pos: start,
                    })
                }
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_hash(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        self.next_char(); // consume '#'
        match self.peek() {
            Some('t') => {
                self.next_char();
                Ok(Expr {
                    kind: ExprKind::Boolean(true),
                    pos: start,
                })
            }
            Some('f') => {
                self.next_char();
                Ok(Expr {
                    kind: ExprKind::Boolean(false),
                    pos: start,
                })
            }
            Some('\\') => {
                self.next_char(); // consume '\'
                // Try to read a named character literal
                let char_start = self.pos;
                // Read first char
                match self.next_char() {
                    Some(c) => {
                        // Check if it's a letter that could start a name
                        if c.is_alphabetic() {
                            // Try to read more letters
                            let mut name = String::new();
                            name.push(c);
                            while self.pos < self.chars.len() {
                                let next = self.chars[self.pos];
                                if next.is_alphabetic() {
                                    name.push(next);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                            if name.len() == 1 {
                                // Single character like #\a
                                Ok(Expr {
                                    kind: ExprKind::Char(c),
                                    pos: start,
                                })
                            } else {
                                // Named character
                                match name.as_str() {
                                    "space" => Ok(Expr {
                                        kind: ExprKind::Char(' '),
                                        pos: start,
                                    }),
                                    "newline" => Ok(Expr {
                                        kind: ExprKind::Char('\n'),
                                        pos: start,
                                    }),
                                    "tab" => Ok(Expr {
                                        kind: ExprKind::Char('\t'),
                                        pos: start,
                                    }),
                                    _ => Err(EvalError::Parse(format!(
                                        "unknown character name '{}' at {}",
                                        name,
                                        start.fmt()
                                    ))),
                                }
                            }
                        } else {
                            Ok(Expr {
                                kind: ExprKind::Char(c),
                                pos: start,
                            })
                        }
                    }
                    None => Err(EvalError::Parse(format!(
                        "unterminated character literal at {}",
                        start.fmt()
                    ))),
                }
            }
            _ => Err(EvalError::Parse(format!(
                "invalid # literal at {}",
                start.fmt()
            ))),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, EvalError> {
        let start = self.current_pos();
        let char_start = self.pos;
        while self.pos < self.chars.len() {
            let c = self.chars[self.pos];
            if c.is_whitespace() || c == '(' || c == ')' || c == '"' || c == ';' {
                break;
            }
            self.advance();
        }
        let token: String = self.chars[char_start..self.pos].iter().collect();
        if token.is_empty() {
            return Err(EvalError::Parse(format!("empty token at {}", start.fmt())));
        }
        if let Ok(n) = token.parse::<i64>() {
            return Ok(Expr {
                kind: ExprKind::Integer(n),
                pos: start,
            });
        }
        // Try rational literal: digits/digits (e.g., 1/3, -6/4)
        if let Some(slash_pos) = token.find('/') {
            if slash_pos > 0 || (token.starts_with('-') && slash_pos > 1) {
                let num_part = &token[..slash_pos];
                let den_part = &token[slash_pos + 1..];
                if let (Ok(n), Ok(d)) = (num_part.parse::<i64>(), den_part.parse::<i64>()) {
                    if d != 0 {
                        return Ok(Expr {
                            kind: {
                                // Simplify at parse time
                                let sign = if d < 0 { -1 } else { 1 };
                                let n = n * sign;
                                let d = d * sign;
                                let g = gcd(n.abs(), d);
                                let n = n / g;
                                let d = d / g;
                                if d == 1 {
                                    ExprKind::Integer(n)
                                } else {
                                    ExprKind::Rational(n, d)
                                }
                            },
                            pos: start,
                        });
                    }
                }
            }
        }
        // Try float literal
        if let Ok(f) = token.parse::<f64>() {
            return Ok(Expr {
                kind: ExprKind::Float(f),
                pos: start,
            });
        }
        Ok(Expr {
            kind: ExprKind::Symbol(token),
            pos: start,
        })
    }

    fn parse_all(&mut self) -> Result<Vec<Expr>, EvalError> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            if self.pos >= self.chars.len() {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }
}

// ── Evaluator ───────────────────────────────────────────────────────

fn eval(expr: &Expr, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    let result = 'tco: loop {
        let pos = cur_expr.pos;
        match cur_expr.kind.clone() {
            ExprKind::Integer(n) => break 'tco Ok(Value::Integer(n)),
            ExprKind::Float(f) => break 'tco Ok(Value::Float(f)),
            ExprKind::Rational(n, d) => break 'tco Ok(Value::Rational(n, d)),
            ExprKind::Boolean(b) => break 'tco Ok(Value::Boolean(b)),
            ExprKind::Str(s) => break 'tco Ok(Value::Str(s)),
            ExprKind::Char(c) => break 'tco Ok(Value::Char(c)),
            ExprKind::Symbol(name) => {
                if let Some(val) = cur_env.get(&name) {
                    break 'tco Ok(val);
                } else if Value::is_builtin_name(&name) {
                    break 'tco Ok(Value::Builtin(name));
                } else {
                    break 'tco Err(EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt())));
                }
            }
            ExprKind::List(items) => {
                if items.is_empty() {
                    break 'tco Err(EvalError::Parse(format!(
                        "empty application at {}",
                        pos.fmt()
                    )));
                }
                // Check for special forms
                if let ExprKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "if requires 2 or 3 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue 'tco;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue 'tco;
                            } else {
                                break 'tco Ok(Value::Nil);
                            }
                        }
                        "define" => {
                            break 'tco eval_define(&items[1..], pos, &cur_env, out);
                        }
                        "quote" => {
                            break 'tco eval_quote(&items[1..], pos);
                        }
                        "lambda" => {
                            break 'tco eval_lambda(&items[1..], pos, &cur_env);
                        }
                        "and" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Boolean(true));
                            }
                            for a in &args[..args.len() - 1] {
                                let v = eval(a, &cur_env, out)?;
                                if !v.is_truthy() {
                                    break 'tco Ok(v);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "or" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Boolean(false));
                            }
                            for a in &args[..args.len() - 1] {
                                let v = eval(a, &cur_env, out)?;
                                if v.is_truthy() {
                                    break 'tco Ok(v);
                                }
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "let requires bindings and body at {}",
                                    pos.fmt()
                                )));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let ExprKind::Symbol(ref loop_name) = args[0].kind {
                                if args.len() < 3 {
                                    break 'tco Err(EvalError::Arity(format!(
                                        "named let requires bindings and body at {}",
                                        pos.fmt()
                                    )));
                                }
                                let bindings_list = match &args[1].kind {
                                    ExprKind::List(bs) => bs,
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "let: bindings must be a list at {}",
                                        pos.fmt()
                                    ))),
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for b in bindings_list {
                                    match &b.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => break 'tco Err(EvalError::Type(format!(
                                                    "let: binding name must be symbol at {}",
                                                    pair[0].pos.fmt()
                                                ))),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => break 'tco Err(EvalError::Type(format!(
                                            "let: invalid binding at {}",
                                            b.pos.fmt()
                                        ))),
                                    }
                                }
                                let body = Rc::new(args[2..].to_vec());
                                let new_env = Env::with_parent(&cur_env);
                                for (p, v) in params.iter().zip(init_vals.into_iter()) {
                                    new_env.set(p.clone(), v);
                                }
                                let lambda = Value::Lambda {
                                    params: Rc::new(params),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: cur_env.clone(),
                                };
                                new_env.set(loop_name.clone(), lambda);
                                for e in &body[..body.len() - 1] {
                                    eval(e, &new_env, out)?;
                                }
                                cur_expr = body.last().unwrap().clone();
                                cur_env = new_env;
                                    continue 'tco;
                            }
                            // Regular let
                            let bindings = match &args[0].kind {
                                ExprKind::List(bs) => bs,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "let: bindings must be a list at {}",
                                    pos.fmt()
                                ))),
                            };
                            let local_env = Env::with_parent(&cur_env);
                            for b in bindings {
                                match &b.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => break 'tco Err(EvalError::Type(format!(
                                                "let: binding name must be symbol at {}",
                                                pair[0].pos.fmt()
                                            ))),
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        local_env.set(bname, val);
                                    }
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "let: invalid binding at {}",
                                        b.pos.fmt()
                                    ))),
                                }
                            }
                            let body = &args[1..];
                            for e in &body[..body.len() - 1] {
                                eval(e, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                        "begin" => {
                            let args = &items[1..];
                            if args.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &args[..args.len() - 1] {
                                eval(e, &cur_env, out)?;
                            }
                            cur_expr = args.last().unwrap().clone();
                            continue 'tco;
                        }
                        "cond" => {
                            let clauses = &items[1..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(
                                            &parts[0].kind,
                                            ExprKind::Symbol(ref s) if s == "else"
                                        );
                                        if is_else
                                            || eval(&parts[0], &cur_env, out)?.is_truthy()
                                        {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                    }
                                    _ => {
                                        break 'tco Err(EvalError::Type(format!(
                                            "cond: invalid clause at {}",
                                            clause.pos.fmt()
                                        )));
                                    }
                                }
                            }
                            if found {
                                continue 'tco;
                            }
                            break 'tco Ok(Value::Nil);
                        }
                        "set!" => {
                            if items.len() != 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "set! requires 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let name = match &items[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "set!: first argument must be a symbol at {}",
                                    pos.fmt()
                                ))),
                            };
                            let val = eval(&items[2], &cur_env, out)?;
                            if !cur_env.set_existing(&name, val) {
                                break 'tco Err(EvalError::UnboundVariable(format!(
                                    "{} at {}",
                                    name,
                                    pos.fmt()
                                )));
                            }
                            break 'tco Ok(Value::Nil);
                        }
                        "string-set!" => {
                            break 'tco Err(EvalError::Type(format!(
                                "string-set!: strings are immutable at {}",
                                pos.fmt()
                            )));
                        }
                        "letrec" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "letrec requires bindings and body at {}",
                                    pos.fmt()
                                )));
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(bs) => bs,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "letrec: bindings must be a list at {}",
                                    pos.fmt()
                                ))),
                            };
                            let local_env = Env::with_parent(&cur_env);
                            // First, bind all names to Nil
                            let mut names = Vec::new();
                            let mut inits = Vec::new();
                            for b in bindings {
                                match &b.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => break 'tco Err(EvalError::Type(format!(
                                                "letrec: binding name must be symbol at {}",
                                                pair[0].pos.fmt()
                                            ))),
                                        };
                                        local_env.set(bname.clone(), Value::Nil);
                                        names.push(bname);
                                        inits.push(&pair[1]);
                                    }
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "letrec: invalid binding at {}",
                                        b.pos.fmt()
                                    ))),
                                }
                            }
                            // Evaluate inits in the local env (all names visible)
                            for (name, init_expr) in names.iter().zip(inits.iter()) {
                                let val = eval(init_expr, &local_env, out)?;
                                local_env.set(name.clone(), val);
                            }
                            let body = &args[1..];
                            for e in &body[..body.len() - 1] {
                                eval(e, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                        "letrec*" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "letrec* requires bindings and body at {}",
                                    pos.fmt()
                                )));
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(bs) => bs,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "letrec*: bindings must be a list at {}",
                                    pos.fmt()
                                ))),
                            };
                            let local_env = Env::with_parent(&cur_env);
                            for b in bindings {
                                match &b.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => break 'tco Err(EvalError::Type(format!(
                                                "letrec*: binding name must be symbol at {}",
                                                pair[0].pos.fmt()
                                            ))),
                                        };
                                        let val = eval(&pair[1], &local_env, out)?;
                                        local_env.set(bname, val);
                                    }
                                    _ => break 'tco Err(EvalError::Type(format!(
                                        "letrec*: invalid binding at {}",
                                        b.pos.fmt()
                                    ))),
                                }
                            }
                            let body = &args[1..];
                            for e in &body[..body.len() - 1] {
                                eval(e, &local_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = local_env;
                            continue 'tco;
                        }
                        "case" => {
                            if items.len() < 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "case requires key and clauses at {}",
                                    pos.fmt()
                                )));
                            }
                            let key = eval(&items[1], &cur_env, out)?;
                            let clauses = &items[2..];
                            let mut found = false;
                            for clause in clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let is_else = matches!(
                                            &parts[0].kind,
                                            ExprKind::Symbol(ref s) if s == "else"
                                        );
                                        if is_else {
                                            for e in &parts[1..parts.len() - 1] {
                                                eval(e, &cur_env, out)?;
                                            }
                                            cur_expr = parts.last().unwrap().clone();
                                            found = true;
                                            break;
                                        }
                                        // datums list
                                        if let ExprKind::List(datums) = &parts[0].kind {
                                            let mut matched = false;
                                            for datum in datums {
                                                let dval = expr_to_datum(datum);
                                                if values_eqv(&key, &dval) {
                                                    matched = true;
                                                    break;
                                                }
                                            }
                                            if matched {
                                                for e in &parts[1..parts.len() - 1] {
                                                    eval(e, &cur_env, out)?;
                                                }
                                                cur_expr = parts.last().unwrap().clone();
                                                found = true;
                                                break;
                                            }
                                        }
                                    }
                                    _ => {
                                        break 'tco Err(EvalError::Type(format!(
                                            "case: invalid clause at {}",
                                            clause.pos.fmt()
                                        )));
                                    }
                                }
                            }
                            if found {
                                continue 'tco;
                            }
                            break 'tco Ok(Value::Nil);
                        }
                        "raise" => {
                            if items.len() != 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "raise requires 1 argument at {}",
                                    pos.fmt()
                                )));
                            }
                            let val = eval(&items[1], &cur_env, out)?;
                            RAISED_VALUE.with(|rv| *rv.borrow_mut() = Some(val));
                            break 'tco Err(EvalError::Raised);
                        }
                        "guard" => {
                            // (guard (var clause ...) body ...)
                            if items.len() < 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "guard requires at least 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let clauses_expr = match &items[1].kind {
                                ExprKind::List(parts) => parts,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "guard: first argument must be a list at {}",
                                    pos.fmt()
                                ))),
                            };
                            if clauses_expr.is_empty() {
                                break 'tco Err(EvalError::Type(format!(
                                    "guard: missing variable at {}",
                                    pos.fmt()
                                )));
                            }
                            let var_name = match &clauses_expr[0].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "guard: expected symbol at {}",
                                    pos.fmt()
                                ))),
                            };
                            let clauses = &clauses_expr[1..];
                            // Evaluate body expressions
                            let mut body_result = Ok(Value::Nil);
                            for body_expr in &items[2..] {
                                body_result = eval(body_expr, &cur_env, out);
                                if body_result.is_err() {
                                    break;
                                }
                            }
                            match body_result {
                                Ok(val) => break 'tco Ok(val),
                                Err(EvalError::Raised) => {
                                    let exn = RAISED_VALUE.with(|rv| rv.borrow_mut().take())
                                        .unwrap_or(Value::Nil);
                                    // Bind exception to var and test clauses
                                    let guard_env = Env::with_parent(&cur_env);
                                    guard_env.set(var_name.clone(), exn);
                                    let mut matched = false;
                                    let mut result = Value::Nil;
                                    for clause in clauses {
                                        match &clause.kind {
                                            ExprKind::List(parts) if !parts.is_empty() => {
                                                if let ExprKind::Symbol(ref s) = parts[0].kind {
                                                    if s == "else" {
                                                        // else clause
                                                        for p in &parts[1..] {
                                                            result = eval(p, &guard_env, out)?;
                                                        }
                                                        matched = true;
                                                        break;
                                                    }
                                                }
                                                let test = eval(&parts[0], &guard_env, out)?;
                                                if test.is_truthy() {
                                                    if parts.len() > 1 {
                                                        for p in &parts[1..] {
                                                            result = eval(p, &guard_env, out)?;
                                                        }
                                                    } else {
                                                        result = test;
                                                    }
                                                    matched = true;
                                                    break;
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                    if matched {
                                        break 'tco Ok(result);
                                    } else {
                                        // Re-raise
                                        let exn = guard_env.get(&var_name).unwrap_or(Value::Nil);
                                        RAISED_VALUE.with(|rv| *rv.borrow_mut() = Some(exn));
                                        break 'tco Err(EvalError::Raised);
                                    }
                                }
                                Err(e) => break 'tco Err(e),
                            }
                        }
                        "with-exception-handler" => {
                            if items.len() != 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "with-exception-handler requires 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let handler = eval(&items[1], &cur_env, out)?;
                            let thunk = eval(&items[2], &cur_env, out)?;
                            let result = call_thunk(&thunk, pos, &cur_env, out);
                            match result {
                                Ok(val) => break 'tco Ok(val),
                                Err(EvalError::Raised) => {
                                    let exn = RAISED_VALUE.with(|rv| rv.borrow_mut().take())
                                        .unwrap_or(Value::Nil);
                                    break 'tco apply_func(handler, vec![exn], pos, &cur_env, out);
                                }
                                Err(e) => break 'tco Err(e),
                            }
                        }
                        "dynamic-wind" => {
                            if items.len() != 4 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "dynamic-wind requires 3 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let in_thunk = eval(&items[1], &cur_env, out)?;
                            let body_thunk = eval(&items[2], &cur_env, out)?;
                            let out_thunk = eval(&items[3], &cur_env, out)?;
                            // Call in-thunk
                            call_thunk(&in_thunk, pos, &cur_env, out)?;
                            // Call body-thunk, catching ContinuationReturn and Raised
                            let body_result = call_thunk(&body_thunk, pos, &cur_env, out);
                            match body_result {
                                Ok(val) => {
                                    // Normal exit: call out-thunk, return body value
                                    call_thunk(&out_thunk, pos, &cur_env, out)?;
                                    break 'tco Ok(val);
                                }
                                Err(EvalError::ContinuationReturn) => {
                                    // Non-local exit: call out-thunk, then re-throw
                                    call_thunk(&out_thunk, pos, &cur_env, out)?;
                                    break 'tco Err(EvalError::ContinuationReturn);
                                }
                                Err(EvalError::Raised) => {
                                    // Exception: call out-thunk, then re-raise
                                    let saved = RAISED_VALUE.with(|rv| rv.borrow_mut().take());
                                    call_thunk(&out_thunk, pos, &cur_env, out)?;
                                    RAISED_VALUE.with(|rv| *rv.borrow_mut() = saved);
                                    break 'tco Err(EvalError::Raised);
                                }
                                Err(e) => break 'tco Err(e),
                            }
                        }
                        "call/cc" | "call-with-current-continuation" => {
                            if items.len() != 2 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "call/cc requires 1 argument at {}",
                                    pos.fmt()
                                )));
                            }
                            break 'tco do_callcc(&items[1], pos, &cur_env, out);
                        }
                        "define-syntax" => {
                            if items.len() != 3 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "define-syntax requires 2 arguments at {}",
                                    pos.fmt()
                                )));
                            }
                            let macro_name = match &items[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "define-syntax: name must be a symbol at {}",
                                    pos.fmt()
                                ))),
                            };
                            let sr = match &items[2].kind {
                                ExprKind::List(parts) => parts,
                                _ => break 'tco Err(EvalError::Type(format!(
                                    "define-syntax: expected syntax-rules at {}",
                                    pos.fmt()
                                ))),
                            };
                            let literals = Rc::new(match &sr[1].kind {
                                ExprKind::List(lits) => lits.iter().filter_map(|e| {
                                    if let ExprKind::Symbol(s) = &e.kind { Some(s.clone()) } else { None }
                                }).collect(),
                                _ => vec![],
                            });
                            let rules = Rc::new(sr[2..].iter().filter_map(|clause| {
                                if let ExprKind::List(parts) = &clause.kind {
                                    if parts.len() == 2 {
                                        return Some((parts[0].clone(), parts[1].clone()));
                                    }
                                }
                                None
                            }).collect());
                            let macro_val = Value::Macro { literals, rules, def_env: cur_env.clone() };
                            cur_env.set(macro_name, macro_val);
                            break 'tco Ok(Value::Nil);
                        }
                        "define-record-type" => {
                            break 'tco eval_define_record_type(&items[1..], pos, &cur_env);
                        }
                        _ => {
                            if let Some(Value::Macro { ref literals, ref rules, ref def_env }) = cur_env.get(op) {
                                let expanded = expand_macro(&items, literals, rules, def_env, &cur_env, pos)?;
                                cur_expr = expanded;
                                continue 'tco;
                            }
                        }
                    }
                }
                // Function application
                let func_name = match &items[0].kind {
                    ExprKind::Symbol(name) => Some(name.clone()),
                    _ => None,
                };
                let args: Result<Vec<Value>, _> =
                    items[1..].iter().map(|a| eval(a, &cur_env, out)).collect();
                let args = args?;
                if let Some(ref name) = func_name {
                    if name == "apply" {
                        break 'tco call_apply(&args, pos, &cur_env, out);
                    }
                    if name == "call-with-values" {
                        break 'tco call_with_values(&args, pos, &cur_env, out);
                    }
                    if let Some(result) = apply_builtin(name, &args, pos, out)? {
                        break 'tco Ok(result);
                    }
                }
                let func = if let Some(ref name) = func_name {
                    if let Some(val) = cur_env.get(name) {
                        val
                    } else if Value::is_builtin_name(name) {
                        Value::Builtin(name.clone())
                    } else {
                        break 'tco Err(EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt())));
                    }
                } else {
                    eval(&items[0], &cur_env, out)?
                };
                match func {
                    Value::Continuation(cc_pos, top_idx) => {
                        if args.len() != 1 {
                            break 'tco Err(EvalError::Arity(format!(
                                "continuation requires 1 argument at {}",
                                pos.fmt()
                            )));
                        }
                        CONT_RETURN.with(|cr| {
                            *cr.borrow_mut() = Some((cc_pos, top_idx, args.into_iter().next().unwrap()));
                        });
                        break 'tco Err(EvalError::ContinuationReturn);
                    }
                    Value::Builtin(ref bname) => {
                        if bname == "call/cc" {
                            if args.len() != 1 {
                                break 'tco Err(EvalError::Arity(format!(
                                    "call/cc requires 1 argument at {}",
                                    pos.fmt()
                                )));
                            }
                            let pending = CALLCC_PENDING.with(|p| {
                                let mut p = p.borrow_mut();
                                if let Some((pending_pos, _)) = p.as_ref() {
                                    if *pending_pos == pos {
                                        return p.take().map(|(_, v)| v);
                                    }
                                }
                                None
                            });
                            if let Some(val) = pending {
                                break 'tco Ok(val);
                            }
                            let start_idx = CALLCC_TOP_IDX.with(|c| c.get());
                            let k = Value::Continuation(pos, start_idx);
                            let proc = args.into_iter().next().unwrap();
                            break 'tco apply_func(proc, vec![k], pos, &cur_env, out);
                        }
                        if bname == "apply" {
                            break 'tco call_apply(&args, pos, &cur_env, out);
                        }
                        if bname == "call-with-values" {
                            break 'tco call_with_values(&args, pos, &cur_env, out);
                        }
                        match apply_builtin(bname, &args, pos, out)? {
                            Some(result) => break 'tco Ok(result),
                            None => break 'tco Err(EvalError::Type(format!(
                                "unknown builtin {} at {}",
                                bname,
                                pos.fmt()
                            ))),
                        }
                    }
                    Value::Lambda {
                        params,
                        rest_param,
                        body,
                        env: closure_env,
                    } => {
                        if let Some(ref rp) = rest_param {
                            if args.len() < params.len() {
                                break 'tco Err(EvalError::Arity(format!(
                                    "expected at least {} arguments, got {} at {}",
                                    params.len(),
                                    args.len(),
                                    pos.fmt()
                                )));
                            }
                            let new_env = Env::with_parent(&closure_env);
                            cur_env.copy_all_into_if_absent(&new_env);
                            for (p, a) in params.iter().zip(args.iter()) {
                                new_env.set(p.clone(), a.clone());
                            }
                            // Build rest list from excess args
                            let mut rest = Value::Nil;
                            for a in args[params.len()..].iter().rev() {
                                rest = make_pair(a.clone(), rest);
                            }
                            new_env.set(rp.clone(), rest);
                            if body.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &body[..body.len() - 1] {
                                eval(e, &new_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = new_env;
                            continue 'tco;
                        } else {
                            if params.len() != args.len() {
                                break 'tco Err(EvalError::Arity(format!(
                                    "expected {} arguments, got {} at {}",
                                    params.len(),
                                    args.len(),
                                    pos.fmt()
                                )));
                            }
                            let new_env = Env::with_parent(&closure_env);
                            cur_env.copy_all_into_if_absent(&new_env);
                            for (p, a) in params.iter().zip(args.into_iter()) {
                                new_env.set(p.clone(), a);
                            }
                            if body.is_empty() {
                                break 'tco Ok(Value::Nil);
                            }
                            for e in &body[..body.len() - 1] {
                                eval(e, &new_env, out)?;
                            }
                            cur_expr = body.last().unwrap().clone();
                            cur_env = new_env;
                            continue 'tco;
                        }
                    }
                    _ => {
                        break 'tco Err(EvalError::Type(format!(
                            "not a procedure at {}",
                            pos.fmt()
                        )));
                    }
                }
            }
        }
    };
    result
}

fn eval_define(args: &[Expr], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!(
            "define requires arguments at {}",
            pos.fmt()
        )));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "define requires a value at {}",
                    pos.fmt()
                )));
            }
            let val = eval(&args[1], env, out)?;
            env.set(name.clone(), val);
            Ok(Value::Nil)
        }
        ExprKind::List(parts) => {
            if parts.is_empty() {
                return Err(EvalError::Parse(format!(
                    "define: empty name list at {}",
                    pos.fmt()
                )));
            }
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Type(format!(
                        "define: name must be symbol at {}",
                        pos.fmt()
                    )))
                }
            };
            let (params, rest_param) = parse_params(&parts[1..], pos)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params: Rc::new(params),
                rest_param,
                body: Rc::new(body),
                env: env.clone(),
            };
            env.set(name.clone(), lambda);
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type(format!(
            "define: first argument must be symbol or list at {}",
            pos.fmt()
        ))),
    }
}

/// Implements (define-record-type <name> (constructor f1 f2 ...) predicate (f1 acc1) (f2 acc2) ...)
fn eval_define_record_type(args: &[Expr], pos: Pos, env: &Env) -> Result<Value, EvalError> {
    // args[0] = <record-name>
    // args[1] = (constructor-name field1 field2 ...)
    // args[2] = predicate-name
    // args[3..] = (field accessor) ...
    if args.len() < 3 {
        return Err(EvalError::Arity(format!(
            "define-record-type requires at least 3 arguments at {}",
            pos.fmt()
        )));
    }

    let _type_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-record-type: expected type name symbol at {}",
            pos.fmt()
        ))),
    };
    // Strip angle brackets for display name
    let display_name = _type_name.trim_matches(|c| c == '<' || c == '>').to_string();

    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Parse constructor
    let (ctor_name, ctor_fields) = match &args[1].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let name = match &parts[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type(format!(
                    "define-record-type: constructor name must be symbol at {}",
                    pos.fmt()
                ))),
            };
            let fields: Vec<String> = parts[1..].iter().map(|e| {
                match &e.kind {
                    ExprKind::Symbol(s) => Ok(s.clone()),
                    _ => Err(EvalError::Type(format!(
                        "define-record-type: field name must be symbol at {}",
                        pos.fmt()
                    ))),
                }
            }).collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Type(format!(
            "define-record-type: expected constructor spec at {}",
            pos.fmt()
        ))),
    };

    // Parse predicate name
    let pred_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!(
            "define-record-type: predicate must be symbol at {}",
            pos.fmt()
        ))),
    };

    // Parse field accessors
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let field = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(format!(
                        "define-record-type: field spec must start with symbol at {}",
                        pos.fmt()
                    ))),
                };
                let accessor = match &parts[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(format!(
                        "define-record-type: accessor must be symbol at {}",
                        pos.fmt()
                    ))),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Type(format!(
                "define-record-type: expected field spec at {}",
                pos.fmt()
            ))),
        }
    }

    // Build field index map: field_name -> index in constructor
    let field_index: HashMap<String, usize> = ctor_fields.iter().enumerate()
        .map(|(i, f)| (f.clone(), i))
        .collect();

    // Define constructor: a lambda that creates a Record value
    let num_fields = ctor_fields.len();

    let ctor_builtin_name = format!("##record-ctor-{}", type_id);
    let pred_builtin_name = format!("##record-pred-{}", type_id);

    // Store record type info in a thread-local registry
    RECORD_TYPES.with(|rt| {
        rt.borrow_mut().insert(type_id, RecordTypeInfo {
            name: display_name.clone(),
            num_fields,
        });
    });

    // Constructor: lambda that captures type_id and creates a record
    env.set(ctor_name, Value::Builtin(ctor_builtin_name));

    // Predicate
    env.set(pred_name, Value::Builtin(pred_builtin_name));

    // Accessors
    for (field, accessor_name) in &field_accessors {
        let idx = field_index.get(field).ok_or_else(|| {
            EvalError::Type(format!(
                "define-record-type: unknown field {} at {}",
                field, pos.fmt()
            ))
        })?;
        let acc_builtin_name = format!("##record-acc-{}-{}", type_id, idx);
        env.set(accessor_name.clone(), Value::Builtin(acc_builtin_name));
    }

    Ok(Value::Nil)
}

struct RecordTypeInfo {
    name: String,
    num_fields: usize,
}

thread_local! {
    static RECORD_TYPES: RefCell<HashMap<usize, RecordTypeInfo>> = RefCell::new(HashMap::new());
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => Value::Rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = make_pair(expr_to_value(item), result);
            }
            result
        }
    }
}

fn eval_quote(args: &[Expr], pos: Pos) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!(
            "quote requires 1 argument at {}",
            pos.fmt()
        )));
    }
    Ok(expr_to_value(&args[0]))
}

fn eval_lambda(args: &[Expr], pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "lambda requires params and body at {}",
            pos.fmt()
        )));
    }
    match &args[0].kind {
        ExprKind::List(parts) => {
            let (params, rest_param) = parse_params(parts, pos)?;
            Ok(Value::Lambda {
                params: Rc::new(params),
                rest_param,
                body: Rc::new(args[1..].to_vec()),
                env: env.clone(),
            })
        }
        ExprKind::Symbol(s) => {
            // (lambda args body) — all args collected into rest
            Ok(Value::Lambda {
                params: Rc::new(vec![]),
                rest_param: Some(s.clone()),
                body: Rc::new(args[1..].to_vec()),
                env: env.clone(),
            })
        }
        _ => Err(EvalError::Type(format!(
            "lambda: params must be a list or symbol at {}",
            pos.fmt()
        ))),
    }
}

/// Call a zero-argument thunk (used by dynamic-wind).
fn call_thunk(thunk: &Value, pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    match thunk {
        Value::Lambda { params, rest_param, body, env: closure_env } => {
            if !params.is_empty() || rest_param.is_some() {
                return Err(EvalError::Type(format!(
                    "dynamic-wind: thunk must accept 0 arguments at {}",
                    pos.fmt()
                )));
            }
            let new_env = Env::with_parent(closure_env);
            env.copy_all_into_if_absent(&new_env);
            if body.is_empty() {
                return Ok(Value::Nil);
            }
            for e in &body[..body.len() - 1] {
                eval(e, &new_env, out)?;
            }
            eval(body.last().unwrap(), &new_env, out)
        }
        _ => Err(EvalError::Type(format!(
            "dynamic-wind: expected procedure at {}",
            pos.fmt()
        ))),
    }
}

/// Perform call/cc: check for pending return, otherwise create continuation and call proc.
fn do_callcc(proc_expr: &Expr, pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    let pending = CALLCC_PENDING.with(|p| {
        let mut p = p.borrow_mut();
        if let Some((pending_pos, _)) = p.as_ref() {
            if *pending_pos == pos {
                return p.take().map(|(_, v)| v);
            }
        }
        None
    });
    if let Some(val) = pending {
        return Ok(val);
    }
    let proc = eval(proc_expr, env, out)?;
    let start_idx = CALLCC_TOP_IDX.with(|c| c.get());
    let k = Value::Continuation(pos, start_idx);
    apply_func(proc, vec![k], pos, env, out)
}

/// Apply a function value to arguments (non-TCO, used by call/cc).
fn apply_func(func: Value, args: Vec<Value>, pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            let new_env = Env::with_parent(&closure_env);
            env.copy_all_into_if_absent(&new_env);
            if let Some(ref rp) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.set(p.clone(), a.clone());
                }
                let mut rest = Value::Nil;
                for a in args[params.len()..].iter().rev() {
                    rest = make_pair(a.clone(), rest);
                }
                new_env.set(rp.clone(), rest);
            } else {
                if params.len() != args.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {} at {}",
                        params.len(),
                        args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(args.into_iter()) {
                    new_env.set(p.clone(), a);
                }
            }
            if body.is_empty() {
                return Ok(Value::Nil);
            }
            for e in &body[..body.len() - 1] {
                eval(e, &new_env, out)?;
            }
            eval(body.last().unwrap(), &new_env, out)
        }
        Value::Continuation(cc_pos, top_idx) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            CONT_RETURN.with(|cr| {
                *cr.borrow_mut() = Some((cc_pos, top_idx, args.into_iter().next().unwrap()));
            });
            Err(EvalError::ContinuationReturn)
        }
        Value::Builtin(ref bname) => {
            if bname == "apply" {
                return call_apply(&args, pos, env, out);
            }
            if bname == "call-with-values" {
                return call_with_values(&args, pos, env, out);
            }
            match apply_builtin(bname, &args, pos, out)? {
                Some(result) => Ok(result),
                None => Err(EvalError::Type(format!(
                    "unknown builtin {} at {}",
                    bname,
                    pos.fmt()
                ))),
            }
        }
        _ => Err(EvalError::Type(format!(
            "call/cc: argument must be a procedure at {}",
            pos.fmt()
        ))),
    }
}

/// Parse parameter list, handling dot notation for rest params.
/// e.g. [x, y, ., rest] -> (vec!["x", "y"], Some("rest"))
fn parse_params(parts: &[Expr], pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < parts.len() {
        match &parts[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= parts.len() {
                    return Err(EvalError::Parse(format!(
                        "expected rest parameter after . at {}",
                        pos.fmt()
                    )));
                }
                match &parts[i + 1].kind {
                    ExprKind::Symbol(rp) => rest_param = Some(rp.clone()),
                    _ => {
                        return Err(EvalError::Type(format!(
                            "rest parameter must be symbol at {}",
                            parts[i + 1].pos.fmt()
                        )))
                    }
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "parameter must be symbol at {}",
                    parts[i].pos.fmt()
                )))
            }
        }
    }
    Ok((params, rest_param))
}

/// Convert a Value list to a Vec<Value>.
fn value_list_to_vec(val: &Value, pos: Pos) -> Result<Vec<Value>, EvalError> {
    let mut result = Vec::new();
    let mut cur = val.clone();
    loop {
        match &cur {
            Value::Nil => return Ok(result),
            Value::Pair(cell) => {
                let (car, cdr) = {
                    let inner = cell.borrow();
                    (inner.0.clone(), inner.1.clone())
                };
                result.push(car);
                cur = cdr;
            }
            _ => {
                return Err(EvalError::Type(format!(
                    "apply: last argument must be a proper list at {}",
                    pos.fmt()
                )))
            }
        }
    }
}

/// Implement (apply fn arg1 ... argN list)
fn call_with_values(args: &[Value], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "call-with-values requires 2 arguments at {}",
            pos.fmt()
        )));
    }
    let producer = args[0].clone();
    let consumer = args[1].clone();
    let produced = apply_func(producer, vec![], pos, env, out)?;
    let consumer_args = match produced {
        Value::Values(vals) => vals,
        single => vec![single],
    };
    apply_func(consumer, consumer_args, pos, env, out)
}

fn call_apply(args: &[Value], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "apply requires at least 2 arguments at {}",
            pos.fmt()
        )));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();
    let tail = value_list_to_vec(last, pos)?;
    call_args.extend(tail);

    match func {
        Value::Continuation(cc_pos, top_idx) => {
            if call_args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            CONT_RETURN.with(|cr| {
                *cr.borrow_mut() = Some((*cc_pos, *top_idx, call_args.into_iter().next().unwrap()));
            });
            Err(EvalError::ContinuationReturn)
        }
        Value::Builtin(bname) => {
            if bname == "apply" {
                return call_apply(&call_args, pos, env, out);
            }
            if bname == "call-with-values" {
                return call_with_values(&call_args, pos, env, out);
            }
            match apply_builtin(bname, &call_args, pos, out)? {
                Some(result) => Ok(result),
                None => Err(EvalError::Type(format!(
                    "unknown builtin {} at {}",
                    bname,
                    pos.fmt()
                ))),
            }
        }
        Value::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            let new_env = Env::with_parent(closure_env);
            env.copy_all_into_if_absent(&new_env);
            if let Some(ref rp) = rest_param {
                if call_args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {} at {}",
                        params.len(),
                        call_args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(call_args.iter()) {
                    new_env.set(p.clone(), a.clone());
                }
                let mut rest = Value::Nil;
                for a in call_args[params.len()..].iter().rev() {
                    rest = make_pair(a.clone(), rest);
                }
                new_env.set(rp.clone(), rest);
            } else {
                if params.len() != call_args.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {} at {}",
                        params.len(),
                        call_args.len(),
                        pos.fmt()
                    )));
                }
                for (p, a) in params.iter().zip(call_args.into_iter()) {
                    new_env.set(p.clone(), a);
                }
            }
            if body.is_empty() {
                return Ok(Value::Nil);
            }
            for e in &body[..body.len() - 1] {
                eval(e, &new_env, out)?;
            }
            eval(body.last().unwrap(), &new_env, out)
        }
        _ => Err(EvalError::Type(format!(
            "apply: first argument must be a procedure at {}",
            pos.fmt()
        ))),
    }
}

fn eval_string_set(args: &[Expr], pos: Pos, env: &Env, out: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!(
            "string-set! requires 3 arguments at {}",
            pos.fmt()
        )));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => {
            return Err(EvalError::Type(format!(
                "string-set!: first argument must be a variable at {}",
                pos.fmt()
            )))
        }
    };
    let idx = eval(&args[1], env, out)?.as_integer(pos)? as usize;
    let ch = match eval(&args[2], env, out)? {
        Value::Char(c) => c,
        _ => {
            return Err(EvalError::Type(format!(
                "string-set!: third argument must be a character at {}",
                pos.fmt()
            )))
        }
    };
    let val = env.get(&name).ok_or_else(|| {
        EvalError::UnboundVariable(format!("{} at {}", name, pos.fmt()))
    })?;
    match val {
        Value::Str(s) => {
            let mut chars: Vec<char> = s.chars().collect();
            chars[idx] = ch;
            let new_s: String = chars.into_iter().collect();
            env.set_existing(&name, Value::Str(new_s));
            Ok(Value::Nil)
        }
        _ => Err(EvalError::Type(format!(
            "string-set!: expected string at {}",
            pos.fmt()
        ))),
    }
}

fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

fn expr_to_datum(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => Value::Rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                result = make_pair(expr_to_datum(item), result);
            }
            result
        }
    }
}

fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Pair(ac), Value::Pair(bc)) => {
            let (a1, a2) = { let i = ac.borrow(); (i.0.clone(), i.1.clone()) };
            let (b1, b2) = { let i = bc.borrow(); (i.0.clone(), i.1.clone()) };
            values_equal(&a1, &b1) && values_equal(&a2, &b2)
        }
        (Value::Vector(va), Value::Vector(vb)) => {
            let va = va.borrow();
            let vb = vb.borrow();
            va.len() == vb.len() && va.iter().zip(vb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => values_eqv(a, b),
    }
}

fn apply_builtin(op: &str, args: &[Value], pos: Pos, out: &mut String) -> Result<Option<Value>, EvalError> {
    match op {
        "+" => {
            if any_inexact(args) {
                let mut sum = 0.0f64;
                for a in args { sum += value_to_f64(a, pos)?; }
                Ok(Some(Value::Float(sum)))
            } else {
                exact_add(args, pos).map(Some)
            }
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "- requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            if any_inexact(args) {
                if args.len() == 1 {
                    return Ok(Some(Value::Float(-value_to_f64(&args[0], pos)?)));
                }
                let mut result = value_to_f64(&args[0], pos)?;
                for a in &args[1..] { result -= value_to_f64(a, pos)?; }
                Ok(Some(Value::Float(result)))
            } else {
                exact_sub(args, pos).map(Some)
            }
        }
        "*" => {
            if any_inexact(args) {
                let mut product = 1.0f64;
                for a in args { product *= value_to_f64(a, pos)?; }
                Ok(Some(Value::Float(product)))
            } else {
                exact_mul(args, pos).map(Some)
            }
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "/ requires at least 1 argument at {}",
                    pos.fmt()
                )));
            }
            if any_inexact(args) {
                let mut result = value_to_f64(&args[0], pos)?;
                for a in &args[1..] {
                    let d = value_to_f64(a, pos)?;
                    if d == 0.0 {
                        return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
                    }
                    result /= d;
                }
                Ok(Some(Value::Float(result)))
            } else {
                exact_div(args, pos).map(Some)
            }
        }
        "<" => compare_op_val(args, |a, b| a < b, pos),
        ">" => compare_op_val(args, |a, b| a > b, pos),
        "=" => compare_op_val(args, |a, b| a == b, pos),
        "<=" => compare_op_val(args, |a, b| a <= b, pos),
        ">=" => compare_op_val(args, |a, b| a >= b, pos),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "not requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(!args[0].is_truthy())))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "cons requires 2 arguments at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(make_pair(args[0].clone(), args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "car requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(cell) => Ok(Some(cell.borrow().0.clone())),
                _ => Err(EvalError::Type(format!("car: not a pair at {}", pos.fmt()))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "cdr requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Pair(cell) => Ok(Some(cell.borrow().1.clone())),
                _ => Err(EvalError::Type(format!("cdr: not a pair at {}", pos.fmt()))),
            }
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("set-car! requires 2 arguments at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().0 = args[1].clone();
                    Ok(Some(Value::Nil))
                }
                _ => Err(EvalError::Type(format!("set-car!: not a pair at {}", pos.fmt()))),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("set-cdr! requires 2 arguments at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    cell.borrow_mut().1 = args[1].clone();
                    Ok(Some(Value::Nil))
                }
                _ => Err(EvalError::Type(format!("set-cdr!: not a pair at {}", pos.fmt()))),
            }
        }
        "cadr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cadr requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    let cdr = cell.borrow().1.clone();
                    match &cdr {
                        Value::Pair(cell2) => Ok(Some(cell2.borrow().0.clone())),
                        _ => Err(EvalError::Type(format!("cadr: not a pair at {}", pos.fmt()))),
                    }
                }
                _ => Err(EvalError::Type(format!("cadr: not a pair at {}", pos.fmt()))),
            }
        }
        "cddr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cddr requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    let cdr = cell.borrow().1.clone();
                    match &cdr {
                        Value::Pair(cell2) => Ok(Some(cell2.borrow().1.clone())),
                        _ => Err(EvalError::Type(format!("cddr: not a pair at {}", pos.fmt()))),
                    }
                }
                _ => Err(EvalError::Type(format!("cddr: not a pair at {}", pos.fmt()))),
            }
        }
        "caar" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("caar requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    let car = cell.borrow().0.clone();
                    match &car {
                        Value::Pair(cell2) => Ok(Some(cell2.borrow().0.clone())),
                        _ => Err(EvalError::Type(format!("caar: not a pair at {}", pos.fmt()))),
                    }
                }
                _ => Err(EvalError::Type(format!("caar: not a pair at {}", pos.fmt()))),
            }
        }
        "cdar" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("cdar requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Pair(cell) => {
                    let car = cell.borrow().0.clone();
                    match &car {
                        Value::Pair(cell2) => Ok(Some(cell2.borrow().1.clone())),
                        _ => Err(EvalError::Type(format!("cdar: not a pair at {}", pos.fmt()))),
                    }
                }
                _ => Err(EvalError::Type(format!("cdar: not a pair at {}", pos.fmt()))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "null? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Nil))))
        }
        "list" => {
            let mut result = Value::Nil;
            for a in args.iter().rev() {
                result = make_pair(a.clone(), result);
            }
            Ok(Some(result))
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "length requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            let mut count = 0i64;
            let mut cur = args[0].clone();
            loop {
                match cur {
                    Value::Nil => break,
                    Value::Pair(cell) => {
                        count += 1;
                        cur = cell.borrow().1.clone();
                    }
                    _ => {
                        return Err(EvalError::Type(format!(
                            "length: not a proper list at {}",
                            pos.fmt()
                        )))
                    }
                }
            }
            Ok(Some(Value::Integer(count)))
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Str(_)))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "number? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(is_numeric(&args[0]))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "boolean? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Boolean(_)))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "pair? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Pair(_)))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "symbol? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Symbol(_)))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "char? requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Char(_)))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "display requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            out.push_str(&args[0].display_unquoted());
            Ok(Some(Value::Nil))
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "write requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            out.push_str(&args[0].display());
            Ok(Some(Value::Nil))
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!(
                    "newline requires 0 arguments at {}",
                    pos.fmt()
                )));
            }
            out.push('\n');
            Ok(Some(Value::Nil))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!(
                        "string-append: expected string at {}",
                        pos.fmt()
                    ))),
                }
            }
            Ok(Some(Value::Str(result)))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string-length requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Integer(s.chars().count() as i64))),
                _ => Err(EvalError::Type(format!(
                    "string-length: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!(
                    "substring requires 3 arguments at {}",
                    pos.fmt()
                )));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!(
                    "substring: expected string at {}",
                    pos.fmt()
                ))),
            };
            let start = args[1].as_integer(pos)? as usize;
            let end = args[2].as_integer(pos)? as usize;
            let chars: Vec<char> = s.chars().collect();
            Ok(Some(Value::Str(chars[start..end].iter().collect())))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string->number requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    if let Ok(n) = s.parse::<i64>() {
                        Ok(Some(Value::Integer(n)))
                    } else if let Ok(f) = s.parse::<f64>() {
                        Ok(Some(Value::Float(f)))
                    } else {
                        Ok(Some(Value::Boolean(false)))
                    }
                },
                _ => Err(EvalError::Type(format!(
                    "string->number: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "number->string requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            Ok(Some(Value::Str(args[0].display())))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "symbol->string requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Some(Value::Str(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "symbol->string: expected symbol at {}",
                    pos.fmt()
                ))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string->symbol requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Symbol(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "string->symbol: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string-copy requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Str(s.clone()))),
                _ => Err(EvalError::Type(format!(
                    "string-copy: expected string at {}",
                    pos.fmt()
                ))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!(
                    "string-ref requires 2 arguments at {}",
                    pos.fmt()
                )));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!(
                    "string-ref: expected string at {}",
                    pos.fmt()
                ))),
            };
            let idx = args[1].as_integer(pos)? as usize;
            let chars: Vec<char> = s.chars().collect();
            Ok(Some(Value::Char(chars[idx])))
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "string->list requires 1 argument at {}", pos.fmt()
                )));
            }
            match &args[0] {
                Value::Str(s) => {
                    let list = s.chars().rev().fold(Value::Nil, |acc, c| {
                        make_pair(Value::Char(c), acc)
                    });
                    Ok(Some(list))
                }
                _ => Err(EvalError::Type(format!(
                    "string->list: expected string at {}", pos.fmt()
                ))),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "list->string requires 1 argument at {}", pos.fmt()
                )));
            }
            let mut chars = String::new();
            let mut cur = args[0].clone();
            loop {
                match &cur {
                    Value::Pair(cell) => {
                        let (car, cdr) = {
                            let inner = cell.borrow();
                            (inner.0.clone(), inner.1.clone())
                        };
                        match car {
                            Value::Char(c) => chars.push(c),
                            _ => return Err(EvalError::Type(format!(
                                "list->string: expected character in list at {}", pos.fmt()
                            ))),
                        }
                        cur = cdr;
                    }
                    Value::Nil => break,
                    _ => return Err(EvalError::Type(format!(
                        "list->string: expected proper list at {}", pos.fmt()
                    ))),
                }
            }
            Ok(Some(Value::Str(chars)))
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "char->integer requires 1 argument at {}", pos.fmt()
                )));
            }
            match &args[0] {
                Value::Char(c) => Ok(Some(Value::Integer(*c as i64))),
                _ => Err(EvalError::Type(format!(
                    "char->integer: expected character at {}", pos.fmt()
                ))),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "integer->char requires 1 argument at {}", pos.fmt()
                )));
            }
            let n = args[0].as_integer(pos)?;
            Ok(Some(Value::Char(char::from_u32(n as u32).unwrap_or('\u{FFFD}'))))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("abs requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Integer(args[0].as_integer(pos)?.abs())))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("modulo requires 2 arguments at {}", pos.fmt())));
            }
            let a = args[0].as_integer(pos)?;
            let b = args[1].as_integer(pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
            }
            Ok(Some(Value::Integer(((a % b) + b) % b)))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("remainder requires 2 arguments at {}", pos.fmt())));
            }
            let a = args[0].as_integer(pos)?;
            let b = args[1].as_integer(pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
            }
            Ok(Some(Value::Integer(a % b)))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("quotient requires 2 arguments at {}", pos.fmt())));
            }
            let a = args[0].as_integer(pos)?;
            let b = args[1].as_integer(pos)?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(format!("at {}", pos.fmt())));
            }
            Ok(Some(Value::Integer(a / b)))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("min requires at least 1 argument at {}", pos.fmt())));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                let v = a.as_integer(pos)?;
                if v < result { result = v; }
            }
            Ok(Some(Value::Integer(result)))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("max requires at least 1 argument at {}", pos.fmt())));
            }
            let mut result = args[0].as_integer(pos)?;
            for a in &args[1..] {
                let v = a.as_integer(pos)?;
                if v > result { result = v; }
            }
            Ok(Some(Value::Integer(result)))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("expt requires 2 arguments at {}", pos.fmt())));
            }
            let base = args[0].as_integer(pos)?;
            let exp = args[1].as_integer(pos)?;
            if exp < 0 {
                Ok(Some(Value::Integer(0)))
            } else {
                Ok(Some(Value::Integer(base.pow(exp as u32))))
            }
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("zero? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(args[0].as_integer(pos)? == 0)))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("positive? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(args[0].as_integer(pos)? > 0)))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("negative? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(args[0].as_integer(pos)? < 0)))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("odd? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(args[0].as_integer(pos)? % 2 != 0)))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("even? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(args[0].as_integer(pos)? % 2 == 0)))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("list-ref requires 2 arguments at {}", pos.fmt())));
            }
            let idx = args[1].as_integer(pos)? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                let next = match &cur {
                    Value::Pair(cell) => cell.borrow().1.clone(),
                    _ => return Err(EvalError::Type(format!("list-ref: index out of range at {}", pos.fmt()))),
                };
                cur = next;
            }
            match &cur {
                Value::Pair(cell) => Ok(Some(cell.borrow().0.clone())),
                _ => Err(EvalError::Type(format!("list-ref: index out of range at {}", pos.fmt()))),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("list-tail requires 2 arguments at {}", pos.fmt())));
            }
            let idx = args[1].as_integer(pos)? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                let next = match &cur {
                    Value::Pair(cell) => cell.borrow().1.clone(),
                    _ => return Err(EvalError::Type(format!("list-tail: index out of range at {}", pos.fmt()))),
                };
                cur = next;
            }
            Ok(Some(cur))
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("list? requires 1 argument at {}", pos.fmt())));
            }
            // Floyd's cycle detection for list?
            let mut slow = args[0].clone();
            let mut fast = args[0].clone();
            let result = loop {
                // Advance slow by 1
                let next_slow = match &slow {
                    Value::Nil => break true,
                    Value::Pair(cell) => cell.borrow().1.clone(),
                    _ => break false,
                };
                slow = next_slow;
                // Advance fast by 2
                for _ in 0..2 {
                    let next_fast = match &fast {
                        Value::Nil => { fast = Value::Nil; break; }
                        Value::Pair(cell) => cell.borrow().1.clone(),
                        _ => break,
                    };
                    fast = next_fast;
                }
                // Check if slow == fast (cycle)
                if let (Value::Pair(sc), Value::Pair(fc)) = (&slow, &fast) {
                    if Rc::ptr_eq(sc, fc) {
                        break false;
                    }
                }
            };
            Ok(Some(Value::Boolean(result)))
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("reverse requires 1 argument at {}", pos.fmt())));
            }
            let mut cur = args[0].clone();
            let mut result = Value::Nil;
            loop {
                match &cur {
                    Value::Nil => break,
                    Value::Pair(cell) => {
                        let (car, cdr) = {
                            let inner = cell.borrow();
                            (inner.0.clone(), inner.1.clone())
                        };
                        result = make_pair(car, result);
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("reverse: not a proper list at {}", pos.fmt()))),
                }
            }
            Ok(Some(result))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("eq? requires 2 arguments at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(values_eq(&args[0], &args[1]))))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("equal? requires 2 arguments at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(values_equal(&args[0], &args[1]))))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("assoc requires 2 arguments at {}", pos.fmt())));
            }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Value::Nil => return Ok(Some(Value::Boolean(false))),
                    Value::Pair(cell) => {
                        let (car, cdr) = {
                            let inner = cell.borrow();
                            (inner.0.clone(), inner.1.clone())
                        };
                        if let Value::Pair(ref entry_cell) = car {
                            let entry_key = entry_cell.borrow().0.clone();
                            if values_equal(key, &entry_key) {
                                return Ok(Some(car));
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("assoc: not a proper list at {}", pos.fmt()))),
                }
            }
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("map requires at least 2 arguments at {}", pos.fmt())));
            }
            let func = &args[0];
            let mut lists: Vec<Value> = args[1..].to_vec();
            let mut result_items = Vec::new();
            loop {
                // Check if any list is nil (done)
                let mut any_nil = false;
                for l in &lists {
                    if matches!(l, Value::Nil) {
                        any_nil = true;
                        break;
                    }
                }
                if any_nil { break; }
                // Extract car of each list
                let mut call_args = Vec::new();
                let mut new_lists: Vec<Value> = Vec::new();
                for l in &lists {
                    match l {
                        Value::Pair(cell) => {
                            let inner = cell.borrow();
                            call_args.push(inner.0.clone());
                            new_lists.push(inner.1.clone());
                        }
                        _ => return Err(EvalError::Type(format!("map: not a proper list at {}", pos.fmt()))),
                    }
                }
                // Apply function
                let val = match func {
                    Value::Builtin(bname) => {
                        match apply_builtin(bname, &call_args, pos, out)? {
                            Some(v) => v,
                            None => return Err(EvalError::Type(format!("unknown builtin {} at {}", bname, pos.fmt()))),
                        }
                    }
                    Value::Lambda { params, rest_param, body, env: closure_env } => {
                        let new_env = Env::with_parent(closure_env);
                        if let Some(ref rp) = rest_param {
                            for (p, a) in params.iter().zip(call_args.iter()) {
                                new_env.set(p.clone(), a.clone());
                            }
                            let mut rest = Value::Nil;
                            for a in call_args[params.len()..].iter().rev() {
                                rest = make_pair(a.clone(), rest);
                            }
                            new_env.set(rp.clone(), rest);
                        } else {
                            for (p, a) in params.iter().zip(call_args.into_iter()) {
                                new_env.set(p.clone(), a);
                            }
                        }
                        let mut v = Value::Nil;
                        for e in body.iter() {
                            v = eval(e, &new_env, out)?;
                        }
                        v
                    }
                    _ => return Err(EvalError::Type(format!("map: first argument must be a procedure at {}", pos.fmt()))),
                };
                result_items.push(val);
                lists = new_lists;
            }
            let mut result = Value::Nil;
            for item in result_items.into_iter().rev() {
                result = make_pair(item, result);
            }
            Ok(Some(result))
        }
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("char-alphabetic? requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Char(c) => Ok(Some(Value::Boolean(c.is_alphabetic()))),
                _ => Err(EvalError::Type(format!("char-alphabetic?: expected char at {}", pos.fmt()))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("char-numeric? requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Char(c) => Ok(Some(Value::Boolean(c.is_ascii_digit()))),
                _ => Err(EvalError::Type(format!("char-numeric?: expected char at {}", pos.fmt()))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("char-upcase requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Char(c) => Ok(Some(Value::Char(c.to_ascii_uppercase()))),
                _ => Err(EvalError::Type(format!("char-upcase: expected char at {}", pos.fmt()))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("char-downcase requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Char(c) => Ok(Some(Value::Char(c.to_ascii_lowercase()))),
                _ => Err(EvalError::Type(format!("char-downcase: expected char at {}", pos.fmt()))),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("char=? requires 2 arguments at {}", pos.fmt())));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Some(Value::Boolean(a == b))),
                _ => Err(EvalError::Type(format!("char=?: expected chars at {}", pos.fmt()))),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("char<? requires 2 arguments at {}", pos.fmt())));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Some(Value::Boolean(a < b))),
                _ => Err(EvalError::Type(format!("char<?: expected chars at {}", pos.fmt()))),
            }
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("string=? requires 2 arguments at {}", pos.fmt())));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Some(Value::Boolean(a == b))),
                _ => Err(EvalError::Type(format!("string=?: expected strings at {}", pos.fmt()))),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("string<? requires 2 arguments at {}", pos.fmt())));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Some(Value::Boolean(a < b))),
                _ => Err(EvalError::Type(format!("string<?: expected strings at {}", pos.fmt()))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("string-ci=? requires 2 arguments at {}", pos.fmt())));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Some(Value::Boolean(a.to_lowercase() == b.to_lowercase()))),
                _ => Err(EvalError::Type(format!("string-ci=?: expected strings at {}", pos.fmt()))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string-upcase requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Str(s.to_uppercase()))),
                _ => Err(EvalError::Type(format!("string-upcase: expected string at {}", pos.fmt()))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("string-downcase requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Str(s) => Ok(Some(Value::Str(s.to_lowercase()))),
                _ => Err(EvalError::Type(format!("string-downcase: expected string at {}", pos.fmt()))),
            }
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("eqv? requires 2 arguments at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(values_eqv(&args[0], &args[1]))))
        }
        "vector" => {
            Ok(Some(Value::Vector(Rc::new(RefCell::new(args.to_vec())))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity(format!("make-vector requires 1-2 arguments at {}", pos.fmt())));
            }
            let size = args[0].as_integer(pos)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Some(Value::Vector(Rc::new(RefCell::new(vec![fill; size])))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("vector-ref requires 2 arguments at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = args[1].as_integer(pos)? as usize;
                    let v = v.borrow();
                    if idx >= v.len() {
                        return Err(EvalError::Type(format!("vector-ref: index out of range at {}", pos.fmt())));
                    }
                    Ok(Some(v[idx].clone()))
                }
                _ => Err(EvalError::Type(format!("vector-ref: expected vector at {}", pos.fmt()))),
            }
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("vector-set! requires 3 arguments at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = args[1].as_integer(pos)? as usize;
                    let mut v = v.borrow_mut();
                    if idx >= v.len() {
                        return Err(EvalError::Type(format!("vector-set!: index out of range at {}", pos.fmt())));
                    }
                    v[idx] = args[2].clone();
                    Ok(Some(Value::Nil))
                }
                _ => Err(EvalError::Type(format!("vector-set!: expected vector at {}", pos.fmt()))),
            }
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("vector-length requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Vector(v) => Ok(Some(Value::Integer(v.borrow().len() as i64))),
                _ => Err(EvalError::Type(format!("vector-length: expected vector at {}", pos.fmt()))),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("vector? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(matches!(&args[0], Value::Vector(_)))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("vector->list requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let v = v.borrow();
                    let mut result = Value::Nil;
                    for item in v.iter().rev() {
                        result = make_pair(item.clone(), result);
                    }
                    Ok(Some(result))
                }
                _ => Err(EvalError::Type(format!("vector->list: expected vector at {}", pos.fmt()))),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("list->vector requires 1 argument at {}", pos.fmt())));
            }
            let mut items = Vec::new();
            let mut cur = args[0].clone();
            loop {
                match &cur {
                    Value::Nil => break,
                    Value::Pair(cell) => {
                        let (car, cdr) = {
                            let inner = cell.borrow();
                            (inner.0.clone(), inner.1.clone())
                        };
                        items.push(car);
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type(format!("list->vector: not a proper list at {}", pos.fmt()))),
                }
            }
            Ok(Some(Value::Vector(Rc::new(RefCell::new(items)))))
        }
        "values" => {
            match args.len() {
                0 => Ok(Some(Value::Values(vec![]))),
                1 => Ok(Some(args[0].clone())),
                _ => Ok(Some(Value::Values(args.to_vec()))),
            }
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("exact? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _)))))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("inexact? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Float(_)))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("exact->inexact requires 1 argument at {}", pos.fmt())));
            }
            let f = value_to_f64(&args[0], pos)?;
            Ok(Some(Value::Float(f)))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("inexact->exact requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Some(Value::Integer(*n))),
                Value::Rational(n, d) => Ok(Some(Value::Rational(*n, *d))),
                Value::Float(f) => {
                    // Convert float to exact rational using continued fraction approximation
                    let (n, d) = float_to_rational(*f);
                    Ok(Some(make_rational_value(n, d)))
                }
                _ => Err(EvalError::Type(format!("inexact->exact: expected number at {}", pos.fmt()))),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("numerator requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Some(Value::Integer(*n))),
                Value::Rational(n, _) => Ok(Some(Value::Integer(*n))),
                _ => Err(EvalError::Type(format!("numerator: expected rational at {}", pos.fmt()))),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("denominator requires 1 argument at {}", pos.fmt())));
            }
            match &args[0] {
                Value::Integer(_) => Ok(Some(Value::Integer(1))),
                Value::Rational(_, d) => Ok(Some(Value::Integer(*d))),
                _ => Err(EvalError::Type(format!("denominator: expected rational at {}", pos.fmt()))),
            }
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("rational? requires 1 argument at {}", pos.fmt())));
            }
            Ok(Some(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _)))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("integer? requires 1 argument at {}", pos.fmt())));
            }
            let is_int = match &args[0] {
                Value::Integer(_) => true,
                Value::Rational(_, d) => *d == 1, // shouldn't happen since make_rational normalizes
                Value::Float(f) => *f == f.trunc() && f.is_finite(),
                _ => false,
            };
            Ok(Some(Value::Boolean(is_int)))
        }
        _ if op.starts_with("##record-ctor-") => {
            let type_id: usize = op["##record-ctor-".len()..].parse().unwrap();
            let info = RECORD_TYPES.with(|rt| {
                rt.borrow().get(&type_id).map(|i| (i.name.clone(), i.num_fields))
            });
            let (type_name, num_fields) = info.unwrap();
            if args.len() != num_fields {
                return Err(EvalError::Arity(format!(
                    "record constructor {} requires {} arguments, got {} at {}",
                    type_name, num_fields, args.len(), pos.fmt()
                )));
            }
            Ok(Some(Value::Record {
                type_id,
                type_name,
                fields: Rc::new(RefCell::new(args.to_vec())),
            }))
        }
        _ if op.starts_with("##record-pred-") => {
            let type_id: usize = op["##record-pred-".len()..].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record predicate requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            let result = matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id);
            Ok(Some(Value::Boolean(result)))
        }
        _ if op.starts_with("##record-acc-") => {
            let rest = &op["##record-acc-".len()..];
            let parts: Vec<&str> = rest.splitn(2, '-').collect();
            let type_id: usize = parts[0].parse().unwrap();
            let field_idx: usize = parts[1].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "record accessor requires 1 argument at {}",
                    pos.fmt()
                )));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if *tid == type_id => {
                    Ok(Some(fields.borrow()[field_idx].clone()))
                }
                _ => Err(EvalError::Type(format!(
                    "record accessor: expected record of correct type at {}",
                    pos.fmt()
                ))),
            }
        }
        _ => Ok(None),
    }
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 { return (0, 1); }
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    // Use the fact that many common floats have exact binary representations
    // Try simple denominators first
    for d in 1..=1000000i64 {
        let n = (f * d as f64).round() as i64;
        if ((n as f64 / d as f64) - f).abs() < 1e-10 {
            let g = gcd(n, d);
            return (sign * n / g, d / g);
        }
    }
    (sign * (f * 1000000.0).round() as i64, 1000000)
}

fn compare_op_val(
    args: &[Value],
    cmp: impl Fn(f64, f64) -> bool,
    pos: Pos,
) -> Result<Option<Value>, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!(
            "comparison requires at least 2 arguments at {}",
            pos.fmt()
        )));
    }
    let mut prev = value_to_f64(&args[0], pos)?;
    for a in &args[1..] {
        let cur = value_to_f64(a, pos)?;
        if !cmp(prev, cur) {
            return Ok(Some(Value::Boolean(false)));
        }
        prev = cur;
    }
    Ok(Some(Value::Boolean(true)))
}

// ── Hygienic Macros ─────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "define" | "quote" | "lambda" | "and" | "or"
            | "let" | "begin" | "cond" | "set!" | "call/cc"
            | "call-with-current-continuation" | "string-set!"
            | "define-syntax" | "syntax-rules" | "dynamic-wind"
            | "raise" | "guard" | "with-exception-handler"
            | "define-record-type"
    ) || Value::is_builtin_name(name)
}

fn match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, MacroBinding>> {
    let mut bindings = HashMap::new();
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        // Check for ellipsis
        if pi + 1 < pattern.len() {
            if let ExprKind::Symbol(ref s) = pattern[pi + 1].kind {
                if s == "..." {
                    let var_name = match &pattern[pi].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return None,
                    };
                    let remaining_pattern = pattern.len() - pi - 2;
                    let available = input.len().checked_sub(ii + remaining_pattern)?;
                    let mut collected = Vec::new();
                    for _ in 0..available {
                        collected.push(input[ii].clone());
                        ii += 1;
                    }
                    bindings.insert(var_name, MacroBinding::List(collected));
                    pi += 2;
                    continue;
                }
            }
        }

        if ii >= input.len() {
            return None;
        }

        match &pattern[pi].kind {
            ExprKind::Symbol(ref s) if literals.contains(s) => {
                match &input[ii].kind {
                    ExprKind::Symbol(ref is) if is == s => {}
                    _ => return None,
                }
            }
            ExprKind::Symbol(ref s) if s == "_" => {}
            ExprKind::Symbol(ref s) => {
                bindings.insert(s.clone(), MacroBinding::Single(input[ii].clone()));
            }
            ExprKind::List(sub_pat) => match &input[ii].kind {
                ExprKind::List(sub_inp) => {
                    let sub = match_pattern(sub_pat, sub_inp, literals)?;
                    bindings.extend(sub);
                }
                _ => return None,
            },
            ExprKind::Integer(n) => match &input[ii].kind {
                ExprKind::Integer(m) if n == m => {}
                _ => return None,
            },
            ExprKind::Boolean(b) => match &input[ii].kind {
                ExprKind::Boolean(b2) if b == b2 => {}
                _ => return None,
            },
            _ => return None,
        }

        pi += 1;
        ii += 1;
    }

    if ii == input.len() {
        Some(bindings)
    } else {
        None
    }
}

fn find_ellipsis_var(expr: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(ref s) => {
            if let Some(MacroBinding::List(_)) = bindings.get(s) {
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

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(ref s) => {
            if let Some(MacroBinding::Single(expr)) = bindings.get(s) {
                return expr.clone();
            }
            if let Some(new_name) = renames.get(s) {
                return Expr {
                    kind: ExprKind::Symbol(new_name.clone()),
                    pos: template.pos,
                };
            }
            template.clone()
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() {
                    if let ExprKind::Symbol(ref s) = items[i + 1].kind {
                        if s == "..." {
                            if let Some(var_name) = find_ellipsis_var(&items[i], bindings) {
                                if let Some(MacroBinding::List(ref elems)) = bindings.get(&var_name) {
                                    for elem in elems {
                                        let mut local = bindings.clone();
                                        local.insert(var_name.clone(), MacroBinding::Single(elem.clone()));
                                        expanded.push(expand_template(&items[i], &local, renames));
                                    }
                                }
                            }
                            i += 2;
                            continue;
                        }
                    }
                }
                expanded.push(expand_template(&items[i], bindings, renames));
                i += 1;
            }
            Expr {
                kind: ExprKind::List(expanded),
                pos: template.pos,
            }
        }
        _ => template.clone(),
    }
}

fn collect_free_symbols(
    expr: &Expr,
    pattern_vars: &HashSet<String>,
    result: &mut HashSet<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(ref s) => {
            if s != "..." && !pattern_vars.contains(s) && !is_keyword(s) {
                result.insert(s.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_free_symbols(item, pattern_vars, result);
            }
        }
        _ => {}
    }
}

fn expand_macro(
    input: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    pos: Pos,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        let pat_items = match &pattern.kind {
            ExprKind::List(items) => items,
            _ => continue,
        };
        if let Some(bindings) = match_pattern(&pat_items[1..], &input[1..], literals) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let mut free_syms = HashSet::new();
            collect_free_symbols(template, &pattern_vars, &mut free_syms);

            let mut renames = HashMap::new();
            for sym in &free_syms {
                renames.insert(sym.clone(), gensym(sym));
            }

            for (orig, renamed) in &renames {
                if let Some(val) = def_env.get(orig) {
                    use_env.set(renamed.clone(), val);
                }
            }

            return Ok(expand_template(template, &bindings, &renames));
        }
    }
    Err(EvalError::Type(format!(
        "no matching macro pattern at {}",
        pos.fmt()
    )))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    let mut out = String::new();
    let mut result = Value::Boolean(false);
    let mut i = 0;
    while i < exprs.len() {
        CALLCC_TOP_IDX.with(|c| c.set(i));
        match eval(&exprs[i], &env, &mut out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (cc_pos, start_idx, value) =
                    CONT_RETURN.with(|cr| cr.borrow_mut().take().unwrap());
                CALLCC_PENDING.with(|p| *p.borrow_mut() = Some((cc_pos, value)));
                i = start_idx;
            }
            Err(e) => return Err(e),
        }
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = Env::new();
    let mut out = String::new();
    let mut result = Value::Boolean(false);
    let mut i = 0;
    while i < exprs.len() {
        CALLCC_TOP_IDX.with(|c| c.set(i));
        match eval(&exprs[i], &env, &mut out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (cc_pos, start_idx, value) =
                    CONT_RETURN.with(|cr| cr.borrow_mut().take().unwrap());
                CALLCC_PENDING.with(|p| *p.borrow_mut() = Some((cc_pos, value)));
                i = start_idx;
            }
            Err(e) => return Err(e),
        }
    }
    Ok((result.display(), out))
}

#[cfg(test)]
mod tests;
