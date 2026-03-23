pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

thread_local! {
    static OUTPUT_BUF: RefCell<String> = RefCell::new(String::new());
    static WIND_STACK: RefCell<Vec<WindEntry>> = RefCell::new(Vec::new());
    static RAISED_VALUE: RefCell<Option<Value>> = RefCell::new(None);
}

static WIND_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
struct WindEntry {
    id: usize,
    in_thunk: Value,
    out_thunk: Value,
}

fn wind_stack_snapshot() -> Vec<WindEntry> {
    WIND_STACK.with(|ws| ws.borrow().clone())
}

fn wind_stack_reset() {
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
}

fn raised_value_reset() {
    RAISED_VALUE.with(|rv| *rv.borrow_mut() = None);
}

fn output_write(s: &str) {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push_str(s));
}

fn output_take() -> String {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().split_off(0))
}

fn make_string(s: std::string::String) -> Value {
    Value::String(Rc::new(RefCell::new(s)), false)
}

fn make_immutable_string(s: std::string::String) -> Value {
    Value::String(Rc::new(RefCell::new(s)), true)
}

/// A Scheme runtime value.
#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(Rc<RefCell<std::string::String>>, bool),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Symbol(String),
    Void,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String, fn(&[Value]) -> Result<Value, EvalError>),
    CallCC,
    DynamicWind,
    SchemeApply,
    Raise,
    WithExceptionHandler,
    Continuation(Rc<Vec<Frame>>, Rc<Vec<WindEntry>>),
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    SchemeValues,
    CallWithValues,
    MultipleValues(Vec<Value>),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({n})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::String(s, _) => write!(f, "String({:?})", s.borrow()),
            Value::Char(c) => write!(f, "Char({c:?})"),
            Value::List(l) => write!(f, "List({l:?})"),
            Value::Pair(a, b) => write!(f, "Pair({a:?}, {b:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::Void => write!(f, "Void"),
            Value::Lambda { params, rest_param, .. } => write!(f, "Lambda({params:?}, rest={rest_param:?})"),
            Value::Builtin(name, _) => write!(f, "Builtin({name})"),
            Value::CallCC => write!(f, "CallCC"),
            Value::DynamicWind => write!(f, "DynamicWind"),
            Value::SchemeApply => write!(f, "SchemeApply"),
            Value::Raise => write!(f, "Raise"),
            Value::WithExceptionHandler => write!(f, "WithExceptionHandler"),
            Value::Continuation(..) => write!(f, "Continuation(...)"),
            Value::Vector(v) => write!(f, "Vector({:?})", v.borrow()),
            Value::Macro { .. } => write!(f, "Macro(...)"),
            Value::SchemeValues => write!(f, "SchemeValues"),
            Value::CallWithValues => write!(f, "CallWithValues"),
            Value::MultipleValues(vs) => write!(f, "MultipleValues({vs:?})"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a, _), Value::String(b, _)) => *a.borrow() == *b.borrow(),
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, b1), Value::Pair(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Void, Value::Void) => true,
            (Value::CallCC, Value::CallCC) => true,
            (Value::DynamicWind, Value::DynamicWind) => true,
            (Value::SchemeApply, Value::SchemeApply) => true,
            (Value::Raise, Value::Raise) => true,
            (Value::WithExceptionHandler, Value::WithExceptionHandler) => true,
            (Value::SchemeValues, Value::SchemeValues) => true,
            (Value::CallWithValues, Value::CallWithValues) => true,
            _ => false,
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes, chars as raw character.
    fn display_fmt(&self, out: &mut String) {
        match self {
            Value::String(s, _) => out.push_str(&s.borrow()),
            Value::Char(c) => out.push(*c),
            Value::Vector(_) => out.push_str(&self.to_string()),
            Value::Macro { .. } => out.push_str("#<macro>"),
            Value::MultipleValues(_) => out.push_str("#<values>"),
            _ => out.push_str(&self.to_string()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::String(s, _) => write!(f, "\"{}\"", s.borrow()),
            Value::Char(c) => write!(f, "#\\{c}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Pair(a, b) => {
                write!(f, "({a}")?;
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cb) => {
                            write!(f, " {ca}")?;
                            cur = cb.as_ref();
                        }
                        Value::List(l) if l.is_empty() => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Symbol(s) => write!(f, "{s}"),
            Value::Void => write!(f, "#<void>"),
            Value::Vector(v) => {
                write!(f, "#(")?;
                let elems = v.borrow();
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Lambda { .. } | Value::Builtin(..) | Value::CallCC | Value::DynamicWind | Value::SchemeApply | Value::Raise | Value::WithExceptionHandler | Value::Continuation(..) | Value::SchemeValues | Value::CallWithValues => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::MultipleValues(_) => write!(f, "#<values>"),
        }
    }
}

// ── AST with source positions ──

#[derive(Clone, Debug, PartialEq)]
struct Expr {
    kind: ExprKind,
    line: usize,
    col: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum ExprKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => make_immutable_string(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(elems) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

// ── Environment ──

type Env = Rc<RefCell<EnvInner>>;

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
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_update(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        Ok(())
    } else if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        Err(EvalError::UnboundVariable(name.to_string()))
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&str, fn(&[Value]) -> Result<Value, EvalError>)] = &[
        ("+", arith_add),
        ("-", arith_sub),
        ("*", arith_mul),
        ("/", arith_div),
        ("<", |a| cmp_op(a, |x, y| x < y)),
        (">", |a| cmp_op(a, |x, y| x > y)),
        ("=", |a| cmp_op(a, |x, y| x == y)),
        ("<=", |a| cmp_op(a, |x, y| x <= y)),
        (">=", |a| cmp_op(a, |x, y| x >= y)),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("append", builtin_append),
        ("string?", |a| builtin_type_pred(a, "string?")),
        ("number?", |a| builtin_type_pred(a, "number?")),
        ("boolean?", |a| builtin_type_pred(a, "boolean?")),
        ("pair?", |a| builtin_type_pred(a, "pair?")),
        ("symbol?", |a| builtin_type_pred(a, "symbol?")),
        ("char?", |a| builtin_type_pred(a, "char?")),
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
        ("string-set!", builtin_string_set),
        ("string->list", builtin_string_to_list),
        ("list->string", builtin_list_to_string),
        ("char->integer", builtin_char_to_integer),
        ("integer->char", builtin_integer_to_char),
        ("eq?", builtin_eq),
        ("equal?", builtin_equal),
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
        ("list?", builtin_list_pred),
        ("assoc", builtin_assoc),
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
        ("eqv?", builtin_eqv),
        ("vector", builtin_vector),
        ("make-vector", builtin_make_vector),
        ("vector-ref", builtin_vector_ref),
        ("vector-set!", builtin_vector_set),
        ("vector-length", builtin_vector_length),
        ("vector?", |a| builtin_type_pred(a, "vector?")),
        ("vector->list", builtin_vector_to_list),
        ("list->vector", builtin_list_to_vector),
        ("procedure?", |a| builtin_type_pred(a, "procedure?")),
        ("integer?", |a| builtin_type_pred(a, "number?")),
        ("memq", builtin_memq),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string(), func));
    }
    env_set(&env, "apply".to_string(), Value::SchemeApply);
    env_set(&env, "call/cc".to_string(), Value::CallCC);
    env_set(&env, "call-with-current-continuation".to_string(), Value::CallCC);
    env_set(&env, "dynamic-wind".to_string(), Value::DynamicWind);
    env_set(&env, "raise".to_string(), Value::Raise);
    env_set(&env, "with-exception-handler".to_string(), Value::WithExceptionHandler);
    env_set(&env, "values".to_string(), Value::SchemeValues);
    env_set(&env, "call-with-values".to_string(), Value::CallWithValues);
    // Install Scheme-level prelude (map, etc.)
    let prelude = r#"
(define (__map1 f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (__map1 f (cdr lst)))))
(define (map proc . lists)
  (if (null? (car lists))
      '()
      (cons (apply proc (__map1 car lists))
            (apply map proc (__map1 cdr lists)))))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (assq key alist)
  (cond ((null? alist) #f)
        ((eq? (car (car alist)) key) (car alist))
        (else (assq key (cdr alist)))))
(define (reverse lst)
  (define (rev-helper lst acc)
    (if (null? lst) acc
        (rev-helper (cdr lst) (cons (car lst) acc))))
  (rev-helper lst '()))
(define (for-each proc . lists)
  (if (null? (car lists))
      (if #f #f)
      (begin (apply proc (__map1 car lists))
             (apply for-each proc (__map1 cdr lists)))))
"#;
    let mut parser = Parser::new(prelude);
    let exprs = parser.parse_all().expect("prelude parse error");
    eval_top(&exprs, &env).expect("prelude eval error");
    env
}

// ── Parser ──

struct Token {
    text: String,
    line: usize,
    col: usize,
}

fn tokenize(input: &str) -> Vec<Token> {
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
            '(' | ')' => {
                tokens.push(Token { text: chars[i].to_string(), line, col });
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push(Token { text: "'".to_string(), line, col });
                i += 1;
                col += 1;
            }
            '"' => {
                let tok_line = line;
                let tok_col = col;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        i += 1;
                        col += 1;
                        s.push(chars[i]);
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        i += 1;
                    } else {
                        s.push(chars[i]);
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: tok_line, col: tok_col });
            }
            '#' => {
                let tok_col = col;
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')') {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: tok_col });
            }
            _ => {
                let tok_col = col;
                let mut tok = String::new();
                while i < chars.len() && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';') {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: tok_col });
            }
        }
    }
    tokens
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        Self {
            tokens: tokenize(input),
            pos: 0,
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, EvalError> {
        if self.pos >= self.tokens.len() {
            return Err(EvalError::Parse("unexpected end of input".into()));
        }
        let line = self.tokens[self.pos].line;
        let col = self.tokens[self.pos].col;
        let text = self.tokens[self.pos].text.clone();

        if text == "(" {
            self.pos += 1;
            let mut elems = Vec::new();
            while self.pos < self.tokens.len() && self.tokens[self.pos].text != ")" {
                elems.push(self.parse_expr()?);
            }
            if self.pos >= self.tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            self.pos += 1;
            Ok(Expr { kind: ExprKind::List(elems), line, col })
        } else if text == "'" {
            self.pos += 1;
            let inner = self.parse_expr()?;
            Ok(Expr {
                kind: ExprKind::List(vec![
                    Expr { kind: ExprKind::Symbol("quote".into()), line, col },
                    inner,
                ]),
                line,
                col,
            })
        } else {
            self.pos += 1;
            Ok(Expr { kind: parse_atom(&text), line, col })
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

fn parse_atom(token: &str) -> ExprKind {
    if token == "#t" {
        ExprKind::Boolean(true)
    } else if token == "#f" {
        ExprKind::Boolean(false)
    } else if token.starts_with("#\\") {
        let rest = &token[2..];
        let c = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => rest.chars().next().unwrap_or('?'),
        };
        ExprKind::Char(c)
    } else if token.starts_with('"') && token.ends_with('"') {
        ExprKind::Str(token[1..token.len() - 1].to_string())
    } else if let Ok(n) = token.parse::<i64>() {
        ExprKind::Integer(n)
    } else {
        ExprKind::Symbol(token.to_string())
    }
}

// ── Macro Expansion (syntax-rules) ──

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__hyg{}", base, n)
}

#[derive(Clone, Debug)]
enum PatBinding {
    One(Expr),
    Many(Vec<Expr>),
}

fn match_syntax(pattern: &Expr, form: &[Expr], literals: &[String]) -> Option<HashMap<String, PatBinding>> {
    let pat_elems = match &pattern.kind {
        ExprKind::List(e) => e,
        _ => return None,
    };
    if pat_elems.is_empty() { return None; }
    let mut bindings = HashMap::new();
    if match_elems(&pat_elems[1..], &form[1..], literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_elems(pats: &[Expr], forms: &[Expr], literals: &[String], bindings: &mut HashMap<String, PatBinding>) -> bool {
    let mut pi = 0;
    let mut fi = 0;
    while pi < pats.len() {
        let is_ellipsis = pi + 1 < pats.len()
            && matches!(&pats[pi + 1].kind, ExprKind::Symbol(s) if s == "...");
        if is_ellipsis {
            let remaining = count_required(&pats[pi + 2..]);
            let available = if forms.len() >= fi + remaining {
                forms.len() - fi - remaining
            } else {
                return false;
            };
            let vars = pattern_vars(&pats[pi], literals);
            for v in &vars {
                bindings.insert(v.clone(), PatBinding::Many(vec![]));
            }
            for j in 0..available {
                let mut sub = HashMap::new();
                if !match_one(&pats[pi], &forms[fi + j], literals, &mut sub) {
                    return false;
                }
                for (k, v) in sub {
                    if let PatBinding::One(expr) = v {
                        if let Some(PatBinding::Many(ref mut vec)) = bindings.get_mut(&k) {
                            vec.push(expr);
                        }
                    }
                }
            }
            fi += available;
            pi += 2;
        } else {
            if fi >= forms.len() { return false; }
            if !match_one(&pats[pi], &forms[fi], literals, bindings) {
                return false;
            }
            pi += 1;
            fi += 1;
        }
    }
    fi == forms.len()
}

fn count_required(pats: &[Expr]) -> usize {
    let mut count = 0;
    let mut i = 0;
    while i < pats.len() {
        if i + 1 < pats.len() && matches!(&pats[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
            i += 2;
        } else {
            count += 1;
            i += 1;
        }
    }
    count
}

fn pattern_vars(pat: &Expr, literals: &[String]) -> Vec<String> {
    match &pat.kind {
        ExprKind::Symbol(s) if s != "..." && s != "_" && !literals.contains(s) => vec![s.clone()],
        ExprKind::List(elems) => elems.iter().flat_map(|e| pattern_vars(e, literals)).collect(),
        _ => vec![],
    }
}

fn match_one(pat: &Expr, form: &Expr, literals: &[String], bindings: &mut HashMap<String, PatBinding>) -> bool {
    match &pat.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&form.kind, ExprKind::Symbol(fs) if fs == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), PatBinding::One(form.clone()));
            true
        }
        ExprKind::List(pel) => {
            if let ExprKind::List(fel) = &form.kind {
                match_elems(pel, fel, literals, bindings)
            } else {
                false
            }
        }
        ExprKind::Boolean(b) => matches!(&form.kind, ExprKind::Boolean(fb) if fb == b),
        ExprKind::Integer(n) => matches!(&form.kind, ExprKind::Integer(m) if m == n),
        _ => false,
    }
}

const SPECIAL_FORMS: &[&str] = &[
    "if", "define", "lambda", "let", "let*", "letrec", "letrec*",
    "set!", "begin", "quote", "and", "or", "cond", "case", "do", "when",
    "define-syntax", "syntax-rules",
];

fn expand_macro(
    form: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        if let Some(bindings) = match_syntax(pattern, form, literals) {
            let mut gensym_map = HashMap::new();
            return Ok(instantiate(template, &bindings, def_env, use_env, &mut gensym_map));
        }
    }
    Err(EvalError::Type("no matching syntax-rules pattern".into()))
}

fn instantiate(
    tmpl: &Expr,
    bindings: &HashMap<String, PatBinding>,
    def_env: &Env,
    use_env: &Env,
    gensym_map: &mut HashMap<String, String>,
) -> Expr {
    let line = tmpl.line;
    let col = tmpl.col;
    match &tmpl.kind {
        ExprKind::Symbol(s) => {
            if let Some(PatBinding::One(expr)) = bindings.get(s) {
                return expr.clone();
            }
            if SPECIAL_FORMS.contains(&s.as_str()) {
                return tmpl.clone();
            }
            if let Some(val) = env_get(def_env, s) {
                match val {
                    Value::Macro { .. } => return tmpl.clone(),
                    _ => {
                        let gs = gensym_map.entry(s.clone()).or_insert_with(|| {
                            let g = gensym(s);
                            env_set(use_env, g.clone(), val);
                            g
                        });
                        return Expr { kind: ExprKind::Symbol(gs.clone()), line, col };
                    }
                }
            }
            tmpl.clone()
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                let is_ellipsis = i + 1 < elems.len()
                    && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...");
                if is_ellipsis {
                    let vars = template_many_vars(&elems[i], bindings);
                    let count = vars.first()
                        .and_then(|v| match bindings.get(v) {
                            Some(PatBinding::Many(vec)) => Some(vec.len()),
                            _ => None,
                        })
                        .unwrap_or(0);
                    for j in 0..count {
                        let mut sub = bindings.clone();
                        for v in &vars {
                            if let Some(PatBinding::Many(vec)) = bindings.get(v) {
                                sub.insert(v.clone(), PatBinding::One(vec[j].clone()));
                            }
                        }
                        result.push(instantiate(&elems[i], &sub, def_env, use_env, gensym_map));
                    }
                    i += 2;
                } else {
                    result.push(instantiate(&elems[i], bindings, def_env, use_env, gensym_map));
                    i += 1;
                }
            }
            Expr { kind: ExprKind::List(result), line, col }
        }
        _ => tmpl.clone(),
    }
}

fn template_many_vars(tmpl: &Expr, bindings: &HashMap<String, PatBinding>) -> Vec<String> {
    let mut vars = Vec::new();
    collect_many_vars(tmpl, bindings, &mut vars);
    vars
}

fn collect_many_vars(tmpl: &Expr, bindings: &HashMap<String, PatBinding>, vars: &mut Vec<String>) {
    match &tmpl.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(PatBinding::Many(_))) && !vars.contains(s) {
                vars.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_many_vars(e, bindings, vars);
            }
        }
        _ => {}
    }
}

// ── CEK Machine ──

#[derive(Clone)]
enum WindAction {
    Unwind(Value),
    Rewind(WindEntry),
}

/// Continuation frame for the CEK machine.
#[derive(Clone)]
enum Frame {
    Define(String, Env),
    SetBang(String, Env),
    IfTest { then_br: Expr, else_br: Option<Expr>, env: Env },
    EvalFunc { args: Vec<Expr>, env: Env },
    CollectArgs { func: Value, unevaluated: Vec<Expr>, evaluated: Vec<Value>, env: Env },
    Seq(Vec<Expr>, Env),
    And(Vec<Expr>, Env),
    Or(Vec<Expr>, Env),
    LetBind { var: String, remaining: Vec<(String, Expr)>, body: Vec<Expr>, outer_env: Env, local_env: Env },
    CondTest { body: Vec<Expr>, rest: Vec<Expr>, env: Env },
    CaseKey { clauses: Vec<Expr>, env: Env },
    DoTest {
        vars: Vec<String>,
        steps: Vec<Option<Expr>>,
        test: Expr,
        exprs: Vec<Expr>,
        body: Vec<Expr>,
        env: Env,
    },
    LetrecBind {
        bindings: Vec<(String, Expr)>,
        idx: usize,
        body: Vec<Expr>,
        env: Env,
    },
    DynamicWindAfterIn { body_thunk: Value, out_thunk: Value, in_thunk: Value, wind_id: usize },
    DynamicWindAfterBody { out_thunk: Value },
    DynamicWindAfterOut { result: Value },
    WindTransition { actions: Vec<WindAction>, value: Value },
    Guard { var: String, clauses: Vec<Expr>, env: Env },
    ExceptionHandler { handler: Value },
    RaiseHandlerReturn { raised_value: Value },
    GuardTest { raised_value: Value, body: Vec<Expr>, rest: Vec<Expr>, env: Env },
    GuardClauseEval { clauses: Vec<Expr>, raised_value: Value, env: Env },
    CallWithValuesConsumer { consumer: Value },
}

enum Act {
    Ev(Expr, Env),
    Ret(Value),
    Ap(Value, Vec<Value>),
}

fn eval_top(exprs: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut stack: Vec<Frame> = Vec::new();
    if exprs.len() > 1 {
        stack.push(Frame::Seq(exprs[1..].to_vec(), env.clone()));
    }
    let mut act = Act::Ev(exprs[0].clone(), env.clone());
    let mut last_line = 0usize;
    let mut last_col = 0usize;
    loop {
        let result = match act {
            Act::Ev(e, env) => {
                last_line = e.line;
                last_col = e.col;
                step_eval(e, env, &mut stack)
            }
            Act::Ret(val) => match stack.pop() {
                Some(frame) => step_ret(val, frame, &mut stack),
                None => return Ok(val),
            },
            Act::Ap(func, args) => step_apply(func, args, &mut stack),
        };
        act = match result {
            Ok(next_act) => next_act,
            Err(e) if is_scheme_raise(&e) => {
                let raised_val = RAISED_VALUE.with(|rv| rv.borrow_mut().take().unwrap());
                handle_raise(&mut stack, raised_val).map_err(|e| e.at(last_line, last_col))?
            }
            Err(e) => return Err(e.at(last_line, last_col)),
        };
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Value, EvalError> {
    eval_top(std::slice::from_ref(expr), env)
}

fn step_eval(expr: Expr, env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Act::Ret(Value::Integer(*n))),
        ExprKind::Boolean(b) => Ok(Act::Ret(Value::Boolean(*b))),
        ExprKind::Str(s) => Ok(Act::Ret(make_immutable_string(s.clone()))),
        ExprKind::Char(c) => Ok(Act::Ret(Value::Char(*c))),
        ExprKind::Symbol(name) => env_get(&env, name)
            .map(Act::Ret)
            .ok_or_else(|| EvalError::UnboundVariable(name.clone()).at(expr.line, expr.col)),
        ExprKind::List(elems) if elems.is_empty() => {
            Err(EvalError::Parse("empty application".into()).at(expr.line, expr.col))
        }
        ExprKind::List(elems) => {
            if let ExprKind::Symbol(name) = &elems[0].kind {
                match name.as_str() {
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity("quote expects 1 argument".into()).at(expr.line, expr.col));
                        }
                        return Ok(Act::Ret(expr_to_value(&elems[1])));
                    }
                    "if" => {
                        let a = &elems[1..];
                        if a.len() < 2 || a.len() > 3 {
                            return Err(EvalError::Arity("if expects 2 or 3 arguments".into()).at(expr.line, expr.col));
                        }
                        let else_br = if a.len() == 3 { Some(a[2].clone()) } else { None };
                        stack.push(Frame::IfTest { then_br: a[1].clone(), else_br, env: env.clone() });
                        return Ok(Act::Ev(a[0].clone(), env));
                    }
                    "define" => return sf_define(&elems[1..], env, stack),
                    "lambda" => return eval_lambda(&elems[1..], &env).map(Act::Ret),
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity("set! expects 2 arguments".into()));
                        }
                        let sym = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("set! requires a symbol".into())),
                        };
                        stack.push(Frame::SetBang(sym, env.clone()));
                        return Ok(Act::Ev(elems[2].clone(), env));
                    }
                    "and" => return sf_and(&elems[1..], env, stack),
                    "or" => return sf_or(&elems[1..], env, stack),
                    "let" => return sf_let(&elems[1..], env, stack),
                    "let*" => return sf_let_star(&elems[1..], env, stack),
                    "letrec" => return sf_letrec(&elems[1..], env, stack),
                    "letrec*" => return sf_letrec_star(&elems[1..], env, stack),
                    "begin" => return sf_seq(&elems[1..], env, stack),
                    "cond" => return sf_cond(&elems[1..], env, stack),
                    "case" => return sf_case(&elems[1..], env, stack),
                    "do" => return sf_do(&elems[1..], env, stack),
                    "when" => return sf_when(&elems[1..], env, stack),
                    "guard" => return sf_guard(&elems[1..], env, stack),
                    "define-syntax" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity("define-syntax expects 2 arguments".into()));
                        }
                        let macro_name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("define-syntax: expected symbol".into())),
                        };
                        let sr = match &elems[2].kind {
                            ExprKind::List(parts) => parts,
                            _ => return Err(EvalError::Type("define-syntax: expected syntax-rules".into())),
                        };
                        if sr.is_empty() || !matches!(&sr[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
                            return Err(EvalError::Type("define-syntax: expected syntax-rules".into()));
                        }
                        if sr.len() < 2 {
                            return Err(EvalError::Type("syntax-rules: expected literals list".into()));
                        }
                        let literals = match &sr[1].kind {
                            ExprKind::List(lits) => {
                                let mut v = Vec::new();
                                for l in lits {
                                    match &l.kind {
                                        ExprKind::Symbol(s) => v.push(s.clone()),
                                        _ => return Err(EvalError::Type("syntax-rules: literals must be symbols".into())),
                                    }
                                }
                                v
                            }
                            _ => return Err(EvalError::Type("syntax-rules: expected literals list".into())),
                        };
                        let mut rules = Vec::new();
                        for rule in &sr[2..] {
                            match &rule.kind {
                                ExprKind::List(parts) if parts.len() == 2 => {
                                    rules.push((parts[0].clone(), parts[1].clone()));
                                }
                                _ => return Err(EvalError::Type("syntax-rules: each rule must be (pattern template)".into())),
                            }
                        }
                        env_set(&env, macro_name, Value::Macro { literals, rules, def_env: env.clone() });
                        return Ok(Act::Ret(Value::Void));
                    }
                    _ => {
                        if let Some(val) = env_get(&env, name) {
                            if let Value::Macro { literals, rules, def_env } = val {
                                let expanded = expand_macro(elems, &literals, &rules, &def_env, &env)?;
                                return Ok(Act::Ev(expanded, env));
                            }
                        }
                    }
                }
            }
            // Function application: evaluate func, then args right-to-left
            stack.push(Frame::EvalFunc { args: elems[1..].to_vec(), env: env.clone() });
            Ok(Act::Ev(elems[0].clone(), env))
        }
    }
}

fn step_ret(val: Value, frame: Frame, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    match frame {
        Frame::Define(name, env) => {
            env_set(&env, name, val);
            Ok(Act::Ret(Value::Void))
        }
        Frame::SetBang(name, env) => {
            env_update(&env, &name, val)?;
            Ok(Act::Ret(Value::Void))
        }
        Frame::IfTest { then_br, else_br, env } => {
            if val != Value::Boolean(false) {
                Ok(Act::Ev(then_br, env))
            } else if let Some(e) = else_br {
                Ok(Act::Ev(e, env))
            } else {
                Ok(Act::Ret(Value::Void))
            }
        }
        Frame::EvalFunc { args, env } => {
            // Function evaluated; now collect args right-to-left
            if args.is_empty() {
                Ok(Act::Ap(val, vec![]))
            } else {
                let mut unevaluated = args;
                let rightmost = unevaluated.pop().unwrap();
                stack.push(Frame::CollectArgs { func: val, unevaluated, evaluated: vec![], env: env.clone() });
                Ok(Act::Ev(rightmost, env))
            }
        }
        Frame::CollectArgs { func, mut unevaluated, mut evaluated, env } => {
            evaluated.push(val);
            if let Some(next) = unevaluated.pop() {
                stack.push(Frame::CollectArgs { func, unevaluated, evaluated, env: env.clone() });
                Ok(Act::Ev(next, env))
            } else {
                // All args collected (in reverse order). Reverse to original order.
                evaluated.reverse();
                Ok(Act::Ap(func, evaluated))
            }
        }
        Frame::Seq(mut rest, env) => {
            if rest.len() == 1 {
                Ok(Act::Ev(rest.pop().unwrap(), env))
            } else {
                let next = rest.remove(0);
                stack.push(Frame::Seq(rest, env.clone()));
                Ok(Act::Ev(next, env))
            }
        }
        Frame::And(remaining, env) => {
            if val == Value::Boolean(false) {
                return Ok(Act::Ret(Value::Boolean(false)));
            }
            if remaining.len() == 1 {
                Ok(Act::Ev(remaining.into_iter().next().unwrap(), env))
            } else {
                let mut r = remaining;
                let next = r.remove(0);
                stack.push(Frame::And(r, env.clone()));
                Ok(Act::Ev(next, env))
            }
        }
        Frame::Or(remaining, env) => {
            if val != Value::Boolean(false) {
                return Ok(Act::Ret(val));
            }
            if remaining.len() == 1 {
                Ok(Act::Ev(remaining.into_iter().next().unwrap(), env))
            } else {
                let mut r = remaining;
                let next = r.remove(0);
                stack.push(Frame::Or(r, env.clone()));
                Ok(Act::Ev(next, env))
            }
        }
        Frame::LetBind { var, mut remaining, body, outer_env, local_env } => {
            env_set(&local_env, var, val);
            if let Some((next_var, next_expr)) = remaining.first().cloned() {
                remaining.remove(0);
                stack.push(Frame::LetBind { var: next_var, remaining, body, outer_env: outer_env.clone(), local_env });
                Ok(Act::Ev(next_expr, outer_env))
            } else {
                sf_seq(&body, local_env, stack)
            }
        }
        Frame::CondTest { body, rest, env } => {
            if val != Value::Boolean(false) {
                if body.is_empty() {
                    Ok(Act::Ret(val))
                } else {
                    sf_seq(&body, env, stack)
                }
            } else {
                sf_cond_clauses(&rest, env, stack)
            }
        }
        Frame::CaseKey { clauses, env } => {
            // val is the evaluated key; match against clauses
            for clause in &clauses {
                if let ExprKind::List(parts) = &clause.kind {
                    if parts.is_empty() { continue; }
                    // Check for else
                    if let ExprKind::Symbol(s) = &parts[0].kind {
                        if s == "else" {
                            return sf_seq(&parts[1..], env, stack);
                        }
                    }
                    // parts[0] should be a list of datums
                    if let ExprKind::List(datums) = &parts[0].kind {
                        for datum in datums {
                            let dv = expr_to_value(datum);
                            if eqv_check(&val, &dv) {
                                return sf_seq(&parts[1..], env, stack);
                            }
                        }
                    }
                }
            }
            Ok(Act::Ret(Value::Void))
        }
        Frame::DoTest { vars, steps, test, exprs, body, env } => {
            if val != Value::Boolean(false) {
                // Test passed; evaluate exprs and return last
                if exprs.is_empty() {
                    Ok(Act::Ret(Value::Void))
                } else {
                    sf_seq(&exprs, env, stack)
                }
            } else {
                // Evaluate body, then step variables (parallel update)
                // First collect current values for step expressions
                let mut new_vals = Vec::new();
                for (i, step) in steps.iter().enumerate() {
                    if let Some(step_expr) = step {
                        // Need to evaluate step_expr in current env
                        let step_val = eval(step_expr, &env)?;
                        new_vals.push((vars[i].clone(), Some(step_val)));
                    } else {
                        new_vals.push((vars[i].clone(), None));
                    }
                }
                // Execute body for side effects
                if !body.is_empty() {
                    let _ = eval_top(&body, &env)?;
                }
                // Apply parallel updates
                for (name, maybe_val) in new_vals {
                    if let Some(v) = maybe_val {
                        env_update(&env, &name, v)?;
                    }
                }
                // Loop: test again
                stack.push(Frame::DoTest { vars, steps, test: test.clone(), exprs, body, env: env.clone() });
                Ok(Act::Ev(test, env))
            }
        }
        Frame::LetrecBind { bindings, idx, body, env } => {
            // val is the result of evaluating bindings[idx-1]
            env_update(&env, &bindings[idx - 1].0, val)?;
            if idx < bindings.len() {
                stack.push(Frame::LetrecBind { bindings: bindings.clone(), idx: idx + 1, body, env: env.clone() });
                Ok(Act::Ev(bindings[idx].1.clone(), env))
            } else {
                sf_seq(&body, env, stack)
            }
        }
        Frame::DynamicWindAfterIn { body_thunk, out_thunk, in_thunk, wind_id } => {
            // in-thunk returned; push wind entry and call body
            WIND_STACK.with(|ws| ws.borrow_mut().push(WindEntry {
                id: wind_id, in_thunk, out_thunk: out_thunk.clone(),
            }));
            stack.push(Frame::DynamicWindAfterBody { out_thunk });
            Ok(Act::Ap(body_thunk, vec![]))
        }
        Frame::DynamicWindAfterBody { out_thunk } => {
            // body returned; pop wind, call out-thunk, remember body result
            WIND_STACK.with(|ws| ws.borrow_mut().pop());
            stack.push(Frame::DynamicWindAfterOut { result: val });
            Ok(Act::Ap(out_thunk, vec![]))
        }
        Frame::DynamicWindAfterOut { result } => {
            // out-thunk returned; return body's result
            Ok(Act::Ret(result))
        }
        Frame::WindTransition { mut actions, value } => {
            // A wind thunk just returned; continue with next action
            if actions.is_empty() {
                Ok(Act::Ret(value))
            } else {
                let action = actions.remove(0);
                if !actions.is_empty() {
                    stack.push(Frame::WindTransition { actions, value });
                } else {
                    // Last action — after it returns, return value
                    stack.push(Frame::WindTransition { actions: vec![], value });
                }
                match action {
                    WindAction::Unwind(out_thunk) => {
                        WIND_STACK.with(|ws| ws.borrow_mut().pop());
                        Ok(Act::Ap(out_thunk, vec![]))
                    }
                    WindAction::Rewind(entry) => {
                        let in_thunk = entry.in_thunk.clone();
                        WIND_STACK.with(|ws| ws.borrow_mut().push(entry));
                        Ok(Act::Ap(in_thunk, vec![]))
                    }
                }
            }
        }
        Frame::Guard { .. } => {
            // Body completed normally; return body's value
            Ok(Act::Ret(val))
        }
        Frame::ExceptionHandler { .. } => {
            // Thunk completed normally; discard handler, return value
            Ok(Act::Ret(val))
        }
        Frame::RaiseHandlerReturn { raised_value } => {
            // Handler returned normally without escaping; re-raise
            RAISED_VALUE.with(|rv| *rv.borrow_mut() = Some(raised_value));
            Err(EvalError::SchemeRaise)
        }
        Frame::GuardTest { raised_value, body, rest, env } => {
            if val != Value::Boolean(false) {
                if body.is_empty() {
                    Ok(Act::Ret(val))
                } else {
                    sf_seq(&body, env, stack)
                }
            } else {
                eval_guard_clauses_inner(&rest, raised_value, env, stack)
            }
        }
        Frame::GuardClauseEval { clauses, raised_value, env } => {
            // Wind transitions completed; now evaluate guard clauses
            eval_guard_clauses_inner(&clauses, raised_value, env, stack)
        }
        Frame::CallWithValuesConsumer { consumer } => {
            let args = match val {
                Value::MultipleValues(vs) => vs,
                other => vec![other],
            };
            Ok(Act::Ap(consumer, args))
        }
    }
}

fn step_apply(func: Value, args: Vec<Value>, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local = new_env(Some(env));
            if let Some(ref rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
                for (p, a) in params.iter().zip(&args) {
                    env_set(&local, p.clone(), a.clone());
                }
                env_set(&local, rest.clone(), Value::List(args[params.len()..].to_vec()));
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    )));
                }
                for (p, a) in params.iter().zip(&args) {
                    env_set(&local, p.clone(), a.clone());
                }
            }
            sf_seq(&body, local, stack)
        }
        Value::Builtin(_, f) => f(&args).map(Act::Ret),
        Value::CallCC => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc expects 1 argument".into()));
            }
            let proc = args.into_iter().next().unwrap();
            let cont = Value::Continuation(Rc::new(stack.clone()), Rc::new(wind_stack_snapshot()));
            Ok(Act::Ap(proc, vec![cont]))
        }
        Value::DynamicWind => {
            if args.len() != 3 {
                return Err(EvalError::Arity("dynamic-wind expects 3 arguments".into()));
            }
            let in_thunk = args[0].clone();
            let body_thunk = args[1].clone();
            let out_thunk = args[2].clone();
            let wind_id = WIND_ID_COUNTER.fetch_add(1, Ordering::Relaxed);
            let call_in = in_thunk.clone();
            stack.push(Frame::DynamicWindAfterIn { body_thunk, out_thunk, in_thunk, wind_id });
            Ok(Act::Ap(call_in, vec![]))
        }
        Value::Raise => {
            if args.len() != 1 {
                return Err(EvalError::Arity("raise expects 1 argument".into()));
            }
            RAISED_VALUE.with(|rv| *rv.borrow_mut() = Some(args.into_iter().next().unwrap()));
            Err(EvalError::SchemeRaise)
        }
        Value::WithExceptionHandler => {
            if args.len() != 2 {
                return Err(EvalError::Arity("with-exception-handler expects 2 arguments".into()));
            }
            let handler = args[0].clone();
            let thunk = args[1].clone();
            stack.push(Frame::ExceptionHandler { handler });
            Ok(Act::Ap(thunk, vec![]))
        }
        Value::SchemeValues => {
            if args.len() == 1 {
                Ok(Act::Ret(args.into_iter().next().unwrap()))
            } else {
                Ok(Act::Ret(Value::MultipleValues(args)))
            }
        }
        Value::CallWithValues => {
            if args.len() != 2 {
                return Err(EvalError::Arity("call-with-values expects 2 arguments".into()));
            }
            let producer = args[0].clone();
            let consumer = args[1].clone();
            stack.push(Frame::CallWithValuesConsumer { consumer });
            Ok(Act::Ap(producer, vec![]))
        }
        Value::SchemeApply => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()));
            }
            let func = args[0].clone();
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(l) => l.clone(),
                _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
            };
            let mut full: Vec<Value> = args[1..args.len() - 1].to_vec();
            full.extend(tail);
            Ok(Act::Ap(func, full))
        }
        Value::Continuation(frames, target_winds) => {
            if args.is_empty() {
                return Err(EvalError::Arity("continuation expects 1 argument".into()));
            }
            let value = args.into_iter().next().unwrap();
            let current_winds = wind_stack_snapshot();

            // Find common prefix length
            let common = current_winds.iter().zip(target_winds.iter())
                .take_while(|(a, b)| a.id == b.id)
                .count();

            // Build wind transition actions
            let mut actions: Vec<WindAction> = Vec::new();
            // Unwind from innermost to common
            for entry in current_winds[common..].iter().rev() {
                actions.push(WindAction::Unwind(entry.out_thunk.clone()));
            }
            // Rewind from common to innermost
            for entry in &target_winds[common..] {
                actions.push(WindAction::Rewind(entry.clone()));
            }

            *stack = (*frames).clone();

            if actions.is_empty() {
                Ok(Act::Ret(value))
            } else {
                let first = actions.remove(0);
                if !actions.is_empty() {
                    stack.push(Frame::WindTransition { actions, value: value.clone() });
                }
                match first {
                    WindAction::Unwind(out_thunk) => {
                        WIND_STACK.with(|ws| ws.borrow_mut().pop());
                        Ok(Act::Ap(out_thunk, vec![]))
                    }
                    WindAction::Rewind(entry) => {
                        let in_thunk = entry.in_thunk.clone();
                        WIND_STACK.with(|ws| ws.borrow_mut().push(entry));
                        Ok(Act::Ap(in_thunk, vec![]))
                    }
                }
            }
        }
        _ => Err(EvalError::Type(format!("not a procedure: {func}"))),
    }
}

// ── Special form helpers ──

/// Parse a parameter list, handling dot notation for rest params.
fn parse_params(param_exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse("expected parameter after dot".into()));
                }
                rest_param = Some(match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("expected symbol after dot".into())),
                });
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(EvalError::Type("expected symbol in params".into())),
        }
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires params and body".into()));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(elems) => parse_params(elems)?,
        ExprKind::Symbol(s) => {
            // (lambda args body) — all args collected as rest
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn is_scheme_raise(e: &EvalError) -> bool {
    match e {
        EvalError::SchemeRaise => true,
        EvalError::WithPosition { inner, .. } => is_scheme_raise(inner),
        _ => false,
    }
}

fn handle_raise(stack: &mut Vec<Frame>, raised_val: Value) -> Result<Act, EvalError> {
    // Search stack from top for nearest Guard or ExceptionHandler
    let mut handler_idx = None;
    for i in (0..stack.len()).rev() {
        match &stack[i] {
            Frame::Guard { .. } | Frame::ExceptionHandler { .. } => {
                handler_idx = Some(i);
                break;
            }
            _ => {}
        }
    }

    let idx = match handler_idx {
        Some(i) => i,
        None => return Err(EvalError::Type(format!("unhandled exception: {}", raised_val))),
    };

    let is_guard = matches!(&stack[idx], Frame::Guard { .. });

    if !is_guard {
        // ExceptionHandler: remove it, call handler with stack intact
        let frame = stack.remove(idx);
        let handler = match frame {
            Frame::ExceptionHandler { handler } => handler,
            _ => unreachable!(),
        };
        stack.push(Frame::RaiseHandlerReturn { raised_value: raised_val.clone() });
        Ok(Act::Ap(handler, vec![raised_val]))
    } else {
        // Guard: pop all frames above and including Guard, collect wind unwinds
        let mut out_thunks = Vec::new();
        while stack.len() > idx + 1 {
            let frame = stack.pop().unwrap();
            if let Frame::DynamicWindAfterBody { out_thunk } = frame {
                out_thunks.push(out_thunk);
            }
        }
        // Pop the Guard frame itself
        let guard_frame = stack.pop().unwrap();
        let (var, clauses, env) = match guard_frame {
            Frame::Guard { var, clauses, env } => (var, clauses, env),
            _ => unreachable!(),
        };

        let guard_env = new_env(Some(env));
        env_set(&guard_env, var, raised_val.clone());

        if out_thunks.is_empty() {
            eval_guard_clauses_inner(&clauses, raised_val, guard_env, stack)
        } else {
            stack.push(Frame::GuardClauseEval { clauses, raised_value: raised_val, env: guard_env });
            let actions: Vec<WindAction> = out_thunks.into_iter().map(WindAction::Unwind).collect();
            stack.push(Frame::WindTransition { actions, value: Value::Void });
            Ok(Act::Ret(Value::Void))
        }
    }
}

fn eval_guard_clauses_inner(clauses: &[Expr], raised_value: Value, env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if clauses.is_empty() {
        // No clause matched; re-raise
        RAISED_VALUE.with(|rv| *rv.borrow_mut() = Some(raised_value));
        return Err(EvalError::SchemeRaise);
    }
    match &clauses[0].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            if let ExprKind::Symbol(s) = &parts[0].kind {
                if s == "else" {
                    return sf_seq(&parts[1..], env, stack);
                }
            }
            stack.push(Frame::GuardTest {
                raised_value,
                body: parts[1..].to_vec(),
                rest: clauses[1..].to_vec(),
                env: env.clone(),
            });
            Ok(Act::Ev(parts[0].clone(), env))
        }
        _ => Err(EvalError::Type("guard: invalid clause".into())),
    }
}

fn sf_guard(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("guard requires arguments".into()));
    }
    let guard_spec = match &args[0].kind {
        ExprKind::List(spec) => spec,
        _ => return Err(EvalError::Type("guard: expected (var clause ...)".into())),
    };
    if guard_spec.is_empty() {
        return Err(EvalError::Type("guard: expected variable".into()));
    }
    let var = match &guard_spec[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("guard: expected symbol as variable".into())),
    };
    let clauses = guard_spec[1..].to_vec();
    let body = args[1..].to_vec();

    stack.push(Frame::Guard { var, clauses, env: env.clone() });
    sf_seq(&body, env, stack)
}

fn sf_define(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires arguments".into()));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define expects 2 arguments".into()));
            }
            stack.push(Frame::Define(name.clone(), env.clone()));
            Ok(Act::Ev(args[1].clone(), env))
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol as function name".into())),
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
            env_set(&env, name, lambda);
            Ok(Act::Ret(Value::Void))
        }
        _ => Err(EvalError::Type("define: expected symbol or list".into())),
    }
}

fn sf_and(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Ok(Act::Ret(Value::Boolean(true)));
    }
    if args.len() == 1 {
        return Ok(Act::Ev(args[0].clone(), env));
    }
    stack.push(Frame::And(args[1..].to_vec(), env.clone()));
    Ok(Act::Ev(args[0].clone(), env))
}

fn sf_or(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Ok(Act::Ret(Value::Boolean(false)));
    }
    if args.len() == 1 {
        return Ok(Act::Ev(args[0].clone(), env));
    }
    stack.push(Frame::Or(args[1..].to_vec(), env.clone()));
    Ok(Act::Ev(args[0].clone(), env))
}

fn sf_let(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into()));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into()));
        }
        let bl = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Type("let: expected bindings list".into())),
        };
        let mut bindings = Vec::new();
        for b in bl {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        bindings.push((s.clone(), pair[1].clone()));
                    } else {
                        return Err(EvalError::Type("let: expected symbol in binding".into()));
                    }
                }
                _ => return Err(EvalError::Type("let: invalid binding".into())),
            }
        }
        let body = args[2..].to_vec();
        let params: Vec<String> = bindings.iter().map(|(v, _)| v.clone()).collect();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda { params, rest_param: None, body: body.clone(), env: local_env.clone() };
        env_set(&local_env, name.clone(), lambda);
        if bindings.is_empty() {
            return sf_seq(&body, local_env, stack);
        }
        let (first_var, first_expr) = bindings.remove(0);
        stack.push(Frame::LetBind { var: first_var, remaining: bindings, body, outer_env: env.clone(), local_env });
        return Ok(Act::Ev(first_expr, env));
    }
    // Regular let
    let bl = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let mut bindings = Vec::new();
    for b in bl {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    bindings.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("let: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env.clone()));
    if bindings.is_empty() {
        return sf_seq(&body, local_env, stack);
    }
    let (first_var, first_expr) = bindings.remove(0);
    stack.push(Frame::LetBind { var: first_var, remaining: bindings, body, outer_env: env.clone(), local_env });
    Ok(Act::Ev(first_expr, env))
}

fn sf_seq(exprs: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if exprs.is_empty() {
        return Ok(Act::Ret(Value::Void));
    }
    if exprs.len() == 1 {
        return Ok(Act::Ev(exprs[0].clone(), env));
    }
    stack.push(Frame::Seq(exprs[1..].to_vec(), env.clone()));
    Ok(Act::Ev(exprs[0].clone(), env))
}

fn sf_cond(clauses: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    sf_cond_clauses(clauses, env, stack)
}

fn sf_cond_clauses(clauses: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if clauses.is_empty() {
        return Ok(Act::Ret(Value::Void));
    }
    match &clauses[0].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            if let ExprKind::Symbol(s) = &parts[0].kind {
                if s == "else" {
                    return sf_seq(&parts[1..], env, stack);
                }
            }
            stack.push(Frame::CondTest { body: parts[1..].to_vec(), rest: clauses[1..].to_vec(), env: env.clone() });
            Ok(Act::Ev(parts[0].clone(), env))
        }
        _ => Err(EvalError::Type("cond: invalid clause".into())),
    }
}

fn sf_let_star(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bl = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Type("let*: expected bindings list".into())),
    };
    let mut bindings = Vec::new();
    for b in bl {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    bindings.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("let*: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("let*: invalid binding".into())),
        }
    }
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env.clone()));
    if bindings.is_empty() {
        return sf_seq(&body, local_env, stack);
    }
    // For let*, each binding is evaluated in the local_env (sequential visibility)
    let (first_var, first_expr) = bindings.remove(0);
    stack.push(Frame::LetBind { var: first_var, remaining: bindings, body, outer_env: local_env.clone(), local_env });
    Ok(Act::Ev(first_expr, env))
}

fn sf_letrec(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bl = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let mut bindings = Vec::new();
    for b in bl {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    bindings.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("letrec: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env));
    // Pre-bind all variables to void so they're mutually visible
    for (name, _) in &bindings {
        env_set(&local_env, name.clone(), Value::Void);
    }
    if bindings.is_empty() {
        return sf_seq(&body, local_env, stack);
    }
    // Evaluate all init expressions in the local_env
    stack.push(Frame::LetrecBind { bindings: bindings.clone(), idx: 1, body, env: local_env.clone() });
    Ok(Act::Ev(bindings[0].1.clone(), local_env))
}

fn sf_letrec_star(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bl = match &args[0].kind {
        ExprKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let mut bindings = Vec::new();
    for b in bl {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    bindings.push((s.clone(), pair[1].clone()));
                } else {
                    return Err(EvalError::Type("letrec*: expected symbol in binding".into()));
                }
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let body = args[1..].to_vec();
    let local_env = new_env(Some(env));
    // Pre-bind all variables to void
    for (name, _) in &bindings {
        env_set(&local_env, name.clone(), Value::Void);
    }
    if bindings.is_empty() {
        return sf_seq(&body, local_env, stack);
    }
    // For letrec*, evaluate sequentially in local_env, updating as we go
    let (first_var, first_expr) = bindings.remove(0);
    stack.push(Frame::LetBind { var: first_var, remaining: bindings, body, outer_env: local_env.clone(), local_env: local_env.clone() });
    Ok(Act::Ev(first_expr, local_env))
}

fn sf_case(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires a key and clauses".into()));
    }
    let clauses = args[1..].to_vec();
    stack.push(Frame::CaseKey { clauses, env: env.clone() });
    Ok(Act::Ev(args[0].clone(), env))
}

fn sf_do(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable bindings and test".into()));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(v) => v,
        _ => return Err(EvalError::Type("do: expected variable list".into())),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(t) => t,
        _ => return Err(EvalError::Type("do: expected test clause".into())),
    };
    if test_clause.is_empty() {
        return Err(EvalError::Type("do: test clause must have at least a test expression".into()));
    }
    let body = args[2..].to_vec();
    let local_env = new_env(Some(env.clone()));
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Expr>> = Vec::new();
    // Evaluate init expressions in the outer env, bind in local_env
    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("do: expected symbol".into())),
                };
                let init_val = eval(&parts[1], &env)?;
                env_set(&local_env, name.clone(), init_val);
                var_names.push(name);
                if parts.len() >= 3 {
                    step_exprs.push(Some(parts[2].clone()));
                } else {
                    step_exprs.push(None);
                }
            }
            _ => return Err(EvalError::Type("do: invalid variable spec".into())),
        }
    }
    let test = test_clause[0].clone();
    let exprs = test_clause[1..].to_vec();
    // Evaluate test
    stack.push(Frame::DoTest { vars: var_names, steps: step_exprs, test: test.clone(), exprs, body, env: local_env.clone() });
    Ok(Act::Ev(test, local_env))
}

fn sf_when(args: &[Expr], env: Env, stack: &mut Vec<Frame>) -> Result<Act, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("when requires a test and body".into()));
    }
    stack.push(Frame::IfTest { then_br: {
        let body = args[1..].to_vec();
        if body.len() == 1 {
            body[0].clone()
        } else {
            Expr { kind: ExprKind::List(
                std::iter::once(Expr { kind: ExprKind::Symbol("begin".into()), line: args[0].line, col: args[0].col })
                    .chain(body.into_iter())
                    .collect()
            ), line: args[0].line, col: args[0].col }
        }
    }, else_br: None, env: env.clone() });
    Ok(Act::Ev(args[0].clone(), env))
}

fn eqv_check(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

// ── Builtins ──

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not expects 1 argument".into()));
    }
    Ok(Value::Boolean(args[0] == Value::Boolean(false)))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons expects 2 arguments".into()));
    }
    match &args[1] {
        Value::List(elems) => {
            let mut new = vec![args[0].clone()];
            new.extend(elems.iter().cloned());
            Ok(Value::List(new))
        }
        Value::Pair(..) => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
        _ => Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone()))),
    }
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
        Value::Pair(a, _) => Ok(*a.clone()),
        _ => Err(EvalError::Type("car: expected pair".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) if !elems.is_empty() => Ok(Value::List(elems[1..].to_vec())),
        Value::Pair(_, b) => Ok(*b.clone()),
        _ => Err(EvalError::Type("cdr: expected pair".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? expects 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(&args[0], Value::List(l) if l.is_empty())))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::List(args.to_vec()))
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
        _ => Err(EvalError::Type("length: expected list".into())),
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            Value::List(elems) => result.extend(elems.iter().cloned()),
            _ => return Err(EvalError::Type("append: expected list".into())),
        }
    }
    Ok(Value::List(result))
}

fn builtin_type_pred(args: &[Value], name: &str) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("{name} expects 1 argument")));
    }
    let result = match name {
        "string?" => matches!(&args[0], Value::String(_, _)),
        "number?" => matches!(&args[0], Value::Integer(_)),
        "boolean?" => matches!(&args[0], Value::Boolean(_)),
        "pair?" => matches!(&args[0], Value::List(l) if !l.is_empty()) || matches!(&args[0], Value::Pair(..)),
        "symbol?" => matches!(&args[0], Value::Symbol(_)),
        "char?" => matches!(&args[0], Value::Char(_)),
        "vector?" => matches!(&args[0], Value::Vector(_)),
        "procedure?" => matches!(&args[0], Value::Lambda { .. } | Value::Builtin(..) | Value::CallCC | Value::DynamicWind | Value::SchemeApply | Value::Continuation(..) | Value::SchemeValues | Value::CallWithValues),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn require_ints(args: &[Value]) -> Result<Vec<i64>, EvalError> {
    args.iter()
        .map(|v| match v {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected integer, got {v}"))),
        })
        .collect()
}

fn arith_add(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().sum()))
}

fn arith_sub(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if nums.len() == 1 {
        Ok(Value::Integer(-nums[0]))
    } else {
        Ok(Value::Integer(nums[0] - nums[1..].iter().sum::<i64>()))
    }
}

fn arith_mul(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums.iter().product()))
}

fn arith_div(args: &[Value]) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("/ requires at least 2 arguments".into()));
    }
    let mut result = nums[0];
    for &d in &nums[1..] {
        if d == 0 {
            return Err(EvalError::Type("division by zero".into()));
        }
        result /= d;
    }
    Ok(Value::Integer(result))
}

fn cmp_op(args: &[Value], op: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    let nums = require_ints(args)?;
    if nums.len() < 2 {
        return Err(EvalError::Arity("comparison requires at least 2 arguments".into()));
    }
    let result = nums.windows(2).all(|w| op(w[0], w[1]));
    Ok(Value::Boolean(result))
}

fn builtin_display(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display expects 1 argument".into()));
    }
    let mut s = String::new();
    args[0].display_fmt(&mut s);
    output_write(&s);
    Ok(Value::Void)
}

fn builtin_write(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write expects 1 argument".into()));
    }
    output_write(&args[0].to_string());
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline expects 0 arguments".into()));
    }
    output_write("\n");
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = std::string::String::new();
    for arg in args {
        match arg {
            Value::String(s, _) => result.push_str(&s.borrow()),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(make_string(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s, _) => Ok(Value::Integer(s.borrow().len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s, _), Value::Integer(start), Value::Integer(end)) => {
            let s = s.borrow();
            let start = *start as usize;
            let end = *end as usize;
            if start > end || end > s.len() {
                return Err(EvalError::Type("substring: index out of range".into()));
            }
            Ok(make_string(s[start..end].to_string()))
        }
        _ => Err(EvalError::Type("substring: expected string and two integers".into())),
    }
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s, _) => match s.borrow().parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string expects 1 argument".into()));
    }
    match &args[0] {
        Value::Integer(n) => Ok(make_string(n.to_string())),
        _ => Err(EvalError::Type("number->string: expected number".into())),
    }
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string expects 1 argument".into()));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(make_string(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s, _) => Ok(Value::Symbol(s.borrow().clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref expects 2 arguments".into()));
    }
    match (&args[0], &args[1]) {
        (Value::String(s, _), Value::Integer(idx)) => {
            let idx = *idx as usize;
            s.borrow().chars().nth(idx)
                .map(Value::Char)
                .ok_or_else(|| EvalError::Type("string-ref: index out of range".into()))
        }
        _ => Err(EvalError::Type("string-ref: expected string and integer".into())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s, _) => Ok(make_string(s.borrow().clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_string_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s, immutable), Value::Integer(idx), Value::Char(c)) => {
            if *immutable {
                return Err(EvalError::Type("string-set!: strings are immutable".into()));
            }
            let idx = *idx as usize;
            let mut borrowed = s.borrow_mut();
            if idx >= borrowed.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            // Replace the character at position idx
            let mut chars: Vec<char> = borrowed.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            chars[idx] = *c;
            *borrowed = chars.into_iter().collect();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string, integer, char".into())),
    }
}

fn builtin_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->list expects 1 argument".into()));
    }
    match &args[0] {
        Value::String(s, _) => {
            let chars: Vec<Value> = s.borrow().chars().map(Value::Char).collect();
            Ok(Value::List(chars))
        }
        _ => Err(EvalError::Type("string->list: expected string".into())),
    }
}

fn builtin_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("list->string expects 1 argument".into()));
    }
    match &args[0] {
        Value::List(items) => {
            let mut s = String::new();
            for item in items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected list of characters".into())),
                }
            }
            Ok(make_string(s))
        }
        _ => Err(EvalError::Type("list->string: expected list".into())),
    }
}

fn builtin_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char->integer expects 1 argument".into()));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        _ => Err(EvalError::Type("char->integer: expected char".into())),
    }
}

fn builtin_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("integer->char expects 1 argument".into()));
    }
    match &args[0] {
        Value::Integer(n) => {
            let c = char::from_u32(*n as u32)
                .ok_or_else(|| EvalError::Type("integer->char: invalid code point".into()))?;
            Ok(Value::Char(c))
        }
        _ => Err(EvalError::Type("integer->char: expected integer".into())),
    }
}

fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eq? expects 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Void, Value::Void) => true,
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::String(a, _), Value::String(b, _)) => Rc::ptr_eq(a, b),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn builtin_equal(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("equal? expects 2 arguments".into())); }
    Ok(Value::Boolean(args[0] == args[1]))
}

fn builtin_abs(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("abs expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(n.abs())),
        _ => Err(EvalError::Type("abs: expected integer".into())),
    }
}

fn builtin_modulo(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("modulo expects 2 arguments".into())); }
    let nums = require_ints(args)?;
    if nums[1] == 0 { return Err(EvalError::Type("modulo: division by zero".into())); }
    Ok(Value::Integer(((nums[0] % nums[1]) + nums[1]) % nums[1]))
}

fn builtin_remainder(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("remainder expects 2 arguments".into())); }
    let nums = require_ints(args)?;
    if nums[1] == 0 { return Err(EvalError::Type("remainder: division by zero".into())); }
    Ok(Value::Integer(nums[0] % nums[1]))
}

fn builtin_quotient(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("quotient expects 2 arguments".into())); }
    let nums = require_ints(args)?;
    if nums[1] == 0 { return Err(EvalError::Type("quotient: division by zero".into())); }
    Ok(Value::Integer(nums[0] / nums[1]))
}

fn builtin_min(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("min expects at least 1 argument".into())); }
    let nums = require_ints(args)?;
    Ok(Value::Integer(*nums.iter().min().unwrap()))
}

fn builtin_max(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("max expects at least 1 argument".into())); }
    let nums = require_ints(args)?;
    Ok(Value::Integer(*nums.iter().max().unwrap()))
}

fn builtin_expt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("expt expects 2 arguments".into())); }
    let nums = require_ints(args)?;
    Ok(Value::Integer(nums[0].pow(nums[1] as u32)))
}

fn builtin_zero(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("zero? expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n == 0)),
        _ => Err(EvalError::Type("zero?: expected integer".into())),
    }
}

fn builtin_positive(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive? expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n > 0)),
        _ => Err(EvalError::Type("positive?: expected integer".into())),
    }
}

fn builtin_negative(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative? expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(*n < 0)),
        _ => Err(EvalError::Type("negative?: expected integer".into())),
    }
}

fn builtin_odd(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("odd? expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 != 0)),
        _ => Err(EvalError::Type("odd?: expected integer".into())),
    }
}

fn builtin_even(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("even? expects 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Boolean(n % 2 == 0)),
        _ => Err(EvalError::Type("even?: expected integer".into())),
    }
}

fn builtin_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-ref expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::List(elems), Value::Integer(idx)) => {
            let idx = *idx as usize;
            elems.get(idx).cloned().ok_or_else(|| EvalError::Type("list-ref: index out of range".into()))
        }
        _ => Err(EvalError::Type("list-ref: expected list and integer".into())),
    }
}

fn builtin_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-tail expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::List(elems), Value::Integer(idx)) => {
            let idx = *idx as usize;
            if idx > elems.len() { return Err(EvalError::Type("list-tail: index out of range".into())); }
            Ok(Value::List(elems[idx..].to_vec()))
        }
        _ => Err(EvalError::Type("list-tail: expected list and integer".into())),
    }
}

fn builtin_list_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list? expects 1 argument".into())); }
    Ok(Value::Boolean(matches!(&args[0], Value::List(_))))
}

fn builtin_assoc(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assoc expects 2 arguments".into())); }
    let key = &args[0];
    match &args[1] {
        Value::List(alist) => {
            for item in alist {
                match item {
                    Value::List(pair) if !pair.is_empty() && pair[0] == *key => {
                        return Ok(item.clone());
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("assoc: expected list".into())),
    }
}

fn builtin_char_alphabetic(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? expects 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
        _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
    }
}

fn builtin_char_numeric(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-numeric? expects 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
        _ => Err(EvalError::Type("char-numeric?: expected char".into())),
    }
}

fn builtin_char_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-upcase expects 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
        _ => Err(EvalError::Type("char-upcase: expected char".into())),
    }
}

fn builtin_char_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-downcase expects 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
        _ => Err(EvalError::Type("char-downcase: expected char".into())),
    }
}

fn builtin_char_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char=? expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("char=?: expected chars".into())),
    }
}

fn builtin_char_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<? expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("char<?: expected chars".into())),
    }
}

fn builtin_string_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string=? expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(*a.borrow() == *b.borrow())),
        _ => Err(EvalError::Type("string=?: expected strings".into())),
    }
}

fn builtin_string_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<? expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(*a.borrow() < *b.borrow())),
        _ => Err(EvalError::Type("string<?: expected strings".into())),
    }
}

fn builtin_string_ci_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ci=? expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::String(a, _), Value::String(b, _)) => Ok(Value::Boolean(a.borrow().to_lowercase() == b.borrow().to_lowercase())),
        _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
    }
}

fn builtin_string_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-upcase expects 1 argument".into())); }
    match &args[0] {
        Value::String(s, _) => Ok(make_string(s.borrow().to_uppercase())),
        _ => Err(EvalError::Type("string-upcase: expected string".into())),
    }
}

fn builtin_string_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-downcase expects 1 argument".into())); }
    match &args[0] {
        Value::String(s, _) => Ok(make_string(s.borrow().to_lowercase())),
        _ => Err(EvalError::Type("string-downcase: expected string".into())),
    }
}

fn builtin_eqv(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eqv? expects 2 arguments".into())); }
    Ok(Value::Boolean(eqv_check(&args[0], &args[1])))
}

fn builtin_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

fn builtin_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-vector expects 1 or 2 arguments".into()));
    }
    let size = match &args[0] {
        Value::Integer(n) => *n as usize,
        _ => return Err(EvalError::Type("make-vector: expected integer".into())),
    };
    let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; size]))))
}

fn builtin_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-ref expects 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Vector(v), Value::Integer(idx)) => {
            let v = v.borrow();
            let idx = *idx as usize;
            v.get(idx).cloned().ok_or_else(|| EvalError::Type("vector-ref: index out of range".into()))
        }
        _ => Err(EvalError::Type("vector-ref: expected vector and integer".into())),
    }
}

fn builtin_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set! expects 3 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Vector(v), Value::Integer(idx)) => {
            let mut v = v.borrow_mut();
            let idx = *idx as usize;
            if idx >= v.len() {
                return Err(EvalError::Type("vector-set!: index out of range".into()));
            }
            v[idx] = args[2].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector and integer".into())),
    }
}

fn builtin_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector-length expects 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
        _ => Err(EvalError::Type("vector-length: expected vector".into())),
    }
}

fn builtin_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector->list expects 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
        _ => Err(EvalError::Type("vector->list: expected vector".into())),
    }
}

fn builtin_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->vector expects 1 argument".into())); }
    match &args[0] {
        Value::List(l) => Ok(Value::Vector(Rc::new(RefCell::new(l.clone())))),
        _ => Err(EvalError::Type("list->vector: expected list".into())),
    }
}

fn builtin_memq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("memq expects 2 arguments".into())); }
    let key = &args[0];
    match &args[1] {
        Value::List(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                if eqv_check(key, elem) {
                    return Ok(Value::List(elems[i..].to_vec()));
                }
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("memq: expected list".into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    wind_stack_reset();
    raised_value_reset();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = default_env();
    let result = eval_top(&exprs, &env)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    // Clear any stale state
    output_take();
    wind_stack_reset();
    raised_value_reset();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into()));
    }
    let env = default_env();
    let result = eval_top(&exprs, &env)?;
    let output = output_take();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
