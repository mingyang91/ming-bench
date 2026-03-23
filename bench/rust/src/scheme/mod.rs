pub mod error;

pub use error::EvalError;

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

/// A dynamic-wind entry tracking in/out thunks for the current dynamic extent.
#[derive(Clone, Debug)]
struct WindEntry {
    id: u64,
    in_thunk: Value,
    out_thunk: Value,
}

static WIND_ID_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_wind_id() -> u64 {
    WIND_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

fn next_record_type_id() -> u64 {
    RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Inner pair storage with iterative drop to prevent stack overflow on long chains.
#[derive(Debug, Clone)]
struct PairCell(Value, Value);

impl Drop for PairCell {
    fn drop(&mut self) {
        let mut cdr = std::mem::replace(&mut self.1, Value::Void);
        while let Value::Pair(rc) = cdr {
            match std::rc::Rc::try_unwrap(rc) {
                Ok(cell) => {
                    let mut inner = cell.into_inner();
                    cdr = std::mem::replace(&mut inner.1, Value::Void);
                    // inner.0 (car) drops normally; inner.1 is Void so no deep recursion
                }
                Err(_) => break,
            }
        }
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
    List(Vec<Value>),
    Pair(std::rc::Rc<std::cell::RefCell<PairCell>>),
    Void,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation(Vec<KontFrame>, Vec<WindEntry>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Vector(std::rc::Rc<std::cell::RefCell<Vec<Value>>>),
    /// Multiple return values from `values`.
    Values(Vec<Value>),
    /// Record instance: type_id, field values
    Record { type_id: u64, fields: Vec<Value> },
    /// Record constructor: type_id, type_name, field_names
    RecordConstructor { type_id: u64, type_name: String, field_names: Vec<String> },
    /// Record predicate: type_id
    RecordPredicate { type_id: u64 },
    /// Record accessor: type_id, type_name, field_index
    RecordAccessor { type_id: u64, type_name: String, field_index: usize, field_name: String },
    /// Syntax object (wraps an Expr for syntax-case macros)
    Syntax(Expr),
    /// Macro transformer procedure (lambda-based macro from syntax-case)
    MacroTransformer(Box<Value>),
    /// case-lambda: multiple clauses with different arities
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>, // (params, rest_param, body)
        env: Env,
    },
}

thread_local! {
    static OUTPUT_BUFFER: std::cell::RefCell<String> = std::cell::RefCell::new(String::new());
    /// Stack of syntax-case pattern bindings for use by `(syntax ...)` / `#'`.
    static SYNTAX_BINDINGS: std::cell::RefCell<Vec<SyntaxBindingEntry>> = std::cell::RefCell::new(Vec::new());
    /// Stack of use-site environments for macro transformer hygiene.
    static MACRO_USE_ENV: std::cell::RefCell<Vec<Env>> = std::cell::RefCell::new(Vec::new());
    /// Step counter for eval_str_with_limit. 0 means unlimited.
    static STEP_LIMIT: std::cell::Cell<u64> = std::cell::Cell::new(0);
    static STEP_COUNT: std::cell::Cell<u64> = std::cell::Cell::new(0);
}

struct SyntaxBindingEntry {
    bindings: HashMap<String, Vec<Expr>>,
    ellipsis_vars: HashSet<String>,
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{}", f);
                if s.contains('.') || s.contains('e') || s.contains('E') || s.contains("inf") || s.contains("NaN") { s } else { format!("{}.0", s) }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => match *c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{}", c),
            },
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(p) => {
                let mut parts = vec![];
                let mut cur = self.clone();
                let mut seen = std::collections::HashSet::new();
                loop {
                    match &cur {
                        Value::Pair(p) => {
                            let ptr = std::rc::Rc::as_ptr(p) as usize;
                            if !seen.insert(ptr) {
                                parts.push("...".to_string());
                                break;
                            }
                            let inner = p.borrow();
                            parts.push(inner.0.display());
                            let next = inner.1.clone();
                            drop(inner);
                            cur = next;
                        }
                        Value::List(items) if items.is_empty() => break,
                        Value::List(items) => {
                            for item in items {
                                parts.push(item.display());
                            }
                            break;
                        }
                        other => {
                            parts.push(format!(". {}", other.display()));
                            break;
                        }
                    }
                }
                format!("({})", parts.join(" "))
            }
            Value::Void => "#<void>".to_string(),
            Value::Lambda { .. } | Value::CaseLambda { .. } => "#<procedure>".to_string(),
            Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Continuation(..) => "#<continuation>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Values(vs) => {
                let inner: Vec<String> = vs.iter().map(|v| v.display()).collect();
                format!("#<values: {}>", inner.join(" "))
            }
            Value::Record { .. } => "#<record>".to_string(),
            Value::RecordConstructor { .. } | Value::RecordPredicate { .. } | Value::RecordAccessor { .. } => "#<procedure>".to_string(),
            Value::Syntax(_) => "#<syntax>".to_string(),
            Value::MacroTransformer(_) => "#<macro>".to_string(),
        }
    }

    /// Display without quotes (for `display`).
    fn display_write(&self, write_mode: bool) -> String {
        match self {
            Value::Str(s) if write_mode => format!("\"{}\"", s),
            Value::Str(s) => s.clone(),
            Value::Char(c) if write_mode => match *c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{}", c),
            },
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_write(write_mode)).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => {
                let mut parts = vec![];
                let mut cur = self.clone();
                let mut seen = std::collections::HashSet::new();
                loop {
                    match &cur {
                        Value::Pair(p) => {
                            let ptr = std::rc::Rc::as_ptr(p) as usize;
                            if !seen.insert(ptr) {
                                parts.push("...".to_string());
                                break;
                            }
                            let inner = p.borrow();
                            parts.push(inner.0.display_write(write_mode));
                            let next = inner.1.clone();
                            drop(inner);
                            cur = next;
                        }
                        Value::List(items) if items.is_empty() => break,
                        Value::List(items) => {
                            for item in items {
                                parts.push(item.display_write(write_mode));
                            }
                            break;
                        }
                        other => {
                            parts.push(format!(". {}", other.display_write(write_mode)));
                            break;
                        }
                    }
                }
                format!("({})", parts.join(" "))
            }
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display_write(write_mode)).collect();
                format!("#({})", inner.join(" "))
            }
            _ => self.display(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(std::rc::Rc::new(std::cell::RefCell::new(PairCell(car, cdr))))
}

fn list_from_vec(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(items) if items.is_empty() => Some(vec![]),
        Value::List(items) => Some(items.clone()),
        Value::Pair(p) => {
            let mut result = vec![];
            let mut cur = val.clone();
            let mut seen = std::collections::HashSet::new();
            loop {
                match &cur {
                    Value::Pair(p) => {
                        let ptr = std::rc::Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) {
                            return None; // cycle
                        }
                        let inner = p.borrow();
                        result.push(inner.0.clone());
                        let next = inner.1.clone();
                        drop(inner);
                        cur = next;
                    }
                    Value::List(items) if items.is_empty() => return Some(result),
                    Value::List(items) => {
                        result.extend(items.iter().cloned());
                        return Some(result);
                    }
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

fn is_nil(val: &Value) -> bool {
    matches!(val, Value::List(items) if items.is_empty())
}

// --- Environment ---

type Env = std::rc::Rc<std::cell::RefCell<EnvInner>>;

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
    std::rc::Rc::new(std::cell::RefCell::new(EnvInner {
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

fn env_set_existing(env: &Env, name: &str, val: Value) -> bool {
    let has_key = env.borrow().bindings.contains_key(name);
    if has_key {
        env.borrow_mut().bindings.insert(name.to_string(), val);
        true
    } else {
        let parent = env.borrow().parent.clone();
        if let Some(ref p) = parent {
            env_set_existing(p, name, val)
        } else {
            false
        }
    }
}

// --- Parser ---

fn skip_whitespace(input: &[u8], pos: usize) -> usize {
    let mut i = pos;
    while i < input.len() {
        if input[i].is_ascii_whitespace() {
            i += 1;
        } else if input[i] == b';' {
            while i < input.len() && input[i] != b'\n' {
                i += 1;
            }
        } else {
            break;
        }
    }
    i
}

/// Compute (line, col) from byte offset. Both 1-based.
fn line_col(input: &[u8], offset: usize) -> (u32, u32) {
    let mut line = 1u32;
    let mut col = 1u32;
    for &b in &input[..offset.min(input.len())] {
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
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

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    line: u32,
    col: u32,
}

impl Expr {
    fn new(kind: ExprKind, line: u32, col: u32) -> Self {
        Expr { kind, line, col }
    }
}

fn parse_expr(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let i = skip_whitespace(input, pos);
    if i >= input.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let (l, c) = line_col(input, i);

    match input[i] {
        b'(' => parse_list(input, i),
        b'\'' => {
            let (expr, next) = parse_expr(input, i + 1)?;
            Ok((
                Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("quote".into()), l, c),
                        expr,
                    ]),
                    l,
                    c,
                ),
                next,
            ))
        }
        b'`' => {
            let (expr, next) = parse_expr(input, i + 1)?;
            Ok((
                Expr::new(
                    ExprKind::List(vec![
                        Expr::new(ExprKind::Symbol("quasiquote".into()), l, c),
                        expr,
                    ]),
                    l,
                    c,
                ),
                next,
            ))
        }
        b',' => {
            if i + 1 < input.len() && input[i + 1] == b'@' {
                let (expr, next) = parse_expr(input, i + 2)?;
                Ok((
                    Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol("unquote-splicing".into()), l, c),
                            expr,
                        ]),
                        l,
                        c,
                    ),
                    next,
                ))
            } else {
                let (expr, next) = parse_expr(input, i + 1)?;
                Ok((
                    Expr::new(
                        ExprKind::List(vec![
                            Expr::new(ExprKind::Symbol("unquote".into()), l, c),
                            expr,
                        ]),
                        l,
                        c,
                    ),
                    next,
                ))
            }
        }
        b'"' => parse_string(input, i),
        b'#' => {
            if i + 1 < input.len() {
                match input[i + 1] {
                    b't' => Ok((Expr::new(ExprKind::Boolean(true), l, c), i + 2)),
                    b'f' => Ok((Expr::new(ExprKind::Boolean(false), l, c), i + 2)),
                    b'\\' => {
                        // Character literal: #\x, #\space, #\newline, #\tab
                        if i + 2 >= input.len() {
                            return Err(EvalError::Parse(format!(
                                "{l}:{c}: unexpected end after #\\"
                            )));
                        }
                        let start = i + 2;
                        let mut end = start;
                        while end < input.len() && input[end].is_ascii_alphabetic() {
                            end += 1;
                        }
                        if end == start {
                            // Single non-alpha char like #\( or #\)
                            let ch = input[start] as char;
                            Ok((Expr::new(ExprKind::Char(ch), l, c), start + 1))
                        } else {
                            let name = std::str::from_utf8(&input[start..end]).unwrap();
                            if name.len() == 1 {
                                Ok((Expr::new(ExprKind::Char(name.chars().next().unwrap()), l, c), end))
                            } else {
                                let ch = match name.to_lowercase().as_str() {
                                    "space" => ' ',
                                    "newline" => '\n',
                                    "tab" => '\t',
                                    _ => {
                                        return Err(EvalError::Parse(format!(
                                            "{l}:{c}: unknown character name: {name}"
                                        )))
                                    }
                                };
                                Ok((Expr::new(ExprKind::Char(ch), l, c), end))
                            }
                        }
                    }
                    b'(' => {
                        // Vector literal #(...)
                        let (list_expr, next) = parse_list(input, i + 1)?;
                        match list_expr.kind {
                            ExprKind::List(items) => {
                                let mut elems = vec![Expr::new(ExprKind::Symbol("vector".into()), l, c)];
                                elems.extend(items);
                                Ok((Expr::new(ExprKind::List(elems), l, c), next))
                            }
                            _ => unreachable!(),
                        }
                    }
                    b'\'' => {
                        // Syntax quote: #'expr → (syntax expr)
                        let (expr, next) = parse_expr(input, i + 2)?;
                        Ok((
                            Expr::new(
                                ExprKind::List(vec![
                                    Expr::new(ExprKind::Symbol("syntax".into()), l, c),
                                    expr,
                                ]),
                                l,
                                c,
                            ),
                            next,
                        ))
                    }
                    _ => Err(EvalError::Parse(format!(
                        "{l}:{c}: unexpected character after #"
                    ))),
                }
            } else {
                Err(EvalError::Parse(format!(
                    "{l}:{c}: unexpected end after #"
                )))
            }
        }
        _ => parse_atom(input, i),
    }
}

fn parse_string(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos + 1;
    let mut s = String::new();
    while i < input.len() && input[i] != b'"' {
        if input[i] == b'\\' && i + 1 < input.len() {
            i += 1;
            match input[i] {
                b'n' => s.push('\n'),
                b't' => s.push('\t'),
                b'\\' => s.push('\\'),
                b'"' => s.push('"'),
                ch => {
                    s.push('\\');
                    s.push(ch as char);
                }
            }
        } else {
            s.push(input[i] as char);
        }
        i += 1;
    }
    if i >= input.len() {
        return Err(EvalError::Parse(format!("{l}:{c}: unterminated string")));
    }
    Ok((Expr::new(ExprKind::Str(s), l, c), i + 1))
}

fn parse_list(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos + 1;
    let mut items = Vec::new();
    loop {
        i = skip_whitespace(input, i);
        if i >= input.len() {
            return Err(EvalError::Parse(format!("{l}:{c}: unterminated list")));
        }
        if input[i] == b')' {
            return Ok((Expr::new(ExprKind::List(items), l, c), i + 1));
        }
        let (expr, next) = parse_expr(input, i)?;
        items.push(expr);
        i = next;
    }
}

fn is_symbol_char(b: u8) -> bool {
    !b.is_ascii_whitespace() && b != b'(' && b != b')' && b != b'"' && b != b';'
}

fn parse_atom(input: &[u8], pos: usize) -> Result<(Expr, usize), EvalError> {
    let (l, c) = line_col(input, pos);
    let mut i = pos;
    while i < input.len() && is_symbol_char(input[i]) {
        i += 1;
    }
    if i == pos {
        return Err(EvalError::Parse(format!(
            "{l}:{c}: unexpected character: {}",
            input[pos] as char
        )));
    }
    let token = std::str::from_utf8(&input[pos..i]).unwrap();
    if let Ok(n) = token.parse::<i64>() {
        return Ok((Expr::new(ExprKind::Integer(n), l, c), i));
    }
    // Rational literal: digits/digits (e.g., 1/3, -5/2)
    if let Some(slash) = token.find('/') {
        if slash > 0 || (token.starts_with('-') && slash > 1) {
            if let (Ok(num), Ok(den)) = (token[..slash].parse::<i64>(), token[slash+1..].parse::<i64>()) {
                if den != 0 {
                    let g = gcd(num.abs(), den.abs());
                    let sign = if den < 0 { -1 } else { 1 };
                    let n = num * sign / g;
                    let d = den.abs() / g;
                    if d == 1 {
                        return Ok((Expr::new(ExprKind::Integer(n), l, c), i));
                    }
                    return Ok((Expr::new(ExprKind::Rational(n, d), l, c), i));
                }
            }
        }
    }
    // Float literal: contains a dot (e.g., 1.5, 0.3)
    if let Ok(f) = token.parse::<f64>() {
        if token.contains('.') {
            return Ok((Expr::new(ExprKind::Float(f), l, c), i));
        }
    }
    Ok((Expr::new(ExprKind::Symbol(token.to_string()), l, c), i))
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let bytes = input.as_bytes();
    let mut pos = 0;
    let mut exprs = Vec::new();
    loop {
        pos = skip_whitespace(bytes, pos);
        if pos >= bytes.len() {
            break;
        }
        let (expr, next) = parse_expr(bytes, pos)?;
        exprs.push(expr);
        pos = next;
    }
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    Ok(exprs)
}

// --- Evaluator ---

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
            if items.is_empty() {
                Value::List(vec![])
            } else {
                // Check for dotted pair notation: (a b . c)
                // The dot is the second-to-last element
                if items.len() >= 3 {
                    if let ExprKind::Symbol(ref s) = items[items.len() - 2].kind {
                        if s == "." {
                            let tail = expr_to_value(&items[items.len() - 1]);
                            let mut result = tail;
                            for item in items[..items.len() - 2].iter().rev() {
                                result = make_pair(expr_to_value(item), result);
                            }
                            return result;
                        }
                    }
                }
                list_from_vec(items.iter().map(expr_to_value).collect())
            }
        }
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "not"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "append"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "string->list"
            | "list->string"
            | "char->integer"
            | "integer->char"
            | "char?"
            | "apply"
            | "call/cc"
            | "call-with-current-continuation"
            | "eq?"
            | "equal?"
            | "abs"
            | "modulo"
            | "remainder"
            | "quotient"
            | "min"
            | "max"
            | "expt"
            | "zero?"
            | "positive?"
            | "negative?"
            | "odd?"
            | "even?"
            | "list-ref"
            | "list-tail"
            | "list?"
            | "assoc"
            | "map"
            | "char-alphabetic?"
            | "char-numeric?"
            | "char-upcase"
            | "char-downcase"
            | "char=?"
            | "char<?"
            | "string=?"
            | "string<?"
            | "string-ci=?"
            | "string-upcase"
            | "string-downcase"
            | "eqv?"
            | "vector"
            | "make-vector"
            | "vector-ref"
            | "vector-set!"
            | "vector-length"
            | "vector?"
            | "vector->list"
            | "list->vector"
            | "for-each"
            | "procedure?"
            | "integer?"
            | "dynamic-wind"
            | "reverse"
            | "raise"
            | "with-exception-handler"
            | "exit"
            | "values"
            | "call-with-values"
            | "exact?"
            | "inexact?"
            | "exact->inexact"
            | "inexact->exact"
            | "numerator"
            | "denominator"
            | "rational?"
            | "set-car!"
            | "set-cdr!"
            | "assq"
            | "assv"
            | "memq"
            | "member"
            | "caar" | "cadr" | "cdar" | "cddr"
            | "caaar" | "caadr" | "cadar" | "caddr"
            | "cdaar" | "cdadr" | "cddar" | "cdddr"
            | "caaaar" | "caaadr" | "caadar" | "caaddr"
            | "cadaar" | "cadadr" | "caddar" | "cadddr"
            | "cdaaar" | "cdaadr" | "cdadar" | "cdaddr"
            | "cddaar" | "cddadr" | "cdddar" | "cddddr"
            | "error"
            | "gcd"
            | "char-whitespace?"
            | "write-char"
            | "string-contains"
            | "lcm"
            | "floor" | "ceiling" | "truncate" | "round"
            | "sqrt"
            | "memv"
            | "char<=?" | "char>=?"
            | "make-string"
            | "string"
            | "string-for-each"
            | "string>?" | "string<=?" | "string>=?"
            | "char>?" | "char<?"
            | "not"
            | "syntax->datum"
            | "datum->syntax"
    )
}

/// Parse a parameter list that may contain dot notation for rest params.
/// Returns (fixed_params, rest_param).
fn parse_params(exprs: &[Expr], el: u32, ec: u32) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse("expected parameter after dot".into()).at(el, ec));
                }
                match &exprs[i + 1].kind {
                    ExprKind::Symbol(rest) => rest_param = Some(rest.clone()),
                    _ => return Err(EvalError::Type("expected symbol after dot".into()).at(el, ec)),
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(EvalError::Type("expected symbol in params".into()).at(el, ec)),
        }
    }
    Ok((params, rest_param))
}

// --- CEK Machine (Continuation-based evaluator) ---

/// Continuation frame for the CEK machine.
#[derive(Clone, Debug)]
enum KontFrame {
    /// Sequence: discard current value, evaluate remaining expressions.
    Seq { rest: Vec<Expr>, env: Env },
    /// If: condition was evaluated, pick branch.
    If { then_br: Expr, else_br: Option<Expr>, env: Env },
    /// Define: value was evaluated, bind to name.
    Define { name: String, env: Env },
    /// Set!: value was evaluated, update existing binding.
    Set { name: String, env: Env, el: u32, ec: u32 },
    /// Function position was evaluated; now evaluate arguments right-to-left.
    EvalFunc { arg_exprs: Vec<Expr>, env: Env, el: u32, ec: u32 },
    /// Evaluating arguments right-to-left.
    /// `remaining`: args not yet evaluated (original order, pop from end).
    /// `done`: values evaluated so far (reversed order; reversed at end).
    EvalArg { func: Value, remaining: Vec<Expr>, done: Vec<Value>, env: Env, el: u32, ec: u32 },
    /// And: short-circuit evaluation.
    And { rest: Vec<Expr>, env: Env },
    /// Or: short-circuit evaluation.
    Or { rest: Vec<Expr>, env: Env },
    /// Let: evaluating binding values left-to-right.
    LetBind { name: String, done: Vec<(String, Value)>, remaining: Vec<(String, Expr)>, body: Vec<Expr>, outer: Env },
    /// Named let: evaluating init values left-to-right.
    NamedLetBind { loop_name: String, all_params: Vec<String>, done_vals: Vec<Value>, remaining_inits: Vec<Expr>, body: Vec<Expr>, outer: Env },
    /// Cond: test was evaluated; decide whether to run body or try next clause.
    CondClause { body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Env, el: u32, ec: u32 },
    /// Cond =>: proc was evaluated; apply it to the test value.
    CondArrow { test_val: Value, el: u32, ec: u32 },
    /// String-set!: index expression was evaluated.
    StrSetIdx { var: String, ch_expr: Expr, env: Env, el: u32, ec: u32 },
    /// String-set!: char expression was evaluated.
    StrSetCh { var: String, idx: usize, env: Env, el: u32, ec: u32 },
    /// call/cc: the function argument was evaluated.
    CallCC { el: u32, ec: u32 },
    /// Letrec/letrec*: evaluating init values in the letrec env.
    LetrecBind {
        names: Vec<String>,
        current_idx: usize,
        values: Vec<Value>,
        remaining_inits: Vec<Expr>,
        body: Vec<Expr>,
        env: Env,
        sequential: bool, // true for letrec*, false for letrec
    },
    /// Case: key was evaluated, match against clauses.
    CaseKey { clauses: Vec<Expr>, env: Env, el: u32, ec: u32 },
    /// dynamic-wind: in-thunk returned, now push wind entry and run body.
    DynWindBody { in_thunk: Value, body_thunk: Value, out_thunk: Value, wind_id: u64, el: u32, ec: u32 },
    /// dynamic-wind: body returned, now pop wind entry and run out-thunk.
    DynWindOut { wind_id: u64, el: u32, ec: u32 },
    /// dynamic-wind: out-thunk returned, return the saved body result.
    DynWindDone { body_result: Value },
    /// Wind transfer during continuation invocation.
    WindTransfer {
        pending_outs: Vec<Value>,
        pending_ins: Vec<WindEntry>,
        target_kont: Vec<KontFrame>,
        target_wind: Vec<WindEntry>,
        value: Value,
    },
    /// with-exception-handler: marks handler boundary in kont stack.
    ExceptionHandler { handler: Value },
    /// guard: marks guard boundary in kont stack; when exception is caught, evaluate clauses.
    GuardHandler { var: String, clauses: Vec<Expr>, env: Env },
    /// Raise unwinding: calling out-thunks before handling exception.
    RaiseUnwind {
        pending_outs: Vec<Value>,
        handler_action: RaiseAction,
        exception: Value,
    },
    /// Guard clause evaluation: test was evaluated, decide to return result or try next.
    GuardClause { body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Env, exception: Value },
    /// call-with-values: producer returned, now apply consumer to the values.
    CallWithValues { consumer: Value, el: u32, ec: u32 },
    /// map iteration: collect results.
    MapIter { func: Value, remaining: Vec<Vec<Value>>, remaining_idx: usize, done: Vec<Value>, el: u32, ec: u32 },
    /// for-each iteration.
    ForEachIter { func: Value, remaining: Vec<Vec<Value>>, remaining_idx: usize, el: u32, ec: u32 },
    /// define-syntax: transformer expression was evaluated, bind to name.
    DefineSyntax { name: String, env: Env },
    /// Macro transformer returned a value; convert Syntax → Expr and evaluate.
    MacroExpand { env: Env, el: u32, ec: u32 },
    /// syntax-case: stx was evaluated; now do pattern matching.
    SyntaxCase { literals: Vec<String>, clauses: Vec<(Vec<Expr>, Option<Expr>, Expr)>, env: Env, el: u32, ec: u32 },
    /// Pop syntax bindings after syntax-case body returns.
    SyntaxCaseCleanup,
    /// with-syntax: evaluating binding RHS expressions one at a time.
    WithSyntax {
        patterns: Vec<Vec<Expr>>,    // remaining patterns to match
        rhs_exprs: Vec<Expr>,        // remaining RHS exprs to evaluate
        done_bindings: HashMap<String, Vec<Expr>>,
        done_evars: HashSet<String>,
        body: Vec<Expr>,
        env: Env,
        el: u32, ec: u32,
    },
    /// Pop syntax bindings after with-syntax body returns.
    WithSyntaxCleanup,
}

/// What to do after unwinding for a raise.
#[derive(Clone, Debug)]
enum RaiseAction {
    CallHandler { handler: Value },
    EvalGuard { var: String, clauses: Vec<Expr>, env: Env },
}

/// Transform a quasiquote expression into equivalent code using cons/append/list.
/// Returns an Expr that, when evaluated, produces the quasiquoted value.
fn expand_quasiquote(expr: &Expr) -> Expr {
    fn sym(s: &str, l: u32, c: u32) -> Expr { Expr::new(ExprKind::Symbol(s.into()), l, c) }
    fn make_call(name: &str, args: Vec<Expr>, l: u32, c: u32) -> Expr {
        let mut items = vec![sym(name, l, c)];
        items.extend(args);
        Expr::new(ExprKind::List(items), l, c)
    }
    fn quote_expr(e: &Expr) -> Expr {
        Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), e.line, e.col),
            e.clone(),
        ]), e.line, e.col)
    }

    match &expr.kind {
        ExprKind::List(items) if !items.is_empty() => {
            // (unquote expr) → expr
            if let ExprKind::Symbol(ref s) = items[0].kind {
                if s == "unquote" && items.len() == 2 {
                    return items[1].clone();
                }
            }

            let (l, c) = (expr.line, expr.col);

            // Check for dotted pair notation
            let is_dotted = items.len() >= 3 && matches!(&items[items.len() - 2].kind, ExprKind::Symbol(ref s) if s == ".");

            if is_dotted {
                // (a b . c) → build with cons
                let tail = expand_quasiquote(&items[items.len() - 1]);
                let mut result = tail;
                for item in items[..items.len() - 2].iter().rev() {
                    // Check for unquote-splicing
                    if let ExprKind::List(ref sub) = item.kind {
                        if sub.len() == 2 {
                            if let ExprKind::Symbol(ref s) = sub[0].kind {
                                if s == "unquote-splicing" {
                                    result = make_call("append", vec![sub[1].clone(), result], l, c);
                                    continue;
                                }
                            }
                        }
                    }
                    result = make_call("cons", vec![expand_quasiquote(item), result], l, c);
                }
                result
            } else {
                // Check if any element uses unquote-splicing
                let has_splice = items.iter().any(|item| {
                    if let ExprKind::List(ref sub) = item.kind {
                        if sub.len() == 2 {
                            if let ExprKind::Symbol(ref s) = sub[0].kind {
                                return s == "unquote-splicing";
                            }
                        }
                    }
                    false
                });

                if has_splice {
                    // Use append strategy: group consecutive non-splice items into (list ...) calls
                    let mut segments: Vec<Expr> = Vec::new();
                    let mut current_group: Vec<Expr> = Vec::new();
                    for item in items {
                        if let ExprKind::List(ref sub) = item.kind {
                            if sub.len() == 2 {
                                if let ExprKind::Symbol(ref s) = sub[0].kind {
                                    if s == "unquote-splicing" {
                                        if !current_group.is_empty() {
                                            segments.push(make_call("list", std::mem::take(&mut current_group), l, c));
                                        }
                                        segments.push(sub[1].clone());
                                        continue;
                                    }
                                }
                            }
                        }
                        current_group.push(expand_quasiquote(item));
                    }
                    if !current_group.is_empty() {
                        segments.push(make_call("list", current_group, l, c));
                    }
                    make_call("append", segments, l, c)
                } else {
                    // Simple list: use (list expanded-items...)
                    let expanded: Vec<Expr> = items.iter().map(|i| expand_quasiquote(i)).collect();
                    make_call("list", expanded, l, c)
                }
            }
        }
        _ => quote_expr(expr),
    }
}

/// Control state of the CEK machine.
enum Ctrl {
    /// Evaluate an expression in an environment.
    Eval(Expr, Env),
    /// A value has been produced; apply it to the continuation.
    Val(Value),
}

/// Set up evaluation of a body (sequence of expressions) with proper tail position.
fn eval_body(body: &[Expr], env: Env, kont: &mut Vec<KontFrame>) -> Ctrl {
    if body.is_empty() {
        Ctrl::Val(Value::Void)
    } else if body.len() == 1 {
        Ctrl::Eval(body[0].clone(), env)
    } else {
        kont.push(KontFrame::Seq { rest: body[1..].to_vec(), env: env.clone() });
        Ctrl::Eval(body[0].clone(), env)
    }
}

/// Start evaluating guard clauses with exception bound to var.
fn eval_guard_clauses(exception: Value, var: &str, clauses: &[Expr], env: &Env, kont: &mut Vec<KontFrame>) -> Result<Ctrl, EvalError> {
    let guard_env = new_env(Some(env.clone()));
    env_set(&guard_env, var.to_string(), exception.clone());
    if clauses.is_empty() {
        return Err(EvalError::Type(format!("guard: no matching clause for {}", exception.display())));
    }
    match &clauses[0].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let is_else = matches!(&parts[0].kind, ExprKind::Symbol(ref s) if s == "else");
            if is_else {
                Ok(eval_body(&parts[1..], guard_env, kont))
            } else {
                let body = parts[1..].to_vec();
                let rest = clauses[1..].to_vec();
                kont.push(KontFrame::GuardClause { body, rest_clauses: rest, env: guard_env.clone(), exception });
                Ok(Ctrl::Eval(parts[0].clone(), guard_env))
            }
        }
        _ => Err(EvalError::Type("guard: invalid clause".into())),
    }
}

/// Start evaluating cond clauses.
fn eval_cond(clauses: &[Expr], env: Env, el: u32, ec: u32, kont: &mut Vec<KontFrame>) -> Result<Ctrl, EvalError> {
    if clauses.is_empty() {
        return Ok(Ctrl::Val(Value::Void));
    }
    match &clauses[0].kind {
        ExprKind::List(parts) if !parts.is_empty() => {
            let is_else = matches!(&parts[0].kind, ExprKind::Symbol(ref s) if s == "else");
            if is_else {
                Ok(eval_body(&parts[1..], env, kont))
            } else {
                let body = parts[1..].to_vec();
                let rest = clauses[1..].to_vec();
                kont.push(KontFrame::CondClause { body, rest_clauses: rest, env: env.clone(), el, ec });
                Ok(Ctrl::Eval(parts[0].clone(), env))
            }
        }
        _ => Err(EvalError::Type("cond: invalid clause".into()).at(el, ec)),
    }
}

// --- Hygienic Macros ---

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__macro_{}", base, n)
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "quote" | "if" | "define" | "set!" | "lambda" | "and" | "or"
            | "let" | "let*" | "begin" | "cond" | "string-set!" | "call/cc"
            | "call-with-current-continuation" | "define-syntax" | "syntax-rules"
            | "letrec" | "letrec*" | "case" | "do" | "when" | "unless"
            | "guard" | "define-record-type"
            | "syntax-case" | "syntax" | "with-syntax" | "case-lambda"
            | "quasiquote"
    )
}

fn parse_syntax_rules(expr: &Expr, el: u32, ec: u32) -> Result<(Vec<String>, Vec<(Expr, Expr)>), EvalError> {
    let items = match &expr.kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Parse("expected syntax-rules".into()).at(el, ec)),
    };
    if items.len() < 2 {
        return Err(EvalError::Parse("syntax-rules: too few parts".into()).at(el, ec));
    }
    match &items[0].kind {
        ExprKind::Symbol(s) if s == "syntax-rules" => {}
        _ => return Err(EvalError::Parse("expected syntax-rules".into()).at(el, ec)),
    }
    let literals = match &items[1].kind {
        ExprKind::List(lits) => {
            let mut ls = Vec::new();
            for l in lits {
                match &l.kind {
                    ExprKind::Symbol(s) => ls.push(s.clone()),
                    _ => return Err(EvalError::Parse("literal must be identifier".into()).at(el, ec)),
                }
            }
            ls
        }
        _ => return Err(EvalError::Parse("expected literals list".into()).at(el, ec)),
    };
    let mut rules = Vec::new();
    for item in &items[2..] {
        match &item.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                rules.push((pair[0].clone(), pair[1].clone()));
            }
            _ => return Err(EvalError::Parse("syntax rule must be (pattern template)".into()).at(el, ec)),
        }
    }
    Ok((literals, rules))
}

fn macro_match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, Vec<Expr>>,
    ellipsis_vars: &mut HashSet<String>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1].kind, ExprKind::Symbol(ref s) if s == "...");
        if has_ellipsis {
            let var_name = match &pattern[pi].kind {
                ExprKind::Symbol(s) if !literals.contains(s) => s.clone(),
                _ => return false,
            };
            let remaining_pats = pattern.len() - pi - 2;
            let available = if input.len() >= ii + remaining_pats {
                input.len() - ii - remaining_pats
            } else {
                return false;
            };
            let matched: Vec<Expr> = input[ii..ii + available].to_vec();
            ellipsis_vars.insert(var_name.clone());
            bindings.insert(var_name, matched);
            ii += available;
            pi += 2;
        } else {
            if ii >= input.len() {
                return false;
            }
            match &pattern[pi].kind {
                ExprKind::Symbol(s) if literals.contains(s) => {
                    if !matches!(&input[ii].kind, ExprKind::Symbol(ref t) if t == s) {
                        return false;
                    }
                    pi += 1;
                    ii += 1;
                }
                ExprKind::Symbol(s) if s == "_" => {
                    pi += 1;
                    ii += 1;
                }
                ExprKind::Symbol(s) => {
                    bindings.insert(s.clone(), vec![input[ii].clone()]);
                    pi += 1;
                    ii += 1;
                }
                ExprKind::List(sub_pat) => {
                    if let ExprKind::List(ref sub_inp) = input[ii].kind {
                        if !macro_match_pattern(sub_pat, sub_inp, literals, bindings, ellipsis_vars) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                    pi += 1;
                    ii += 1;
                }
                _ => {
                    pi += 1;
                    ii += 1;
                }
            }
        }
    }
    ii == input.len()
}

fn collect_ellipsis_vars_in(expr: &Expr, evars: &HashSet<String>, found: &mut Vec<String>) {
    match &expr.kind {
        ExprKind::Symbol(s) if evars.contains(s.as_str()) => {
            if !found.contains(s) {
                found.push(s.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_ellipsis_vars_in(item, evars, found);
            }
        }
        _ => {}
    }
}

fn instantiate_template(
    template: &Expr,
    bindings: &HashMap<String, Vec<Expr>>,
    ellipsis_vars: &HashSet<String>,
    rename_map: &mut HashMap<String, String>,
    def_env: &Env,
    use_env: &Env,
) -> Result<Expr, EvalError> {
    let (l, c) = (template.line, template.col);
    match &template.kind {
        ExprKind::Symbol(s) if s == "..." => Ok(template.clone()),
        ExprKind::Symbol(s) => {
            if let Some(vals) = bindings.get(s) {
                if vals.len() == 1 {
                    Ok(vals[0].clone())
                } else {
                    Ok(template.clone())
                }
            } else if is_special_form(s) || is_builtin(s) {
                Ok(template.clone())
            } else {
                let renamed = rename_map
                    .entry(s.clone())
                    .or_insert_with(|| gensym(s))
                    .clone();
                if let Some(val) = env_get(def_env, s) {
                    env_set(use_env, renamed.clone(), val);
                }
                Ok(Expr::new(ExprKind::Symbol(renamed), l, c))
            }
        }
        ExprKind::List(items) => {
            // Don't rename inside (quote ...)
            if !items.is_empty() {
                if let ExprKind::Symbol(ref s) = items[0].kind {
                    if s == "quote" {
                        return Ok(template.clone());
                    }
                }
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len()
                    && matches!(&items[i + 1].kind, ExprKind::Symbol(ref s) if s == "...")
                {
                    let mut found = Vec::new();
                    collect_ellipsis_vars_in(&items[i], ellipsis_vars, &mut found);
                    if found.is_empty() {
                        i += 2;
                        continue;
                    }
                    let count = bindings.get(&found[0]).map(|v| v.len()).unwrap_or(0);
                    for j in 0..count {
                        let mut sub_bindings = bindings.clone();
                        for var in &found {
                            if let Some(vals) = bindings.get(var) {
                                if j < vals.len() {
                                    sub_bindings.insert(var.clone(), vec![vals[j].clone()]);
                                }
                            }
                        }
                        let expanded = instantiate_template(
                            &items[i],
                            &sub_bindings,
                            ellipsis_vars,
                            rename_map,
                            def_env,
                            use_env,
                        )?;
                        result.push(expanded);
                    }
                    i += 2;
                } else {
                    let expanded = instantiate_template(
                        &items[i], bindings, ellipsis_vars, rename_map, def_env, use_env,
                    )?;
                    result.push(expanded);
                    i += 1;
                }
            }
            Ok(Expr::new(ExprKind::List(result), l, c))
        }
        _ => Ok(template.clone()),
    }
}

fn expand_macro(
    input: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    el: u32,
    ec: u32,
) -> Result<Expr, EvalError> {
    for (pattern, template) in rules {
        let pat_items = match &pattern.kind {
            ExprKind::List(items) => items,
            _ => continue,
        };
        let pat_args = if pat_items.len() > 1 { &pat_items[1..] } else { &[] as &[Expr] };
        let input_args = if input.len() > 1 { &input[1..] } else { &[] as &[Expr] };
        let mut bindings = HashMap::new();
        let mut evars = HashSet::new();
        if macro_match_pattern(pat_args, input_args, literals, &mut bindings, &mut evars) {
            let mut rename_map = HashMap::new();
            return instantiate_template(template, &bindings, &evars, &mut rename_map, def_env, use_env);
        }
    }
    Err(EvalError::Parse("no matching syntax rule".into()).at(el, ec))
}

/// Apply a function value to evaluated arguments within the CEK machine.
fn apply_func(func: Value, args: Vec<Value>, kont: &mut Vec<KontFrame>, wind: &mut Vec<WindEntry>, el: u32, ec: u32) -> Result<Ctrl, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            bind_lambda_args(&params, &rest_param, &args, &env, el, ec)?;
            let local_env = new_env(Some(env));
            for (p, a) in params.iter().zip(&args) {
                env_set(&local_env, p.clone(), a.clone());
            }
            if let Some(ref rest) = rest_param {
                let rest_args = if args.len() > params.len() {
                    args[params.len()..].to_vec()
                } else {
                    Vec::new()
                };
                env_set(&local_env, rest.clone(), list_from_vec(rest_args));
            }
            Ok(eval_body(&body, local_env, kont))
        }
        Value::CaseLambda { clauses, env } => {
            let nargs = args.len();
            for (params, rest_param, body) in &clauses {
                let matches = if rest_param.is_some() {
                    nargs >= params.len()
                } else {
                    nargs == params.len()
                };
                if matches {
                    let local_env = new_env(Some(env));
                    for (p, a) in params.iter().zip(&args) {
                        env_set(&local_env, p.clone(), a.clone());
                    }
                    if let Some(ref rest) = rest_param {
                        let rest_args = if args.len() > params.len() {
                            args[params.len()..].to_vec()
                        } else {
                            Vec::new()
                        };
                        env_set(&local_env, rest.clone(), list_from_vec(rest_args));
                    }
                    return Ok(eval_body(body, local_env, kont));
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", nargs)).at(el, ec))
        }
        Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into()).at(el, ec));
            }
            let cont_val = Value::Continuation(kont.clone(), wind.clone());
            let f = args.into_iter().next().unwrap();
            apply_func(f, vec![cont_val], kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "dynamic-wind" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("dynamic-wind requires 3 arguments".into()).at(el, ec));
            }
            let in_thunk = args[0].clone();
            let body_thunk = args[1].clone();
            let out_thunk = args[2].clone();
            let wind_id = next_wind_id();
            kont.push(KontFrame::DynWindBody {
                in_thunk: in_thunk.clone(),
                body_thunk,
                out_thunk,
                wind_id,
                el,
                ec,
            });
            // Call in-thunk; when it returns, DynWindBody will handle the rest
            apply_func(in_thunk, vec![], kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "with-exception-handler" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("with-exception-handler requires 2 arguments".into()).at(el, ec));
            }
            let handler = args[0].clone();
            let thunk = args[1].clone();
            kont.push(KontFrame::ExceptionHandler { handler });
            apply_func(thunk, vec![], kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "raise" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("raise requires 1 argument".into()).at(el, ec));
            }
            let exception = args.into_iter().next().unwrap();
            // Search kont stack for nearest exception handler or guard handler
            let mut handler_idx = None;
            for (i, frame) in kont.iter().enumerate().rev() {
                match frame {
                    KontFrame::ExceptionHandler { .. } | KontFrame::GuardHandler { .. } => {
                        handler_idx = Some(i);
                        break;
                    }
                    _ => {}
                }
            }
            let idx = match handler_idx {
                Some(i) => i,
                None => return Err(EvalError::Type(format!("unhandled exception: {}", exception.display())).at(el, ec)),
            };
            let frame = kont.remove(idx);
            // Count wind entries that need unwinding: wind entries added after the handler
            // We need to figure out how many wind entries existed when the handler was installed.
            // Count DynWindOut frames between idx and top of kont to determine wind depth.
            let mut wind_at_handler = wind.len();
            for frame in kont[idx..].iter() {
                match frame {
                    KontFrame::DynWindOut { .. } => { wind_at_handler -= 1; }
                    KontFrame::DynWindBody { .. } => { wind_at_handler -= 1; }
                    _ => {}
                }
            }
            // Truncate kont to the handler position
            kont.truncate(idx);
            // Collect out-thunks to call (innermost first)
            let pending_outs: Vec<Value> = wind[wind_at_handler..].iter().rev()
                .map(|e| e.out_thunk.clone()).collect();
            wind.truncate(wind_at_handler);
            let action = match frame {
                KontFrame::ExceptionHandler { handler } => RaiseAction::CallHandler { handler },
                KontFrame::GuardHandler { var, clauses, env } => RaiseAction::EvalGuard { var, clauses, env },
                _ => unreachable!(),
            };
            if pending_outs.is_empty() {
                // No unwinding needed, proceed directly
                match action {
                    RaiseAction::CallHandler { handler } => {
                        apply_func(handler, vec![exception], kont, wind, el, ec)
                    }
                    RaiseAction::EvalGuard { var, clauses, env } => {
                        eval_guard_clauses(exception, &var, &clauses, &env, kont)
                    }
                }
            } else {
                kont.push(KontFrame::RaiseUnwind { pending_outs, handler_action: action, exception });
                Ok(Ctrl::Val(Value::Void)) // trigger the RaiseUnwind frame
            }
        }
        Value::Builtin(ref name) if name == "values" => {
            match args.len() {
                1 => Ok(Ctrl::Val(args.into_iter().next().unwrap())),
                _ => Ok(Ctrl::Val(Value::Values(args))),
            }
        }
        Value::Builtin(ref name) if name == "call-with-values" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("call-with-values requires 2 arguments".into()).at(el, ec));
            }
            let mut args_iter = args.into_iter();
            let producer = args_iter.next().unwrap();
            let consumer = args_iter.next().unwrap();
            kont.push(KontFrame::CallWithValues { consumer, el, ec });
            apply_func(producer, vec![], kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "exit" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("exit requires 1 argument".into()).at(el, ec));
            }
            let val = args.into_iter().next().unwrap();
            kont.clear();
            wind.clear();
            Ok(Ctrl::Val(val))
        }
        Value::Builtin(ref name) if name == "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("map requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                value_to_vec(a).ok_or_else(|| EvalError::Type("map: argument is not a list".into()).at(el, ec))
            }).collect::<Result<Vec<_>, _>>()?;
            if lists.is_empty() || lists[0].is_empty() {
                return Ok(Ctrl::Val(Value::List(vec![])));
            }
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len {
                    return Err(EvalError::Type("map: lists must have same length".into()).at(el, ec));
                }
            }
            // Use continuation-based iteration to avoid stack overflow
            let call_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
            kont.push(KontFrame::MapIter { func: func.clone(), remaining: lists, remaining_idx: 1, done: vec![], el, ec });
            apply_func(func, call_args, kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("for-each requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let lists: Vec<Vec<Value>> = args[1..].iter().map(|a| {
                value_to_vec(a).ok_or_else(|| EvalError::Type("for-each: not a list".into()).at(el, ec))
            }).collect::<Result<Vec<_>, _>>()?;
            if lists.is_empty() || lists[0].is_empty() {
                return Ok(Ctrl::Val(Value::Void));
            }
            let call_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
            kont.push(KontFrame::ForEachIter { func: func.clone(), remaining: lists, remaining_idx: 1, el, ec });
            apply_func(func, call_args, kont, wind, el, ec)
        }
        Value::Builtin(ref name) if name == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let last = &args[args.len() - 1];
            let tail_args = match value_to_vec(last) {
                Some(items) => items,
                None => return Err(EvalError::Type("apply: last argument must be a list".into()).at(el, ec)),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_args);
            apply_func(func, all_args, kont, wind, el, ec)
        }
        Value::RecordConstructor { type_id, ref type_name, ref field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!("{}: expected {} arguments, got {}", type_name, field_names.len(), args.len())).at(el, ec));
            }
            Ok(Ctrl::Val(Value::Record { type_id, fields: args }))
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into()).at(el, ec));
            }
            let result = matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id);
            Ok(Ctrl::Val(Value::Boolean(result)))
        }
        Value::RecordAccessor { type_id, ref type_name, field_index, ref field_name } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{}: requires 1 argument", field_name)).at(el, ec));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields } if *tid == type_id => {
                    Ok(Ctrl::Val(fields[field_index].clone()))
                }
                _ => Err(EvalError::Type(format!("{}: expected record of type {}", field_name, type_name)).at(el, ec)),
            }
        }
        Value::Builtin(ref name) => {
            let result = eval_builtin(name, &args).map_err(|e| e.at(el, ec))?;
            Ok(Ctrl::Val(result))
        }
        Value::Continuation(saved_kont, saved_wind) => {
            let value = if args.len() == 1 {
                args.into_iter().next().unwrap()
            } else {
                Value::Values(args)
            };
            // Compute common prefix length between current and saved wind stacks
            let common_len = wind.iter().zip(saved_wind.iter())
                .take_while(|(a, b)| a.id == b.id)
                .count();
            let need_unwind = wind.len() > common_len;
            let need_rewind = saved_wind.len() > common_len;
            if !need_unwind && !need_rewind {
                // No winding needed, just restore
                *kont = saved_kont;
                *wind = saved_wind;
                Ok(Ctrl::Val(value))
            } else {
                // Collect out-thunks to call (innermost first = reverse order from common_len..)
                let pending_outs: Vec<Value> = wind[common_len..].iter().rev()
                    .map(|e| e.out_thunk.clone()).collect();
                // Collect wind entries to rewind (outermost first = order from common_len..)
                let pending_ins: Vec<WindEntry> = saved_wind[common_len..].to_vec();
                // Truncate wind to common prefix before starting unwind
                wind.truncate(common_len);
                kont.push(KontFrame::WindTransfer {
                    pending_outs,
                    pending_ins,
                    target_kont: saved_kont,
                    target_wind: saved_wind,
                    value: value.clone(),
                });
                // Start by calling the first out-thunk (or in-thunk if no outs)
                Ok(Ctrl::Val(value))
            }
        }
        _ => Err(EvalError::Type("not a procedure".into()).at(el, ec)),
    }
}

/// Main CEK machine loop.
fn run_cek(initial_ctrl: Ctrl, initial_kont: Vec<KontFrame>) -> Result<Value, EvalError> {
    let mut ctrl = initial_ctrl;
    let mut kont = initial_kont;
    let mut wind: Vec<WindEntry> = Vec::new();

    loop {
        // Step limit check
        let limit = STEP_LIMIT.with(|c| c.get());
        if limit > 0 {
            let count = STEP_COUNT.with(|c| {
                let n = c.get() + 1;
                c.set(n);
                n
            });
            if count > limit {
                return Err(EvalError::StepLimitExceeded);
            }
        }

        ctrl = match ctrl {
            Ctrl::Eval(expr, env) => {
                let (el, ec) = (expr.line, expr.col);
                match expr.kind {
                    ExprKind::Integer(n) => Ctrl::Val(Value::Integer(n)),
                    ExprKind::Float(f) => Ctrl::Val(Value::Float(f)),
                    ExprKind::Rational(n, d) => Ctrl::Val(Value::Rational(n, d)),
                    ExprKind::Boolean(b) => Ctrl::Val(Value::Boolean(b)),
                    ExprKind::Str(s) => Ctrl::Val(Value::Str(s)),
                    ExprKind::Char(c) => Ctrl::Val(Value::Char(c)),
                    ExprKind::Symbol(ref name) => {
                        // Check environment first so local bindings shadow builtins
                        match env_get(&env, name) {
                            Some(v) => Ctrl::Val(v),
                            None if is_builtin(name) => Ctrl::Val(Value::Builtin(name.clone())),
                            None => return Err(EvalError::UnboundVariable(name.clone()).at(el, ec)),
                        }
                    }
                    ExprKind::List(items) => {
                        if items.is_empty() {
                            Ctrl::Val(Value::List(vec![]))
                        } else {
                            // Extract operator name if it's a symbol (for special form dispatch)
                            let op_name: Option<String> = match &items[0].kind {
                                ExprKind::Symbol(s) => Some(s.clone()),
                                _ => None,
                            };
                            match op_name.as_deref() {
                                Some("quote") => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("quote requires 1 argument".into()).at(el, ec));
                                    }
                                    Ctrl::Val(expr_to_value(&items[1]))
                                }
                                Some("quasiquote") => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("quasiquote requires 1 argument".into()).at(el, ec));
                                    }
                                    let expanded = expand_quasiquote(&items[1]);
                                    Ctrl::Eval(expanded, env)
                                }
                                Some("if") => {
                                    if items.len() < 3 || items.len() > 4 {
                                        return Err(EvalError::Arity("if requires 2 or 3 arguments".into()).at(el, ec));
                                    }
                                    let else_br = if items.len() == 4 { Some(items[3].clone()) } else { None };
                                    kont.push(KontFrame::If { then_br: items[2].clone(), else_br, env: env.clone() });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                Some("define") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("define requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    match &items[1].kind {
                                        ExprKind::Symbol(name) => {
                                            kont.push(KontFrame::Define { name: name.clone(), env: env.clone() });
                                            Ctrl::Eval(items[2].clone(), env)
                                        }
                                        ExprKind::List(sig) => {
                                            if sig.is_empty() {
                                                return Err(EvalError::Parse("define: empty signature".into()).at(el, ec));
                                            }
                                            let name = match &sig[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Type("define: expected symbol".into()).at(el, ec)),
                                            };
                                            let (params, rest_param) = parse_params(&sig[1..], el, ec)?;
                                            let body = items[2..].to_vec();
                                            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
                                            env_set(&env, name, lambda);
                                            Ctrl::Val(Value::Void)
                                        }
                                        _ => return Err(EvalError::Type("define: expected symbol or list".into()).at(el, ec)),
                                    }
                                }
                                Some("set!") => {
                                    if items.len() != 3 {
                                        return Err(EvalError::Arity("set! requires exactly 2 arguments".into()).at(el, ec));
                                    }
                                    let name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("set!: expected symbol".into()).at(el, ec)),
                                    };
                                    kont.push(KontFrame::Set { name, env: env.clone(), el, ec });
                                    Ctrl::Eval(items[2].clone(), env)
                                }
                                Some("lambda") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let (params, rest_param) = match &items[1].kind {
                                        ExprKind::List(param_exprs) => parse_params(param_exprs, el, ec)?,
                                        ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
                                        _ => return Err(EvalError::Type("lambda: expected parameter list".into()).at(el, ec)),
                                    };
                                    let body = items[2..].to_vec();
                                    Ctrl::Val(Value::Lambda { params, rest_param, body, env })
                                }
                                Some("case-lambda") => {
                                    let mut clauses = Vec::new();
                                    for clause_expr in &items[1..] {
                                        let clause_items = match &clause_expr.kind {
                                            ExprKind::List(items) => items,
                                            _ => return Err(EvalError::Type("case-lambda: expected clause list".into()).at(el, ec)),
                                        };
                                        if clause_items.is_empty() {
                                            return Err(EvalError::Type("case-lambda: empty clause".into()).at(el, ec));
                                        }
                                        let (params, rest_param) = match &clause_items[0].kind {
                                            ExprKind::List(param_exprs) => parse_params(param_exprs, el, ec)?,
                                            ExprKind::Symbol(s) => (Vec::new(), Some(s.clone())),
                                            _ => return Err(EvalError::Type("case-lambda: expected parameter list".into()).at(el, ec)),
                                        };
                                        let body = clause_items[1..].to_vec();
                                        clauses.push((params, rest_param, body));
                                    }
                                    Ctrl::Val(Value::CaseLambda { clauses, env })
                                }
                                Some("and") => {
                                    if items.len() == 1 {
                                        Ctrl::Val(Value::Boolean(true))
                                    } else if items.len() == 2 {
                                        Ctrl::Eval(items[1].clone(), env)
                                    } else {
                                        kont.push(KontFrame::And { rest: items[2..].to_vec(), env: env.clone() });
                                        Ctrl::Eval(items[1].clone(), env)
                                    }
                                }
                                Some("or") => {
                                    if items.len() == 1 {
                                        Ctrl::Val(Value::Boolean(false))
                                    } else if items.len() == 2 {
                                        Ctrl::Eval(items[1].clone(), env)
                                    } else {
                                        kont.push(KontFrame::Or { rest: items[2..].to_vec(), env: env.clone() });
                                        Ctrl::Eval(items[1].clone(), env)
                                    }
                                }
                                Some("let") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("let requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    // Named let: (let name ((var init) ...) body ...)
                                    if let ExprKind::Symbol(ref loop_name) = items[1].kind {
                                        let bindings_expr = match &items[2].kind {
                                            ExprKind::List(bs) => bs,
                                            _ => return Err(EvalError::Type("let: expected bindings list".into()).at(el, ec)),
                                        };
                                        let mut params = Vec::new();
                                        let mut inits = Vec::new();
                                        for b in bindings_expr {
                                            match &b.kind {
                                                ExprKind::List(pair) if pair.len() == 2 => {
                                                    if let ExprKind::Symbol(s) = &pair[0].kind {
                                                        params.push(s.clone());
                                                        inits.push(pair[1].clone());
                                                    } else {
                                                        return Err(EvalError::Type("let: expected symbol".into()).at(el, ec));
                                                    }
                                                }
                                                _ => return Err(EvalError::Type("let: invalid binding".into()).at(el, ec)),
                                            }
                                        }
                                        let body = items[3..].to_vec();
                                        if inits.is_empty() {
                                            let local_env = new_env(Some(env));
                                            let loop_lambda = Value::Lambda { params: vec![], rest_param: None, body: body.clone(), env: local_env.clone() };
                                            env_set(&local_env, loop_name.clone(), loop_lambda);
                                            eval_body(&body, local_env, &mut kont)
                                        } else {
                                            let mut remaining_inits = inits;
                                            let first_init = remaining_inits.remove(0);
                                            kont.push(KontFrame::NamedLetBind {
                                                loop_name: loop_name.clone(),
                                                all_params: params,
                                                done_vals: vec![],
                                                remaining_inits,
                                                body,
                                                outer: env.clone(),
                                            });
                                            Ctrl::Eval(first_init, env)
                                        }
                                    } else {
                                        // Regular let: (let ((var init) ...) body ...)
                                        let bindings_expr = match &items[1].kind {
                                            ExprKind::List(bs) => bs,
                                            _ => return Err(EvalError::Type("let: expected bindings list".into()).at(el, ec)),
                                        };
                                        let mut bindings = Vec::new();
                                        for b in bindings_expr {
                                            match &b.kind {
                                                ExprKind::List(pair) if pair.len() == 2 => {
                                                    if let ExprKind::Symbol(s) = &pair[0].kind {
                                                        bindings.push((s.clone(), pair[1].clone()));
                                                    } else {
                                                        return Err(EvalError::Type("let: expected symbol".into()).at(el, ec));
                                                    }
                                                }
                                                _ => return Err(EvalError::Type("let: invalid binding".into()).at(el, ec)),
                                            }
                                        }
                                        let body = items[2..].to_vec();
                                        if bindings.is_empty() {
                                            let local_env = new_env(Some(env));
                                            eval_body(&body, local_env, &mut kont)
                                        } else {
                                            let mut bindings = bindings;
                                            let (first_name, first_expr) = bindings.remove(0);
                                            kont.push(KontFrame::LetBind {
                                                name: first_name,
                                                done: vec![],
                                                remaining: bindings,
                                                body,
                                                outer: env.clone(),
                                            });
                                            Ctrl::Eval(first_expr, env)
                                        }
                                    }
                                }
                                Some("begin") => {
                                    if items.len() <= 1 {
                                        Ctrl::Val(Value::Void)
                                    } else {
                                        eval_body(&items[1..], env, &mut kont)
                                    }
                                }
                                Some("cond") => {
                                    eval_cond(&items[1..], env, el, ec, &mut kont)?
                                }
                                Some("string-set!") => {
                                    if items.len() != 4 {
                                        return Err(EvalError::Arity("string-set! requires 3 arguments".into()).at(el, ec));
                                    }
                                    let var_name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        ExprKind::Str(_) => {
                                            return Err(EvalError::Type("string-set!: strings are immutable".into()).at(el, ec));
                                        }
                                        _ => return Err(EvalError::Type("string-set!: first argument must be a string variable".into()).at(el, ec)),
                                    };
                                    kont.push(KontFrame::StrSetIdx {
                                        var: var_name,
                                        ch_expr: items[3].clone(),
                                        env: env.clone(),
                                        el,
                                        ec,
                                    });
                                    Ctrl::Eval(items[2].clone(), env)
                                }
                                Some("call/cc") | Some("call-with-current-continuation") => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("call/cc requires 1 argument".into()).at(el, ec));
                                    }
                                    kont.push(KontFrame::CallCC { el, ec });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                Some("define-syntax") => {
                                    if items.len() != 3 {
                                        return Err(EvalError::Arity("define-syntax requires 2 arguments".into()).at(el, ec));
                                    }
                                    let name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("define-syntax: expected symbol".into()).at(el, ec)),
                                    };
                                    // Check if it's syntax-rules or a transformer expression (lambda)
                                    let is_syntax_rules = matches!(&items[2].kind,
                                        ExprKind::List(inner) if !inner.is_empty() && matches!(&inner[0].kind, ExprKind::Symbol(s) if s == "syntax-rules"));
                                    if is_syntax_rules {
                                        let (literals, rules) = parse_syntax_rules(&items[2], el, ec)?;
                                        env_set(&env, name, Value::Macro { literals, rules, def_env: env.clone() });
                                        Ctrl::Val(Value::Void)
                                    } else {
                                        // Evaluate transformer expression
                                        kont.push(KontFrame::DefineSyntax { name, env: env.clone() });
                                        Ctrl::Eval(items[2].clone(), env)
                                    }
                                }
                                Some("let*") => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("let* requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let bindings_expr = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type("let*: expected bindings list".into()).at(el, ec)),
                                    };
                                    let body = items[2..].to_vec();
                                    // Build nested lets
                                    let mut result_body = body;
                                    for b in bindings_expr.iter().rev() {
                                        let pair = match &b.kind {
                                            ExprKind::List(p) if p.len() == 2 => p,
                                            _ => return Err(EvalError::Type("let*: invalid binding".into()).at(el, ec)),
                                        };
                                        let mut let_expr = vec![
                                            Expr::new(ExprKind::Symbol("let".into()), el, ec),
                                            Expr::new(ExprKind::List(vec![b.clone()]), el, ec),
                                        ];
                                        let_expr.extend(result_body);
                                        result_body = vec![Expr::new(ExprKind::List(let_expr), el, ec)];
                                    }
                                    if result_body.len() == 1 {
                                        Ctrl::Eval(result_body.remove(0), env)
                                    } else {
                                        eval_body(&result_body, env, &mut kont)
                                    }
                                }
                                Some("letrec") | Some("letrec*") => {
                                    let is_star = matches!(op_name.as_deref(), Some("letrec*"));
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("letrec requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let bindings_expr = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type("letrec: expected bindings list".into()).at(el, ec)),
                                    };
                                    let mut names = Vec::new();
                                    let mut inits = Vec::new();
                                    for b in bindings_expr {
                                        match &b.kind {
                                            ExprKind::List(pair) if pair.len() == 2 => {
                                                if let ExprKind::Symbol(s) = &pair[0].kind {
                                                    names.push(s.clone());
                                                    inits.push(pair[1].clone());
                                                } else {
                                                    return Err(EvalError::Type("letrec: expected symbol".into()).at(el, ec));
                                                }
                                            }
                                            _ => return Err(EvalError::Type("letrec: invalid binding".into()).at(el, ec)),
                                        }
                                    }
                                    let body = items[2..].to_vec();
                                    let local_env = new_env(Some(env));
                                    // Pre-bind all to Void
                                    for n in &names {
                                        env_set(&local_env, n.clone(), Value::Void);
                                    }
                                    if inits.is_empty() {
                                        eval_body(&body, local_env, &mut kont)
                                    } else {
                                        let mut remaining = inits;
                                        let first = remaining.remove(0);
                                        kont.push(KontFrame::LetrecBind {
                                            names,
                                            current_idx: 0,
                                            values: vec![],
                                            remaining_inits: remaining,
                                            body,
                                            env: local_env.clone(),
                                            sequential: is_star,
                                        });
                                        Ctrl::Eval(first, local_env)
                                    }
                                }
                                Some("case") => {
                                    if items.len() < 2 {
                                        return Err(EvalError::Arity("case requires at least 1 argument".into()).at(el, ec));
                                    }
                                    let clauses = items[2..].to_vec();
                                    kont.push(KontFrame::CaseKey { clauses, env: env.clone(), el, ec });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                Some("do") => {
                                    // Transform to named let
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("do requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let bindings_list = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type("do: expected bindings list".into()).at(el, ec)),
                                    };
                                    let test_clause = match &items[2].kind {
                                        ExprKind::List(tc) if !tc.is_empty() => tc,
                                        _ => return Err(EvalError::Type("do: expected test clause".into()).at(el, ec)),
                                    };
                                    let body = &items[3..];

                                    let mut vars = Vec::new();
                                    let mut inits = Vec::new();
                                    let mut steps: Vec<Option<Expr>> = Vec::new();
                                    for b in bindings_list {
                                        match &b.kind {
                                            ExprKind::List(parts) if parts.len() >= 2 => {
                                                let var_name = match &parts[0].kind {
                                                    ExprKind::Symbol(s) => s.clone(),
                                                    _ => return Err(EvalError::Type("do: expected symbol".into()).at(el, ec)),
                                                };
                                                vars.push(var_name);
                                                inits.push(parts[1].clone());
                                                if parts.len() >= 3 {
                                                    steps.push(Some(parts[2].clone()));
                                                } else {
                                                    steps.push(None);
                                                }
                                            }
                                            _ => return Err(EvalError::Type("do: invalid binding".into()).at(el, ec)),
                                        }
                                    }

                                    let loop_name = "__do_loop__";
                                    // Build named let bindings: ((var1 init1) (var2 init2) ...)
                                    let let_bindings: Vec<Expr> = vars.iter().zip(inits.iter()).map(|(v, i)| {
                                        Expr::new(ExprKind::List(vec![
                                            Expr::new(ExprKind::Symbol(v.clone()), el, ec),
                                            i.clone(),
                                        ]), el, ec)
                                    }).collect();

                                    // Build loop call args
                                    let loop_args: Vec<Expr> = steps.iter().enumerate().map(|(i, step)| {
                                        match step {
                                            Some(s) => s.clone(),
                                            None => Expr::new(ExprKind::Symbol(vars[i].clone()), el, ec),
                                        }
                                    }).collect();

                                    // Build loop call: (__do_loop__ step1 var2 step3 ...)
                                    let mut loop_call_items = vec![Expr::new(ExprKind::Symbol(loop_name.into()), el, ec)];
                                    loop_call_items.extend(loop_args);
                                    let loop_call = Expr::new(ExprKind::List(loop_call_items), el, ec);

                                    // Build test success expression
                                    let test_expr = test_clause[0].clone();
                                    let test_body = if test_clause.len() > 1 {
                                        let mut begin_items = vec![Expr::new(ExprKind::Symbol("begin".into()), el, ec)];
                                        begin_items.extend(test_clause[1..].to_vec());
                                        Expr::new(ExprKind::List(begin_items), el, ec)
                                    } else {
                                        // void
                                        Expr::new(ExprKind::List(vec![
                                            Expr::new(ExprKind::Symbol("if".into()), el, ec),
                                            Expr::new(ExprKind::Boolean(false), el, ec),
                                            Expr::new(ExprKind::Boolean(false), el, ec),
                                        ]), el, ec)
                                    };

                                    // Build else branch (body ... (loop ...))
                                    let else_body = if body.is_empty() {
                                        loop_call
                                    } else {
                                        let mut begin_items = vec![Expr::new(ExprKind::Symbol("begin".into()), el, ec)];
                                        begin_items.extend(body.to_vec());
                                        begin_items.push(loop_call);
                                        Expr::new(ExprKind::List(begin_items), el, ec)
                                    };

                                    // Build if expression
                                    let if_expr = Expr::new(ExprKind::List(vec![
                                        Expr::new(ExprKind::Symbol("if".into()), el, ec),
                                        test_expr,
                                        test_body,
                                        else_body,
                                    ]), el, ec);

                                    // Build named let
                                    let mut named_let = vec![
                                        Expr::new(ExprKind::Symbol("let".into()), el, ec),
                                        Expr::new(ExprKind::Symbol(loop_name.into()), el, ec),
                                        Expr::new(ExprKind::List(let_bindings), el, ec),
                                        if_expr,
                                    ];

                                    let result_expr = Expr::new(ExprKind::List(named_let), el, ec);
                                    Ctrl::Eval(result_expr, env)
                                }
                                Some("when") => {
                                    if items.len() < 2 {
                                        return Err(EvalError::Arity("when requires at least 1 argument".into()).at(el, ec));
                                    }
                                    // (when test body ...) → (if test (begin body ...))
                                    let test_expr = items[1].clone();
                                    let then_body = if items.len() > 2 {
                                        let mut begin = vec![Expr::new(ExprKind::Symbol("begin".into()), el, ec)];
                                        begin.extend(items[2..].to_vec());
                                        Expr::new(ExprKind::List(begin), el, ec)
                                    } else {
                                        Expr::new(ExprKind::Str("".into()), el, ec) // shouldn't happen
                                    };
                                    let if_expr = Expr::new(ExprKind::List(vec![
                                        Expr::new(ExprKind::Symbol("if".into()), el, ec),
                                        test_expr,
                                        then_body,
                                    ]), el, ec);
                                    Ctrl::Eval(if_expr, env)
                                }
                                Some("unless") => {
                                    if items.len() < 2 {
                                        return Err(EvalError::Arity("unless requires at least 1 argument".into()).at(el, ec));
                                    }
                                    // (unless test body ...) → (if (not test) (begin body ...))
                                    let test_expr = items[1].clone();
                                    let not_test = Expr::new(ExprKind::List(vec![
                                        Expr::new(ExprKind::Symbol("not".into()), el, ec),
                                        test_expr,
                                    ]), el, ec);
                                    let then_body = if items.len() > 2 {
                                        let mut begin = vec![Expr::new(ExprKind::Symbol("begin".into()), el, ec)];
                                        begin.extend(items[2..].to_vec());
                                        Expr::new(ExprKind::List(begin), el, ec)
                                    } else {
                                        Expr::new(ExprKind::Str("".into()), el, ec)
                                    };
                                    let if_expr = Expr::new(ExprKind::List(vec![
                                        Expr::new(ExprKind::Symbol("if".into()), el, ec),
                                        not_test,
                                        then_body,
                                    ]), el, ec);
                                    Ctrl::Eval(if_expr, env)
                                }
                                Some("define-record-type") => {
                                    // (define-record-type <name> (constructor field...) pred? (field accessor) ...)
                                    if items.len() < 4 {
                                        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()).at(el, ec));
                                    }
                                    // Parse type name (may be <name>)
                                    let _type_name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("define-record-type: expected type name".into()).at(el, ec)),
                                    };
                                    // Parse constructor: (ctor-name field ...)
                                    let (ctor_name, ctor_fields) = match &items[2].kind {
                                        ExprKind::List(parts) if !parts.is_empty() => {
                                            let cname = match &parts[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Type("define-record-type: expected constructor name".into()).at(el, ec)),
                                            };
                                            let mut fields = Vec::new();
                                            for p in &parts[1..] {
                                                match &p.kind {
                                                    ExprKind::Symbol(s) => fields.push(s.clone()),
                                                    _ => return Err(EvalError::Type("define-record-type: expected field name".into()).at(el, ec)),
                                                }
                                            }
                                            (cname, fields)
                                        }
                                        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into()).at(el, ec)),
                                    };
                                    // Parse predicate name
                                    let pred_name = match &items[3].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into()).at(el, ec)),
                                    };
                                    // Parse field specs: (field-name accessor-name)
                                    let mut field_accessors: Vec<(String, String)> = Vec::new();
                                    for item in &items[4..] {
                                        match &item.kind {
                                            ExprKind::List(parts) if parts.len() >= 2 => {
                                                let fname = match &parts[0].kind {
                                                    ExprKind::Symbol(s) => s.clone(),
                                                    _ => return Err(EvalError::Type("define-record-type: expected field name".into()).at(el, ec)),
                                                };
                                                let aname = match &parts[1].kind {
                                                    ExprKind::Symbol(s) => s.clone(),
                                                    _ => return Err(EvalError::Type("define-record-type: expected accessor name".into()).at(el, ec)),
                                                };
                                                field_accessors.push((fname, aname));
                                            }
                                            _ => return Err(EvalError::Type("define-record-type: expected field spec".into()).at(el, ec)),
                                        }
                                    }
                                    let type_id = next_record_type_id();
                                    // Define constructor
                                    env_set(&env, ctor_name, Value::RecordConstructor {
                                        type_id,
                                        type_name: _type_name.clone(),
                                        field_names: ctor_fields.clone(),
                                    });
                                    // Define predicate
                                    env_set(&env, pred_name, Value::RecordPredicate { type_id });
                                    // Define accessors
                                    for (fname, aname) in &field_accessors {
                                        let idx = ctor_fields.iter().position(|f| f == fname)
                                            .ok_or_else(|| EvalError::Type(format!("define-record-type: field '{}' not in constructor", fname)).at(el, ec))?;
                                        env_set(&env, aname.clone(), Value::RecordAccessor {
                                            type_id,
                                            type_name: _type_name.clone(),
                                            field_index: idx,
                                            field_name: fname.clone(),
                                        });
                                    }
                                    Ctrl::Val(Value::Void)
                                }
                                Some("guard") => {
                                    // (guard (var clause ...) body ...)
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("guard requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let clauses_expr = match &items[1].kind {
                                        ExprKind::List(parts) if !parts.is_empty() => parts,
                                        _ => return Err(EvalError::Type("guard: expected (var clause ...) list".into()).at(el, ec)),
                                    };
                                    let var = match &clauses_expr[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type("guard: expected variable name".into()).at(el, ec)),
                                    };
                                    let clauses = clauses_expr[1..].to_vec();
                                    let body = items[2..].to_vec();
                                    kont.push(KontFrame::GuardHandler { var, clauses, env: env.clone() });
                                    eval_body(&body, env, &mut kont)
                                }
                                Some("syntax-case") => {
                                    // (syntax-case stx-expr (literals) clause ...)
                                    if items.len() < 4 {
                                        return Err(EvalError::Arity("syntax-case requires at least 3 arguments".into()).at(el, ec));
                                    }
                                    let literals = match &items[2].kind {
                                        ExprKind::List(lits) => {
                                            let mut ls = Vec::new();
                                            for l in lits {
                                                match &l.kind {
                                                    ExprKind::Symbol(s) => ls.push(s.clone()),
                                                    _ => return Err(EvalError::Parse("syntax-case: literal must be identifier".into()).at(el, ec)),
                                                }
                                            }
                                            ls
                                        }
                                        _ => return Err(EvalError::Parse("syntax-case: expected literals list".into()).at(el, ec)),
                                    };
                                    // Parse clauses: (pattern body) or (pattern fender body)
                                    let mut clauses = Vec::new();
                                    for item in &items[3..] {
                                        match &item.kind {
                                            ExprKind::List(parts) if parts.len() == 2 => {
                                                clauses.push((vec![parts[0].clone()], None, parts[1].clone()));
                                            }
                                            ExprKind::List(parts) if parts.len() == 3 => {
                                                clauses.push((vec![parts[0].clone()], Some(parts[1].clone()), parts[2].clone()));
                                            }
                                            _ => return Err(EvalError::Parse("syntax-case: invalid clause".into()).at(el, ec)),
                                        }
                                    }
                                    kont.push(KontFrame::SyntaxCase { literals, clauses, env: env.clone(), el, ec });
                                    Ctrl::Eval(items[1].clone(), env)
                                }
                                Some("syntax") => {
                                    // (syntax template) — instantiate template with current syntax bindings
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity("syntax requires 1 argument".into()).at(el, ec));
                                    }
                                    let template = &items[1];
                                    let result = SYNTAX_BINDINGS.with(|sb| {
                                        let stack = sb.borrow();
                                        let entry = stack.last();
                                        let (bindings, evars) = match entry {
                                            Some(e) => (&e.bindings, &e.ellipsis_vars),
                                            None => return Err(EvalError::Parse("syntax: not inside syntax-case".into()).at(el, ec)),
                                        };
                                        let use_env = MACRO_USE_ENV.with(|mue| {
                                            let stack = mue.borrow();
                                            stack.last().cloned().unwrap_or_else(|| env.clone())
                                        });
                                        let mut rename_map = HashMap::new();
                                        instantiate_template(template, bindings, evars, &mut rename_map, &env, &use_env)
                                    })?;
                                    Ctrl::Val(Value::Syntax(result))
                                }
                                Some("with-syntax") => {
                                    // (with-syntax ((pattern expr) ...) body ...)
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity("with-syntax requires at least 2 arguments".into()).at(el, ec));
                                    }
                                    let bindings_list = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type("with-syntax: expected bindings list".into()).at(el, ec)),
                                    };
                                    let mut patterns = Vec::new();
                                    let mut rhs_exprs = Vec::new();
                                    for b in bindings_list {
                                        match &b.kind {
                                            ExprKind::List(parts) if parts.len() == 2 => {
                                                // Pattern is first element; wrap in a list for matching
                                                let pat = match &parts[0].kind {
                                                    ExprKind::List(p) => p.clone(),
                                                    _ => vec![parts[0].clone()],
                                                };
                                                patterns.push(pat);
                                                rhs_exprs.push(parts[1].clone());
                                            }
                                            _ => return Err(EvalError::Type("with-syntax: expected (pattern expr) binding".into()).at(el, ec)),
                                        }
                                    }
                                    let body = items[2..].to_vec();
                                    if rhs_exprs.is_empty() {
                                        // No bindings, just evaluate body
                                        eval_body(&body, env, &mut kont)
                                    } else {
                                        let first_rhs = rhs_exprs.remove(0);
                                        kont.push(KontFrame::WithSyntax {
                                            patterns,
                                            rhs_exprs,
                                            done_bindings: HashMap::new(),
                                            done_evars: HashSet::new(),
                                            body,
                                            env: env.clone(),
                                            el, ec,
                                        });
                                        Ctrl::Eval(first_rhs, env)
                                    }
                                }
                                _ => {
                                    // Check for macro invocation
                                    if let Some(ref name) = op_name {
                                        match env_get(&env, name) {
                                            Some(Value::Macro { literals, rules, def_env }) => {
                                                let expanded = expand_macro(&items, &literals, &rules, &def_env, &env, el, ec)?;
                                                Ctrl::Eval(expanded, env)
                                            }
                                            Some(Value::MacroTransformer(transformer)) => {
                                                // Convert input form to syntax object and call transformer
                                                let stx = Value::Syntax(Expr::new(ExprKind::List(items.clone()), el, ec));
                                                MACRO_USE_ENV.with(|mue| mue.borrow_mut().push(env.clone()));
                                                kont.push(KontFrame::MacroExpand { env: env.clone(), el, ec });
                                                apply_func(*transformer, vec![stx], &mut kont, &mut wind, el, ec)?
                                            }
                                            _ => {
                                                let func_expr = items[0].clone();
                                                let arg_exprs = items[1..].to_vec();
                                                kont.push(KontFrame::EvalFunc { arg_exprs, env: env.clone(), el, ec });
                                                Ctrl::Eval(func_expr, env)
                                            }
                                        }
                                    } else {
                                        let func_expr = items[0].clone();
                                        let arg_exprs = items[1..].to_vec();
                                        kont.push(KontFrame::EvalFunc { arg_exprs, env: env.clone(), el, ec });
                                        Ctrl::Eval(func_expr, env)
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Ctrl::Val(val) => {
                if let Some(frame) = kont.pop() {
                    match frame {
                        KontFrame::Seq { mut rest, env } => {
                            // Discard val, evaluate next
                            if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::Seq { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::If { then_br, else_br, env } => {
                            if val.is_truthy() {
                                Ctrl::Eval(then_br, env)
                            } else if let Some(eb) = else_br {
                                Ctrl::Eval(eb, env)
                            } else {
                                Ctrl::Val(Value::Void)
                            }
                        }
                        KontFrame::Define { name, env } => {
                            env_set(&env, name, val);
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::Set { name, env, el, ec } => {
                            if !env_set_existing(&env, &name, val) {
                                return Err(EvalError::UnboundVariable(name).at(el, ec));
                            }
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::EvalFunc { mut arg_exprs, env, el, ec } => {
                            // val is the evaluated function; now evaluate args right-to-left
                            if arg_exprs.is_empty() {
                                apply_func(val, vec![], &mut kont, &mut wind, el, ec)?
                            } else {
                                let last = arg_exprs.pop().unwrap();
                                kont.push(KontFrame::EvalArg { func: val, remaining: arg_exprs, done: vec![], env: env.clone(), el, ec });
                                Ctrl::Eval(last, env)
                            }
                        }
                        KontFrame::EvalArg { func, mut remaining, mut done, env, el, ec } => {
                            done.push(val);
                            if remaining.is_empty() {
                                done.reverse();
                                apply_func(func, done, &mut kont, &mut wind, el, ec)?
                            } else {
                                let next = remaining.pop().unwrap();
                                kont.push(KontFrame::EvalArg { func, remaining, done, env: env.clone(), el, ec });
                                Ctrl::Eval(next, env)
                            }
                        }
                        KontFrame::And { mut rest, env } => {
                            if !val.is_truthy() {
                                Ctrl::Val(val)
                            } else if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::And { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::Or { mut rest, env } => {
                            if val.is_truthy() {
                                Ctrl::Val(val)
                            } else if rest.len() == 1 {
                                Ctrl::Eval(rest.remove(0), env)
                            } else {
                                let first = rest.remove(0);
                                kont.push(KontFrame::Or { rest, env: env.clone() });
                                Ctrl::Eval(first, env)
                            }
                        }
                        KontFrame::LetBind { name, mut done, mut remaining, body, outer } => {
                            done.push((name, val));
                            if remaining.is_empty() {
                                let local_env = new_env(Some(outer));
                                for (n, v) in done {
                                    env_set(&local_env, n, v);
                                }
                                eval_body(&body, local_env, &mut kont)
                            } else {
                                let (next_name, next_expr) = remaining.remove(0);
                                kont.push(KontFrame::LetBind { name: next_name, done, remaining, body, outer: outer.clone() });
                                Ctrl::Eval(next_expr, outer)
                            }
                        }
                        KontFrame::NamedLetBind { loop_name, all_params, mut done_vals, mut remaining_inits, body, outer } => {
                            done_vals.push(val);
                            if remaining_inits.is_empty() {
                                let local_env = new_env(Some(outer));
                                let loop_lambda = Value::Lambda {
                                    params: all_params.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: local_env.clone(),
                                };
                                env_set(&local_env, loop_name, loop_lambda);
                                let call_env = new_env(Some(local_env));
                                for (p, v) in all_params.iter().zip(done_vals) {
                                    env_set(&call_env, p.clone(), v);
                                }
                                eval_body(&body, call_env, &mut kont)
                            } else {
                                let next_init = remaining_inits.remove(0);
                                kont.push(KontFrame::NamedLetBind { loop_name, all_params, done_vals, remaining_inits, body, outer: outer.clone() });
                                Ctrl::Eval(next_init, outer)
                            }
                        }
                        KontFrame::CondClause { body, rest_clauses, env, el, ec } => {
                            if val.is_truthy() {
                                // Check for => syntax: (test => proc)
                                if body.len() == 2 && matches!(&body[0].kind, ExprKind::Symbol(s) if s == "=>") {
                                    // Evaluate proc, then apply it to the test result
                                    kont.push(KontFrame::CondArrow { test_val: val, el, ec });
                                    Ctrl::Eval(body[1].clone(), env)
                                } else if body.is_empty() {
                                    Ctrl::Val(val)
                                } else {
                                    eval_body(&body, env, &mut kont)
                                }
                            } else if rest_clauses.is_empty() {
                                Ctrl::Val(Value::Void)
                            } else {
                                eval_cond(&rest_clauses, env, el, ec, &mut kont)?
                            }
                        }
                        KontFrame::CondArrow { test_val, el, ec } => {
                            // val is the proc, apply it to test_val
                            apply_func(val, vec![test_val], &mut kont, &mut wind, el, ec)?
                        }
                        KontFrame::StrSetIdx { var, ch_expr, env, el, ec } => {
                            let idx = as_integer(&val).map_err(|e| e.at(el, ec))? as usize;
                            kont.push(KontFrame::StrSetCh { var, idx, env: env.clone(), el, ec });
                            Ctrl::Eval(ch_expr, env)
                        }
                        KontFrame::StrSetCh { var, idx, env, el, ec } => {
                            let ch = match val {
                                Value::Char(c) => c,
                                _ => return Err(EvalError::Type("string-set!: third argument must be a character".into()).at(el, ec)),
                            };
                            env_mutate_string(&env, &var, idx, ch).map_err(|e| e.at(el, ec))?;
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::CallCC { el, ec } => {
                            // val is the function to call with the continuation
                            let cont_val = Value::Continuation(kont.clone(), wind.clone());
                            apply_func(val, vec![cont_val], &mut kont, &mut wind, el, ec)?
                        }
                        KontFrame::LetrecBind { names, current_idx, mut values, mut remaining_inits, body, env, sequential } => {
                            if sequential {
                                // letrec*: set binding immediately
                                env_set(&env, names[current_idx].clone(), val.clone());
                            }
                            values.push(val);
                            if remaining_inits.is_empty() {
                                if !sequential {
                                    // letrec: set all bindings now
                                    for (i, v) in values.into_iter().enumerate() {
                                        env_set(&env, names[i].clone(), v);
                                    }
                                }
                                eval_body(&body, env, &mut kont)
                            } else {
                                let next = remaining_inits.remove(0);
                                let next_idx = current_idx + 1;
                                kont.push(KontFrame::LetrecBind {
                                    names, current_idx: next_idx, values, remaining_inits, body, env: env.clone(), sequential,
                                });
                                Ctrl::Eval(next, env)
                            }
                        }
                        KontFrame::CaseKey { clauses, env, el, ec } => {
                            // val is the key; match against clauses
                            let mut result = Ctrl::Val(Value::Void);
                            for clause in &clauses {
                                match &clause.kind {
                                    ExprKind::List(parts) if !parts.is_empty() => {
                                        // Check for else clause
                                        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(ref s) if s == "else");
                                        if is_else {
                                            result = eval_body(&parts[1..], env, &mut kont);
                                            break;
                                        }
                                        // Check datums
                                        let datums = match &parts[0].kind {
                                            ExprKind::List(ds) => ds,
                                            _ => return Err(EvalError::Type("case: expected datum list".into()).at(el, ec)),
                                        };
                                        let mut matched = false;
                                        for d in datums {
                                            let dval = expr_to_value(d);
                                            if values_eqv(&val, &dval) {
                                                matched = true;
                                                break;
                                            }
                                        }
                                        if matched {
                                            result = eval_body(&parts[1..], env, &mut kont);
                                            break;
                                        }
                                    }
                                    _ => return Err(EvalError::Type("case: invalid clause".into()).at(el, ec)),
                                }
                            }
                            result
                        }
                        KontFrame::DynWindBody { in_thunk, body_thunk, out_thunk, wind_id, el, ec } => {
                            // in-thunk just returned; push wind entry and call body
                            wind.push(WindEntry { id: wind_id, in_thunk, out_thunk });
                            kont.push(KontFrame::DynWindOut { wind_id, el, ec });
                            apply_func(body_thunk, vec![], &mut kont, &mut wind, el, ec)?
                        }
                        KontFrame::DynWindOut { wind_id, el, ec } => {
                            // body just returned; pop wind entry and call out-thunk
                            let body_result = val;
                            if let Some(pos) = wind.iter().rposition(|e| e.id == wind_id) {
                                let entry = wind.remove(pos);
                                kont.push(KontFrame::DynWindDone { body_result });
                                apply_func(entry.out_thunk, vec![], &mut kont, &mut wind, el, ec)?
                            } else {
                                Ctrl::Val(body_result)
                            }
                        }
                        KontFrame::DynWindDone { body_result } => {
                            // out-thunk returned; return the saved body result
                            Ctrl::Val(body_result)
                        }
                        KontFrame::WindTransfer { mut pending_outs, mut pending_ins, target_kont, target_wind, value } => {
                            // Process wind transfer: first call out-thunks (innermost first), then in-thunks
                            if !pending_outs.is_empty() {
                                let out_thunk = pending_outs.remove(0);
                                kont.push(KontFrame::WindTransfer {
                                    pending_outs,
                                    pending_ins,
                                    target_kont,
                                    target_wind,
                                    value,
                                });
                                apply_func(out_thunk, vec![], &mut kont, &mut wind, 0, 0)?
                            } else if !pending_ins.is_empty() {
                                let entry = pending_ins.remove(0);
                                let in_thunk = entry.in_thunk.clone();
                                wind.push(entry);
                                kont.push(KontFrame::WindTransfer {
                                    pending_outs: vec![],
                                    pending_ins,
                                    target_kont,
                                    target_wind,
                                    value,
                                });
                                apply_func(in_thunk, vec![], &mut kont, &mut wind, 0, 0)?
                            } else {
                                // All winding done; restore target
                                kont = target_kont;
                                wind = target_wind;
                                Ctrl::Val(value)
                            }
                        }
                        KontFrame::ExceptionHandler { .. } => {
                            // Thunk returned normally; just pass value through
                            Ctrl::Val(val)
                        }
                        KontFrame::GuardHandler { .. } => {
                            // Body returned normally; just pass value through
                            Ctrl::Val(val)
                        }
                        KontFrame::RaiseUnwind { mut pending_outs, handler_action, exception } => {
                            if !pending_outs.is_empty() {
                                let out_thunk = pending_outs.remove(0);
                                kont.push(KontFrame::RaiseUnwind { pending_outs, handler_action, exception });
                                apply_func(out_thunk, vec![], &mut kont, &mut wind, 0, 0)?
                            } else {
                                match handler_action {
                                    RaiseAction::CallHandler { handler } => {
                                        apply_func(handler, vec![exception], &mut kont, &mut wind, 0, 0)?
                                    }
                                    RaiseAction::EvalGuard { var, clauses, env } => {
                                        eval_guard_clauses(exception, &var, &clauses, &env, &mut kont)?
                                    }
                                }
                            }
                        }
                        KontFrame::GuardClause { body, rest_clauses, env, exception } => {
                            if val.is_truthy() {
                                if body.is_empty() {
                                    Ctrl::Val(val)
                                } else {
                                    eval_body(&body, env, &mut kont)
                                }
                            } else if rest_clauses.is_empty() {
                                return Err(EvalError::Type(format!("guard: no matching clause for {}", exception.display())));
                            } else {
                                match &rest_clauses[0].kind {
                                    ExprKind::List(parts) if !parts.is_empty() => {
                                        let is_else = matches!(&parts[0].kind, ExprKind::Symbol(ref s) if s == "else");
                                        if is_else {
                                            eval_body(&parts[1..], env, &mut kont)
                                        } else {
                                            let body = parts[1..].to_vec();
                                            let rest = rest_clauses[1..].to_vec();
                                            kont.push(KontFrame::GuardClause { body, rest_clauses: rest, env: env.clone(), exception });
                                            Ctrl::Eval(parts[0].clone(), env)
                                        }
                                    }
                                    _ => return Err(EvalError::Type("guard: invalid clause".into())),
                                }
                            }
                        }
                        KontFrame::CallWithValues { consumer, el, ec } => {
                            let args = match val {
                                Value::Values(vs) => vs,
                                other => vec![other],
                            };
                            apply_func(consumer, args, &mut kont, &mut wind, el, ec)?
                        }
                        KontFrame::MapIter { func, remaining, remaining_idx, mut done, el, ec } => {
                            done.push(val);
                            if remaining_idx >= remaining[0].len() {
                                // All elements processed
                                Ctrl::Val(list_from_vec(done))
                            } else {
                                let call_args: Vec<Value> = remaining.iter().map(|l| l[remaining_idx].clone()).collect();
                                kont.push(KontFrame::MapIter { func: func.clone(), remaining, remaining_idx: remaining_idx + 1, done, el, ec });
                                apply_func(func, call_args, &mut kont, &mut wind, el, ec)?
                            }
                        }
                        KontFrame::ForEachIter { func, remaining, remaining_idx, el, ec } => {
                            if remaining_idx >= remaining[0].len() {
                                Ctrl::Val(Value::Void)
                            } else {
                                let call_args: Vec<Value> = remaining.iter().map(|l| l[remaining_idx].clone()).collect();
                                kont.push(KontFrame::ForEachIter { func: func.clone(), remaining, remaining_idx: remaining_idx + 1, el, ec });
                                apply_func(func, call_args, &mut kont, &mut wind, el, ec)?
                            }
                        }
                        KontFrame::DefineSyntax { name, env } => {
                            // Store evaluated transformer as MacroTransformer
                            env_set(&env, name, Value::MacroTransformer(Box::new(val)));
                            Ctrl::Val(Value::Void)
                        }
                        KontFrame::MacroExpand { env, el, ec } => {
                            // Pop use-env from thread-local
                            MACRO_USE_ENV.with(|mue| mue.borrow_mut().pop());
                            match val {
                                Value::Syntax(expr) => Ctrl::Eval(expr, env),
                                _ => Err(EvalError::Type("macro transformer must return a syntax object".into()).at(el, ec))?,
                            }
                        }
                        KontFrame::SyntaxCase { literals, clauses, env, el, ec } => {
                            // val is the evaluated stx expression
                            let stx_expr = match &val {
                                Value::Syntax(e) => e.clone(),
                                _ => return Err(EvalError::Type("syntax-case: expected syntax object".into()).at(el, ec)),
                            };
                            let stx_items = match &stx_expr.kind {
                                ExprKind::List(items) => items.clone(),
                                _ => vec![stx_expr.clone()],
                            };
                            // Try each clause
                            let mut matched_result = None;
                            for (pattern_vec, _fender, body) in clauses {
                                let pattern = &pattern_vec[0];
                                let mut bindings = HashMap::new();
                                let mut evars = HashSet::new();
                                let matched = match &pattern.kind {
                                    ExprKind::List(pat_items) => {
                                        macro_match_pattern(pat_items, &stx_items, &literals, &mut bindings, &mut evars)
                                    }
                                    ExprKind::Symbol(s) => {
                                        bindings.insert(s.clone(), vec![stx_expr.clone()]);
                                        true
                                    }
                                    _ => false,
                                };
                                if matched {
                                    matched_result = Some((bindings, evars, body));
                                    break;
                                }
                            }
                            if let Some((bindings, evars, body)) = matched_result {
                                SYNTAX_BINDINGS.with(|sb| {
                                    sb.borrow_mut().push(SyntaxBindingEntry {
                                        bindings,
                                        ellipsis_vars: evars,
                                    });
                                });
                                kont.push(KontFrame::SyntaxCaseCleanup);
                                Ctrl::Eval(body, env)
                            } else {
                                return Err(EvalError::Parse("syntax-case: no matching clause".into()).at(el, ec));
                            }
                        }
                        KontFrame::SyntaxCaseCleanup => {
                            SYNTAX_BINDINGS.with(|sb| sb.borrow_mut().pop());
                            Ctrl::Val(val)
                        }
                        KontFrame::WithSyntax { mut patterns, mut rhs_exprs, mut done_bindings, mut done_evars, body, env, el, ec } => {
                            // val is the evaluated RHS; match against the current pattern
                            let pat = patterns.remove(0);
                            let stx_expr = match &val {
                                Value::Syntax(e) => e.clone(),
                                // Also accept non-syntax values (e.g., from datum->syntax which returns Syntax, but just in case)
                                _ => return Err(EvalError::Type("with-syntax: expected syntax object".into()).at(el, ec)),
                            };
                            let stx_items = match &stx_expr.kind {
                                ExprKind::List(items) => items.clone(),
                                _ => vec![stx_expr.clone()],
                            };
                            // Single pattern variable: just bind
                            if pat.len() == 1 {
                                if let ExprKind::Symbol(s) = &pat[0].kind {
                                    done_bindings.insert(s.clone(), vec![stx_expr]);
                                } else {
                                    macro_match_pattern(&pat, &stx_items, &[], &mut done_bindings, &mut done_evars);
                                }
                            } else {
                                macro_match_pattern(&pat, &stx_items, &[], &mut done_bindings, &mut done_evars);
                            }

                            if rhs_exprs.is_empty() {
                                // All bindings done; merge with current syntax bindings and evaluate body
                                SYNTAX_BINDINGS.with(|sb| {
                                    let mut stack = sb.borrow_mut();
                                    // Merge with existing top entry if present
                                    if let Some(top) = stack.last_mut() {
                                        top.bindings.extend(done_bindings);
                                        top.ellipsis_vars.extend(done_evars);
                                    } else {
                                        stack.push(SyntaxBindingEntry {
                                            bindings: done_bindings,
                                            ellipsis_vars: done_evars,
                                        });
                                    }
                                });
                                kont.push(KontFrame::WithSyntaxCleanup);
                                eval_body(&body, env, &mut kont)
                            } else {
                                let next_rhs = rhs_exprs.remove(0);
                                kont.push(KontFrame::WithSyntax {
                                    patterns,
                                    rhs_exprs,
                                    done_bindings,
                                    done_evars,
                                    body,
                                    env: env.clone(),
                                    el, ec,
                                });
                                Ctrl::Eval(next_rhs, env)
                            }
                        }
                        KontFrame::WithSyntaxCleanup => {
                            // We merged into the existing top entry, so we just need to undo the merge.
                            // For simplicity, we don't undo — the SyntaxCaseCleanup will pop the whole entry.
                            Ctrl::Val(val)
                        }
                    }
                } else {
                    return Ok(val);
                }
            }
        };
    }
}

fn env_mutate_string(env: &Env, name: &str, idx: usize, ch: char) -> Result<(), EvalError> {
    let mut inner = env.borrow_mut();
    if let Some(val) = inner.bindings.get_mut(name) {
        if let Value::Str(ref mut s) = val {
            let mut chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            chars[idx] = ch;
            *s = chars.into_iter().collect();
            return Ok(());
        }
        return Err(EvalError::Type("string-set!: not a string".into()));
    }
    drop(inner);
    let inner = env.borrow();
    if let Some(ref parent) = inner.parent {
        return env_mutate_string(parent, name, idx, ch);
    }
    Err(EvalError::UnboundVariable(name.to_string()))
}

fn bind_lambda_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    _env: &Env,
    el: u32,
    ec: u32,
) -> Result<(), EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {}",
                params.len(),
                args.len()
            ))
            .at(el, ec));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "expected {} arguments, got {}",
            params.len(),
            args.len()
        ))
        .at(el, ec));
    }
    Ok(())
}

fn eval_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut result = Value::Integer(0);
            for a in args {
                result = num_add(&result, a)?;
            }
            Ok(result)
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return num_negate(&args[0]);
            }
            let mut result = args[0].clone();
            for a in &args[1..] {
                result = num_sub(&result, a)?;
            }
            Ok(result)
        }
        "*" => {
            let mut result = Value::Integer(1);
            for a in args {
                result = num_mul(&result, a)?;
            }
            Ok(result)
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            if args.len() == 1 {
                return num_div(&Value::Integer(1), &args[0]);
            }
            let mut result = args[0].clone();
            for a in &args[1..] {
                result = num_div(&result, a)?;
            }
            Ok(result)
        }
        "<" => val_cmp_op(args, |a, b| a < b),
        ">" => val_cmp_op(args, |a, b| a > b),
        "=" => val_cmp_op(args, |a, b| a == b),
        "<=" => val_cmp_op(args, |a, b| a <= b),
        ">=" => val_cmp_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not requires 1 argument".into()));
            }
            Ok(Value::Boolean(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons requires 2 arguments".into()));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(p) => Ok(p.borrow().0.clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    if items.len() == 1 {
                        Ok(Value::List(vec![]))
                    } else {
                        Ok(list_from_vec(items[1..].to_vec()))
                    }
                }
                Value::Pair(p) => Ok(p.borrow().1.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(list_from_vec(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
            match value_to_vec(&args[0]) {
                Some(items) => Ok(Value::Integer(items.len() as i64)),
                None => Err(EvalError::Type("length: not a proper list".into())),
            }
        }
        "append" => {
            if args.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Last arg can be any value (tail of result)
            if args.len() == 1 {
                return Ok(args[0].clone());
            }
            let mut result = Vec::new();
            for a in &args[..args.len()-1] {
                match value_to_vec(a) {
                    Some(items) => result.extend(items),
                    None => return Err(EvalError::Type("append: not a list".into())),
                }
            }
            let last = &args[args.len()-1];
            // If last arg is a proper list, extend; otherwise create improper list
            match value_to_vec(last) {
                Some(items) => {
                    result.extend(items);
                    Ok(list_from_vec(result))
                }
                None => {
                    // improper list: build pair chain ending with last
                    let mut tail = last.clone();
                    for item in result.into_iter().rev() {
                        tail = make_pair(item, tail);
                    }
                    Ok(tail)
                }
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("reverse requires 1 argument".into()));
            }
            match value_to_vec(&args[0]) {
                Some(mut items) => {
                    items.reverse();
                    Ok(list_from_vec(items))
                }
                None => Err(EvalError::Type("reverse: not a list".into())),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Float(_) | Value::Rational(..))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("boolean? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("pair? requires 1 argument".into()));
            }
            Ok(Value::Boolean(
                matches!(&args[0], Value::List(items) if !items.is_empty())
                    || matches!(&args[0], Value::Pair(_)),
            ))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("display requires 1 argument".into()));
            }
            let text = args[0].display_write(false);
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("write requires 1 argument".into()));
            }
            let text = args[0].display_write(true);
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&text));
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity("newline requires 0 arguments".into()));
            }
            OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push('\n'));
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: not a string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-length requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type("string-length: not a string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("substring requires 3 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("substring: not a string".into())),
            };
            let start = as_integer(&args[1])? as usize;
            let end = as_integer(&args[2])? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->number requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type("string->number: not a string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("number->string requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Str(n.to_string())),
                Value::Float(_) => Ok(Value::Str(args[0].display())),
                Value::Rational(n, d) => Ok(Value::Str(format!("{}/{}", n, d))),
                _ => Err(EvalError::Type("number->string: not a number".into())),
            }
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("symbol->string requires 1 argument".into()));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("symbol->string: not a symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->symbol requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: not a string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-ref requires 2 arguments".into()));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type("string-ref: not a string".into())),
            };
            let idx = as_integer(&args[1])? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                EvalError::Type("string-ref: index out of range".into())
            })?))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-copy requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type("string-copy: not a string".into())),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string->list requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => {
                    let chars: Vec<Value> = s.chars().map(Value::Char).collect();
                    Ok(list_from_vec(chars))
                }
                _ => Err(EvalError::Type("string->list: not a string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->string requires 1 argument".into()));
            }
            let items = match value_to_vec(&args[0]) {
                Some(items) => items,
                None => return Err(EvalError::Type("list->string: not a proper list".into())),
            };
            let mut s = String::new();
            for item in &items {
                match item {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: element is not a character".into())),
                }
            }
            Ok(Value::Str(s))
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char->integer requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::Type("char->integer: not a character".into())),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("integer->char requires 1 argument".into()));
            }
            let n = as_integer(&args[0])?;
            match char::from_u32(n as u32) {
                Some(c) => Ok(Value::Char(c)),
                None => Err(EvalError::Type("integer->char: invalid code point".into())),
            }
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("eq? requires 2 arguments".into()));
            }
            Ok(Value::Boolean(values_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("equal? requires 2 arguments".into()));
            }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("abs requires 1 argument".into()));
            }
            Ok(Value::Integer(as_integer(&args[0])?.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("modulo requires 2 arguments".into()));
            }
            let a = as_integer(&args[0])?;
            let b = as_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("remainder requires 2 arguments".into()));
            }
            let a = as_integer(&args[0])?;
            let b = as_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("quotient requires 2 arguments".into()));
            }
            let a = as_integer(&args[0])?;
            let b = as_integer(&args[1])?;
            if b == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity("min requires at least 1 argument".into()));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                let v = as_integer(a)?;
                if v < result {
                    result = v;
                }
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity("max requires at least 1 argument".into()));
            }
            let mut result = as_integer(&args[0])?;
            for a in &args[1..] {
                let v = as_integer(a)?;
                if v > result {
                    result = v;
                }
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("expt requires 2 arguments".into()));
            }
            let base = as_integer(&args[0])?;
            let exp = as_integer(&args[1])?;
            if exp < 0 {
                return Err(EvalError::Type("expt: negative exponent".into()));
            }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("zero? requires 1 argument".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("positive? requires 1 argument".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("negative? requires 1 argument".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("odd? requires 1 argument".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("even? requires 1 argument".into()));
            }
            Ok(Value::Boolean(as_integer(&args[0])? % 2 == 0))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-ref requires 2 arguments".into()));
            }
            let idx = as_integer(&args[1])? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match &cur {
                    Value::Pair(p) => {
                        let next = p.borrow().1.clone();
                        cur = next;
                    }
                    Value::List(items) if !items.is_empty() => {
                        // remaining items as a sub-slice
                        if items.len() <= 1 {
                            return Err(EvalError::Type("list-ref: index out of range".into()));
                        }
                        cur = list_from_vec(items[1..].to_vec());
                    }
                    _ => return Err(EvalError::Type("list-ref: index out of range".into())),
                }
            }
            match &cur {
                Value::Pair(p) => Ok(p.borrow().0.clone()),
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                _ => Err(EvalError::Type("list-ref: index out of range".into())),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-tail requires 2 arguments".into()));
            }
            let idx = as_integer(&args[1])? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match &cur {
                    Value::Pair(p) => {
                        let next = p.borrow().1.clone();
                        cur = next;
                    }
                    Value::List(items) if !items.is_empty() => {
                        if items.len() == 1 {
                            cur = Value::List(vec![]);
                        } else {
                            cur = list_from_vec(items[1..].to_vec());
                        }
                    }
                    _ => return Err(EvalError::Type("list-tail: index out of range".into())),
                }
            }
            Ok(cur)
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list? requires 1 argument".into()));
            }
            Ok(Value::Boolean(is_proper_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("assoc requires 2 arguments".into()));
            }
            let key = &args[0];
            let items = match value_to_vec(&args[1]) {
                Some(items) => items,
                None => return Err(EvalError::Type("assoc: not a list".into())),
            };
            for entry in &items {
                match entry {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_equal(key, &inner.0) {
                            return Ok(entry.clone());
                        }
                    }
                    Value::List(pair) if !pair.is_empty() => {
                        if values_equal(key, &pair[0]) {
                            return Ok(entry.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        "assq" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("assq requires 2 arguments".into()));
            }
            let key = &args[0];
            let items = match value_to_vec(&args[1]) {
                Some(items) => items,
                None => return Err(EvalError::Type("assq: not a list".into())),
            };
            for entry in &items {
                match entry {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_eq(key, &inner.0) {
                            return Ok(entry.clone());
                        }
                    }
                    Value::List(pair) if !pair.is_empty() => {
                        if values_eq(key, &pair[0]) {
                            return Ok(entry.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        "assv" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("assv requires 2 arguments".into()));
            }
            let key = &args[0];
            let items = match value_to_vec(&args[1]) {
                Some(items) => items,
                None => return Err(EvalError::Type("assv: not a list".into())),
            };
            for entry in &items {
                match entry {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_eqv(key, &inner.0) {
                            return Ok(entry.clone());
                        }
                    }
                    Value::List(pair) if !pair.is_empty() => {
                        if values_eqv(key, &pair[0]) {
                            return Ok(entry.clone());
                        }
                    }
                    _ => {}
                }
            }
            Ok(Value::Boolean(false))
        }
        "memq" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("memq requires 2 arguments".into()));
            }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_eq(key, &inner.0) {
                            drop(inner);
                            return Ok(cur);
                        }
                        let next = inner.1.clone();
                        drop(inner);
                        cur = next;
                    }
                    Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
                    Value::List(items) => {
                        for (i, item) in items.iter().enumerate() {
                            if values_eq(key, item) {
                                return Ok(list_from_vec(items[i..].to_vec()));
                            }
                        }
                        return Ok(Value::Boolean(false));
                    }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "member" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("member requires 2 arguments".into()));
            }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_equal(key, &inner.0) {
                            drop(inner);
                            return Ok(cur);
                        }
                        let next = inner.1.clone();
                        drop(inner);
                        cur = next;
                    }
                    Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
                    Value::List(items) => {
                        for (i, item) in items.iter().enumerate() {
                            if values_equal(key, item) {
                                return Ok(list_from_vec(items[i..].to_vec()));
                            }
                        }
                        return Ok(Value::Boolean(false));
                    }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "set-car!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("set-car! requires 2 arguments".into()));
            }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().0 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type("set-car!: not a pair".into())),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("set-cdr! requires 2 arguments".into()));
            }
            match &args[0] {
                Value::Pair(p) => {
                    p.borrow_mut().1 = args[1].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type("set-cdr!: not a pair".into())),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char-alphabetic? requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type("char-alphabetic?: not a character".into())),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char-numeric? requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: not a character".into())),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char-upcase requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: not a character".into())),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char-downcase requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: not a character".into())),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("char=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("char=?: not characters".into())),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("char<? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: not characters".into())),
            }
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type("string=?: not strings".into())),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string<? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("string<?: not strings".into())),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-ci=? requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => {
                    Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
                }
                _ => Err(EvalError::Type("string-ci=?: not strings".into())),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-upcase requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type("string-upcase: not a string".into())),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("string-downcase requires 1 argument".into()));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type("string-downcase: not a string".into())),
            }
        }
        "eqv?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("eqv? requires 2 arguments".into()));
            }
            Ok(Value::Boolean(values_eqv(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
            }
            let len = as_integer(&args[0])? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("vector-ref requires 2 arguments".into()));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = as_integer(&args[1])? as usize;
                    let items = v.borrow();
                    items.get(idx).cloned().ok_or_else(|| EvalError::Type("vector-ref: index out of range".into()))
                }
                _ => Err(EvalError::Type("vector-ref: not a vector".into())),
            }
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity("vector-set! requires 3 arguments".into()));
            }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = as_integer(&args[1])? as usize;
                    let mut items = v.borrow_mut();
                    if idx >= items.len() {
                        return Err(EvalError::Type("vector-set!: index out of range".into()));
                    }
                    items[idx] = args[2].clone();
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type("vector-set!: not a vector".into())),
            }
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector-length requires 1 argument".into()));
            }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::Type("vector-length: not a vector".into())),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("vector->list requires 1 argument".into()));
            }
            match &args[0] {
                Value::Vector(v) => Ok(list_from_vec(v.borrow().clone())),
                _ => Err(EvalError::Type("vector->list: not a vector".into())),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->vector requires 1 argument".into()));
            }
            match value_to_vec(&args[0]) {
                Some(items) => Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(items)))),
                None => Err(EvalError::Type("list->vector: not a list".into())),
            }
        }
        "for-each" => {
            // Handled in apply_func for continuation-based iteration
            // This path shouldn't be reached since for-each is intercepted in apply_func
            Err(EvalError::Type("for-each: should be handled in apply_func".into()))
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("procedure? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Lambda { .. } | Value::CaseLambda { .. } | Value::Builtin(_) | Value::Continuation(..))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("integer? requires 1 argument".into()));
            }
            // Rationals that simplify to integers (d==1) are already Value::Integer,
            // so we just check for Integer here. Floats that are whole numbers could
            // also be considered integer, but our rationals always simplify.
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "exact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("exact? requires 1 argument".into()));
            }
            Ok(Value::Boolean(is_exact(&args[0])))
        }
        "inexact?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("inexact? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("exact->inexact requires 1 argument".into()));
            }
            Ok(Value::Float(value_to_f64(&args[0])?))
        }
        "inexact->exact" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("inexact->exact requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(_) | Value::Rational(..) => Ok(args[0].clone()),
                Value::Float(f) => {
                    // Convert float to exact rational using continued fraction / simple approach
                    // For 0.5 -> 1/2, etc.
                    let bits = f.to_bits();
                    let sign = if bits >> 63 == 0 { 1i64 } else { -1i64 };
                    let exponent = ((bits >> 52) & 0x7FF) as i64 - 1023;
                    let mantissa = if exponent == -1023 {
                        (bits & 0x000F_FFFF_FFFF_FFFF) << 1
                    } else {
                        (bits & 0x000F_FFFF_FFFF_FFFF) | 0x0010_0000_0000_0000
                    } as i64;
                    // value = sign * mantissa * 2^(exponent - 52)
                    let exp = exponent - 52;
                    if exp >= 0 {
                        Ok(Value::Integer(sign * mantissa * (1i64 << exp as u32)))
                    } else {
                        let denom = 1i64 << (-exp) as u32;
                        Ok(make_rational(sign * mantissa, denom))
                    }
                }
                _ => Err(EvalError::Type("inexact->exact: not a number".into())),
            }
        }
        "numerator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("numerator requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Rational(n, _) => Ok(Value::Integer(*n)),
                _ => Err(EvalError::Type("numerator: not a rational".into())),
            }
        }
        "denominator" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("denominator requires 1 argument".into()));
            }
            match &args[0] {
                Value::Integer(_) => Ok(Value::Integer(1)),
                Value::Rational(_, d) => Ok(Value::Integer(*d)),
                _ => Err(EvalError::Type("denominator: not a rational".into())),
            }
        }
        "rational?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("rational? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_) | Value::Rational(..))))
        }
        // c*r compositions
        "caar" => { eval_builtin("car", &[eval_builtin("car", args)?]) }
        "cadr" => { eval_builtin("car", &[eval_builtin("cdr", args)?]) }
        "cdar" => { eval_builtin("cdr", &[eval_builtin("car", args)?]) }
        "cddr" => { eval_builtin("cdr", &[eval_builtin("cdr", args)?]) }
        "caaar" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("car", args)?])?]) }
        "caadr" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?]) }
        "cadar" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?]) }
        "caddr" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?]) }
        "cdaar" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("car", args)?])?]) }
        "cdadr" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?]) }
        "cddar" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?]) }
        "cdddr" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?]) }
        // 4-level c*r compositions
        "caaaar" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("car", &[eval_builtin("car", args)?])?])?]) }
        "caaadr" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?])?]) }
        "caadar" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?])?]) }
        "caaddr" => { eval_builtin("car", &[eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?])?]) }
        "cadaar" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("car", args)?])?])?]) }
        "cadadr" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?])?]) }
        "caddar" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?])?]) }
        "cadddr" => { eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?])?]) }
        "cdaaar" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("car", &[eval_builtin("car", args)?])?])?]) }
        "cdaadr" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?])?]) }
        "cdadar" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?])?]) }
        "cdaddr" => { eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?])?]) }
        "cddaar" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("car", args)?])?])?]) }
        "cddadr" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("car", &[eval_builtin("cdr", args)?])?])?]) }
        "cdddar" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("car", args)?])?])?]) }
        "cddddr" => { eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("cdr", &[eval_builtin("cdr", args)?])?])?]) }
        "error" => {
            if args.is_empty() {
                return Err(EvalError::Type("error".into()));
            }
            let msg = match &args[0] {
                Value::Str(s) => s.clone(),
                other => other.display(),
            };
            if args.len() > 1 {
                let irritants: Vec<String> = args[1..].iter().map(|a| a.display()).collect();
                Err(EvalError::Type(format!("{}: {}", msg, irritants.join(" "))))
            } else {
                Err(EvalError::Type(msg))
            }
        }
        "gcd" => {
            if args.is_empty() {
                return Ok(Value::Integer(0));
            }
            let mut result = as_integer(&args[0])?.abs();
            for a in &args[1..] {
                let v = as_integer(a)?.abs();
                result = gcd(result, v);
            }
            Ok(Value::Integer(result))
        }
        "lcm" => {
            if args.is_empty() {
                return Ok(Value::Integer(1));
            }
            let mut result = as_integer(&args[0])?.abs();
            for a in &args[1..] {
                let v = as_integer(a)?.abs();
                if result == 0 && v == 0 { result = 0; }
                else { result = result / gcd(result, v) * v; }
            }
            Ok(Value::Integer(result))
        }
        "char-whitespace?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("char-whitespace? requires 1 argument".into()));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_whitespace())),
                _ => Err(EvalError::Type("char-whitespace?: not a character".into())),
            }
        }
        "write-char" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("write-char requires 1 or 2 arguments".into()));
            }
            match &args[0] {
                Value::Char(c) => {
                    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push(*c));
                    Ok(Value::Void)
                }
                _ => Err(EvalError::Type("write-char: not a character".into())),
            }
        }
        "string-contains" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("string-contains requires 2 arguments".into()));
            }
            match (&args[0], &args[1]) {
                (Value::Str(haystack), Value::Str(needle)) => {
                    Ok(Value::Boolean(haystack.contains(needle.as_str())))
                }
                _ => Err(EvalError::Type("string-contains: not strings".into())),
            }
        }
        "floor" => {
            if args.len() != 1 { return Err(EvalError::Arity("floor requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.floor() as i64)),
                Value::Rational(n, d) => {
                    let q = *n / *d;
                    if (*n < 0) && (*n % *d != 0) { Ok(Value::Integer(q - 1)) } else { Ok(Value::Integer(q)) }
                }
                _ => Err(EvalError::Type("floor: not a number".into())),
            }
        }
        "ceiling" => {
            if args.len() != 1 { return Err(EvalError::Arity("ceiling requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.ceil() as i64)),
                Value::Rational(n, d) => {
                    let q = *n / *d;
                    if (*n > 0) && (*n % *d != 0) { Ok(Value::Integer(q + 1)) } else { Ok(Value::Integer(q)) }
                }
                _ => Err(EvalError::Type("ceiling: not a number".into())),
            }
        }
        "truncate" => {
            if args.len() != 1 { return Err(EvalError::Arity("truncate requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.trunc() as i64)),
                Value::Rational(n, d) => Ok(Value::Integer(*n / *d)),
                _ => Err(EvalError::Type("truncate: not a number".into())),
            }
        }
        "round" => {
            if args.len() != 1 { return Err(EvalError::Arity("round requires 1 argument".into())); }
            match &args[0] {
                Value::Integer(n) => Ok(Value::Integer(*n)),
                Value::Float(f) => Ok(Value::Integer(f.round() as i64)),
                _ => Err(EvalError::Type("round: not a number".into())),
            }
        }
        "sqrt" => {
            if args.len() != 1 { return Err(EvalError::Arity("sqrt requires 1 argument".into())); }
            let f = value_to_f64(&args[0])?;
            let r = f.sqrt();
            if r == (r as i64) as f64 { Ok(Value::Integer(r as i64)) } else { Ok(Value::Float(r)) }
        }
        "memv" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("memv requires 2 arguments".into()));
            }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Value::Pair(p) => {
                        let inner = p.borrow();
                        if values_eqv(key, &inner.0) {
                            drop(inner);
                            return Ok(cur);
                        }
                        let next = inner.1.clone();
                        drop(inner);
                        cur = next;
                    }
                    Value::List(items) if items.is_empty() => return Ok(Value::Boolean(false)),
                    Value::List(items) => {
                        for (i, item) in items.iter().enumerate() {
                            if values_eqv(key, item) {
                                return Ok(list_from_vec(items[i..].to_vec()));
                            }
                        }
                        return Ok(Value::Boolean(false));
                    }
                    _ => return Ok(Value::Boolean(false)),
                }
            }
        }
        "string" => {
            // (string ch1 ch2 ...) -> string made from the given characters
            let mut s = String::with_capacity(args.len());
            for a in args {
                match a {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("string: argument is not a character".into())),
                }
            }
            Ok(Value::Str(s))
        }
        "string>?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a > b)),
                _ => Err(EvalError::Type("string>?: not strings".into())),
            }
        }
        "string<=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a <= b)),
                _ => Err(EvalError::Type("string<=?: not strings".into())),
            }
        }
        "string>=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a >= b)),
                _ => Err(EvalError::Type("string>=?: not strings".into())),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-string requires 1 or 2 arguments".into()));
            }
            let len = as_integer(&args[0])? as usize;
            let ch = if args.len() == 2 {
                match &args[1] {
                    Value::Char(c) => *c,
                    _ => return Err(EvalError::Type("make-string: second argument must be a character".into())),
                }
            } else {
                '\0'
            };
            Ok(Value::Str(std::iter::repeat(ch).take(len).collect()))
        }
        "string-for-each" => {
            // Simple eager implementation - ok for correctness
            if args.len() != 2 {
                return Err(EvalError::Arity("string-for-each requires 2 arguments".into()));
            }
            let s = match &args[1] {
                Value::Str(s) => s.clone(),
                _ => return Err(EvalError::Type("string-for-each: not a string".into())),
            };
            let func = &args[0];
            for ch in s.chars() {
                match func {
                    Value::Lambda { params, rest_param, body, env } => {
                        let call_args = vec![Value::Char(ch)];
                        bind_lambda_args(params, rest_param, &call_args, env, 0, 0)?;
                        let local_env = new_env(Some(env.clone()));
                        for (p, a) in params.iter().zip(&call_args) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        let ctrl = eval_body(body, local_env, &mut Vec::new());
                        run_cek(ctrl, Vec::new())?;
                    }
                    Value::Builtin(ref bname) => {
                        eval_builtin(bname, &[Value::Char(ch)])?;
                    }
                    _ => return Err(EvalError::Type("string-for-each: not a procedure".into())),
                }
            }
            Ok(Value::Void)
        }
        "char>?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char>? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a > b)),
                _ => Err(EvalError::Type("char>?: not characters".into())),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type("char<?: not characters".into())),
            }
        }
        "char<=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a <= b)),
                _ => Err(EvalError::Type("char<=?: not characters".into())),
            }
        }
        "char>=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char>=? requires 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a >= b)),
                _ => Err(EvalError::Type("char>=?: not characters".into())),
            }
        }
        "syntax->datum" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("syntax->datum requires 1 argument".into()));
            }
            match &args[0] {
                Value::Syntax(expr) => Ok(expr_to_datum(expr)),
                _ => Err(EvalError::Type("syntax->datum: expected syntax object".into())),
            }
        }
        "datum->syntax" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("datum->syntax requires 2 arguments".into()));
            }
            // First arg is context syntax object (for hygiene); second is datum to convert
            let datum = &args[1];
            Ok(Value::Syntax(datum_to_expr(datum)))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

/// Convert an Expr (syntax object internals) to a Scheme datum Value.
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
            if items.is_empty() {
                Value::List(vec![])
            } else {
                // Handle dotted pair notation
                if items.len() >= 3 {
                    if let ExprKind::Symbol(ref s) = items[items.len() - 2].kind {
                        if s == "." {
                            let tail = expr_to_datum(&items[items.len() - 1]);
                            let mut result = tail;
                            for item in items[..items.len() - 2].iter().rev() {
                                result = make_pair(expr_to_datum(item), result);
                            }
                            return result;
                        }
                    }
                }
                list_from_vec(items.iter().map(expr_to_datum).collect())
            }
        }
    }
}

/// Convert a Scheme datum Value to an Expr for use in syntax objects.
fn datum_to_expr(val: &Value) -> Expr {
    match val {
        Value::Integer(n) => Expr::new(ExprKind::Integer(*n), 0, 0),
        Value::Float(f) => Expr::new(ExprKind::Float(*f), 0, 0),
        Value::Rational(n, d) => Expr::new(ExprKind::Rational(*n, *d), 0, 0),
        Value::Boolean(b) => Expr::new(ExprKind::Boolean(*b), 0, 0),
        Value::Str(s) => Expr::new(ExprKind::Str(s.clone()), 0, 0),
        Value::Symbol(s) => Expr::new(ExprKind::Symbol(s.clone()), 0, 0),
        Value::Char(c) => Expr::new(ExprKind::Char(*c), 0, 0),
        Value::Syntax(e) => e.clone(),
        Value::List(items) => {
            let exprs: Vec<Expr> = items.iter().map(datum_to_expr).collect();
            Expr::new(ExprKind::List(exprs), 0, 0)
        }
        Value::Pair(p) => {
            // Convert pair to ExprKind::List with dot notation
            let (car_val, cdr_val) = {
                let inner = p.borrow();
                (inner.0.clone(), inner.1.clone())
            };
            let mut items = vec![datum_to_expr(&car_val)];
            let mut cur = cdr_val;
            loop {
                match cur {
                    Value::Pair(p2) => {
                        let (car2, cdr2) = {
                            let inner2 = p2.borrow();
                            (inner2.0.clone(), inner2.1.clone())
                        };
                        items.push(datum_to_expr(&car2));
                        cur = cdr2;
                    }
                    Value::List(ref l) if l.is_empty() => {
                        return Expr::new(ExprKind::List(items), 0, 0);
                    }
                    _ => {
                        items.push(Expr::new(ExprKind::Symbol(".".into()), 0, 0));
                        items.push(datum_to_expr(&cur));
                        return Expr::new(ExprKind::List(items), 0, 0);
                    }
                }
            }
        }
        _ => Expr::new(ExprKind::Symbol(val.display()), 0, 0),
    }
}

fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Void, Value::Void) => true,
        (Value::List(x), Value::List(y)) if x.is_empty() && y.is_empty() => true,
        (Value::Pair(x), Value::Pair(y)) => std::rc::Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => std::rc::Rc::ptr_eq(x, y),
        _ => false,
    }
}

fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Void, Value::Void) => true,
        (Value::List(x), Value::List(y)) if x.is_empty() && y.is_empty() => true,
        (Value::Pair(x), Value::Pair(y)) => std::rc::Rc::ptr_eq(x, y),
        (Value::Vector(x), Value::Vector(y)) => std::rc::Rc::ptr_eq(x, y),
        _ => false,
    }
}

fn values_equal_inner(a: &Value, b: &Value, seen: &mut std::collections::HashSet<(usize, usize)>) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Void, Value::Void) => true,
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| values_equal_inner(a, b, seen))
        }
        (Value::Pair(p1), Value::Pair(p2)) => {
            let k = (std::rc::Rc::as_ptr(p1) as usize, std::rc::Rc::as_ptr(p2) as usize);
            if std::rc::Rc::ptr_eq(p1, p2) { return true; }
            if !seen.insert(k) { return true; }
            let inner1 = p1.borrow();
            let inner2 = p2.borrow();
            values_equal_inner(&inner1.0, &inner2.0, seen) && values_equal_inner(&inner1.1, &inner2.1, seen)
        }
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal_inner(a, b, seen))
        }
        // Handle mixed List/Pair comparisons
        (Value::Pair(_), Value::List(items)) | (Value::List(items), Value::Pair(_)) if items.is_empty() => false,
        (Value::Pair(p), Value::List(items)) => {
            if items.is_empty() { return false; }
            let inner = p.borrow();
            let rest = if items.len() == 1 { Value::List(vec![]) } else { list_from_vec(items[1..].to_vec()) };
            values_equal_inner(&inner.0, &items[0], seen) && values_equal_inner(&inner.1, &rest, seen)
        }
        (Value::List(items), Value::Pair(p)) => {
            if items.is_empty() { return false; }
            let inner = p.borrow();
            let rest = if items.len() == 1 { Value::List(vec![]) } else { list_from_vec(items[1..].to_vec()) };
            values_equal_inner(&items[0], &inner.0, seen) && values_equal_inner(&rest, &inner.1, seen)
        }
        _ => false,
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    let mut seen = std::collections::HashSet::new();
    values_equal_inner(a, b, &mut seen)
}

fn is_proper_list(v: &Value) -> bool {
    // Tortoise-and-hare cycle detection
    let mut slow = v.clone();
    let mut fast = v.clone();
    loop {
        // Advance fast by 2
        for _ in 0..2 {
            match &fast {
                Value::Pair(p) => {
                    let next = p.borrow().1.clone();
                    fast = next;
                }
                Value::List(items) if items.is_empty() => return true,
                Value::List(_) => return true,
                _ => return false,
            }
        }
        // Advance slow by 1
        match &slow {
            Value::Pair(p) => {
                let next = p.borrow().1.clone();
                slow = next;
            }
            Value::List(_) => return true,
            _ => return false,
        }
        // Check if they point to the same pair
        if let (Value::Pair(s), Value::Pair(f)) = (&slow, &fast) {
            if std::rc::Rc::ptr_eq(s, f) {
                return false; // cycle detected
            }
        }
    }
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
        panic!("zero denominator in make_rational");
    }
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

fn value_to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type("expected number".into())),
    }
}

fn is_exact(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(..))
}

/// Perform arithmetic on two numeric values, keeping exactness when possible.
fn num_add(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x + y)),
        (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
            Ok(make_rational(x * d + n, *d))
        }
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            Ok(make_rational(n1 * d2 + n2 * d1, d1 * d2))
        }
        _ => Ok(Value::Float(value_to_f64(a)? + value_to_f64(b)?)),
    }
}

fn num_sub(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x - y)),
        (Value::Integer(x), Value::Rational(n, d)) => Ok(make_rational(x * d - n, *d)),
        (Value::Rational(n, d), Value::Integer(x)) => Ok(make_rational(n - x * d, *d)),
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            Ok(make_rational(n1 * d2 - n2 * d1, d1 * d2))
        }
        _ => Ok(Value::Float(value_to_f64(a)? - value_to_f64(b)?)),
    }
}

fn num_mul(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x * y)),
        (Value::Integer(x), Value::Rational(n, d)) | (Value::Rational(n, d), Value::Integer(x)) => {
            Ok(make_rational(x * n, *d))
        }
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            Ok(make_rational(n1 * n2, d1 * d2))
        }
        _ => Ok(Value::Float(value_to_f64(a)? * value_to_f64(b)?)),
    }
}

fn num_div(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => {
            if *y == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(make_rational(*x, *y))
        }
        (Value::Integer(x), Value::Rational(n, d)) => {
            if *n == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(make_rational(x * d, *n))
        }
        (Value::Rational(n, d), Value::Integer(y)) => {
            if *y == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(make_rational(*n, d * y))
        }
        (Value::Rational(n1, d1), Value::Rational(n2, d2)) => {
            if *n2 == 0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(make_rational(n1 * d2, d1 * n2))
        }
        _ => {
            let dv = value_to_f64(b)?;
            if dv == 0.0 {
                return Err(EvalError::DivisionByZero(String::new()));
            }
            Ok(Value::Float(value_to_f64(a)? / dv))
        }
    }
}

fn num_negate(a: &Value) -> Result<Value, EvalError> {
    match a {
        Value::Integer(x) => Ok(Value::Integer(-x)),
        Value::Float(f) => Ok(Value::Float(-f)),
        Value::Rational(n, d) => Ok(Value::Rational(-n, *d)),
        _ => Err(EvalError::Type("expected number".into())),
    }
}

fn as_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn val_cmp_op(args: &[Value], f: fn(f64, f64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let mut prev = value_to_f64(&args[0])?;
    for a in &args[1..] {
        let cur = value_to_f64(a)?;
        if !f(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut kont = Vec::new();
    if exprs.len() > 1 {
        kont.push(KontFrame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
    }
    let ctrl = Ctrl::Eval(exprs[0].clone(), env);
    let result = run_cek(ctrl, kont)?;
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut kont = Vec::new();
    if exprs.len() > 1 {
        kont.push(KontFrame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
    }
    let ctrl = Ctrl::Eval(exprs[0].clone(), env);
    let result = run_cek(ctrl, kont)?;
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((result.display_write(false), output))
}

/// Evaluate Scheme expressions with a step budget. Each CEK machine
/// iteration counts as one step; exceeding `max_steps` returns an error.
pub fn eval_str_with_limit(input: &str, max_steps: u64) -> Result<String, EvalError> {
    STEP_LIMIT.with(|c| c.set(max_steps));
    STEP_COUNT.with(|c| c.set(0));
    let result = eval_str(input);
    STEP_LIMIT.with(|c| c.set(0));
    STEP_COUNT.with(|c| c.set(0));
    result
}

#[cfg(test)]
mod tests;
