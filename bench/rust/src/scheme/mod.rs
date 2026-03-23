pub mod error;

pub use error::EvalError;

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Void,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation(Vec<KontFrame>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Vector(std::rc::Rc<std::cell::RefCell<Vec<Value>>>),
}

thread_local! {
    static OUTPUT_BUFFER: std::cell::RefCell<String> = std::cell::RefCell::new(String::new());
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
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
            Value::Pair(a, b) => {
                let mut parts = vec![a.display()];
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Value::List(items) if items.is_empty() => break,
                        Value::Pair(ca, cb) => {
                            parts.push(ca.display());
                            cur = cb.as_ref();
                        }
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
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Continuation(_) => "#<continuation>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("#({})", inner.join(" "))
            }
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
            Value::Pair(a, b) => {
                let mut parts = vec![a.display_write(write_mode)];
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Value::List(items) if items.is_empty() => break,
                        Value::Pair(ca, cb) => {
                            parts.push(ca.display_write(write_mode));
                            cur = cb.as_ref();
                        }
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
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
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
fn apply_func(func: Value, args: Vec<Value>, kont: &mut Vec<KontFrame>, el: u32, ec: u32) -> Result<Ctrl, EvalError> {
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
                env_set(&local_env, rest.clone(), Value::List(rest_args));
            }
            Ok(eval_body(&body, local_env, kont))
        }
        Value::Builtin(ref name) if name == "call/cc" || name == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc requires 1 argument".into()).at(el, ec));
            }
            let cont_val = Value::Continuation(kont.clone());
            let f = args.into_iter().next().unwrap();
            apply_func(f, vec![cont_val], kont, el, ec)
        }
        Value::Builtin(ref name) if name == "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("map requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
                Value::List(items) => Ok(items),
                _ => Err(EvalError::Type("map: argument is not a list".into()).at(el, ec)),
            }).collect::<Result<Vec<_>, _>>()?;
            if lists.is_empty() {
                return Ok(Ctrl::Val(Value::List(vec![])));
            }
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len {
                    return Err(EvalError::Type("map: lists must have same length".into()).at(el, ec));
                }
            }
            // Iterative map using the CEK machine: we'll build the result list
            // by evaluating func on each set of elements. We do this by
            // constructing an equivalent expression and evaluating it.
            // Simpler approach: compute eagerly since we have all values.
            let mut result = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                // We need to apply func to call_args. Since we're inside the CEK machine,
                // we can't easily recurse. Instead, build the result iteratively.
                // For simplicity, use a mini CEK call for each element.
                let mini_result = match &func {
                    Value::Lambda { params, rest_param, body, env } => {
                        bind_lambda_args(params, rest_param, &call_args, env, el, ec)?;
                        let local_env = new_env(Some(env.clone()));
                        for (p, a) in params.iter().zip(&call_args) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        if let Some(ref rest) = rest_param {
                            let rest_args = if call_args.len() > params.len() {
                                call_args[params.len()..].to_vec()
                            } else {
                                Vec::new()
                            };
                            env_set(&local_env, rest.clone(), Value::List(rest_args));
                        }
                        let ctrl = eval_body(body, local_env, &mut Vec::new());
                        run_cek(ctrl, Vec::new())?
                    }
                    Value::Builtin(ref bname) => {
                        eval_builtin(bname, &call_args).map_err(|e| e.at(el, ec))?
                    }
                    _ => return Err(EvalError::Type("map: not a procedure".into()).at(el, ec)),
                };
                result.push(mini_result);
            }
            Ok(Ctrl::Val(Value::List(result)))
        }
        Value::Builtin(ref name) if name == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("apply requires at least 2 arguments".into()).at(el, ec));
            }
            let func = args[0].clone();
            let last = &args[args.len() - 1];
            let tail_args = match last {
                Value::List(items) => items.clone(),
                _ => return Err(EvalError::Type("apply: last argument must be a list".into()).at(el, ec)),
            };
            let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail_args);
            apply_func(func, all_args, kont, el, ec)
        }
        Value::Builtin(ref name) => {
            let result = eval_builtin(name, &args).map_err(|e| e.at(el, ec))?;
            Ok(Ctrl::Val(result))
        }
        Value::Continuation(saved) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into()).at(el, ec));
            }
            *kont = saved;
            Ok(Ctrl::Val(args.into_iter().next().unwrap()))
        }
        _ => Err(EvalError::Type("not a procedure".into()).at(el, ec)),
    }
}

/// Main CEK machine loop.
fn run_cek(initial_ctrl: Ctrl, initial_kont: Vec<KontFrame>) -> Result<Value, EvalError> {
    let mut ctrl = initial_ctrl;
    let mut kont = initial_kont;

    loop {
        ctrl = match ctrl {
            Ctrl::Eval(expr, env) => {
                let (el, ec) = (expr.line, expr.col);
                match expr.kind {
                    ExprKind::Integer(n) => Ctrl::Val(Value::Integer(n)),
                    ExprKind::Boolean(b) => Ctrl::Val(Value::Boolean(b)),
                    ExprKind::Str(s) => Ctrl::Val(Value::Str(s)),
                    ExprKind::Char(c) => Ctrl::Val(Value::Char(c)),
                    ExprKind::Symbol(ref name) => {
                        if is_builtin(name) {
                            Ctrl::Val(Value::Builtin(name.clone()))
                        } else {
                            match env_get(&env, name) {
                                Some(v) => Ctrl::Val(v),
                                None => return Err(EvalError::UnboundVariable(name.clone()).at(el, ec)),
                            }
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
                                    let (literals, rules) = parse_syntax_rules(&items[2], el, ec)?;
                                    env_set(&env, name, Value::Macro { literals, rules, def_env: env.clone() });
                                    Ctrl::Val(Value::Void)
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
                                _ => {
                                    // Check for macro invocation
                                    if let Some(ref name) = op_name {
                                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(&env, name) {
                                            let expanded = expand_macro(&items, &literals, &rules, &def_env, &env, el, ec)?;
                                            Ctrl::Eval(expanded, env)
                                        } else {
                                            let func_expr = items[0].clone();
                                            let arg_exprs = items[1..].to_vec();
                                            kont.push(KontFrame::EvalFunc { arg_exprs, env: env.clone(), el, ec });
                                            Ctrl::Eval(func_expr, env)
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
                                apply_func(val, vec![], &mut kont, el, ec)?
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
                                apply_func(func, done, &mut kont, el, ec)?
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
                                if body.is_empty() {
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
                            let cont_val = Value::Continuation(kont.clone());
                            apply_func(val, vec![cont_val], &mut kont, el, ec)?
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
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("- requires at least 1 argument".into()));
            }
            let first = as_integer(&args[0])?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= as_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity("/ requires at least 1 argument".into()));
            }
            let first = as_integer(&args[0])?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero(String::new()));
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = as_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero(String::new()));
                }
                result /= d;
            }
            Ok(Value::Integer(result))
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
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::Pair(a, _) => Ok(*a.clone()),
                _ => Err(EvalError::Type("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) if !items.is_empty() => {
                    Ok(Value::List(items[1..].to_vec()))
                }
                Value::Pair(_, b) => Ok(*b.clone()),
                _ => Err(EvalError::Type("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length requires 1 argument".into()));
            }
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
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
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
                    || matches!(&args[0], Value::Pair(_, _)),
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
            let n = as_integer(&args[0])?;
            Ok(Value::Str(n.to_string()))
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
                    Ok(Value::List(chars))
                }
                _ => Err(EvalError::Type("string->list: not a string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->string requires 1 argument".into()));
            }
            let items = match &args[0] {
                Value::List(v) => v.clone(),
                Value::Pair(..) => {
                    let mut items = Vec::new();
                    let mut cur = args[0].clone();
                    loop {
                        match cur {
                            Value::Pair(car, cdr) => {
                                items.push(*car);
                                cur = *cdr;
                            }
                            Value::List(ref v) if v.is_empty() => break,
                            _ => return Err(EvalError::Type("list->string: not a proper list".into())),
                        }
                    }
                    items
                }
                _ => return Err(EvalError::Type("list->string: not a proper list".into())),
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
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-ref: not a list".into())),
            };
            let idx = as_integer(&args[1])? as usize;
            items.get(idx).cloned().ok_or_else(|| EvalError::Type("list-ref: index out of range".into()))
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("list-tail requires 2 arguments".into()));
            }
            let items = match &args[0] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("list-tail: not a list".into())),
            };
            let idx = as_integer(&args[1])? as usize;
            if idx > items.len() {
                return Err(EvalError::Type("list-tail: index out of range".into()));
            }
            Ok(Value::List(items[idx..].to_vec()))
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
            let alist = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("assoc: not a list".into())),
            };
            for entry in alist {
                match entry {
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
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                _ => Err(EvalError::Type("vector->list: not a vector".into())),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("list->vector requires 1 argument".into()));
            }
            match &args[0] {
                Value::List(items) => Ok(Value::Vector(std::rc::Rc::new(std::cell::RefCell::new(items.clone())))),
                _ => Err(EvalError::Type("list->vector: not a list".into())),
            }
        }
        "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("for-each requires at least 2 arguments".into()));
            }
            // Simplified: just iterate without collecting results
            let func = &args[0];
            let items = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::Type("for-each: not a list".into())),
            };
            for item in items {
                match func {
                    Value::Lambda { params, rest_param, body, env } => {
                        bind_lambda_args(params, rest_param, &[item.clone()], env, 0, 0)?;
                        let local_env = new_env(Some(env.clone()));
                        for (p, a) in params.iter().zip(&[item.clone()]) {
                            env_set(&local_env, p.clone(), a.clone());
                        }
                        if let Some(ref rest) = rest_param {
                            env_set(&local_env, rest.clone(), Value::List(vec![]));
                        }
                        let ctrl = eval_body(body, local_env, &mut Vec::new());
                        run_cek(ctrl, Vec::new())?;
                    }
                    Value::Builtin(ref bname) => {
                        eval_builtin(bname, &[item.clone()])?;
                    }
                    _ => return Err(EvalError::Type("for-each: not a procedure".into())),
                }
            }
            Ok(Value::Void)
        }
        "procedure?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("procedure? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_))))
        }
        "integer?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("integer? requires 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Void, Value::Void) => true,
        (Value::List(x), Value::List(y)) if x.is_empty() && y.is_empty() => true,
        (Value::Vector(x), Value::Vector(y)) => std::rc::Rc::ptr_eq(x, y),
        _ => std::ptr::eq(a as *const _, b as *const _),
    }
}

fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Void, Value::Void) => true,
        (Value::List(x), Value::List(y)) if x.is_empty() && y.is_empty() => true,
        (Value::Vector(x), Value::Vector(y)) => std::rc::Rc::ptr_eq(x, y),
        _ => std::ptr::eq(a as *const _, b as *const _),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(x), Value::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Pair(a1, b1), Value::Pair(a2, b2)) => {
            values_equal(a1, a2) && values_equal(b1, b2)
        }
        (Value::Vector(x), Value::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_, b) => is_proper_list(b),
        _ => false,
    }
}

fn as_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type("expected integer".into())),
    }
}

fn val_cmp_op(args: &[Value], f: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let mut prev = as_integer(&args[0])?;
    for a in &args[1..] {
        let cur = as_integer(a)?;
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
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
