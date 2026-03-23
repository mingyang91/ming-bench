pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    static OUTPUT_BUF: RefCell<String> = RefCell::new(String::new());
}

fn output_write(s: &str) {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push_str(s));
}

fn output_take() -> String {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().split_off(0))
}

fn make_string(s: std::string::String) -> Value {
    Value::String(Rc::new(RefCell::new(s)))
}

/// A Scheme runtime value.
#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(Rc<RefCell<std::string::String>>),
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
    SchemeApply,
    Continuation(Rc<Vec<Frame>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({n})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::String(s) => write!(f, "String({:?})", s.borrow()),
            Value::Char(c) => write!(f, "Char({c:?})"),
            Value::List(l) => write!(f, "List({l:?})"),
            Value::Pair(a, b) => write!(f, "Pair({a:?}, {b:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::Void => write!(f, "Void"),
            Value::Lambda { params, rest_param, .. } => write!(f, "Lambda({params:?}, rest={rest_param:?})"),
            Value::Builtin(name, _) => write!(f, "Builtin({name})"),
            Value::CallCC => write!(f, "CallCC"),
            Value::SchemeApply => write!(f, "SchemeApply"),
            Value::Continuation(_) => write!(f, "Continuation(...)"),
            Value::Macro { .. } => write!(f, "Macro(...)"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => *a.borrow() == *b.borrow(),
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, b1), Value::Pair(a2, b2)) => a1 == a2 && b1 == b2,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Void, Value::Void) => true,
            (Value::CallCC, Value::CallCC) => true,
            (Value::SchemeApply, Value::SchemeApply) => true,
            _ => false,
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes, chars as raw character.
    fn display_fmt(&self, out: &mut String) {
        match self {
            Value::String(s) => out.push_str(&s.borrow()),
            Value::Char(c) => out.push(*c),
            Value::Macro { .. } => out.push_str("#<macro>"),
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
            Value::String(s) => write!(f, "\"{}\"", s.borrow()),
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
            Value::Lambda { .. } | Value::Builtin(..) | Value::CallCC | Value::SchemeApply | Value::Continuation(_) => write!(f, "#<procedure>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
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
        ExprKind::Str(s) => make_string(s.clone()),
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
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string(), func));
    }
    env_set(&env, "apply".to_string(), Value::SchemeApply);
    env_set(&env, "call/cc".to_string(), Value::CallCC);
    env_set(&env, "call-with-current-continuation".to_string(), Value::CallCC);
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

use std::sync::atomic::{AtomicUsize, Ordering};
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
    "if", "define", "lambda", "let", "set!", "begin", "quote",
    "and", "or", "cond", "define-syntax", "syntax-rules",
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
        act = match act {
            Act::Ev(e, env) => {
                last_line = e.line;
                last_col = e.col;
                step_eval(e, env, &mut stack).map_err(|e| e.at(last_line, last_col))?
            }
            Act::Ret(val) => match stack.pop() {
                Some(frame) => step_ret(val, frame, &mut stack).map_err(|e| e.at(last_line, last_col))?,
                None => return Ok(val),
            },
            Act::Ap(func, args) => step_apply(func, args, &mut stack).map_err(|e| e.at(last_line, last_col))?,
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
        ExprKind::Str(s) => Ok(Act::Ret(make_string(s.clone()))),
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
                    "begin" => return sf_seq(&elems[1..], env, stack),
                    "cond" => return sf_cond(&elems[1..], env, stack),
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
            let cont = Value::Continuation(Rc::new(stack.clone()));
            Ok(Act::Ap(proc, vec![cont]))
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
        Value::Continuation(frames) => {
            if args.is_empty() {
                return Err(EvalError::Arity("continuation expects 1 argument".into()));
            }
            *stack = (*frames).clone();
            Ok(Act::Ret(args.into_iter().next().unwrap()))
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
        "string?" => matches!(&args[0], Value::String(_)),
        "number?" => matches!(&args[0], Value::Integer(_)),
        "boolean?" => matches!(&args[0], Value::Boolean(_)),
        "pair?" => matches!(&args[0], Value::List(l) if !l.is_empty()) || matches!(&args[0], Value::Pair(..)),
        "symbol?" => matches!(&args[0], Value::Symbol(_)),
        "char?" => matches!(&args[0], Value::Char(_)),
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
            Value::String(s) => result.push_str(&s.borrow()),
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
        Value::String(s) => Ok(Value::Integer(s.borrow().len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Integer(start), Value::Integer(end)) => {
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
        Value::String(s) => match s.borrow().parse::<i64>() {
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
        Value::String(s) => Ok(Value::Symbol(s.borrow().clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref expects 2 arguments".into()));
    }
    match (&args[0], &args[1]) {
        (Value::String(s), Value::Integer(idx)) => {
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
        Value::String(s) => Ok(make_string(s.borrow().clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

fn builtin_string_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! expects 3 arguments".into()));
    }
    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Integer(idx), Value::Char(c)) => {
            let idx = *idx as usize;
            let mut borrowed = s.borrow_mut();
            let len = borrowed.len();
            if idx >= len {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            // Safe for ASCII; for full Unicode we work char-by-char
            let mut chars: Vec<char> = borrowed.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-set!: index out of range".into()));
            }
            chars[idx] = *c;
            *borrowed = chars.into_iter().collect();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("string-set!: expected string, integer, and char".into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
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
    // Clear any stale output
    output_take();
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
