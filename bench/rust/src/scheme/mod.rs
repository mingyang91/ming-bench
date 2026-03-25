pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

// ── Source positions ──

#[derive(Debug, Clone, Copy)]
struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// ── Values ──

#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Bool(bool),
    Char(char),
    Str(String, bool), // (content, mutable)
    Symbol(String),
    Pair(Rc<RefCell<(Val, Val)>>),
    Nil,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Void,
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Record {
        type_id: usize,
        type_name: String,
        fields: Vec<(String, Val)>,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>, // (params, rest_param, body)
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Val>>>),
    Continuation(Rc<ContData>),
    Values(Vec<Val>),
}

fn make_pair(car: Val, cdr: Val) -> Val {
    Val::Pair(Rc::new(RefCell::new((car, cdr))))
}

static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
struct RecordTypeInfo {
    type_name: String,
    ctor_name: String,
    field_names: Vec<String>,
}

thread_local! {
    static RECORD_REGISTRY: RefCell<HashMap<usize, RecordTypeInfo>> = RefCell::new(HashMap::new());
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Float(v) => {
                let s = format!("{}", v);
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    write!(f, "{}", s)
                } else {
                    write!(f, "{}.0", s)
                }
            }
            Val::Rational(n, d) => write!(f, "{}/{}", n, d),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Str(s, _) => write!(f, "\"{}\"", s),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::Nil => write!(f, "()"),
            Val::Pair(_) => {
                write!(f, "(")?;
                let mut cur = self.clone();
                let mut first = true;
                let mut seen = HashSet::new();
                loop {
                    match &cur {
                        Val::Pair(p) => {
                            let ptr = Rc::as_ptr(p) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            if !first { write!(f, " ")?; }
                            first = false;
                            let pair = p.borrow();
                            write!(f, "{}", pair.0)?;
                            let next = pair.1.clone();
                            drop(pair);
                            cur = next;
                        }
                        Val::Nil => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Val::Lambda { .. } => write!(f, "#<procedure>"),
            Val::CaseLambda { .. } => write!(f, "#<procedure>"),
            Val::Builtin(_) => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
            Val::Macro { .. } => write!(f, "#<macro>"),
            Val::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Val::Vector(v) => {
                write!(f, "#(")?;
                let elems = v.borrow();
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Continuation(_) => write!(f, "#<continuation>"),
            Val::Values(vals) => {
                if vals.len() == 1 {
                    write!(f, "{}", vals[0])
                } else {
                    write!(f, "#<values>")
                }
            }
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
    Int(i64),
    Bool(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Expr>),
    Literal(Val),
}

// ── Tokenizer ──

#[derive(Debug, Clone)]
struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        match chars[i] {
            '\n' => { line += 1; col = 1; i += 1; }
            ' ' | '\t' | '\r' => { col += 1; i += 1; }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); i += 1; col += 1; }
            '#' => {
                let start_col = col;
                if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') {
                    let tok: String = chars[i..i+2].iter().collect();
                    tokens.push(Token { text: tok, span: Span::new(line, start_col) });
                    i += 2;
                    col += 2;
                } else {
                    let mut tok = String::new();
                    while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                        tok.push(chars[i]);
                        i += 1;
                        col += 1;
                    }
                    tokens.push(Token { text: tok, span: Span::new(line, start_col) });
                }
            }
            '"' => {
                let start_col = col;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                        col += 2;
                    } else {
                        if chars[i] == '\n' { line += 1; col = 1; } else { col += 1; }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, span: Span::new(line, start_col) });
            }
            _ => {
                let start_col = col;
                let mut tok = String::new();
                while i < chars.len() && !chars[i].is_whitespace() && chars[i] != '(' && chars[i] != ')' {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, span: Span::new(line, start_col) });
            }
        }
    }
    tokens
}

// ── Parser ──

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[*pos];
    let span = tok.span;
    if tok.text == "'" {
        *pos += 1;
        let inner = parse(tokens, pos)?;
        return Ok(Expr { kind: ExprKind::List(vec![
            Expr { kind: ExprKind::Symbol("quote".into()), span },
            inner,
        ]), span });
    }
    if tok.text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1; // skip ')'
        Ok(Expr { kind: ExprKind::List(list), span })
    } else if tok.text == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if tok.text == "#t" {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Bool(true), span })
    } else if tok.text == "#f" {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Bool(false), span })
    } else if tok.text.starts_with('"') {
        *pos += 1;
        let inner = &tok.text[1..tok.text.len()-1];
        let mut s = String::new();
        let cs: Vec<char> = inner.chars().collect();
        let mut j = 0;
        while j < cs.len() {
            if cs[j] == '\\' && j + 1 < cs.len() {
                match cs[j + 1] {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    '\\' => s.push('\\'),
                    '"' => s.push('"'),
                    c => { s.push('\\'); s.push(c); }
                }
                j += 2;
            } else {
                s.push(cs[j]);
                j += 1;
            }
        }
        Ok(Expr { kind: ExprKind::Str(s), span })
    } else if tok.text.starts_with("#\\") {
        *pos += 1;
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.chars().count() == 1 => s.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("invalid character literal: {}", tok.text))),
        };
        Ok(Expr { kind: ExprKind::Char(ch), span })
    } else if let Ok(n) = tok.text.parse::<i64>() {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Int(n), span })
    } else if let Some(slash_pos) = tok.text.find('/') {
        // Try rational literal: num/den
        let num_str = &tok.text[..slash_pos];
        let den_str = &tok.text[slash_pos+1..];
        if let (Ok(n), Ok(d)) = (num_str.parse::<i64>(), den_str.parse::<i64>()) {
            if d != 0 {
                *pos += 1;
                Ok(Expr { kind: ExprKind::Literal(make_rational(n, d)), span })
            } else {
                *pos += 1;
                Ok(Expr { kind: ExprKind::Symbol(tok.text.clone()), span })
            }
        } else {
            *pos += 1;
            Ok(Expr { kind: ExprKind::Symbol(tok.text.clone()), span })
        }
    } else if let Ok(f) = tok.text.parse::<f64>() {
        if tok.text.contains('.') {
            *pos += 1;
            Ok(Expr { kind: ExprKind::Literal(Val::Float(f)), span })
        } else {
            *pos += 1;
            Ok(Expr { kind: ExprKind::Symbol(tok.text.clone()), span })
        }
    } else {
        *pos += 1;
        Ok(Expr { kind: ExprKind::Symbol(tok.text.clone()), span })
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ── Environment ──

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Val>,
    parent: Option<Env>,
}

type Env = Rc<RefCell<EnvInner>>;

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &Env, name: &str) -> Option<Val> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Val) {
    env.borrow_mut().bindings.insert(name, val);
}

/// Mutate an existing binding in the environment chain. Returns false if not found.
fn env_set_existing(env: &Env, name: &str, val: Val) -> bool {
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

/// Format a value using `display` semantics (no quotes on strings).
fn display_val(v: &Val, f: &mut String) {
    match v {
        Val::Str(s, _) => f.push_str(s),
        Val::Char(c) => f.push(*c),
        Val::Float(_) | Val::Rational(_, _) | Val::Int(_) => f.push_str(&v.to_string()),
        Val::Pair(_) => {
            f.push('(');
            let mut cur = v.clone();
            let mut first = true;
            let mut seen = HashSet::new();
            loop {
                match &cur {
                    Val::Pair(p) => {
                        let ptr = Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) {
                            f.push_str(" ...");
                            break;
                        }
                        if !first { f.push(' '); }
                        first = false;
                        let pair = p.borrow();
                        display_val(&pair.0, f);
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    Val::Nil => break,
                    other => {
                        f.push_str(" . ");
                        display_val(other, f);
                        break;
                    }
                }
            }
            f.push(')');
        }
        Val::Vector(v) => {
            f.push_str("#(");
            let elems = v.borrow();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { f.push(' '); }
                display_val(e, f);
            }
            f.push(')');
        }
        other => f.push_str(&other.to_string()),
    }
}

// ── ExprKind::Literal variant ──

// Add Literal variant to ExprKind
// (handled below in the enum)

// ── Evaluator ──

fn is_truthy(v: &Val) -> bool {
    !matches!(v, Val::Bool(false))
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

fn make_rational(n: i64, d: i64) -> Val {
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Val::Int(n) } else { Val::Rational(n, d) }
}

fn val_to_f64(v: &Val) -> Option<f64> {
    match v {
        Val::Int(n) => Some(*n as f64),
        Val::Float(f) => Some(*f),
        Val::Rational(n, d) => Some(*n as f64 / *d as f64),
        _ => None,
    }
}

fn val_is_inexact(v: &Val) -> bool {
    matches!(v, Val::Float(_))
}

fn val_is_number(v: &Val) -> bool {
    matches!(v, Val::Int(_) | Val::Float(_) | Val::Rational(_, _))
}

fn val_to_exact_pair(v: &Val) -> Option<(i64, i64)> {
    match v {
        Val::Int(n) => Some((*n, 1)),
        Val::Rational(n, d) => Some((*n, *d)),
        _ => None,
    }
}

fn as_number_f64(v: &Val, ctx: &str) -> Result<f64, EvalError> {
    val_to_f64(v).ok_or_else(|| EvalError::Type(format!("{ctx}: expected number, got {v}")))
}

fn as_int(v: &Val, ctx: &str) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{ctx}: expected integer, got {v}"))),
    }
}

fn quote_expr(expr: &Expr) -> Val {
    match &expr.kind {
        ExprKind::Int(n) => Val::Int(*n),
        ExprKind::Bool(b) => Val::Bool(*b),
        ExprKind::Char(c) => Val::Char(*c),
        ExprKind::Str(s) => Val::Str(s.clone(), false),
        ExprKind::Symbol(s) => Val::Symbol(s.clone()),
        ExprKind::Literal(v) => v.clone(),
        ExprKind::List(items) => {
            let mut result = Val::Nil;
            for item in items.iter().rev() {
                result = make_pair(quote_expr(item), result);
            }
            result
        }
    }
}

/// Create an error with position info
fn err_at(span: Span, err: EvalError) -> EvalError {
    match err {
        EvalError::Parse(msg) => EvalError::Parse(format!("{msg} at {span}")),
        EvalError::UnboundVariable(name) => EvalError::UnboundVariable(format!("{name} at {span}")),
        EvalError::Type(msg) => EvalError::Type(format!("{msg} at {span}")),
        EvalError::Arity(msg) => EvalError::Arity(format!("{msg} at {span}")),
    }
}

// ── Macro system ──

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}__hyg_{}", base, n)
}

fn is_syntax_keyword(s: &str) -> bool {
    matches!(s, "if" | "let" | "let*" | "letrec" | "letrec*" | "begin" | "set!" | "define"
        | "define-syntax" | "define-record-type" | "lambda" | "quote" | "cond" | "and" | "or" | "else"
        | "syntax-rules" | "case" | "do")
}

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

fn collect_pattern_vars(elems: &[Expr], literals: &[String]) -> HashSet<String> {
    let mut vars = HashSet::new();
    for e in elems {
        match &e.kind {
            ExprKind::Symbol(s) if s != "..." && s != "_" && !literals.contains(s) => {
                vars.insert(s.clone());
            }
            ExprKind::List(inner) => {
                vars.extend(collect_pattern_vars(inner, literals));
            }
            _ => {}
        }
    }
    vars
}

fn match_pattern_elems(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let has_ellipsis = pattern.len() >= 2
        && matches!(&pattern[pattern.len()-1].kind, ExprKind::Symbol(s) if s == "...");

    if has_ellipsis {
        let fixed_count = pattern.len() - 2;
        if input.len() < fixed_count {
            return false;
        }
        for i in 0..fixed_count {
            if !match_single_pattern(&pattern[i], &input[i], literals, bindings) {
                return false;
            }
        }
        match &pattern[pattern.len() - 2].kind {
            ExprKind::Symbol(s) if !literals.contains(s) => {
                let rest: Vec<Expr> = input[fixed_count..].to_vec();
                bindings.insert(s.clone(), MacroBinding::Many(rest));
                true
            }
            _ => false,
        }
    } else {
        if input.len() != pattern.len() {
            return false;
        }
        for (p, i) in pattern.iter().zip(input.iter()) {
            if !match_single_pattern(p, i, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn match_single_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&input.kind, ExprKind::Symbol(t) if t == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        _ => false,
    }
}

fn collect_template_symbols(expr: &Expr) -> HashSet<String> {
    let mut syms = HashSet::new();
    match &expr.kind {
        ExprKind::Symbol(s) => { syms.insert(s.clone()); }
        ExprKind::List(items) => {
            for item in items {
                syms.extend(collect_template_symbols(item));
            }
        }
        _ => {}
    }
    syms
}

fn substitute_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    free_to_literal: &HashMap<String, Val>,
    free_to_gensym: &HashMap<String, String>,
) -> Expr {
    let span = template.span;
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(e) => e.clone(),
                    MacroBinding::Many(_) => template.clone(),
                }
            } else if let Some(val) = free_to_literal.get(s) {
                Expr { kind: ExprKind::Literal(val.clone()), span }
            } else if let Some(renamed) = free_to_gensym.get(s) {
                Expr { kind: ExprKind::Symbol(renamed.clone()), span }
            } else {
                template.clone()
            }
        }
        ExprKind::List(items) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len()
                    && matches!(&items[i+1].kind, ExprKind::Symbol(s) if s == "...")
                {
                    let syms = collect_template_symbols(&items[i]);
                    let many_var = syms.iter().find(|s| {
                        matches!(bindings.get(*s), Some(MacroBinding::Many(_)))
                    }).cloned();
                    if let Some(var_name) = many_var {
                        if let Some(MacroBinding::Many(elems)) = bindings.get(&var_name) {
                            for elem in elems {
                                let mut new_bindings = bindings.clone();
                                new_bindings.insert(var_name.clone(), MacroBinding::Single(elem.clone()));
                                result.push(substitute_template(
                                    &items[i], &new_bindings, free_to_literal, free_to_gensym,
                                ));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(substitute_template(
                        &items[i], bindings, free_to_literal, free_to_gensym,
                    ));
                    i += 1;
                }
            }
            Expr { kind: ExprKind::List(result), span }
        }
        _ => template.clone(),
    }
}

fn expand_macro(
    macro_val: &Val,
    input: &[Expr],
    span: Span,
    _def_env_override: &Env,
) -> Result<Expr, EvalError> {
    let (literals, rules, def_env) = match macro_val {
        Val::Macro { literals, rules, def_env } => (literals, rules, def_env),
        _ => return Err(EvalError::Type("not a macro".into())),
    };

    let input_args = &input[1..];

    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ExprKind::List(elems) => &elems[1..],
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern_elems(pat_elems, input_args, literals, &mut bindings) {
            let pattern_vars = collect_pattern_vars(pat_elems, literals);
            let template_syms = collect_template_symbols(template);

            let mut free_to_literal = HashMap::new();
            let mut free_to_gensym = HashMap::new();

            for sym in &template_syms {
                if pattern_vars.contains(sym) || is_syntax_keyword(sym) || sym == "..." {
                    continue;
                }
                if let Some(val) = env_get(def_env, sym) {
                    if matches!(val, Val::Macro { .. }) {
                        continue;
                    }
                    free_to_literal.insert(sym.clone(), val);
                } else if is_builtin(sym) {
                    continue;
                } else {
                    free_to_gensym.insert(sym.clone(), gensym(sym));
                }
            }

            return Ok(substitute_template(template, &bindings, &free_to_literal, &free_to_gensym));
        }
    }

    Err(err_at(span, EvalError::Parse("no matching syntax-rules pattern".into())))
}

// ── call/cc support ──

/// Continuation data stored in Val::Continuation.
#[derive(Clone)]
struct ContData {
    /// Is the call/cc's lambda still executing? (escape-eligible)
    active: Rc<Cell<bool>>,
    /// Body expressions to re-evaluate (from the enclosing body sequence).
    reexec_exprs: Vec<Expr>,
    /// Environment for re-evaluation.
    reexec_env: Env,
    /// Wind stack at capture time (for dynamic-wind re-entry).
    wind_stack: Vec<WindEntry>,
}

impl fmt::Debug for ContData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ContData{{active={}}}", self.active.get())
    }
}

/// Dummy Kont type (only used for Debug derive on Val).
#[derive(Clone)]
struct Kont;

impl fmt::Debug for Kont {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Kont")
    }
}

/// A dynamic-wind frame on the wind stack.
#[derive(Clone)]
struct WindEntry {
    in_thunk: Val,
    out_thunk: Val,
}

thread_local! {
    /// Override for call/cc re-execution: when set, the next call/cc returns this value.
    static CALLCC_OVERRIDE: RefCell<Option<Val>> = RefCell::new(None);
    /// Signal for re-execution: (exprs, env) to re-evaluate.
    static REEXEC_SIGNAL: RefCell<Option<(Vec<Expr>, Env)>> = RefCell::new(None);
    /// Stack of body contexts for call/cc capture.
    static BODY_CTX: RefCell<Vec<BodyCtx>> = RefCell::new(Vec::new());
    /// Current dynamic-wind stack.
    static WIND_STACK: RefCell<Vec<WindEntry>> = RefCell::new(Vec::new());
    /// Target wind stack for continuation re-entry.
    static REENTRY_WINDS: RefCell<Option<Vec<WindEntry>>> = RefCell::new(None);
    /// Exception handler stack (for with-exception-handler / raise).
    static EXCEPTION_HANDLERS: RefCell<Vec<Val>> = RefCell::new(Vec::new());
    /// Raised value during raise signal propagation.
    static RAISED_VALUE: RefCell<Option<Val>> = RefCell::new(None);
}

#[derive(Clone)]
struct BodyCtx {
    exprs: Vec<Expr>,
    cur_idx: usize,
    env: Env,
    toplevel: bool,
}

/// Push a body context, evaluate a body sequence, pop on completion.
fn eval_body_seq(body: &[Expr], env: &Env, out: &mut String) -> Result<Val, EvalError> {
    BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
        exprs: body.to_vec(),
        cur_idx: 0,
        env: env.clone(),
        toplevel: false,
    }));
    let mut result = Val::Void;
    for (idx, expr) in body.iter().enumerate() {
        BODY_CTX.with(|ctx| {
            if let Some(top) = ctx.borrow_mut().last_mut() {
                top.cur_idx = idx;
            }
        });
        result = eval(expr, env, out)?;
    }
    BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
    Ok(result)
}

/// Handle call/cc: called when we encounter (call/cc f) or (call-with-current-continuation f).
fn handle_callcc(f: &Val, env: &Env, out: &mut String, span: Span) -> Result<Val, EvalError> {
    // Check for re-execution override
    let override_val = CALLCC_OVERRIDE.with(|o| o.borrow_mut().take());
    if let Some(val) = override_val {
        return Ok(val);
    }

    // Capture body context for re-execution
    let body_ctx = BODY_CTX.with(|ctx| ctx.borrow().last().cloned());
    let (reexec_exprs, reexec_env) = if let Some(ref bc) = body_ctx {
        (bc.exprs[bc.cur_idx..].to_vec(), bc.env.clone())
    } else {
        (vec![], env.clone())
    };

    let wind_stack = WIND_STACK.with(|w| w.borrow().clone());
    let active = Rc::new(Cell::new(true));
    let cont_data = ContData {
        active: active.clone(),
        reexec_exprs,
        reexec_env,
        wind_stack,
    };
    let cont_val = Val::Continuation(Rc::new(cont_data));

    // Call the lambda with the continuation
    let result = call_function(f, vec![cont_val], span, out);

    active.set(false);

    match result {
        Ok(val) => Ok(val),
        Err(e) => {
            // Check for escape (continuation invoked during lambda execution)
            let escape = CALLCC_OVERRIDE.with(|o| o.borrow().is_some());
            if escape {
                // The continuation was invoked as escape — the override value
                // is the value to return from this call/cc
                let val = CALLCC_OVERRIDE.with(|o| o.borrow_mut().take()).unwrap();
                Ok(val)
            } else {
                // Check for re-execution signal
                let reexec = REEXEC_SIGNAL.with(|s| s.borrow().is_some());
                if reexec {
                    // Propagate the re-execution signal upward
                    Err(e)
                } else {
                    Err(e)
                }
            }
        }
    }
}

/// Check if an error is a call/cc signal (escape or reexec).
fn is_callcc_signal(e: &EvalError) -> bool {
    match e {
        EvalError::Type(s) => s.contains("__callcc_escape__") || s.contains("__callcc_reexec__"),
        _ => false,
    }
}

/// Check if an error is a raise signal.
fn is_raise_signal(e: &EvalError) -> bool {
    match e {
        EvalError::Type(s) => s.contains("__raise_signal__"),
        _ => false,
    }
}

/// Invoke a continuation value with a given argument.
fn invoke_continuation(data: &ContData, val: Val) -> Result<Val, EvalError> {
    if data.active.get() {
        // Escape: set the value as override and return error to unwind to call/cc
        CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some(val));
        Err(EvalError::Type("__callcc_escape__".into()))
    } else {
        // Re-invocation: signal re-execution
        CALLCC_OVERRIDE.with(|o| *o.borrow_mut() = Some(val));
        REEXEC_SIGNAL.with(|s| {
            *s.borrow_mut() = Some((data.reexec_exprs.clone(), data.reexec_env.clone()));
        });
        REENTRY_WINDS.with(|w| *w.borrow_mut() = Some(data.wind_stack.clone()));
        Err(EvalError::Type("__callcc_reexec__".into()))
    }
}

/// Top-level evaluator with re-execution loop for call/cc.
fn cek_eval(exprs: &[Expr], env: &Env, out: &mut String) -> Result<Val, EvalError> {
    // Push top-level body context
    BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
        exprs: exprs.to_vec(),
        cur_idx: 0,
        env: env.clone(),
        toplevel: true,
    }));

    let result = eval_toplevel_seq(exprs, env, out);

    BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
    result
}

fn eval_toplevel_seq(exprs: &[Expr], env: &Env, out: &mut String) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    let mut idx = 0;
    while idx < exprs.len() {
        BODY_CTX.with(|ctx| {
            if let Some(top) = ctx.borrow_mut().last_mut() {
                top.cur_idx = idx;
            }
        });
        match eval(&exprs[idx], env, out) {
            Ok(val) => result = val,
            Err(e) => {
                // Check for re-execution signal
                let reexec = REEXEC_SIGNAL.with(|s| s.borrow_mut().take());
                if let Some((re_exprs, re_env)) = reexec {
                    // Wind transition for dynamic-wind re-entry
                    let target_winds = REENTRY_WINDS.with(|w| w.borrow_mut().take());
                    let dummy_span = Span::new(0, 0);
                    if let Some(ref winds) = target_winds {
                        // Unwind current wind stack
                        let current = WIND_STACK.with(|w| {
                            let mut stack = w.borrow_mut();
                            let c = stack.clone();
                            stack.clear();
                            c
                        });
                        for frame in current.iter().rev() {
                            let _ = call_function(&frame.out_thunk, vec![], dummy_span, out);
                        }
                        // Rewind target wind stack
                        for frame in winds.iter() {
                            let _ = call_function(&frame.in_thunk, vec![], dummy_span, out);
                            WIND_STACK.with(|w| w.borrow_mut().push(frame.clone()));
                        }
                    }

                    // Re-evaluate the captured expressions
                    result = eval_reexec(&re_exprs, &re_env, out)?;

                    // Unwind wind stack after re-execution
                    if target_winds.is_some() {
                        let winds = WIND_STACK.with(|w| {
                            let mut stack = w.borrow_mut();
                            let c = stack.clone();
                            stack.clear();
                            c
                        });
                        for frame in winds.iter().rev() {
                            let _ = call_function(&frame.out_thunk, vec![], dummy_span, out);
                        }
                    }

                    // Continue with remaining expressions
                    idx += 1;
                    continue;
                }
                return Err(e);
            }
        }
        idx += 1;
    }
    Ok(result)
}

fn eval_reexec(exprs: &[Expr], env: &Env, out: &mut String) -> Result<Val, EvalError> {
    // Push body context for re-execution (so nested call/cc works)
    BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
        exprs: exprs.to_vec(),
        cur_idx: 0,
        env: env.clone(),
        toplevel: true,
    }));

    let mut result = Val::Void;
    for (idx, expr) in exprs.iter().enumerate() {
        BODY_CTX.with(|ctx| {
            if let Some(top) = ctx.borrow_mut().last_mut() {
                top.cur_idx = idx;
            }
        });
        match eval(expr, env, out) {
            Ok(val) => result = val,
            Err(e) => {
                // Check for another re-execution signal (continuation invoked again)
                let reexec = REEXEC_SIGNAL.with(|s| s.borrow_mut().take());
                if let Some((re_exprs, re_env)) = reexec {
                    // Clear any wind transition info (handled by caller)
                    let _ = REENTRY_WINDS.with(|w| w.borrow_mut().take());
                    BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
                    return eval_reexec(&re_exprs, &re_env, out);
                }
                BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
                return Err(e);
            }
        }
    }
    BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
    Ok(result)
}

fn eval(expr: &Expr, env: &Env, out: &mut String) -> Result<Val, EvalError> {
    // PLACEHOLDER_MARKER_FOR_OLD_EVAL
    let mut cur = expr.clone();
    let mut cur_env = env.clone();
    loop {
        let mut bounce: Option<(Expr, Env)> = None;
        let result = eval_body(&cur, &cur_env, out, &mut bounce)?;
        if let Some((next_expr, next_env)) = bounce {
            cur = next_expr;
            cur_env = next_env;
        } else {
            return Ok(result);
        }
    }
}

fn eval_body(expr: &Expr, env: &Env, out: &mut String, tco: &mut Option<(Expr, Env)>) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone(), false)),
        ExprKind::Literal(v) => Ok(v.clone()),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .or_else(|| if is_builtin(name) { Some(Val::Builtin(name.clone())) } else { None })
                .ok_or_else(|| err_at(span, EvalError::UnboundVariable(name.clone())))
        }
        ExprKind::List(list) => {
            if list.is_empty() {
                return Err(err_at(span, EvalError::Parse("empty application".into())));
            }
            // Check for define-syntax and macro expansion
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "define-syntax" {
                    if list.len() != 3 {
                        return Err(err_at(span, EvalError::Parse("define-syntax: bad syntax".into())));
                    }
                    let name = match &list[1].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(err_at(span, EvalError::Parse("define-syntax: expected name".into()))),
                    };
                    let sr = match &list[2].kind {
                        ExprKind::List(sr_list) => sr_list,
                        _ => return Err(err_at(span, EvalError::Parse("define-syntax: expected syntax-rules".into()))),
                    };
                    if sr.is_empty() || !matches!(&sr[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
                        return Err(err_at(span, EvalError::Parse("define-syntax: expected syntax-rules".into())));
                    }
                    let literals = match &sr[1].kind {
                        ExprKind::List(lits) => {
                            let mut ls = Vec::new();
                            for l in lits {
                                match &l.kind {
                                    ExprKind::Symbol(s) => ls.push(s.clone()),
                                    _ => return Err(err_at(span, EvalError::Parse("define-syntax: bad literal".into()))),
                                }
                            }
                            ls
                        }
                        _ => return Err(err_at(span, EvalError::Parse("define-syntax: expected literals list".into()))),
                    };
                    let mut rules = Vec::new();
                    for rule in &sr[2..] {
                        match &rule.kind {
                            ExprKind::List(r) if r.len() == 2 => {
                                rules.push((r[0].clone(), r[1].clone()));
                            }
                            _ => return Err(err_at(span, EvalError::Parse("define-syntax: bad rule".into()))),
                        }
                    }
                    env_set(env, name, Val::Macro { literals, rules, def_env: env.clone() });
                    return Ok(Val::Void);
                }
                if op == "define-record-type" {
                    // (define-record-type <name> (ctor field ...) pred? (field accessor) ...)
                    if list.len() < 4 {
                        return Err(err_at(span, EvalError::Parse("define-record-type: bad syntax".into())));
                    }
                    let _type_name = match &list[1].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected type name".into()))),
                    };
                    let (ctor_name, ctor_fields) = match &list[2].kind {
                        ExprKind::List(ctor_list) if !ctor_list.is_empty() => {
                            let name = match &ctor_list[0].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected constructor name".into()))),
                            };
                            let mut fields = Vec::new();
                            for f in &ctor_list[1..] {
                                match &f.kind {
                                    ExprKind::Symbol(s) => fields.push(s.clone()),
                                    _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected field name".into()))),
                                }
                            }
                            (name, fields)
                        }
                        _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected constructor".into()))),
                    };
                    let pred_name = match &list[3].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected predicate name".into()))),
                    };
                    let mut field_accessors = Vec::new();
                    for spec in &list[4..] {
                        match &spec.kind {
                            ExprKind::List(fa) if fa.len() >= 2 => {
                                let fname = match &fa[0].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected field name".into()))),
                                };
                                let accessor = match &fa[1].kind {
                                    ExprKind::Symbol(s) => s.clone(),
                                    _ => return Err(err_at(span, EvalError::Parse("define-record-type: expected accessor name".into()))),
                                };
                                field_accessors.push((fname, accessor));
                            }
                            _ => return Err(err_at(span, EvalError::Parse("define-record-type: bad field spec".into()))),
                        }
                    }
                    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

                    // Register record type info in global registry
                    RECORD_REGISTRY.with(|reg| {
                        reg.borrow_mut().insert(type_id, RecordTypeInfo {
                            type_name: _type_name.clone(),
                            ctor_name: ctor_name.clone(),
                            field_names: ctor_fields.clone(),
                        });
                    });

                    // Define constructor, predicate, and accessors as specially-named builtins
                    env_set(env, ctor_name.clone(), Val::Builtin(format!("__ctor_{}_{}", type_id, ctor_name)));
                    env_set(env, pred_name.clone(), Val::Builtin(format!("__pred_{}_{}", type_id, pred_name)));
                    for (fname, accessor_name) in &field_accessors {
                        env_set(env, accessor_name.clone(), Val::Builtin(format!("__acc_{}_{}_{}", type_id, fname, accessor_name)));
                    }

                    return Ok(Val::Void);
                }
                // Check for macro expansion
                if let Some(mac @ Val::Macro { .. }) = env_get(env, op) {
                    let expanded = expand_macro(&mac, list, span, env)?;
                    *tco = Some((expanded, env.clone()));
                    return Ok(Val::Void);
                }
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &list[0].kind {
                match op.as_str() {
                    "define" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("define: bad syntax".into())));
                        }
                        match &list[1].kind {
                            // (define x expr)
                            ExprKind::Symbol(name) => {
                                let val = eval(&list[2], env, out)?;
                                env_set(env, name.clone(), val);
                                return Ok(Val::Void);
                            }
                            // (define (f params...) body...)
                            ExprKind::List(sig) => {
                                if sig.is_empty() {
                                    return Err(err_at(span, EvalError::Parse("define: empty signature".into())));
                                }
                                let name = match &sig[0].kind {
                                    ExprKind::Symbol(n) => n.clone(),
                                    _ => return Err(err_at(span, EvalError::Parse("define: expected symbol".into()))),
                                };
                                let (params, rest_param) = parse_params(&sig[1..], span)?;
                                let body = list[2..].to_vec();
                                let lambda = Val::Lambda {
                                    params,
                                    rest_param,
                                    body,
                                    env: env.clone(),
                                };
                                env_set(env, name, lambda);
                                return Ok(Val::Void);
                            }
                            _ => return Err(err_at(span, EvalError::Parse("define: bad syntax".into()))),
                        }
                    }
                    "if" => {
                        if list.len() < 3 || list.len() > 4 {
                            return Err(err_at(span, EvalError::Parse("if: bad syntax".into())));
                        }
                        let cond = eval(&list[1], env, out)?;
                        if is_truthy(&cond) {
                            *tco = Some((list[2].clone(), env.clone()));
                            return Ok(Val::Void);
                        } else if list.len() == 4 {
                            *tco = Some((list[3].clone(), env.clone()));
                            return Ok(Val::Void);
                        } else {
                            return Ok(Val::Void);
                        }
                    }
                    "quote" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Parse("quote: need exactly 1 argument".into())));
                        }
                        return Ok(quote_expr(&list[1]));
                    }
                    "lambda" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("lambda: bad syntax".into())));
                        }
                        let (params, rest_param) = match &list[1].kind {
                            ExprKind::List(param_list) => parse_params(param_list, span)?,
                            ExprKind::Symbol(s) => {
                                // (lambda rest body) - single rest param
                                (vec![], Some(s.clone()))
                            }
                            _ => return Err(err_at(span, EvalError::Parse("lambda: expected parameter list".into()))),
                        };
                        let body = list[2..].to_vec();
                        return Ok(Val::Lambda {
                            params,
                            rest_param,
                            body,
                            env: env.clone(),
                        });
                    }
                    "case-lambda" => {
                        if list.len() < 2 {
                            return Err(err_at(span, EvalError::Parse("case-lambda: need at least one clause".into())));
                        }
                        let mut clauses = Vec::new();
                        for clause_expr in &list[1..] {
                            match &clause_expr.kind {
                                ExprKind::List(clause) if clause.len() >= 2 => {
                                    let (params, rest_param) = match &clause[0].kind {
                                        ExprKind::List(param_list) => parse_params(param_list, span)?,
                                        ExprKind::Symbol(s) => (vec![], Some(s.clone())),
                                        _ => return Err(err_at(span, EvalError::Parse("case-lambda: expected parameter list".into()))),
                                    };
                                    let body = clause[1..].to_vec();
                                    clauses.push((params, rest_param, body));
                                }
                                _ => return Err(err_at(span, EvalError::Parse("case-lambda: bad clause".into()))),
                            }
                        }
                        return Ok(Val::CaseLambda {
                            clauses,
                            env: env.clone(),
                        });
                    }
                    "let" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("let: bad syntax".into())));
                        }
                        // Named let: (let name ((var init) ...) body ...)
                        if let ExprKind::Symbol(name) = &list[1].kind {
                            if list.len() < 4 {
                                return Err(err_at(span, EvalError::Parse("let: bad syntax".into())));
                            }
                            let bindings = match &list[2].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(err_at(span, EvalError::Parse("let: expected bindings list".into()))),
                            };
                            let mut params = Vec::new();
                            let mut init_vals = Vec::new();
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let pname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(err_at(span, EvalError::Parse("let: expected symbol".into()))),
                                        };
                                        let val = eval(&pair[1], env, out)?;
                                        params.push(pname);
                                        init_vals.push(val);
                                    }
                                    _ => return Err(err_at(span, EvalError::Parse("let: bad binding".into()))),
                                }
                            }
                            let let_env = new_env(Some(env.clone()));
                            let body = list[3..].to_vec();
                            let lambda = Val::Lambda {
                                params: params.clone(),
                                rest_param: None,
                                body,
                                env: let_env.clone(),
                            };
                            env_set(&let_env, name.clone(), lambda);
                            let call_env = new_env(Some(let_env.clone()));
                            for (p, v) in params.iter().zip(init_vals) {
                                env_set(&call_env, p.clone(), v);
                            }
                            let body_exprs = &list[3..];
                            if body_exprs.is_empty() { return Ok(Val::Void); }
                            for i in 0..body_exprs.len() - 1 {
                                eval(&body_exprs[i], &call_env, out)?;
                            }
                            *tco = Some((body_exprs.last().unwrap().clone(), call_env));
                            return Ok(Val::Void);
                        }
                        let bindings = match &list[1].kind {
                            ExprKind::List(b) => b,
                            _ => return Err(err_at(span, EvalError::Parse("let: expected bindings list".into()))),
                        };
                        let let_env = new_env(Some(env.clone()));
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("let: expected symbol".into()))),
                                    };
                                    let val = eval(&pair[1], env, out)?;
                                    env_set(&let_env, name, val);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("let: bad binding".into()))),
                            }
                        }
                        let body_exprs = &list[2..];
                        if body_exprs.is_empty() { return Ok(Val::Void); }
                        BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
                            exprs: body_exprs.to_vec(),
                            cur_idx: 0,
                            env: let_env.clone(),
                            toplevel: false,
                        }));
                        for i in 0..body_exprs.len() - 1 {
                            BODY_CTX.with(|ctx| {
                                if let Some(top) = ctx.borrow_mut().last_mut() {
                                    top.cur_idx = i;
                                }
                            });
                            eval(&body_exprs[i], &let_env, out)?;
                        }
                        BODY_CTX.with(|ctx| {
                            if let Some(top) = ctx.borrow_mut().last_mut() {
                                top.cur_idx = body_exprs.len() - 1;
                            }
                        });
                        BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
                        *tco = Some((body_exprs.last().unwrap().clone(), let_env));
                        return Ok(Val::Void);
                    }
                    "set!" => {
                        if list.len() != 3 {
                            return Err(err_at(span, EvalError::Parse("set!: bad syntax".into())));
                        }
                        let name = match &list[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(err_at(span, EvalError::Parse("set!: expected symbol".into()))),
                        };
                        let val = eval(&list[2], env, out)?;
                        if !env_set_existing(env, &name, val) {
                            return Err(err_at(span, EvalError::UnboundVariable(name)));
                        }
                        return Ok(Val::Void);
                    }
                    "begin" => {
                        if list.len() <= 1 { return Ok(Val::Void); }
                        for i in 1..list.len() - 1 {
                            eval(&list[i], env, out)?;
                        }
                        *tco = Some((list.last().unwrap().clone(), env.clone()));
                        return Ok(Val::Void);
                    }
                    "cond" => {
                        for clause in &list[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            if parts.len() <= 1 { return Ok(Val::Void); }
                                            for i in 1..parts.len() - 1 {
                                                eval(&parts[i], env, out)?;
                                            }
                                            *tco = Some((parts.last().unwrap().clone(), env.clone()));
                                            return Ok(Val::Void);
                                        }
                                    }
                                    let cond_val = eval(&parts[0], env, out)?;
                                    if is_truthy(&cond_val) {
                                        if parts.len() <= 1 { return Ok(cond_val); }
                                        for i in 1..parts.len() - 1 {
                                            eval(&parts[i], env, out)?;
                                        }
                                        *tco = Some((parts.last().unwrap().clone(), env.clone()));
                                        return Ok(Val::Void);
                                    }
                                }
                                _ => return Err(err_at(span, EvalError::Parse("cond: bad clause".into()))),
                            }
                        }
                        return Ok(Val::Void);
                    }
                    "and" => {
                        if list.len() <= 1 { return Ok(Val::Bool(true)); }
                        for i in 1..list.len() - 1 {
                            let result = eval(&list[i], env, out)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        *tco = Some((list.last().unwrap().clone(), env.clone()));
                        return Ok(Val::Void);
                    }
                    "or" => {
                        if list.len() <= 1 { return Ok(Val::Bool(false)); }
                        for i in 1..list.len() - 1 {
                            let result = eval(&list[i], env, out)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        *tco = Some((list.last().unwrap().clone(), env.clone()));
                        return Ok(Val::Void);
                    }
                    "let*" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("let*: bad syntax".into())));
                        }
                        let bindings = match &list[1].kind {
                            ExprKind::List(b) => b,
                            _ => return Err(err_at(span, EvalError::Parse("let*: expected bindings list".into()))),
                        };
                        let let_env = new_env(Some(env.clone()));
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("let*: expected symbol".into()))),
                                    };
                                    let val = eval(&pair[1], &let_env, out)?;
                                    env_set(&let_env, name, val);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("let*: bad binding".into()))),
                            }
                        }
                        let body_exprs = &list[2..];
                        if body_exprs.is_empty() { return Ok(Val::Void); }
                        for i in 0..body_exprs.len() - 1 {
                            eval(&body_exprs[i], &let_env, out)?;
                        }
                        *tco = Some((body_exprs.last().unwrap().clone(), let_env));
                        return Ok(Val::Void);
                    }
                    "letrec" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("letrec: bad syntax".into())));
                        }
                        let bindings = match &list[1].kind {
                            ExprKind::List(b) => b,
                            _ => return Err(err_at(span, EvalError::Parse("letrec: expected bindings list".into()))),
                        };
                        let letrec_env = new_env(Some(env.clone()));
                        // First, bind all names to Void
                        let mut names = Vec::new();
                        let mut init_exprs = Vec::new();
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("letrec: expected symbol".into()))),
                                    };
                                    env_set(&letrec_env, name.clone(), Val::Void);
                                    names.push(name);
                                    init_exprs.push(&pair[1]);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("letrec: bad binding".into()))),
                            }
                        }
                        // Evaluate inits in the letrec env
                        let vals: Vec<Val> = init_exprs.iter()
                            .map(|e| eval(e, &letrec_env, out))
                            .collect::<Result<_, _>>()?;
                        for (name, val) in names.into_iter().zip(vals) {
                            env_set(&letrec_env, name, val);
                        }
                        let body_exprs = &list[2..];
                        if body_exprs.is_empty() { return Ok(Val::Void); }
                        for i in 0..body_exprs.len() - 1 {
                            eval(&body_exprs[i], &letrec_env, out)?;
                        }
                        *tco = Some((body_exprs.last().unwrap().clone(), letrec_env));
                        return Ok(Val::Void);
                    }
                    "letrec*" => {
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("letrec*: bad syntax".into())));
                        }
                        let bindings = match &list[1].kind {
                            ExprKind::List(b) => b,
                            _ => return Err(err_at(span, EvalError::Parse("letrec*: expected bindings list".into()))),
                        };
                        let letrec_env = new_env(Some(env.clone()));
                        for binding in bindings {
                            match &binding.kind {
                                ExprKind::List(pair) if pair.len() == 2 => {
                                    let name = match &pair[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("letrec*: expected symbol".into()))),
                                    };
                                    let val = eval(&pair[1], &letrec_env, out)?;
                                    env_set(&letrec_env, name, val);
                                }
                                _ => return Err(err_at(span, EvalError::Parse("letrec*: bad binding".into()))),
                            }
                        }
                        let body_exprs = &list[2..];
                        if body_exprs.is_empty() { return Ok(Val::Void); }
                        for i in 0..body_exprs.len() - 1 {
                            eval(&body_exprs[i], &letrec_env, out)?;
                        }
                        *tco = Some((body_exprs.last().unwrap().clone(), letrec_env));
                        return Ok(Val::Void);
                    }
                    "case" => {
                        if list.len() < 2 {
                            return Err(err_at(span, EvalError::Parse("case: bad syntax".into())));
                        }
                        let key = eval(&list[1], env, out)?;
                        for clause in &list[2..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    // else clause
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            if parts.len() <= 1 { return Ok(Val::Void); }
                                            for i in 1..parts.len() - 1 {
                                                eval(&parts[i], env, out)?;
                                            }
                                            *tco = Some((parts.last().unwrap().clone(), env.clone()));
                                            return Ok(Val::Void);
                                        }
                                    }
                                    // datum clause: ((datum ...) expr ...)
                                    let datums = match &parts[0].kind {
                                        ExprKind::List(d) => d,
                                        _ => return Err(err_at(span, EvalError::Parse("case: expected datum list".into()))),
                                    };
                                    let matched = datums.iter().any(|d| {
                                        let datum_val = quote_expr(d);
                                        val_eqv(&key, &datum_val)
                                    });
                                    if matched {
                                        if parts.len() <= 1 { return Ok(Val::Void); }
                                        for i in 1..parts.len() - 1 {
                                            eval(&parts[i], env, out)?;
                                        }
                                        *tco = Some((parts.last().unwrap().clone(), env.clone()));
                                        return Ok(Val::Void);
                                    }
                                }
                                _ => return Err(err_at(span, EvalError::Parse("case: bad clause".into()))),
                            }
                        }
                        return Ok(Val::Void);
                    }
                    "do" => {
                        // (do ((var init step) ...) (test expr ...) body ...)
                        if list.len() < 3 {
                            return Err(err_at(span, EvalError::Parse("do: bad syntax".into())));
                        }
                        let var_specs = match &list[1].kind {
                            ExprKind::List(specs) => specs,
                            _ => return Err(err_at(span, EvalError::Parse("do: expected variable specs".into()))),
                        };
                        let test_clause = match &list[2].kind {
                            ExprKind::List(tc) if !tc.is_empty() => tc,
                            _ => return Err(err_at(span, EvalError::Parse("do: expected test clause".into()))),
                        };
                        // Parse variable specs
                        let mut var_names = Vec::new();
                        let mut step_exprs: Vec<Option<&Expr>> = Vec::new();
                        let do_env = new_env(Some(env.clone()));
                        for spec in var_specs {
                            match &spec.kind {
                                ExprKind::List(parts) if parts.len() >= 2 => {
                                    let name = match &parts[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(err_at(span, EvalError::Parse("do: expected variable name".into()))),
                                    };
                                    let init = eval(&parts[1], env, out)?;
                                    env_set(&do_env, name.clone(), init);
                                    var_names.push(name);
                                    step_exprs.push(if parts.len() >= 3 { Some(&parts[2]) } else { None });
                                }
                                _ => return Err(err_at(span, EvalError::Parse("do: bad variable spec".into()))),
                            }
                        }
                        let body = &list[3..];
                        loop {
                            // Evaluate test
                            let test_val = eval(&test_clause[0], &do_env, out)?;
                            if is_truthy(&test_val) {
                                // Evaluate result expressions with TCO on last
                                if test_clause.len() <= 1 { return Ok(Val::Void); }
                                for i in 1..test_clause.len() - 1 {
                                    eval(&test_clause[i], &do_env, out)?;
                                }
                                *tco = Some((test_clause.last().unwrap().clone(), do_env.clone()));
                                return Ok(Val::Void);
                            }
                            // Evaluate body
                            for expr in body {
                                eval(expr, &do_env, out)?;
                            }
                            // Parallel step: evaluate all step exprs with current values
                            let new_vals: Vec<Option<Val>> = step_exprs.iter()
                                .map(|se| {
                                    se.map(|e| eval(e, &do_env, out)).transpose()
                                })
                                .collect::<Result<_, _>>()?;
                            // Update all variables
                            for (name, new_val) in var_names.iter().zip(new_vals) {
                                if let Some(v) = new_val {
                                    env_set(&do_env, name.clone(), v);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Output builtins need access to `out`
            if let ExprKind::Symbol(op) = &list[0].kind {
                match op.as_str() {
                    "display" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Arity("display: need 1 argument".into())));
                        }
                        let val = eval(&list[1], env, out)?;
                        display_val(&val, out);
                        return Ok(Val::Void);
                    }
                    "write" => {
                        if list.len() != 2 {
                            return Err(err_at(span, EvalError::Arity("write: need 1 argument".into())));
                        }
                        let val = eval(&list[1], env, out)?;
                        out.push_str(&val.to_string());
                        return Ok(Val::Void);
                    }
                    "newline" => {
                        if list.len() != 1 {
                            return Err(err_at(span, EvalError::Arity("newline: need 0 arguments".into())));
                        }
                        out.push('\n');
                        return Ok(Val::Void);
                    }
                    "string-set!" => {
                        if list.len() != 4 {
                            return Err(err_at(span, EvalError::Arity("string-set!: need 3 arguments".into())));
                        }
                        let var_name = match &list[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(err_at(span, EvalError::Type("string-set!: first argument must be a variable".into()))),
                        };
                        let idx_val = eval(&list[2], env, out)?;
                        let idx = match &idx_val {
                            Val::Int(n) => *n as usize,
                            _ => return Err(err_at(span, EvalError::Type("string-set!: expected integer index".into()))),
                        };
                        let char_val = eval(&list[3], env, out)?;
                        let ch = match &char_val {
                            Val::Char(c) => *c,
                            _ => return Err(err_at(span, EvalError::Type("string-set!: expected char".into()))),
                        };
                        let (mut s, mutable) = match env_get(env, &var_name) {
                            Some(Val::Str(s, m)) => (s, m),
                            Some(_) => return Err(err_at(span, EvalError::Type("string-set!: expected string".into()))),
                            None => return Err(err_at(span, EvalError::UnboundVariable(var_name.clone()))),
                        };
                        if !mutable {
                            return Err(err_at(span, EvalError::Type("string-set!: strings are immutable".into())));
                        }
                        let mut chars: Vec<char> = s.chars().collect();
                        if idx >= chars.len() {
                            return Err(err_at(span, EvalError::Type("string-set!: index out of range".into())));
                        }
                        chars[idx] = ch;
                        s = chars.into_iter().collect();
                        env_set(env, var_name, Val::Str(s, true));
                        return Ok(Val::Void);
                    }
                    _ => {}
                }
            }

            // raise handling (only if not locally rebound)
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "raise" && env_get(env, "raise").is_none() {
                    if list.len() != 2 {
                        return Err(err_at(span, EvalError::Arity("raise: need 1 argument".into())));
                    }
                    let val = eval(&list[1], env, out)?;
                    // Check for with-exception-handler handlers
                    let handler = EXCEPTION_HANDLERS.with(|h| h.borrow_mut().pop());
                    if let Some(handler_fn) = handler {
                        let result = call_function(&handler_fn, vec![val], span, out);
                        match result {
                            Ok(_) => {
                                return Err(err_at(span, EvalError::Type(
                                    "raise: exception handler returned".into()
                                )));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    // No handler — use signal mechanism (for guard to catch)
                    RAISED_VALUE.with(|r| *r.borrow_mut() = Some(val));
                    return Err(err_at(span, EvalError::Type("__raise_signal__".into())));
                }
            }

            // guard handling
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "guard" {
                    if list.len() < 3 {
                        return Err(err_at(span, EvalError::Arity("guard: need clauses and body".into())));
                    }
                    let clauses_list = match &list[1].kind {
                        ExprKind::List(l) => l.clone(),
                        _ => return Err(err_at(span, EvalError::Parse("guard: bad clause form".into()))),
                    };
                    if clauses_list.is_empty() {
                        return Err(err_at(span, EvalError::Parse("guard: need variable name".into())));
                    }
                    let var_name = match &clauses_list[0].kind {
                        ExprKind::Symbol(s) => s.clone(),
                        _ => return Err(err_at(span, EvalError::Parse("guard: variable must be symbol".into()))),
                    };
                    let clauses = clauses_list[1..].to_vec();
                    let body = &list[2..];

                    // Evaluate body, catching raise signals
                    let body_result = eval_body_seq(body, env, out);
                    match body_result {
                        Ok(val) => return Ok(val),
                        Err(ref e) if is_raise_signal(e) => {
                            let raised = RAISED_VALUE.with(|r| r.borrow_mut().take())
                                .unwrap_or(Val::Void);
                            let guard_env = new_env(Some(env.clone()));
                            env_set(&guard_env, var_name.clone(), raised.clone());

                            for clause in &clauses {
                                match &clause.kind {
                                    ExprKind::List(cl) if !cl.is_empty() => {
                                        if let ExprKind::Symbol(s) = &cl[0].kind {
                                            if s == "else" {
                                                let mut result = Val::Void;
                                                for expr in &cl[1..] {
                                                    result = eval(expr, &guard_env, out)?;
                                                }
                                                return Ok(result);
                                            }
                                        }
                                        let test = eval(&cl[0], &guard_env, out)?;
                                        if is_truthy(&test) {
                                            if cl.len() == 1 {
                                                return Ok(test);
                                            }
                                            let mut result = Val::Void;
                                            for expr in &cl[1..] {
                                                result = eval(expr, &guard_env, out)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            // No clause matched — re-raise
                            RAISED_VALUE.with(|r| *r.borrow_mut() = Some(raised));
                            return Err(err_at(span, EvalError::Type("__raise_signal__".into())));
                        }
                        Err(e) => return Err(e),
                    }
                }
            }

            // with-exception-handler handling (only if not locally rebound)
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "with-exception-handler" && env_get(env, "with-exception-handler").is_none() {
                    if list.len() != 3 {
                        return Err(err_at(span, EvalError::Arity(
                            "with-exception-handler: need 2 arguments".into()
                        )));
                    }
                    let handler = eval(&list[1], env, out)?;
                    let thunk = eval(&list[2], env, out)?;
                    let depth_before = EXCEPTION_HANDLERS.with(|h| h.borrow().len());
                    EXCEPTION_HANDLERS.with(|h| h.borrow_mut().push(handler));
                    let result = call_function(&thunk, vec![], span, out);
                    // Pop our handler if raise didn't already consume it
                    EXCEPTION_HANDLERS.with(|h| {
                        let mut handlers = h.borrow_mut();
                        if handlers.len() > depth_before {
                            handlers.pop();
                        }
                    });
                    return result;
                }
            }

            // values handling
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "values" {
                    let mut vals = Vec::new();
                    for a in &list[1..] {
                        vals.push(eval(a, env, out)?);
                    }
                    if vals.len() == 1 {
                        return Ok(vals.into_iter().next().unwrap());
                    }
                    return Ok(Val::Values(vals));
                }
                if op == "call-with-values" {
                    if list.len() != 3 {
                        return Err(err_at(span, EvalError::Arity("call-with-values: need 2 arguments".into())));
                    }
                    let producer = eval(&list[1], env, out)?;
                    let consumer = eval(&list[2], env, out)?;
                    let produced = call_function(&producer, vec![], span, out)?;
                    let args = match produced {
                        Val::Values(vals) => vals,
                        other => vec![other],
                    };
                    return call_function(&consumer, args, span, out);
                }
            }

            // dynamic-wind handling
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "dynamic-wind" {
                    if list.len() != 4 {
                        return Err(err_at(span, EvalError::Arity("dynamic-wind: need 3 arguments".into())));
                    }
                    let in_thunk = eval(&list[1], env, out)?;
                    let body_thunk = eval(&list[2], env, out)?;
                    let out_thunk = eval(&list[3], env, out)?;
                    // Call in-thunk
                    call_function(&in_thunk, vec![], span, out)?;
                    // Push wind entry
                    WIND_STACK.with(|w| w.borrow_mut().push(WindEntry {
                        in_thunk: in_thunk.clone(),
                        out_thunk: out_thunk.clone(),
                    }));
                    // Call body-thunk, catching callcc signals to run out-thunk
                    let body_result = call_function(&body_thunk, vec![], span, out);
                    // Pop wind entry
                    WIND_STACK.with(|w| w.borrow_mut().pop());
                    match body_result {
                        Ok(val) => {
                            // Normal exit: call out-thunk, return body value
                            call_function(&out_thunk, vec![], span, out)?;
                            return Ok(val);
                        }
                        Err(ref e) if is_callcc_signal(e) => {
                            // Non-local exit: call out-thunk, then re-throw
                            let _ = call_function(&out_thunk, vec![], span, out);
                            return body_result;
                        }
                        Err(_) => {
                            // Other error: call out-thunk, then re-throw
                            let _ = call_function(&out_thunk, vec![], span, out);
                            return body_result;
                        }
                    }
                }
            }

            // call/cc handling
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "call/cc" || op == "call-with-current-continuation" {
                    if list.len() != 2 {
                        return Err(err_at(span, EvalError::Arity("call/cc: need 1 argument".into())));
                    }
                    let f = eval(&list[1], env, out)?;
                    return handle_callcc(&f, env, out, span);
                }
            }

            // Function application: handle map/apply specially (needs out), then builtins, then lambdas
            if let ExprKind::Symbol(op) = &list[0].kind {
                if op == "map" {
                    if list.len() < 3 {
                        return Err(err_at(span, EvalError::Arity("map: need at least 2 arguments".into())));
                    }
                    let func = eval(&list[1], env, out)?;
                    let mut lists: Vec<Vec<Val>> = Vec::new();
                    for a in &list[2..] {
                        let v = eval(a, env, out)?;
                        lists.push(val_list_to_vec(&v)?);
                    }
                    let len = lists[0].len();
                    for l in &lists {
                        if l.len() != len {
                            return Err(err_at(span, EvalError::Type("map: lists must have same length".into())));
                        }
                    }
                    let mut result = Val::Nil;
                    let mut results = Vec::new();
                    for i in 0..len {
                        let call_args: Vec<Val> = lists.iter().map(|l| l[i].clone()).collect();
                        results.push(call_function(&func, call_args, span, out)?);
                    }
                    for r in results.into_iter().rev() {
                        result = make_pair(r, result);
                    }
                    return Ok(result);
                }
                if op == "for-each" {
                    if list.len() < 3 {
                        return Err(err_at(span, EvalError::Arity("for-each: need at least 2 arguments".into())));
                    }
                    let func = eval(&list[1], env, out)?;
                    let mut lists: Vec<Vec<Val>> = Vec::new();
                    for a in &list[2..] {
                        let v = eval(a, env, out)?;
                        lists.push(val_list_to_vec(&v)?);
                    }
                    let len = lists[0].len();
                    for i in 0..len {
                        let call_args: Vec<Val> = lists.iter().map(|l| l[i].clone()).collect();
                        call_function(&func, call_args, span, out)?;
                    }
                    return Ok(Val::Void);
                }
                if op == "apply" {
                    let mut args = Vec::new();
                    for a in &list[1..] {
                        args.push(eval(a, env, out)?);
                    }
                    return do_apply(&args, span, out);
                }
                if is_builtin(op) {
                    let mut args = Vec::new();
                    for a in &list[1..] {
                        args.push(eval(a, env, out)?);
                    }
                    return apply_builtin(op, &args).map_err(|e| err_at(span, e));
                }
            }

            let func = eval(&list[0], env, out)?;

            // Handle call/cc applied as first-class value
            if let Val::Builtin(ref name) = func {
                if name == "call/cc" || name == "call-with-current-continuation" {
                    if list.len() != 2 {
                        return Err(err_at(span, EvalError::Arity("call/cc: need 1 argument".into())));
                    }
                    let f = eval(&list[1], env, out)?;
                    return handle_callcc(&f, env, out, span);
                }
                if name == "values" {
                    let mut vals = Vec::new();
                    for a in &list[1..] {
                        vals.push(eval(a, env, out)?);
                    }
                    if vals.len() == 1 {
                        return Ok(vals.into_iter().next().unwrap());
                    }
                    return Ok(Val::Values(vals));
                }
                if name == "call-with-values" {
                    if list.len() != 3 {
                        return Err(err_at(span, EvalError::Arity("call-with-values: need 2 arguments".into())));
                    }
                    let producer = eval(&list[1], env, out)?;
                    let consumer = eval(&list[2], env, out)?;
                    let produced = call_function(&producer, vec![], span, out)?;
                    let args = match produced {
                        Val::Values(vals) => vals,
                        other => vec![other],
                    };
                    return call_function(&consumer, args, span, out);
                }
            }

            let mut args = Vec::new();
            for a in &list[1..] {
                args.push(eval(a, env, out)?);
            }

            // TCO for Lambda/CaseLambda, delegate others to call_function
            match func {
                Val::Lambda { params, rest_param, mut body, env: closure_env } => {
                    if let Some(ref _rp) = rest_param {
                        if args.len() < params.len() {
                            return Err(err_at(span, EvalError::Arity(format!(
                                "expected at least {} arguments, got {}", params.len(), args.len()
                            ))));
                        }
                    } else if args.len() != params.len() {
                        return Err(err_at(span, EvalError::Arity(format!(
                            "expected {} arguments, got {}", params.len(), args.len()
                        ))));
                    }
                    let call_env = new_env(Some(closure_env));
                    for (p, a) in params.iter().zip(args.iter()) {
                        env_set(&call_env, p.clone(), a.clone());
                    }
                    if let Some(ref rp) = rest_param {
                        let rest_args = &args[params.len()..];
                        let mut rest_list = Val::Nil;
                        for a in rest_args.iter().rev() {
                            rest_list = make_pair(a.clone(), rest_list);
                        }
                        env_set(&call_env, rp.clone(), rest_list);
                    }
                    if body.is_empty() { return Ok(Val::Void); }
                    let full_body = body.clone();
                    let last = body.pop().unwrap();
                    BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
                        exprs: full_body,
                        cur_idx: 0,
                        env: call_env.clone(),
                        toplevel: false,
                    }));
                    for (idx, e) in body.iter().enumerate() {
                        BODY_CTX.with(|ctx| {
                            if let Some(top) = ctx.borrow_mut().last_mut() {
                                top.cur_idx = idx;
                            }
                        });
                        eval(e, &call_env, out)?;
                    }
                    BODY_CTX.with(|ctx| {
                        if let Some(top) = ctx.borrow_mut().last_mut() {
                            top.cur_idx = body.len();
                        }
                    });
                    BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
                    *tco = Some((last, call_env));
                    Ok(Val::Void)
                }
                Val::CaseLambda { clauses, env: closure_env } => {
                    for (params, rest_param, mut body_cl) in clauses {
                        let matches = if rest_param.is_some() {
                            args.len() >= params.len()
                        } else {
                            args.len() == params.len()
                        };
                        if matches {
                            let call_env = new_env(Some(closure_env.clone()));
                            for (p, a) in params.iter().zip(args.iter()) {
                                env_set(&call_env, p.clone(), a.clone());
                            }
                            if let Some(ref rp) = rest_param {
                                let rest_args = &args[params.len()..];
                                let mut rest_list = Val::Nil;
                                for a in rest_args.iter().rev() {
                                    rest_list = make_pair(a.clone(), rest_list);
                                }
                                env_set(&call_env, rp.clone(), rest_list);
                            }
                            if body_cl.is_empty() { return Ok(Val::Void); }
                            let full_body_cl = body_cl.clone();
                            let last = body_cl.pop().unwrap();
                            BODY_CTX.with(|ctx| ctx.borrow_mut().push(BodyCtx {
                                exprs: full_body_cl,
                                cur_idx: 0,
                                env: call_env.clone(),
                                toplevel: false,
                            }));
                            for (idx, e) in body_cl.iter().enumerate() {
                                BODY_CTX.with(|ctx| {
                                    if let Some(top) = ctx.borrow_mut().last_mut() {
                                        top.cur_idx = idx;
                                    }
                                });
                                eval(e, &call_env, out)?;
                            }
                            BODY_CTX.with(|ctx| ctx.borrow_mut().pop());
                            *tco = Some((last, call_env));
                            return Ok(Val::Void);
                        }
                    }
                    Err(err_at(span, EvalError::Arity(format!(
                        "case-lambda: no matching clause for {} arguments", args.len()
                    ))))
                }
                other => call_function(&other, args, span, out),
            }
        }
    }
}

fn is_builtin(op: &str) -> bool {
    matches!(op, "+" | "-" | "*" | "/" | "=" | "<" | ">" | "<=" | ">=" | "not"
        | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
        | "integer?" | "rational?" | "exact?" | "inexact?"
        | "exact->inexact" | "inexact->exact" | "numerator" | "denominator"
        | "string-append" | "string-length" | "substring" | "string->number"
        | "number->string" | "symbol->string" | "string->symbol" | "string-ref"
        | "string-copy" | "string->list" | "list->string" | "char->integer" | "integer->char" | "apply"
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "list-ref" | "list-tail" | "list?" | "assoc" | "equal?" | "eq?" | "eqv?"
        | "char-alphabetic?" | "char-numeric?" | "char-upcase" | "char-downcase"
        | "char=?" | "char<?"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase"
        | "map" | "procedure?"
        | "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length"
        | "vector?" | "vector->list" | "list->vector"
        | "set-car!" | "set-cdr!" | "memq" | "assq"
        | "caar" | "cadr" | "cdar" | "cddr" | "caddr" | "cdddr" | "caaar" | "cdaar"
        | "for-each" | "reverse" | "member" | "assv" | "memv"
        | "cadar" | "caddar" | "cadddr" | "cdadr" | "cddar" | "cddddr"
        | "error" | "gcd" | "lcm" | "truncate" | "round"
        | "make-string" | "string" | "string>?" | "string<=?" | "string>=?"
        | "display" | "write" | "newline"
        | "call/cc" | "call-with-current-continuation"
        | "dynamic-wind"
        | "values" | "call-with-values")
}

/// Parse a parameter list that may contain dot notation for rest params.
/// Returns (fixed_params, rest_param).
fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 != exprs.len() - 1 {
                    return Err(err_at(span, EvalError::Parse("bad dot in parameter list".into())));
                }
                match &exprs[i + 1].kind {
                    ExprKind::Symbol(r) => rest = Some(r.clone()),
                    _ => return Err(err_at(span, EvalError::Parse("expected symbol after dot".into()))),
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(err_at(span, EvalError::Parse("expected parameter name".into()))),
        }
        i += 1;
    }
    Ok((params, rest))
}

/// Convert a Val list to a Vec<Val>.
fn val_list_to_vec(v: &Val) -> Result<Vec<Val>, EvalError> {
    let mut result = Vec::new();
    let mut cur = v.clone();
    loop {
        match &cur {
            Val::Nil => break,
            Val::Pair(p) => {
                let pair = p.borrow();
                result.push(pair.0.clone());
                let next = pair.1.clone();
                drop(pair);
                cur = next;
            }
            _ => return Err(EvalError::Type("apply: last argument must be a list".into())),
        }
    }
    Ok(result)
}

/// Call a function value with given args.
fn call_function(func: &Val, args: Vec<Val>, span: Span, out: &mut String) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env: closure_env } => {
            if let Some(ref rp) = rest_param {
                if args.len() < params.len() {
                    return Err(err_at(span, EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    ))));
                }
            } else if args.len() != params.len() {
                return Err(err_at(span, EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                ))));
            }
            let call_env = new_env(Some(closure_env.clone()));
            for (p, a) in params.iter().zip(args.iter()) {
                env_set(&call_env, p.clone(), a.clone());
            }
            if let Some(ref rp) = rest_param {
                let rest_args = &args[params.len()..];
                let mut rest_list = Val::Nil;
                for a in rest_args.iter().rev() {
                    rest_list = make_pair(a.clone(), rest_list);
                }
                env_set(&call_env, rp.clone(), rest_list);
            }
            eval_body_seq(body, &call_env, out)
        }
        Val::CaseLambda { clauses, env: closure_env } => {
            // Find matching clause by arity
            for (params, rest_param, body) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let call_env = new_env(Some(closure_env.clone()));
                    for (p, a) in params.iter().zip(args.iter()) {
                        env_set(&call_env, p.clone(), a.clone());
                    }
                    if let Some(ref rp) = rest_param {
                        let rest_args = &args[params.len()..];
                        let mut rest_list = Val::Nil;
                        for a in rest_args.iter().rev() {
                            rest_list = make_pair(a.clone(), rest_list);
                        }
                        env_set(&call_env, rp.clone(), rest_list);
                    }
                    return eval_body_seq(body, &call_env, out);
                }
            }
            Err(err_at(span, EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            ))))
        }
        Val::Builtin(name) => {
            if name == "call/cc" || name == "call-with-current-continuation" {
                if args.len() != 1 {
                    return Err(err_at(span, EvalError::Arity("call/cc: need 1 argument".into())));
                }
                return handle_callcc(&args[0], &new_env(None), out, span);
            } else if name == "values" {
                if args.len() == 1 {
                    return Ok(args.into_iter().next().unwrap());
                }
                return Ok(Val::Values(args));
            } else if name == "call-with-values" {
                if args.len() != 2 {
                    return Err(err_at(span, EvalError::Arity("call-with-values: need 2 arguments".into())));
                }
                let mut args_iter = args.into_iter();
                let producer = args_iter.next().unwrap();
                let consumer = args_iter.next().unwrap();
                let produced = call_function(&producer, vec![], span, out)?;
                let cargs = match produced {
                    Val::Values(vals) => vals,
                    other => vec![other],
                };
                return call_function(&consumer, cargs, span, out);
            } else if name == "apply" {
                do_apply(&args, span, out)
            } else if name.starts_with("__ctor_") {
                let rest = &name[7..]; // skip "__ctor_"
                let id_end = rest.find('_').unwrap();
                let type_id: usize = rest[..id_end].parse().unwrap();
                let info = RECORD_REGISTRY.with(|reg| reg.borrow().get(&type_id).cloned());
                match info {
                    Some(rec_info) => {
                        if args.len() != rec_info.field_names.len() {
                            return Err(err_at(span, EvalError::Arity(format!(
                                "{}: expected {} arguments, got {}",
                                rec_info.ctor_name, rec_info.field_names.len(), args.len()
                            ))));
                        }
                        let fields: Vec<(String, Val)> = rec_info.field_names.iter()
                            .zip(args.into_iter())
                            .map(|(n, v)| (n.clone(), v))
                            .collect();
                        Ok(Val::Record {
                            type_id,
                            type_name: rec_info.type_name.clone(),
                            fields,
                        })
                    }
                    None => Err(err_at(span, EvalError::Type("unknown record type".into()))),
                }
            } else if name.starts_with("__pred_") {
                let rest = &name[7..];
                let id_end = rest.find('_').unwrap();
                let type_id: usize = rest[..id_end].parse().unwrap();
                if args.len() != 1 {
                    return Err(err_at(span, EvalError::Arity("predicate: expected 1 argument".into())));
                }
                Ok(Val::Bool(matches!(&args[0], Val::Record { type_id: tid, .. } if *tid == type_id)))
            } else if name.starts_with("__acc_") {
                let rest = &name[6..];
                let id_end = rest.find('_').unwrap();
                let type_id: usize = rest[..id_end].parse().unwrap();
                let field_and_acc = &rest[id_end+1..];
                let field_end = field_and_acc.find('_').unwrap();
                let field_name = &field_and_acc[..field_end];
                if args.len() != 1 {
                    return Err(err_at(span, EvalError::Arity("accessor: expected 1 argument".into())));
                }
                match &args[0] {
                    Val::Record { type_id: tid, fields, .. } if *tid == type_id => {
                        for (fname, val) in fields {
                            if fname == field_name {
                                return Ok(val.clone());
                            }
                        }
                        Err(err_at(span, EvalError::Type(format!("record has no field {field_name}"))))
                    }
                    _ => Err(err_at(span, EvalError::Type("accessor: expected record of correct type".into()))),
                }
            } else {
                apply_builtin(name, &args).map_err(|e| err_at(span, e))
            }
        }
        Val::Continuation(ref data) => {
            if args.len() != 1 {
                return Err(err_at(span, EvalError::Arity("continuation: expected 1 argument".into())));
            }
            invoke_continuation(data, args[0].clone())
        }
        _ => Err(err_at(span, EvalError::Type("not a procedure".into()))),
    }
}

/// Implement apply: (apply fn arg1 ... args-list)
fn do_apply(args: &[Val], span: Span, out: &mut String) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(err_at(span, EvalError::Arity("apply: need at least 2 arguments".into())));
    }
    let func = &args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Val> = args[1..args.len()-1].to_vec();
    let tail = val_list_to_vec(last)?;
    call_args.extend(tail);
    call_function(func, call_args, span, out)
}

/// Deep structural equality (equal?) with cycle detection
fn val_equal(a: &Val, b: &Val) -> bool {
    val_equal_inner(a, b, &mut HashSet::new())
}

fn val_equal_inner(a: &Val, b: &Val, seen: &mut HashSet<(usize, usize)>) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Str(x, _), Val::Str(y, _)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::Nil, Val::Nil) => true,
        (Val::Pair(pa), Val::Pair(pb)) => {
            let key = (Rc::as_ptr(pa) as usize, Rc::as_ptr(pb) as usize);
            if !seen.insert(key) {
                return true; // already comparing these, assume equal
            }
            let (a_car, a_cdr) = { let p = pa.borrow(); (p.0.clone(), p.1.clone()) };
            let (b_car, b_cdr) = { let p = pb.borrow(); (p.0.clone(), p.1.clone()) };
            val_equal_inner(&a_car, &b_car, seen) && val_equal_inner(&a_cdr, &b_cdr, seen)
        }
        (Val::Vector(a), Val::Vector(b)) => {
            let av = a.borrow();
            let bv = b.borrow();
            av.len() == bv.len() && av.iter().zip(bv.iter()).all(|(x, y)| val_equal_inner(x, y, seen))
        }
        _ => false,
    }
}

/// Identity/shallow equality (eq?)
fn val_eq(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::Nil, Val::Nil) => true,
        (Val::Void, Val::Void) => true,
        (Val::Pair(a), Val::Pair(b)) => Rc::ptr_eq(a, b),
        (Val::Vector(a), Val::Vector(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

/// eqv? semantics
fn val_eqv(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::Nil, Val::Nil) => true,
        (Val::Pair(a), Val::Pair(b)) => Rc::ptr_eq(a, b),
        (Val::Vector(a), Val::Vector(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

/// Apply a sequence of car/cdr operations. ops[0] is outermost.
/// e.g. "cadr" = car(cdr(x)) -> ops = ['a', 'd']
fn cxr_ops(v: &Val, ops: &[char], name: &str) -> Result<Val, EvalError> {
    let mut cur = v.clone();
    for &op in ops.iter().rev() {
        let next = match &cur {
            Val::Pair(p) => {
                let pair = p.borrow();
                if op == 'a' { pair.0.clone() } else { pair.1.clone() }
            }
            _ => return Err(EvalError::Type(format!("{name}: expected pair"))),
        };
        cur = next;
    }
    Ok(cur)
}

fn apply_builtin(op: &str, args: &[Val]) -> Result<Val, EvalError> {
    match op {
        "+" => {
            if args.iter().any(|a| val_is_inexact(a)) {
                let mut sum = 0.0f64;
                for a in args { sum += as_number_f64(a, "+")?; }
                Ok(Val::Float(sum))
            } else {
                let mut n = 0i64;
                let mut d = 1i64;
                for a in args {
                    let (an, ad) = val_to_exact_pair(a)
                        .ok_or_else(|| EvalError::Type(format!("+: expected number, got {a}")))?;
                    n = n * ad + an * d;
                    d *= ad;
                    let g = gcd(n.abs(), d);
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity("-: need at least 1 argument".into()));
            }
            if args.iter().any(|a| val_is_inexact(a)) {
                if args.len() == 1 {
                    Ok(Val::Float(-as_number_f64(&args[0], "-")?))
                } else {
                    let mut result = as_number_f64(&args[0], "-")?;
                    for a in &args[1..] { result -= as_number_f64(a, "-")?; }
                    Ok(Val::Float(result))
                }
            } else if args.len() == 1 {
                let (n, d) = val_to_exact_pair(&args[0])
                    .ok_or_else(|| EvalError::Type(format!("-: expected number, got {}", args[0])))?;
                Ok(make_rational(-n, d))
            } else {
                let (mut n, mut d) = val_to_exact_pair(&args[0])
                    .ok_or_else(|| EvalError::Type(format!("-: expected number, got {}", args[0])))?;
                for a in &args[1..] {
                    let (an, ad) = val_to_exact_pair(a)
                        .ok_or_else(|| EvalError::Type(format!("-: expected number, got {a}")))?;
                    n = n * ad - an * d;
                    d *= ad;
                    let g = gcd(n.abs(), d);
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "*" => {
            if args.iter().any(|a| val_is_inexact(a)) {
                let mut prod = 1.0f64;
                for a in args { prod *= as_number_f64(a, "*")?; }
                Ok(Val::Float(prod))
            } else {
                let mut n = 1i64;
                let mut d = 1i64;
                for a in args {
                    let (an, ad) = val_to_exact_pair(a)
                        .ok_or_else(|| EvalError::Type(format!("*: expected number, got {a}")))?;
                    n *= an;
                    d *= ad;
                    let g = gcd(n.abs(), d);
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("/: need at least 2 arguments".into()));
            }
            if args.iter().any(|a| val_is_inexact(a)) {
                let mut result = as_number_f64(&args[0], "/")?;
                for a in &args[1..] {
                    let d = as_number_f64(a, "/")?;
                    if d == 0.0 { return Err(EvalError::Type("division by zero".into())); }
                    result /= d;
                }
                Ok(Val::Float(result))
            } else {
                let (mut n, mut d) = val_to_exact_pair(&args[0])
                    .ok_or_else(|| EvalError::Type(format!("/: expected number, got {}", args[0])))?;
                for a in &args[1..] {
                    let (an, ad) = val_to_exact_pair(a)
                        .ok_or_else(|| EvalError::Type(format!("/: expected number, got {a}")))?;
                    if an == 0 { return Err(EvalError::Type("division by zero".into())); }
                    n *= ad;
                    d *= an;
                    let g = gcd(n.abs(), d.abs());
                    n /= g; d /= g;
                }
                Ok(make_rational(n, d))
            }
        }
        "=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("=: need at least 2 arguments".into()));
            }
            let first = as_number_f64(&args[0], "=")?;
            Ok(Val::Bool(args[1..].iter().all(|a| as_number_f64(a, "=").map_or(false, |n| n == first))))
        }
        "<" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("<: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<f64>, _> = args.iter().map(|a| as_number_f64(a, "<")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] < w[1])))
        }
        ">" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(">: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<f64>, _> = args.iter().map(|a| as_number_f64(a, ">")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] > w[1])))
        }
        "<=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity("<=: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<f64>, _> = args.iter().map(|a| as_number_f64(a, "<=")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] <= w[1])))
        }
        ">=" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(">=: need at least 2 arguments".into()));
            }
            let vals: Result<Vec<f64>, _> = args.iter().map(|a| as_number_f64(a, ">=")).collect();
            let vals = vals?;
            Ok(Val::Bool(vals.windows(2).all(|w| w[0] >= w[1])))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("not: need exactly 1 argument".into()));
            }
            Ok(Val::Bool(!is_truthy(&args[0])))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity("cons: need exactly 2 arguments".into()));
            }
            Ok(make_pair(args[0].clone(), args[1].clone()))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(p) => Ok(p.borrow().0.clone()),
                _ => Err(EvalError::Type("car: expected pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(p) => Ok(p.borrow().1.clone()),
                _ => Err(EvalError::Type("cdr: expected pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("null?: need exactly 1 argument".into()));
            }
            Ok(Val::Bool(matches!(args[0], Val::Nil)))
        }
        "list" => {
            let mut result = Val::Nil;
            for item in args.iter().rev() {
                result = make_pair(item.clone(), result);
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length: need exactly 1 argument".into()));
            }
            let mut count = 0i64;
            let mut cur = args[0].clone();
            loop {
                match &cur {
                    Val::Nil => break,
                    Val::Pair(p) => {
                        count += 1;
                        let next = p.borrow().1.clone();
                        cur = next;
                    }
                    _ => return Err(EvalError::Type("length: expected list".into())),
                }
            }
            Ok(Val::Int(count))
        }
        "append" => {
            if args.is_empty() {
                return Ok(Val::Nil);
            }
            let mut result = args.last().unwrap().clone();
            for arg in args[..args.len()-1].iter().rev() {
                let mut items = Vec::new();
                let mut cur = arg.clone();
                loop {
                    match &cur {
                        Val::Nil => break,
                        Val::Pair(p) => {
                            let pair = p.borrow();
                            items.push(pair.0.clone());
                            let next = pair.1.clone();
                            drop(pair);
                            cur = next;
                        }
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
                for item in items.into_iter().rev() {
                    result = make_pair(item, result);
                }
            }
            Ok(result)
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity("string?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Str(_, _))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity("number?: need 1 argument".into())); }
            Ok(Val::Bool(val_is_number(&args[0])))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity("boolean?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity("pair?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Pair(_))))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Symbol(_))))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Char(_))))
        }
        "procedure?" => {
            if args.len() != 1 { return Err(EvalError::Arity("procedure?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(_) | Val::Continuation(_))))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Val::Str(s, _) => result.push_str(s),
                    _ => return Err(EvalError::Type("string-append: expected string".into())),
                }
            }
            Ok(Val::Str(result, true))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-length: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => Ok(Val::Int(s.chars().count() as i64)),
                _ => Err(EvalError::Type("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity("substring: need 3 arguments".into())); }
            let s = match &args[0] {
                Val::Str(s, _) => s,
                _ => return Err(EvalError::Type("substring: expected string".into())),
            };
            let start = as_int(&args[1], "substring")? as usize;
            let end = as_int(&args[2], "substring")? as usize;
            let chars: Vec<char> = s.chars().collect();
            if end > chars.len() || start > end {
                return Err(EvalError::Type("substring: index out of range".into()));
            }
            Ok(Val::Str(chars[start..end].iter().collect(), true))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->number: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => match s.parse::<i64>() {
                    Ok(n) => Ok(Val::Int(n)),
                    Err(_) => Ok(Val::Bool(false)),
                },
                _ => Err(EvalError::Type("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("number->string: need 1 argument".into())); }
            match &args[0] {
                Val::Int(n) => Ok(Val::Str(n.to_string(), true)),
                Val::Float(_) | Val::Rational(_, _) => Ok(Val::Str(args[0].to_string(), true)),
                _ => Err(EvalError::Type("number->string: expected number".into())),
            }
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("symbol->string: need 1 argument".into())); }
            match &args[0] {
                Val::Symbol(s) => Ok(Val::Str(s.clone(), false)),
                _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->symbol: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => Ok(Val::Symbol(s.clone())),
                _ => Err(EvalError::Type("string->symbol: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ref: need 2 arguments".into())); }
            let s = match &args[0] {
                Val::Str(s, _) => s,
                _ => return Err(EvalError::Type("string-ref: expected string".into())),
            };
            let idx = as_int(&args[1], "string-ref")? as usize;
            let chars: Vec<char> = s.chars().collect();
            if idx >= chars.len() {
                return Err(EvalError::Type("string-ref: index out of range".into()));
            }
            Ok(Val::Char(chars[idx]))
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-copy: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => Ok(Val::Str(s.clone(), true)),
                _ => Err(EvalError::Type("string-copy: expected string".into())),
            }
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("string->list: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => {
                    let mut result = Val::Nil;
                    for c in s.chars().rev() {
                        result = make_pair(Val::Char(c), result);
                    }
                    Ok(result)
                }
                _ => Err(EvalError::Type("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->string: need 1 argument".into())); }
            let mut chars = Vec::new();
            let mut cur = args[0].clone();
            loop {
                match &cur {
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        match &pair.0 {
                            Val::Char(c) => chars.push(*c),
                            _ => return Err(EvalError::Type("list->string: list must contain only characters".into())),
                        }
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    Val::Nil => break,
                    _ => return Err(EvalError::Type("list->string: expected proper list".into())),
                }
            }
            Ok(Val::Str(chars.into_iter().collect(), true))
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity("char->integer: need 1 argument".into())); }
            match &args[0] {
                Val::Char(c) => Ok(Val::Int(*c as i64)),
                _ => Err(EvalError::Type("char->integer: expected char".into())),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer->char: need 1 argument".into())); }
            match &args[0] {
                Val::Int(n) => {
                    match char::from_u32(*n as u32) {
                        Some(c) => Ok(Val::Char(c)),
                        None => Err(EvalError::Type("integer->char: invalid code point".into())),
                    }
                }
                _ => Err(EvalError::Type("integer->char: expected integer".into())),
            }
        }
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity("abs: need 1 argument".into())); }
            Ok(Val::Int(as_int(&args[0], "abs")?.abs()))
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity("modulo: need 2 arguments".into())); }
            let a = as_int(&args[0], "modulo")?;
            let b = as_int(&args[1], "modulo")?;
            if b == 0 { return Err(EvalError::Type("modulo: division by zero".into())); }
            Ok(Val::Int(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity("remainder: need 2 arguments".into())); }
            let a = as_int(&args[0], "remainder")?;
            let b = as_int(&args[1], "remainder")?;
            if b == 0 { return Err(EvalError::Type("remainder: division by zero".into())); }
            Ok(Val::Int(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity("quotient: need 2 arguments".into())); }
            let a = as_int(&args[0], "quotient")?;
            let b = as_int(&args[1], "quotient")?;
            if b == 0 { return Err(EvalError::Type("quotient: division by zero".into())); }
            Ok(Val::Int(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity("min: need at least 1 argument".into())); }
            let mut m = as_int(&args[0], "min")?;
            for a in &args[1..] { m = m.min(as_int(a, "min")?); }
            Ok(Val::Int(m))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity("max: need at least 1 argument".into())); }
            let mut m = as_int(&args[0], "max")?;
            for a in &args[1..] { m = m.max(as_int(a, "max")?); }
            Ok(Val::Int(m))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity("expt: need 2 arguments".into())); }
            let base = as_int(&args[0], "expt")?;
            let exp = as_int(&args[1], "expt")?;
            if exp < 0 { return Ok(Val::Int(0)); }
            Ok(Val::Int(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity("zero?: need 1 argument".into())); }
            Ok(Val::Bool(as_int(&args[0], "zero?")? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity("positive?: need 1 argument".into())); }
            Ok(Val::Bool(as_int(&args[0], "positive?")? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity("negative?: need 1 argument".into())); }
            Ok(Val::Bool(as_int(&args[0], "negative?")? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity("odd?: need 1 argument".into())); }
            Ok(Val::Bool(as_int(&args[0], "odd?")? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity("even?: need 1 argument".into())); }
            Ok(Val::Bool(as_int(&args[0], "even?")? % 2 == 0))
        }
        "list-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-ref: need 2 arguments".into())); }
            let idx = as_int(&args[1], "list-ref")? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                let next = match &cur {
                    Val::Pair(p) => p.borrow().1.clone(),
                    _ => return Err(EvalError::Type("list-ref: index out of range".into())),
                };
                cur = next;
            }
            match &cur {
                Val::Pair(p) => Ok(p.borrow().0.clone()),
                _ => Err(EvalError::Type("list-ref: index out of range".into())),
            }
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail: need 2 arguments".into())); }
            let idx = as_int(&args[1], "list-tail")? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match &cur.clone() {
                    Val::Pair(p) => { cur = p.borrow().1.clone(); }
                    _ => return Err(EvalError::Type("list-tail: index out of range".into())),
                }
            }
            Ok(cur)
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list?: need 1 argument".into())); }
            let mut cur = args[0].clone();
            let mut seen = HashSet::new();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(true)),
                    Val::Pair(p) => {
                        let ptr = Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) {
                            return Ok(Val::Bool(false)); // cycle
                        }
                        let next = p.borrow().1.clone();
                        cur = next;
                    }
                    _ => return Ok(Val::Bool(false)),
                }
            }
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Val::Pair(kp) = &car {
                            if val_equal(&kp.borrow().0, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type("assoc: expected list".into())),
                }
            }
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity("equal?: need 2 arguments".into())); }
            Ok(Val::Bool(val_equal(&args[0], &args[1])))
        }
        "eq?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eq?: need 2 arguments".into())); }
            Ok(Val::Bool(val_eq(&args[0], &args[1])))
        }
        "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity("eqv?: need 2 arguments".into())); }
            Ok(Val::Bool(val_eqv(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Val::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity("make-vector: need 1 or 2 arguments".into()));
            }
            let len = as_int(&args[0], "make-vector")? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Val::Int(0) };
            Ok(Val::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity("vector-ref: need 2 arguments".into())); }
            match &args[0] {
                Val::Vector(v) => {
                    let idx = as_int(&args[1], "vector-ref")? as usize;
                    let elems = v.borrow();
                    if idx >= elems.len() {
                        return Err(EvalError::Type("vector-ref: index out of range".into()));
                    }
                    Ok(elems[idx].clone())
                }
                _ => Err(EvalError::Type("vector-ref: expected vector".into())),
            }
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity("vector-set!: need 3 arguments".into())); }
            match &args[0] {
                Val::Vector(v) => {
                    let idx = as_int(&args[1], "vector-set!")? as usize;
                    let mut elems = v.borrow_mut();
                    if idx >= elems.len() {
                        return Err(EvalError::Type("vector-set!: index out of range".into()));
                    }
                    elems[idx] = args[2].clone();
                    Ok(Val::Void)
                }
                _ => Err(EvalError::Type("vector-set!: expected vector".into())),
            }
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector-length: need 1 argument".into())); }
            match &args[0] {
                Val::Vector(v) => Ok(Val::Int(v.borrow().len() as i64)),
                _ => Err(EvalError::Type("vector-length: expected vector".into())),
            }
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity("vector->list: need 1 argument".into())); }
            match &args[0] {
                Val::Vector(v) => {
                    let elems = v.borrow();
                    let mut result = Val::Nil;
                    for e in elems.iter().rev() {
                        result = make_pair(e.clone(), result);
                    }
                    Ok(result)
                }
                _ => Err(EvalError::Type("vector->list: expected vector".into())),
            }
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->vector: need 1 argument".into())); }
            let items = val_list_to_vec(&args[0])?;
            Ok(Val::Vector(Rc::new(RefCell::new(items))))
        }
        "char-alphabetic?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic?: need 1 argument".into())); }
            match &args[0] {
                Val::Char(c) => Ok(Val::Bool(c.is_alphabetic())),
                _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-numeric?: need 1 argument".into())); }
            match &args[0] {
                Val::Char(c) => Ok(Val::Bool(c.is_ascii_digit())),
                _ => Err(EvalError::Type("char-numeric?: expected char".into())),
            }
        }
        "char-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-upcase: need 1 argument".into())); }
            match &args[0] {
                Val::Char(c) => Ok(Val::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type("char-upcase: expected char".into())),
            }
        }
        "char-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("char-downcase: need 1 argument".into())); }
            match &args[0] {
                Val::Char(c) => Ok(Val::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type("char-downcase: expected char".into())),
            }
        }
        "char=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char=?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a == b)),
                _ => Err(EvalError::Type("char=?: expected chars".into())),
            }
        }
        "char<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("char<?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a < b)),
                _ => Err(EvalError::Type("char<?: expected chars".into())),
            }
        }
        "string=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string=?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a == b)),
                _ => Err(EvalError::Type("string=?: expected strings".into())),
            }
        }
        "string<?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a < b)),
                _ => Err(EvalError::Type("string<?: expected strings".into())),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string-ci=?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
            }
        }
        "string-upcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-upcase: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => Ok(Val::Str(s.to_uppercase(), true)),
                _ => Err(EvalError::Type("string-upcase: expected string".into())),
            }
        }
        "string-downcase" => {
            if args.len() != 1 { return Err(EvalError::Arity("string-downcase: need 1 argument".into())); }
            match &args[0] {
                Val::Str(s, _) => Ok(Val::Str(s.to_lowercase(), true)),
                _ => Err(EvalError::Type("string-downcase: expected string".into())),
            }
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity("integer?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Int(_))))
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity("rational?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Int(_) | Val::Rational(_, _))))
        }
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Int(_) | Val::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact?: need 1 argument".into())); }
            Ok(Val::Bool(matches!(args[0], Val::Float(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity("exact->inexact: need 1 argument".into())); }
            let f = as_number_f64(&args[0], "exact->inexact")?;
            Ok(Val::Float(f))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity("inexact->exact: need 1 argument".into())); }
            match &args[0] {
                Val::Int(_) | Val::Rational(_, _) => Ok(args[0].clone()),
                Val::Float(f) => {
                    // Convert float to exact rational via power-of-2 denominator
                    if *f == f.floor() {
                        return Ok(Val::Int(*f as i64));
                    }
                    let mut num = *f;
                    let mut den = 1i64;
                    for _ in 0..53 {
                        if num == num.floor() { break; }
                        num *= 2.0;
                        den *= 2;
                    }
                    let n = num as i64;
                    Ok(make_rational(n, den))
                }
                _ => Err(EvalError::Type("inexact->exact: expected number".into())),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity("numerator: need 1 argument".into())); }
            match &args[0] {
                Val::Int(n) => Ok(Val::Int(*n)),
                Val::Rational(n, _) => Ok(Val::Int(*n)),
                _ => Err(EvalError::Type("numerator: expected rational".into())),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity("denominator: need 1 argument".into())); }
            match &args[0] {
                Val::Int(_) => Ok(Val::Int(1)),
                Val::Rational(_, d) => Ok(Val::Int(*d)),
                _ => Err(EvalError::Type("denominator: expected rational".into())),
            }
        }
        "gcd" => {
            if args.is_empty() { return Ok(Val::Int(0)); }
            let mut result = as_int(&args[0], "gcd")?.abs();
            for a in &args[1..] {
                result = gcd(result, as_int(a, "gcd")?.abs());
            }
            Ok(Val::Int(result))
        }
        "lcm" => {
            if args.is_empty() { return Ok(Val::Int(1)); }
            let mut result = as_int(&args[0], "lcm")?.abs();
            for a in &args[1..] {
                let b = as_int(a, "lcm")?.abs();
                if result == 0 && b == 0 { result = 0; }
                else { result = result / gcd(result, b) * b; }
            }
            Ok(Val::Int(result))
        }
        "truncate" => {
            if args.len() != 1 { return Err(EvalError::Arity("truncate: need 1 argument".into())); }
            match &args[0] {
                Val::Int(n) => Ok(Val::Int(*n)),
                Val::Float(f) => Ok(Val::Int(f.trunc() as i64)),
                Val::Rational(n, d) => Ok(Val::Int(n / d)),
                _ => Err(EvalError::Type("truncate: expected number".into())),
            }
        }
        "round" => {
            if args.len() != 1 { return Err(EvalError::Arity("round: need 1 argument".into())); }
            match &args[0] {
                Val::Int(n) => Ok(Val::Int(*n)),
                Val::Float(f) => Ok(Val::Int(f.round() as i64)),
                Val::Rational(n, d) => Ok(Val::Int((*n as f64 / *d as f64).round() as i64)),
                _ => Err(EvalError::Type("round: expected number".into())),
            }
        }
        "make-string" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity("make-string: need 1 or 2 arguments".into())); }
            let len = as_int(&args[0], "make-string")? as usize;
            let ch = if args.len() == 2 {
                match &args[1] { Val::Char(c) => *c, _ => return Err(EvalError::Type("make-string: expected char".into())) }
            } else { '\0' };
            Ok(Val::Str(std::iter::repeat(ch).take(len).collect(), true))
        }
        "string" => {
            let mut s = String::new();
            for a in args {
                match a { Val::Char(c) => s.push(*c), _ => return Err(EvalError::Type("string: expected char".into())) }
            }
            Ok(Val::Str(s, true))
        }
        "string>?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a > b)),
                _ => Err(EvalError::Type("string>?: expected strings".into())),
            }
        }
        "string<=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string<=?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a <= b)),
                _ => Err(EvalError::Type("string<=?: expected strings".into())),
            }
        }
        "string>=?" => {
            if args.len() != 2 { return Err(EvalError::Arity("string>=?: need 2 arguments".into())); }
            match (&args[0], &args[1]) {
                (Val::Str(a, _), Val::Str(b, _)) => Ok(Val::Bool(a >= b)),
                _ => Err(EvalError::Type("string>=?: expected strings".into())),
            }
        }
        "error" => {
            let msg = if args.is_empty() {
                "error".to_string()
            } else {
                let mut parts = Vec::new();
                for a in args { parts.push(format!("{}", a)); }
                parts.join(" ")
            };
            return Err(EvalError::Type(msg));
        }
        "assv" => {
            if args.len() != 2 { return Err(EvalError::Arity("assv: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Val::Pair(kp) = &car {
                            if val_eqv(&kp.borrow().0, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type("assv: expected list".into())),
                }
            }
        }
        "memv" => {
            if args.len() != 2 { return Err(EvalError::Arity("memv: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        if val_eqv(&pair.0, key) {
                            drop(pair);
                            return Ok(cur);
                        }
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type("memv: expected list".into())),
                }
            }
        }
        "caar" => {
            if args.len() != 1 { return Err(EvalError::Arity("caar: need 1 argument".into())); }
            let a = match &args[0] { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("caar: expected pair".into())) };
            match &a { Val::Pair(p) => Ok(p.borrow().0.clone()), _ => Err(EvalError::Type("caar: expected pair".into())) }
        }
        "cadr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cadr: need 1 argument".into())); }
            let d = match &args[0] { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("cadr: expected pair".into())) };
            match &d { Val::Pair(p) => Ok(p.borrow().0.clone()), _ => Err(EvalError::Type("cadr: expected pair".into())) }
        }
        "cdar" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdar: need 1 argument".into())); }
            let a = match &args[0] { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("cdar: expected pair".into())) };
            match &a { Val::Pair(p) => Ok(p.borrow().1.clone()), _ => Err(EvalError::Type("cdar: expected pair".into())) }
        }
        "cddr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cddr: need 1 argument".into())); }
            let d = match &args[0] { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("cddr: expected pair".into())) };
            match &d { Val::Pair(p) => Ok(p.borrow().1.clone()), _ => Err(EvalError::Type("cddr: expected pair".into())) }
        }
        "caddr" => {
            if args.len() != 1 { return Err(EvalError::Arity("caddr: need 1 argument".into())); }
            let d = match &args[0] { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("caddr: expected pair".into())) };
            let dd = match &d { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("caddr: expected pair".into())) };
            match &dd { Val::Pair(p) => Ok(p.borrow().0.clone()), _ => Err(EvalError::Type("caddr: expected pair".into())) }
        }
        "cdddr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdddr: need 1 argument".into())); }
            let d = match &args[0] { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("cdddr: expected pair".into())) };
            let dd = match &d { Val::Pair(p) => p.borrow().1.clone(), _ => return Err(EvalError::Type("cdddr: expected pair".into())) };
            match &dd { Val::Pair(p) => Ok(p.borrow().1.clone()), _ => Err(EvalError::Type("cdddr: expected pair".into())) }
        }
        "caaar" => {
            if args.len() != 1 { return Err(EvalError::Arity("caaar: need 1 argument".into())); }
            let a = match &args[0] { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("caaar: expected pair".into())) };
            let aa = match &a { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("caaar: expected pair".into())) };
            match &aa { Val::Pair(p) => Ok(p.borrow().0.clone()), _ => Err(EvalError::Type("caaar: expected pair".into())) }
        }
        "cdaar" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdaar: need 1 argument".into())); }
            let a = match &args[0] { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("cdaar: expected pair".into())) };
            let aa = match &a { Val::Pair(p) => p.borrow().0.clone(), _ => return Err(EvalError::Type("cdaar: expected pair".into())) };
            match &aa { Val::Pair(p) => Ok(p.borrow().1.clone()), _ => Err(EvalError::Type("cdaar: expected pair".into())) }
        }
        "cadar" => {
            if args.len() != 1 { return Err(EvalError::Arity("cadar: need 1 argument".into())); }
            cxr_ops(&args[0], &['a', 'd', 'a'], "cadar")
        }
        "caddar" => {
            if args.len() != 1 { return Err(EvalError::Arity("caddar: need 1 argument".into())); }
            cxr_ops(&args[0], &['a', 'd', 'd', 'a'], "caddar")
        }
        "cadddr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cadddr: need 1 argument".into())); }
            cxr_ops(&args[0], &['a', 'd', 'd', 'd'], "cadddr")
        }
        "cdadr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cdadr: need 1 argument".into())); }
            cxr_ops(&args[0], &['d', 'a', 'd'], "cdadr")
        }
        "cddar" => {
            if args.len() != 1 { return Err(EvalError::Arity("cddar: need 1 argument".into())); }
            cxr_ops(&args[0], &['d', 'd', 'a'], "cddar")
        }
        "cddddr" => {
            if args.len() != 1 { return Err(EvalError::Arity("cddddr: need 1 argument".into())); }
            cxr_ops(&args[0], &['d', 'd', 'd', 'd'], "cddddr")
        }
        "member" => {
            if args.len() != 2 { return Err(EvalError::Arity("member: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        if val_equal(&pair.0, key) {
                            drop(pair);
                            return Ok(cur);
                        }
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type("member: expected list".into())),
                }
            }
        }
        "reverse" => {
            if args.len() != 1 { return Err(EvalError::Arity("reverse: need 1 argument".into())); }
            let mut result = Val::Nil;
            let mut cur = args[0].clone();
            loop {
                match &cur {
                    Val::Nil => break,
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        result = make_pair(pair.0.clone(), result);
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type("reverse: expected list".into())),
                }
            }
            Ok(result)
        }
        "set-car!" => {
            if args.len() != 2 { return Err(EvalError::Arity("set-car!: need 2 arguments".into())); }
            match &args[0] {
                Val::Pair(p) => { p.borrow_mut().0 = args[1].clone(); Ok(Val::Void) }
                _ => Err(EvalError::Type("set-car!: expected pair".into())),
            }
        }
        "set-cdr!" => {
            if args.len() != 2 { return Err(EvalError::Arity("set-cdr!: need 2 arguments".into())); }
            match &args[0] {
                Val::Pair(p) => { p.borrow_mut().1 = args[1].clone(); Ok(Val::Void) }
                _ => Err(EvalError::Type("set-cdr!: expected pair".into())),
            }
        }
        "memq" => {
            if args.len() != 2 { return Err(EvalError::Arity("memq: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        if val_eq(&pair.0, key) {
                            drop(pair);
                            return Ok(cur);
                        }
                        let next = pair.1.clone();
                        drop(pair);
                        cur = next;
                    }
                    _ => return Err(EvalError::Type("memq: expected list".into())),
                }
            }
        }
        "assq" => {
            if args.len() != 2 { return Err(EvalError::Arity("assq: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = args[1].clone();
            loop {
                match &cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(p) => {
                        let pair = p.borrow();
                        let car = pair.0.clone();
                        let cdr = pair.1.clone();
                        drop(pair);
                        if let Val::Pair(kp) = &car {
                            if val_eq(&kp.borrow().0, key) {
                                return Ok(car);
                            }
                        }
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type("assq: expected list".into())),
                }
            }
        }
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut out = String::new();
    let result = cek_eval(&exprs, &env, &mut out)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut out = String::new();
    let result = cek_eval(&exprs, &env, &mut out)?;
    let result_str = match result {
        Val::Void => String::new(),
        other => other.to_string(),
    };
    Ok((result_str, out))
}

#[cfg(test)]
mod tests;
