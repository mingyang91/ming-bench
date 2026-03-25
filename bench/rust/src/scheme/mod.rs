pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
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
    Pair(Box<Val>, Box<Val>),
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
            Val::Pair(_, _) => {
                write!(f, "(")?;
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Val::Pair(car, cdr) => {
                            if !first { write!(f, " ")?; }
                            first = false;
                            write!(f, "{car}")?;
                            cur = cdr;
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
        Val::Pair(_, _) => {
            f.push('(');
            let mut cur: &Val = v;
            let mut first = true;
            loop {
                match cur {
                    Val::Pair(car, cdr) => {
                        if !first { f.push(' '); }
                        first = false;
                        display_val(car, f);
                        cur = cdr;
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
                result = Val::Pair(Box::new(quote_expr(item)), Box::new(result));
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

fn eval(expr: &Expr, env: &Env, out: &mut String) -> Result<Val, EvalError> {
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
                    return eval(&expanded, env, out);
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
                            return eval(&list[2], env, out);
                        } else if list.len() == 4 {
                            return eval(&list[3], env, out);
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
                            let mut result = Val::Void;
                            for expr in &list[3..] {
                                result = eval(expr, &call_env, out)?;
                            }
                            return Ok(result);
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
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &let_env, out)?;
                        }
                        return Ok(result);
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
                        let mut result = Val::Void;
                        for expr in &list[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &list[1..] {
                            match &clause.kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    if let ExprKind::Symbol(s) = &parts[0].kind {
                                        if s == "else" {
                                            let mut result = Val::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, env, out)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let cond_val = eval(&parts[0], env, out)?;
                                    if is_truthy(&cond_val) {
                                        let mut result = cond_val;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env, out)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(err_at(span, EvalError::Parse("cond: bad clause".into()))),
                            }
                        }
                        return Ok(Val::Void);
                    }
                    "and" => {
                        let mut result = Val::Bool(true);
                        for arg in &list[1..] {
                            result = eval(arg, env, out)?;
                            if !is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Val::Bool(false);
                        for arg in &list[1..] {
                            result = eval(arg, env, out)?;
                            if is_truthy(&result) {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
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
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &let_env, out)?;
                        }
                        return Ok(result);
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
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &letrec_env, out)?;
                        }
                        return Ok(result);
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
                        let mut result = Val::Void;
                        for expr in &list[2..] {
                            result = eval(expr, &letrec_env, out)?;
                        }
                        return Ok(result);
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
                                            let mut result = Val::Void;
                                            for expr in &parts[1..] {
                                                result = eval(expr, env, out)?;
                                            }
                                            return Ok(result);
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
                                        let mut result = Val::Void;
                                        for expr in &parts[1..] {
                                            result = eval(expr, env, out)?;
                                        }
                                        return Ok(result);
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
                                // Evaluate result expressions
                                let mut result = Val::Void;
                                for expr in &test_clause[1..] {
                                    result = eval(expr, &do_env, out)?;
                                }
                                return Ok(result);
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
                        result = Val::Pair(Box::new(r), Box::new(result));
                    }
                    return Ok(result);
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
            let mut args = Vec::new();
            for a in &list[1..] {
                args.push(eval(a, env, out)?);
            }

            call_function(&func, args, span, out)
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
        | "vector?" | "vector->list" | "list->vector")
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
    let mut cur = v;
    loop {
        match cur {
            Val::Nil => break,
            Val::Pair(car, cdr) => {
                result.push(*car.clone());
                cur = cdr;
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
                    rest_list = Val::Pair(Box::new(a.clone()), Box::new(rest_list));
                }
                env_set(&call_env, rp.clone(), rest_list);
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &call_env, out)?;
            }
            Ok(result)
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
                            rest_list = Val::Pair(Box::new(a.clone()), Box::new(rest_list));
                        }
                        env_set(&call_env, rp.clone(), rest_list);
                    }
                    let mut result = Val::Void;
                    for expr in body {
                        result = eval(expr, &call_env, out)?;
                    }
                    return Ok(result);
                }
            }
            Err(err_at(span, EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            ))))
        }
        Val::Builtin(name) => {
            if name == "apply" {
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

/// Deep structural equality (equal?)
fn val_equal(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Str(x, _), Val::Str(y, _)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::Nil, Val::Nil) => true,
        (Val::Pair(a1, a2), Val::Pair(b1, b2)) => val_equal(a1, b1) && val_equal(a2, b2),
        (Val::Vector(a), Val::Vector(b)) => {
            let av = a.borrow();
            let bv = b.borrow();
            av.len() == bv.len() && av.iter().zip(bv.iter()).all(|(x, y)| val_equal(x, y))
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
        (Val::Vector(a), Val::Vector(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
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
            Ok(Val::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("car: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type("car: expected pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("cdr: need exactly 1 argument".into()));
            }
            match &args[0] {
                Val::Pair(_, cdr) => Ok(*cdr.clone()),
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
                result = Val::Pair(Box::new(item.clone()), Box::new(result));
            }
            Ok(result)
        }
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity("length: need exactly 1 argument".into()));
            }
            let mut count = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Val::Nil => break,
                    Val::Pair(_, cdr) => {
                        count += 1;
                        cur = cdr;
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
                let mut cur = arg;
                loop {
                    match cur {
                        Val::Nil => break,
                        Val::Pair(car, cdr) => {
                            items.push(*car.clone());
                            cur = cdr;
                        }
                        _ => return Err(EvalError::Type("append: expected list".into())),
                    }
                }
                for item in items.into_iter().rev() {
                    result = Val::Pair(Box::new(item), Box::new(result));
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
            Ok(Val::Bool(matches!(args[0], Val::Pair(_, _))))
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
            Ok(Val::Bool(matches!(args[0], Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(_))))
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
                        result = Val::Pair(Box::new(Val::Char(c)), Box::new(result));
                    }
                    Ok(result)
                }
                _ => Err(EvalError::Type("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity("list->string: need 1 argument".into())); }
            let mut chars = Vec::new();
            let mut cur = &args[0];
            loop {
                match cur {
                    Val::Pair(a, b) => {
                        match a.as_ref() {
                            Val::Char(c) => chars.push(*c),
                            _ => return Err(EvalError::Type("list->string: list must contain only characters".into())),
                        }
                        cur = b.as_ref();
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
            let mut cur = &args[0];
            for _ in 0..idx {
                match cur {
                    Val::Pair(_, cdr) => cur = cdr,
                    _ => return Err(EvalError::Type("list-ref: index out of range".into())),
                }
            }
            match cur {
                Val::Pair(car, _) => Ok(*car.clone()),
                _ => Err(EvalError::Type("list-ref: index out of range".into())),
            }
        }
        "list-tail" => {
            if args.len() != 2 { return Err(EvalError::Arity("list-tail: need 2 arguments".into())); }
            let idx = as_int(&args[1], "list-tail")? as usize;
            let mut cur = args[0].clone();
            for _ in 0..idx {
                match cur {
                    Val::Pair(_, cdr) => cur = *cdr,
                    _ => return Err(EvalError::Type("list-tail: index out of range".into())),
                }
            }
            Ok(cur)
        }
        "list?" => {
            if args.len() != 1 { return Err(EvalError::Arity("list?: need 1 argument".into())); }
            let mut cur = &args[0];
            loop {
                match cur {
                    Val::Nil => return Ok(Val::Bool(true)),
                    Val::Pair(_, cdr) => cur = cdr,
                    _ => return Ok(Val::Bool(false)),
                }
            }
        }
        "assoc" => {
            if args.len() != 2 { return Err(EvalError::Arity("assoc: need 2 arguments".into())); }
            let key = &args[0];
            let mut cur = &args[1];
            loop {
                match cur {
                    Val::Nil => return Ok(Val::Bool(false)),
                    Val::Pair(car, cdr) => {
                        if let Val::Pair(k, _) = car.as_ref() {
                            if val_equal(k, key) {
                                return Ok(*car.clone());
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
                        result = Val::Pair(Box::new(e.clone()), Box::new(result));
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
        _ => Err(EvalError::UnboundVariable(op.into())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Val::Void;
    let mut out = String::new();
    for expr in &exprs {
        result = eval(&expr, &env, &mut out)?;
    }
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = new_env(None);
    let mut result = Val::Void;
    let mut out = String::new();
    for expr in &exprs {
        result = eval(&expr, &env, &mut out)?;
    }
    let result_str = match result {
        Val::Void => String::new(),
        other => other.to_string(),
    };
    Ok((result_str, out))
}

#[cfg(test)]
mod tests;
