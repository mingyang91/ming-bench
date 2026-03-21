pub mod error;

pub use error::{EvalError, Pos};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

/// A Scheme value.
#[derive(Debug, Clone)]
enum Val {
    Int(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Bool(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Val>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
    Continuation(u64),
    Vector(Rc<RefCell<Vec<Val>>>),
    Pair(Box<Val>, Box<Val>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Values(Vec<Val>),
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Val)>,
    },
    Void,
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
    if d == 0 {
        panic!("rational with zero denominator");
    }
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n, d);
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Val::Int(n)
    } else {
        Val::Rational(n, d)
    }
}

fn val_to_f64(v: &Val) -> Option<f64> {
    match v {
        Val::Int(n) => Some(*n as f64),
        Val::Float(f) => Some(*f),
        Val::Rational(n, d) => Some(*n as f64 / *d as f64),
        _ => None,
    }
}

fn is_numeric(v: &Val) -> bool {
    matches!(v, Val::Int(_) | Val::Float(_) | Val::Rational(_, _))
}

fn is_exact(v: &Val) -> bool {
    matches!(v, Val::Int(_) | Val::Rational(_, _))
}

type Output = Rc<RefCell<String>>;

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
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
            Val::Symbol(s) => write!(f, "{}", s),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Val::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                _ => write!(f, "#\\{}", c),
            },
            Val::Pair(a, b) => {
                write!(f, "({}", a)?;
                let mut current: &Val = b;
                loop {
                    match current {
                        Val::Pair(ca, cb) => {
                            write!(f, " {}", ca)?;
                            current = cb;
                        }
                        Val::List(elems) if elems.is_empty() => break,
                        other => {
                            write!(f, " . {}", other)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Val::Vector(v) => {
                write!(f, "#(")?;
                let elems = v.borrow();
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } => write!(f, "#<procedure>"),
            Val::Builtin(name) => write!(f, "#<builtin:{}>", name),
            Val::Continuation(_) => write!(f, "#<continuation>"),
            Val::Macro { .. } => write!(f, "#<macro>"),
            Val::Values(vals) => {
                for (i, v) in vals.iter().enumerate() {
                    if i > 0 { write!(f, "\n")?; }
                    write!(f, "{}", v)?;
                }
                Ok(())
            }
            Val::Record { type_name, fields, .. } => {
                write!(f, "#<{}", type_name)?;
                for (name, val) in fields {
                    write!(f, " {}={}", name, val)?;
                }
                write!(f, ">")
            }
            Val::Void => write!(f, ""),
        }
    }
}

// ---------- Environment ----------

type Env = Rc<RefCell<EnvInner>>;

struct EnvInner {
    bindings: HashMap<String, Val>,
    parent: Option<Env>,
}

impl fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
            .field("bindings", &self.bindings.keys().collect::<Vec<_>>())
            .finish()
    }
}

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

fn env_update(env: &Env, name: &str, val: Val) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        true
    } else if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        false
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    for name in &[
        "+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
        "cons", "car", "cdr", "null?", "list", "length",
        "string?", "number?", "boolean?", "pair?", "symbol?",
        "display", "write", "newline",
        "string-append", "string-length", "substring",
        "string->number", "number->string",
        "symbol->string", "string->symbol",
        "string-ref", "char?", "string-copy",
        "string->list", "list->string", "char->integer", "integer->char",
        "map", "apply", "call/cc", "dynamic-wind",
        "equal?", "eqv?", "eq?",
        "vector", "make-vector", "vector-ref", "vector-set!", "vector-length",
        "vector?", "vector->list", "list->vector",
        "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
        "zero?", "positive?", "negative?", "odd?", "even?",
        "list-ref", "list-tail", "list?", "assoc", "reverse", "append",
        "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
        "char=?", "char<?",
        "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
        "raise", "with-exception-handler",
        "values", "call-with-values",
        "exact?", "inexact?", "rational?", "integer?",
        "exact->inexact", "inexact->exact",
        "numerator", "denominator",
    ] {
        env_set(&env, name.to_string(), Val::Builtin(name.to_string()));
    }
    env
}

/// Display a value without quotes (for `display`).
fn display_val(v: &Val) -> String {
    match v {
        Val::Str(s) => s.clone(),
        Val::Char(c) => c.to_string(),
        Val::Float(_) | Val::Rational(_, _) => v.to_string(),
        Val::Pair(a, b) => {
            let mut s = String::from("(");
            s.push_str(&display_val(a));
            let mut current: &Val = b;
            loop {
                match current {
                    Val::Pair(ca, cb) => {
                        s.push(' ');
                        s.push_str(&display_val(ca));
                        current = cb;
                    }
                    Val::List(elems) if elems.is_empty() => break,
                    other => {
                        s.push_str(" . ");
                        s.push_str(&display_val(other));
                        break;
                    }
                }
            }
            s.push(')');
            s
        }
        Val::List(elems) => {
            let mut s = String::from("(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                s.push_str(&display_val(e));
            }
            s.push(')');
            s
        }
        Val::Vector(v) => {
            let elems = v.borrow();
            let mut s = String::from("#(");
            for (i, e) in elems.iter().enumerate() {
                if i > 0 {
                    s.push(' ');
                }
                s.push_str(&display_val(e));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}

// ---------- Continuation State ----------

type ContKey = (Pos, u32);

struct ContState {
    next_id: u64,
    current_expr_index: usize,
    expr_start_ids: Vec<u64>,
    expr_start_pos_counts: Vec<HashMap<Pos, u32>>,
    pos_counts: HashMap<Pos, u32>,
    id_to_key: HashMap<u64, ContKey>,
    cont_expr_index: HashMap<u64, usize>,
    signal: Option<(u64, Val)>,
    resume: Option<(ContKey, Val)>,
}

impl ContState {
    fn new() -> Self {
        ContState {
            next_id: 0,
            current_expr_index: 0,
            expr_start_ids: Vec::new(),
            expr_start_pos_counts: Vec::new(),
            pos_counts: HashMap::new(),
            id_to_key: HashMap::new(),
            cont_expr_index: HashMap::new(),
            signal: None,
            resume: None,
        }
    }
}

thread_local! {
    static CONT_STATE: RefCell<ContState> = RefCell::new(ContState::new());
    static EXCEPTION_VAL: RefCell<Option<Val>> = RefCell::new(None);
}

fn callcc_exec(proc: &Val, pos: Pos, out: &Output) -> Result<Val, EvalError> {
    let (id, resume_val) = CONT_STATE.with(|cs| {
        let mut state = cs.borrow_mut();
        let id = state.next_id;
        state.next_id += 1;

        // Compute position-based key for this call/cc invocation
        let count = state.pos_counts.entry(pos).or_insert(0);
        let key = (pos, *count);
        *count += 1;
        state.id_to_key.insert(id, key);

        // Match resume by position-based key, not sequential ID
        if let Some((target_key, _)) = &state.resume {
            if key == *target_key {
                let (_, val) = state.resume.take().unwrap();
                return (id, Some(val));
            }
        }

        let expr_idx = state.current_expr_index;
        state.cont_expr_index.insert(id, expr_idx);
        (id, None)
    });

    if let Some(val) = resume_val {
        return Ok(val);
    }

    let cont_val = Val::Continuation(id);
    apply_func(proc, &[cont_val], pos, out)
}

fn invoke_continuation(id: u64, val: Val) -> EvalError {
    CONT_STATE.with(|cs| {
        cs.borrow_mut().signal = Some((id, val));
    });
    EvalError::ContinuationReturn
}

// ---------- Tokenizer ----------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Quote,
}

fn tokenize(input: &str) -> Result<Vec<(Token, Pos)>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;

    while i < chars.len() {
        match chars[i] {
            '\n' => {
                line += 1;
                col = 1;
                i += 1;
            }
            ' ' | '\t' | '\r' => {
                col += 1;
                i += 1;
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' => {
                tokens.push((Token::LParen, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            ')' => {
                tokens.push((Token::RParen, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            '\'' => {
                tokens.push((Token::Quote, Pos::new(line, col)));
                i += 1;
                col += 1;
            }
            '"' => {
                let start_pos = Pos::new(line, col);
                i += 1;
                col += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\n' {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '"' => s.push('"'),
                            '\\' => s.push('\\'),
                            c => s.push(c),
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse {
                        msg: "unterminated string".into(),
                        pos: start_pos,
                    });
                }
                i += 1; // skip closing "
                col += 1;
                tokens.push((Token::Str(s), start_pos));
            }
            '#' => {
                let p = Pos::new(line, col);
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push((Token::Bool(true), p));
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push((Token::Bool(false), p));
                            i += 2;
                            col += 2;
                        }
                        '\\' => {
                            // Character literal #\<char> or #\space, #\newline
                            if i + 2 >= chars.len() {
                                return Err(EvalError::Parse {
                                    msg: "unexpected end of character literal".into(),
                                    pos: p,
                                });
                            }
                            // Try to read a named character
                            let rest: String = chars[i + 2..].iter()
                                .take_while(|c| c.is_alphabetic())
                                .collect();
                            if rest.len() > 1 {
                                let ch = match rest.as_str() {
                                    "space" => ' ',
                                    "newline" => '\n',
                                    "tab" => '\t',
                                    _ => return Err(EvalError::Parse {
                                        msg: format!("unknown character name: {}", rest),
                                        pos: p,
                                    }),
                                };
                                let advance = 2 + rest.len();
                                tokens.push((Token::Char(ch), p));
                                i += advance;
                                col += advance;
                            } else {
                                let ch = chars[i + 2];
                                tokens.push((Token::Char(ch), p));
                                i += 3;
                                col += 3;
                            }
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                msg: format!("unexpected #{}", chars[i + 1]),
                                pos: p,
                            })
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        msg: "unexpected #".into(),
                        pos: p,
                    });
                }
            }
            _ => {
                // number or symbol
                let start = i;
                let p = Pos::new(line, col);
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '"' | '\'')
                {
                    i += 1;
                    col += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push((Token::Int(n), p));
                } else if word.contains('/') {
                    // Try rational literal like 1/3, -5/2
                    let parts: Vec<&str> = word.splitn(2, '/').collect();
                    if parts.len() == 2 {
                        if let (Ok(n), Ok(d)) = (parts[0].parse::<i64>(), parts[1].parse::<i64>()) {
                            if d != 0 {
                                tokens.push((Token::Rational(n, d), p));
                            } else {
                                tokens.push((Token::Symbol(word), p));
                            }
                        } else {
                            tokens.push((Token::Symbol(word), p));
                        }
                    } else {
                        tokens.push((Token::Symbol(word), p));
                    }
                } else if word.contains('.') {
                    if let Ok(f) = word.parse::<f64>() {
                        tokens.push((Token::Float(f), p));
                    } else {
                        tokens.push((Token::Symbol(word), p));
                    }
                } else {
                    tokens.push((Token::Symbol(word), p));
                }
            }
        }
    }
    Ok(tokens)
}

// ---------- Parser ----------

#[derive(Debug, Clone)]
struct Expr {
    kind: ExprKind,
    pos: Pos,
}

#[derive(Debug, Clone)]
enum ExprKind {
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
    fn new(kind: ExprKind, pos: Pos) -> Self {
        Self { kind, pos }
    }
}

fn parse(tokens: &[(Token, Pos)], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        let p = if tokens.is_empty() {
            Pos::new(1, 1)
        } else {
            tokens[tokens.len() - 1].1
        };
        return Err(EvalError::Parse {
            msg: "unexpected end of input".into(),
            pos: p,
        });
    }
    let (ref tok, tpos) = tokens[*pos];
    match tok {
        Token::Int(n) => {
            let n = *n;
            *pos += 1;
            Ok(Expr::new(ExprKind::Int(n), tpos))
        }
        Token::Float(f) => {
            let f = *f;
            *pos += 1;
            Ok(Expr::new(ExprKind::Float(f), tpos))
        }
        Token::Rational(n, d) => {
            let (n, d) = (*n, *d);
            *pos += 1;
            Ok(Expr::new(ExprKind::Rational(n, d), tpos))
        }
        Token::Bool(b) => {
            let b = *b;
            *pos += 1;
            Ok(Expr::new(ExprKind::Bool(b), tpos))
        }
        Token::Str(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Str(s), tpos))
        }
        Token::Symbol(s) => {
            let s = s.clone();
            *pos += 1;
            Ok(Expr::new(ExprKind::Symbol(s), tpos))
        }
        Token::Char(c) => {
            let c = *c;
            *pos += 1;
            Ok(Expr::new(ExprKind::Char(c), tpos))
        }
        Token::Quote => {
            let qpos = tpos;
            *pos += 1;
            let inner = parse(tokens, pos)?;
            Ok(Expr::new(
                ExprKind::List(vec![
                    Expr::new(ExprKind::Symbol("quote".into()), qpos),
                    inner,
                ]),
                qpos,
            ))
        }
        Token::LParen => {
            let lpos = tpos;
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && !matches!(tokens[*pos].0, Token::RParen) {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse {
                    msg: "unmatched (".into(),
                    pos: lpos,
                });
            }
            *pos += 1; // skip )
            Ok(Expr::new(ExprKind::List(elems), lpos))
        }
        Token::RParen => Err(EvalError::Parse {
            msg: "unexpected )".into(),
            pos: tpos,
        }),
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------- Macro Support ----------

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__hyg_{}", base, id)
}

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

/// Match syntax-rules pattern args against input args.
fn macro_match(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, MacroBinding>> {
    let mut bindings = HashMap::new();

    let ellipsis_pos = pattern
        .iter()
        .position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));

    if let Some(epos) = ellipsis_pos {
        if epos == 0 {
            return None;
        }
        let fixed_before = epos - 1;
        let fixed_after = pattern.len() - epos - 1;

        if input.len() < fixed_before + fixed_after {
            return None;
        }

        for i in 0..fixed_before {
            match_single(&pattern[i], &input[i], literals, &mut bindings)?;
        }

        let var_name = match &pattern[epos - 1].kind {
            ExprKind::Symbol(s) => s.clone(),
            _ => return None,
        };
        let var_end = input.len() - fixed_after;
        let var_exprs: Vec<Expr> = input[fixed_before..var_end].to_vec();
        bindings.insert(var_name, MacroBinding::Many(var_exprs));

        for i in 0..fixed_after {
            match_single(
                &pattern[epos + 1 + i],
                &input[var_end + i],
                literals,
                &mut bindings,
            )?;
        }
    } else {
        if pattern.len() != input.len() {
            return None;
        }
        for (p, inp) in pattern.iter().zip(input.iter()) {
            match_single(p, inp, literals, &mut bindings)?;
        }
    }

    Some(bindings)
}

fn match_single(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> Option<()> {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => Some(()),
        ExprKind::Symbol(s) if literals.contains(s) => match &input.kind {
            ExprKind::Symbol(s2) if s == s2 => Some(()),
            _ => None,
        },
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            Some(())
        }
        ExprKind::List(pelems) => match &input.kind {
            ExprKind::List(ielems) => {
                let sub = macro_match(pelems, ielems, literals)?;
                bindings.extend(sub);
                Some(())
            }
            _ => None,
        },
        ExprKind::Int(n) => match &input.kind {
            ExprKind::Int(n2) if n == n2 => Some(()),
            _ => None,
        },
        ExprKind::Bool(b) => match &input.kind {
            ExprKind::Bool(b2) if b == b2 => Some(()),
            _ => None,
        },
        _ => None,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(MacroBinding::Single(expr)) = bindings.get(s) {
                expr.clone()
            } else if let Some(new_name) = renames.get(s) {
                Expr::new(ExprKind::Symbol(new_name.clone()), template.pos)
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() {
                    if let ExprKind::Symbol(s) = &elems[i + 1].kind {
                        if s == "..." {
                            let expanded = expand_ellipsis(&elems[i], bindings, renames);
                            result.extend(expanded);
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(expand_template(&elems[i], bindings, renames));
                i += 1;
            }
            Expr::new(ExprKind::List(result), template.pos)
        }
        _ => template.clone(),
    }
}

fn expand_ellipsis(
    template_elem: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Vec<Expr> {
    let mut many_var = None;
    collect_many_var(template_elem, bindings, &mut many_var);

    if let Some(var_name) = many_var {
        if let Some(MacroBinding::Many(exprs)) = bindings.get(&var_name) {
            return exprs
                .iter()
                .map(|expr| {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var_name.clone(), MacroBinding::Single(expr.clone()));
                    expand_template(template_elem, &new_bindings, renames)
                })
                .collect();
        }
    }

    vec![expand_template(template_elem, bindings, renames)]
}

fn collect_many_var(
    expr: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    result: &mut Option<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Many(_))) {
                *result = Some(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_many_var(e, bindings, result);
            }
        }
        _ => {}
    }
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String], vars: &mut HashSet<String>) {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "..." || s == "_" || literals.contains(s) => {}
        ExprKind::Symbol(s) => {
            vars.insert(s.clone());
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_pattern_vars(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn collect_free_vars(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    free_vars: &mut HashSet<String>,
) {
    match &template.kind {
        ExprKind::Symbol(s) if s == "..." => {}
        ExprKind::Symbol(s) => {
            if !pattern_vars.contains(s) && !is_special_form(s) {
                free_vars.insert(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_free_vars(e, pattern_vars, free_vars);
            }
        }
        _ => {}
    }
}

fn is_special_form(s: &str) -> bool {
    matches!(
        s,
        "if" | "define"
            | "set!"
            | "quote"
            | "lambda"
            | "and"
            | "or"
            | "let"
            | "letrec"
            | "letrec*"
            | "case"
            | "begin"
            | "cond"
            | "define-syntax"
            | "syntax-rules"
            | "string-set!"
            | "guard"
            | "define-record-type"
    )
}

// ---------- Value Equality ----------

fn eqv_vals(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => x == y,
        (Val::Float(x), Val::Float(y)) => x == y,
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => n1 == n2 && d1 == d2,
        (Val::Bool(x), Val::Bool(y)) => x == y,
        (Val::Char(x), Val::Char(y)) => x == y,
        (Val::Symbol(x), Val::Symbol(y)) => x == y,
        (Val::Str(x), Val::Str(y)) => x == y,
        (Val::List(x), Val::List(y)) => x.is_empty() && y.is_empty(),
        (Val::Void, Val::Void) => true,
        _ => false,
    }
}

fn equal_vals(a: &Val, b: &Val) -> bool {
    match (a, b) {
        (Val::List(x), Val::List(y)) => {
            x.len() == y.len() && x.iter().zip(y.iter()).all(|(a, b)| equal_vals(a, b))
        }
        (Val::Pair(a1, b1), Val::Pair(a2, b2)) => {
            equal_vals(a1, a2) && equal_vals(b1, b2)
        }
        (Val::Vector(x), Val::Vector(y)) => {
            let xb = x.borrow();
            let yb = y.borrow();
            xb.len() == yb.len() && xb.iter().zip(yb.iter()).all(|(a, b)| equal_vals(a, b))
        }
        _ => eqv_vals(a, b),
    }
}

// ---------- Evaluator ----------

fn eval(expr: &Expr, env: &Env, out: &Output) -> Result<Val, EvalError> {
    let mut cur_expr = expr.clone();
    let mut cur_env = env.clone();

    'tco: loop {
        let p = cur_expr.pos;
        match &cur_expr.kind {
            ExprKind::Int(n) => return Ok(Val::Int(*n)),
            ExprKind::Float(f) => return Ok(Val::Float(*f)),
            ExprKind::Rational(n, d) => return Ok(make_rational(*n, *d)),
            ExprKind::Bool(b) => return Ok(Val::Bool(*b)),
            ExprKind::Str(s) => return Ok(Val::Str(s.clone())),
            ExprKind::Char(c) => return Ok(Val::Char(*c)),
            ExprKind::Symbol(name) => {
                return env_get(&cur_env, name).ok_or_else(|| EvalError::UnboundVariable {
                    name: name.clone(),
                    pos: p,
                });
            }
            ExprKind::List(elems) => {
                if elems.is_empty() {
                    return Ok(Val::List(vec![]));
                }

                // Check for special forms
                if let ExprKind::Symbol(op) = &elems[0].kind {
                    match op.as_str() {
                        "if" => {
                            let args = &elems[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Arity {
                                    msg: "if requires 2 or 3 arguments".into(),
                                    pos: p,
                                });
                            }
                            let cond = eval(&args[0], &cur_env, out)?;
                            if cond.is_truthy() {
                                cur_expr = args[1].clone();
                                continue;
                            } else if args.len() == 3 {
                                cur_expr = args[2].clone();
                                continue;
                            } else {
                                return Ok(Val::Void);
                            }
                        }
                        "define" => return eval_define(&elems[1..], &cur_env, p, out),
                        "set!" => {
                            if elems.len() != 3 {
                                return Err(EvalError::Arity {
                                    msg: "set! requires exactly 2 arguments".to_string(),
                                    pos: p,
                                });
                            }
                            let name = match &elems[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type {
                                    msg: "set! requires a symbol as first argument".to_string(),
                                    pos: p,
                                }),
                            };
                            let val = eval(&elems[2], &cur_env, out)?;
                            if !env_update(&cur_env, &name, val) {
                                return Err(EvalError::UnboundVariable { name, pos: p });
                            }
                            return Ok(Val::Void);
                        }
                        "quote" => return eval_quote(&elems[1..], p),
                        "lambda" => return eval_lambda(&elems[1..], &cur_env, p),
                        "and" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Bool(true));
                            }
                            for expr in &args[..args.len() - 1] {
                                let result = eval(expr, &cur_env, out)?;
                                if !result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "or" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Bool(false));
                            }
                            for expr in &args[..args.len() - 1] {
                                let result = eval(expr, &cur_env, out)?;
                                if result.is_truthy() {
                                    return Ok(result);
                                }
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "let" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity {
                                    msg: "let requires bindings and body".into(),
                                    pos: p,
                                });
                            }
                            // Named let: (let name ((var init) ...) body...)
                            if let ExprKind::Symbol(name) = &args[0].kind {
                                let name = name.clone();
                                if args.len() < 3 {
                                    return Err(EvalError::Arity {
                                        msg: "named let requires bindings and body".into(),
                                        pos: p,
                                    });
                                }
                                let bindings_expr = match &args[1].kind {
                                    ExprKind::List(b) => b,
                                    _ => return Err(EvalError::Parse {
                                        msg: "let: expected bindings list".into(),
                                        pos: p,
                                    }),
                                };
                                let mut param_names = Vec::new();
                                let mut init_vals = Vec::new();
                                for binding in bindings_expr {
                                    match &binding.kind {
                                        ExprKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                ExprKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::Parse {
                                                    msg: "let: expected symbol".into(),
                                                    pos: pair[0].pos,
                                                }),
                                            };
                                            let val = eval(&pair[1], &cur_env, out)?;
                                            param_names.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(EvalError::Parse {
                                            msg: "let: invalid binding".into(),
                                            pos: binding.pos,
                                        }),
                                    }
                                }
                                let body = args[2..].to_vec();
                                // Create env where the named function closes over itself
                                let fn_env = new_env(Some(cur_env.clone()));
                                let lambda = Val::Lambda {
                                    params: param_names.clone(),
                                    rest_param: None,
                                    body: body.clone(),
                                    env: fn_env.clone(),
                                };
                                env_set(&fn_env, name, lambda);

                                // Set up call env with initial bindings
                                let call_env = new_env(Some(fn_env));
                                for (pn, val) in param_names.iter().zip(init_vals.iter()) {
                                    env_set(&call_env, pn.clone(), val.clone());
                                }

                                for expr in &body[..body.len() - 1] {
                                    eval(expr, &call_env, out)?;
                                }
                                cur_expr = body[body.len() - 1].clone();
                                cur_env = call_env;
                                continue;
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => {
                                    return Err(EvalError::Parse {
                                        msg: "let: expected bindings list".into(),
                                        pos: p,
                                    })
                                }
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => {
                                                return Err(EvalError::Parse {
                                                    msg: "let: expected symbol".into(),
                                                    pos: pair[0].pos,
                                                })
                                            }
                                        };
                                        let val = eval(&pair[1], &cur_env, out)?;
                                        env_set(&local_env, bname, val);
                                    }
                                    _ => {
                                        return Err(EvalError::Parse {
                                            msg: "let: invalid binding".into(),
                                            pos: binding.pos,
                                        })
                                    }
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body[body.len() - 1].clone();
                            cur_env = local_env;
                            continue;
                        }
                        "letrec" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity {
                                    msg: "letrec requires bindings and body".into(),
                                    pos: p,
                                });
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Parse {
                                    msg: "letrec: expected bindings list".into(),
                                    pos: p,
                                }),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            // First pass: bind all names to Void
                            let mut names = Vec::new();
                            let mut init_exprs = Vec::new();
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Parse {
                                                msg: "letrec: expected symbol".into(),
                                                pos: pair[0].pos,
                                            }),
                                        };
                                        env_set(&local_env, bname.clone(), Val::Void);
                                        names.push(bname);
                                        init_exprs.push(&pair[1]);
                                    }
                                    _ => return Err(EvalError::Parse {
                                        msg: "letrec: invalid binding".into(),
                                        pos: binding.pos,
                                    }),
                                }
                            }
                            // Second pass: evaluate inits in local_env and assign
                            for (name, init_expr) in names.iter().zip(init_exprs.iter()) {
                                let val = eval(init_expr, &local_env, out)?;
                                env_set(&local_env, name.clone(), val);
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body[body.len() - 1].clone();
                            cur_env = local_env;
                            continue;
                        }
                        "letrec*" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity {
                                    msg: "letrec* requires bindings and body".into(),
                                    pos: p,
                                });
                            }
                            let bindings = match &args[0].kind {
                                ExprKind::List(b) => b,
                                _ => return Err(EvalError::Parse {
                                    msg: "letrec*: expected bindings list".into(),
                                    pos: p,
                                }),
                            };
                            let local_env = new_env(Some(cur_env.clone()));
                            for binding in bindings {
                                match &binding.kind {
                                    ExprKind::List(pair) if pair.len() == 2 => {
                                        let bname = match &pair[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Parse {
                                                msg: "letrec*: expected symbol".into(),
                                                pos: pair[0].pos,
                                            }),
                                        };
                                        let val = eval(&pair[1], &local_env, out)?;
                                        env_set(&local_env, bname, val);
                                    }
                                    _ => return Err(EvalError::Parse {
                                        msg: "letrec*: invalid binding".into(),
                                        pos: binding.pos,
                                    }),
                                }
                            }
                            let body = &args[1..];
                            for expr in &body[..body.len() - 1] {
                                eval(expr, &local_env, out)?;
                            }
                            cur_expr = body[body.len() - 1].clone();
                            cur_env = local_env;
                            continue;
                        }
                        "case" => {
                            let args = &elems[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity {
                                    msg: "case requires key and at least one clause".into(),
                                    pos: p,
                                });
                            }
                            let key = eval(&args[0], &cur_env, out)?;
                            let clauses = &args[1..];
                            let mut found = false;
                            for clause in clauses {
                                let parts = match &clause.kind {
                                    ExprKind::List(p) => p,
                                    _ => return Err(EvalError::Parse {
                                        msg: "case: expected clause list".into(),
                                        pos: clause.pos,
                                    }),
                                };
                                if parts.is_empty() {
                                    return Err(EvalError::Parse {
                                        msg: "case: empty clause".into(),
                                        pos: clause.pos,
                                    });
                                }
                                // Check for else clause
                                if let ExprKind::Symbol(s) = &parts[0].kind {
                                    if s == "else" {
                                        for expr in &parts[1..parts.len() - 1] {
                                            eval(expr, &cur_env, out)?;
                                        }
                                        if parts.len() > 1 {
                                            cur_expr = parts[parts.len() - 1].clone();
                                        } else {
                                            return Ok(Val::Void);
                                        }
                                        found = true;
                                        break;
                                    }
                                }
                                // Match datums list
                                let datums = match &parts[0].kind {
                                    ExprKind::List(d) => d,
                                    _ => return Err(EvalError::Parse {
                                        msg: "case: expected datum list".into(),
                                        pos: parts[0].pos,
                                    }),
                                };
                                let mut matched = false;
                                for datum in datums {
                                    let datum_val = expr_to_val(datum)?;
                                    if eqv_vals(&key, &datum_val) {
                                        matched = true;
                                        break;
                                    }
                                }
                                if matched {
                                    for expr in &parts[1..parts.len() - 1] {
                                        eval(expr, &cur_env, out)?;
                                    }
                                    if parts.len() > 1 {
                                        cur_expr = parts[parts.len() - 1].clone();
                                    } else {
                                        return Ok(Val::Void);
                                    }
                                    found = true;
                                    break;
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Val::Void);
                        }
                        "begin" => {
                            let args = &elems[1..];
                            if args.is_empty() {
                                return Ok(Val::Void);
                            }
                            for expr in &args[..args.len() - 1] {
                                eval(expr, &cur_env, out)?;
                            }
                            cur_expr = args[args.len() - 1].clone();
                            continue;
                        }
                        "cond" => {
                            let clauses = &elems[1..];
                            let mut found = false;
                            for clause in clauses {
                                let parts = match &clause.kind {
                                    ExprKind::List(p) => p,
                                    _ => {
                                        return Err(EvalError::Parse {
                                            msg: "cond: expected clause list".into(),
                                            pos: clause.pos,
                                        })
                                    }
                                };
                                if parts.is_empty() {
                                    return Err(EvalError::Parse {
                                        msg: "cond: empty clause".into(),
                                        pos: clause.pos,
                                    });
                                }
                                if let ExprKind::Symbol(s) = &parts[0].kind {
                                    if s == "else" {
                                        for expr in &parts[1..parts.len() - 1] {
                                            eval(expr, &cur_env, out)?;
                                        }
                                        if parts.len() > 1 {
                                            cur_expr = parts[parts.len() - 1].clone();
                                        } else {
                                            return Ok(Val::Void);
                                        }
                                        found = true;
                                        break;
                                    }
                                }
                                let test = eval(&parts[0], &cur_env, out)?;
                                if test.is_truthy() {
                                    if parts.len() == 1 {
                                        return Ok(test);
                                    }
                                    for expr in &parts[1..parts.len() - 1] {
                                        eval(expr, &cur_env, out)?;
                                    }
                                    cur_expr = parts[parts.len() - 1].clone();
                                    found = true;
                                    break;
                                }
                            }
                            if found {
                                continue;
                            }
                            return Ok(Val::Void);
                        }
                        "string-set!" => return eval_string_set(&elems[1..], &cur_env, p, out),
                        "guard" => {
                            // (guard (var clause ...) body ...)
                            if elems.len() < 3 {
                                return Err(EvalError::Arity {
                                    msg: "guard requires variable/clauses and body".into(),
                                    pos: p,
                                });
                            }
                            let guard_spec = match &elems[1].kind {
                                ExprKind::List(parts) => parts,
                                _ => return Err(EvalError::Parse {
                                    msg: "guard: expected (var clause ...) list".into(),
                                    pos: elems[1].pos,
                                }),
                            };
                            if guard_spec.is_empty() {
                                return Err(EvalError::Parse {
                                    msg: "guard: expected variable name".into(),
                                    pos: elems[1].pos,
                                });
                            }
                            let var_name = match &guard_spec[0].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Parse {
                                    msg: "guard: expected symbol as variable".into(),
                                    pos: guard_spec[0].pos,
                                }),
                            };
                            let clauses = &guard_spec[1..];
                            let body = &elems[2..];

                            // Evaluate body expressions, catching raised exceptions
                            let body_result = (|| -> Result<Val, EvalError> {
                                let mut res = Val::Void;
                                for expr in body {
                                    res = eval(expr, &cur_env, out)?;
                                }
                                Ok(res)
                            })();

                            match body_result {
                                Ok(val) => return Ok(val),
                                Err(EvalError::RaisedException) => {
                                    let exn_val = EXCEPTION_VAL.with(|ev| ev.borrow_mut().take())
                                        .unwrap_or(Val::Void);
                                    // Bind var to exception value and evaluate clauses
                                    let guard_env = new_env(Some(cur_env.clone()));
                                    env_set(&guard_env, var_name.clone(), exn_val.clone());
                                    let mut matched = false;
                                    for clause in clauses {
                                        let parts = match &clause.kind {
                                            ExprKind::List(p) => p,
                                            _ => return Err(EvalError::Parse {
                                                msg: "guard: expected clause list".into(),
                                                pos: clause.pos,
                                            }),
                                        };
                                        if parts.is_empty() {
                                            return Err(EvalError::Parse {
                                                msg: "guard: empty clause".into(),
                                                pos: clause.pos,
                                            });
                                        }
                                        // Check for else clause
                                        if let ExprKind::Symbol(s) = &parts[0].kind {
                                            if s == "else" {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, &guard_env, out)?;
                                                }
                                                if parts.len() > 1 {
                                                    cur_expr = parts[parts.len() - 1].clone();
                                                    cur_env = guard_env;
                                                    matched = true;
                                                    break;
                                                } else {
                                                    return Ok(Val::Void);
                                                }
                                            }
                                        }
                                        let test = eval(&parts[0], &guard_env, out)?;
                                        if test.is_truthy() {
                                            if parts.len() == 1 {
                                                return Ok(test);
                                            }
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, &guard_env, out)?;
                                            }
                                            cur_expr = parts[parts.len() - 1].clone();
                                            cur_env = guard_env;
                                            matched = true;
                                            break;
                                        }
                                    }
                                    if matched {
                                        continue;
                                    }
                                    // No clause matched, re-raise
                                    EXCEPTION_VAL.with(|ev| *ev.borrow_mut() = Some(exn_val));
                                    return Err(EvalError::RaisedException);
                                }
                                Err(e) => return Err(e),
                            }
                        }
                        "define-syntax" => {
                            if elems.len() != 3 {
                                return Err(EvalError::Arity {
                                    msg: "define-syntax requires 2 arguments".into(),
                                    pos: p,
                                });
                            }
                            let mac_name = match &elems[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Parse {
                                    msg: "define-syntax: expected symbol".into(),
                                    pos: elems[1].pos,
                                }),
                            };
                            let sr = match &elems[2].kind {
                                ExprKind::List(parts) => parts,
                                _ => return Err(EvalError::Parse {
                                    msg: "define-syntax: expected syntax-rules".into(),
                                    pos: elems[2].pos,
                                }),
                            };
                            if sr.len() < 2 {
                                return Err(EvalError::Parse {
                                    msg: "syntax-rules: need literals and at least one rule".into(),
                                    pos: elems[2].pos,
                                });
                            }
                            match &sr[0].kind {
                                ExprKind::Symbol(s) if s == "syntax-rules" => {}
                                _ => return Err(EvalError::Parse {
                                    msg: "define-syntax: expected syntax-rules".into(),
                                    pos: sr[0].pos,
                                }),
                            }
                            let literals = match &sr[1].kind {
                                ExprKind::List(lits) => {
                                    let mut v = Vec::new();
                                    for l in lits {
                                        match &l.kind {
                                            ExprKind::Symbol(s) => v.push(s.clone()),
                                            _ => return Err(EvalError::Parse {
                                                msg: "syntax-rules: expected symbol in literals".into(),
                                                pos: l.pos,
                                            }),
                                        }
                                    }
                                    v
                                }
                                _ => return Err(EvalError::Parse {
                                    msg: "syntax-rules: expected literals list".into(),
                                    pos: sr[1].pos,
                                }),
                            };
                            let mut rules = Vec::new();
                            for rule in &sr[2..] {
                                match &rule.kind {
                                    ExprKind::List(parts) if parts.len() == 2 => {
                                        rules.push((parts[0].clone(), parts[1].clone()));
                                    }
                                    _ => return Err(EvalError::Parse {
                                        msg: "syntax-rules: expected (pattern template)".into(),
                                        pos: rule.pos,
                                    }),
                                }
                            }
                            env_set(
                                &cur_env,
                                mac_name,
                                Val::Macro {
                                    literals,
                                    rules,
                                    def_env: cur_env.clone(),
                                },
                            );
                            return Ok(Val::Void);
                        }
                        "define-record-type" => {
                            // (define-record-type <name> (constructor field-names...) predicate (field accessor)...)
                            if elems.len() < 4 {
                                return Err(EvalError::Arity {
                                    msg: "define-record-type requires at least 3 arguments".into(),
                                    pos: p,
                                });
                            }
                            let type_name = match &elems[1].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Parse {
                                    msg: "define-record-type: expected type name".into(),
                                    pos: elems[1].pos,
                                }),
                            };
                            let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

                            // Parse constructor: (make-foo field1 field2 ...)
                            let (ctor_name, ctor_fields) = match &elems[2].kind {
                                ExprKind::List(parts) if !parts.is_empty() => {
                                    let name = match &parts[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Parse {
                                            msg: "define-record-type: expected constructor name".into(),
                                            pos: parts[0].pos,
                                        }),
                                    };
                                    let fields: Vec<String> = parts[1..].iter().map(|f| {
                                        match &f.kind {
                                            ExprKind::Symbol(s) => Ok(s.clone()),
                                            _ => Err(EvalError::Parse {
                                                msg: "define-record-type: expected field name".into(),
                                                pos: f.pos,
                                            }),
                                        }
                                    }).collect::<Result<_, _>>()?;
                                    (name, fields)
                                }
                                _ => return Err(EvalError::Parse {
                                    msg: "define-record-type: expected constructor".into(),
                                    pos: elems[2].pos,
                                }),
                            };

                            // Parse predicate name
                            let pred_name = match &elems[3].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Parse {
                                    msg: "define-record-type: expected predicate name".into(),
                                    pos: elems[3].pos,
                                }),
                            };

                            // Parse field specs: (field-name accessor-name)
                            let mut field_accessors: Vec<(String, String)> = Vec::new();
                            for spec in &elems[4..] {
                                match &spec.kind {
                                    ExprKind::List(parts) if parts.len() >= 2 => {
                                        let field = match &parts[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Parse {
                                                msg: "define-record-type: expected field name".into(),
                                                pos: parts[0].pos,
                                            }),
                                        };
                                        let accessor = match &parts[1].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::Parse {
                                                msg: "define-record-type: expected accessor name".into(),
                                                pos: parts[1].pos,
                                            }),
                                        };
                                        field_accessors.push((field, accessor));
                                    }
                                    _ => return Err(EvalError::Parse {
                                        msg: "define-record-type: expected field spec".into(),
                                        pos: spec.pos,
                                    }),
                                }
                            }

                            // Define constructor: encode type_name and field names in tag
                            // Format: __record_ctor_{type_id}:{type_name}:{field1},{field2},...
                            let fields_str = ctor_fields.join(",");
                            let ctor_tag = format!("__record_ctor_{}:{}:{}", type_id, type_name, fields_str);
                            env_set(&cur_env, ctor_name.clone(), Val::Builtin(ctor_tag));

                            // Define predicate
                            let pred_tag = format!("__record_pred_{}", type_id);
                            env_set(&cur_env, pred_name, Val::Builtin(pred_tag));

                            // Define accessors
                            for (field_name, accessor_name) in &field_accessors {
                                let acc_tag = format!("__record_acc_{}_{}", type_id, field_name);
                                env_set(&cur_env, accessor_name.clone(), Val::Builtin(acc_tag));
                            }

                            return Ok(Val::Void);
                        }
                        _ => {
                            // Check if it's a macro call
                            let op_name = op.clone();
                            if let Some(Val::Macro {
                                literals,
                                rules,
                                def_env,
                            }) = env_get(&cur_env, &op_name)
                            {
                                let input_args: Vec<Expr> = elems[1..].to_vec();
                                for (pattern, template) in &rules {
                                    let pat_elems = match &pattern.kind {
                                        ExprKind::List(e) => e,
                                        _ => continue,
                                    };
                                    if let Some(bindings) =
                                        macro_match(&pat_elems[1..], &input_args, &literals)
                                    {
                                        let mut pattern_vars = HashSet::new();
                                        for pe in &pat_elems[1..] {
                                            collect_pattern_vars(pe, &literals, &mut pattern_vars);
                                        }
                                        let mut free_vars = HashSet::new();
                                        collect_free_vars(
                                            template,
                                            &pattern_vars,
                                            &mut free_vars,
                                        );
                                        let mut renames = HashMap::new();
                                        for fv in &free_vars {
                                            renames.insert(fv.clone(), gensym(fv));
                                        }
                                        for (original, renamed) in &renames {
                                            if let Some(val) = env_get(&def_env, original) {
                                                env_set(&cur_env, renamed.clone(), val);
                                            }
                                        }
                                        let expanded =
                                            expand_template(template, &bindings, &renames);
                                        cur_expr = expanded;
                                        continue 'tco;
                                    }
                                }
                                return Err(EvalError::Runtime {
                                    msg: format!(
                                        "no matching pattern for macro {}",
                                        op_name
                                    ),
                                    pos: p,
                                });
                            }
                        }
                    }
                }

                // Function call
                let func = eval(&elems[0], &cur_env, out)?;
                let args: Vec<Val> = elems[1..]
                    .iter()
                    .map(|e| eval(e, &cur_env, out))
                    .collect::<Result<Vec<_>, _>>()?;

                match func {
                    Val::Builtin(ref name) if name == "call/cc" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity {
                                msg: "call/cc requires 1 argument".into(),
                                pos: p,
                            });
                        }
                        return callcc_exec(&args[0], p, out);
                    }
                    Val::Continuation(id) => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity {
                                msg: "continuation requires 1 argument".into(),
                                pos: p,
                            });
                        }
                        return Err(invoke_continuation(id, args[0].clone()));
                    }
                    Val::Builtin(name) => return apply_builtin(&name, &args, p, out),
                    Val::Lambda {
                        params,
                        rest_param,
                        body,
                        env: closure_env,
                    } => {
                        if rest_param.is_some() {
                            if args.len() < params.len() {
                                return Err(EvalError::Arity {
                                    msg: format!("expected at least {} arguments, got {}", params.len(), args.len()),
                                    pos: p,
                                });
                            }
                        } else if args.len() != params.len() {
                            return Err(EvalError::Arity {
                                msg: format!("expected {} arguments, got {}", params.len(), args.len()),
                                pos: p,
                            });
                        }
                        let local_env = new_env(Some(closure_env.clone()));
                        for (param, arg) in params.iter().zip(args.iter()) {
                            env_set(&local_env, param.clone(), arg.clone());
                        }
                        if let Some(ref rp) = rest_param {
                            let rest = args[params.len()..].to_vec();
                            env_set(&local_env, rp.clone(), Val::List(rest));
                        }
                        for expr in &body[..body.len().saturating_sub(1)] {
                            eval(expr, &local_env, out)?;
                        }
                        if body.is_empty() {
                            return Ok(Val::Void);
                        }
                        cur_expr = body[body.len() - 1].clone();
                        cur_env = local_env;
                        continue;
                    }
                    _ => {
                        return Err(EvalError::Type {
                            msg: format!("not a procedure: {}", func),
                            pos: p,
                        });
                    }
                }
            }
        }
    }
}

fn apply_func(func: &Val, args: &[Val], pos: Pos, out: &Output) -> Result<Val, EvalError> {
    match func {
        Val::Builtin(name) if name == "call/cc" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "call/cc requires 1 argument".into(),
                    pos,
                });
            }
            callcc_exec(&args[0], pos, out)
        }
        Val::Continuation(id) => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "continuation requires 1 argument".into(),
                    pos,
                });
            }
            Err(invoke_continuation(*id, args[0].clone()))
        }
        Val::Builtin(name) => apply_builtin(name, args, pos, out),
        Val::Lambda {
            params,
            rest_param,
            body,
            env: closure_env,
        } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity {
                        msg: format!("expected at least {} arguments, got {}", params.len(), args.len()),
                        pos,
                    });
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity {
                    msg: format!("expected {} arguments, got {}", params.len(), args.len()),
                    pos,
                });
            }
            let local_env = new_env(Some(closure_env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            if let Some(rp) = rest_param {
                let rest = args[params.len()..].to_vec();
                env_set(&local_env, rp.clone(), Val::List(rest));
            }
            let mut result = Val::Void;
            for expr in body {
                result = eval(expr, &local_env, out)?;
            }
            Ok(result)
        }
        _ => Err(EvalError::Type {
            msg: format!("not a procedure: {}", func),
            pos,
        }),
    }
}

fn eval_define(args: &[Expr], env: &Env, pos: Pos, out: &Output) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            msg: "define requires at least 2 arguments".into(),
            pos,
        });
    }
    match &args[0].kind {
        // (define x expr)
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    msg: "define requires 2 arguments".into(),
                    pos,
                });
            }
            let val = eval(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Val::Void)
        }
        // (define (f params...) body...)
        ExprKind::List(name_and_params) => {
            if name_and_params.is_empty() {
                return Err(EvalError::Parse {
                    msg: "define: empty name list".into(),
                    pos,
                });
            }
            let name = match &name_and_params[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Parse {
                        msg: "define: expected symbol for name".into(),
                        pos,
                    })
                }
            };
            let (params, rest_param) = parse_params(&name_and_params[1..], pos)?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse {
            msg: "define: expected symbol or list".into(),
            pos,
        }),
    }
}

fn eval_quote(args: &[Expr], pos: Pos) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            msg: "quote requires 1 argument".into(),
            pos,
        });
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Float(f) => Ok(Val::Float(*f)),
        ExprKind::Rational(n, d) => Ok(make_rational(*n, *d)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems
                .iter()
                .map(expr_to_val)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn parse_params(param_exprs: &[Expr], pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse {
                        msg: "expected rest parameter after dot".into(),
                        pos,
                    });
                }
                rest_param = Some(match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse {
                        msg: "expected symbol for rest parameter".into(),
                        pos: param_exprs[i + 1].pos,
                    }),
                });
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse {
                msg: "expected symbol for parameter".into(),
                pos: param_exprs[i].pos,
            }),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &Env, pos: Pos) -> Result<Val, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            msg: "lambda requires at least 2 arguments".into(),
            pos,
        });
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, pos)?,
        _ => {
            return Err(EvalError::Parse {
                msg: "lambda: expected parameter list".into(),
                pos,
            })
        }
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_string_set(_args: &[Expr], _env: &Env, pos: Pos, _out: &Output) -> Result<Val, EvalError> {
    Err(EvalError::Runtime {
        msg: "string-set!: strings are immutable".into(),
        pos,
    })
}

fn apply_builtin(name: &str, args: &[Val], pos: Pos, out: &Output) -> Result<Val, EvalError> {
    match name {
        "+" => {
            for a in args { as_num(a, pos)?; }
            if any_inexact(args) {
                let mut sum = 0.0f64;
                for a in args { sum += num_to_f64(a); }
                Ok(Val::Float(sum))
            } else {
                let mut result: Val = Val::Int(0);
                for a in args { result = exact_add(&result, a); }
                Ok(result)
            }
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    msg: "- requires at least 1 argument".into(),
                    pos,
                });
            }
            for a in args { as_num(a, pos)?; }
            if any_inexact(args) {
                if args.len() == 1 {
                    return Ok(Val::Float(-num_to_f64(&args[0])));
                }
                let mut result = num_to_f64(&args[0]);
                for a in &args[1..] { result -= num_to_f64(a); }
                Ok(Val::Float(result))
            } else {
                if args.len() == 1 {
                    return Ok(exact_sub(&Val::Int(0), &args[0]));
                }
                let mut result = args[0].clone();
                for a in &args[1..] { result = exact_sub(&result, a); }
                Ok(result)
            }
        }
        "*" => {
            for a in args { as_num(a, pos)?; }
            if any_inexact(args) {
                let mut product = 1.0f64;
                for a in args { product *= num_to_f64(a); }
                Ok(Val::Float(product))
            } else {
                let mut result: Val = Val::Int(1);
                for a in args { result = exact_mul(&result, a); }
                Ok(result)
            }
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity {
                    msg: "/ requires at least 1 argument".into(),
                    pos,
                });
            }
            for a in args { as_num(a, pos)?; }
            if any_inexact(args) {
                let mut result = num_to_f64(&args[0]);
                if args.len() == 1 {
                    return Ok(Val::Float(1.0 / result));
                }
                for a in &args[1..] {
                    let d = num_to_f64(a);
                    if d == 0.0 {
                        return Err(EvalError::Runtime {
                            msg: "division by zero".into(),
                            pos,
                        });
                    }
                    result /= d;
                }
                Ok(Val::Float(result))
            } else {
                let mut result = args[0].clone();
                if args.len() == 1 {
                    return exact_div(&Val::Int(1), &result, pos);
                }
                for a in &args[1..] {
                    result = exact_div(&result, a, pos)?;
                }
                Ok(result)
            }
        }
        "<" => {
            if args.len() != 2 { return Err(EvalError::Arity { msg: "< requires 2 arguments".into(), pos }); }
            as_num(&args[0], pos)?; as_num(&args[1], pos)?;
            Ok(Val::Bool(num_cmp(&args[0], &args[1]) == std::cmp::Ordering::Less))
        }
        ">" => {
            if args.len() != 2 { return Err(EvalError::Arity { msg: "> requires 2 arguments".into(), pos }); }
            as_num(&args[0], pos)?; as_num(&args[1], pos)?;
            Ok(Val::Bool(num_cmp(&args[0], &args[1]) == std::cmp::Ordering::Greater))
        }
        "=" => {
            if args.len() != 2 { return Err(EvalError::Arity { msg: "= requires 2 arguments".into(), pos }); }
            as_num(&args[0], pos)?; as_num(&args[1], pos)?;
            Ok(Val::Bool(num_eq(&args[0], &args[1])))
        }
        "<=" => {
            if args.len() != 2 { return Err(EvalError::Arity { msg: "<= requires 2 arguments".into(), pos }); }
            as_num(&args[0], pos)?; as_num(&args[1], pos)?;
            Ok(Val::Bool(num_cmp(&args[0], &args[1]) != std::cmp::Ordering::Greater))
        }
        ">=" => {
            if args.len() != 2 { return Err(EvalError::Arity { msg: ">= requires 2 arguments".into(), pos }); }
            as_num(&args[0], pos)?; as_num(&args[1], pos)?;
            Ok(Val::Bool(num_cmp(&args[0], &args[1]) != std::cmp::Ordering::Less))
        }
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "not requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(!args[0].is_truthy()))
        }
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    msg: "cons requires 2 arguments".into(),
                    pos,
                });
            }
            match &args[1] {
                Val::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Val::List(new_list))
                }
                _ => {
                    // Improper pair
                    Ok(Val::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "car requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Val::Pair(a, _) => Ok(a.as_ref().clone()),
                _ => Err(EvalError::Type {
                    msg: "car: not a pair".into(),
                    pos,
                }),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "cdr requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) if !elems.is_empty() => Ok(Val::List(elems[1..].to_vec())),
                Val::Pair(_, b) => Ok(b.as_ref().clone()),
                _ => Err(EvalError::Type {
                    msg: "cdr: not a pair".into(),
                    pos,
                }),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "null? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if e.is_empty())))
        }
        "list" => Ok(Val::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "length requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::List(elems) => Ok(Val::Int(elems.len() as i64)),
                _ => Err(EvalError::Type {
                    msg: "length: not a list".into(),
                    pos,
                }),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "string? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Str(_))))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "number? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_) | Val::Float(_) | Val::Rational(_, _))))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "boolean? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Bool(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "pair? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(e) if !e.is_empty()) || matches!(&args[0], Val::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "symbol? requires 1 argument".into(),
                    pos,
                });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Symbol(_))))
        }
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "display requires 1 argument".into(), pos });
            }
            out.borrow_mut().push_str(&display_val(&args[0]));
            Ok(Val::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "write requires 1 argument".into(), pos });
            }
            out.borrow_mut().push_str(&args[0].to_string());
            Ok(Val::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity { msg: "newline takes 0 arguments".into(), pos });
            }
            out.borrow_mut().push('\n');
            Ok(Val::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Val::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type { msg: "string-append: expected string".into(), pos }),
                }
            }
            Ok(Val::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-length requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Int(s.len() as i64)),
                _ => Err(EvalError::Type { msg: "string-length: expected string".into(), pos }),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity { msg: "substring requires 3 arguments".into(), pos });
            }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type { msg: "substring: expected string".into(), pos }),
            };
            let start = as_int(&args[1], pos)? as usize;
            let end = as_int(&args[2], pos)? as usize;
            Ok(Val::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->number requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Val::Int(n)),
                    Err(_) => Ok(Val::Bool(false)),
                },
                _ => Err(EvalError::Type { msg: "string->number: expected string".into(), pos }),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "number->string requires 1 argument".into(), pos });
            }
            as_num(&args[0], pos)?;
            Ok(Val::Str(args[0].to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "symbol->string requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Symbol(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type { msg: "symbol->string: expected symbol".into(), pos }),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->symbol requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Symbol(s.clone())),
                _ => Err(EvalError::Type { msg: "string->symbol: expected string".into(), pos }),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "string-ref requires 2 arguments".into(), pos });
            }
            let s = match &args[0] {
                Val::Str(s) => s,
                _ => return Err(EvalError::Type { msg: "string-ref: expected string".into(), pos }),
            };
            let idx = as_int(&args[1], pos)? as usize;
            Ok(Val::Char(s.chars().nth(idx).ok_or_else(|| EvalError::Runtime {
                msg: "string-ref: index out of bounds".into(), pos,
            })?))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Char(_))))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-copy requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Str(s.clone())),
                _ => Err(EvalError::Type { msg: "string-copy: expected string".into(), pos }),
            }
        }
        "string->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string->list requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::List(s.chars().map(Val::Char).collect())),
                _ => Err(EvalError::Type { msg: "string->list: expected string".into(), pos }),
            }
        }
        "list->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "list->string requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => {
                    let mut s = String::new();
                    for e in elems {
                        match e {
                            Val::Char(c) => s.push(*c),
                            _ => return Err(EvalError::Type { msg: "list->string: expected list of characters".into(), pos }),
                        }
                    }
                    Ok(Val::Str(s))
                }
                _ => Err(EvalError::Type { msg: "list->string: expected list".into(), pos }),
            }
        }
        "char->integer" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char->integer requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Int(*c as i64)),
                _ => Err(EvalError::Type { msg: "char->integer: expected character".into(), pos }),
            }
        }
        "integer->char" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "integer->char requires 1 argument".into(), pos });
            }
            let n = as_int(&args[0], pos)?;
            Ok(Val::Char(char::from_u32(n as u32).ok_or_else(|| EvalError::Runtime {
                msg: format!("integer->char: invalid code point {}", n), pos,
            })?))
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity { msg: "map requires at least 2 arguments".into(), pos });
            }
            let func = &args[0];
            let mut lists: Vec<&Vec<Val>> = Vec::new();
            for a in &args[1..] {
                match a {
                    Val::List(elems) => lists.push(elems),
                    _ => return Err(EvalError::Type { msg: "map: expected list argument".into(), pos }),
                }
            }
            let len = lists[0].len();
            let mut results = Vec::with_capacity(len);
            for i in 0..len {
                let call_args: Vec<Val> = lists.iter().map(|l| l[i].clone()).collect();
                results.push(apply_func(func, &call_args, pos, out)?);
            }
            Ok(Val::List(results))
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity { msg: "apply requires at least 2 arguments".into(), pos });
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Val::List(elems) => elems.clone(),
                _ => return Err(EvalError::Type { msg: "apply: last argument must be a list".into(), pos }),
            };
            let mut all_args: Vec<Val> = args[1..args.len() - 1].to_vec();
            all_args.extend(tail);
            apply_func(func, &all_args, pos, out)
        }
        "dynamic-wind" => {
            if args.len() != 3 {
                return Err(EvalError::Arity { msg: "dynamic-wind requires 3 arguments".into(), pos });
            }
            let in_thunk = &args[0];
            let body_thunk = &args[1];
            let out_thunk = &args[2];
            // Run in-thunk
            apply_func(in_thunk, &[], pos, out)?;
            // Run body-thunk, catching non-local exit to ensure out-thunk runs
            let body_result = match apply_func(body_thunk, &[], pos, out) {
                Ok(val) => {
                    // Normal exit: run out-thunk, return body value
                    apply_func(out_thunk, &[], pos, out)?;
                    Ok(val)
                }
                Err(EvalError::ContinuationReturn) => {
                    // Non-local exit: run out-thunk, then re-raise
                    apply_func(out_thunk, &[], pos, out)?;
                    Err(EvalError::ContinuationReturn)
                }
                Err(EvalError::RaisedException) => {
                    // Exception: run out-thunk, then re-raise
                    apply_func(out_thunk, &[], pos, out)?;
                    Err(EvalError::RaisedException)
                }
                Err(e) => {
                    // Other error: run out-thunk, then re-raise
                    let _ = apply_func(out_thunk, &[], pos, out);
                    Err(e)
                }
            };
            body_result
        }
        "raise" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "raise requires 1 argument".into(), pos });
            }
            EXCEPTION_VAL.with(|ev| *ev.borrow_mut() = Some(args[0].clone()));
            Err(EvalError::RaisedException)
        }
        "with-exception-handler" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "with-exception-handler requires 2 arguments".into(), pos });
            }
            let handler = &args[0];
            let thunk = &args[1];
            match apply_func(thunk, &[], pos, out) {
                Ok(val) => Ok(val),
                Err(EvalError::RaisedException) => {
                    let exn_val = EXCEPTION_VAL.with(|ev| ev.borrow_mut().take())
                        .unwrap_or(Val::Void);
                    apply_func(handler, &[exn_val], pos, out)
                }
                Err(e) => Err(e),
            }
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "equal? requires 2 arguments".into(), pos });
            }
            Ok(Val::Bool(equal_vals(&args[0], &args[1])))
        }
        "eqv?" | "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: format!("{} requires 2 arguments", name), pos });
            }
            Ok(Val::Bool(eqv_vals(&args[0], &args[1])))
        }
        "vector" => {
            Ok(Val::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 {
                return Err(EvalError::Arity { msg: "make-vector requires 1 or 2 arguments".into(), pos });
            }
            let len = as_int(&args[0], pos)? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Val::Int(0) };
            Ok(Val::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "vector-ref requires 2 arguments".into(), pos });
            }
            match &args[0] {
                Val::Vector(v) => {
                    let idx = as_int(&args[1], pos)? as usize;
                    let elems = v.borrow();
                    if idx >= elems.len() {
                        return Err(EvalError::Runtime { msg: "vector-ref: index out of bounds".into(), pos });
                    }
                    Ok(elems[idx].clone())
                }
                _ => Err(EvalError::Type { msg: "vector-ref: expected vector".into(), pos }),
            }
        }
        "vector-set!" => {
            if args.len() != 3 {
                return Err(EvalError::Arity { msg: "vector-set! requires 3 arguments".into(), pos });
            }
            match &args[0] {
                Val::Vector(v) => {
                    let idx = as_int(&args[1], pos)? as usize;
                    let mut elems = v.borrow_mut();
                    if idx >= elems.len() {
                        return Err(EvalError::Runtime { msg: "vector-set!: index out of bounds".into(), pos });
                    }
                    elems[idx] = args[2].clone();
                    Ok(Val::Void)
                }
                _ => Err(EvalError::Type { msg: "vector-set!: expected vector".into(), pos }),
            }
        }
        "vector-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "vector-length requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Vector(v) => Ok(Val::Int(v.borrow().len() as i64)),
                _ => Err(EvalError::Type { msg: "vector-length: expected vector".into(), pos }),
            }
        }
        "vector?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "vector? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(matches!(&args[0], Val::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "vector->list requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Vector(v) => Ok(Val::List(v.borrow().clone())),
                _ => Err(EvalError::Type { msg: "vector->list: expected vector".into(), pos }),
            }
        }
        "list->vector" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "list->vector requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => Ok(Val::Vector(Rc::new(RefCell::new(elems.clone())))),
                _ => Err(EvalError::Type { msg: "list->vector: expected list".into(), pos }),
            }
        }
        // ===== Level 15: Numeric utilities =====
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "abs requires 1 argument".into(), pos });
            }
            Ok(Val::Int(as_int(&args[0], pos)?.abs()))
        }
        "modulo" => {
            let (a, b) = two_ints(args, "modulo", pos)?;
            let r = a % b;
            let result = if r != 0 && (r > 0) != (b > 0) { r + b } else { r };
            Ok(Val::Int(result))
        }
        "remainder" => {
            let (a, b) = two_ints(args, "remainder", pos)?;
            Ok(Val::Int(a % b))
        }
        "quotient" => {
            let (a, b) = two_ints(args, "quotient", pos)?;
            Ok(Val::Int(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity { msg: "min requires at least 1 argument".into(), pos });
            }
            let mut result = as_int(&args[0], pos)?;
            for a in &args[1..] {
                let n = as_int(a, pos)?;
                if n < result { result = n; }
            }
            Ok(Val::Int(result))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity { msg: "max requires at least 1 argument".into(), pos });
            }
            let mut result = as_int(&args[0], pos)?;
            for a in &args[1..] {
                let n = as_int(a, pos)?;
                if n > result { result = n; }
            }
            Ok(Val::Int(result))
        }
        "expt" => {
            let (base, exp) = two_ints(args, "expt", pos)?;
            Ok(Val::Int(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "zero? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(as_int(&args[0], pos)? == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "positive? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(as_int(&args[0], pos)? > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "negative? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(as_int(&args[0], pos)? < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "odd? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(as_int(&args[0], pos)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "even? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(as_int(&args[0], pos)? % 2 == 0))
        }
        // ===== Level 15: List utilities =====
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "list-ref requires 2 arguments".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => {
                    let idx = as_int(&args[1], pos)? as usize;
                    if idx >= elems.len() {
                        return Err(EvalError::Runtime { msg: "list-ref: index out of bounds".into(), pos });
                    }
                    Ok(elems[idx].clone())
                }
                _ => Err(EvalError::Type { msg: "list-ref: expected list".into(), pos }),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "list-tail requires 2 arguments".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => {
                    let idx = as_int(&args[1], pos)? as usize;
                    Ok(Val::List(elems[idx..].to_vec()))
                }
                _ => Err(EvalError::Type { msg: "list-tail: expected list".into(), pos }),
            }
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "list? requires 1 argument".into(), pos });
            }
            Ok(Val::Bool(matches!(&args[0], Val::List(_))))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "assoc requires 2 arguments".into(), pos });
            }
            let key = &args[0];
            match &args[1] {
                Val::List(elems) => {
                    for e in elems {
                        match e {
                            Val::List(pair) if !pair.is_empty() => {
                                if equal_vals(key, &pair[0]) {
                                    return Ok(e.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                    Ok(Val::Bool(false))
                }
                _ => Err(EvalError::Type { msg: "assoc: expected list".into(), pos }),
            }
        }
        "reverse" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "reverse requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::List(elems) => {
                    let mut rev = elems.clone();
                    rev.reverse();
                    Ok(Val::List(rev))
                }
                _ => Err(EvalError::Type { msg: "reverse: expected list".into(), pos }),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for (i, arg) in args.iter().enumerate() {
                if i == args.len() - 1 {
                    // Last argument can be any value (improper list tail)
                    match arg {
                        Val::List(elems) => result.extend(elems.iter().cloned()),
                        other => {
                            if result.is_empty() {
                                return Ok(other.clone());
                            }
                            // For now, just add as element (proper list)
                            result.push(other.clone());
                        }
                    }
                } else {
                    match arg {
                        Val::List(elems) => result.extend(elems.iter().cloned()),
                        _ => return Err(EvalError::Type { msg: "append: expected list".into(), pos }),
                    }
                }
            }
            Ok(Val::List(result))
        }
        // ===== Level 15: Character utilities =====
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char-alphabetic? requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Bool(c.is_alphabetic())),
                _ => Err(EvalError::Type { msg: "char-alphabetic?: expected character".into(), pos }),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char-numeric? requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Bool(c.is_ascii_digit())),
                _ => Err(EvalError::Type { msg: "char-numeric?: expected character".into(), pos }),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char-upcase requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type { msg: "char-upcase: expected character".into(), pos }),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "char-downcase requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Char(c) => Ok(Val::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type { msg: "char-downcase: expected character".into(), pos }),
            }
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "char=? requires 2 arguments".into(), pos });
            }
            match (&args[0], &args[1]) {
                (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a == b)),
                _ => Err(EvalError::Type { msg: "char=?: expected characters".into(), pos }),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "char<? requires 2 arguments".into(), pos });
            }
            match (&args[0], &args[1]) {
                (Val::Char(a), Val::Char(b)) => Ok(Val::Bool(a < b)),
                _ => Err(EvalError::Type { msg: "char<?: expected characters".into(), pos }),
            }
        }
        // ===== Level 15: String utilities =====
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "string=? requires 2 arguments".into(), pos });
            }
            match (&args[0], &args[1]) {
                (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a == b)),
                _ => Err(EvalError::Type { msg: "string=?: expected strings".into(), pos }),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "string<? requires 2 arguments".into(), pos });
            }
            match (&args[0], &args[1]) {
                (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a < b)),
                _ => Err(EvalError::Type { msg: "string<?: expected strings".into(), pos }),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "string-ci=? requires 2 arguments".into(), pos });
            }
            match (&args[0], &args[1]) {
                (Val::Str(a), Val::Str(b)) => Ok(Val::Bool(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type { msg: "string-ci=?: expected strings".into(), pos }),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-upcase requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Str(s.to_uppercase())),
                _ => Err(EvalError::Type { msg: "string-upcase: expected string".into(), pos }),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity { msg: "string-downcase requires 1 argument".into(), pos });
            }
            match &args[0] {
                Val::Str(s) => Ok(Val::Str(s.to_lowercase())),
                _ => Err(EvalError::Type { msg: "string-downcase: expected string".into(), pos }),
            }
        }
        "values" => {
            if args.len() == 1 {
                Ok(args[0].clone())
            } else {
                Ok(Val::Values(args.to_vec()))
            }
        }
        "call-with-values" => {
            if args.len() != 2 {
                return Err(EvalError::Arity { msg: "call-with-values requires 2 arguments".into(), pos });
            }
            let producer = &args[0];
            let consumer = &args[1];
            let produced = apply_func(producer, &[], pos, out)?;
            match produced {
                Val::Values(vals) => apply_func(consumer, &vals, pos, out),
                single => apply_func(consumer, &[single], pos, out),
            }
        }
        // ===== Level 19: Exact arithmetic & rationals =====
        "exact?" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "exact? requires 1 argument".into(), pos }); }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_) | Val::Rational(_, _))))
        }
        "inexact?" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "inexact? requires 1 argument".into(), pos }); }
            Ok(Val::Bool(matches!(&args[0], Val::Float(_))))
        }
        "rational?" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "rational? requires 1 argument".into(), pos }); }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_) | Val::Rational(_, _))))
        }
        "integer?" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "integer? requires 1 argument".into(), pos }); }
            Ok(Val::Bool(matches!(&args[0], Val::Int(_))))
        }
        "exact->inexact" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "exact->inexact requires 1 argument".into(), pos }); }
            Ok(Val::Float(num_to_f64(&args[0])))
        }
        "inexact->exact" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "inexact->exact requires 1 argument".into(), pos }); }
            match &args[0] {
                Val::Int(_) => Ok(args[0].clone()),
                Val::Rational(_, _) => Ok(args[0].clone()),
                Val::Float(f) => {
                    // Convert float to rational using continued fraction approximation
                    // For simple cases like 0.5 -> 1/2
                    let (n, d) = float_to_rational(*f);
                    Ok(make_rational(n, d))
                }
                _ => Err(EvalError::Type { msg: "inexact->exact: expected number".into(), pos }),
            }
        }
        "numerator" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "numerator requires 1 argument".into(), pos }); }
            match &args[0] {
                Val::Int(n) => Ok(Val::Int(*n)),
                Val::Rational(n, _) => Ok(Val::Int(*n)),
                _ => Err(EvalError::Type { msg: "numerator: expected rational".into(), pos }),
            }
        }
        "denominator" => {
            if args.len() != 1 { return Err(EvalError::Arity { msg: "denominator requires 1 argument".into(), pos }); }
            match &args[0] {
                Val::Int(_) => Ok(Val::Int(1)),
                Val::Rational(_, d) => Ok(Val::Int(*d)),
                _ => Err(EvalError::Type { msg: "denominator: expected rational".into(), pos }),
            }
        }
        _ if name.starts_with("__record_ctor_") => {
            // Format: __record_ctor_{type_id}:{type_name}:{field1},{field2},...
            let rest = &name["__record_ctor_".len()..];
            let parts: Vec<&str> = rest.splitn(3, ':').collect();
            let tid: u64 = parts[0].parse().unwrap();
            let type_name = parts[1].to_string();
            let field_names: Vec<&str> = if parts[2].is_empty() {
                vec![]
            } else {
                parts[2].split(',').collect()
            };
            if args.len() != field_names.len() {
                return Err(EvalError::Arity {
                    msg: format!("record constructor expects {} arguments, got {}", field_names.len(), args.len()),
                    pos,
                });
            }
            let fields: Vec<(String, Val)> = field_names.iter()
                .zip(args.iter())
                .map(|(name, val)| (name.to_string(), val.clone()))
                .collect();
            Ok(Val::Record { type_id: tid, type_name, fields })
        }
        _ if name.starts_with("__record_pred_") => {
            let tid: u64 = name["__record_pred_".len()..].parse().unwrap();
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "record predicate requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::Record { type_id, .. } if *type_id == tid => Ok(Val::Bool(true)),
                _ => Ok(Val::Bool(false)),
            }
        }
        _ if name.starts_with("__record_acc_") => {
            let rest = &name["__record_acc_".len()..];
            let underscore_pos = rest.find('_').unwrap();
            let tid: u64 = rest[..underscore_pos].parse().unwrap();
            let field_name = &rest[underscore_pos + 1..];
            if args.len() != 1 {
                return Err(EvalError::Arity {
                    msg: "record accessor requires 1 argument".into(),
                    pos,
                });
            }
            match &args[0] {
                Val::Record { type_id, fields, type_name } if *type_id == tid => {
                    for (fname, val) in fields {
                        if fname == field_name {
                            return Ok(val.clone());
                        }
                    }
                    Err(EvalError::Runtime {
                        msg: format!("record {} has no field {}", type_name, field_name),
                        pos,
                    })
                }
                _ => Err(EvalError::Type {
                    msg: format!("accessor: expected record of correct type, got {}", args[0]),
                    pos,
                }),
            }
        }
        _ => Err(EvalError::UnboundVariable {
            name: name.into(),
            pos,
        }),
    }
}

fn as_int(v: &Val, pos: Pos) -> Result<i64, EvalError> {
    match v {
        Val::Int(n) => Ok(*n),
        Val::Rational(n, d) if *d == 1 => Ok(*n),
        _ => Err(EvalError::Type {
            msg: format!("expected integer, got {}", v),
            pos,
        }),
    }
}

fn as_num(v: &Val, pos: Pos) -> Result<&Val, EvalError> {
    match v {
        Val::Int(_) | Val::Float(_) | Val::Rational(_, _) => Ok(v),
        _ => Err(EvalError::Type {
            msg: format!("expected number, got {}", v),
            pos,
        }),
    }
}

fn two_ints(args: &[Val], op: &str, pos: Pos) -> Result<(i64, i64), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity {
            msg: format!("{} requires 2 arguments", op),
            pos,
        });
    }
    Ok((as_int(&args[0], pos)?, as_int(&args[1], pos)?))
}

/// Check if any argument is inexact (float)
fn any_inexact(args: &[Val]) -> bool {
    args.iter().any(|a| matches!(a, Val::Float(_)))
}

/// Check if any argument is a rational (non-integer exact)
fn any_rational(args: &[Val]) -> bool {
    args.iter().any(|a| matches!(a, Val::Rational(_, _)))
}

/// Add two exact values (Int or Rational), returns exact result
fn exact_add(a: &Val, b: &Val) -> Val {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => Val::Int(x + y),
        (Val::Int(x), Val::Rational(n, d)) | (Val::Rational(n, d), Val::Int(x)) => {
            make_rational(x * d + n, *d)
        }
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => {
            make_rational(n1 * d2 + n2 * d1, d1 * d2)
        }
        _ => unreachable!(),
    }
}

/// Subtract: a - b for exact values
fn exact_sub(a: &Val, b: &Val) -> Val {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => Val::Int(x - y),
        (Val::Int(x), Val::Rational(n, d)) => make_rational(x * d - n, *d),
        (Val::Rational(n, d), Val::Int(x)) => make_rational(n - x * d, *d),
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => {
            make_rational(n1 * d2 - n2 * d1, d1 * d2)
        }
        _ => unreachable!(),
    }
}

/// Multiply two exact values
fn exact_mul(a: &Val, b: &Val) -> Val {
    match (a, b) {
        (Val::Int(x), Val::Int(y)) => Val::Int(x * y),
        (Val::Int(x), Val::Rational(n, d)) | (Val::Rational(n, d), Val::Int(x)) => {
            make_rational(x * n, *d)
        }
        (Val::Rational(n1, d1), Val::Rational(n2, d2)) => {
            make_rational(n1 * n2, d1 * d2)
        }
        _ => unreachable!(),
    }
}

/// Divide two exact values, returns exact rational
fn exact_div(a: &Val, b: &Val, pos: Pos) -> Result<Val, EvalError> {
    let (an, ad) = match a {
        Val::Int(x) => (*x, 1i64),
        Val::Rational(n, d) => (*n, *d),
        _ => unreachable!(),
    };
    let (bn, bd) = match b {
        Val::Int(x) => (*x, 1i64),
        Val::Rational(n, d) => (*n, *d),
        _ => unreachable!(),
    };
    if bn == 0 {
        return Err(EvalError::Runtime {
            msg: "division by zero".into(),
            pos,
        });
    }
    Ok(make_rational(an * bd, ad * bn))
}

fn num_to_f64(v: &Val) -> f64 {
    match v {
        Val::Int(n) => *n as f64,
        Val::Float(f) => *f,
        Val::Rational(n, d) => *n as f64 / *d as f64,
        _ => unreachable!(),
    }
}

fn num_cmp(a: &Val, b: &Val) -> std::cmp::Ordering {
    let af = num_to_f64(a);
    let bf = num_to_f64(b);
    af.partial_cmp(&bf).unwrap_or(std::cmp::Ordering::Equal)
}

fn num_eq(a: &Val, b: &Val) -> bool {
    // Cross-tower: compare as f64
    num_to_f64(a) == num_to_f64(b)
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 {
        return (0, 1);
    }
    let sign = if f < 0.0 { -1 } else { 1 };
    let f = f.abs();
    // Use continued fraction approximation
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
        if frac.abs() < 1e-15 {
            break;
        }
        x = 1.0 / frac;
    }
    (sign * p1, q1)
}

fn eval_program(exprs: &[Expr], env: &Env, out: &Output) -> Result<Val, EvalError> {
    CONT_STATE.with(|cs| *cs.borrow_mut() = ContState::new());

    let mut i = 0;
    let mut result = Val::Void;

    while i < exprs.len() {
        CONT_STATE.with(|cs| {
            let mut state = cs.borrow_mut();
            state.current_expr_index = i;
            if state.expr_start_ids.len() <= i {
                let id = state.next_id;
                state.expr_start_ids.push(id);
                let pc = state.pos_counts.clone();
                state.expr_start_pos_counts.push(pc);
            }
        });

        match eval(&exprs[i], env, out) {
            Ok(val) => {
                result = val;
                i += 1;
            }
            Err(EvalError::ContinuationReturn) => {
                let (cont_id, cont_val) = CONT_STATE.with(|cs| {
                    cs.borrow_mut().signal.take().unwrap()
                });
                let restart_idx = CONT_STATE.with(|cs| {
                    cs.borrow().cont_expr_index[&cont_id]
                });
                let key = CONT_STATE.with(|cs| {
                    cs.borrow().id_to_key[&cont_id]
                });
                CONT_STATE.with(|cs| {
                    let mut state = cs.borrow_mut();
                    state.next_id = state.expr_start_ids[restart_idx];
                    state.pos_counts = state.expr_start_pos_counts[restart_idx].clone();
                    state.resume = Some((key, cont_val));
                });
                i = restart_idx;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(result)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let result = eval_program(&exprs, &env, &out)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    let env = default_env();
    let out: Output = Rc::new(RefCell::new(String::new()));
    let result = eval_program(&exprs, &env, &out)?;
    let output = out.borrow().clone();
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
