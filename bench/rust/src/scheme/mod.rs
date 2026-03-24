mod builtins;
mod cek;
pub mod error;
mod macros;
mod parser;
mod syntax_case;

pub use error::EvalError;
use error::Span;
use parser::{Parser, parse_params, parse_params_from_value};
use builtins::{apply_builtin, format_float_value, values_eqv};
pub(crate) use builtins::make_rational;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

pub(super) const DUMMY_SPAN: Span = Span { line: 0, col: 0 };

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);
static WIND_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(crate) type WindFrame = (Value, Value, u64); // in_thunk, out_thunk, marker

thread_local! {
    pub(super) static WIND_STACK: RefCell<Vec<WindFrame>> = const { RefCell::new(Vec::new()) };
    pub(super) static EXCEPTION_HANDLERS: RefCell<Vec<ExceptionHandler>> = const { RefCell::new(Vec::new()) };
    // syntax-case support: stack of pattern bindings and rename sink for hygiene
    pub(super) static SYNTAX_CASE_BINDINGS: RefCell<Vec<macros::Bindings>> = const { RefCell::new(Vec::new()) };
    pub(super) static SYNTAX_RENAME_SINK: RefCell<Vec<(String, Value)>> = const { RefCell::new(Vec::new()) };
    /// Step limit for eval_str_with_limit: None = unlimited, Some(n) = n steps remaining.
    pub(super) static STEP_LIMIT: RefCell<Option<u64>> = const { RefCell::new(None) };
}

#[derive(Clone)]
pub(crate) enum ExceptionHandler {
    Proc(Value),
    Guard {
        var: String,
        clauses: Vec<Spanned>,
        env: Env,
        kont: Rc<Kont>,
        winds: Vec<WindFrame>,
    },
}

pub(crate) fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__g{}", base, n)
}

pub(crate) type Output = Rc<RefCell<String>>;

/// A single lambda clause: (params, rest_param, body, closure_env).
pub(crate) type LambdaClause = (Vec<String>, Option<String>, Vec<Spanned>, Env);

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Spanned>),
    Pair(Rc<RefCell<(Value, Value)>>), // mutable cons cell
    Lambda(Vec<String>, Option<String>, Vec<Spanned>, Env), // params, rest_param, body, closure env
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<(Spanned, Spanned)>, // (pattern, template)
        def_env: Env,
    },
    CaseLambda(Vec<LambdaClause>), // clauses: (params, rest, body, env)
    Vector(Rc<RefCell<Vec<Value>>>),       // mutable fixed-size array
    Record(u64, Vec<Value>),              // type_id, field values
    RecordConstructor(u64, usize),        // type_id, field_count
    RecordPredicate(u64),                 // type_id
    RecordAccessor(u64, usize),           // type_id, field_index
    Continuation(Rc<Kont>, Vec<WindFrame>), // captured continuation + wind stack
    Values(Vec<Value>), // multiple return values (L21)
    SyntaxTransformer(Box<Value>), // syntax-case macro transformer (wraps a Lambda)
    Void,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(n1, d1), Value::Rational(n2, d2)) => n1 == n2 && d1 == d2,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Record(t1, f1), Value::Record(t2, f2)) => t1 == t2 && f1 == f2,
            (Value::RecordConstructor(a, b), Value::RecordConstructor(c, d)) => a == c && b == d,
            (Value::RecordPredicate(a), Value::RecordPredicate(b)) => a == b,
            (Value::RecordAccessor(a, b), Value::RecordAccessor(c, d)) => a == c && b == d,
            (Value::Continuation(a, _), Value::Continuation(b, _)) => Rc::ptr_eq(a, b),
            (Value::Void, Value::Void) => true,
            (Value::SyntaxTransformer(_), Value::SyntaxTransformer(_)) => false,
            _ => false,
        }
    }
}

pub(crate) fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

/// A value annotated with its source position.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Spanned {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl Spanned {
    pub(crate) fn new(val: Value, span: Span) -> Self {
        Self { val, span }
    }
}

impl Value {
    fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => format_float_value(*f),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", match c {
                ' ' => "space".to_string(),
                '\n' => "newline".to_string(),
                '\t' => "tab".to_string(),
                _ => c.to_string(),
            }),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.val.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(cell) => {
                let mut seen = std::collections::HashSet::new();
                display_pair_chain(cell, &mut seen, false)
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|val| val.display_value()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Lambda(..) | Value::CaseLambda(..) | Value::Continuation(..) => "#<procedure>".into(),
            Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) => "#<procedure>".into(),
            Value::Record(..) => "#<record>".into(),
            Value::SyntaxRules { .. } | Value::SyntaxTransformer(_) => "#<syntax>".into(),
            Value::Values(vs) => {
                if vs.len() == 1 { vs[0].display_value() } else { "".into() }
            }
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — strings without quotes, chars as raw char.
    fn format_display(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.val.format_display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(cell) => {
                let mut seen = std::collections::HashSet::new();
                display_pair_chain(cell, &mut seen, true)
            }
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|val| val.format_display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Record(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) | Value::Continuation(..) => self.display_value(),
            _ => self.display_value(),
        }
    }

    fn is_exact_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(..))
    }

    fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Float(_) | Value::Rational(..))
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn display_val(&self, display_mode: bool) -> String {
        if display_mode { self.format_display() } else { self.display_value() }
    }
}

fn display_pair_chain(cell: &Rc<RefCell<(Value, Value)>>, seen: &mut std::collections::HashSet<usize>, display_mode: bool) -> String {
    let ptr = Rc::as_ptr(cell) as usize;
    if !seen.insert(ptr) {
        return "(...)".to_string();
    }
    let (car, cdr) = {
        let inner = cell.borrow();
        (inner.0.clone(), inner.1.clone())
    };
    let mut parts = vec![car.display_val(display_mode)];
    let mut current = cdr;
    loop {
        match &current {
            Value::List(items) if items.is_empty() => break,
            Value::List(items) => {
                for item in items {
                    parts.push(item.val.display_val(display_mode));
                }
                break;
            }
            Value::Pair(cell2) => {
                let ptr2 = Rc::as_ptr(cell2) as usize;
                if !seen.insert(ptr2) {
                    parts.push(". (...)".to_string());
                    break;
                }
                let (car2, cdr2) = {
                    let inner2 = cell2.borrow();
                    (inner2.0.clone(), inner2.1.clone())
                };
                parts.push(car2.display_val(display_mode));
                current = cdr2;
            }
            _ => {
                parts.push(format!(". {}", current.display_val(display_mode)));
                break;
            }
        }
    }
    format!("({})", parts.join(" "))
}

// --- Environment ---

pub(crate) type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug, PartialEq)]
pub(crate) struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

pub(super) fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

pub(crate) fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(val) = inner.bindings.get(name) {
        Some(val.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

pub(crate) fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

pub(super) fn env_update(env: &Env, name: &str, val: Value) -> bool {
    let mut inner = env.borrow_mut();
    if inner.bindings.contains_key(name) {
        inner.bindings.insert(name.to_string(), val);
        return true;
    }
    if let Some(ref parent) = inner.parent {
        env_update(parent, name, val)
    } else {
        false
    }
}

// --- TCO Trampoline ---

pub(super) enum Bounce {
    Done(Value),
    Tail(Spanned, Env),
}

// --- CEK Machine for call/cc ---

#[derive(Clone)]
pub(crate) enum Kont {
    Halt,
    Seq { remaining: Vec<Spanned>, env: Env, next: Rc<Kont> },
    IfDecide { then_br: Spanned, else_br: Option<Spanned>, env: Env, next: Rc<Kont> },
    DefineVar { name: String, env: Env, next: Rc<Kont> },
    SetVar { name: String, env: Env, span: Span, next: Rc<Kont> },
    EvalHead { arg_exprs: Vec<Spanned>, env: Env, span: Span, next: Rc<Kont> },
    EvalArgs {
        func: Value,
        all_arg_exprs: Vec<Spanned>,
        done_vals: Vec<Value>,
        remaining: Vec<Spanned>,
        env: Env,
        span: Span,
        next: Rc<Kont>,
    },
    CallCC { captured: Rc<Kont>, span: Span },
    And { remaining: Vec<Spanned>, env: Env, next: Rc<Kont> },
    Or { remaining: Vec<Spanned>, env: Env, next: Rc<Kont> },
    Not { next: Rc<Kont> },
    WhenTest { body: Vec<Spanned>, env: Env, next: Rc<Kont> },
    CondTest { clause_body: Vec<Spanned>, remaining_clauses: Vec<Spanned>, env: Env, span: Span, next: Rc<Kont> },
    CondArrow { test_value: Value, next: Rc<Kont> },
    CaseKey { clauses: Vec<Spanned>, env: Env, span: Span, next: Rc<Kont> },
    LetBind {
        current_name: String,
        remaining: Vec<(String, Spanned)>,
        local_env: Env,
        eval_env: Env,
        body: Vec<Spanned>,
        next: Rc<Kont>,
    },
    LetStarBind {
        current_name: String,
        remaining: Vec<(String, Spanned)>,
        local_env: Env,
        body: Vec<Spanned>,
        next: Rc<Kont>,
    },
    LetrecBind {
        idx: usize,
        names: Vec<String>,
        remaining_inits: Vec<Spanned>,
        local_env: Env,
        body: Vec<Spanned>,
        next: Rc<Kont>,
    },
    NamedLetBind {
        loop_name: String,
        params: Vec<String>,
        done_inits: Vec<Value>,
        remaining_inits: Vec<Spanned>,
        body: Vec<Spanned>,
        eval_env: Env,
        next: Rc<Kont>,
    },
    // dynamic-wind support
    DynWindBody { body_thunk: Value, out_thunk: Value, in_thunk: Value, marker: u64, next: Rc<Kont> },
    DynWindAfterBody { out_thunk: Value, next: Rc<Kont> },
    DynWindAfterOut { body_value: Value, next: Rc<Kont> },
    DynWindTransition {
        out_thunks: Vec<Value>,
        in_thunks: Vec<Value>,
        rewind_frames: Vec<WindFrame>,
        target_kont: Rc<Kont>,
        value: Value,
        is_resume: bool,
    },
    // Exception handling (L20)
    PopExceptionHandler { next: Rc<Kont> },
    GuardTest { var: String, exn: Value, clauses: Vec<Spanned>, env: Env, next: Rc<Kont> },
    // Multiple values (L21)
    CallWithValuesConsumer { consumer: Value, next: Rc<Kont> },
}

// Kont is not Debug-derivable due to Value, but we don't need Debug
impl std::fmt::Debug for Kont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<kont>")
    }
}

pub(super) fn eval_define_record_type(items: &[Spanned], env: &Env, span: Span) -> Result<(), EvalError> {
    if items.len() < 4 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into(), span));
    }
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
    let Value::List(ctor_parts) = &items[2].val else {
        return Err(EvalError::Type("define-record-type: expected constructor spec".into(), span));
    };
    if ctor_parts.is_empty() {
        return Err(EvalError::Parse("define-record-type: empty constructor".into(), span));
    }
    let ctor_name = match &ctor_parts[0].val {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected constructor name".into(), span)),
    };
    let ctor_fields: Vec<String> = ctor_parts[1..].iter().map(|p| match &p.val {
        Value::Symbol(s) => Ok(s.clone()),
        _ => Err(EvalError::Type("define-record-type: expected field name".into(), span)),
    }).collect::<Result<_, _>>()?;
    let field_count = ctor_fields.len();
    let pred_name = match &items[3].val {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into(), span)),
    };
    env_set(env, ctor_name, Value::RecordConstructor(type_id, field_count));
    env_set(env, pred_name, Value::RecordPredicate(type_id));
    for field_spec in &items[4..] {
        let Value::List(fparts) = &field_spec.val else {
            return Err(EvalError::Type("define-record-type: expected field spec".into(), span));
        };
        if fparts.len() < 2 {
            return Err(EvalError::Arity("define-record-type: field spec needs name and accessor".into(), span));
        }
        let field_name = match &fparts[0].val {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("define-record-type: expected field name".into(), span)),
        };
        let accessor_name = match &fparts[1].val {
            Value::Symbol(s) => s.clone(),
            _ => return Err(EvalError::Type("define-record-type: expected accessor name".into(), span)),
        };
        let idx = ctor_fields.iter().position(|f| f == &field_name)
            .ok_or_else(|| EvalError::Type(format!("define-record-type: unknown field {}", field_name), span))?;
        env_set(env, accessor_name, Value::RecordAccessor(type_id, idx));
    }
    Ok(())
}

pub(super) fn eval_define_syntax(items: &[Spanned], env: &Env, span: Span) -> Result<(), EvalError> {
    if items.len() != 3 {
        return Err(EvalError::Arity("define-syntax requires 2 arguments".into(), span));
    }
    let Value::Symbol(macro_name) = &items[1].val else {
        return Err(EvalError::Type("define-syntax: expected symbol".into(), span));
    };
    let Value::List(parts) = &items[2].val else {
        return Err(EvalError::Type("define-syntax: expected syntax-rules or lambda".into(), span));
    };
    if parts.is_empty() {
        return Err(EvalError::Type("define-syntax: empty transformer".into(), span));
    }
    match &parts[0].val {
        Value::Symbol(s) if s == "syntax-rules" => {
            if parts.len() < 2 {
                return Err(EvalError::Arity("syntax-rules requires literals list".into(), span));
            }
            let Value::List(lit_list) = &parts[1].val else {
                return Err(EvalError::Type("syntax-rules: expected literals list".into(), span));
            };
            let literals: Vec<String> = lit_list.iter().filter_map(|l| {
                if let Value::Symbol(s) = &l.val { Some(s.clone()) } else { None }
            }).collect();
            let mut rules = Vec::new();
            for rule in &parts[2..] {
                let Value::List(rparts) = &rule.val else {
                    return Err(EvalError::Type("syntax-rules: expected rule".into(), span));
                };
                if rparts.len() != 2 {
                    return Err(EvalError::Arity("syntax-rules: rule needs pattern and template".into(), span));
                }
                rules.push((rparts[0].clone(), rparts[1].clone()));
            }
            let val = Value::SyntaxRules { literals, rules, def_env: env.clone() };
            env_set(env, macro_name.clone(), val);
        }
        Value::Symbol(s) if s == "lambda" => {
            if parts.len() < 3 {
                return Err(EvalError::Arity("lambda requires params and body".into(), span));
            }
            let (params, rest) = parse_params_from_value(&parts[1].val, "lambda", span)?;
            let body = parts[2..].to_vec();
            let lambda = Value::Lambda(params, rest, body, env.clone());
            env_set(env, macro_name.clone(), Value::SyntaxTransformer(Box::new(lambda)));
        }
        _ => return Err(EvalError::Type("define-syntax: expected syntax-rules or lambda".into(), span)),
    }
    Ok(())
}

pub(super) fn eval_string_set_standalone(items: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
    if items.len() != 4 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into(), span));
    }
    if matches!(&items[1].val, Value::Str(_)) {
        return Err(EvalError::Type("string-set!: strings are immutable".into(), span));
    }
    let name = match &items[1].val {
        Value::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: expected string variable".into(), span)),
    };
    let s_val = env_get(env, &name).ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))?;
    let s = match s_val {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into(), span)),
    };
    let idx = match eval(&items[2], env, out)? {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into(), span)),
    };
    let ch = match eval(&items[3], env, out)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into(), span)),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type("string-set!: index out of bounds".into(), span));
    }
    chars[idx] = ch;
    env_update(env, &name, Value::Str(chars.into_iter().collect()));
    Ok(Value::Void)
}

// --- Macros (syntax-rules) ---

pub(super) fn expand_macro(
    items: &[Spanned],
    literals: &[String],
    rules: &[(Spanned, Spanned)],
    def_env: &Env,
    env: &Env,
    span: Span,
) -> Result<Spanned, EvalError> {
    let Some((expanded, renames)) = macros::try_expand(items, literals, rules) else {
        return Err(EvalError::Type("no matching pattern for macro".into(), span));
    };
    for (orig, gs) in &renames {
        if let Some(val) = env_get(def_env, orig) {
            env_set(env, gs.clone(), val);
        }
    }
    Ok(expanded)
}

// --- Evaluator ---

/// Decrement step counter; return Err if budget exhausted.
pub(super) fn check_step_limit() -> Result<(), EvalError> {
    STEP_LIMIT.with(|sl| {
        let mut opt = sl.borrow_mut();
        if let Some(ref mut remaining) = *opt {
            if *remaining == 0 {
                return Err(EvalError::StepLimitExceeded);
            }
            *remaining -= 1;
        }
        Ok(())
    })
}

pub(super) fn eval(expr: &Spanned, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut cur = expr.clone();
    let mut cur_env = env.clone();
    loop {
        check_step_limit()?;
        match eval_step(&cur, &cur_env, out)? {
            Bounce::Done(v) => return Ok(v),
            Bounce::Tail(next, next_env) => {
                cur = next;
                cur_env = next_env;
            }
        }
    }
}

fn eval_step(expr: &Spanned, env: &Env, out: &Output) -> Result<Bounce, EvalError> {
    let span = expr.span;
    match &expr.val {
        Value::Integer(_) | Value::Float(_) | Value::Rational(..) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Pair(..) | Value::Lambda(..) | Value::CaseLambda(..) | Value::SyntaxRules { .. } | Value::SyntaxTransformer(..) | Value::Vector(..) | Value::Record(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) | Value::Continuation(..) | Value::Values(..) => Ok(Bounce::Done(expr.val.clone())),
        Value::Symbol(name) => {
            env_get(env, name).map(Bounce::Done).ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))
        }
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse("empty application".into(), span));
            }
            let head = &items[0];
            if let Value::Symbol(name) = &head.val {
                match name.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quote requires 1 argument".into(), span));
                        }
                        return Ok(Bounce::Done(items[1].val.clone()));
                    }
                    "quasiquote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("quasiquote requires 1 argument".into(), span));
                        }
                        let result = expand_quasiquote(&items[1].val, env, out, 1, span)?;
                        return Ok(Bounce::Done(result));
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into(), span));
                        }
                        let cond = eval(&items[1], env, out)?;
                        if cond.is_truthy() {
                            return Ok(Bounce::Tail(items[2].clone(), env.clone()));
                        } else if items.len() == 4 {
                            return Ok(Bounce::Tail(items[3].clone(), env.clone()));
                        } else {
                            return Ok(Bounce::Done(Value::Void));
                        }
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("define requires at least 2 arguments".into(), span));
                        }
                        match &items[1].val {
                            Value::Symbol(var_name) => {
                                let val = eval(&items[2], env, out)?;
                                env_set(env, var_name.clone(), val);
                                return Ok(Bounce::Done(Value::Void));
                            }
                            Value::List(sig) => {
                                if sig.is_empty() {
                                    return Err(EvalError::Parse("define: empty signature".into(), span));
                                }
                                let func_name = match &sig[0].val {
                                    Value::Symbol(s) => s.clone(),
                                    _ => return Err(EvalError::Type("define: expected symbol for function name".into(), span)),
                                };
                                let (params, rest) = parse_params(&sig[1..], "define", span)?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda(params, rest, body, env.clone());
                                env_set(env, func_name, lambda);
                                return Ok(Bounce::Done(Value::Void));
                            }
                            Value::Pair(cell) => {
                                let (car, cdr) = { let b = cell.borrow(); (b.0.clone(), b.1.clone()) };
                                let func_name = match car {
                                    Value::Symbol(s) => s,
                                    _ => return Err(EvalError::Type("define: expected symbol for function name".into(), span)),
                                };
                                let (params, rest) = parse_params_from_value(&cdr, "define", span)?;
                                let body = items[2..].to_vec();
                                let lambda = Value::Lambda(params, rest, body, env.clone());
                                env_set(env, func_name, lambda);
                                return Ok(Bounce::Done(Value::Void));
                            }
                            _ => return Err(EvalError::Type("define: expected symbol or list".into(), span)),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires at least 2 arguments".into(), span));
                        }
                        let (params, rest) = parse_params_from_value(&items[1].val, "lambda", span)?;
                        let body = items[2..].to_vec();
                        return Ok(Bounce::Done(Value::Lambda(params, rest, body, env.clone())));
                    }
                    "case-lambda" => {
                        let mut clauses = Vec::new();
                        for clause in &items[1..] {
                            let Value::List(parts) = &clause.val else {
                                return Err(EvalError::Type("case-lambda: clause must be a list".into(), span));
                            };
                            if parts.is_empty() {
                                return Err(EvalError::Arity("case-lambda: clause must have formals and body".into(), span));
                            }
                            let (params, rest) = parse_params_from_value(&parts[0].val, "case-lambda", span)?;
                            let body = parts[1..].to_vec();
                            clauses.push((params, rest, body, env.clone()));
                        }
                        return Ok(Bounce::Done(Value::CaseLambda(clauses)));
                    }
                    "let" => return eval_let_step(&items[1..], env, out, span),
                    "begin" => {
                        if items.len() <= 1 {
                            return Ok(Bounce::Done(Value::Void));
                        }
                        for e in &items[1..items.len()-1] {
                            eval(e, env, out)?;
                        }
                        return Ok(Bounce::Tail(items[items.len()-1].clone(), env.clone()));
                    }
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("set! requires 2 arguments".into(), span));
                        }
                        let Value::Symbol(name) = &items[1].val else {
                            return Err(EvalError::Type("set!: first argument must be a symbol".into(), span));
                        };
                        let val = eval(&items[2], env, out)?;
                        if !env_update(env, name, val) {
                            return Err(EvalError::UnboundVariable(name.clone(), span));
                        }
                        return Ok(Bounce::Done(Value::Void));
                    }
                    "cond" => return eval_cond_step(&items[1..], env, out, span),
                    "and" => return eval_and_step(&items[1..], env, out),
                    "or" => return eval_or_step(&items[1..], env, out),
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity("string-set! requires 3 arguments".into(), span));
                        }
                        // string-set! on a literal string is an error (immutable)
                        if matches!(&items[1].val, Value::Str(_)) {
                            return Err(EvalError::Type("string-set!: strings are immutable".into(), span));
                        }
                        let name = match &items[1].val {
                            Value::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("string-set!: expected string variable".into(), span)),
                        };
                        let s_val = env_get(env, &name).ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))?;
                        let s = match s_val {
                            Value::Str(s) => s,
                            _ => return Err(EvalError::Type("string-set!: expected string".into(), span)),
                        };
                        let idx = match eval(&items[2], env, out)? {
                            Value::Integer(n) => n as usize,
                            _ => return Err(EvalError::Type("string-set!: expected integer index".into(), span)),
                        };
                        let ch = match eval(&items[3], env, out)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type("string-set!: expected char".into(), span)),
                        };
                        let mut chars: Vec<char> = s.chars().collect();
                        if idx >= chars.len() {
                            return Err(EvalError::Type("string-set!: index out of bounds".into(), span));
                        }
                        chars[idx] = ch;
                        env_update(env, &name, Value::Str(chars.into_iter().collect()));
                        return Ok(Bounce::Done(Value::Void));
                    }
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into(), span));
                        }
                        let v = eval(&items[1], env, out)?;
                        return Ok(Bounce::Done(Value::Boolean(!v.is_truthy())));
                    }
                    "define-record-type" => {
                        // (define-record-type <name> (ctor field...) pred (field accessor)...)
                        if items.len() < 4 {
                            return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into(), span));
                        }
                        let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
                        // Parse constructor
                        let Value::List(ctor_parts) = &items[2].val else {
                            return Err(EvalError::Type("define-record-type: expected constructor spec".into(), span));
                        };
                        if ctor_parts.is_empty() {
                            return Err(EvalError::Parse("define-record-type: empty constructor".into(), span));
                        }
                        let ctor_name = match &ctor_parts[0].val {
                            Value::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("define-record-type: expected constructor name".into(), span)),
                        };
                        let ctor_fields: Vec<String> = ctor_parts[1..].iter().map(|p| match &p.val {
                            Value::Symbol(s) => Ok(s.clone()),
                            _ => Err(EvalError::Type("define-record-type: expected field name".into(), span)),
                        }).collect::<Result<_, _>>()?;
                        let field_count = ctor_fields.len();
                        // Parse predicate
                        let pred_name = match &items[3].val {
                            Value::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type("define-record-type: expected predicate name".into(), span)),
                        };
                        // Bind constructor and predicate
                        env_set(env, ctor_name, Value::RecordConstructor(type_id, field_count));
                        env_set(env, pred_name, Value::RecordPredicate(type_id));
                        // Parse field accessors
                        for field_spec in &items[4..] {
                            let Value::List(fparts) = &field_spec.val else {
                                return Err(EvalError::Type("define-record-type: expected field spec".into(), span));
                            };
                            if fparts.len() < 2 {
                                return Err(EvalError::Arity("define-record-type: field spec needs name and accessor".into(), span));
                            }
                            let field_name = match &fparts[0].val {
                                Value::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type("define-record-type: expected field name".into(), span)),
                            };
                            let accessor_name = match &fparts[1].val {
                                Value::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::Type("define-record-type: expected accessor name".into(), span)),
                            };
                            let idx = ctor_fields.iter().position(|f| f == &field_name)
                                .ok_or_else(|| EvalError::Type(format!("define-record-type: unknown field {}", field_name), span))?;
                            env_set(env, accessor_name, Value::RecordAccessor(type_id, idx));
                        }
                        return Ok(Bounce::Done(Value::Void));
                    }
                    "let*" => return eval_let_star_step(&items[1..], env, out, span),
                    "letrec" => return eval_letrec_step(&items[1..], env, out, span),
                    "letrec*" => return eval_letrec_step(&items[1..], env, out, span),
                    "when" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("when requires test and body".into(), span));
                        }
                        let test = eval(&items[1], env, out)?;
                        if test.is_truthy() {
                            return eval_body_step(&items[2..], env, out);
                        }
                        return Ok(Bounce::Done(Value::Void));
                    }
                    "case" => return eval_case_step(&items[1..], env, out, span),
                    "do" => return Ok(Bounce::Done(eval_do(&items[1..], env, out, span)?)),
                    "call/cc" | "call-with-current-continuation" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("call/cc requires 1 argument".into(), span));
                        }
                        let proc = eval(&items[1], env, out)?;
                        // Create a lightweight continuation (Halt kont).
                        // Non-escaping uses are caught here; escaping uses propagate
                        // ContinuationInvoked up to eval_smart which resumes the CEK machine.
                        let cont_val = Value::Continuation(Rc::new(Kont::Halt), vec![]);
                        match apply(&proc, &[cont_val], out, span) {
                            Ok(v) => return Ok(Bounce::Done(v)),
                            Err(EvalError::ContinuationInvoked) => {
                                let (_, value) = cek::CONT_JUMP.with(|c| c.borrow_mut().take().expect("CONT_JUMP must be set after ContinuationInvoked"));
                                return Ok(Bounce::Done(value));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    "define-syntax" => {
                        eval_define_syntax(items, env, span)?;
                        return Ok(Bounce::Done(Value::Void));
                    }
                    "syntax-case" => {
                        return syntax_case::eval_syntax_case(items, env, out, span);
                    }
                    "syntax" => {
                        return syntax_case::eval_syntax_template(items, env, span);
                    }
                    "with-syntax" => {
                        return syntax_case::eval_with_syntax(items, env, out, span);
                    }
                    _ => {
                        // Check for macro application
                        if let Some(val) = env_get(env, name) {
                            match val {
                                Value::SyntaxRules { ref literals, ref rules, ref def_env } => {
                                    let expanded = expand_macro(items, literals, rules, def_env, env, span)?;
                                    return Ok(Bounce::Tail(expanded, env.clone()));
                                }
                                Value::SyntaxTransformer(ref transformer) => {
                                    let expanded = syntax_case::expand_syntax_case_macro(items, transformer, env, out, span)?;
                                    return Ok(Bounce::Tail(expanded, env.clone()));
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            // Function application
            let func = eval(head, env, out)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env, out)).collect();
            let args = args?;
            apply_step(&func, &args, out, span)
        }
        Value::Void => Ok(Bounce::Done(Value::Void)),
    }
}

/// Set up a lambda environment: bind params and optional rest param.
pub(super) fn bind_lambda_env(
    params: &[String], rest: &Option<String>, args: &[Value],
    closure_env: &Env, span: Span,
) -> Result<Env, EvalError> {
    let local_env = new_env(Some(closure_env.clone()));
    if let Some(rest_name) = rest {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "expected at least {} arguments, got {}", params.len(), args.len()
            ), span));
        }
        for (param, arg) in params.iter().zip(args.iter()) {
            env_set(&local_env, param.clone(), arg.clone());
        }
        let rest_list = args[params.len()..].iter()
            .map(|a| Spanned::new(a.clone(), DUMMY_SPAN))
            .collect();
        env_set(&local_env, rest_name.clone(), Value::List(rest_list));
    } else {
        if args.len() != params.len() {
            return Err(EvalError::Arity(format!(
                "expected {} arguments, got {}", params.len(), args.len()
            ), span));
        }
        for (param, arg) in params.iter().zip(args.iter()) {
            env_set(&local_env, param.clone(), arg.clone());
        }
    }
    Ok(local_env)
}

/// Evaluate body expressions, returning a Bounce for the last (tail position).
fn eval_body_step(body: &[Spanned], env: &Env, out: &Output) -> Result<Bounce, EvalError> {
    if body.is_empty() {
        return Ok(Bounce::Done(Value::Void));
    }
    for expr in &body[..body.len()-1] {
        eval(expr, env, out)?;
    }
    Ok(Bounce::Tail(body[body.len()-1].clone(), env.clone()))
}

fn apply_step(func: &Value, args: &[Value], out: &Output, span: Span) -> Result<Bounce, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            let local_env = bind_lambda_env(params, rest, args, closure_env, span)?;
            eval_body_step(body, &local_env, out)
        }
        Value::CaseLambda(clauses) => {
            for (params, rest, body, closure_env) in clauses {
                let matches = if rest.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = bind_lambda_env(params, rest, args, closure_env, span)?;
                    return eval_body_step(body, &local_env, out);
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len()), span))
        }
        Value::RecordConstructor(type_id, field_count) => {
            if args.len() != *field_count {
                return Err(EvalError::Arity(format!("record constructor expects {} arguments, got {}", field_count, args.len()), span));
            }
            Ok(Bounce::Done(Value::Record(*type_id, args.to_vec())))
        }
        Value::RecordPredicate(type_id) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into(), span));
            }
            Ok(Bounce::Done(Value::Boolean(matches!(&args[0], Value::Record(tid, _) if tid == type_id))))
        }
        Value::RecordAccessor(type_id, idx) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::Record(tid, fields) if tid == type_id => Ok(Bounce::Done(fields[*idx].clone())),
                _ => Err(EvalError::Type("record accessor: wrong record type".into(), span)),
            }
        }
        Value::Continuation(kont, target_winds) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("continuation requires 1 argument".into(), span));
            }
            let value = args[0].clone();
            let current_winds = WIND_STACK.with(|ws| ws.borrow().clone());
            let common = current_winds.iter().zip(target_winds.iter())
                .take_while(|(a, b)| a.2 == b.2).count();
            if common == current_winds.len() && common == target_winds.len() {
                cek::CONT_JUMP.with(|c| {
                    *c.borrow_mut() = Some((kont.clone(), value));
                });
                Err(EvalError::ContinuationInvoked)
            } else {
                let out_thunks: Vec<Value> = current_winds[common..].iter().rev()
                    .map(|(_, o, _)| o.clone()).collect();
                let in_thunks: Vec<Value> = target_winds[common..].iter()
                    .map(|(i, _, _)| i.clone()).collect();
                let rewind_frames: Vec<WindFrame> = target_winds[common..].to_vec();
                let transition = Rc::new(Kont::DynWindTransition {
                    out_thunks, in_thunks, rewind_frames,
                    target_kont: kont.clone(), value, is_resume: true,
                });
                cek::CONT_JUMP.with(|c| {
                    *c.borrow_mut() = Some((transition, Value::Void));
                });
                Err(EvalError::ContinuationInvoked)
            }
        }
        Value::Symbol(name) => Ok(Bounce::Done(apply_builtin(name, args, out, span, apply)?)),
        _ => Err(EvalError::Type("not a procedure".into(), span)),
    }
}

pub(super) fn apply(func: &Value, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match apply_step(func, args, out, span)? {
        Bounce::Done(v) => Ok(v),
        Bounce::Tail(expr, env) => eval(&expr, &env, out),
    }
}

fn eval_and_step(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Bounce, EvalError> {
    if exprs.is_empty() {
        return Ok(Bounce::Done(Value::Boolean(true)));
    }
    for expr in &exprs[..exprs.len()-1] {
        let result = eval(expr, env, out)?;
        if !result.is_truthy() {
            return Ok(Bounce::Done(result));
        }
    }
    Ok(Bounce::Tail(exprs[exprs.len()-1].clone(), env.clone()))
}

fn eval_or_step(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Bounce, EvalError> {
    if exprs.is_empty() {
        return Ok(Bounce::Done(Value::Boolean(false)));
    }
    for expr in &exprs[..exprs.len()-1] {
        let result = eval(expr, env, out)?;
        if result.is_truthy() {
            return Ok(Bounce::Done(result));
        }
    }
    Ok(Bounce::Tail(exprs[exprs.len()-1].clone(), env.clone()))
}

fn eval_let_step(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    // Named let: (let name ((var init) ...) body...)
    if let Value::Symbol(name) = &args[0].val {
        if args.len() < 3 {
            return Err(EvalError::Arity("named let requires bindings and body".into(), span));
        }
        let Value::List(bindings) = &args[1].val else {
            return Err(EvalError::Type("named let: expected bindings list".into(), span));
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            let Value::List(pair) = &b.val else {
                return Err(EvalError::Type("let: binding must be a list".into(), span));
            };
            if pair.len() != 2 {
                return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
            }
            let Value::Symbol(p) = &pair[0].val else {
                return Err(EvalError::Type("let: expected symbol in binding".into(), span));
            };
            params.push(p.clone());
            inits.push(eval(&pair[1], env, out)?);
        }
        let body = args[2..].to_vec();
        let loop_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda(params.clone(), None, body.clone(), loop_env.clone());
        env_set(&loop_env, name.clone(), lambda);
        let call_env = new_env(Some(loop_env));
        for (p, v) in params.iter().zip(inits.iter()) {
            env_set(&call_env, p.clone(), v.clone());
        }
        return eval_body_step(&body, &call_env, out);
    }
    // Regular let: (let ((var init) ...) body...)
    if args.len() < 2 {
        return Err(EvalError::Arity("let requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("let: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("let: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("let: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("let: expected symbol in binding".into(), span));
        };
        let val = eval(&pair[1], env, out)?;
        env_set(&local_env, name.clone(), val);
    }
    eval_body_step(&args[1..], &local_env, out)
}

fn eval_let_star_step(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Bounce, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("let*: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("let*: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("let*: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("let*: expected symbol in binding".into(), span));
        };
        let val = eval(&pair[1], &local_env, out)?;
        env_set(&local_env, name.clone(), val);
    }
    eval_body_step(&args[1..], &local_env, out)
}

/// Tries to handle an `unquote-splicing` item during quasiquote expansion.
/// Returns `Ok(true)` if the item was spliced into `result`, `Ok(false)` if not an unquote-splicing form.
fn try_expand_splice(
    item: &Spanned, env: &Env, out: &Output, depth: usize, span: Span, result: &mut Vec<Spanned>,
) -> Result<bool, EvalError> {
    let Value::List(sub) = &item.val else { return Ok(false) };
    if sub.len() != 2 { return Ok(false) }
    let Value::Symbol(s) = &sub[0].val else { return Ok(false) };
    if s != "unquote-splicing" { return Ok(false) }

    if depth != 1 {
        let inner = expand_quasiquote(&sub[1].val, env, out, depth - 1, span)?;
        result.push(Spanned::new(Value::List(vec![
            sub[0].clone(),
            Spanned::new(inner, sub[1].span),
        ]), item.span));
        return Ok(true);
    }

    let val = eval(&sub[1], env, out)?;
    match val {
        Value::List(elems) => {
            result.extend(elems);
        }
        Value::Pair(_) => {
            splice_pair_to_vec(val, span, result)?;
        }
        _ => return Err(EvalError::Type("unquote-splicing: not a list".into(), span)),
    }
    Ok(true)
}

fn splice_pair_to_vec(val: Value, span: Span, result: &mut Vec<Spanned>) -> Result<(), EvalError> {
    let mut cur = val;
    loop {
        match cur {
            Value::Pair(cell) => {
                let (car, cdr) = { let b = cell.borrow(); (b.0.clone(), b.1.clone()) };
                result.push(Spanned::new(car, span));
                cur = cdr;
            }
            Value::List(items) if items.is_empty() => break,
            _ => return Err(EvalError::Type("unquote-splicing: not a proper list".into(), span)),
        }
    }
    Ok(())
}

pub(super) fn expand_quasiquote(expr: &Value, env: &Env, out: &Output, depth: usize, span: Span) -> Result<Value, EvalError> {
    match expr {
        Value::List(items) if !items.is_empty() => {
            if let Value::Symbol(s) = &items[0].val {
                if s == "unquote" && items.len() == 2 {
                    if depth == 1 {
                        return eval(&items[1], env, out);
                    } else {
                        let inner = expand_quasiquote(&items[1].val, env, out, depth - 1, span)?;
                        return Ok(Value::List(vec![
                            items[0].clone(),
                            Spanned::new(inner, items[1].span),
                        ]));
                    }
                }
                if s == "quasiquote" && items.len() == 2 {
                    let inner = expand_quasiquote(&items[1].val, env, out, depth + 1, span)?;
                    return Ok(Value::List(vec![
                        items[0].clone(),
                        Spanned::new(inner, items[1].span),
                    ]));
                }
            }
            // Process each element, handling unquote-splicing
            let mut result = Vec::new();
            for item in items {
                if try_expand_splice(item, env, out, depth, span, &mut result)? {
                    continue;
                }
                let expanded = expand_quasiquote(&item.val, env, out, depth, span)?;
                result.push(Spanned::new(expanded, item.span));
            }
            Ok(Value::List(result))
        }
        Value::Pair(cell) => {
            let (car, cdr) = { let b = cell.borrow(); (b.0.clone(), b.1.clone()) };
            // Check for (unquote x) as a pair
            if let Value::Symbol(s) = &car {
                if s == "unquote" && depth == 1 {
                    // cdr should be a list with one element
                    if let Value::List(items) = &cdr {
                        if items.len() == 1 {
                            return eval(&items[0], env, out);
                        }
                    }
                }
            }
            let expanded_car = expand_quasiquote(&car, env, out, depth, span)?;
            let expanded_cdr = expand_quasiquote(&cdr, env, out, depth, span)?;
            Ok(Value::Pair(Rc::new(RefCell::new((expanded_car, expanded_cdr)))))
        }
        _ => Ok(expr.clone()),
    }
}


fn eval_cond_step(clauses: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Bounce, EvalError> {
    for clause in clauses {
        let Value::List(parts) = &clause.val else {
            return Err(EvalError::Type("cond: expected list clause".into(), span));
        };
        if parts.is_empty() {
            return Err(EvalError::Arity("cond: empty clause".into(), span));
        }
        if let Value::Symbol(s) = &parts[0].val {
            if s == "else" {
                return eval_body_step(&parts[1..], env, out);
            }
        }
        let test = eval(&parts[0], env, out)?;
        if test.is_truthy() {
            if parts.len() == 1 {
                return Ok(Bounce::Done(test));
            }
            if parts.len() == 2 && matches!(&parts[1].val, Value::Symbol(s) if s == "=>") {
                return Err(EvalError::Arity("cond =>: missing procedure".into(), span));
            }
            if parts.len() >= 2 && matches!(&parts[1].val, Value::Symbol(s) if s == "=>") {
                let proc = eval(&parts[2], env, out)?;
                return apply_step(&proc, &[test], out, span);
            }
            return eval_body_step(&parts[1..], env, out);
        }
    }
    Ok(Bounce::Done(Value::Void))
}

fn eval_letrec_step(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("letrec requires bindings and body".into(), span));
    }
    let Value::List(bindings) = &args[0].val else {
        return Err(EvalError::Type("letrec: expected bindings list".into(), span));
    };
    let local_env = new_env(Some(env.clone()));
    // First pass: bind all names to Void
    let mut names = Vec::new();
    for b in bindings {
        let Value::List(pair) = &b.val else {
            return Err(EvalError::Type("letrec: binding must be a list".into(), span));
        };
        if pair.len() != 2 {
            return Err(EvalError::Arity("letrec: binding must have 2 elements".into(), span));
        }
        let Value::Symbol(name) = &pair[0].val else {
            return Err(EvalError::Type("letrec: expected symbol in binding".into(), span));
        };
        names.push(name.clone());
        env_set(&local_env, name.clone(), Value::Void);
    }
    // Second pass: evaluate init expressions
    for (i, b) in bindings.iter().enumerate() {
        let Value::List(pair) = &b.val else { unreachable!() };
        let val = eval(&pair[1], &local_env, out)?;
        env_set(&local_env, names[i].clone(), val);
    }
    eval_body_step(&args[1..], &local_env, out)
}

fn eval_case_step(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Bounce, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires key and clauses".into(), span));
    }
    let key = eval(&args[0], env, out)?;
    for clause in &args[1..] {
        let Value::List(parts) = &clause.val else {
            return Err(EvalError::Type("case: expected clause list".into(), span));
        };
        if parts.is_empty() {
            return Err(EvalError::Arity("case: empty clause".into(), span));
        }
        // Check for else clause
        if let Value::Symbol(s) = &parts[0].val {
            if s == "else" {
                return eval_body_step(&parts[1..], env, out);
            }
        }
        // Datum list
        let Value::List(datums) = &parts[0].val else {
            return Err(EvalError::Type("case: expected datum list".into(), span));
        };
        for datum in datums {
            if values_eqv(&key, &datum.val) {
                return eval_body_step(&parts[1..], env, out);
            }
        }
    }
    Ok(Bounce::Done(Value::Void))
}


pub(super) fn eval_do(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable specs and test".into(), span));
    }
    let Value::List(var_specs) = &args[0].val else {
        return Err(EvalError::Type("do: expected variable spec list".into(), span));
    };
    let Value::List(test_clause) = &args[1].val else {
        return Err(EvalError::Type("do: expected test clause".into(), span));
    };
    if test_clause.is_empty() {
        return Err(EvalError::Arity("do: test clause must have test expression".into(), span));
    }
    // Parse variable specs
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Spanned>> = Vec::new();
    let local_env = new_env(Some(env.clone()));
    for spec in var_specs {
        let Value::List(parts) = &spec.val else {
            return Err(EvalError::Type("do: expected variable spec".into(), span));
        };
        if parts.len() < 2 || parts.len() > 3 {
            return Err(EvalError::Arity("do: variable spec needs (var init) or (var init step)".into(), span));
        }
        let Value::Symbol(name) = &parts[0].val else {
            return Err(EvalError::Type("do: expected variable name".into(), span));
        };
        let init_val = eval(&parts[1], env, out)?;
        var_names.push(name.clone());
        step_exprs.push(if parts.len() == 3 { Some(parts[2].clone()) } else { None });
        env_set(&local_env, name.clone(), init_val);
    }
    let body = &args[2..];
    // Iteration loop
    loop {
        // Test
        let test_val = eval(&test_clause[0], &local_env, out)?;
        if test_val.is_truthy() {
            // Evaluate result expressions
            let mut result = Value::Void;
            for expr in &test_clause[1..] {
                result = eval(expr, &local_env, out)?;
            }
            return Ok(result);
        }
        // Execute body for side effects
        for expr in body {
            eval(expr, &local_env, out)?;
        }
        // Parallel step: evaluate all step expressions using current values
        let mut new_vals = Vec::new();
        for (i, step) in step_exprs.iter().enumerate() {
            if let Some(step_expr) = step {
                new_vals.push(Some(eval(step_expr, &local_env, out)?));
            } else {
                new_vals.push(None);
            }
            let _ = i;
        }
        // Update variables
        for (i, name) in var_names.iter().enumerate() {
            if let Some(val) = &new_vals[i] {
                env_set(&local_env, name.clone(), val.clone());
            }
        }
    }
}


fn make_global_env() -> Env {
    let env = new_env(None);
    for name in &["+", "-", "*", "/", "<", ">", "=", "<=", ">=",
                   "cons", "car", "cdr", "null?", "list", "length", "append",
                   "number?", "boolean?", "string?", "pair?", "symbol?",
                   "modulo", "remainder", "quotient",
                   "display", "write", "newline",
                   "string-append", "string-length", "substring",
                   "string->number", "number->string",
                   "symbol->string", "string->symbol",
                   "string-ref", "string-copy", "char?",
                   "apply",
                   "equal?", "eq?", "eqv?",
                   // L09
                   "zero?", "positive?", "negative?", "odd?", "even?",
                   "abs", "min", "max", "expt",
                   "list?", "list-ref", "list-tail", "assoc",
                   "map", "for-each",
                   "char-alphabetic?", "char-numeric?",
                   "char-upcase", "char-downcase", "char=?", "char<?",
                   "string=?", "string<?", "string-ci=?",
                   "string-upcase", "string-downcase",
                   // L11
                   "integer?", "rational?",
                   "exact?", "inexact?",
                   "exact->inexact", "inexact->exact",
                   "numerator", "denominator",
                   // L13
                   "procedure?",
                   // L14
                   "vector", "make-vector", "vector-ref", "vector-set!",
                   "vector-length", "vector?", "vector->list", "list->vector",
                   "assq", "memq",
                   // L15
                   "string->list", "list->string",
                   "char->integer", "integer->char",
                   // L17
                   "set-car!", "set-cdr!",
                   "caar", "cadr", "cdar", "cddr", "caddr", "cdddr",
                   "reverse", "member", "assv", "memv", "error",
                   "gcd", "lcm", "truncate", "round",
                   "make-string", "string",
                   "string<=?", "string>=?", "string>?",
                   // Dynamic c..r (4 levels)
                   "caaar", "caadr", "cdaar", "cdadr",
                   "caaaar", "caaadr", "caadar", "caaddr",
                   "cadaar", "cadadr", "caddar", "cadddr",
                   "cdaaar", "cdaadr", "cdadar", "cdaddr",
                   "cddaar", "cddadr", "cdddar", "cddddr",
                   "cadar", "cddar",
                   // L18
                   "call/cc", "call-with-current-continuation",
                   // L19
                   "dynamic-wind",
                   // L20
                   "raise", "with-exception-handler", "guard",
                   // L21
                   "values", "call-with-values",
                   // L22
                   "syntax->datum", "datum->syntax"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

/// Evaluate a list of expressions using the fast old eval for expressions
/// without call/cc, switching to CEK for call/cc expressions and all
/// subsequent ones (so continuations capture the full remaining program).
fn eval_exprs(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    let mut last = Value::Void;
    for (i, expr) in exprs.iter().enumerate() {
        if cek::expr_uses_callcc(expr) {
            // Evaluate this and ALL remaining expressions with CEK,
            // so captured continuations include the rest of the program.
            return cek::cek_eval(&exprs[i..], env, out);
        }
        match eval(expr, env, out) {
            Ok(v) => last = v,
            Err(EvalError::ContinuationInvoked) => {
                // An escaped continuation (from a prior CEK eval) was invoked.
                let (kont, value) = cek::CONT_JUMP.with(|c| c.borrow_mut().take().expect("CONT_JUMP must be set after ContinuationInvoked"));
                return cek::cek_resume(kont, value, out);
            }
            Err(e) => return Err(e),
        }
    }
    Ok(last)
}

/// Reset all thread-local state to ensure isolation between eval_str calls.
fn reset_thread_locals() {
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
    EXCEPTION_HANDLERS.with(|h| h.borrow_mut().clear());
    SYNTAX_CASE_BINDINGS.with(|b| b.borrow_mut().clear());
    SYNTAX_RENAME_SINK.with(|s| s.borrow_mut().clear());
    STEP_LIMIT.with(|sl| *sl.borrow_mut() = None);
    cek::CONT_JUMP.with(|c| *c.borrow_mut() = None);
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    reset_thread_locals();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let last = eval_exprs(&exprs, &env, &out)?;
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    reset_thread_locals();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let last = eval_exprs(&exprs, &env, &out)?;
    let output = out.borrow().clone();
    Ok((last.format_display(), output))
}

/// Evaluate Scheme expressions with a step budget.
/// Each eval dispatch counts as one step; exceeding the limit returns an error.
pub fn eval_str_with_limit(input: &str, max_steps: u64) -> Result<String, EvalError> {
    reset_thread_locals();
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    STEP_LIMIT.with(|sl| *sl.borrow_mut() = Some(max_steps));
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let result = eval_exprs(&exprs, &env, &out);
    STEP_LIMIT.with(|sl| *sl.borrow_mut() = None);
    result.map(|v| v.display_value())
}

#[cfg(test)]
mod tests;
