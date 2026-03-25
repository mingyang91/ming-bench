pub mod error;
mod builtins;
mod eval_forms;
mod macros;
mod parser;
mod cek_forms;
mod special_forms;
mod winders;

pub use error::EvalError;
use builtins::*;
use cek_forms::*;
use macros::expand_macro;
use parser::parse_all;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static WINDER_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("#{}#{}", prefix, n)
}

/// Exception handler entry — installed by `guard` or `with-exception-handler`.
#[derive(Clone)]
pub(crate) enum ExcHandler {
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Env,
        saved_kont: Vec<KFrame>,
        saved_winders: Vec<Winder>,
    },
    Handler(Val),
}

/// A dynamic-wind frame: tracks in/out thunks for a dynamic extent.
#[derive(Clone)]
pub(crate) struct Winder {
    id: usize,
    in_thunk: Box<Val>,
    out_thunk: Box<Val>,
}

/// Actions to perform during continuation wind/unwind.
#[derive(Clone)]
pub(crate) enum DwAction {
    CallThunk(Box<Val>),
    PopWinder(usize),
    PushWinder(Winder),
}

pub(crate) type BuiltinFn = fn(&[Val], &Env) -> Result<Val, EvalError>;

#[derive(Clone)]
pub(crate) enum Val {
    Int(i64),
    Float(f64),
    Rational(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Val>),
    Pair(Rc<RefCell<(Val, Val)>>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Expr>)>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Val>>>),
    Builtin(BuiltinFn),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    SyntaxCaseMacro {
        transformer: Box<Val>,
        def_env: Env,
    },
    SyntaxObject(Box<Expr>),
    Void,
    /// The call/cc primitive as a first-class value.
    CallCC,
    /// The dynamic-wind primitive as a first-class value.
    DynamicWind,
    /// The raise primitive as a first-class value.
    Raise,
    /// The with-exception-handler primitive as a first-class value.
    WithExcHandler,
    /// The values primitive as a first-class value.
    Values,
    /// The call-with-values primitive as a first-class value.
    CallWithValues,
    /// A captured continuation (clone of the CEK continuation stack + winders).
    Continuation(Rc<Vec<KFrame>>, Rc<Vec<Winder>>),
    /// Multiple return values (from `values` with 0 or 2+ args).
    MultipleValues(Vec<Val>),
}

// --- CEK Machine Types ---

/// Continuation frame — one pending operation in the CEK machine.
#[derive(Clone)]
pub(crate) enum KFrame {
    /// Evaluated the operator of a call; now evaluate args right-to-left.
    CallOp {
        args: Vec<Expr>,
        env: Env,
        span: Span,
    },
    /// Evaluating arguments right-to-left; `done` accumulates in reverse order.
    CallArgs {
        func: Val,
        done: Vec<Val>,
        rest: Vec<Expr>,
        env: Env,
        span: Span,
    },
    /// Sequence of expressions — evaluate remaining, return last value.
    Seq {
        rest: Vec<Expr>,
        env: Env,
    },
    /// Bind value to a variable via `define`.
    Define {
        name: String,
        env: Env,
    },
    /// Assign value to a variable via `set!`.
    SetBang {
        name: String,
        env: Env,
        span: Span,
    },
    /// Branch on condition result.
    IfBranch {
        then_e: Expr,
        else_e: Option<Expr>,
        env: Env,
    },
    /// `and` — short-circuit evaluation.
    And {
        rest: Vec<Expr>,
        env: Env,
    },
    /// `or` — short-circuit evaluation.
    Or {
        rest: Vec<Expr>,
        env: Env,
    },
    /// dynamic-wind: in-thunk done, push winder, call body-thunk.
    DwAfterIn {
        body_thunk: Box<Val>,
        out_thunk: Box<Val>,
        winder: Winder,
        span: Span,
        env: Env,
    },
    /// dynamic-wind: body done, pop winder, call out-thunk, save result.
    DwAfterBody {
        out_thunk: Box<Val>,
        winder_id: usize,
        span: Span,
        env: Env,
    },
    /// dynamic-wind: out-thunk done, return saved body result.
    DwAfterOut {
        result: Box<Val>,
    },
    /// Continuation switch: run wind/unwind thunks then restore kont.
    DwSwitch {
        actions: Vec<DwAction>,
        value: Box<Val>,
        target_kont: Rc<Vec<KFrame>>,
        span: Span,
        env: Env,
    },
    /// Pop the top exception handler on normal body completion.
    PopExcHandler {
        env: Env,
    },
    /// Evaluate guard cond-clauses after exception unwinding completes.
    GuardClauses {
        var: String,
        obj: Box<Val>,
        clauses: Vec<Expr>,
        env: Env,
    },
    /// Sentinel: if a `raise` handler returns, this is an error.
    RaiseGuard,
    /// call-with-values: producer done, now apply consumer to its values.
    CwvConsumer {
        consumer: Val,
        span: Span,
        env: Env,
    },
    /// syntax-case: stx-expr evaluated, now match patterns.
    SyntaxCaseMatch {
        literals: Vec<String>,
        clauses: Vec<Expr>,
        env: Env,
    },
    /// syntax-case macro expansion: transformer returned, now eval the result.
    SyntaxCaseExpand {
        use_env: Env,
        def_env: Env,
    },
}

/// CEK machine state.
pub(crate) enum CekState {
    /// Evaluate an expression in an environment.
    Eval(Expr, Env),
    /// Apply a value to the current continuation (top frame).
    ApplyK(Val),
}

pub(crate) fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.abs();
    b = b.abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub(crate) fn make_rational(n: i64, d: i64) -> Val {
    if d == 0 {
        return Val::Int(0); // shouldn't happen
    }
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n.abs(), d);
    let n = n / g;
    let d = d / g;
    if d == 1 { Val::Int(n) } else { Val::Rational(n, d) }
}

pub(crate) fn make_pair(car: Val, cdr: Val) -> Val {
    Val::Pair(Rc::new(RefCell::new((car, cdr))))
}

pub(crate) fn vec_to_cons(items: Vec<Val>) -> Val {
    let mut result = Val::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

/// Collect elements from a proper list (either Val::List or cons chain) into a Vec.
pub(crate) fn collect_list(v: &Val) -> Result<Vec<Val>, EvalError> {
    match v {
        Val::List(elems) => Ok(elems.clone()),
        Val::Pair(_) => {
            let mut items = Vec::new();
            let mut cur = v.clone();
            loop {
                match &cur {
                    Val::List(elems) if elems.is_empty() => return Ok(items),
                    Val::Pair(rc) => {
                        let (car, cdr) = {
                            let pair = rc.borrow();
                            (pair.0.clone(), pair.1.clone())
                        };
                        items.push(car);
                        cur = cdr;
                    }
                    _ => return Err(EvalError::Type("expected proper list".into())),
                }
            }
        }
        _ => Err(EvalError::Type("expected list".into())),
    }
}

fn pair_cdr(v: &Val) -> Option<Val> {
    match v {
        Val::Pair(rc) => Some(rc.borrow().1.clone()),
        _ => None,
    }
}

fn pair_ptr(v: &Val) -> Option<usize> {
    match v {
        Val::Pair(rc) => Some(Rc::as_ptr(rc) as usize),
        _ => None,
    }
}

/// Check if a value is a proper list (nil-terminated), with cycle detection.
pub(crate) fn is_proper_list(v: &Val) -> bool {
    match v {
        Val::List(elems) => elems.is_empty(),
        Val::Pair(_) => {
            let mut tortoise = v.clone();
            let mut hare = v.clone();
            loop {
                // hare moves two steps
                for _ in 0..2 {
                    match pair_cdr(&hare) {
                        Some(next) => hare = next,
                        None => {
                            return matches!(hare, Val::List(ref e) if e.is_empty());
                        }
                    }
                }
                // tortoise moves one step
                if let Some(next) = pair_cdr(&tortoise) {
                    tortoise = next;
                } else {
                    return true;
                }
                // check cycle by Rc pointer
                if let (Some(tp), Some(hp)) = (pair_ptr(&tortoise), pair_ptr(&hare)) {
                    if tp == hp {
                        return false;
                    }
                }
            }
        }
        _ => false,
    }
}

impl Val {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
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
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Pair(rc) => {
                use std::collections::HashSet;
                let mut seen = HashSet::new();
                seen.insert(Rc::as_ptr(rc) as usize);
                let (car, cdr) = {
                    let pair = rc.borrow();
                    (format!("{}", pair.0), pair.1.clone())
                };
                write!(f, "({car}")?;
                let mut cur = cdr;
                loop {
                    match &cur {
                        Val::List(v) if v.is_empty() => break,
                        Val::List(v) => {
                            // Non-empty quoted list as tail: print elements inline
                            for e in v {
                                write!(f, " {e}")?;
                            }
                            break;
                        }
                        Val::Pair(rc2) => {
                            let ptr = Rc::as_ptr(rc2) as usize;
                            if !seen.insert(ptr) {
                                write!(f, " ...")?;
                                break;
                            }
                            let (car2, cdr2) = {
                                let p = rc2.borrow();
                                (format!("{}", p.0), p.1.clone())
                            };
                            write!(f, " {car2}")?;
                            cur = cdr2;
                        }
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Val::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::CaseLambda { .. } | Val::Builtin(..) | Val::Macro { .. }
            | Val::SyntaxCaseMacro { .. }
            | Val::CallCC | Val::DynamicWind | Val::Raise | Val::WithExcHandler
            | Val::Values | Val::CallWithValues
            | Val::Continuation(..) => write!(f, "#<procedure>"),
            Val::SyntaxObject(_) => write!(f, "#<syntax>"),
            Val::MultipleValues(_) => write!(f, "#<values>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Environment ---

type Frame = Rc<RefCell<HashMap<String, Val>>>;

#[derive(Clone)]
pub(crate) struct Env {
    frames: Vec<Frame>,
    pub(crate) output: Rc<RefCell<String>>,
    winders: Rc<RefCell<Vec<Winder>>>,
    exception_handlers: Rc<RefCell<Vec<ExcHandler>>>,
}

impl Env {
    fn new() -> Self {
        let frame = Rc::new(RefCell::new(HashMap::new()));
        let builtins: &[(&str, BuiltinFn)] = &[
            ("+", builtin_add as BuiltinFn),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_lt),
            (">", builtin_gt),
            ("=", builtin_eq),
            ("<=", builtin_le),
            (">=", builtin_ge),
            ("not", builtin_not),
            ("cons", builtin_cons as BuiltinFn),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("list", builtin_list),
            ("null?", builtin_null),
            ("length", builtin_length),
            ("append", builtin_append),
            ("boolean?", builtin_is_boolean),
            ("number?", builtin_is_number),
            ("string?", builtin_is_string),
            ("symbol?", builtin_is_symbol),
            ("pair?", builtin_is_pair),
            ("char?", builtin_is_char),
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
            ("string->list", builtin_string_to_list),
            ("list->string", builtin_list_to_string),
            ("char->integer", builtin_char_to_integer),
            ("integer->char", builtin_integer_to_char),
            ("apply", builtin_apply),
            ("equal?", builtin_equal),
            ("eq?", builtin_eq_pred),
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
            ("list?", builtin_is_list),
            ("assoc", builtin_assoc),
            ("map", builtin_map),
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
            ("integer?", builtin_is_integer),
            ("rational?", builtin_is_rational),
            ("exact?", builtin_is_exact),
            ("inexact?", builtin_is_inexact),
            ("exact->inexact", builtin_exact_to_inexact),
            ("inexact->exact", builtin_inexact_to_exact),
            ("numerator", builtin_numerator),
            ("denominator", builtin_denominator),
            ("procedure?", builtin_is_procedure),
            ("eqv?", builtin_eqv),
            ("vector", builtin_vector),
            ("make-vector", builtin_make_vector),
            ("vector-ref", builtin_vector_ref),
            ("vector-set!", builtin_vector_set),
            ("vector-length", builtin_vector_length),
            ("vector?", builtin_is_vector),
            ("vector->list", builtin_vector_to_list),
            ("list->vector", builtin_list_to_vector),
            ("for-each", builtin_for_each),
            ("assq", builtin_assq),
            ("assv", builtin_assv),
            ("memq", builtin_memq),
            ("memv", builtin_memv),
            ("member", builtin_member),
            ("reverse", builtin_reverse),
            ("gcd", builtin_gcd),
            ("lcm", builtin_lcm),
            ("truncate", builtin_truncate),
            ("round", builtin_round),
            ("floor", builtin_floor),
            ("ceiling", builtin_ceiling),
            ("make-string", builtin_make_string),
            ("string", builtin_string),
            ("string>?", builtin_string_gt),
            ("string<=?", builtin_string_le),
            ("string>=?", builtin_string_ge),
            ("error", builtin_error),
            ("null-environment", builtin_null_environment),
            ("char>?", builtin_char_gt),
            ("char<=?", builtin_char_le),
            ("char>=?", builtin_char_ge),
            ("vector-fill!", builtin_vector_fill),
            ("set-car!", builtin_set_car),
            ("set-cdr!", builtin_set_cdr),
            ("syntax->datum", builtin_syntax_to_datum),
            ("datum->syntax", builtin_datum_to_syntax),
        ];
        // call/cc as a first-class value
        frame.borrow_mut().insert("call/cc".to_string(), Val::CallCC);
        frame.borrow_mut().insert("call-with-current-continuation".to_string(), Val::CallCC);
        frame.borrow_mut().insert("dynamic-wind".to_string(), Val::DynamicWind);
        frame.borrow_mut().insert("raise".to_string(), Val::Raise);
        frame.borrow_mut().insert("with-exception-handler".to_string(), Val::WithExcHandler);
        frame.borrow_mut().insert("values".to_string(), Val::Values);
        frame.borrow_mut().insert("call-with-values".to_string(), Val::CallWithValues);
        for &(name, f) in builtins {
            frame.borrow_mut().insert(name.to_string(), Val::Builtin(f));
        }
        let env = Env { frames: vec![frame], output: Rc::new(RefCell::new(String::new())), winders: Rc::new(RefCell::new(Vec::new())), exception_handlers: Rc::new(RefCell::new(Vec::new())) };
        // Load cxr helper definitions
        let prelude = "\
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
";
        let exprs = parse_all(prelude).expect("prelude must parse");
        for expr in &exprs {
            eval(expr, &env).expect("prelude must eval");
        }
        env
    }

    pub(crate) fn get(&self, name: &str) -> Option<Val> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    pub(crate) fn define(&self, name: String, val: Val) {
        self.frames.last().expect("env has no frames").borrow_mut().insert(name, val);
    }

    pub(crate) fn set(&self, name: &str, val: Val) -> Result<(), EvalError> {
        for frame in self.frames.iter().rev() {
            let mut f = frame.borrow_mut();
            if f.contains_key(name) {
                f.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(EvalError::UnboundVariable(name.to_string()))
    }

    pub(crate) fn push(&self) -> Env {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(RefCell::new(HashMap::new())));
        Env { frames, output: Rc::clone(&self.output), winders: Rc::clone(&self.winders), exception_handlers: Rc::clone(&self.exception_handlers) }
    }
}

// --- Source Positions ---

#[derive(Debug, Clone, Copy)]
pub(crate) struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
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
    pub(crate) fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

// --- Evaluator ---

fn span_err(span: Span, err: EvalError) -> EvalError {
    // If the error message already contains position info, return as-is
    let msg = err.to_string();
    if msg.contains(':') && msg.bytes().any(|b| b.is_ascii_digit()) {
        // Check more carefully: look for digit:digit pattern
        let bytes = msg.as_bytes();
        let has_pos = bytes.windows(3).any(|w| {
            w[0].is_ascii_digit() && w[1] == b':' && w[2].is_ascii_digit()
        });
        if has_pos {
            return err;
        }
    }
    match err {
        EvalError::Parse(m) => EvalError::Parse(format!("{m} at {span}")),
        EvalError::Type(m) => EvalError::Type(format!("{m} at {span}")),
        EvalError::UnboundVariable(m) => EvalError::UnboundVariable(format!("{m} at {span}")),
        EvalError::Arity(m) => EvalError::Arity(format!("{m} at {span}")),
        EvalError::Runtime(m) => EvalError::Runtime(format!("{m} at {span}")),
    }
}

/// Evaluate a single expression using the CEK machine.
pub(crate) fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    eval_seq(std::slice::from_ref(expr), env)
}

/// Evaluate a sequence of expressions in a single CEK machine, returning the
/// last value.  All expressions share one continuation stack, which is
/// essential for `call/cc` to capture continuations across top-level forms.
pub(crate) fn eval_seq(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Void);
    }
    let mut kont: Vec<KFrame> = Vec::new();
    if exprs.len() > 1 {
        kont.push(KFrame::Seq { rest: exprs[1..].to_vec(), env: env.clone() });
    }
    let mut state = CekState::Eval(exprs[0].clone(), env.clone());

    loop {
        match state {
            CekState::Eval(expr, env) => {
                state = cek_step(expr, env, &mut kont)?;
            }
            CekState::ApplyK(val) => {
                match kont.pop() {
                    None => return Ok(val),
                    Some(frame) => {
                        state = apply_frame(frame, val, &mut kont)?;
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
//  CEK step: evaluate one expression, producing either a value (ApplyK) or
//  another expression to evaluate.
// ---------------------------------------------------------------------------

fn cek_step(expr: Expr, env: Env, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    let Expr { kind, span } = expr;
    match kind {
        ExprKind::Int(n) => Ok(CekState::ApplyK(Val::Int(n))),
        ExprKind::Float(x) => Ok(CekState::ApplyK(Val::Float(x))),
        ExprKind::Rational(n, d) => Ok(CekState::ApplyK(Val::Rational(n, d))),
        ExprKind::Bool(b) => Ok(CekState::ApplyK(Val::Bool(b))),
        ExprKind::Str(s) => Ok(CekState::ApplyK(Val::Str(s))),
        ExprKind::Char(c) => Ok(CekState::ApplyK(Val::Char(c))),
        ExprKind::Symbol(name) => {
            let val = env.get(&name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))?;
            Ok(CekState::ApplyK(val))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(CekState::ApplyK(Val::List(vec![])));
            }
            let maybe_op: Option<String> = match &elems[0].kind {
                ExprKind::Symbol(s) => Some(s.clone()),
                _ => None,
            };

            if let Some(ref op) = maybe_op {
                match op.as_str() {
                    // --- Immediate forms (no sub-expression eval in CEK spine) ---
                    "quote" => {
                        return Ok(CekState::ApplyK(eval_quote(&elems[1..], span)?));
                    }
                    "lambda" => {
                        return Ok(CekState::ApplyK(eval_lambda(&elems[1..], &env, span)?));
                    }
                    "case-lambda" => {
                        return Ok(CekState::ApplyK(eval_case_lambda(&elems[1..], &env, span)?));
                    }
                    "define-syntax" => {
                        return Ok(CekState::ApplyK(eval_define_syntax(&elems[1..], &env, span)?));
                    }
                    "syntax" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("syntax: expected 1 argument at {span}")));
                        }
                        return Ok(CekState::ApplyK(macros::eval_syntax_form(&elems[1], &env)));
                    }
                    "syntax-case" => {
                        // (syntax-case stx-expr (literals...) clause ...)
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("syntax-case: expected at least 2 arguments at {span}")));
                        }
                        let literals = match &elems[2].kind {
                            ExprKind::List(lits) => lits.iter().map(|e| match &e.kind {
                                ExprKind::Symbol(s) => Ok(s.clone()),
                                _ => Err(EvalError::Parse(format!("syntax-case: expected literal symbol at {span}"))),
                            }).collect::<Result<Vec<_>, _>>()?,
                            _ => return Err(EvalError::Parse(format!("syntax-case: expected literals list at {span}"))),
                        };
                        let clauses = elems[3..].to_vec();
                        kont.push(KFrame::SyntaxCaseMatch { literals, clauses, env: env.clone() });
                        return Ok(CekState::Eval(elems[1].clone(), env));
                    }
                    "with-syntax" => {
                        // (with-syntax ((var expr) ...) body ...)
                        // Desugar to let
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("with-syntax: expected bindings and body at {span}")));
                        }
                        let let_form = Expr::new(ExprKind::List({
                            let mut parts = vec![Expr::new(ExprKind::Symbol("let".into()), span)];
                            parts.push(elems[1].clone());
                            parts.extend_from_slice(&elems[2..]);
                            parts
                        }), span);
                        return Ok(CekState::Eval(let_form, env));
                    }
                    "define-record-type" => {
                        return Ok(CekState::ApplyK(eval_define_record_type(&elems[1..], &env, span)?));
                    }

                    // --- CEK-aware special forms ---
                    "define" => {
                        return cek_define(&elems[1..], env, span, kont);
                    }
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity(format!("set!: expected 2 arguments at {span}")));
                        }
                        let name = match &elems[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Parse(format!("set!: expected symbol at {span}"))),
                        };
                        kont.push(KFrame::SetBang { name, env: env.clone(), span });
                        return Ok(CekState::Eval(elems[2].clone(), env));
                    }
                    "if" => {
                        let nargs = elems.len() - 1;
                        if !(2..=3).contains(&nargs) {
                            return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {span}")));
                        }
                        let else_e = if nargs == 3 { Some(elems[3].clone()) } else { None };
                        kont.push(KFrame::IfBranch { then_e: elems[2].clone(), else_e, env: env.clone() });
                        return Ok(CekState::Eval(elems[1].clone(), env));
                    }
                    "begin" => {
                        if elems.len() <= 1 {
                            return Ok(CekState::ApplyK(Val::Void));
                        }
                        return enter_body_cek(&elems[1..], &env, kont);
                    }
                    "and" => {
                        if elems.len() <= 1 {
                            return Ok(CekState::ApplyK(Val::Bool(true)));
                        }
                        if elems.len() > 2 {
                            kont.push(KFrame::And { rest: elems[2..].to_vec(), env: env.clone() });
                        }
                        return Ok(CekState::Eval(elems[1].clone(), env));
                    }
                    "or" => {
                        if elems.len() <= 1 {
                            return Ok(CekState::ApplyK(Val::Bool(false)));
                        }
                        if elems.len() > 2 {
                            kont.push(KFrame::Or { rest: elems[2..].to_vec(), env: env.clone() });
                        }
                        return Ok(CekState::Eval(elems[1].clone(), env));
                    }

                    // --- Forms that use nested eval for sub-expressions ---
                    "cond" => return cek_cond(&elems[1..], &env, kont),
                    "let" => return cek_let(&elems, &env, span, kont),
                    "let*" => return cek_let_star(&elems[1..], &env, span, kont),
                    "letrec" => return cek_letrec(&elems[1..], &env, span, kont),
                    "letrec*" => return cek_letrec_star(&elems[1..], &env, span, kont),
                    "do" => return cek_do(&elems[1..], &env, span, kont),
                    "case" => return cek_case(&elems[1..], &env, span, kont),
                    "guard" => {
                        // (guard (var clause ...) body ...)
                        if elems.len() < 3 {
                            return Err(EvalError::Parse(format!("guard: expected at least 2 arguments at {span}")));
                        }
                        let guard_spec = match &elems[1].kind {
                            ExprKind::List(parts) if !parts.is_empty() => parts,
                            _ => return Err(EvalError::Parse(format!("guard: expected (var clause ...) at {span}"))),
                        };
                        let var = match &guard_spec[0].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Parse(format!("guard: expected variable name at {span}"))),
                        };
                        let clauses = guard_spec[1..].to_vec();
                        let saved_kont = kont.clone();
                        let saved_winders = env.winders.borrow().clone();
                        env.exception_handlers.borrow_mut().push(ExcHandler::Guard {
                            var,
                            clauses,
                            env: env.clone(),
                            saved_kont,
                            saved_winders,
                        });
                        kont.push(KFrame::PopExcHandler { env: env.clone() });
                        return enter_body_cek(&elems[2..], &env, kont);
                    }
                    "string-set!" => {
                        return Ok(CekState::ApplyK(eval_string_set(&elems[1..], &env, span)?));
                    }
                    "set-car!" => {
                        return Ok(CekState::ApplyK(eval_set_car(&elems[1..], &env, span)?));
                    }
                    "set-cdr!" => {
                        return Ok(CekState::ApplyK(eval_set_cdr(&elems[1..], &env, span)?));
                    }

                    _ => {
                        // Single lookup: check for macro or reuse value for application
                        match env.get(op) {
                            Some(Val::Macro { literals, rules, def_env }) => {
                                let (expanded, eval_env) = expand_macro(
                                    &elems, &literals, &rules, &def_env, &env, span,
                                )?;
                                return Ok(CekState::Eval(expanded, eval_env));
                            }
                            Some(Val::SyntaxCaseMacro { transformer, def_env }) => {
                                let call_expr = Expr::new(ExprKind::List(elems.clone()), span);
                                let stx_obj = Val::SyntaxObject(Box::new(call_expr));
                                kont.push(KFrame::SyntaxCaseExpand { use_env: env.clone(), def_env });
                                return apply_function_cek(*transformer, vec![stx_obj], kont, span, &env);
                            }
                            Some(resolved) => {
                                // Reuse the looked-up value directly for function application
                                kont.push(KFrame::CallOp { args: elems[1..].to_vec(), env: env.clone(), span });
                                return Ok(CekState::ApplyK(resolved));
                            }
                            None => {
                                return Err(EvalError::UnboundVariable(format!("{op} at {span}")));
                            }
                        }
                    }
                }
            }

            // --- Function application (non-symbol operator) ---
            // Push CallOp frame; evaluate the operator first
            kont.push(KFrame::CallOp { args: elems[1..].to_vec(), env: env.clone(), span });
            Ok(CekState::Eval(elems[0].clone(), env))
        }
    }
}

// ---------------------------------------------------------------------------
//  Apply a value to the top continuation frame.
// ---------------------------------------------------------------------------

fn apply_frame(frame: KFrame, val: Val, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    match frame {
        KFrame::CallOp { mut args, env, span } => {
            // val = evaluated operator.  Evaluate args right-to-left.
            if args.is_empty() {
                apply_function_cek(val, vec![], kont, span, &env)
            } else {
                let next = args.pop().expect("args verified non-empty");
                kont.push(KFrame::CallArgs { func: val, done: vec![], rest: args, env: env.clone(), span });
                Ok(CekState::Eval(next, env))
            }
        }
        KFrame::CallArgs { func, mut done, mut rest, env, span } => {
            // val = just-evaluated argument (right-to-left order)
            done.push(val);
            if rest.is_empty() {
                done.reverse(); // convert R-to-L accumulation → L-to-R argument order
                apply_function_cek(func, done, kont, span, &env)
            } else {
                let next = rest.pop().expect("rest verified non-empty");
                kont.push(KFrame::CallArgs { func, done, rest, env: env.clone(), span });
                Ok(CekState::Eval(next, env))
            }
        }
        KFrame::Seq { rest, env } => {
            // Discard val (not the last expr); evaluate remaining body.
            enter_body_cek(&rest, &env, kont)
        }
        KFrame::Define { name, env } => {
            env.define(name, val);
            Ok(CekState::ApplyK(Val::Void))
        }
        KFrame::SetBang { name, env, span } => {
            env.set(&name, val).map_err(|e| span_err(span, e))?;
            Ok(CekState::ApplyK(Val::Void))
        }
        KFrame::IfBranch { then_e, else_e, env } => {
            if val.is_truthy() {
                Ok(CekState::Eval(then_e, env))
            } else if let Some(e) = else_e {
                Ok(CekState::Eval(e, env))
            } else {
                Ok(CekState::ApplyK(Val::Void))
            }
        }
        KFrame::And { rest, env } => {
            if !val.is_truthy() {
                Ok(CekState::ApplyK(val))
            } else if rest.len() == 1 {
                // Tail position
                Ok(CekState::Eval(rest.into_iter().next().expect("rest len verified == 1"), env))
            } else {
                kont.push(KFrame::And { rest: rest[1..].to_vec(), env: env.clone() });
                Ok(CekState::Eval(rest[0].clone(), env))
            }
        }
        KFrame::Or { rest, env } => {
            if val.is_truthy() {
                Ok(CekState::ApplyK(val))
            } else if rest.len() == 1 {
                Ok(CekState::Eval(rest.into_iter().next().expect("rest len verified == 1"), env))
            } else {
                kont.push(KFrame::Or { rest: rest[1..].to_vec(), env: env.clone() });
                Ok(CekState::Eval(rest[0].clone(), env))
            }
        }
        KFrame::DwAfterIn { body_thunk, out_thunk, winder, span, env } => {
            // in-thunk completed (val ignored). Push winder, call body.
            let winder_id = winder.id;
            env.winders.borrow_mut().push(winder);
            kont.push(KFrame::DwAfterBody { out_thunk, winder_id, span, env: env.clone() });
            apply_function_cek(*body_thunk, vec![], kont, span, &env)
        }
        KFrame::DwAfterBody { out_thunk, winder_id, span, env } => {
            // body completed, val = body result. Pop winder, call out-thunk.
            {
                let mut w = env.winders.borrow_mut();
                if let Some(pos) = w.iter().rposition(|x| x.id == winder_id) {
                    w.remove(pos);
                }
            }
            kont.push(KFrame::DwAfterOut { result: Box::new(val) });
            apply_function_cek(*out_thunk, vec![], kont, span, &env)
        }
        KFrame::DwAfterOut { result } => {
            // out-thunk completed (val ignored). Return saved body result.
            Ok(CekState::ApplyK(*result))
        }
        KFrame::DwSwitch { actions, value, target_kont, span, env } => {
            // A wind/unwind thunk completed (val ignored). Continue with remaining actions.
            process_dw_actions(actions, *value, target_kont, kont, span, &env)
        }
        KFrame::PopExcHandler { env } => {
            // Normal body completion — pop the exception handler.
            env.exception_handlers.borrow_mut().pop();
            Ok(CekState::ApplyK(val))
        }
        KFrame::GuardClauses { var, obj, clauses, env } => {
            // Exception unwinding complete — evaluate guard cond-clauses.
            let clause_env = env.push();
            clause_env.define(var, *obj);
            eval_guard_clauses(&clauses, &clause_env, kont)
        }
        KFrame::RaiseGuard => {
            // A with-exception-handler handler returned from raise — error.
            Err(EvalError::Runtime("exception handler returned from raise".into()))
        }
        KFrame::CwvConsumer { consumer, span, env } => {
            // Producer completed — val is the result. Unpack MultipleValues.
            let args = match val {
                Val::MultipleValues(vals) => vals,
                other => vec![other],
            };
            apply_function_cek(consumer, args, kont, span, &env)
        }
        KFrame::SyntaxCaseMatch { literals, clauses, env } => {
            let stx_expr = match &val {
                Val::SyntaxObject(e) => (**e).clone(),
                _ => return Err(EvalError::Type("syntax-case: expected syntax object".into())),
            };
            macros::eval_syntax_case_match(stx_expr, &literals, &clauses, &env, kont)
        }
        KFrame::SyntaxCaseExpand { use_env, def_env } => {
            match val {
                Val::SyntaxObject(expr) => {
                    // Apply hygiene: rename free vars from def_env
                    let (expanded, eval_env) = macros::apply_syntax_hygiene(*expr, &def_env, &use_env);
                    Ok(CekState::Eval(expanded, eval_env))
                }
                _ => Err(EvalError::Type("syntax-case macro must return a syntax object".into())),
            }
        }
    }
}

// ---------------------------------------------------------------------------
//  Function application in the CEK machine.
// ---------------------------------------------------------------------------

fn apply_function_cek(
    func: Val,
    args: Vec<Val>,
    kont: &mut Vec<KFrame>,
    span: Span,
    caller_env: &Env,
) -> Result<CekState, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env } => {
            let new_env = eval_forms::bind_lambda_args(&params, &rest_param, &args, &env, span)?;
            enter_body_cek(&body, &new_env, kont)
        }
        Val::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let new_env = eval_forms::bind_lambda_args(&params, &rest_param, &args, &env, span)?;
                    return enter_body_cek(&body, &new_env, kont);
                }
            }
            Err(EvalError::Arity(format!("no matching clause for {} arguments", args.len())))
        }
        Val::Builtin(f) => {
            let result = f(&args, caller_env).map_err(|e| span_err(span, e))?;
            Ok(CekState::ApplyK(result))
        }
        Val::CallCC => {
            // (call/cc proc): capture current continuation, call proc with it
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("call/cc: expected 1 argument, got {} at {span}", args.len())));
            }
            let saved_kont = kont.clone();
            let saved_winders = caller_env.winders.borrow().clone();
            let cont_val = Val::Continuation(Rc::new(saved_kont), Rc::new(saved_winders));
            // Apply the procedure argument to [continuation]
            apply_function_cek(args.into_iter().next().expect("args len verified == 1"), vec![cont_val], kont, span, caller_env)
        }
        Val::DynamicWind => {
            // (dynamic-wind in-thunk body-thunk out-thunk)
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("dynamic-wind: expected 3 arguments, got {} at {span}", args.len())));
            }
            let mut it = args.into_iter();
            let in_thunk = it.next().expect("arity checked");
            let body_thunk = it.next().expect("arity checked");
            let out_thunk = it.next().expect("arity checked");
            let winder_id = WINDER_COUNTER.fetch_add(1, Ordering::SeqCst);
            let winder = Winder { id: winder_id, in_thunk: Box::new(in_thunk.clone()), out_thunk: Box::new(out_thunk.clone()) };
            // Push frame to handle what happens after in-thunk completes
            kont.push(KFrame::DwAfterIn { body_thunk: Box::new(body_thunk), out_thunk: Box::new(out_thunk), winder, span, env: caller_env.clone() });
            // Call the in-thunk
            apply_function_cek(in_thunk, vec![], kont, span, caller_env)
        }
        Val::Raise => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("raise: expected 1 argument, got {} at {span}", args.len())));
            }
            let obj = args.into_iter().next().expect("arity checked");
            let handler = caller_env.exception_handlers.borrow_mut().pop();
            match handler {
                Some(ExcHandler::Guard { var, clauses, env: guard_env, saved_kont, saved_winders }) => {
                    let current_winders = caller_env.winders.borrow().clone();
                    let actions = compute_wind_actions(&current_winders, &saved_winders);
                    let mut target = saved_kont;
                    target.push(KFrame::GuardClauses {
                        var,
                        obj: Box::new(obj),
                        clauses,
                        env: guard_env,
                    });
                    if actions.is_empty() {
                        *kont = target;
                        Ok(CekState::ApplyK(Val::Void))
                    } else {
                        process_dw_actions(actions, Val::Void, Rc::new(target), kont, span, caller_env)
                    }
                }
                Some(ExcHandler::Handler(handler_fn)) => {
                    kont.push(KFrame::RaiseGuard);
                    apply_function_cek(handler_fn, vec![obj], kont, span, caller_env)
                }
                None => {
                    Err(EvalError::Runtime(format!("unhandled exception: {obj}")))
                }
            }
        }
        Val::WithExcHandler => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("with-exception-handler: expected 2 arguments, got {} at {span}", args.len())));
            }
            let mut it = args.into_iter();
            let handler = it.next().expect("arity checked");
            let thunk = it.next().expect("arity checked");
            caller_env.exception_handlers.borrow_mut().push(ExcHandler::Handler(handler));
            kont.push(KFrame::PopExcHandler { env: caller_env.clone() });
            apply_function_cek(thunk, vec![], kont, span, caller_env)
        }
        Val::Values => {
            // (values) → MultipleValues([])
            // (values x) → x  (transparent single value)
            // (values x y ...) → MultipleValues([x, y, ...])
            if args.len() == 1 {
                Ok(CekState::ApplyK(args.into_iter().next().expect("len checked == 1")))
            } else {
                Ok(CekState::ApplyK(Val::MultipleValues(args)))
            }
        }
        Val::CallWithValues => {
            // (call-with-values producer consumer)
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("call-with-values: expected 2 arguments, got {} at {span}", args.len())));
            }
            let mut it = args.into_iter();
            let producer = it.next().expect("arity checked");
            let consumer = it.next().expect("arity checked");
            // Push frame to capture producer result, then apply consumer
            kont.push(KFrame::CwvConsumer { consumer, span, env: caller_env.clone() });
            // Call the producer thunk with no arguments
            apply_function_cek(producer, vec![], kont, span, caller_env)
        }
        Val::Continuation(saved_kont, saved_winders) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "continuation: expected 1 argument, got {} at {span}", args.len()
                )));
            }
            let value = args.into_iter().next().expect("args len verified == 1");
            let current_winders = caller_env.winders.borrow().clone();
            let actions = compute_wind_actions(&current_winders, &saved_winders);
            if actions.is_empty() {
                *kont = (*saved_kont).clone();
                Ok(CekState::ApplyK(value))
            } else {
                process_dw_actions(actions, value, saved_kont, kont, span, caller_env)
            }
        }
        _ => Err(EvalError::Type(format!("not a procedure at {span}"))),
    }
}

// ---------------------------------------------------------------------------
//  dynamic-wind helpers
// ---------------------------------------------------------------------------

/// Compute the sequence of wind/unwind actions needed to switch from
/// `current` winders to `target` winders.
use winders::compute_wind_actions;

/// Process a list of wind/unwind actions. Non-thunk actions (push/pop winder)
/// are executed immediately; thunk calls push a DwSwitch frame and return.
fn process_dw_actions(
    mut actions: Vec<DwAction>,
    value: Val,
    target_kont: Rc<Vec<KFrame>>,
    kont: &mut Vec<KFrame>,
    span: Span,
    env: &Env,
) -> Result<CekState, EvalError> {
    while !actions.is_empty() {
        match actions.remove(0) {
            DwAction::CallThunk(thunk) => {
                kont.push(KFrame::DwSwitch {
                    actions,
                    value: Box::new(value),
                    target_kont,
                    span,
                    env: env.clone(),
                });
                return apply_function_cek(*thunk, vec![], kont, span, env);
            }
            DwAction::PopWinder(id) => {
                let mut w = env.winders.borrow_mut();
                if let Some(pos) = w.iter().rposition(|x| x.id == id) {
                    w.remove(pos);
                }
            }
            DwAction::PushWinder(winder) => {
                env.winders.borrow_mut().push(winder);
            }
        }
    }
    // All actions done — restore target continuation and deliver value.
    *kont = (*target_kont).clone();
    Ok(CekState::ApplyK(value))
}

// ---------------------------------------------------------------------------
//  Exception handling helpers
// ---------------------------------------------------------------------------

/// Evaluate guard cond-clauses with the exception variable already bound.
fn eval_guard_clauses(clauses: &[Expr], env: &Env, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    for clause in clauses {
        let parts = match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => parts,
            _ => return Err(EvalError::Parse("guard: invalid clause".into())),
        };
        if matches!(&parts[0].kind, ExprKind::Symbol(s) if s == "else") {
            return enter_body_cek(&parts[1..], env, kont);
        }
        let test = eval(&parts[0], env)?;
        if test.is_truthy() {
            if parts.len() <= 1 {
                return Ok(CekState::ApplyK(test));
            }
            return enter_body_cek(&parts[1..], env, kont);
        }
    }
    // No clause matched — re-raise
    Err(EvalError::Runtime("guard: no matching clause and no else".into()))
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

/// Enter a body (sequence of expressions) in the CEK machine, with TCO
/// for the last expression.
pub(crate) fn enter_body_cek(body: &[Expr], env: &Env, kont: &mut Vec<KFrame>) -> Result<CekState, EvalError> {
    if body.is_empty() {
        return Ok(CekState::ApplyK(Val::Void));
    }
    if body.len() > 1 {
        kont.push(KFrame::Seq { rest: body[1..].to_vec(), env: env.clone() });
    }
    Ok(CekState::Eval(body[0].clone(), env.clone()))
}

pub(crate) fn apply_val(func: &Val, args: &[Val], caller_env: &Env) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env } => {
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                new_env.define(rest.clone(), vec_to_cons(args[params.len()..].to_vec()));
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            }
        }
        Val::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if let Some(_rest) = rest_param {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let new_env = env.push();
                    for (p, a) in params.iter().zip(args.iter()) {
                        new_env.define(p.clone(), a.clone());
                    }
                    if let Some(rest) = rest_param {
                        new_env.define(rest.clone(), vec_to_cons(args[params.len()..].to_vec()));
                    }
                    let mut result = Val::Void;
                    for expr in body {
                        result = eval(expr, &new_env)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "no matching clause for {} arguments", args.len()
            )))
        }
        Val::Builtin(f) => f(args, caller_env),
        Val::CallCC => {
            // call/cc invoked via apply_val (e.g. from builtin `apply`)
            if args.len() != 1 {
                return Err(EvalError::Arity("call/cc: expected 1 argument".into()));
            }
            // No CEK kont to capture here; just call the proc with a dummy continuation
            // that raises an error if invoked.
            let cont = Val::Continuation(Rc::new(vec![]), Rc::new(vec![]));
            apply_val(&args[0], &[cont], caller_env)
        }
        Val::DynamicWind => {
            // dynamic-wind invoked outside CEK machine
            if args.len() != 3 {
                return Err(EvalError::Arity("dynamic-wind: expected 3 arguments".into()));
            }
            apply_val(&args[0], &[], caller_env)?;
            let result = apply_val(&args[1], &[], caller_env)?;
            apply_val(&args[2], &[], caller_env)?;
            Ok(result)
        }
        Val::Continuation(..) => {
            // Continuation invoked from outside the CEK machine (e.g. inside map).
            // This is a limitation — full support would require all evaluation
            // paths to go through the CEK machine.
            Err(EvalError::Runtime("continuation invoked outside CEK machine".into()))
        }
        Val::Raise => {
            if args.len() != 1 {
                return Err(EvalError::Arity("raise: expected 1 argument".into()));
            }
            Err(EvalError::Runtime(format!("unhandled exception: {}", args[0])))
        }
        Val::WithExcHandler => {
            if args.len() != 2 {
                return Err(EvalError::Arity("with-exception-handler: expected 2 arguments".into()));
            }
            apply_val(&args[1], &[], caller_env)
        }
        Val::Values => {
            if args.len() == 1 {
                Ok(args[0].clone())
            } else {
                Ok(Val::MultipleValues(args.to_vec()))
            }
        }
        Val::CallWithValues => {
            if args.len() != 2 {
                return Err(EvalError::Arity("call-with-values: expected 2 arguments".into()));
            }
            let producer_result = apply_val(&args[0], &[], caller_env)?;
            let consumer_args = match producer_result {
                Val::MultipleValues(vals) => vals,
                other => vec![other],
            };
            apply_val(&args[1], &consumer_args, caller_env)
        }
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

use special_forms::{
    eval_define_record_type, eval_define_syntax,
    eval_quote, eval_lambda, eval_case_lambda,
    eval_string_set, eval_set_car, eval_set_cdr,
};

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let last = eval_seq(&exprs, &env)?;
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let last = eval_seq(&exprs, &env)?;
    let output = env.output.borrow().clone();
    Ok((last.to_string(), output))
}

#[cfg(test)]
mod tests;
