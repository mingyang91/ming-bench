pub mod error;
mod builtins;
mod forms;
mod macros;
mod parser;
mod values;
mod wind;

use parser::{parse_all, Expr, Pos};

pub use error::EvalError;
use builtins::apply_builtin;
use forms::{
    cek_eval_and, cek_eval_case, cek_eval_cond, cek_eval_define, cek_eval_do, cek_eval_let,
    cek_eval_let_star, cek_eval_letrec, cek_eval_letrec_star, cek_eval_or,
    cek_eval_string_set, case_clause_matches, eval_case_lambda, eval_define_record_type,
    eval_lambda, expr_to_value, make_begin,
};
use macros::{eval_define_syntax, expand_macro, expand_syntax_form, match_pattern};
use values::{values_eq, values_equal, values_eqv, is_proper_list, to_list_vec};
use wind::{apply_wind_step, apply_continuation};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;

/// A wind entry: unique id + in-thunk + out-thunk.
#[derive(Debug, Clone)]
pub(super) struct WindEntry {
    id: u64,
    in_thunk: Value,
    out_thunk: Value,
}

static WIND_ID_COUNTER: AtomicU64 = AtomicU64::new(0);

fn next_wind_id() -> u64 {
    WIND_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

/// Exception handler stack entry.
#[derive(Clone)]
enum HandlerEntry {
    Guard {
        var: String,
        clauses: Vec<Expr>,
        env: Env,
        kont: Kont,
        wind_stack: Vec<WindEntry>,
    },
    User(Value),
}

fn common_prefix_len(a: &[WindEntry], b: &[WindEntry]) -> usize {
    a.iter().zip(b.iter()).take_while(|(x, y)| x.id == y.id).count()
}

#[derive(Debug, Clone)]
pub(super) struct CaseLambdaClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: Env,
}

pub(super) type Kont = Rc<KontFrame>;

fn halt_kont() -> Kont {
    Rc::new(KontFrame::Halt)
}

/// A Scheme value.
#[derive(Debug, Clone)]
pub(super) enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0, denom != 1)
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<RefCell<(Value, Value)>>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Char(char),
    Builtin(String),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Record {
        type_id: u64,
        type_name: String,
        fields: Vec<(String, Value)>,
    },
    RecordConstructor {
        type_id: u64,
        type_name: String,
        field_names: Vec<String>,
    },
    RecordPredicate {
        type_id: u64,
    },
    RecordAccessor {
        type_id: u64,
        field_name: String,
    },
    CaseLambda {
        clauses: Vec<CaseLambdaClause>,
    },
    Continuation(Kont, Vec<WindEntry>),
    Values(Vec<Value>),
    MacroTransformer(Box<Value>),
    SyntaxObject(Box<Expr>, Vec<(String, Value)>),  // (expr, hygiene_bindings)
    SyntaxList(Vec<Expr>),
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

pub(super) fn make_rational(n: i64, d: i64) -> Value {
    let (n, d) = if d < 0 { (-n, -d) } else { (n, d) };
    let g = gcd(n.abs(), d);
    let (n, d) = (n / g, d / g);
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

pub(super) fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

pub(super) fn vec_to_pair_chain(elems: &[Value]) -> Value {
    let mut result = Value::List(vec![]);
    for e in elems.iter().rev() {
        result = make_pair(e.clone(), result);
    }
    result
}

// ---------- Environment ----------

pub(super) type Env = Rc<RefCell<EnvInner>>;

pub(super) struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl fmt::Debug for EnvInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Env")
            .field("bindings", &self.bindings.keys().collect::<Vec<_>>())
            .finish()
    }
}

pub(super) fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
}

fn make_hygiene_env(base: &Env, bindings: &[(String, Value)]) -> Env {
    if bindings.is_empty() {
        return base.clone();
    }
    let hyg_env = new_env(Some(base.clone()));
    for (name, val) in bindings {
        env_set(&hyg_env, name.clone(), val.clone());
    }
    hyg_env
}

pub(super) fn env_get(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Some(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        None
    }
}

pub(super) fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_update(env: &Env, name: &str, val: Value) -> bool {
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
    for &name in BUILTINS {
        env_set(&env, name.to_string(), Value::Builtin(name.to_string()));
    }
    env
}

// ---------- Evaluator ----------

pub(super) fn is_truthy(v: &Value) -> bool {
    !matches!(v, Value::Boolean(false))
}

pub(super) const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", ">", "=", "<=", ">=",
    "cons", "car", "cdr", "null?", "list", "length", "append",
    "number?", "boolean?", "string?", "pair?", "symbol?",
    "display", "write", "newline",
    "string-append", "string-length", "substring",
    "string->number", "number->string",
    "symbol->string", "string->symbol",
    "string-ref", "char?", "string-copy",
    "apply", "eq?", "equal?", "map",
    "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
    "zero?", "positive?", "negative?", "odd?", "even?",
    "integer?", "rational?", "exact?", "inexact?",
    "exact->inexact", "inexact->exact",
    "numerator", "denominator",
    "list-ref", "list-tail", "list?", "assoc",
    "char=?", "char<?", "char-alphabetic?", "char-numeric?",
    "char-upcase", "char-downcase",
    "string=?", "string<?", "string-ci=?",
    "string-upcase", "string-downcase",
    "string->list", "list->string",
    "char->integer", "integer->char",
    "procedure?",
    "eqv?",
    "vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
    "vector->list", "list->vector",
    "memq", "assq",
    "for-each",
    "set-car!", "set-cdr!",
    "error",
    "caar", "cadr", "cdar", "cddr", "caddr", "cadddr", "cadar", "caddar",
    "reverse",
    "member", "assv",
    "make-string", "string",
    "truncate", "round",
    "string>?", "string<=?", "string>=?",
    "gcd", "lcm",
    "call/cc", "call-with-current-continuation",
    "dynamic-wind",
    "raise", "with-exception-handler",
    "values", "call-with-values",
    "syntax->datum", "datum->syntax",
];

// ---------- CEK Machine ----------

/// Continuation frames — explicit representation of "what to do next".
pub(super) enum KontFrame {
    Halt,
    /// Sequence: discard current value, eval rest[0], ... rest[-1] goes to next
    Seq { rest: Vec<Expr>, env: Env, next: Kont },
    /// Evaluated function position of call — now evaluate arguments
    EvCallFunc { arg_exprs: Vec<Expr>, env: Env, pos: Pos, next: Kont },
    /// Evaluating arguments of a function call
    EvCallArgs { func: Value, all_arg_exprs: Vec<Expr>, eval_idx: usize, done: Vec<Value>, env: Env, pos: Pos, next: Kont },
    /// Evaluated test of `if`
    EvIf { then_e: Expr, else_e: Option<Expr>, env: Env, next: Kont },
    /// Evaluating value for `define`
    EvDefine { name: String, env: Env, next: Kont },
    /// Evaluating value for `set!`
    EvSet { name: String, env: Env, pos: Pos, next: Kont },
    /// Evaluating argument of `not`
    EvNot { next: Kont },
    /// And chain
    EvAnd { rest: Vec<Expr>, env: Env, next: Kont },
    /// Or chain
    EvOr { rest: Vec<Expr>, env: Env, next: Kont },
    /// Cond: evaluated test — if truthy eval body, else try rest_clauses
    EvCondTest { body: Vec<Expr>, rest_clauses: Vec<Expr>, env: Env, next: Kont },
    /// Let bindings (inits evaluated in outer_env, bound in local_env)
    EvLetBind { name: String, rest: Vec<(String, Expr)>, outer_env: Env, local_env: Env, body: Vec<Expr>, next: Kont },
    /// Let*/letrec bindings (inits evaluated in local_env)
    EvLetSeqBind { name: String, rest: Vec<(String, Expr)>, local_env: Env, body: Vec<Expr>, next: Kont },
    /// String-set!: evaluated index, need char
    EvStrSetIdx { var_name: String, char_expr: Expr, env: Env, pos: Pos, next: Kont },
    /// String-set!: evaluated char, perform mutation
    EvStrSetChar { var_name: String, idx: usize, env: Env, pos: Pos, next: Kont },
    /// Case: evaluated key
    EvCaseKey { clauses: Vec<Expr>, env: Env, next: Kont },
    /// Map: iterating
    EvMap { func: Value, lists: Vec<Vec<Value>>, idx: usize, results: Vec<Value>, pos: Pos, next: Kont },
    /// ForEach: iterating
    EvForEach { func: Value, lists: Vec<Vec<Value>>, idx: usize, pos: Pos, next: Kont },
    /// dynamic-wind: after in-thunk, call body
    EvDynWindAfterIn { in_thunk: Value, body_thunk: Value, out_thunk: Value, pos: Pos, next: Kont },
    /// dynamic-wind: after body, save result, call out-thunk
    EvDynWindAfterBody { out_thunk: Value, pos: Pos, next: Kont },
    /// dynamic-wind: after out-thunk, return saved body value
    EvDynWindAfterOut { body_val: Value, next: Kont },
    /// Winding: run unwind out-thunks then rewind in-thunks, then apply continuation
    EvWind { unwind_outs: Vec<Value>, rewind_ins: Vec<Value>, target_ws: Vec<WindEntry>, val: Value, target_kont: Kont, pos: Pos },
    /// Winding: after an out-thunk or in-thunk call, continue winding
    EvWindStep { unwind_outs: Vec<Value>, rewind_ins: Vec<Value>, target_ws: Vec<WindEntry>, val: Value, target_kont: Kont, pos: Pos },
    /// guard: setup — push handler onto handler_stack, then eval body
    EvGuardSetup { var: String, clauses: Vec<Expr>, body: Vec<Expr>, env: Env, next: Kont },
    /// guard: pop handler on normal body return
    EvGuardBody { next: Kont },
    /// guard: evaluate cond clauses after exception (val = exception value)
    EvGuardClauses { var: String, clauses: Vec<Expr>, env: Env, next: Kont },
    /// with-exception-handler: pop handler on normal thunk return
    EvWithExcHandler { next: Kont },
    /// raise: error if handler returns from non-continuable raise
    EvRaiseContinuationError,
    /// call-with-values: producer done, invoke consumer with result values
    EvCallWithValues { consumer: Value, pos: Pos, next: Kont },
    /// syntax-case: stx-expr evaluated, now pattern-match
    EvSyntaxCase { literals: Vec<String>, clauses: Vec<Expr>, env: Env, next: Kont },
    /// macro transformer returned a syntax object, unwrap and evaluate
    EvMacroExpand { env: Env, hygiene: Vec<(String, Value)>, next: Kont },
}

impl fmt::Debug for KontFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KontFrame::Halt => write!(f, "Halt"),
            KontFrame::Seq { .. } => write!(f, "Seq(..)"),
            _ => write!(f, "KontFrame(..)"),
        }
    }
}

/// CEK machine state.
pub(super) enum State {
    /// Evaluate an expression in an environment with a continuation
    Eval(Expr, Env, Kont),
    /// Return a value to a continuation
    Apply(Value, Kont),
    /// Invoke a function with arguments
    Invoke(Value, Vec<Value>, Pos, Kont),
    /// Terminal
    Done(Value),
}

/// Main CEK loop: evaluate a sequence of expressions.
fn cek_run(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }
    let mut wind_stack: Vec<WindEntry> = vec![];
    let mut handler_stack: Vec<HandlerEntry> = vec![];
    let kont = halt_kont();
    let mut state = eval_body_state(exprs, env.clone(), kont);
    loop {
        match state {
            State::Done(val) => return Ok(val),
            State::Eval(ref expr, ref env, ref kont) => {
                let e = expr.clone();
                let env = env.clone();
                let kont = kont.clone();
                state = cek_eval(&e, &env, kont, output)?;
            }
            State::Apply(val, kont) => {
                state = cek_apply_kont(val, &kont, &mut wind_stack, &mut handler_stack, output)?;
            }
            State::Invoke(func, args, pos, kont) => {
                state = cek_invoke(func, args, pos, kont, &mut wind_stack, &mut handler_stack, output)?;
            }
        }
    }
}

/// Set up state to evaluate a body (sequence of expressions).
pub(super) fn eval_body_state(body: &[Expr], env: Env, kont: Kont) -> State {
    if body.is_empty() {
        State::Apply(Value::Void, kont)
    } else if body.len() == 1 {
        State::Eval(body[0].clone(), env, kont)
    } else {
        let kont = Rc::new(KontFrame::Seq {
            rest: body[1..].to_vec(),
            env: env.clone(),
            next: kont,
        });
        State::Eval(body[0].clone(), env, kont)
    }
}

/// Try to evaluate an expression eagerly without creating continuation frames.
/// Returns None if the expression requires CPS (special forms, lambdas, call/cc, etc.)
pub(super) fn eval_simple(expr: &Expr, env: &Env, output: &mut String) -> Option<Result<Value, EvalError>> {
    match expr {
        Expr::Integer(n, _) => Some(Ok(Value::Integer(*n))),
        Expr::Float(f, _) => Some(Ok(Value::Float(*f))),
        Expr::Rational(n, d, _) => Some(Ok(make_rational(*n, *d))),
        Expr::Boolean(b, _) => Some(Ok(Value::Boolean(*b))),
        Expr::Str(s, _) => Some(Ok(Value::Str(s.clone()))),
        Expr::Char(c, _) => Some(Ok(Value::Char(*c))),
        Expr::Symbol(name, _) => {
            let p = expr.pos();
            Some(env_get(env, name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}"))))
        }
        Expr::List(elems, _) if !elems.is_empty() => {
            if let Expr::Symbol(op, _) = &elems[0] {
                if let Some(Value::Builtin(name)) = env_get(env, op) {
                    // Don't eagerly evaluate CPS-requiring builtins
                    if matches!(name.as_str(), "call/cc" | "call-with-current-continuation"
                        | "apply" | "map" | "for-each" | "dynamic-wind"
                        | "raise" | "with-exception-handler"
                        | "values" | "call-with-values") {
                        return None;
                    }
                    let mut args = Vec::with_capacity(elems.len() - 1);
                    for e in &elems[1..] {
                        match eval_simple(e, env, output) {
                            Some(Ok(v)) => args.push(v),
                            Some(Err(e)) => return Some(Err(e)),
                            None => return None,
                        }
                    }
                    return Some(apply_builtin(&name, &args, expr.pos(), output));
                }
                // quote
                if op == "quote" && elems.len() == 2 {
                    return Some(Ok(expr_to_value(&elems[1])));
                }
            }
            None
        }
        _ => None,
    }
}

/// CEK step: evaluate expression.
fn cek_eval(expr: &Expr, env: &Env, kont: Kont, _output: &mut String) -> Result<State, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(State::Apply(Value::Integer(*n), kont)),
        Expr::Float(f, _) => Ok(State::Apply(Value::Float(*f), kont)),
        Expr::Rational(n, d, _) => Ok(State::Apply(make_rational(*n, *d), kont)),
        Expr::Boolean(b, _) => Ok(State::Apply(Value::Boolean(*b), kont)),
        Expr::Str(s, _) => Ok(State::Apply(Value::Str(s.clone()), kont)),
        Expr::Char(c, _) => Ok(State::Apply(Value::Char(*c), kont)),
        Expr::Symbol(name, _) => {
            env_get(env, name)
                .map(|v| State::Apply(v, kont))
                .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(State::Apply(Value::List(vec![]), kont));
            }

            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return cek_eval_define(&elems[1..], p, env, kont),
                    "if" => {
                        if elems.len() < 3 || elems.len() > 4 {
                            return Err(EvalError::Arity(format!("{p}: if requires 2 or 3 arguments")));
                        }
                        // Fast path: try evaluating condition eagerly
                        if let Some(cond_result) = eval_simple(&elems[1], env, _output) {
                            let cond = cond_result?;
                            if is_truthy(&cond) {
                                return Ok(State::Eval(elems[2].clone(), env.clone(), kont));
                            } else if elems.len() == 4 {
                                return Ok(State::Eval(elems[3].clone(), env.clone(), kont));
                            } else {
                                return Ok(State::Apply(Value::Void, kont));
                            }
                        }
                        let then_e = elems[2].clone();
                        let else_e = if elems.len() == 4 { Some(elems[3].clone()) } else { None };
                        let kont = Rc::new(KontFrame::EvIf { then_e, else_e, env: env.clone(), next: kont });
                        return Ok(State::Eval(elems[1].clone(), env.clone(), kont));
                    }
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote expects 1 argument")));
                        }
                        return Ok(State::Apply(expr_to_value(&elems[1]), kont));
                    }
                    "lambda" => return Ok(State::Apply(eval_lambda(&elems[1..], p, env)?, kont)),
                    "case-lambda" => return Ok(State::Apply(eval_case_lambda(&elems[1..], p, env)?, kont)),
                    "let" => return cek_eval_let(&elems[1..], p, env, kont),
                    "begin" => return Ok(eval_body_state(&elems[1..], env.clone(), kont)),
                    "cond" => return cek_eval_cond(&elems[1..], env, kont),
                    "and" => return cek_eval_and(&elems[1..], env, kont),
                    "or" => return cek_eval_or(&elems[1..], env, kont),
                    "set!" => {
                        if elems.len() != 3 {
                            return Err(EvalError::Arity(format!("{p}: set! requires 2 arguments")));
                        }
                        let name = match &elems[1] {
                            Expr::Symbol(s, _) => s.clone(),
                            _ => return Err(EvalError::Parse(format!("{p}: set!: expected symbol"))),
                        };
                        // Fast path: evaluate value eagerly if simple
                        if let Some(result) = eval_simple(&elems[2], env, _output) {
                            let val = result?;
                            if !env_update(env, &name, val) {
                                return Err(EvalError::UnboundVariable(format!("{p}: {name}")));
                            }
                            return Ok(State::Apply(Value::Void, kont));
                        }
                        let kont = Rc::new(KontFrame::EvSet { name, env: env.clone(), pos: p, next: kont });
                        return Ok(State::Eval(elems[2].clone(), env.clone(), kont));
                    }
                    "string-set!" => return cek_eval_string_set(&elems[1..], p, env, kont),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: not expects 1 argument")));
                        }
                        if let Some(result) = eval_simple(&elems[1], env, _output) {
                            return Ok(State::Apply(Value::Boolean(!is_truthy(&result?)), kont));
                        }
                        let kont = Rc::new(KontFrame::EvNot { next: kont });
                        return Ok(State::Eval(elems[1].clone(), env.clone(), kont));
                    }
                    "define-syntax" => {
                        let _ = eval_define_syntax(&elems[1..], p, env)?;
                        return Ok(State::Apply(Value::Void, kont));
                    }
                    "define-record-type" => {
                        let _ = eval_define_record_type(&elems[1..], p, env)?;
                        return Ok(State::Apply(Value::Void, kont));
                    }
                    "letrec" => return cek_eval_letrec(&elems[1..], p, env, kont),
                    "letrec*" => return cek_eval_letrec_star(&elems[1..], p, env, kont),
                    "case" => return cek_eval_case(&elems[1..], p, env, kont),
                    "do" => return cek_eval_do(&elems[1..], p, env, kont),
                    "let*" => return cek_eval_let_star(&elems[1..], p, env, kont),
                    "when" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Arity(format!("{p}: when requires test and body")));
                        }
                        let body_expr = make_begin(&elems[2..], p);
                        let kont = Rc::new(KontFrame::EvIf { then_e: body_expr, else_e: None, env: env.clone(), next: kont });
                        return Ok(State::Eval(elems[1].clone(), env.clone(), kont));
                    }
                    "guard" => {
                        let vc = match &elems[1] {
                            Expr::List(vc, _) if !vc.is_empty() => vc,
                            _ => return Err(EvalError::Parse(format!("{p}: guard: expected (var clause ...)"))),
                        };
                        let var = match &vc[0] {
                            Expr::Symbol(s, _) => s.clone(),
                            _ => return Err(EvalError::Parse(format!("{p}: guard: expected variable"))),
                        };
                        let clauses = vc[1..].to_vec();
                        let body = elems[2..].to_vec();
                        let setup = Rc::new(KontFrame::EvGuardSetup {
                            var,
                            clauses,
                            body,
                            env: env.clone(),
                            next: kont,
                        });
                        return Ok(State::Apply(Value::Void, setup));
                    }
                    "unless" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Arity(format!("{p}: unless requires test and body")));
                        }
                        let body_expr = make_begin(&elems[2..], p);
                        // (unless test body...) = (if (not test) body...)
                        let not_test = Expr::List(vec![Expr::Symbol("not".to_string(), p), elems[1].clone()], p);
                        let kont = Rc::new(KontFrame::EvIf { then_e: body_expr, else_e: None, env: env.clone(), next: kont });
                        return Ok(State::Eval(not_test, env.clone(), kont));
                    }
                    "syntax-case" => {
                        // (syntax-case stx-expr (literal ...) clause ...)
                        if elems.len() < 4 {
                            return Err(EvalError::Arity(format!("{p}: syntax-case requires stx-expr, literals, and clauses")));
                        }
                        let literals = match &elems[2] {
                            Expr::List(lits, _) => {
                                lits.iter().map(|e| match e {
                                    Expr::Symbol(s, _) => Ok(s.clone()),
                                    _ => Err(EvalError::Parse(format!("{p}: syntax-case: literals must be symbols")))
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            _ => return Err(EvalError::Parse(format!("{p}: syntax-case: expected literals list")))
                        };
                        let clauses = elems[3..].to_vec();
                        // Try fast path for stx-expr
                        if let Some(result) = eval_simple(&elems[1], env, _output) {
                            let stx_val = result?;
                            return handle_syntax_case(stx_val, &literals, &clauses, env, kont, p);
                        }
                        let kont = Rc::new(KontFrame::EvSyntaxCase {
                            literals, clauses, env: env.clone(), next: kont
                        });
                        return Ok(State::Eval(elems[1].clone(), env.clone(), kont));
                    }
                    "syntax" => {
                        // (syntax template) — aka #'template
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: syntax expects 1 argument")));
                        }
                        let template = &elems[1];
                        // If it's a simple symbol bound to SyntaxObject, just return it
                        if let Expr::Symbol(name, _) = template {
                            if let Some(val @ Value::SyntaxObject(..)) = env_get(env, name) {
                                return Ok(State::Apply(val, kont));
                            }
                        }
                        let (expanded, hygiene) = expand_syntax_form(template, env);
                        return Ok(State::Apply(
                            Value::SyntaxObject(Box::new(expanded), hygiene),
                            kont,
                        ));
                    }
                    "with-syntax" => {
                        // (with-syntax ((pat expr) ...) body ...)
                        // Desugar to let
                        let mut let_form = vec![Expr::Symbol("let".to_string(), p)];
                        let_form.extend(elems[1..].iter().cloned());
                        return cek_eval(&Expr::List(let_form, p), env, kont, _output);
                    }
                    _ => {
                        // Check for macro invocation
                        if let Some(val) = env_get(env, op) {
                            match val {
                                Value::Macro { literals, rules, def_env } => {
                                    let (expanded, hygiene_bindings) = expand_macro(&literals, &rules, elems, p, &def_env)?;
                                    let eval_env = make_hygiene_env(env, &hygiene_bindings);
                                    return Ok(State::Eval(expanded, eval_env, kont));
                                }
                                Value::MacroTransformer(transformer) => {
                                    let call_expr = Expr::List(elems.clone(), p);
                                    let stx_arg = Value::SyntaxObject(Box::new(call_expr), vec![]);
                                    let expand_kont = Rc::new(KontFrame::EvMacroExpand {
                                        env: env.clone(),
                                        hygiene: vec![],
                                        next: kont,
                                    });
                                    return Ok(State::Invoke(*transformer, vec![stx_arg], p, expand_kont));
                                }
                                _ => {} // Fall through to function call
                            }
                        }
                    }
                }
            }

            // Function call: try fast path — resolve function + args eagerly
            if let Expr::Symbol(fname, _) = &elems[0] {
                if let Some(func_val) = env_get(env, fname) {
                    let mut args = Vec::with_capacity(elems.len() - 1);
                    let mut all_simple = true;
                    for arg_e in &elems[1..] {
                        match eval_simple(arg_e, env, _output) {
                            Some(Ok(v)) => args.push(v),
                            Some(Err(e)) => return Err(e),
                            None => { all_simple = false; break; }
                        }
                    }
                    if all_simple {
                        return Ok(State::Invoke(func_val, args, p, kont));
                    }
                }
            }
            // Slow path: evaluate function expression, then arguments via continuations
            let kont = Rc::new(KontFrame::EvCallFunc {
                arg_exprs: elems[1..].to_vec(),
                env: env.clone(),
                pos: p,
                next: kont,
            });
            Ok(State::Eval(elems[0].clone(), env.clone(), kont))
        }
    }
}

/// Handle syntax-case pattern matching after stx-expr has been evaluated.
fn handle_syntax_case(
    stx_val: Value,
    literals: &[String],
    clauses: &[Expr],
    env: &Env,
    kont: Kont,
    pos: Pos,
) -> Result<State, EvalError> {
    let stx_expr = match &stx_val {
        Value::SyntaxObject(expr, _) => (**expr).clone(),
        // If not a syntax object, convert value to expr for matching
        other => macros::value_to_expr(other),
    };

    for clause in clauses {
        let clause_elems = match clause {
            Expr::List(elems, _) if elems.len() >= 2 => elems,
            _ => {
                return Err(EvalError::Parse(format!(
                    "{pos}: syntax-case: invalid clause"
                )))
            }
        };

        let pattern = &clause_elems[0];
        let body = &clause_elems[clause_elems.len() - 1];

        let mut bindings = std::collections::HashMap::new();
        if match_pattern(pattern, &stx_expr, literals, &mut bindings) {
            // Bind pattern vars as SyntaxObject/SyntaxList in a new env
            let clause_env = new_env(Some(env.clone()));
            for (name, binding) in bindings {
                match binding {
                    PatternBinding::Single(expr) => {
                        env_set(
                            &clause_env,
                            name,
                            Value::SyntaxObject(Box::new(expr), vec![]),
                        );
                    }
                    PatternBinding::Repeated(exprs) => {
                        env_set(&clause_env, name, Value::SyntaxList(exprs));
                    }
                }
            }
            return Ok(State::Eval(body.clone(), clause_env, kont));
        }
    }

    Err(EvalError::Parse(format!(
        "{pos}: syntax-case: no matching clause"
    )))
}

/// CEK step: return a value to a continuation.
fn cek_apply_kont(val: Value, kont: &Kont, wind_stack: &mut Vec<WindEntry>, handler_stack: &mut Vec<HandlerEntry>, _output: &mut String) -> Result<State, EvalError> {
    match kont.as_ref() {
        KontFrame::Halt => Ok(State::Done(val)),

        KontFrame::Seq { rest, env, next } => {
            if rest.len() == 1 {
                Ok(State::Eval(rest[0].clone(), env.clone(), next.clone()))
            } else {
                let kont = Rc::new(KontFrame::Seq {
                    rest: rest[1..].to_vec(),
                    env: env.clone(),
                    next: next.clone(),
                });
                Ok(State::Eval(rest[0].clone(), env.clone(), kont))
            }
        }

        KontFrame::EvCallFunc { arg_exprs, env, pos, next } => {
            // val is the function
            if arg_exprs.is_empty() {
                Ok(State::Invoke(val, vec![], *pos, next.clone()))
            } else {
                let kont = Rc::new(KontFrame::EvCallArgs {
                    func: val,
                    all_arg_exprs: arg_exprs.clone(),
                    eval_idx: 0,
                    done: vec![],
                    env: env.clone(),
                    pos: *pos,
                    next: next.clone(),
                });
                Ok(State::Eval(arg_exprs[0].clone(), env.clone(), kont))
            }
        }

        KontFrame::EvCallArgs { func, all_arg_exprs, eval_idx, done, env, pos, next } => {
            let mut new_done = done.clone();
            new_done.push(val);
            let next_idx = eval_idx + 1;
            if next_idx >= all_arg_exprs.len() {
                Ok(State::Invoke(func.clone(), new_done, *pos, next.clone()))
            } else {
                let kont = Rc::new(KontFrame::EvCallArgs {
                    func: func.clone(),
                    all_arg_exprs: all_arg_exprs.clone(),
                    eval_idx: next_idx,
                    done: new_done,
                    env: env.clone(),
                    pos: *pos,
                    next: next.clone(),
                });
                Ok(State::Eval(all_arg_exprs[next_idx].clone(), env.clone(), kont))
            }
        }

        KontFrame::EvIf { then_e, else_e, env, next } => {
            if is_truthy(&val) {
                Ok(State::Eval(then_e.clone(), env.clone(), next.clone()))
            } else if let Some(else_e) = else_e {
                Ok(State::Eval(else_e.clone(), env.clone(), next.clone()))
            } else {
                Ok(State::Apply(Value::Void, next.clone()))
            }
        }

        KontFrame::EvDefine { name, env, next } => {
            env_set(env, name.clone(), val);
            Ok(State::Apply(Value::Void, next.clone()))
        }

        KontFrame::EvSet { name, env, pos, next } => {
            if !env_update(env, name, val) {
                return Err(EvalError::UnboundVariable(format!("{pos}: {name}")));
            }
            Ok(State::Apply(Value::Void, next.clone()))
        }

        KontFrame::EvNot { next } => {
            Ok(State::Apply(Value::Boolean(!is_truthy(&val)), next.clone()))
        }

        KontFrame::EvAnd { rest, env, next } => {
            if !is_truthy(&val) || rest.is_empty() {
                Ok(State::Apply(val, next.clone()))
            } else if rest.len() == 1 {
                Ok(State::Eval(rest[0].clone(), env.clone(), next.clone()))
            } else {
                let kont = Rc::new(KontFrame::EvAnd {
                    rest: rest[1..].to_vec(),
                    env: env.clone(),
                    next: next.clone(),
                });
                Ok(State::Eval(rest[0].clone(), env.clone(), kont))
            }
        }

        KontFrame::EvOr { rest, env, next } => {
            if is_truthy(&val) || rest.is_empty() {
                Ok(State::Apply(val, next.clone()))
            } else if rest.len() == 1 {
                Ok(State::Eval(rest[0].clone(), env.clone(), next.clone()))
            } else {
                let kont = Rc::new(KontFrame::EvOr {
                    rest: rest[1..].to_vec(),
                    env: env.clone(),
                    next: next.clone(),
                });
                Ok(State::Eval(rest[0].clone(), env.clone(), kont))
            }
        }

        KontFrame::EvCondTest { body, rest_clauses, env, next } => {
            if is_truthy(&val) {
                if body.is_empty() {
                    // (cond (test)) — return test value
                    Ok(State::Apply(val, next.clone()))
                } else {
                    Ok(eval_body_state(body, env.clone(), next.clone()))
                }
            } else {
                cek_eval_cond(rest_clauses, env, next.clone())
            }
        }

        KontFrame::EvLetBind { name, rest, outer_env, local_env, body, next } => {
            env_set(local_env, name.clone(), val);
            if rest.is_empty() {
                Ok(eval_body_state(body, local_env.clone(), next.clone()))
            } else {
                let (next_name, next_init) = (rest[0].0.clone(), rest[0].1.clone());
                let kont = Rc::new(KontFrame::EvLetBind {
                    name: next_name,
                    rest: rest[1..].to_vec(),
                    outer_env: outer_env.clone(),
                    local_env: local_env.clone(),
                    body: body.clone(),
                    next: next.clone(),
                });
                Ok(State::Eval(next_init, outer_env.clone(), kont))
            }
        }

        KontFrame::EvLetSeqBind { name, rest, local_env, body, next } => {
            env_set(local_env, name.clone(), val);
            if rest.is_empty() {
                Ok(eval_body_state(body, local_env.clone(), next.clone()))
            } else {
                let (next_name, next_init) = (rest[0].0.clone(), rest[0].1.clone());
                let kont = Rc::new(KontFrame::EvLetSeqBind {
                    name: next_name,
                    rest: rest[1..].to_vec(),
                    local_env: local_env.clone(),
                    body: body.clone(),
                    next: next.clone(),
                });
                Ok(State::Eval(next_init, local_env.clone(), kont))
            }
        }

        KontFrame::EvStrSetIdx { var_name, char_expr, env, pos, next } => {
            let idx = match &val {
                Value::Integer(n) => *n as usize,
                _ => return Err(EvalError::Type(format!("{pos}: string-set!: expected integer index"))),
            };
            let kont = Rc::new(KontFrame::EvStrSetChar {
                var_name: var_name.clone(),
                idx,
                env: env.clone(),
                pos: *pos,
                next: next.clone(),
            });
            Ok(State::Eval(char_expr.clone(), env.clone(), kont))
        }

        KontFrame::EvStrSetChar { var_name, idx, env, pos, next } => {
            let ch = match &val {
                Value::Char(c) => *c,
                _ => return Err(EvalError::Type(format!("{pos}: string-set!: expected character"))),
            };
            let mut s = match env_get(env, var_name) {
                Some(Value::Str(s)) => s,
                Some(_) => return Err(EvalError::Type(format!("{pos}: string-set!: expected string"))),
                None => return Err(EvalError::UnboundVariable(format!("{pos}: {var_name}"))),
            };
            let mut chars: Vec<char> = s.chars().collect();
            if *idx >= chars.len() {
                return Err(EvalError::Type(format!("{pos}: string-set!: index out of range")));
            }
            chars[*idx] = ch;
            s = chars.into_iter().collect();
            env_update(env, var_name, Value::Str(s));
            Ok(State::Apply(Value::Void, next.clone()))
        }

        KontFrame::EvCaseKey { clauses, env, next } => {
            // val is the case key
            for clause in clauses {
                let Expr::List(parts, _) = clause else { continue };
                if parts.is_empty() { continue; }
                if matches!(&parts[0], Expr::Symbol(s, _) if s == "else") {
                    return Ok(eval_body_state(&parts[1..], env.clone(), next.clone()));
                }
                if case_clause_matches(&val, parts) {
                    return Ok(eval_body_state(&parts[1..], env.clone(), next.clone()));
                }
            }
            Ok(State::Apply(Value::Void, next.clone()))
        }

        KontFrame::EvMap { func, lists, idx, results, pos, next } => {
            let mut new_results = results.clone();
            new_results.push(val);
            let next_idx = idx + 1;
            if next_idx >= lists[0].len() {
                Ok(State::Apply(vec_to_pair_chain(&new_results), next.clone()))
            } else {
                let next_args: Vec<Value> = lists.iter().map(|l| l[next_idx].clone()).collect();
                let new_kont = Rc::new(KontFrame::EvMap {
                    func: func.clone(),
                    lists: lists.clone(),
                    idx: next_idx,
                    results: new_results,
                    pos: *pos,
                    next: next.clone(),
                });
                Ok(State::Invoke(func.clone(), next_args, *pos, new_kont))
            }
        }

        KontFrame::EvForEach { func, lists, idx, pos, next } => {
            let next_idx = idx + 1;
            if next_idx >= lists[0].len() {
                Ok(State::Apply(Value::Void, next.clone()))
            } else {
                let next_args: Vec<Value> = lists.iter().map(|l| l[next_idx].clone()).collect();
                let new_kont = Rc::new(KontFrame::EvForEach {
                    func: func.clone(),
                    lists: lists.clone(),
                    idx: next_idx,
                    pos: *pos,
                    next: next.clone(),
                });
                Ok(State::Invoke(func.clone(), next_args, *pos, new_kont))
            }
        }

        KontFrame::EvDynWindAfterIn { in_thunk, body_thunk, out_thunk, pos, next } => {
            // in-thunk finished; push wind entry, call body
            let entry = WindEntry { id: next_wind_id(), in_thunk: in_thunk.clone(), out_thunk: out_thunk.clone() };
            wind_stack.push(entry);
            let after_body = Rc::new(KontFrame::EvDynWindAfterBody {
                out_thunk: out_thunk.clone(),
                pos: *pos,
                next: next.clone(),
            });
            Ok(State::Invoke(body_thunk.clone(), vec![], *pos, after_body))
        }

        KontFrame::EvDynWindAfterBody { out_thunk, pos, next } => {
            // body finished; pop wind entry, call out-thunk, save body value
            wind_stack.pop();
            let after_out = Rc::new(KontFrame::EvDynWindAfterOut {
                body_val: val,
                next: next.clone(),
            });
            Ok(State::Invoke(out_thunk.clone(), vec![], *pos, after_out))
        }

        KontFrame::EvDynWindAfterOut { body_val, next } => {
            // out-thunk finished; return body value
            Ok(State::Apply(body_val.clone(), next.clone()))
        }

        KontFrame::EvWind { unwind_outs, rewind_ins, target_ws, val, target_kont, pos } |
        KontFrame::EvWindStep { unwind_outs, rewind_ins, target_ws, val, target_kont, pos } => {
            apply_wind_step(unwind_outs, rewind_ins, target_ws, val, target_kont, *pos, wind_stack)
        }

        KontFrame::EvGuardSetup { var, clauses, body, env, next } => {
            // Push guard handler capturing current continuation and wind state
            handler_stack.push(HandlerEntry::Guard {
                var: var.clone(),
                clauses: clauses.clone(),
                env: env.clone(),
                kont: next.clone(),
                wind_stack: wind_stack.clone(),
            });
            let body_kont = Rc::new(KontFrame::EvGuardBody { next: next.clone() });
            Ok(eval_body_state(body, env.clone(), body_kont))
        }

        KontFrame::EvGuardBody { next } => {
            // Body completed normally — pop guard handler
            handler_stack.pop();
            Ok(State::Apply(val, next.clone()))
        }

        KontFrame::EvGuardClauses { var, clauses, env, next } => {
            // val is the exception value; bind var and evaluate cond clauses
            let local_env = new_env(Some(env.clone()));
            env_set(&local_env, var.clone(), val);
            cek_eval_cond(clauses, &local_env, next.clone())
        }

        KontFrame::EvWithExcHandler { next } => {
            // Thunk completed normally — pop handler
            handler_stack.pop();
            Ok(State::Apply(val, next.clone()))
        }

        KontFrame::EvRaiseContinuationError => {
            Err(EvalError::Exception("handler returned from non-continuable exception".into()))
        }

        KontFrame::EvCallWithValues { consumer, pos, next } => {
            // Producer returned — unpack values and invoke consumer
            let args = match val {
                Value::Values(vals) => vals,
                other => vec![other],
            };
            Ok(State::Invoke(consumer.clone(), args, *pos, next.clone()))
        }

        KontFrame::EvSyntaxCase { literals, clauses, env, next } => {
            handle_syntax_case(val, literals, clauses, env, next.clone(), Pos::default())
        }

        KontFrame::EvMacroExpand { env, hygiene, next } => {
            // Transformer returned a syntax object — unwrap and evaluate
            match val {
                Value::SyntaxObject(expr, mut stx_hygiene) => {
                    stx_hygiene.extend(hygiene.iter().cloned());
                    let eval_env = make_hygiene_env(env, &stx_hygiene);
                    Ok(State::Eval(*expr, eval_env, next.clone()))
                }
                _ => Err(EvalError::Type(
                    "macro transformer must return a syntax object".into(),
                ))
            }
        }
    }
}

/// CEK step: invoke a function with arguments.
fn cek_invoke(func: Value, args: Vec<Value>, pos: Pos, kont: Kont, wind_stack: &mut Vec<WindEntry>, handler_stack: &mut Vec<HandlerEntry>, output: &mut String) -> Result<State, EvalError> {
    match &func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = new_env(Some(env.clone()));
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "{pos}: expected at least {} arguments, got {}",
                        params.len(), args.len()
                    )));
                }
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                env_set(&local_env, rest.clone(), Value::List(args[params.len()..].to_vec()));
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "{pos}: expected {} arguments, got {}",
                        params.len(), args.len()
                    )));
                }
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
            }
            Ok(eval_body_state(body, local_env, kont))
        }

        Value::CaseLambda { clauses } => {
            for clause in clauses {
                let matches = if clause.rest_param.is_some() {
                    args.len() >= clause.params.len()
                } else {
                    args.len() == clause.params.len()
                };
                if matches {
                    let local_env = new_env(Some(clause.env.clone()));
                    for (param, arg) in clause.params.iter().zip(args.iter()) {
                        env_set(&local_env, param.clone(), arg.clone());
                    }
                    if let Some(ref rest) = clause.rest_param {
                        env_set(&local_env, rest.clone(), Value::List(args[clause.params.len()..].to_vec()));
                    }
                    return Ok(eval_body_state(&clause.body, local_env, kont));
                }
            }
            Err(EvalError::Arity(format!(
                "{pos}: no matching case-lambda clause for {} arguments",
                args.len()
            )))
        }

        Value::Continuation(k, saved_ws) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!(
                    "{pos}: continuation expects 1 argument, got {}",
                    args.len()
                )));
            }
            let val = args.into_iter().next().expect("arity checked above");
            // Do winding: unwind current, rewind to saved
            let current_ws = wind_stack.clone();
            let common = common_prefix_len(&current_ws, saved_ws);
            // Unwind from innermost to outermost (reverse order)
            let to_unwind: Vec<Value> = current_ws[common..].iter().rev().map(|e| e.out_thunk.clone()).collect();
            // Rewind from outermost to innermost
            let to_rewind: Vec<Value> = saved_ws[common..].iter().map(|e| e.in_thunk.clone()).collect();
            if to_unwind.is_empty() && to_rewind.is_empty() {
                // No winding needed
                apply_continuation(val, k)
            } else {
                // Build winding continuation: unwind first, then rewind, then apply
                let target_kont = k.clone();
                let target_ws = saved_ws.clone();
                let wind_kont = Rc::new(KontFrame::EvWind {
                    unwind_outs: to_unwind,
                    rewind_ins: to_rewind,
                    target_ws,
                    val: val.clone(),
                    target_kont,
                    pos,
                });
                // Start the winding process
                cek_apply_kont(Value::Void, &wind_kont, wind_stack, handler_stack, output)
            }
        }

        Value::Builtin(name) => {
            match name.as_str() {
                "call/cc" | "call-with-current-continuation" => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity(format!(
                            "{pos}: call/cc expects 1 argument, got {}",
                            args.len()
                        )));
                    }
                    let cont_val = Value::Continuation(kont.clone(), wind_stack.clone());
                    let proc = args.into_iter().next().expect("arity checked above");
                    Ok(State::Invoke(proc, vec![cont_val], pos, kont))
                }
                "raise" => {
                    if args.len() != 1 {
                        return Err(EvalError::Arity(format!(
                            "{pos}: raise expects 1 argument, got {}",
                            args.len()
                        )));
                    }
                    let exn = args.into_iter().next().expect("arity checked above");
                    if handler_stack.is_empty() {
                        return Err(EvalError::Exception(format!("{exn}")));
                    }
                    let handler = handler_stack.pop().expect("checked non-empty above");
                    match handler {
                        HandlerEntry::User(proc) => {
                            let error_kont = Rc::new(KontFrame::EvRaiseContinuationError);
                            Ok(State::Invoke(proc, vec![exn], pos, error_kont))
                        }
                        HandlerEntry::Guard { var, clauses, env, kont: guard_kont, wind_stack: guard_wind } => {
                            let clause_kont = Rc::new(KontFrame::EvGuardClauses {
                                var,
                                clauses,
                                env,
                                next: guard_kont,
                            });
                            let current_ws = wind_stack.clone();
                            let common = common_prefix_len(&current_ws, &guard_wind);
                            let to_unwind: Vec<Value> = current_ws[common..].iter().rev().map(|e| e.out_thunk.clone()).collect();
                            let to_rewind: Vec<Value> = guard_wind[common..].iter().map(|e| e.in_thunk.clone()).collect();
                            if to_unwind.is_empty() && to_rewind.is_empty() {
                                Ok(State::Apply(exn, clause_kont))
                            } else {
                                let wind_kont = Rc::new(KontFrame::EvWind {
                                    unwind_outs: to_unwind,
                                    rewind_ins: to_rewind,
                                    target_ws: guard_wind,
                                    val: exn,
                                    target_kont: clause_kont,
                                    pos,
                                });
                                cek_apply_kont(Value::Void, &wind_kont, wind_stack, handler_stack, output)
                            }
                        }
                    }
                }
                "with-exception-handler" => {
                    if args.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "{pos}: with-exception-handler expects 2 arguments, got {}",
                            args.len()
                        )));
                    }
                    let handler = args[0].clone();
                    let thunk = args[1].clone();
                    handler_stack.push(HandlerEntry::User(handler));
                    let after_kont = Rc::new(KontFrame::EvWithExcHandler { next: kont });
                    Ok(State::Invoke(thunk, vec![], pos, after_kont))
                }
                "values" => {
                    if args.len() == 1 {
                        // Single value is transparent
                        Ok(State::Apply(args.into_iter().next().expect("len checked == 1"), kont))
                    } else {
                        Ok(State::Apply(Value::Values(args), kont))
                    }
                }
                "call-with-values" => {
                    if args.len() != 2 {
                        return Err(EvalError::Arity(format!(
                            "{pos}: call-with-values expects 2 arguments, got {}",
                            args.len()
                        )));
                    }
                    let producer = args[0].clone();
                    let consumer = args[1].clone();
                    let cwv_kont = Rc::new(KontFrame::EvCallWithValues {
                        consumer,
                        pos,
                        next: kont,
                    });
                    Ok(State::Invoke(producer, vec![], pos, cwv_kont))
                }
                "dynamic-wind" => {
                    if args.len() != 3 {
                        return Err(EvalError::Arity(format!(
                            "{pos}: dynamic-wind expects 3 arguments, got {}",
                            args.len()
                        )));
                    }
                    let in_thunk = args[0].clone();
                    let body_thunk = args[1].clone();
                    let out_thunk = args[2].clone();
                    // Call in-thunk first; after it completes, EvDynWindAfterIn takes over
                    let after_in = Rc::new(KontFrame::EvDynWindAfterIn {
                        in_thunk: in_thunk.clone(),
                        body_thunk,
                        out_thunk,
                        pos,
                        next: kont,
                    });
                    Ok(State::Invoke(in_thunk, vec![], pos, after_in))
                }
                "apply" => {
                    if args.len() < 2 {
                        return Err(EvalError::Arity(format!("{pos}: apply requires at least 2 arguments")));
                    }
                    let func = args[0].clone();
                    let last = &args[args.len() - 1];
                    let tail = to_list_vec(last)
                        .ok_or_else(|| EvalError::Type(format!("{pos}: apply: last argument must be a list")))?;
                    let mut combined = args[1..args.len() - 1].to_vec();
                    combined.extend(tail);
                    Ok(State::Invoke(func, combined, pos, kont))
                }
                "map" => {
                    if args.len() < 2 {
                        return Err(EvalError::Arity(format!("{pos}: map requires at least 2 arguments")));
                    }
                    let func = args[0].clone();
                    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a|
                        to_list_vec(a).ok_or_else(|| EvalError::Type(format!("{pos}: map: expected list")))
                    ).collect::<Result<_, _>>()?;
                    if lists[0].is_empty() {
                        return Ok(State::Apply(Value::List(vec![]), kont));
                    }
                    let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
                    let map_kont = Rc::new(KontFrame::EvMap {
                        func: func.clone(),
                        lists,
                        idx: 0,
                        results: vec![],
                        pos,
                        next: kont,
                    });
                    Ok(State::Invoke(func, first_args, pos, map_kont))
                }
                "for-each" => {
                    if args.len() < 2 {
                        return Err(EvalError::Arity(format!("{pos}: for-each requires at least 2 arguments")));
                    }
                    let func = args[0].clone();
                    let lists: Vec<Vec<Value>> = args[1..].iter().map(|a|
                        to_list_vec(a).ok_or_else(|| EvalError::Type(format!("{pos}: for-each: expected list")))
                    ).collect::<Result<_, _>>()?;
                    if lists[0].is_empty() {
                        return Ok(State::Apply(Value::Void, kont));
                    }
                    let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
                    let fe_kont = Rc::new(KontFrame::EvForEach {
                        func: func.clone(),
                        lists,
                        idx: 0,
                        pos,
                        next: kont,
                    });
                    Ok(State::Invoke(func, first_args, pos, fe_kont))
                }
                _ => {
                    // Regular builtin
                    let result = apply_builtin(name, &args, pos, output)?;
                    Ok(State::Apply(result, kont))
                }
            }
        }

        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{pos}: record constructor expects {} arguments, got {}",
                    field_names.len(), args.len()
                )));
            }
            let fields: Vec<(String, Value)> = field_names
                .iter()
                .zip(args.iter())
                .map(|(n, v)| (n.clone(), v.clone()))
                .collect();
            Ok(State::Apply(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields,
            }, kont))
        }

        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: predicate expects 1 argument")));
            }
            Ok(State::Apply(
                Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)),
                kont
            ))
        }

        Value::RecordAccessor { type_id, field_name } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: accessor expects 1 argument")));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if tid == type_id => {
                    let v = fields
                        .iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| EvalError::Type(format!("{pos}: no field {field_name}")))?;
                    Ok(State::Apply(v, kont))
                }
                _ => Err(EvalError::Type(format!("{pos}: expected record, got {}", args[0]))),
            }
        }

        _ => Err(EvalError::Type(format!("{pos}: not a procedure: {func}"))),
    }
}

pub(super) fn as_integer(v: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{call_pos}: expected integer, got {v}"))),
    }
}

pub(super) fn value_to_f64(v: &Value, call_pos: Pos) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(format!("{call_pos}: expected number, got {v}"))),
    }
}

// ---------- Macros (syntax-rules) ----------

pub(super) static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(super) const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "case-lambda", "let", "begin", "cond", "and", "or",
    "set!", "string-set!", "not", "define-syntax", "syntax-rules",
    "letrec", "letrec*", "case", "do", "let*", "when", "unless", "guard",
    "syntax-case", "syntax", "with-syntax",
];

#[derive(Clone)]
pub(super) enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut output = String::new();
    let result = cek_run(&exprs, &env, &mut output)?;
    Ok(result.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = default_env();
    let mut output = String::new();
    let result = cek_run(&exprs, &env, &mut output)?;
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
