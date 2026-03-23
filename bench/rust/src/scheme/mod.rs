pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type VectorRef = Rc<RefCell<Vec<Value>>>;
type WindEntry = Rc<(Value, Value)>; // (in-thunk, out-thunk)
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}__g{}", base, n)
}

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
    static WIND_STACK: RefCell<Vec<WindEntry>> = RefCell::new(Vec::new());
    static EXCEPTION_HANDLERS: RefCell<Vec<ExceptionHandlerEntry>> = RefCell::new(Vec::new());
}

#[derive(Clone)]
enum ExceptionHandlerEntry {
    Guard {
        var_name: String,
        clauses: Vec<Expr>,
        env: Env,
        kont: Rc<Kont>,
        winds: Vec<WindEntry>,
    },
    WithHandler {
        handler: Value,
        kont: Rc<Kont>,
        winds: Vec<WindEntry>,
    },
}

#[derive(Clone)]
enum WindOp {
    Unwind(Value),               // pop wind stack, call out-thunk
    Rewind(Value, WindEntry),    // call in-thunk, then push wind entry
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    Pair(Rc<Value>, Rc<Value>),
    Nil,
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Void,
    Builtin(&'static str, fn(&[Value]) -> Result<Value, EvalError>),
    CallCC,
    DynamicWind,
    Raise,
    WithExceptionHandler,
    Continuation(Rc<Kont>, Vec<WindEntry>),
    Vector(VectorRef),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    MultipleValues(Vec<Value>),
    ValuesProc,
    CallWithValues,
    Record { type_id: usize, fields: Vec<Value> },
    RecordConstructor { type_id: usize, type_name: String, field_count: usize },
    RecordPredicate { type_id: usize },
    RecordAccessor { type_id: usize, type_name: String, field_index: usize, field_name: String },
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
        panic!("rational with zero denominator");
    }
    let sign = if d < 0 { -1 } else { 1 };
    let n = n * sign;
    let d = d * sign;
    let g = gcd(n, d);
    let n = n / g;
    let d = d / g;
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

// Convert a value to f64 for inexact operations
fn value_to_f64(v: &Value) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        other => Err(EvalError::Type(format!("expected number, got {}", other.display()))),
    }
}

fn is_number(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Float(_) | Value::Rational(_, _))
}

fn is_exact(v: &Value) -> bool {
    matches!(v, Value::Integer(_) | Value::Rational(_, _))
}

fn has_inexact(args: &[Value]) -> bool {
    args.iter().any(|a| matches!(a, Value::Float(_)))
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Float(v) => write!(f, "Float({})", v),
            Value::Rational(n, d) => write!(f, "Rational({}/{})", n, d),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Str(s) => write!(f, "Str(\"{}\")", s),
            Value::Char(c) => write!(f, "Char({:?})", c),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
            Value::Pair(a, b) => write!(f, "Pair({:?}, {:?})", a, b),
            Value::Nil => write!(f, "Nil"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Vector(v) => write!(f, "Vector({:?})", v.borrow()),
            Value::Void => write!(f, "Void"),
            Value::Builtin(name, _) => write!(f, "Builtin({})", name),
            Value::CallCC => write!(f, "CallCC"),
            Value::DynamicWind => write!(f, "DynamicWind"),
            Value::Raise => write!(f, "Raise"),
            Value::WithExceptionHandler => write!(f, "WithExceptionHandler"),
            Value::Continuation(..) => write!(f, "#<continuation>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::MultipleValues(vs) => write!(f, "MultipleValues({:?})", vs),
            Value::ValuesProc => write!(f, "ValuesProc"),
            Value::CallWithValues => write!(f, "CallWithValues"),
            Value::Record { type_id, .. } => write!(f, "#<record type={}>", type_id),
            Value::RecordConstructor { type_name, .. } => write!(f, "#<record-constructor {}>", type_name),
            Value::RecordPredicate { .. } => write!(f, "#<record-predicate>"),
            Value::RecordAccessor { type_name, field_name, .. } => write!(f, "#<record-accessor {}.{}>", type_name, field_name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
            (Value::Void, Value::Void) => true,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Record { .. }, Value::Record { .. }) => false,
            (Value::MultipleValues(a), Value::MultipleValues(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                if f.is_infinite() {
                    if *f > 0.0 { "+inf.0".to_string() } else { "-inf.0".to_string() }
                } else if f.is_nan() {
                    "+nan.0".to_string()
                } else {
                    let s = format!("{}", f);
                    if s.contains('.') || s.contains('e') || s.contains('E') {
                        s
                    } else {
                        format!("{}.0", s)
                    }
                }
            }
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{}", c),
            Value::Symbol(s) => s.clone(),
            Value::Nil => "()".to_string(),
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&car.display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            Value::Vector(v) => {
                let elems = v.borrow();
                let mut out = String::from("#(");
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { out.push(' '); }
                    out.push_str(&e.display());
                }
                out.push(')');
                out
            }
            Value::Lambda { .. } | Value::Builtin(_, _) | Value::CallCC | Value::DynamicWind | Value::Continuation(..) | Value::Raise | Value::WithExceptionHandler | Value::ValuesProc | Value::CallWithValues | Value::RecordConstructor { .. } | Value::RecordPredicate { .. } | Value::RecordAccessor { .. } => {
                "#<procedure>".to_string()
            }
            Value::Record { .. } => "#<record>".to_string(),
            Value::MultipleValues(_) => "#<values>".to_string(),
            Value::Void => "#<void>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
        }
    }

    fn display_for_display(&self) -> String {
        match self {
            Value::Float(_) | Value::Rational(_, _) => self.display(),
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Vector(v) => {
                let elems = v.borrow();
                let mut out = String::from("#(");
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { out.push(' '); }
                    out.push_str(&e.display_for_display());
                }
                out.push(')');
                out
            }
            Value::Pair(_, _) => {
                let mut out = String::from("(");
                let mut cur = self;
                let mut first = true;
                loop {
                    match cur {
                        Value::Pair(car, cdr) => {
                            if !first { out.push(' '); }
                            first = false;
                            out.push_str(&car.display_for_display());
                            cur = cdr;
                        }
                        Value::Nil => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.display_for_display());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
            _ => self.display(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type(format!(
                "expected integer, got {}",
                other.display()
            ))),
        }
    }

    fn as_number_to_rational(&self) -> Result<(i64, i64), EvalError> {
        match self {
            Value::Integer(n) => Ok((*n, 1)),
            Value::Rational(n, d) => Ok((*n, *d)),
            other => Err(EvalError::Type(format!("expected number, got {}", other.display()))),
        }
    }
}

// --- Expr with position ---

#[derive(Debug, Clone, PartialEq)]
struct Expr {
    kind: ExprKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum ExprKind {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, line: usize, col: usize) -> Self {
        Self { kind, line, col }
    }

    fn wrap_err(&self, err: EvalError) -> EvalError {
        match &err {
            EvalError::WithPosition { .. } => err,
            _ => EvalError::WithPosition {
                source: Box::new(err),
                line: self.line,
                col: self.col,
            },
        }
    }
}

// --- Environment ---

type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug)]
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

fn env_get(env: &Env, name: &str) -> Result<Value, EvalError> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        Ok(v.clone())
    } else if let Some(ref parent) = inner.parent {
        env_get(parent, name)
    } else {
        Err(EvalError::UnboundVariable(name.to_string()))
    }
}

/// Check if a binding is a macro without cloning the value.
fn env_get_macro(env: &Env, name: &str) -> Option<Value> {
    let inner = env.borrow();
    if let Some(v) = inner.bindings.get(name) {
        if matches!(v, Value::Macro { .. }) {
            Some(v.clone())
        } else {
            None
        }
    } else if let Some(ref parent) = inner.parent {
        env_get_macro(parent, name)
    } else {
        None
    }
}

fn env_set(env: &Env, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn env_update(env: &Env, name: &str, val: Value) -> Result<(), EvalError> {
    let mut cur = env.clone();
    loop {
        {
            let mut inner = cur.borrow_mut();
            if inner.bindings.contains_key(name) {
                inner.bindings.insert(name.to_string(), val);
                return Ok(());
            }
        }
        let parent = cur.borrow().parent.clone();
        match parent {
            Some(p) => cur = p,
            None => return Err(EvalError::UnboundVariable(name.to_string())),
        }
    }
}

// --- Continuation (CEK machine) ---

#[derive(Clone)]
enum Kont {
    Done,
    Define { name: String, env: Env, next: Rc<Kont> },
    Set { form: Expr, name: String, env: Env, next: Rc<Kont> },
    If { then_expr: Expr, else_expr: Option<Expr>, env: Env, next: Rc<Kont> },
    Seq { remaining: Vec<Expr>, env: Env, next: Rc<Kont> },
    And { remaining: Vec<Expr>, env: Env, next: Rc<Kont> },
    Or { remaining: Vec<Expr>, env: Env, next: Rc<Kont> },
    CondTest { form: Expr, body: Vec<Expr>, remaining_clauses: Vec<Expr>, env: Env, next: Rc<Kont> },
    LetBind { form: Expr, eval_env: Env, done: Vec<(String, Value)>, current_name: String, remaining: Vec<(String, Expr)>, body: Vec<Expr>, next: Rc<Kont> },
    NamedLetBind { form: Expr, loop_name: String, eval_env: Env, done: Vec<(String, Value)>, current_name: String, remaining: Vec<(String, Expr)>, body: Vec<Expr>, next: Rc<Kont> },
    Operator { form: Expr, args: Vec<Expr>, env: Env, next: Rc<Kont> },
    Arg { form: Expr, func: Value, collected: Vec<Value>, before_exprs: Vec<Expr>, env: Env, next: Rc<Kont> },
    StringSetIdx { form: Expr, var_name: String, char_expr: Expr, env: Env, next: Rc<Kont> },
    StringSetChar { form: Expr, var_name: String, idx: usize, env: Env, next: Rc<Kont> },
    LetrecBind { letrec_env: Env, remaining: Vec<(String, Expr)>, body: Vec<Expr>, next: Rc<Kont> },
    CaseKey { form: Expr, clauses: Vec<Expr>, env: Env, next: Rc<Kont> },
    // dynamic-wind: after in-thunk returns, push wind entry, call body
    DynamicWindRunBody { body_thunk: Value, out_thunk: Value, wind_entry: WindEntry, next: Rc<Kont> },
    // dynamic-wind: after body returns, pop wind entry, call out-thunk
    DynamicWindRunOut { out_thunk: Value, next: Rc<Kont> },
    // dynamic-wind: after out-thunk returns, restore body value
    DynamicWindFinish { body_value: Value, next: Rc<Kont> },
    // continuation transfer: chain of wind operations
    WindChain { remaining: Vec<WindOp>, cont_value: Value, target_kont: Rc<Kont> },
    // continuation transfer: after rewind in-thunk, push entry and continue
    WindChainPush { wind_entry: WindEntry, remaining: Vec<WindOp>, cont_value: Value, target_kont: Rc<Kont> },
    // exception handling: pop handler when body completes normally
    GuardCleanup { next: Rc<Kont> },
    ExceptionHandlerCleanup { next: Rc<Kont> },
    // guard clause dispatch: test clauses with exception bound
    GuardClauseTest { exception: Value, body: Vec<Expr>, remaining_clauses: Vec<Expr>, env: Env, next: Rc<Kont> },
    // after wind unwinding for raise, dispatch to guard clauses
    DispatchGuardClauses { exception: Value, clauses: Vec<Expr>, env: Env, next: Rc<Kont> },
    // after wind unwinding for raise, call handler with exception
    CallHandlerAfterWind { handler: Value, exception: Value, next: Rc<Kont> },
    // call-with-values: after producer returns, call consumer with the values
    CallWithValuesConsumer { consumer: Value, form: Expr, next: Rc<Kont> },
}

impl std::fmt::Debug for Kont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<continuation>")
    }
}

// --- Builtins ---

fn num_add(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x + y)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
        (Value::Float(_), _) | (_, Value::Float(_)) => {
            Ok(Value::Float(value_to_f64(a)? + value_to_f64(b)?))
        }
        _ => {
            let (an, ad) = a.as_number_to_rational()?;
            let (bn, bd) = b.as_number_to_rational()?;
            Ok(make_rational(an * bd + bn * ad, ad * bd))
        }
    }
}

fn num_sub(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x - y)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
        (Value::Float(_), _) | (_, Value::Float(_)) => {
            Ok(Value::Float(value_to_f64(a)? - value_to_f64(b)?))
        }
        _ => {
            let (an, ad) = a.as_number_to_rational()?;
            let (bn, bd) = b.as_number_to_rational()?;
            Ok(make_rational(an * bd - bn * ad, ad * bd))
        }
    }
}

fn num_mul(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x * y)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
        (Value::Float(_), _) | (_, Value::Float(_)) => {
            Ok(Value::Float(value_to_f64(a)? * value_to_f64(b)?))
        }
        _ => {
            let (an, ad) = a.as_number_to_rational()?;
            let (bn, bd) = b.as_number_to_rational()?;
            Ok(make_rational(an * bn, ad * bd))
        }
    }
}

fn num_div(a: &Value, b: &Value) -> Result<Value, EvalError> {
    match (a, b) {
        (Value::Integer(_), Value::Integer(0)) | (Value::Rational(_, _), Value::Integer(0)) => {
            Err(EvalError::DivisionByZero)
        }
        (Value::Float(_), _) | (_, Value::Float(_)) => {
            let bv = value_to_f64(b)?;
            if bv == 0.0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Float(value_to_f64(a)? / bv))
        }
        _ => {
            let (an, ad) = a.as_number_to_rational()?;
            let (bn, bd) = b.as_number_to_rational()?;
            if bn == 0 { return Err(EvalError::DivisionByZero); }
            Ok(make_rational(an * bd, ad * bn))
        }
    }
}

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Integer(0);
    for a in args {
        result = num_add(&result, a)?;
    }
    Ok(result)
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    if args.len() == 1 {
        return match &args[0] {
            Value::Integer(n) => Ok(Value::Integer(-n)),
            Value::Float(f) => Ok(Value::Float(-f)),
            Value::Rational(n, d) => Ok(Value::Rational(-n, *d)),
            other => Err(EvalError::Type(format!("expected number, got {}", other.display()))),
        };
    }
    let mut result = args[0].clone();
    for a in &args[1..] {
        result = num_sub(&result, a)?;
    }
    Ok(result)
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Integer(1);
    for a in args {
        result = num_mul(&result, a)?;
    }
    Ok(result)
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
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

fn compare_nums(a: &Value, b: &Value) -> Result<std::cmp::Ordering, EvalError> {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => Ok(x.cmp(y)),
        (Value::Float(_), _) | (_, Value::Float(_)) => {
            let fa = value_to_f64(a)?;
            let fb = value_to_f64(b)?;
            fa.partial_cmp(&fb).ok_or_else(|| EvalError::Type("cannot compare NaN".into()))
        }
        _ => {
            let (an, ad) = a.as_number_to_rational()?;
            let (bn, bd) = b.as_number_to_rational()?;
            Ok((an * bd).cmp(&(bn * ad)))
        }
    }
}

fn compare_values(args: &[Value], cmp: fn(std::cmp::Ordering) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    for w in args.windows(2) {
        let ord = compare_nums(&w[0], &w[1])?;
        if !cmp(ord) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |o| o == std::cmp::Ordering::Less)
}
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |o| o == std::cmp::Ordering::Greater)
}
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |o| o == std::cmp::Ordering::Equal)
}
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |o| o != std::cmp::Ordering::Greater)
}
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |o| o != std::cmp::Ordering::Less)
}

fn builtin_not(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("not requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(!args[0].is_truthy()))
}

fn builtin_cons(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("cons requires exactly 2 arguments".into()));
    }
    Ok(Value::Pair(Rc::new(args[0].clone()), Rc::new(args[1].clone())))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(car, _) => Ok(Value::clone(car)),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(_, cdr) => Ok(Value::clone(cdr)),
        _ => Err(EvalError::Type("cdr: not a pair".into())),
    }
}

fn builtin_null(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("null? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Nil)))
}

fn builtin_list(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = Value::Nil;
    for a in args.iter().rev() {
        result = Value::Pair(Rc::new(a.clone()), Rc::new(result));
    }
    Ok(result)
}

fn builtin_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("length requires exactly 1 argument".into()));
    }
    let mut count = 0i64;
    let mut cur = &args[0];
    loop {
        match cur {
            Value::Nil => return Ok(Value::Integer(count)),
            Value::Pair(_, cdr) => {
                count += 1;
                cur = cdr;
            }
            _ => return Err(EvalError::Type("length: not a proper list".into())),
        }
    }
}

fn builtin_append(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Nil);
    }
    let mut result = args[args.len() - 1].clone();
    for i in (0..args.len() - 1).rev() {
        let mut elems = Vec::new();
        let mut cur = &args[i];
        loop {
            match cur {
                Value::Nil => break,
                Value::Pair(car, cdr) => {
                    elems.push(Value::clone(car));
                    cur = cdr;
                }
                _ => return Err(EvalError::Type("append: not a proper list".into())),
            }
        }
        for e in elems.into_iter().rev() {
            result = Value::Pair(Rc::new(e), Rc::new(result));
        }
    }
    Ok(result)
}

fn builtin_reverse(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("reverse requires exactly 1 argument".into()));
    }
    let mut result = Value::Nil;
    let mut cur = &args[0];
    loop {
        match cur {
            Value::Nil => return Ok(result),
            Value::Pair(car, cdr) => {
                result = Value::Pair(Rc::new(Value::clone(car)), Rc::new(result));
                cur = cdr;
            }
            _ => return Err(EvalError::Type("reverse: not a proper list".into())),
        }
    }
}

fn builtin_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(is_number(&args[0])))
}

fn builtin_string_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Str(_))))
}

fn builtin_boolean_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("boolean? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Boolean(_))))
}

fn builtin_pair_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("pair? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Pair(_, _))))
}

fn builtin_symbol_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Symbol(_))))
}

fn builtin_char_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Char(_))))
}

fn builtin_display(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("display requires exactly 1 argument".into()));
    }
    let s = args[0].display_for_display();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_write(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("write requires exactly 1 argument".into()));
    }
    let s = args[0].display();
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push_str(&s));
    Ok(Value::Void)
}

fn builtin_newline(args: &[Value]) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::Arity("newline requires 0 arguments".into()));
    }
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().push('\n'));
    Ok(Value::Void)
}

fn builtin_string_append(args: &[Value]) -> Result<Value, EvalError> {
    let mut result = String::new();
    for a in args {
        match a {
            Value::Str(s) => result.push_str(s),
            _ => return Err(EvalError::Type("string-append: expected string".into())),
        }
    }
    Ok(Value::Str(result))
}

fn builtin_string_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-length requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
        _ => Err(EvalError::Type("string-length: expected string".into())),
    }
}

fn builtin_substring(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("substring requires exactly 3 arguments".into()));
    }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("substring: expected string".into())),
    };
    let start = args[1].as_integer()? as usize;
    let end = args[2].as_integer()? as usize;
    Ok(Value::Str(s[start..end].to_string()))
}

fn builtin_string_to_number(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->number requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => {
            if let Ok(n) = s.parse::<i64>() {
                return Ok(Value::Integer(n));
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Float(f));
            }
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string requires exactly 1 argument".into()));
    }
    Ok(Value::Str(args[0].display()))
}

fn builtin_symbol_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("symbol->string requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Symbol(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("symbol->string: expected symbol".into())),
    }
}

fn builtin_string_to_symbol(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->symbol requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Symbol(s.clone())),
        _ => Err(EvalError::Type("string->symbol: expected string".into())),
    }
}

fn builtin_string_copy(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string-copy requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        _ => Err(EvalError::Type("string-copy: expected string".into())),
    }
}

// L14: string immutability helpers
fn builtin_string_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("string->list requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Str(s) => {
            let mut result = Value::Nil;
            for ch in s.chars().rev() {
                result = Value::Pair(Rc::new(Value::Char(ch)), Rc::new(result));
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("string->list: expected string".into())),
    }
}

fn builtin_list_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("list->string requires exactly 1 argument".into()));
    }
    let mut s = String::new();
    let mut cur = args[0].clone();
    loop {
        match &cur {
            Value::Pair(car, cdr) => {
                match car.as_ref() {
                    Value::Char(c) => s.push(*c),
                    _ => return Err(EvalError::Type("list->string: expected list of characters".into())),
                }
                let next = Value::clone(cdr);
                cur = next;
            }
            Value::Nil => break,
            _ => return Err(EvalError::Type("list->string: expected proper list".into())),
        }
    }
    Ok(Value::Str(s))
}

fn builtin_char_to_integer(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("char->integer requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Char(c) => Ok(Value::Integer(*c as i64)),
        _ => Err(EvalError::Type("char->integer: expected character".into())),
    }
}

fn builtin_integer_to_char(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("integer->char requires exactly 1 argument".into()));
    }
    let n = args[0].as_integer()?;
    match char::from_u32(n as u32) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Type("integer->char: invalid code point".into())),
    }
}

fn builtin_string_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity("string-ref requires exactly 2 arguments".into()));
    }
    let s = match &args[0] {
        Value::Str(s) => s,
        _ => return Err(EvalError::Type("string-ref: expected string".into())),
    };
    let idx = args[1].as_integer()? as usize;
    match s.chars().nth(idx) {
        Some(c) => Ok(Value::Char(c)),
        None => Err(EvalError::Type("string-ref: index out of bounds".into())),
    }
}

fn builtin_apply_placeholder(_args: &[Value]) -> Result<Value, EvalError> {
    Err(EvalError::Type("apply: internal error".into()))
}

// --- L13: Numeric utilities ---

fn builtin_abs(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("abs requires exactly 1 argument".into())); }
    Ok(Value::Integer(args[0].as_integer()?.abs()))
}

fn builtin_modulo(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("modulo requires exactly 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(((a % b) + b) % b))
}

fn builtin_remainder(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("remainder requires exactly 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(a % b))
}

fn builtin_quotient(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("quotient requires exactly 2 arguments".into())); }
    let a = args[0].as_integer()?;
    let b = args[1].as_integer()?;
    if b == 0 { return Err(EvalError::DivisionByZero); }
    Ok(Value::Integer(a / b))
}

fn builtin_min(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("min requires at least 1 argument".into())); }
    let mut result = args[0].as_integer()?;
    for a in &args[1..] { let v = a.as_integer()?; if v < result { result = v; } }
    Ok(Value::Integer(result))
}

fn builtin_max(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() { return Err(EvalError::Arity("max requires at least 1 argument".into())); }
    let mut result = args[0].as_integer()?;
    for a in &args[1..] { let v = a.as_integer()?; if v > result { result = v; } }
    Ok(Value::Integer(result))
}

fn builtin_expt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("expt requires exactly 2 arguments".into())); }
    let base = args[0].as_integer()?;
    let exp = args[1].as_integer()?;
    if exp < 0 { return Ok(Value::Integer(0)); }
    Ok(Value::Integer(base.pow(exp as u32)))
}

fn builtin_zero_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("zero? requires exactly 1 argument".into())); }
    let result = match &args[0] {
        Value::Integer(n) => *n == 0,
        Value::Float(f) => *f == 0.0,
        Value::Rational(n, _) => *n == 0,
        _ => return Err(EvalError::Type(format!("expected number, got {}", args[0].display()))),
    };
    Ok(Value::Boolean(result))
}

fn builtin_positive_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(compare_nums(&args[0], &Value::Integer(0))? == std::cmp::Ordering::Greater))
}

fn builtin_negative_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(compare_nums(&args[0], &Value::Integer(0))? == std::cmp::Ordering::Less))
}

fn builtin_odd_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("odd? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? % 2 != 0))
}

fn builtin_even_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("even? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? % 2 == 0))
}

// --- L13: List utilities ---

fn builtin_list_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-ref requires exactly 2 arguments".into())); }
    let idx = args[1].as_integer()? as usize;
    let mut cur = &args[0];
    for _ in 0..idx {
        match cur {
            Value::Pair(_, cdr) => cur = cdr,
            _ => return Err(EvalError::Type("list-ref: index out of range".into())),
        }
    }
    match cur {
        Value::Pair(car, _) => Ok(Value::clone(car)),
        _ => Err(EvalError::Type("list-ref: index out of range".into())),
    }
}

fn builtin_list_tail(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("list-tail requires exactly 2 arguments".into())); }
    let idx = args[1].as_integer()? as usize;
    let mut cur = &args[0];
    for _ in 0..idx {
        match cur {
            Value::Pair(_, cdr) => cur = cdr,
            _ => return Err(EvalError::Type("list-tail: index out of range".into())),
        }
    }
    Ok(cur.clone())
}

fn builtin_list_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list? requires exactly 1 argument".into())); }
    let mut cur = &args[0];
    loop {
        match cur {
            Value::Nil => return Ok(Value::Boolean(true)),
            Value::Pair(_, cdr) => cur = cdr,
            _ => return Ok(Value::Boolean(false)),
        }
    }
}

fn builtin_assoc(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("assoc requires exactly 2 arguments".into())); }
    let key = &args[0];
    let mut cur = &args[1];
    loop {
        match cur {
            Value::Nil => return Ok(Value::Boolean(false)),
            Value::Pair(car, cdr) => {
                if let Value::Pair(pair_car, _) = car.as_ref() {
                    if values_equal(key, pair_car) {
                        return Ok(Value::clone(car));
                    }
                }
                cur = cdr;
            }
            _ => return Err(EvalError::Type("assoc: not a proper list".into())),
        }
    }
}

fn builtin_eq_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eq? requires exactly 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Void, Value::Void) => true,
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => Rc::ptr_eq(a1, b1) && Rc::ptr_eq(a2, b2),
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => values_equal(a1, b1) && values_equal(a2, b2),
        (Value::Vector(va), Value::Vector(vb)) => {
            let va = va.borrow();
            let vb = vb.borrow();
            va.len() == vb.len() && va.iter().zip(vb.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn eqv_compare(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

fn builtin_equal_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("equal? requires exactly 2 arguments".into())); }
    Ok(Value::Boolean(values_equal(&args[0], &args[1])))
}

fn builtin_map_placeholder(_args: &[Value]) -> Result<Value, EvalError> {
    Err(EvalError::Type("map: internal error".into()))
}

// --- L13: Character utilities ---

fn builtin_char_alphabetic(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-alphabetic? requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
        _ => Err(EvalError::Type("char-alphabetic?: expected char".into())),
    }
}

fn builtin_char_numeric(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-numeric? requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
        _ => Err(EvalError::Type("char-numeric?: expected char".into())),
    }
}

fn builtin_char_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-upcase requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
        _ => Err(EvalError::Type("char-upcase: expected char".into())),
    }
}

fn builtin_char_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("char-downcase requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
        _ => Err(EvalError::Type("char-downcase: expected char".into())),
    }
}

fn builtin_char_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char=? requires exactly 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("char=?: expected chars".into())),
    }
}

fn builtin_char_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("char<? requires exactly 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("char<?: expected chars".into())),
    }
}

// --- L13: String utilities ---

fn builtin_string_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string=? requires exactly 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
        _ => Err(EvalError::Type("string=?: expected strings".into())),
    }
}

fn builtin_string_lt(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string<? requires exactly 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
        _ => Err(EvalError::Type("string<?: expected strings".into())),
    }
}

fn builtin_string_ci_eq(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("string-ci=? requires exactly 2 arguments".into())); }
    match (&args[0], &args[1]) {
        (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
        _ => Err(EvalError::Type("string-ci=?: expected strings".into())),
    }
}

fn builtin_string_upcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-upcase requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
        _ => Err(EvalError::Type("string-upcase: expected string".into())),
    }
}

fn builtin_string_downcase(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("string-downcase requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
        _ => Err(EvalError::Type("string-downcase: expected string".into())),
    }
}

// --- L15: eqv? ---

fn builtin_eqv_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("eqv? requires exactly 2 arguments".into())); }
    let result = match (&args[0], &args[1]) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    };
    Ok(Value::Boolean(result))
}

// --- L15: Vector builtins ---

fn builtin_vector(args: &[Value]) -> Result<Value, EvalError> {
    Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
}

fn builtin_make_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() || args.len() > 2 {
        return Err(EvalError::Arity("make-vector requires 1 or 2 arguments".into()));
    }
    let len = args[0].as_integer()? as usize;
    let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
    Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
}

fn builtin_vector_ref(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 { return Err(EvalError::Arity("vector-ref requires exactly 2 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let idx = args[1].as_integer()? as usize;
            let v = v.borrow();
            if idx >= v.len() {
                return Err(EvalError::Type("vector-ref: index out of bounds".into()));
            }
            Ok(v[idx].clone())
        }
        _ => Err(EvalError::Type("vector-ref: expected vector".into())),
    }
}

fn builtin_vector_set(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 3 { return Err(EvalError::Arity("vector-set! requires exactly 3 arguments".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let idx = args[1].as_integer()? as usize;
            let mut v = v.borrow_mut();
            if idx >= v.len() {
                return Err(EvalError::Type("vector-set!: index out of bounds".into()));
            }
            v[idx] = args[2].clone();
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type("vector-set!: expected vector".into())),
    }
}

fn builtin_vector_length(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector-length requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
        _ => Err(EvalError::Type("vector-length: expected vector".into())),
    }
}

fn builtin_vector_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Vector(_))))
}

fn builtin_vector_to_list(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("vector->list requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Vector(v) => {
            let v = v.borrow();
            let mut result = Value::Nil;
            for e in v.iter().rev() {
                result = Value::Pair(Rc::new(e.clone()), Rc::new(result));
            }
            Ok(result)
        }
        _ => Err(EvalError::Type("vector->list: expected vector".into())),
    }
}

fn builtin_list_to_vector(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("list->vector requires exactly 1 argument".into())); }
    let mut elems = Vec::new();
    let mut cur = &args[0];
    loop {
        match cur {
            Value::Nil => break,
            Value::Pair(car, cdr) => {
                elems.push(Value::clone(car));
                cur = cdr;
            }
            _ => return Err(EvalError::Type("list->vector: expected proper list".into())),
        }
    }
    Ok(Value::Vector(Rc::new(RefCell::new(elems))))
}

// --- L19: Exact arithmetic & rationals ---

fn builtin_exact_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(is_exact(&args[0])))
}

fn builtin_inexact_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Float(_))))
}

fn builtin_rational_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("rational? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_) | Value::Rational(_, _))))
}

fn builtin_integer_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("integer? requires exactly 1 argument".into())); }
    let result = match &args[0] {
        Value::Integer(_) => true,
        Value::Float(f) => f.fract() == 0.0 && f.is_finite(),
        Value::Rational(_, _) => false, // already simplified, so if denom != 1 it's not an integer
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn builtin_exact_to_inexact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("exact->inexact requires exactly 1 argument".into())); }
    Ok(Value::Float(value_to_f64(&args[0])?))
}

fn builtin_inexact_to_exact(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("inexact->exact requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Integer(_) | Value::Rational(_, _) => Ok(args[0].clone()),
        Value::Float(f) => {
            // Convert float to exact rational using continued fraction approximation
            if f.fract() == 0.0 && f.is_finite() {
                return Ok(Value::Integer(*f as i64));
            }
            // Use a simple approach: multiply by power of 2 to get exact fraction
            // For 0.5 -> 1/2, 0.333... -> try common fractions
            let (mut num, mut den) = float_to_rational(*f);
            let g = gcd(num.abs(), den.abs());
            num /= g;
            den /= g;
            if den < 0 { num = -num; den = -den; }
            if den == 1 {
                Ok(Value::Integer(num))
            } else {
                Ok(Value::Rational(num, den))
            }
        }
        other => Err(EvalError::Type(format!("expected number, got {}", other.display()))),
    }
}

fn float_to_rational(f: f64) -> (i64, i64) {
    if f == 0.0 { return (0, 1); }
    let sign = if f < 0.0 { -1i64 } else { 1 };
    let f = f.abs();
    // Use continued fraction to find best rational approximation
    let mut p0: i64 = 0;
    let mut q0: i64 = 1;
    let mut p1: i64 = 1;
    let mut q1: i64 = 0;
    let mut x = f;
    for _ in 0..64 {
        let a = x.floor() as i64;
        let p2 = a * p1 + p0;
        let q2 = a * q1 + q0;
        p0 = p1; q0 = q1;
        p1 = p2; q1 = q2;
        let approx = p1 as f64 / q1 as f64;
        if (approx - f).abs() < 1e-15 { break; }
        let rem = x - a as f64;
        if rem.abs() < 1e-15 { break; }
        x = 1.0 / rem;
        if x > 1e15 { break; }
    }
    (sign * p1, q1)
}

fn builtin_numerator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("numerator requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Integer(n) => Ok(Value::Integer(*n)),
        Value::Rational(n, _) => Ok(Value::Integer(*n)),
        other => Err(EvalError::Type(format!("numerator: expected rational, got {}", other.display()))),
    }
}

fn builtin_denominator(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("denominator requires exactly 1 argument".into())); }
    match &args[0] {
        Value::Integer(_) => Ok(Value::Integer(1)),
        Value::Rational(_, d) => Ok(Value::Integer(*d)),
        other => Err(EvalError::Type(format!("denominator: expected rational, got {}", other.display()))),
    }
}

fn default_env() -> Env {
    let env = new_env(None);
    let builtins: &[(&'static str, fn(&[Value]) -> Result<Value, EvalError>)] = &[
        ("+", builtin_add),
        ("-", builtin_sub),
        ("*", builtin_mul),
        ("/", builtin_div),
        ("<", builtin_lt),
        (">", builtin_gt),
        ("=", builtin_eq),
        ("<=", builtin_le),
        (">=", builtin_ge),
        ("not", builtin_not),
        ("cons", builtin_cons),
        ("car", builtin_car),
        ("cdr", builtin_cdr),
        ("null?", builtin_null),
        ("list", builtin_list),
        ("length", builtin_length),
        ("number?", builtin_number_pred),
        ("string?", builtin_string_pred),
        ("boolean?", builtin_boolean_pred),
        ("pair?", builtin_pair_pred),
        ("symbol?", builtin_symbol_pred),
        ("char?", builtin_char_pred),
        ("append", builtin_append),
        ("reverse", builtin_reverse),
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
        ("apply", builtin_apply_placeholder),
        // L13: numeric
        ("abs", builtin_abs),
        ("modulo", builtin_modulo),
        ("remainder", builtin_remainder),
        ("quotient", builtin_quotient),
        ("min", builtin_min),
        ("max", builtin_max),
        ("expt", builtin_expt),
        ("zero?", builtin_zero_pred),
        ("positive?", builtin_positive_pred),
        ("negative?", builtin_negative_pred),
        ("odd?", builtin_odd_pred),
        ("even?", builtin_even_pred),
        // L13: list
        ("list-ref", builtin_list_ref),
        ("list-tail", builtin_list_tail),
        ("list?", builtin_list_pred),
        ("assoc", builtin_assoc),
        ("eq?", builtin_eq_pred),
        ("equal?", builtin_equal_pred),
        ("map", builtin_map_placeholder),
        // L13: char
        ("char-alphabetic?", builtin_char_alphabetic),
        ("char-numeric?", builtin_char_numeric),
        ("char-upcase", builtin_char_upcase),
        ("char-downcase", builtin_char_downcase),
        ("char=?", builtin_char_eq),
        ("char<?", builtin_char_lt),
        // L13: string
        ("string=?", builtin_string_eq),
        ("string<?", builtin_string_lt),
        ("string-ci=?", builtin_string_ci_eq),
        ("string-upcase", builtin_string_upcase),
        ("string-downcase", builtin_string_downcase),
        // L14: string immutability
        ("string->list", builtin_string_to_list),
        ("list->string", builtin_list_to_string),
        ("char->integer", builtin_char_to_integer),
        ("integer->char", builtin_integer_to_char),
        // L15
        ("eqv?", builtin_eqv_pred),
        ("vector", builtin_vector),
        ("make-vector", builtin_make_vector),
        ("vector-ref", builtin_vector_ref),
        ("vector-set!", builtin_vector_set),
        ("vector-length", builtin_vector_length),
        ("vector?", builtin_vector_pred),
        ("vector->list", builtin_vector_to_list),
        ("list->vector", builtin_list_to_vector),
        // L19: exact arithmetic & rationals
        ("exact?", builtin_exact_pred),
        ("inexact?", builtin_inexact_pred),
        ("rational?", builtin_rational_pred),
        ("integer?", builtin_integer_pred),
        ("exact->inexact", builtin_exact_to_inexact),
        ("inexact->exact", builtin_inexact_to_exact),
        ("numerator", builtin_numerator),
        ("denominator", builtin_denominator),
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name, func));
    }
    env_set(&env, "call/cc".to_string(), Value::CallCC);
    env_set(&env, "call-with-current-continuation".to_string(), Value::CallCC);
    env_set(&env, "dynamic-wind".to_string(), Value::DynamicWind);
    env_set(&env, "raise".to_string(), Value::Raise);
    env_set(&env, "with-exception-handler".to_string(), Value::WithExceptionHandler);
    env_set(&env, "values".to_string(), Value::ValuesProc);
    env_set(&env, "call-with-values".to_string(), Value::CallWithValues);
    env
}

// --- Token with position ---

#[derive(Debug, Clone)]
struct Token {
    text: String,
    line: usize,
    col: usize,
}

// --- Parser ---

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
                let start_col = col;
                let start_line = line;
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
                        i += 1;
                        col += 1;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
            '#' if i + 1 < chars.len() && chars[i + 1] == '\\' => {
                let start_col = col;
                i += 2; // skip #\
                col += 2;
                if i < chars.len() {
                    let ch_start = i;
                    if chars[i].is_alphabetic() {
                        while i < chars.len() && chars[i].is_alphabetic() {
                            i += 1;
                            col += 1;
                        }
                        let name = &chars[ch_start..i];
                        let name_str: String = name.iter().collect();
                        let tok = format!("#\\{}", name_str);
                        tokens.push(Token { text: tok, line, col: start_col });
                    } else {
                        let tok = format!("#\\{}", chars[i]);
                        i += 1;
                        col += 1;
                        tokens.push(Token { text: tok, line, col: start_col });
                    }
                }
            }
            '#' if i + 1 < chars.len() && (chars[i + 1] == 't' || chars[i + 1] == 'f') => {
                let start_col = col;
                let mut tok = String::from('#');
                i += 1;
                col += 1;
                tok.push(chars[i]);
                i += 1;
                col += 1;
                tokens.push(Token { text: tok, line, col: start_col });
            }
            _ => {
                let start_col = col;
                let mut tok = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' | '\'')
                {
                    tok.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: tok, line, col: start_col });
            }
        }
    }
    tokens
}

fn parse_tokens(tokens: &[Token], pos: &mut usize) -> Result<Expr, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[*pos];
    let tline = token.line;
    let tcol = token.col;

    if token.text == "'" {
        *pos += 1;
        let inner = parse_tokens(tokens, pos)?;
        Ok(Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), tline, tcol),
            inner,
        ]), tline, tcol))
    } else if token.text == "(" {
        *pos += 1;
        let mut list = Vec::new();
        while *pos < tokens.len() && tokens[*pos].text != ")" {
            list.push(parse_tokens(tokens, pos)?);
        }
        if *pos >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into()));
        }
        *pos += 1;
        Ok(Expr::new(ExprKind::List(list), tline, tcol))
    } else if token.text == ")" {
        Err(EvalError::Parse("unexpected ')'".into()))
    } else if token.text == "#t" {
        *pos += 1;
        Ok(Expr::new(ExprKind::Boolean(true), tline, tcol))
    } else if token.text == "#f" {
        *pos += 1;
        Ok(Expr::new(ExprKind::Boolean(false), tline, tcol))
    } else if token.text.starts_with('"') {
        *pos += 1;
        let inner = &token.text[1..token.text.len() - 1];
        Ok(Expr::new(ExprKind::Str(inner.to_string()), tline, tcol))
    } else if token.text.starts_with("#\\") {
        *pos += 1;
        let char_name = &token.text[2..];
        let ch = match char_name {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().unwrap(),
            _ => return Err(EvalError::Parse(format!("unknown character: {}", token.text))),
        };
        Ok(Expr::new(ExprKind::Char(ch), tline, tcol))
    } else if let Ok(n) = token.text.parse::<i64>() {
        *pos += 1;
        Ok(Expr::new(ExprKind::Integer(n), tline, tcol))
    } else if let Some(slash_pos) = token.text.find('/') {
        // Try parsing as rational: num/den
        if slash_pos > 0 && slash_pos < token.text.len() - 1 {
            if let (Ok(n), Ok(d)) = (
                token.text[..slash_pos].parse::<i64>(),
                token.text[slash_pos + 1..].parse::<i64>(),
            ) {
                *pos += 1;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                let sign = if d < 0 { -1 } else { 1 };
                let (n, d) = (n * sign, d * sign);
                let g = gcd(n, d);
                let (n, d) = (n / g, d / g);
                if d == 1 {
                    return Ok(Expr::new(ExprKind::Integer(n), tline, tcol));
                }
                return Ok(Expr::new(ExprKind::Rational(n, d), tline, tcol));
            }
        }
        *pos += 1;
        Ok(Expr::new(ExprKind::Symbol(token.text.clone()), tline, tcol))
    } else if let Ok(f) = token.text.parse::<f64>() {
        *pos += 1;
        Ok(Expr::new(ExprKind::Float(f), tline, tcol))
    } else {
        *pos += 1;
        Ok(Expr::new(ExprKind::Symbol(token.text.clone()), tline, tcol))
    }
}

fn parse(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_tokens(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// --- Helpers ---

fn parse_params(form: &Expr, param_exprs: &[Expr]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(form.wrap_err(EvalError::BadSyntax("expected rest parameter after dot".into())));
                }
                match &param_exprs[i + 1].kind {
                    ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                    _ => return Err(form.wrap_err(EvalError::BadSyntax("expected symbol for rest parameter".into()))),
                }
                i += 2;
                break;
            }
            ExprKind::Symbol(s) => {
                params.push(s.clone());
                i += 1;
            }
            _ => return Err(form.wrap_err(EvalError::BadSyntax("expected parameter name".into()))),
        }
    }
    Ok((params, rest_param))
}

fn eval_lambda(form: &Expr, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(form.wrap_err(EvalError::BadSyntax(
            "lambda: expected params and body".into(),
        )));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(form, param_exprs)?,
        ExprKind::Symbol(s) => {
            (Vec::new(), Some(s.clone()))
        }
        _ => {
            return Err(form.wrap_err(EvalError::BadSyntax(
                "lambda: expected parameter list".into(),
            )))
        }
    };
    Ok(Value::Lambda {
        params,
        rest_param,
        body: args[1..].to_vec(),
        env: env.clone(),
    })
}

fn eval_quote(form: &Expr, args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(form.wrap_err(EvalError::BadSyntax("quote: expected 1 argument".into())));
    }
    expr_to_value(&args[0])
}

fn expr_to_value(expr: &Expr) -> Result<Value, EvalError> {
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(n, d) => Ok(Value::Rational(*n, *d)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                let v = expr_to_value(item)?;
                result = Value::Pair(Rc::new(v), Rc::new(result));
            }
            Ok(result)
        }
    }
}

fn bind_args(form: &Expr, params: &[String], rest_param: &Option<String>, args: &[Value], closure_env: Env) -> Result<Env, EvalError> {
    if let Some(ref _rp) = rest_param {
        if args.len() < params.len() {
            return Err(form.wrap_err(EvalError::Arity(format!(
                "expected at least {} arguments, got {}",
                params.len(), args.len()
            ))));
        }
    } else if args.len() != params.len() {
        return Err(form.wrap_err(EvalError::Arity(format!(
            "expected {} arguments, got {}",
            params.len(), args.len()
        ))));
    }
    let local_env = new_env(Some(closure_env));
    for (p, a) in params.iter().zip(args.iter()) {
        env_set(&local_env, p.clone(), a.clone());
    }
    if let Some(ref rp) = rest_param {
        let rest = builtin_list(&args[params.len()..]).unwrap();
        env_set(&local_env, rp.clone(), rest);
    }
    Ok(local_env)
}

fn parse_let_bindings(form: &Expr, bindings: &[Expr]) -> Result<Vec<(String, Expr)>, EvalError> {
    let mut result = Vec::new();
    for binding in bindings {
        match &binding.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(form.wrap_err(EvalError::BadSyntax("let: expected symbol".into()))),
                };
                result.push((name, pair[1].clone()));
            }
            _ => return Err(form.wrap_err(EvalError::BadSyntax("let: bad binding".into()))),
        }
    }
    Ok(result)
}

// --- Macro expansion ---

#[derive(Clone)]
enum MacroBinding {
    Single(Expr),
    List(Vec<Expr>),
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String], vars: &mut Vec<String>) {
    match &pattern.kind {
        ExprKind::Symbol(s) if s != "..." && s != "_" && !literals.contains(s) => {
            if !vars.contains(s) {
                vars.push(s.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_pattern_vars(item, literals, vars);
            }
        }
        _ => {}
    }
}

fn match_pattern(pattern: &Expr, input: &Expr, literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if s == "..." => false,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&input.kind, ExprKind::Symbol(is) if is == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        ExprKind::List(pat_items) => {
            if let ExprKind::List(inp_items) = &input.kind {
                match_list_pattern(pat_items, inp_items, literals, bindings)
            } else {
                false
            }
        }
        ExprKind::Integer(n) => matches!(&input.kind, ExprKind::Integer(m) if n == m),
        ExprKind::Float(f) => matches!(&input.kind, ExprKind::Float(g) if f == g),
        ExprKind::Rational(n, d) => matches!(&input.kind, ExprKind::Rational(n2, d2) if n == n2 && d == d2),
        ExprKind::Boolean(b) => matches!(&input.kind, ExprKind::Boolean(b2) if b == b2),
        ExprKind::Str(s) => matches!(&input.kind, ExprKind::Str(s2) if s == s2),
        ExprKind::Char(c) => matches!(&input.kind, ExprKind::Char(c2) if c == c2),
    }
}

fn match_list_pattern(patterns: &[Expr], inputs: &[Expr], literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    let ellipsis_pos = patterns.iter().position(|p| matches!(&p.kind, ExprKind::Symbol(s) if s == "..."));

    if let Some(ep) = ellipsis_pos {
        if ep == 0 { return false; }
        let before = &patterns[..ep - 1];
        let repeated = &patterns[ep - 1];
        let after = &patterns[ep + 1..];
        let min_required = before.len() + after.len();
        if inputs.len() < min_required { return false; }

        for (p, i) in before.iter().zip(inputs.iter()) {
            if !match_pattern(p, i, literals, bindings) { return false; }
        }

        let after_start = inputs.len() - after.len();
        for (p, i) in after.iter().zip(inputs[after_start..].iter()) {
            if !match_pattern(p, i, literals, bindings) { return false; }
        }

        let middle = &inputs[before.len()..after_start];
        let mut repeated_vars = Vec::new();
        collect_pattern_vars(repeated, literals, &mut repeated_vars);
        let mut list_bindings: HashMap<String, Vec<Expr>> = HashMap::new();
        for var in &repeated_vars {
            list_bindings.insert(var.clone(), Vec::new());
        }
        for input in middle {
            let mut sub_bindings = HashMap::new();
            if !match_pattern(repeated, input, literals, &mut sub_bindings) { return false; }
            for var in &repeated_vars {
                if let Some(MacroBinding::Single(expr)) = sub_bindings.get(var) {
                    list_bindings.get_mut(var).unwrap().push(expr.clone());
                }
            }
        }
        for (var, exprs) in list_bindings {
            bindings.insert(var, MacroBinding::List(exprs));
        }
        true
    } else {
        if patterns.len() != inputs.len() { return false; }
        for (p, i) in patterns.iter().zip(inputs.iter()) {
            if !match_pattern(p, i, literals, bindings) { return false; }
        }
        true
    }
}

fn is_special_form(s: &str) -> bool {
    matches!(s, "define" | "lambda" | "quote" | "set!" | "if" | "begin" |
             "cond" | "and" | "or" | "let" | "string-set!" | "define-syntax" |
             "syntax-rules" | "else" | "letrec" | "letrec*" | "case" | "do")
}

fn expand_template(template: &Expr, bindings: &HashMap<String, MacroBinding>, renames: &mut HashMap<String, String>) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(expr) => expr.clone(),
                    MacroBinding::List(_) => template.clone(),
                }
            } else if is_special_form(s) {
                template.clone()
            } else {
                let renamed = renames.entry(s.clone()).or_insert_with(|| gensym(s)).clone();
                Expr::new(ExprKind::Symbol(renamed), template.line, template.col)
            }
        }
        ExprKind::List(items) => {
            let expanded = expand_list_template(items, bindings, renames, template.line, template.col);
            Expr::new(ExprKind::List(expanded), template.line, template.col)
        }
        _ => template.clone(),
    }
}

fn expand_list_template(items: &[Expr], bindings: &HashMap<String, MacroBinding>, renames: &mut HashMap<String, String>, line: usize, col: usize) -> Vec<Expr> {
    let mut result = Vec::new();
    let mut i = 0;
    let _ = (line, col);

    while i < items.len() {
        if i + 1 < items.len() && matches!(&items[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
            let template_elem = &items[i];
            let repeat_count = find_ellipsis_count(template_elem, bindings);
            for j in 0..repeat_count {
                let mut iter_bindings = bindings.clone();
                for (name, binding) in bindings {
                    if let MacroBinding::List(exprs) = binding {
                        if j < exprs.len() {
                            iter_bindings.insert(name.clone(), MacroBinding::Single(exprs[j].clone()));
                        }
                    }
                }
                result.push(expand_template(template_elem, &iter_bindings, renames));
            }
            i += 2;
        } else {
            result.push(expand_template(&items[i], bindings, renames));
            i += 1;
        }
    }
    result
}

fn find_ellipsis_count(template: &Expr, bindings: &HashMap<String, MacroBinding>) -> usize {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(MacroBinding::List(exprs)) = bindings.get(s) {
                return exprs.len();
            }
            0
        }
        ExprKind::List(items) => {
            for item in items {
                let count = find_ellipsis_count(item, bindings);
                if count > 0 { return count; }
            }
            0
        }
        _ => 0,
    }
}

fn expand_macro(items: &[Expr], expr: &Expr, literals: &[String], rules: &[(Expr, Expr)], def_env: &Env, env: &Env) -> Result<(Expr, Env), EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if let ExprKind::List(pat_items) = &pattern.kind {
            if pat_items.len() >= 1 && items.len() >= 1
                && match_list_pattern(&pat_items[1..], &items[1..], literals, &mut bindings)
            {
                let mut renames = HashMap::new();
                let expanded = expand_template(template, &bindings, &mut renames);

                let has_def_bindings = renames.iter().any(|(orig, _)| env_get(def_env, orig).is_ok());
                let eval_env = if has_def_bindings {
                    let child = new_env(Some(env.clone()));
                    for (original, renamed) in &renames {
                        if let Ok(val) = env_get(def_env, original) {
                            env_set(&child, renamed.clone(), val);
                        }
                    }
                    child
                } else {
                    env.clone()
                };

                return Ok((expanded, eval_env));
            }
        }
    }
    Err(expr.wrap_err(EvalError::BadSyntax("no matching pattern in syntax-rules".into())))
}

// --- CEK Machine ---

enum State {
    Eval(Expr, Env),
    Apply(Value),
    RaiseException(Value),
}

fn start_guard_clauses(exception: Value, clauses: &[Expr], env: &Env, kont: &mut Rc<Kont>, next: Rc<Kont>) -> Result<State, EvalError> {
    if clauses.is_empty() {
        return Ok(State::RaiseException(exception));
    }
    let clause = &clauses[0];
    match &clause.kind {
        ExprKind::List(citems) if !citems.is_empty() => {
            let is_else = matches!(&citems[0].kind, ExprKind::Symbol(s) if s == "else");
            if is_else {
                *kont = next;
                Ok(eval_body_state(&citems[1..], env.clone(), kont))
            } else {
                let body = citems[1..].to_vec();
                let remaining = clauses[1..].to_vec();
                *kont = Rc::new(Kont::GuardClauseTest {
                    exception,
                    body,
                    remaining_clauses: remaining,
                    env: env.clone(),
                    next,
                });
                Ok(State::Eval(citems[0].clone(), env.clone()))
            }
        }
        _ => Err(EvalError::BadSyntax("guard: bad clause".into())),
    }
}

fn start_cond_clauses(form: &Expr, clauses: &[Expr], env: &Env, kont: &mut Rc<Kont>) -> Result<State, EvalError> {
    if clauses.is_empty() {
        return Ok(State::Apply(Value::Void));
    }
    let clause = &clauses[0];
    match &clause.kind {
        ExprKind::List(citems) if !citems.is_empty() => {
            let is_else = matches!(&citems[0].kind, ExprKind::Symbol(s) if s == "else");
            if is_else {
                let body = &citems[1..];
                Ok(eval_body_state(body, env.clone(), kont))
            } else {
                let body = citems[1..].to_vec();
                let remaining = clauses[1..].to_vec();
                *kont = Rc::new(Kont::CondTest {
                    form: form.clone(),
                    body,
                    remaining_clauses: remaining,
                    env: env.clone(),
                    next: kont.clone(),
                });
                Ok(State::Eval(citems[0].clone(), env.clone()))
            }
        }
        _ => Err(clause.wrap_err(EvalError::BadSyntax("cond: bad clause".into()))),
    }
}

fn eval_body_state(body: &[Expr], env: Env, kont: &mut Rc<Kont>) -> State {
    if body.is_empty() {
        State::Apply(Value::Void)
    } else if body.len() == 1 {
        State::Eval(body[0].clone(), env)
    } else {
        *kont = Rc::new(Kont::Seq {
            remaining: body[1..].to_vec(),
            env: env.clone(),
            next: kont.clone(),
        });
        State::Eval(body[0].clone(), env)
    }
}

fn dummy_form() -> Expr {
    Expr::new(ExprKind::Symbol("dynamic-wind".into()), 0, 0)
}

fn start_wind_chain(
    remaining: Vec<WindOp>,
    cont_value: Value,
    target_kont: Rc<Kont>,
    kont: &mut Rc<Kont>,
    form: &Expr,
) -> Result<State, EvalError> {
    if remaining.is_empty() {
        *kont = target_kont;
        return Ok(State::Apply(cont_value));
    }
    let op = remaining[0].clone();
    let rest = remaining[1..].to_vec();
    match op {
        WindOp::Unwind(out_thunk) => {
            WIND_STACK.with(|ws| ws.borrow_mut().pop());
            *kont = Rc::new(Kont::WindChain {
                remaining: rest,
                cont_value,
                target_kont,
            });
            apply_function(out_thunk, vec![], form, kont)
        }
        WindOp::Rewind(in_thunk, entry) => {
            *kont = Rc::new(Kont::WindChainPush {
                wind_entry: entry,
                remaining: rest,
                cont_value,
                target_kont,
            });
            apply_function(in_thunk, vec![], form, kont)
        }
    }
}

fn apply_function(func: Value, args: Vec<Value>, form: &Expr, kont: &mut Rc<Kont>) -> Result<State, EvalError> {
    match func {
        Value::Builtin("map", _) => {
            if args.len() < 2 {
                return Err(form.wrap_err(EvalError::Arity("map: expected procedure and at least one list".into())));
            }
            let proc = args[0].clone();
            let lists = &args[1..];
            // Collect all lists into vectors of elements
            let mut iters: Vec<Vec<Value>> = Vec::new();
            for list in lists {
                let mut elems = Vec::new();
                let mut cur = list;
                loop {
                    match cur {
                        Value::Nil => break,
                        Value::Pair(car, cdr) => {
                            elems.push(Value::clone(car));
                            cur = cdr;
                        }
                        _ => return Err(form.wrap_err(EvalError::Type("map: expected list".into()))),
                    }
                }
                iters.push(elems);
            }
            let len = iters[0].len();
            for iter in &iters {
                if iter.len() != len {
                    return Err(form.wrap_err(EvalError::Arity("map: lists must have same length".into())));
                }
            }
            // Apply proc to each set of elements
            let mut results = Vec::new();
            for i in 0..len {
                let call_args: Vec<Value> = iters.iter().map(|v| v[i].clone()).collect();
                let result = match &proc {
                    Value::Builtin(_, fp) => fp(&call_args).map_err(|e| form.wrap_err(e))?,
                    Value::Lambda { params, rest_param, body, env } => {
                        let local_env = bind_args(form, params, rest_param, &call_args, env.clone())?;
                        let mut r = Value::Void;
                        for expr in body {
                            r = eval_cek(expr.clone(), local_env.clone(), Rc::new(Kont::Done))?;
                        }
                        r
                    }
                    _ => return Err(form.wrap_err(EvalError::Type("map: first argument must be a procedure".into()))),
                };
                results.push(result);
            }
            let mut result = Value::Nil;
            for v in results.into_iter().rev() {
                result = Value::Pair(Rc::new(v), Rc::new(result));
            }
            Ok(State::Apply(result))
        }
        Value::Builtin("apply", _) => {
            if args.len() < 2 {
                return Err(form.wrap_err(EvalError::Arity("apply: expected at least 2 arguments".into())));
            }
            let actual_func = args[0].clone();
            let last = &args[args.len() - 1];
            let mut final_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            let mut cur_list = last;
            loop {
                match cur_list {
                    Value::Nil => break,
                    Value::Pair(car, cdr) => {
                        final_args.push(Value::clone(car));
                        cur_list = cdr;
                    }
                    _ => return Err(form.wrap_err(EvalError::Type("apply: last argument must be a list".into()))),
                }
            }
            apply_function(actual_func, final_args, form, kont)
        }
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = bind_args(form, &params, &rest_param, &args, env)?;
            Ok(eval_body_state(&body, local_env, kont))
        }
        Value::Builtin(_, fp) => {
            let result = fp(&args).map_err(|e| form.wrap_err(e))?;
            Ok(State::Apply(result))
        }
        Value::CallCC => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity("call/cc requires exactly 1 argument".into())));
            }
            let winds = WIND_STACK.with(|ws| ws.borrow().clone());
            let cont_value = Value::Continuation(kont.clone(), winds);
            let proc = args.into_iter().next().unwrap();
            apply_function(proc, vec![cont_value], form, kont)
        }
        Value::Raise => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity("raise requires exactly 1 argument".into())));
            }
            let exception = args.into_iter().next().unwrap();
            Ok(State::RaiseException(exception))
        }
        Value::WithExceptionHandler => {
            if args.len() != 2 {
                return Err(form.wrap_err(EvalError::Arity("with-exception-handler requires exactly 2 arguments".into())));
            }
            let mut it = args.into_iter();
            let handler = it.next().unwrap();
            let thunk = it.next().unwrap();
            let winds = WIND_STACK.with(|ws| ws.borrow().clone());
            EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().push(ExceptionHandlerEntry::WithHandler {
                handler,
                kont: kont.clone(),
                winds,
            }));
            *kont = Rc::new(Kont::ExceptionHandlerCleanup { next: kont.clone() });
            apply_function(thunk, vec![], form, kont)
        }
        Value::RecordConstructor { type_id, type_name, field_count } => {
            if args.len() != field_count {
                return Err(form.wrap_err(EvalError::Arity(
                    format!("{}: expected {} arguments, got {}", type_name, field_count, args.len())
                )));
            }
            Ok(State::Apply(Value::Record { type_id, fields: args }))
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity("record predicate: expected 1 argument".into())));
            }
            let result = matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id);
            Ok(State::Apply(Value::Boolean(result)))
        }
        Value::RecordAccessor { type_id, type_name, field_index, field_name } => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity(
                    format!("{}: expected 1 argument", field_name)
                )));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields } if *tid == type_id => {
                    Ok(State::Apply(fields[field_index].clone()))
                }
                _ => Err(form.wrap_err(EvalError::Type(
                    format!("{}: expected record of type {}", field_name, type_name)
                ))),
            }
        }
        Value::DynamicWind => {
            if args.len() != 3 {
                return Err(form.wrap_err(EvalError::Arity("dynamic-wind requires exactly 3 arguments".into())));
            }
            let mut it = args.into_iter();
            let in_thunk = it.next().unwrap();
            let body_thunk = it.next().unwrap();
            let out_thunk = it.next().unwrap();
            let wind_entry: WindEntry = Rc::new((in_thunk.clone(), out_thunk.clone()));
            *kont = Rc::new(Kont::DynamicWindRunBody {
                body_thunk,
                out_thunk,
                wind_entry,
                next: kont.clone(),
            });
            // Call in-thunk (zero args)
            apply_function(in_thunk, vec![], form, kont)
        }
        Value::ValuesProc => {
            if args.len() == 1 {
                Ok(State::Apply(args.into_iter().next().unwrap()))
            } else {
                Ok(State::Apply(Value::MultipleValues(args)))
            }
        }
        Value::CallWithValues => {
            if args.len() != 2 {
                return Err(form.wrap_err(EvalError::Arity("call-with-values requires exactly 2 arguments".into())));
            }
            let mut it = args.into_iter();
            let producer = it.next().unwrap();
            let consumer = it.next().unwrap();
            *kont = Rc::new(Kont::CallWithValuesConsumer {
                consumer,
                form: form.clone(),
                next: kont.clone(),
            });
            // Call producer with no args
            apply_function(producer, vec![], form, kont)
        }
        Value::Continuation(saved_kont, saved_winds) => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity("continuation requires exactly 1 argument".into())));
            }
            let value = args.into_iter().next().unwrap();
            let current_winds = WIND_STACK.with(|ws| ws.borrow().clone());
            // Find common prefix length (by Rc identity)
            let common_len = current_winds.iter().zip(saved_winds.iter())
                .take_while(|(a, b)| Rc::ptr_eq(a, b))
                .count();
            // Build wind operations: unwind current (inner to outer), rewind target (outer to inner)
            let mut ops = Vec::new();
            // Unwind: from innermost to outermost (reverse order from common_len+1 to end)
            for i in (common_len..current_winds.len()).rev() {
                ops.push(WindOp::Unwind(current_winds[i].1.clone()));
            }
            // Rewind: from outermost to innermost
            for i in common_len..saved_winds.len() {
                ops.push(WindOp::Rewind(saved_winds[i].0.clone(), saved_winds[i].clone()));
            }
            if ops.is_empty() {
                *kont = saved_kont;
                Ok(State::Apply(value))
            } else {
                start_wind_chain(ops, value, saved_kont, kont, form)
            }
        }
        _ => Err(form.wrap_err(EvalError::Type(format!(
            "not a procedure: {}",
            func.display()
        )))),
    }
}

fn eval_cek(initial_expr: Expr, initial_env: Env, initial_kont: Rc<Kont>) -> Result<Value, EvalError> {
    let mut state = State::Eval(initial_expr, initial_env);
    let mut kont = initial_kont;

    loop {
        match state {
            State::Eval(expr, env) => {
                state = match &expr.kind {
                    ExprKind::Integer(n) => State::Apply(Value::Integer(*n)),
                    ExprKind::Float(f) => State::Apply(Value::Float(*f)),
                    ExprKind::Rational(n, d) => State::Apply(Value::Rational(*n, *d)),
                    ExprKind::Boolean(b) => State::Apply(Value::Boolean(*b)),
                    ExprKind::Str(s) => State::Apply(Value::Str(s.clone())),
                    ExprKind::Char(c) => State::Apply(Value::Char(*c)),
                    ExprKind::Symbol(name) => {
                        let val = env_get(&env, name).map_err(|e| expr.wrap_err(e))?;
                        State::Apply(val)
                    }
                    ExprKind::List(items) => {
                        if items.is_empty() {
                            return Err(expr.wrap_err(EvalError::BadSyntax("empty application".into())));
                        }

                        // Check for define-record-type
                        let is_define_record_type = matches!(&items[0].kind, ExprKind::Symbol(ref s) if s == "define-record-type");

                        if is_define_record_type {
                            // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
                            let args = &items[1..];
                            if args.len() < 3 {
                                return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: expected type name, constructor, predicate, and field specs".into())));
                            }
                            // type name (ignored for runtime, but we extract it)
                            let _type_name_sym = match &args[0].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: expected type name symbol".into()))),
                            };
                            // constructor: (constructor-name field ...)
                            let (ctor_name, ctor_fields) = match &args[1].kind {
                                ExprKind::List(ctor_items) if ctor_items.len() >= 1 => {
                                    let name = match &ctor_items[0].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: constructor name must be symbol".into()))),
                                    };
                                    let mut fields = Vec::new();
                                    for item in &ctor_items[1..] {
                                        match &item.kind {
                                            ExprKind::Symbol(s) => fields.push(s.clone()),
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: constructor field must be symbol".into()))),
                                        }
                                    }
                                    (name, fields)
                                }
                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: expected constructor spec".into()))),
                            };
                            // predicate
                            let pred_name = match &args[2].kind {
                                ExprKind::Symbol(s) => s.clone(),
                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: expected predicate name".into()))),
                            };
                            // field specs: (field accessor) ...
                            let mut field_accessors: Vec<(String, String)> = Vec::new();
                            for field_spec in &args[3..] {
                                match &field_spec.kind {
                                    ExprKind::List(fs) if fs.len() == 2 => {
                                        let field_name = match &fs[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: field name must be symbol".into()))),
                                        };
                                        let accessor_name = match &fs[1].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: accessor name must be symbol".into()))),
                                        };
                                        field_accessors.push((field_name, accessor_name));
                                    }
                                    _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-record-type: expected (field accessor) spec".into()))),
                                }
                            }
                            // Allocate unique type id
                            let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::SeqCst);
                            let type_name = _type_name_sym;
                            // Bind constructor
                            env_set(&env, ctor_name, Value::RecordConstructor {
                                type_id,
                                type_name: type_name.clone(),
                                field_count: ctor_fields.len(),
                            });
                            // Bind predicate
                            env_set(&env, pred_name, Value::RecordPredicate { type_id });
                            // Bind accessors
                            for (field_name, accessor_name) in &field_accessors {
                                // Find the field index in the constructor fields list
                                let idx = ctor_fields.iter().position(|f| f == field_name)
                                    .ok_or_else(|| expr.wrap_err(EvalError::BadSyntax(
                                        format!("define-record-type: field {} not in constructor", field_name)
                                    )))?;
                                env_set(&env, accessor_name.clone(), Value::RecordAccessor {
                                    type_id,
                                    type_name: type_name.clone(),
                                    field_index: idx,
                                    field_name: field_name.clone(),
                                });
                            }
                            State::Apply(Value::Void)
                        }

                        // Check for define-syntax
                        else { let is_define_syntax = matches!(&items[0].kind, ExprKind::Symbol(ref s) if s == "define-syntax");

                        if is_define_syntax {
                            let args = &items[1..];
                            if args.len() != 2 {
                                return Err(expr.wrap_err(EvalError::BadSyntax("define-syntax: expected name and transformer".into())));
                            }
                            let name = match &args[0].kind {
                                ExprKind::Symbol(n) => n.clone(),
                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-syntax: expected symbol".into()))),
                            };
                            match &args[1].kind {
                                ExprKind::List(tr_items) if !tr_items.is_empty() && matches!(&tr_items[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") => {
                                    if tr_items.len() < 2 {
                                        return Err(expr.wrap_err(EvalError::BadSyntax("syntax-rules: expected literals list".into())));
                                    }
                                    let literals = match &tr_items[1].kind {
                                        ExprKind::List(lits) => {
                                            let mut lit_names = Vec::new();
                                            for lit in lits {
                                                match &lit.kind {
                                                    ExprKind::Symbol(s) => lit_names.push(s.clone()),
                                                    _ => return Err(expr.wrap_err(EvalError::BadSyntax("syntax-rules: literals must be symbols".into()))),
                                                }
                                            }
                                            lit_names
                                        }
                                        _ => return Err(expr.wrap_err(EvalError::BadSyntax("syntax-rules: expected literals list".into()))),
                                    };
                                    let mut rules = Vec::new();
                                    for rule_expr in &tr_items[2..] {
                                        match &rule_expr.kind {
                                            ExprKind::List(rule_parts) if rule_parts.len() == 2 => {
                                                rules.push((rule_parts[0].clone(), rule_parts[1].clone()));
                                            }
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("syntax-rules: each rule must be (pattern template)".into()))),
                                        }
                                    }
                                    let macro_val = Value::Macro { literals, rules, def_env: env.clone() };
                                    env_set(&env, name, macro_val);
                                    State::Apply(Value::Void)
                                }
                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("define-syntax: expected syntax-rules".into()))),
                            }
                        } else if let ExprKind::Symbol(ref s) = items[0].kind {
                                // Check for special forms first (avoid env lookup for perf)
                                let is_special = matches!(s.as_str(), "define" | "quote" | "lambda" | "set!" |
                                    "if" | "begin" | "cond" | "and" | "or" | "let" | "string-set!" |
                                    "letrec" | "letrec*" | "case" | "do" | "guard");

                        if is_special {
                            if let ExprKind::Symbol(ref op) = items[0].kind {
                                match op.as_str() {
                                    "define" => {
                                        let args = &items[1..];
                                        if args.is_empty() {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("define: missing name".into())));
                                        }
                                        match &args[0].kind {
                                            ExprKind::Symbol(name) => {
                                                if args.len() != 2 {
                                                    return Err(expr.wrap_err(EvalError::BadSyntax("define: expected 2 parts".into())));
                                                }
                                                kont = Rc::new(Kont::Define { name: name.clone(), env: env.clone(), next: kont });
                                                State::Eval(args[1].clone(), env)
                                            }
                                            ExprKind::List(sig) => {
                                                if sig.is_empty() {
                                                    return Err(expr.wrap_err(EvalError::BadSyntax("define: empty signature".into())));
                                                }
                                                let name = match &sig[0].kind {
                                                    ExprKind::Symbol(n) => n.clone(),
                                                    _ => return Err(expr.wrap_err(EvalError::BadSyntax("define: expected symbol".into()))),
                                                };
                                                let (params, rest_param) = parse_params(&expr, &sig[1..])?;
                                                let body = args[1..].to_vec();
                                                let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
                                                env_set(&env, name, lambda);
                                                State::Apply(Value::Void)
                                            }
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("define: bad syntax".into()))),
                                        }
                                    }
                                    "quote" => {
                                        let val = eval_quote(&expr, &items[1..])?;
                                        State::Apply(val)
                                    }
                                    "lambda" => {
                                        let val = eval_lambda(&expr, &items[1..], &env)?;
                                        State::Apply(val)
                                    }
                                    "set!" => {
                                        let args = &items[1..];
                                        if args.len() != 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("set! requires exactly 2 arguments".into())));
                                        }
                                        let name = match &args[0].kind {
                                            ExprKind::Symbol(n) => n.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("set!: first argument must be a variable".into()))),
                                        };
                                        kont = Rc::new(Kont::Set { form: expr.clone(), name, env: env.clone(), next: kont });
                                        State::Eval(args[1].clone(), env)
                                    }
                                    "if" => {
                                        let args = &items[1..];
                                        if args.len() < 2 || args.len() > 3 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("if: expected 2 or 3 parts".into())));
                                        }
                                        kont = Rc::new(Kont::If {
                                            then_expr: args[1].clone(),
                                            else_expr: args.get(2).cloned(),
                                            env: env.clone(),
                                            next: kont,
                                        });
                                        State::Eval(args[0].clone(), env)
                                    }
                                    "begin" => {
                                        let args = &items[1..];
                                        eval_body_state(args, env, &mut kont)
                                    }
                                    "cond" => {
                                        start_cond_clauses(&expr, &items[1..], &env, &mut kont)?
                                    }
                                    "and" => {
                                        let args = &items[1..];
                                        if args.is_empty() {
                                            State::Apply(Value::Boolean(true))
                                        } else if args.len() == 1 {
                                            State::Eval(args[0].clone(), env)
                                        } else {
                                            kont = Rc::new(Kont::And {
                                                remaining: args[1..].to_vec(),
                                                env: env.clone(),
                                                next: kont,
                                            });
                                            State::Eval(args[0].clone(), env)
                                        }
                                    }
                                    "or" => {
                                        let args = &items[1..];
                                        if args.is_empty() {
                                            State::Apply(Value::Boolean(false))
                                        } else if args.len() == 1 {
                                            State::Eval(args[0].clone(), env)
                                        } else {
                                            kont = Rc::new(Kont::Or {
                                                remaining: args[1..].to_vec(),
                                                env: env.clone(),
                                                next: kont,
                                            });
                                            State::Eval(args[0].clone(), env)
                                        }
                                    }
                                    "let" => {
                                        let args = &items[1..];
                                        if args.len() < 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("let: expected bindings and body".into())));
                                        }
                                        // Named let
                                        if let ExprKind::Symbol(ref loop_name) = args[0].kind {
                                            if args.len() < 3 {
                                                return Err(expr.wrap_err(EvalError::BadSyntax("let: expected bindings and body".into())));
                                            }
                                            let bindings_expr = match &args[1].kind {
                                                ExprKind::List(b) => b,
                                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("let: expected binding list".into()))),
                                            };
                                            let parsed = parse_let_bindings(&expr, bindings_expr)?;
                                            let body = args[2..].to_vec();
                                            if parsed.is_empty() {
                                                let local_env = new_env(Some(env.clone()));
                                                let lambda = Value::Lambda { params: vec![], rest_param: None, body: body.clone(), env: local_env.clone() };
                                                env_set(&local_env, loop_name.clone(), lambda);
                                                eval_body_state(&body, local_env, &mut kont)
                                            } else {
                                                let (first_name, first_expr) = parsed[0].clone();
                                                let remaining = parsed[1..].to_vec();
                                                kont = Rc::new(Kont::NamedLetBind {
                                                    form: expr.clone(),
                                                    loop_name: loop_name.clone(),
                                                    eval_env: env.clone(),
                                                    done: Vec::new(),
                                                    current_name: first_name,
                                                    remaining,
                                                    body,
                                                    next: kont,
                                                });
                                                State::Eval(first_expr, env)
                                            }
                                        } else {
                                            // Regular let
                                            let bindings_expr = match &args[0].kind {
                                                ExprKind::List(b) => b,
                                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("let: expected binding list".into()))),
                                            };
                                            let parsed = parse_let_bindings(&expr, bindings_expr)?;
                                            let body = args[1..].to_vec();
                                            if parsed.is_empty() {
                                                let local_env = new_env(Some(env.clone()));
                                                eval_body_state(&body, local_env, &mut kont)
                                            } else {
                                                let (first_name, first_expr) = parsed[0].clone();
                                                let remaining = parsed[1..].to_vec();
                                                kont = Rc::new(Kont::LetBind {
                                                    form: expr.clone(),
                                                    eval_env: env.clone(),
                                                    done: Vec::new(),
                                                    current_name: first_name,
                                                    remaining,
                                                    body,
                                                    next: kont,
                                                });
                                                State::Eval(first_expr, env)
                                            }
                                        }
                                    }
                                    "string-set!" => {
                                        let args = &items[1..];
                                        if args.len() != 3 {
                                            return Err(expr.wrap_err(EvalError::Arity("string-set! requires 3 arguments".into())));
                                        }
                                        let var_name = match &args[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::Type("string-set!: first argument must be a variable".into()))),
                                        };
                                        let idx_expr = args[1].clone();
                                        let char_expr = args[2].clone();
                                        kont = Rc::new(Kont::StringSetIdx {
                                            form: expr.clone(),
                                            var_name,
                                            char_expr,
                                            env: env.clone(),
                                            next: kont,
                                        });
                                        State::Eval(idx_expr, env)
                                    }
                                    "letrec" | "letrec*" => {
                                        let args = &items[1..];
                                        if args.len() < 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("letrec: expected bindings and body".into())));
                                        }
                                        let bindings_expr = match &args[0].kind {
                                            ExprKind::List(b) => b,
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("letrec: expected binding list".into()))),
                                        };
                                        let parsed = parse_let_bindings(&expr, bindings_expr)?;
                                        let body = args[1..].to_vec();
                                        let letrec_env = new_env(Some(env));
                                        for (name, _) in &parsed {
                                            env_set(&letrec_env, name.clone(), Value::Void);
                                        }
                                        if parsed.is_empty() {
                                            eval_body_state(&body, letrec_env, &mut kont)
                                        } else {
                                            let (first_name, first_expr) = parsed[0].clone();
                                            kont = Rc::new(Kont::LetrecBind {
                                                letrec_env: letrec_env.clone(),
                                                remaining: parsed[1..].to_vec(),
                                                body,
                                                next: kont,
                                            });
                                            kont = Rc::new(Kont::Define {
                                                name: first_name,
                                                env: letrec_env.clone(),
                                                next: kont,
                                            });
                                            State::Eval(first_expr, letrec_env)
                                        }
                                    }
                                    "case" => {
                                        let args = &items[1..];
                                        if args.len() < 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("case: expected key and clauses".into())));
                                        }
                                        let key_expr = args[0].clone();
                                        let clauses = args[1..].to_vec();
                                        kont = Rc::new(Kont::CaseKey {
                                            form: expr.clone(),
                                            clauses,
                                            env: env.clone(),
                                            next: kont,
                                        });
                                        State::Eval(key_expr, env)
                                    }
                                    "guard" => {
                                        // (guard (var clause ...) body ...)
                                        let args = &items[1..];
                                        if args.len() < 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("guard: expected clauses and body".into())));
                                        }
                                        let clause_spec = match &args[0].kind {
                                            ExprKind::List(v) if v.len() >= 2 => v,
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("guard: expected (var clause ...)".into()))),
                                        };
                                        let var_name = match &clause_spec[0].kind {
                                            ExprKind::Symbol(s) => s.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("guard: expected variable name".into()))),
                                        };
                                        let clauses = clause_spec[1..].to_vec();
                                        let body = args[1..].to_vec();
                                        let winds = WIND_STACK.with(|ws| ws.borrow().clone());
                                        EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().push(ExceptionHandlerEntry::Guard {
                                            var_name,
                                            clauses,
                                            env: env.clone(),
                                            kont: kont.clone(),
                                            winds,
                                        }));
                                        kont = Rc::new(Kont::GuardCleanup { next: kont });
                                        eval_body_state(&body, env, &mut kont)
                                    }
                                    "do" => {
                                        // (do ((var init step) ...) (test expr ...) body ...)
                                        // Desugar to named let
                                        let args = &items[1..];
                                        if args.len() < 2 {
                                            return Err(expr.wrap_err(EvalError::BadSyntax("do: expected var clauses, test, and body".into())));
                                        }
                                        let var_clauses = match &args[0].kind {
                                            ExprKind::List(v) => v,
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("do: expected variable clause list".into()))),
                                        };
                                        let test_clause = match &args[1].kind {
                                            ExprKind::List(v) if !v.is_empty() => v,
                                            _ => return Err(expr.wrap_err(EvalError::BadSyntax("do: expected (test expr ...) clause".into()))),
                                        };
                                        let do_body = &args[2..];

                                        // Parse var clauses: each is (var init) or (var init step)
                                        let mut var_names = Vec::new();
                                        let mut inits = Vec::new();
                                        let mut steps: Vec<Option<Expr>> = Vec::new();
                                        for vc in var_clauses {
                                            match &vc.kind {
                                                ExprKind::List(parts) if parts.len() == 2 || parts.len() == 3 => {
                                                    let name = match &parts[0].kind {
                                                        ExprKind::Symbol(s) => s.clone(),
                                                        _ => return Err(expr.wrap_err(EvalError::BadSyntax("do: expected variable name".into()))),
                                                    };
                                                    var_names.push(name);
                                                    inits.push(parts[1].clone());
                                                    steps.push(parts.get(2).cloned());
                                                }
                                                _ => return Err(expr.wrap_err(EvalError::BadSyntax("do: bad variable clause".into()))),
                                            }
                                        }

                                        let test_expr = test_clause[0].clone();
                                        let result_exprs = test_clause[1..].to_vec();

                                        let loop_name = gensym("do-loop");

                                        // Build step args for the recursive call
                                        let mut step_args: Vec<Expr> = Vec::new();
                                        for (i, step) in steps.iter().enumerate() {
                                            match step {
                                                Some(s) => step_args.push(s.clone()),
                                                None => step_args.push(Expr::new(ExprKind::Symbol(var_names[i].clone()), expr.line, expr.col)),
                                            }
                                        }

                                        // Build: (loop-name step1 step2 ...)
                                        let mut loop_call_items = vec![Expr::new(ExprKind::Symbol(loop_name.clone()), expr.line, expr.col)];
                                        loop_call_items.extend(step_args);
                                        let loop_call = Expr::new(ExprKind::List(loop_call_items), expr.line, expr.col);

                                        // Build else branch: (begin body... (loop-name steps...))
                                        let else_branch = if do_body.is_empty() {
                                            loop_call
                                        } else {
                                            let mut begin_items = vec![Expr::new(ExprKind::Symbol("begin".into()), expr.line, expr.col)];
                                            begin_items.extend(do_body.iter().cloned());
                                            begin_items.push(loop_call);
                                            Expr::new(ExprKind::List(begin_items), expr.line, expr.col)
                                        };

                                        // Build then branch: (begin result-exprs...) or void
                                        let then_branch = if result_exprs.is_empty() {
                                            Expr::new(ExprKind::List(vec![Expr::new(ExprKind::Symbol("if".into()), expr.line, expr.col), Expr::new(ExprKind::Boolean(false), expr.line, expr.col)]), expr.line, expr.col)
                                        } else if result_exprs.len() == 1 {
                                            result_exprs[0].clone()
                                        } else {
                                            let mut begin_items = vec![Expr::new(ExprKind::Symbol("begin".into()), expr.line, expr.col)];
                                            begin_items.extend(result_exprs);
                                            Expr::new(ExprKind::List(begin_items), expr.line, expr.col)
                                        };

                                        // Build: (if test then else)
                                        let if_expr = Expr::new(ExprKind::List(vec![
                                            Expr::new(ExprKind::Symbol("if".into()), expr.line, expr.col),
                                            test_expr,
                                            then_branch,
                                            else_branch,
                                        ]), expr.line, expr.col);

                                        // Build: (let loop-name ((var1 init1) ...) if-expr)
                                        let mut bindings_list = Vec::new();
                                        for (i, name) in var_names.iter().enumerate() {
                                            bindings_list.push(Expr::new(ExprKind::List(vec![
                                                Expr::new(ExprKind::Symbol(name.clone()), expr.line, expr.col),
                                                inits[i].clone(),
                                            ]), expr.line, expr.col));
                                        }
                                        let let_expr = Expr::new(ExprKind::List(vec![
                                            Expr::new(ExprKind::Symbol("let".into()), expr.line, expr.col),
                                            Expr::new(ExprKind::Symbol(loop_name), expr.line, expr.col),
                                            Expr::new(ExprKind::List(bindings_list), expr.line, expr.col),
                                            if_expr,
                                        ]), expr.line, expr.col);

                                        State::Eval(let_expr, env)
                                    }
                                    _ => unreachable!(),
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            // Not a special form: single lookup for macro or function
                            let resolved = env_get(&env, s).ok();
                            if let Some(Value::Macro { literals, rules, def_env }) = resolved {
                                let (expanded, eval_env) = expand_macro(&items, &expr, &literals, &rules, &def_env, &env)?;
                                State::Eval(expanded, eval_env)
                            } else if let Some(func) = resolved {
                                // Already resolved the operator — skip Operator kont + symbol re-eval
                                if items.len() == 1 {
                                    apply_function(func, vec![], &expr, &mut kont)?
                                } else {
                                    let args = &items[1..];
                                    let last_idx = args.len() - 1;
                                    let last_expr = args[last_idx].clone();
                                    let before = args[..last_idx].to_vec();
                                    kont = Rc::new(Kont::Arg {
                                        form: expr.clone(),
                                        func,
                                        collected: Vec::new(),
                                        before_exprs: before,
                                        env: env.clone(),
                                        next: kont,
                                    });
                                    State::Eval(last_expr, env)
                                }
                            } else {
                                // Symbol not found — defer to normal eval which will produce error
                                if items.len() == 1 {
                                    kont = Rc::new(Kont::Operator {
                                        form: expr.clone(),
                                        args: vec![],
                                        env: env.clone(),
                                        next: kont,
                                    });
                                    State::Eval(items[0].clone(), env)
                                } else {
                                    kont = Rc::new(Kont::Operator {
                                        form: expr.clone(),
                                        args: items[1..].to_vec(),
                                        env: env.clone(),
                                        next: kont,
                                    });
                                    State::Eval(items[0].clone(), env)
                                }
                            }
                        } // close: is_special else
                        } else {
                            // Non-symbol in operator position: function application
                            if items.len() == 1 {
                                kont = Rc::new(Kont::Operator {
                                    form: expr.clone(),
                                    args: vec![],
                                    env: env.clone(),
                                    next: kont,
                                });
                                State::Eval(items[0].clone(), env)
                            } else {
                                kont = Rc::new(Kont::Operator {
                                    form: expr.clone(),
                                    args: items[1..].to_vec(),
                                    env: env.clone(),
                                    next: kont,
                                });
                                State::Eval(items[0].clone(), env)
                            }
                        } } // close: if is_define_record_type else { if is_define_syntax / else if Symbol / else }
                    }
                };
            }
            State::Apply(value) => {
                // Take ownership of current continuation
                let old_kont = std::mem::replace(&mut kont, Rc::new(Kont::Done));
                let k = match Rc::try_unwrap(old_kont) {
                    Ok(k) => k,
                    Err(rc) => (*rc).clone(),
                };

                state = match k {
                    Kont::Done => return Ok(value),

                    Kont::Define { name, env, next } => {
                        env_set(&env, name, value);
                        kont = next;
                        State::Apply(Value::Void)
                    }

                    Kont::Set { form, name, env, next } => {
                        env_update(&env, &name, value).map_err(|e| form.wrap_err(e))?;
                        kont = next;
                        State::Apply(Value::Void)
                    }

                    Kont::If { then_expr, else_expr, env, next } => {
                        kont = next;
                        if value.is_truthy() {
                            State::Eval(then_expr, env)
                        } else if let Some(e) = else_expr {
                            State::Eval(e, env)
                        } else {
                            State::Apply(Value::Void)
                        }
                    }

                    Kont::Seq { remaining, env, next } => {
                        // Discard value, evaluate remaining
                        if remaining.len() == 1 {
                            kont = next;
                            State::Eval(remaining.into_iter().next().unwrap(), env)
                        } else {
                            let first = remaining[0].clone();
                            kont = Rc::new(Kont::Seq {
                                remaining: remaining[1..].to_vec(),
                                env: env.clone(),
                                next,
                            });
                            State::Eval(first, env)
                        }
                    }

                    Kont::And { remaining, env, next } => {
                        if !value.is_truthy() {
                            kont = next;
                            State::Apply(value)
                        } else if remaining.len() == 1 {
                            kont = next;
                            State::Eval(remaining.into_iter().next().unwrap(), env)
                        } else {
                            let first = remaining[0].clone();
                            kont = Rc::new(Kont::And {
                                remaining: remaining[1..].to_vec(),
                                env: env.clone(),
                                next,
                            });
                            State::Eval(first, env)
                        }
                    }

                    Kont::Or { remaining, env, next } => {
                        if value.is_truthy() {
                            kont = next;
                            State::Apply(value)
                        } else if remaining.len() == 1 {
                            kont = next;
                            State::Eval(remaining.into_iter().next().unwrap(), env)
                        } else {
                            let first = remaining[0].clone();
                            kont = Rc::new(Kont::Or {
                                remaining: remaining[1..].to_vec(),
                                env: env.clone(),
                                next,
                            });
                            State::Eval(first, env)
                        }
                    }

                    Kont::CondTest { form, body, remaining_clauses, env, next } => {
                        if value.is_truthy() {
                            if body.is_empty() {
                                kont = next;
                                State::Apply(value)
                            } else {
                                kont = next;
                                eval_body_state(&body, env, &mut kont)
                            }
                        } else {
                            kont = next;
                            start_cond_clauses(&form, &remaining_clauses, &env, &mut kont)?
                        }
                    }

                    Kont::Operator { form, args, env, next } => {
                        let func = value;
                        kont = next;
                        if args.is_empty() {
                            apply_function(func, vec![], &form, &mut kont)?
                        } else {
                            // Evaluate arguments right-to-left so that continuations
                            // captured by call/cc re-evaluate preceding arguments
                            let last_idx = args.len() - 1;
                            let last_expr = args[last_idx].clone();
                            let before = args[..last_idx].to_vec();
                            kont = Rc::new(Kont::Arg {
                                form,
                                func,
                                collected: Vec::new(),
                                before_exprs: before,
                                env: env.clone(),
                                next: kont,
                            });
                            State::Eval(last_expr, env)
                        }
                    }

                    Kont::Arg { form, func, mut collected, mut before_exprs, env, next } => {
                        collected.push(value);
                        if before_exprs.is_empty() {
                            // All args collected (in reverse order). Reverse for correct order.
                            collected.reverse();
                            kont = next;
                            apply_function(func, collected, &form, &mut kont)?
                        } else {
                            // Evaluate the rightmost remaining expression
                            let next_expr = before_exprs.pop().unwrap();
                            kont = Rc::new(Kont::Arg {
                                form,
                                func,
                                collected,
                                before_exprs,
                                env: env.clone(),
                                next,
                            });
                            State::Eval(next_expr, env)
                        }
                    }

                    Kont::LetBind { form, eval_env, mut done, current_name, remaining, body, next } => {
                        done.push((current_name, value));
                        if remaining.is_empty() {
                            let local_env = new_env(Some(eval_env));
                            for (n, v) in done {
                                env_set(&local_env, n, v);
                            }
                            kont = next;
                            eval_body_state(&body, local_env, &mut kont)
                        } else {
                            let (next_name, next_expr) = remaining[0].clone();
                            kont = Rc::new(Kont::LetBind {
                                form,
                                eval_env: eval_env.clone(),
                                done,
                                current_name: next_name,
                                remaining: remaining[1..].to_vec(),
                                body,
                                next,
                            });
                            State::Eval(next_expr, eval_env)
                        }
                    }

                    Kont::NamedLetBind { form, loop_name, eval_env, mut done, current_name, remaining, body, next } => {
                        done.push((current_name, value));
                        if remaining.is_empty() {
                            let local_env = new_env(Some(eval_env));
                            let param_names: Vec<String> = done.iter().map(|(n, _)| n.clone()).collect();
                            let lambda = Value::Lambda {
                                params: param_names,
                                rest_param: None,
                                body: body.clone(),
                                env: local_env.clone(),
                            };
                            env_set(&local_env, loop_name, lambda);
                            for (n, v) in done {
                                env_set(&local_env, n, v);
                            }
                            kont = next;
                            eval_body_state(&body, local_env, &mut kont)
                        } else {
                            let (next_name, next_expr) = remaining[0].clone();
                            kont = Rc::new(Kont::NamedLetBind {
                                form,
                                loop_name,
                                eval_env: eval_env.clone(),
                                done,
                                current_name: next_name,
                                remaining: remaining[1..].to_vec(),
                                body,
                                next,
                            });
                            State::Eval(next_expr, eval_env)
                        }
                    }

                    Kont::StringSetIdx { form, var_name, char_expr, env, next } => {
                        let idx = value.as_integer().map_err(|e| form.wrap_err(e))? as usize;
                        kont = Rc::new(Kont::StringSetChar {
                            form,
                            var_name,
                            idx,
                            env: env.clone(),
                            next,
                        });
                        State::Eval(char_expr, env)
                    }

                    Kont::StringSetChar { form, var_name, idx, env, next } => {
                        let ch = match value {
                            Value::Char(c) => c,
                            _ => return Err(form.wrap_err(EvalError::Type("string-set!: third argument must be a character".into()))),
                        };
                        let s = env_get(&env, &var_name).map_err(|e| form.wrap_err(e))?;
                        match s {
                            Value::Str(string) => {
                                let mut chars: Vec<char> = string.chars().collect();
                                if idx >= chars.len() {
                                    return Err(form.wrap_err(EvalError::Type("string-set!: index out of bounds".into())));
                                }
                                chars[idx] = ch;
                                let new_string: String = chars.into_iter().collect();
                                env_update(&env, &var_name, Value::Str(new_string)).map_err(|e| form.wrap_err(e))?;
                                kont = next;
                                State::Apply(Value::Void)
                            }
                            _ => return Err(form.wrap_err(EvalError::Type("string-set!: expected string".into()))),
                        }
                    }

                    Kont::LetrecBind { letrec_env, remaining, body, next } => {
                        // value was just set via Define kont already
                        if remaining.is_empty() {
                            kont = next;
                            eval_body_state(&body, letrec_env, &mut kont)
                        } else {
                            let (next_name, next_expr) = remaining[0].clone();
                            kont = Rc::new(Kont::LetrecBind {
                                letrec_env: letrec_env.clone(),
                                remaining: remaining[1..].to_vec(),
                                body,
                                next,
                            });
                            kont = Rc::new(Kont::Define {
                                name: next_name,
                                env: letrec_env.clone(),
                                next: kont,
                            });
                            State::Eval(next_expr, letrec_env)
                        }
                    }

                    Kont::CaseKey { form, clauses, env, next } => {
                        // value is the evaluated key
                        kont = next;
                        let mut result_state = None;
                        for clause in &clauses {
                            match &clause.kind {
                                ExprKind::List(citems) if !citems.is_empty() => {
                                    let is_else = matches!(&citems[0].kind, ExprKind::Symbol(s) if s == "else");
                                    if is_else {
                                        result_state = Some(eval_body_state(&citems[1..], env.clone(), &mut kont));
                                        break;
                                    }
                                    // citems[0] should be a list of datums
                                    match &citems[0].kind {
                                        ExprKind::List(datums) => {
                                            let mut matched = false;
                                            for datum in datums {
                                                let datum_val = expr_to_value(datum)?;
                                                if eqv_compare(&value, &datum_val) {
                                                    matched = true;
                                                    break;
                                                }
                                            }
                                            if matched {
                                                result_state = Some(eval_body_state(&citems[1..], env.clone(), &mut kont));
                                                break;
                                            }
                                        }
                                        _ => return Err(form.wrap_err(EvalError::BadSyntax("case: expected datum list".into()))),
                                    }
                                }
                                _ => return Err(form.wrap_err(EvalError::BadSyntax("case: bad clause".into()))),
                            }
                        }
                        result_state.unwrap_or(State::Apply(Value::Void))
                    }

                    Kont::DynamicWindRunBody { body_thunk, out_thunk, wind_entry, next } => {
                        // in-thunk finished; push wind entry and call body
                        WIND_STACK.with(|ws| ws.borrow_mut().push(wind_entry));
                        kont = Rc::new(Kont::DynamicWindRunOut { out_thunk, next });
                        let form = dummy_form();
                        apply_function(body_thunk, vec![], &form, &mut kont)?
                    }

                    Kont::DynamicWindRunOut { out_thunk, next } => {
                        // body finished; pop wind entry, save body value, call out-thunk
                        WIND_STACK.with(|ws| ws.borrow_mut().pop());
                        let body_value = value;
                        kont = Rc::new(Kont::DynamicWindFinish { body_value, next });
                        let form = dummy_form();
                        apply_function(out_thunk, vec![], &form, &mut kont)?
                    }

                    Kont::DynamicWindFinish { body_value, next } => {
                        // out-thunk finished; return body value
                        kont = next;
                        State::Apply(body_value)
                    }

                    Kont::WindChain { remaining, cont_value, target_kont } => {
                        // previous thunk finished (value ignored); continue chain
                        let form = dummy_form();
                        start_wind_chain(remaining, cont_value, target_kont, &mut kont, &form)?
                    }

                    Kont::WindChainPush { wind_entry, remaining, cont_value, target_kont } => {
                        // in-thunk finished; push wind entry, continue chain
                        WIND_STACK.with(|ws| ws.borrow_mut().push(wind_entry));
                        let form = dummy_form();
                        start_wind_chain(remaining, cont_value, target_kont, &mut kont, &form)?
                    }

                    Kont::GuardCleanup { next } => {
                        EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().pop());
                        kont = next;
                        State::Apply(value)
                    }

                    Kont::ExceptionHandlerCleanup { next } => {
                        EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().pop());
                        kont = next;
                        State::Apply(value)
                    }

                    Kont::CallWithValuesConsumer { consumer, form, next } => {
                        kont = next;
                        let call_args = match value {
                            Value::MultipleValues(vs) => vs,
                            single => vec![single],
                        };
                        apply_function(consumer, call_args, &form, &mut kont)?
                    }

                    Kont::GuardClauseTest { exception, body, remaining_clauses, env, next } => {
                        if value.is_truthy() {
                            if body.is_empty() {
                                kont = next;
                                State::Apply(value)
                            } else {
                                kont = next;
                                eval_body_state(&body, env, &mut kont)
                            }
                        } else if remaining_clauses.is_empty() {
                            // No clause matched, re-raise
                            State::RaiseException(exception)
                        } else {
                            start_guard_clauses(exception, &remaining_clauses, &env, &mut kont, next)?
                        }
                    }

                    Kont::DispatchGuardClauses { exception, clauses, env, next } => {
                        // Wind unwinding complete, now evaluate guard clauses
                        kont = next.clone();
                        start_guard_clauses(exception, &clauses, &env, &mut kont, next)?
                    }

                    Kont::CallHandlerAfterWind { handler, exception, next } => {
                        // Wind unwinding complete, call handler with exception
                        kont = next;
                        let form = dummy_form();
                        apply_function(handler, vec![exception], &form, &mut kont)?
                    }
                };
            }
            State::RaiseException(exception) => {
                let handler = EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().pop());
                match handler {
                    Some(ExceptionHandlerEntry::Guard { var_name, clauses, env, kont: saved_kont, winds: saved_winds }) => {
                        let current_winds = WIND_STACK.with(|ws| ws.borrow().clone());
                        let common_len = current_winds.iter().zip(saved_winds.iter())
                            .take_while(|(a, b)| Rc::ptr_eq(a, b))
                            .count();
                        let mut ops = Vec::new();
                        for i in (common_len..current_winds.len()).rev() {
                            ops.push(WindOp::Unwind(current_winds[i].1.clone()));
                        }
                        for i in common_len..saved_winds.len() {
                            ops.push(WindOp::Rewind(saved_winds[i].0.clone(), saved_winds[i].clone()));
                        }
                        let guard_env = new_env(Some(env));
                        env_set(&guard_env, var_name, exception.clone());
                        if ops.is_empty() {
                            kont = saved_kont.clone();
                            state = start_guard_clauses(exception, &clauses, &guard_env, &mut kont, saved_kont)?;
                        } else {
                            let target = Rc::new(Kont::DispatchGuardClauses {
                                exception,
                                clauses,
                                env: guard_env,
                                next: saved_kont,
                            });
                            let form = dummy_form();
                            state = start_wind_chain(ops, Value::Void, target, &mut kont, &form)?;
                        }
                    }
                    Some(ExceptionHandlerEntry::WithHandler { handler, kont: saved_kont, winds: saved_winds }) => {
                        let current_winds = WIND_STACK.with(|ws| ws.borrow().clone());
                        let common_len = current_winds.iter().zip(saved_winds.iter())
                            .take_while(|(a, b)| Rc::ptr_eq(a, b))
                            .count();
                        let mut ops = Vec::new();
                        for i in (common_len..current_winds.len()).rev() {
                            ops.push(WindOp::Unwind(current_winds[i].1.clone()));
                        }
                        for i in common_len..saved_winds.len() {
                            ops.push(WindOp::Rewind(saved_winds[i].0.clone(), saved_winds[i].clone()));
                        }
                        if ops.is_empty() {
                            kont = saved_kont;
                            let form = dummy_form();
                            state = apply_function(handler, vec![exception], &form, &mut kont)?;
                        } else {
                            let target = Rc::new(Kont::CallHandlerAfterWind {
                                handler,
                                exception,
                                next: saved_kont,
                            });
                            let form = dummy_form();
                            state = start_wind_chain(ops, Value::Void, target, &mut kont, &form)?;
                        }
                    }
                    None => {
                        return Err(EvalError::Type(format!("unhandled exception: {}", exception.display())));
                    }
                }
            }
        }
    }
}

// --- Public API ---

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
    EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().clear());
    let exprs = parse(input)?;
    if exprs.is_empty() {
        return Ok(Value::Boolean(false).display());
    }
    let env = default_env();
    let done = Rc::new(Kont::Done);
    // Wrap all exprs as a sequence: evaluate each, return last
    if exprs.len() == 1 {
        let result = eval_cek(exprs.into_iter().next().unwrap(), env, done)?;
        Ok(result.display())
    } else {
        let mut kont = done;
        let mut iter = exprs.into_iter();
        let first = iter.next().unwrap();
        let remaining: Vec<Expr> = iter.collect();
        kont = Rc::new(Kont::Seq {
            remaining,
            env: env.clone(),
            next: kont,
        });
        let result = eval_cek(first, env, kont)?;
        Ok(result.display())
    }
}

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    OUTPUT_BUFFER.with(|buf| buf.borrow_mut().clear());
    WIND_STACK.with(|ws| ws.borrow_mut().clear());
    EXCEPTION_HANDLERS.with(|eh| eh.borrow_mut().clear());
    let exprs = parse(input)?;
    if exprs.is_empty() {
        let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
        return Ok((Value::Boolean(false).display(), output));
    }
    let env = default_env();
    let done = Rc::new(Kont::Done);
    let result = if exprs.len() == 1 {
        eval_cek(exprs.into_iter().next().unwrap(), env, done)?
    } else {
        let mut kont = done;
        let mut iter = exprs.into_iter();
        let first = iter.next().unwrap();
        let remaining: Vec<Expr> = iter.collect();
        kont = Rc::new(Kont::Seq {
            remaining,
            env: env.clone(),
            next: kont,
        });
        eval_cek(first, env, kont)?
    };
    let output = OUTPUT_BUFFER.with(|buf| buf.borrow().clone());
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
