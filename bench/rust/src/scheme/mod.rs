pub mod error;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}__g{}", base, n)
}

thread_local! {
    static OUTPUT_BUFFER: RefCell<String> = RefCell::new(String::new());
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    Pair(Box<Value>, Box<Value>),
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
    Continuation(Rc<Kont>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Str(s) => write!(f, "Str(\"{}\")", s),
            Value::Char(c) => write!(f, "Char({:?})", c),
            Value::Symbol(s) => write!(f, "Symbol({})", s),
            Value::Pair(a, b) => write!(f, "Pair({:?}, {:?})", a, b),
            Value::Nil => write!(f, "Nil"),
            Value::Lambda { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, "Void"),
            Value::Builtin(name, _) => write!(f, "Builtin({})", name),
            Value::CallCC => write!(f, "CallCC"),
            Value::Continuation(_) => write!(f, "#<continuation>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Void, Value::Void) => true,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            _ => false,
        }
    }
}

impl Value {
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
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
            Value::Lambda { .. } | Value::Builtin(_, _) | Value::CallCC | Value::Continuation(_) => {
                "#<procedure>".to_string()
            }
            Value::Void => "#<void>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
        }
    }

    fn display_for_display(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
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
}

impl std::fmt::Debug for Kont {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<continuation>")
    }
}

// --- Builtins ---

fn builtin_add(args: &[Value]) -> Result<Value, EvalError> {
    let mut sum: i64 = 0;
    for a in args {
        sum += a.as_integer()?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("- requires at least 1 argument".into()));
    }
    let first = args[0].as_integer()?;
    if args.len() == 1 {
        return Ok(Value::Integer(-first));
    }
    let mut result = first;
    for a in &args[1..] {
        result -= a.as_integer()?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value]) -> Result<Value, EvalError> {
    let mut product: i64 = 1;
    for a in args {
        product *= a.as_integer()?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("/ requires at least 1 argument".into()));
    }
    let first = args[0].as_integer()?;
    if args.len() == 1 {
        if first == 0 {
            return Err(EvalError::DivisionByZero);
        }
        return Ok(Value::Integer(1 / first));
    }
    let mut result = first;
    for a in &args[1..] {
        let v = a.as_integer()?;
        if v == 0 {
            return Err(EvalError::DivisionByZero);
        }
        result /= v;
    }
    Ok(Value::Integer(result))
}

fn compare_values(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(
            "comparison requires at least 2 arguments".into(),
        ));
    }
    let vals: Result<Vec<i64>, _> = args.iter().map(|a| a.as_integer()).collect();
    let vals = vals?;
    for w in vals.windows(2) {
        if !cmp(w[0], w[1]) {
            return Ok(Value::Boolean(false));
        }
    }
    Ok(Value::Boolean(true))
}

fn builtin_lt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a < b)
}
fn builtin_gt(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a > b)
}
fn builtin_eq(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a == b)
}
fn builtin_le(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a <= b)
}
fn builtin_ge(args: &[Value]) -> Result<Value, EvalError> {
    compare_values(args, |a, b| a >= b)
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
    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
}

fn builtin_car(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("car requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(car, _) => Ok(*car.clone()),
        _ => Err(EvalError::Type("car: not a pair".into())),
    }
}

fn builtin_cdr(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("cdr requires exactly 1 argument".into()));
    }
    match &args[0] {
        Value::Pair(_, cdr) => Ok(*cdr.clone()),
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
        result = Value::Pair(Box::new(a.clone()), Box::new(result));
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
                    elems.push(*car.clone());
                    cur = cdr;
                }
                _ => return Err(EvalError::Type("append: not a proper list".into())),
            }
        }
        for e in elems.into_iter().rev() {
            result = Value::Pair(Box::new(e), Box::new(result));
        }
    }
    Ok(result)
}

fn builtin_number_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number? requires exactly 1 argument".into()));
    }
    Ok(Value::Boolean(matches!(args[0], Value::Integer(_))))
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
        Value::Str(s) => match s.parse::<i64>() {
            Ok(n) => Ok(Value::Integer(n)),
            Err(_) => Ok(Value::Boolean(false)),
        },
        _ => Err(EvalError::Type("string->number: expected string".into())),
    }
}

fn builtin_number_to_string(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity("number->string requires exactly 1 argument".into()));
    }
    let n = args[0].as_integer()?;
    Ok(Value::Str(n.to_string()))
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
    Ok(Value::Boolean(args[0].as_integer()? == 0))
}

fn builtin_positive_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("positive? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? > 0))
}

fn builtin_negative_pred(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 { return Err(EvalError::Arity("negative? requires exactly 1 argument".into())); }
    Ok(Value::Boolean(args[0].as_integer()? < 0))
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
        Value::Pair(car, _) => Ok(*car.clone()),
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
                        return Ok(*car.clone());
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
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    };
    Ok(Value::Boolean(result))
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => values_equal(a1, b1) && values_equal(a2, b2),
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
    ];
    for &(name, func) in builtins {
        env_set(&env, name.to_string(), Value::Builtin(name, func));
    }
    env_set(&env, "call/cc".to_string(), Value::CallCC);
    env_set(&env, "call-with-current-continuation".to_string(), Value::CallCC);
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
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(s) => Ok(Value::Symbol(s.clone())),
        ExprKind::List(items) => {
            let mut result = Value::Nil;
            for item in items.iter().rev() {
                let v = expr_to_value(item)?;
                result = Value::Pair(Box::new(v), Box::new(result));
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
             "syntax-rules" | "else")
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
                            elems.push(*car.clone());
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
                result = Value::Pair(Box::new(v), Box::new(result));
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
                        final_args.push(*car.clone());
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
            let cont_value = Value::Continuation(kont.clone());
            let proc = args.into_iter().next().unwrap();
            apply_function(proc, vec![cont_value], form, kont)
        }
        Value::Continuation(saved_kont) => {
            if args.len() != 1 {
                return Err(form.wrap_err(EvalError::Arity("continuation requires exactly 1 argument".into())));
            }
            *kont = saved_kont;
            Ok(State::Apply(args.into_iter().next().unwrap()))
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

                        // Check for define-syntax
                        let is_define_syntax = matches!(&items[0].kind, ExprKind::Symbol(ref s) if s == "define-syntax");

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
                                    "if" | "begin" | "cond" | "and" | "or" | "let" | "string-set!");

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
                                            return Err(expr.wrap_err(EvalError::Arity("string-set! requires exactly 3 arguments".into())));
                                        }
                                        let var_name = match &args[0].kind {
                                            ExprKind::Symbol(name) => name.clone(),
                                            _ => return Err(expr.wrap_err(EvalError::Type("string-set!: first argument must be a variable".into()))),
                                        };
                                        kont = Rc::new(Kont::StringSetIdx {
                                            form: expr.clone(),
                                            var_name,
                                            char_expr: args[2].clone(),
                                            env: env.clone(),
                                            next: kont,
                                        });
                                        State::Eval(args[1].clone(), env)
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
                        } // close: if is_define_syntax / else if Symbol / else
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
                };
            }
        }
    }
}

// --- Public API ---

pub fn eval_str(input: &str) -> Result<String, EvalError> {
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
