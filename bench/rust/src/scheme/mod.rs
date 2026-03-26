pub mod error;
mod builtins;
mod parser;

pub use error::EvalError;

use builtins::apply_builtin_by_name;
use parser::{Expr, ExprKind, Pos, parse_all};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__{base}_{n}")
}

#[derive(Debug, Clone)]
struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

type EnvRef = Rc<RefCell<EnvFrame>>;

#[derive(Debug, Clone)]
struct CaseLambdaClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type ApplyFn = fn(&Value, &[Value], Pos, &RefCell<String>) -> Result<Value, EvalError>;

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: EnvRef,
    },
    Void,
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: EnvRef,
    },
    Record {
        type_id: usize,
        fields: Vec<Value>,
    },
    RecordConstructor {
        type_id: usize,
        num_fields: usize,
    },
    RecordPredicate {
        type_id: usize,
    },
    RecordAccessor {
        type_id: usize,
        field_index: usize,
    },
    CaseLambda {
        clauses: Vec<CaseLambdaClause>,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Rational(a1, a2), Value::Rational(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Pair(a1, a2), Value::Pair(b1, b2)) => a1 == b1 && a2 == b2,
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Record { type_id: a, fields: af }, Value::Record { type_id: b, fields: bf }) => a == b && af == bf,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            _ => false,
        }
    }
}

impl Value {
    /// `write` representation: strings are quoted
    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{f}");
                if f.is_finite() && !s.contains('.') { format!("{f}.0") } else { s }
            }
            Value::Rational(n, d) => format!("{n}/{d}"),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Char(c) => match c {
                ' ' => "#\\space".to_string(),
                '\n' => "#\\newline".to_string(),
                '\t' => "#\\tab".to_string(),
                _ => format!("#\\{c}"),
            },
            Value::Str(s) => format!("\"{s}\""),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(car, cdr) => {
                let mut parts = vec![car.display_scheme()];
                let mut current = cdr.as_ref();
                loop {
                    match current {
                        Value::Pair(a, b) => {
                            parts.push(a.display_scheme());
                            current = b.as_ref();
                        }
                        Value::List(items) if items.is_empty() => break,
                        Value::List(items) => {
                            for item in items {
                                parts.push(item.display_scheme());
                            }
                            break;
                        }
                        other => {
                            return format!("({} . {})", parts.join(" "), other.display_scheme());
                        }
                    }
                }
                format!("({})", parts.join(" "))
            }
            Value::Lambda { .. } | Value::CaseLambda { .. } => "#<procedure>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Record { .. } => "#<record>".to_string(),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => "#<procedure>".to_string(),
            Value::Vector(v) => {
                let items: Vec<String> = v.borrow().iter().map(|v| v.display_scheme()).collect();
                format!("#({})", items.join(" "))
            }
            Value::Void => "".to_string(),
        }
    }

    /// `display` representation: strings are unquoted
    fn display_output(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::Pair(..) => {
                // For display, use display_output for elements
                let mut parts = Vec::new();
                let mut current: &Value = self;
                loop {
                    match current {
                        Value::Pair(car, cdr) => {
                            parts.push(car.display_output());
                            current = cdr.as_ref();
                        }
                        Value::List(items) if items.is_empty() => break,
                        Value::List(items) => {
                            for item in items {
                                parts.push(item.display_output());
                            }
                            break;
                        }
                        other => {
                            return format!("({} . {})", parts.join(" "), other.display_output());
                        }
                    }
                }
                format!("({})", parts.join(" "))
            }
            other => other.display_scheme(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Evaluator ---

fn new_env(parent: Option<EnvRef>) -> EnvRef {
    Rc::new(RefCell::new(EnvFrame {
        bindings: HashMap::new(),
        parent,
    }))
}

fn env_get(env: &EnvRef, name: &str) -> Option<Value> {
    let frame = env.borrow();
    if let Some(val) = frame.bindings.get(name) {
        Some(val.clone())
    } else if let Some(parent) = &frame.parent {
        env_get(parent, name)
    } else {
        None
    }
}

fn env_set(env: &EnvRef, name: String, val: Value) {
    env.borrow_mut().bindings.insert(name, val);
}

fn eval_expr(expr: &Expr, env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    let p = expr.pos;
    match &expr.kind {
        ExprKind::Integer(n) => Ok(Value::Integer(*n)),
        ExprKind::Float(f) => Ok(Value::Float(*f)),
        ExprKind::Rational(n, d) => Ok(builtins::make_rational(*n, *d)),
        ExprKind::Boolean(b) => Ok(Value::Boolean(*b)),
        ExprKind::Str(s) => Ok(Value::Str(s.clone())),
        ExprKind::Char(c) => Ok(Value::Char(*c)),
        ExprKind::Symbol(name) => {
            env_get(env, name)
                .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        ExprKind::List(items) => {
            if items.is_empty() {
                return Ok(Value::List(vec![]));
            }
            if let ExprKind::Symbol(op) = &items[0].kind {
                match op.as_str() {
                    "and" => return eval_and(&items[1..], env, out),
                    "or" => return eval_or(&items[1..], env, out),
                    "define" => return eval_define(&items[1..], env, p, out),
                    "if" => return eval_if(&items[1..], env, p, out),
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote requires 1 argument")));
                        }
                        return Ok(expr_to_value(&items[1]));
                    }
                    "lambda" => return eval_lambda(&items[1..], env, p),
                    "let" => return eval_let(&items[1..], env, p, out),
                    "begin" => return eval_begin(&items[1..], env, out),
                    "cond" => return eval_cond(&items[1..], env, out),
                    "set!" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity(format!("{p}: set! requires 2 arguments")));
                        }
                        let name = match &items[1].kind {
                            ExprKind::Symbol(s) => s.clone(),
                            _ => return Err(EvalError::Type(format!("{p}: set!: expected symbol"))),
                        };
                        let val = eval_expr(&items[2], env, out)?;
                        if !env_set_existing(env, &name, val) {
                            return Err(EvalError::UnboundVariable(format!("{p}: {name}")));
                        }
                        return Ok(Value::Void);
                    }
                    "string-set!" => return eval_string_set(&items[1..], env, p, out),
                    "define-syntax" => return eval_define_syntax(&items[1..], env, p),
                    "define-record-type" => return eval_define_record_type(&items[1..], env, p),
                    "case-lambda" => return eval_case_lambda(&items[1..], env, p),
                    "letrec" => return eval_letrec(&items[1..], env, p, out),
                    "letrec*" => return eval_letrec_star(&items[1..], env, p, out),
                    "let*" => return eval_let_star(&items[1..], env, p, out),
                    "case" => return eval_case(&items[1..], env, p, out),
                    "do" => return eval_do(&items[1..], env, p, out),
                    _ => {
                        // Check if op is a macro
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            return eval_macro_call(items, &literals, &rules, &def_env, env, p, out);
                        }
                    }
                }
            } else {
                // Head is not a symbol - check if it evaluates to a macro
                let head_val = eval_expr(&items[0], env, out)?;
                if let Value::Macro { .. } = &head_val {
                    // Macros called via non-symbol head are unusual; skip for now
                }
                let args: Result<Vec<Value>, _> = items[1..].iter().map(|e| eval_expr(e, env, out)).collect();
                let args = args?;
                return apply_func(&head_val, &args, p, out);
            }
            let func = eval_expr(&items[0], env, out)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|e| eval_expr(e, env, out)).collect();
            let args = args?;
            apply_func(&func, &args, p, out)
        }
    }
}

fn eval_and(exprs: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env, out)?;
        if !val.is_truthy() || i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn eval_or(exprs: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for (i, expr) in exprs.iter().enumerate() {
        let val = eval_expr(expr, env, out)?;
        if val.is_truthy() || i == exprs.len() - 1 {
            return Ok(val);
        }
    }
    unreachable!()
}

fn eval_define(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{p}: define requires at least 2 arguments")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{p}: define requires 2 arguments")));
            }
            let val = eval_expr(&args[1], env, out)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        ExprKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{p}: define: empty signature")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type(format!("{p}: define: expected symbol as function name"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], p)?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type(format!("{p}: define: expected symbol or list"))),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("{p}: if requires 2 or 3 arguments")));
    }
    let cond = eval_expr(&args[0], env, out)?;
    if cond.is_truthy() {
        eval_expr(&args[1], env, out)
    } else if args.len() == 3 {
        eval_expr(&args[2], env, out)
    } else {
        Ok(Value::Void)
    }
}

fn parse_params(sig: &[Expr], p: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < sig.len() {
        match &sig[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 != sig.len() - 1 {
                    return Err(EvalError::Parse(format!("{p}: malformed dot in parameter list")));
                }
                match &sig[i + 1].kind {
                    ExprKind::Symbol(r) => rest_param = Some(r.clone()),
                    _ => return Err(EvalError::Type(format!("{p}: expected symbol after dot"))),
                }
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type(format!("{}: expected symbol in params", sig[i].pos))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: lambda requires params and body")));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(items) => parse_params(items, p)?,
        ExprKind::Symbol(s) => {
            // (lambda rest body) — single rest param captures all args
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Type(format!("{}: lambda: expected parameter list", args[0].pos))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda { params, rest_param, body, env: env.clone() })
}

fn eval_case_lambda(clauses: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    let mut parsed_clauses = Vec::new();
    for clause in clauses {
        let items = match &clause.kind {
            ExprKind::List(items) if items.len() >= 2 => items,
            _ => return Err(EvalError::Type(format!("{p}: case-lambda: invalid clause"))),
        };
        let (params, rest_param) = match &items[0].kind {
            ExprKind::List(sig) => parse_params(sig, p)?,
            ExprKind::Symbol(s) => (vec![], Some(s.clone())),
            _ => return Err(EvalError::Type(format!("{p}: case-lambda: expected parameter list"))),
        };
        let body = items[1..].to_vec();
        parsed_clauses.push(CaseLambdaClause { params, rest_param, body, env: env.clone() });
    }
    Ok(Value::CaseLambda { clauses: parsed_clauses })
}

fn eval_letrec(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: letrec requires bindings and body")));
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: letrec: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    // First, bind all variables to Void
    let mut names = Vec::new();
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    names.push(s.clone());
                    env_set(&local_env, s.clone(), Value::Void);
                } else {
                    return Err(EvalError::Type(format!("{}: letrec: expected symbol", pair[0].pos)));
                }
            }
            _ => return Err(EvalError::Type(format!("{}: letrec: expected (var init)", b.pos))),
        }
    }
    // Then evaluate inits in the local env and update bindings
    for (i, b) in bindings_expr.iter().enumerate() {
        if let ExprKind::List(pair) = &b.kind {
            let val = eval_expr(&pair[1], &local_env, out)?;
            env_set(&local_env, names[i].clone(), val);
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: letrec* requires bindings and body")));
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: letrec*: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval_expr(&pair[1], &local_env, out)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(format!("{}: letrec*: expected symbol", pair[0].pos)));
                }
            }
            _ => return Err(EvalError::Type(format!("{}: letrec*: expected (var init)", b.pos))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_let_star(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: let* requires bindings and body")));
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: let*: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval_expr(&pair[1], &local_env, out)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(format!("{}: let*: expected symbol", pair[0].pos)));
                }
            }
            _ => return Err(EvalError::Type(format!("{}: let*: expected (var init)", b.pos))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eqv_match(key: &Value, datum: &Value) -> bool {
    match (key, datum) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(a1, a2), Value::Rational(b1, b2)) => a1 == b1 && a2 == b2,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

fn eval_case(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{p}: case requires key and clauses")));
    }
    let key = eval_expr(&args[0], env, out)?;
    for clause in &args[1..] {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval_expr(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
                // Normal clause: ((datum ...) expr ...)
                let datums = match &items[0].kind {
                    ExprKind::List(ds) => ds,
                    _ => return Err(EvalError::Type(format!("{}: case: expected datum list", items[0].pos))),
                };
                for datum in datums {
                    let datum_val = expr_to_value(datum);
                    if eqv_match(&key, &datum_val) {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval_expr(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
            }
            _ => return Err(EvalError::Type(format!("{}: case: expected clause", clause.pos))),
        }
    }
    Ok(Value::Void)
}

fn eval_do(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: do requires variable bindings and test")));
    }
    let var_specs = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: do: expected variable list"))),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type(format!("{p}: do: expected test clause"))),
    };
    let body = &args[2..];

    // Parse variable specs: (var init step?)
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Expr>> = Vec::new();
    let local_env = new_env(Some(env.clone()));
    for spec in var_specs {
        match &spec.kind {
            ExprKind::List(items) if items.len() >= 2 => {
                let name = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(format!("{}: do: expected variable name", items[0].pos))),
                };
                let init = eval_expr(&items[1], env, out)?;
                env_set(&local_env, name.clone(), init);
                var_names.push(name);
                if items.len() >= 3 {
                    step_exprs.push(Some(items[2].clone()));
                } else {
                    step_exprs.push(None);
                }
            }
            _ => return Err(EvalError::Type(format!("{}: do: expected (var init step)", spec.pos))),
        }
    }

    loop {
        // Evaluate test
        let test_val = eval_expr(&test_clause[0], &local_env, out)?;
        if test_val.is_truthy() {
            // Test is true: evaluate result expressions
            let mut result = Value::Void;
            for expr in &test_clause[1..] {
                result = eval_expr(expr, &local_env, out)?;
            }
            return Ok(result);
        }
        // Execute body
        for expr in body {
            eval_expr(expr, &local_env, out)?;
        }
        // Parallel step: evaluate all step expressions using current values
        let new_vals: Vec<Option<Value>> = step_exprs.iter().map(|step| {
            match step {
                Some(expr) => Ok(Some(eval_expr(expr, &local_env, out)?)),
                None => Ok(None),
            }
        }).collect::<Result<Vec<_>, EvalError>>()?;
        // Update all variables simultaneously
        for (i, val) in new_vals.into_iter().enumerate() {
            if let Some(v) = val {
                env_set(&local_env, var_names[i].clone(), v);
            }
        }
    }
}

fn eval_let(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{p}: let requires bindings and body")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!("{p}: named let requires bindings and body")));
        }
        let bindings_expr = match &args[1].kind {
            ExprKind::List(items) => items,
            _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval_expr(&pair[1], env, out)?);
                    } else {
                        return Err(EvalError::Type(format!("{}: let: expected symbol in binding", pair[0].pos)));
                    }
                }
                _ => return Err(EvalError::Type(format!("{}: let: expected (var init) binding", b.pos))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda { params: params.clone(), rest_param: None, body, env: local_env.clone() };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(inits.iter()) {
            env_set(&local_env, param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval_expr(expr, &local_env, out)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                if let ExprKind::Symbol(s) = &pair[0].kind {
                    let val = eval_expr(&pair[1], env, out)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Type(format!("{}: let: expected symbol in binding", pair[0].pos)));
                }
            }
            _ => return Err(EvalError::Type(format!("{}: let: expected (var init) binding", b.pos))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval_expr(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval_expr(expr, env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(items) if !items.is_empty() => {
                if let ExprKind::Symbol(s) = &items[0].kind {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval_expr(expr, env, out)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval_expr(&items[0], env, out)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &items[1..] {
                        result = eval_expr(expr, env, out)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Type(format!("{}: cond: expected clause list", clause.pos))),
        }
    }
    Ok(Value::Void)
}

fn expr_to_value(expr: &Expr) -> Value {
    match &expr.kind {
        ExprKind::Integer(n) => Value::Integer(*n),
        ExprKind::Float(f) => Value::Float(*f),
        ExprKind::Rational(n, d) => builtins::make_rational(*n, *d),
        ExprKind::Boolean(b) => Value::Boolean(*b),
        ExprKind::Str(s) => Value::Str(s.clone()),
        ExprKind::Char(c) => Value::Char(*c),
        ExprKind::Symbol(s) => Value::Symbol(s.clone()),
        ExprKind::List(items) => Value::List(items.iter().map(expr_to_value).collect()),
    }
}

fn apply_func(func: &Value, args: &[Value], call_pos: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected at least {} args, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: expected {} args, got {}", params.len(), args.len()
                )));
            }
            let local_env = new_env(Some(env.clone()));
            for (param, arg) in params.iter().zip(args.iter()) {
                env_set(&local_env, param.clone(), arg.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = if args.len() > params.len() {
                    args[params.len()..].to_vec()
                } else {
                    vec![]
                };
                env_set(&local_env, rest.clone(), Value::List(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval_expr(expr, &local_env, out)?;
            }
            Ok(result)
        }
        Value::CaseLambda { clauses } => {
            for clause in clauses {
                let CaseLambdaClause { params, rest_param, body, env } = clause;
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = new_env(Some(env.clone()));
                    for (param, arg) in params.iter().zip(args.iter()) {
                        env_set(&local_env, param.clone(), arg.clone());
                    }
                    if let Some(rest) = rest_param {
                        let rest_args = if args.len() > params.len() {
                            args[params.len()..].to_vec()
                        } else {
                            vec![]
                        };
                        env_set(&local_env, rest.clone(), Value::List(rest_args));
                    }
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval_expr(expr, &local_env, out)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "{call_pos}: case-lambda: no matching clause for {} args", args.len()
            )))
        }
        Value::RecordConstructor { type_id, num_fields } => {
            if args.len() != *num_fields {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: record constructor expected {} args, got {}", num_fields, args.len()
                )));
            }
            Ok(Value::Record { type_id: *type_id, fields: args.to_vec() })
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: record predicate expects 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
        }
        Value::RecordAccessor { type_id, field_index } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: record accessor expects 1 argument")));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields } if tid == type_id => {
                    Ok(fields[*field_index].clone())
                }
                _ => Err(EvalError::Type(format!("{call_pos}: record accessor: wrong type"))),
            }
        }
        Value::Symbol(s) => apply_builtin_by_name(s, args, call_pos, out, apply_func),
        _ => Err(EvalError::Type(format!("{call_pos}: not a procedure: {}", func.display_scheme()))),
    }
}


fn env_set_existing(env: &EnvRef, name: &str, val: Value) -> bool {
    let mut frame = env.borrow_mut();
    if frame.bindings.contains_key(name) {
        frame.bindings.insert(name.to_string(), val);
        return true;
    }
    if let Some(parent) = &frame.parent {
        env_set_existing(parent, name, val)
    } else {
        false
    }
}

fn eval_string_set(args: &[Expr], env: &EnvRef, p: Pos, out: &RefCell<String>) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("{p}: string-set! requires 3 arguments")));
    }
    let var_name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("{p}: string-set!: first argument must be a variable"))),
    };
    let idx = eval_expr(&args[1], env, out)?;
    let idx = match idx {
        Value::Integer(n) => n as usize,
        _ => return Err(EvalError::Type(format!("{p}: string-set!: expected integer index"))),
    };
    let ch = eval_expr(&args[2], env, out)?;
    let ch = match ch {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(format!("{p}: string-set!: expected char"))),
    };
    let s = env_get(env, &var_name)
        .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {var_name}")))?;
    match s {
        Value::Str(mut string) => {
            if idx >= string.len() {
                return Err(EvalError::Type(format!("{p}: string-set!: index out of range")));
            }
            // SAFETY: replacing ASCII-range byte with a char
            unsafe { string.as_bytes_mut()[idx] = ch as u8; }
            env_set_existing(env, &var_name, Value::Str(string));
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type(format!("{p}: string-set!: expected string"))),
    }
}

// --- Records ---

fn eval_define_record_type(args: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field-name ...) predicate (field-name accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity(format!("{p}: define-record-type requires at least 3 arguments")));
    }
    // arg 0: type name (ignored, just used for identity)
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // arg 1: constructor spec (constructor-name field-name ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => {
            let name = match &items[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected constructor name"))),
            };
            let fields: Vec<String> = items[1..].iter().map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type(format!("{p}: define-record-type: expected field name"))),
            }).collect::<Result<Vec<_>, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected constructor spec"))),
    };

    // arg 2: predicate name
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected predicate name"))),
    };

    // remaining args: field specs (field-name accessor-name)
    let mut field_specs: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            ExprKind::List(items) if items.len() >= 2 => {
                let field_name = match &items[0].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected field name"))),
                };
                let accessor_name = match &items[1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected accessor name"))),
                };
                field_specs.push((field_name, accessor_name));
            }
            _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected field spec"))),
        }
    }

    // Bind constructor
    env_set(env, constructor_name, Value::RecordConstructor {
        type_id,
        num_fields: constructor_fields.len(),
    });

    // Bind predicate
    env_set(env, predicate_name, Value::RecordPredicate { type_id });

    // Bind accessors - map field name to index in constructor fields
    for (field_name, accessor_name) in &field_specs {
        let field_index = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Type(format!(
                "{p}: define-record-type: field '{field_name}' not in constructor"
            )))?;
        env_set(env, accessor_name.clone(), Value::RecordAccessor {
            type_id,
            field_index,
        });
    }

    Ok(Value::Void)
}

// --- Macro support ---

const SYNTAX_KEYWORDS: &[&str] = &[
    "if", "let", "let*", "begin", "set!", "cond", "and", "or", "quote",
    "lambda", "define", "define-syntax", "define-record-type", "string-set!",
    "letrec", "letrec*", "case", "do", "case-lambda",
];

#[derive(Clone)]
enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

fn eval_define_syntax(args: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{p}: define-syntax requires 2 arguments")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("{p}: define-syntax: expected symbol"))),
    };
    match &args[1].kind {
        ExprKind::List(items) if items.len() >= 2 => {
            if !matches!(&items[0].kind, ExprKind::Symbol(s) if s == "syntax-rules") {
                return Err(EvalError::Type(format!("{p}: define-syntax: expected syntax-rules")));
            }
            let literals = match &items[1].kind {
                ExprKind::List(lits) => {
                    lits.iter().map(|l| match &l.kind {
                        ExprKind::Symbol(s) => Ok(s.clone()),
                        _ => Err(EvalError::Type(format!("{}: expected symbol in literals", l.pos))),
                    }).collect::<Result<Vec<_>, _>>()?
                }
                _ => return Err(EvalError::Type(format!("{p}: syntax-rules: expected literals list"))),
            };
            let mut rules = Vec::new();
            for rule in &items[2..] {
                match &rule.kind {
                    ExprKind::List(pair) if pair.len() == 2 => {
                        rules.push((pair[0].clone(), pair[1].clone()));
                    }
                    _ => return Err(EvalError::Type(format!("{}: expected (pattern template)", rule.pos))),
                }
            }
            env_set(env, name, Value::Macro { literals, rules, def_env: env.clone() });
            Ok(Value::Void)
        }
        _ => Err(EvalError::Type(format!("{p}: define-syntax: expected syntax-rules expression"))),
    }
}

fn match_pattern(
    pattern: &Expr,
    form: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&form.kind, ExprKind::Symbol(f) if f == s)
        }
        ExprKind::Symbol(s) if s != "..." => {
            bindings.insert(s.clone(), MacroBinding::Single(form.clone()));
            true
        }
        ExprKind::List(pat_items) => {
            if let ExprKind::List(form_items) = &form.kind {
                match_pattern_list(pat_items, form_items, literals, bindings)
            } else {
                false
            }
        }
        ExprKind::Integer(a) => matches!(&form.kind, ExprKind::Integer(b) if a == b),
        ExprKind::Boolean(a) => matches!(&form.kind, ExprKind::Boolean(b) if a == b),
        _ => false,
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    forms: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let has_ellipsis = patterns.len() >= 2
        && matches!(&patterns[patterns.len() - 1].kind, ExprKind::Symbol(s) if s == "...");

    if has_ellipsis {
        let fixed = &patterns[..patterns.len() - 2];
        let ellipsis_pat = &patterns[patterns.len() - 2];
        if forms.len() < fixed.len() {
            return false;
        }
        for (pat, frm) in fixed.iter().zip(forms.iter()) {
            if !match_pattern(pat, frm, literals, bindings) {
                return false;
            }
        }
        let remaining = &forms[fixed.len()..];
        match &ellipsis_pat.kind {
            ExprKind::Symbol(s) if !literals.contains(s) && s != "..." && s != "_" => {
                bindings.insert(s.clone(), MacroBinding::Many(remaining.to_vec()));
                true
            }
            _ => false,
        }
    } else {
        if patterns.len() != forms.len() {
            return false;
        }
        for (pat, frm) in patterns.iter().zip(forms.iter()) {
            if !match_pattern(pat, frm, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn find_many_vars_in_template(template: &Expr, bindings: &HashMap<String, MacroBinding>) -> Vec<String> {
    let mut result = Vec::new();
    match &template.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Many(_))) {
                result.push(s.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                result.extend(find_many_vars_in_template(item, bindings));
            }
        }
        _ => {}
    }
    result
}

/// Try to expand an ellipsis pattern (item followed by `...`).
/// Returns the expanded elements if successful.
fn expand_ellipsis(
    item: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    gensym_map: &mut HashMap<String, String>,
) -> Option<Vec<Expr>> {
    // Simple case: item is a Many var directly
    if let ExprKind::Symbol(var) = &item.kind {
        if let Some(MacroBinding::Many(elems)) = bindings.get(var) {
            return Some(elems.clone());
        }
    }
    // Complex sub-template: find the first Many var and iterate over it
    let many_var = find_many_vars_in_template(item, bindings).into_iter().next()?;
    let MacroBinding::Many(elems) = bindings.get(&many_var)? else { return None };
    let elems = elems.clone();
    let expanded = elems.iter().map(|elem| {
        let mut sub_bindings = bindings.clone();
        sub_bindings.insert(many_var.clone(), MacroBinding::Single(elem.clone()));
        expand_template(item, &sub_bindings, gensym_map)
    }).collect();
    Some(expanded)
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    gensym_map: &mut HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(expr) => expr.clone(),
                    MacroBinding::Many(_) => template.clone(),
                }
            } else if SYNTAX_KEYWORDS.contains(&s.as_str()) {
                template.clone()
            } else {
                // Template-introduced symbol: gensym for hygiene
                let gs = if let Some((existing, _)) = gensym_map.iter().find(|(_, v)| v.as_str() == s) {
                    existing.clone()
                } else {
                    let gs = gensym(s);
                    gensym_map.insert(gs.clone(), s.clone());
                    gs
                };
                Expr::new(ExprKind::Symbol(gs), template.pos)
            }
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() && matches!(&items[i + 1].kind, ExprKind::Symbol(s) if s == "...") {
                    if let Some(elems) = expand_ellipsis(&items[i], bindings, gensym_map) {
                        expanded.extend(elems);
                        i += 2;
                        continue;
                    }
                }
                expanded.push(expand_template(&items[i], bindings, gensym_map));
                i += 1;
            }
            Expr::new(ExprKind::List(expanded), template.pos)
        }
        _ => template.clone(),
    }
}

fn eval_macro_call(
    items: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &EnvRef,
    use_env: &EnvRef,
    p: Pos,
    out: &RefCell<String>,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if let ExprKind::List(pat_items) = &pattern.kind {
            // Skip first element of both pattern and form (macro keyword)
            if match_pattern_list(&pat_items[1..], &items[1..], literals, &mut bindings) {
                let mut gensym_map = HashMap::new();
                let expanded = expand_template(template, &bindings, &mut gensym_map);
                // Set up hygiene: bind gensyms to def-env values
                let hyg_env = new_env(Some(use_env.clone()));
                for (gs, original) in &gensym_map {
                    if let Some(val) = env_get(def_env, original) {
                        env_set(&hyg_env, gs.clone(), val);
                    }
                }
                return eval_expr(&expanded, &hyg_env, out);
            }
        }
    }
    Err(EvalError::Type(format!("{p}: no matching syntax-rules pattern")))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = new_env(None);
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                  "cons", "car", "cdr", "list", "length", "null?",
                  "number?", "boolean?", "pair?", "string?", "symbol?", "procedure?", "append",
                  "display", "write", "newline",
                  "string-append", "string-length", "substring",
                  "string->number", "number->string",
                  "symbol->string", "string->symbol",
                  "string-ref", "char?", "string-copy", "string->list", "list->string", "apply",
                  "char->integer", "integer->char",
                  "equal?", "eq?", "eqv?",
                  "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                  "zero?", "positive?", "negative?", "odd?", "even?",
                  "list-ref", "list-tail", "list?", "assoc", "map", "for-each",
                  "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
                  "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                  "char=?", "char<?",
                  "exact?", "inexact?", "rational?", "integer?",
                  "exact->inexact", "inexact->exact",
                  "numerator", "denominator",
                  "vector", "make-vector", "vector-ref", "vector-set!",
                  "vector-length", "vector?", "vector->list", "list->vector"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    let out = RefCell::new(String::new());
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    Ok(result.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = new_env(None);
    for name in ["+", "-", "*", "/", "<", ">", "=", "<=", ">=", "not",
                  "cons", "car", "cdr", "list", "length", "null?",
                  "number?", "boolean?", "pair?", "string?", "symbol?", "procedure?", "append",
                  "display", "write", "newline",
                  "string-append", "string-length", "substring",
                  "string->number", "number->string",
                  "symbol->string", "string->symbol",
                  "string-ref", "char?", "string-copy", "string->list", "list->string", "apply",
                  "char->integer", "integer->char",
                  "equal?", "eq?", "eqv?",
                  "abs", "modulo", "remainder", "quotient", "min", "max", "expt",
                  "zero?", "positive?", "negative?", "odd?", "even?",
                  "list-ref", "list-tail", "list?", "assoc", "map", "for-each",
                  "string=?", "string<?", "string-ci=?", "string-upcase", "string-downcase",
                  "char-alphabetic?", "char-numeric?", "char-upcase", "char-downcase",
                  "char=?", "char<?",
                  "exact?", "inexact?", "rational?", "integer?",
                  "exact->inexact", "inexact->exact",
                  "numerator", "denominator",
                  "vector", "make-vector", "vector-ref", "vector-set!",
                  "vector-length", "vector?", "vector->list", "list->vector"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    let out = RefCell::new(String::new());
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval_expr(expr, &env, &out)?;
    }
    let output = out.into_inner();
    Ok((result.display_scheme(), output))
}

#[cfg(test)]
mod tests;
