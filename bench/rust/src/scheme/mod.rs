pub mod error;
mod builtins;
mod parser;

pub use error::EvalError;

use builtins::apply_builtin_by_name;
use parser::{Expr, ExprKind, Pos, parse_all};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
static RECORD_TYPE_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__{base}_{n}")
}

#[derive(Clone)]
struct EnvFrame {
    bindings: HashMap<String, Value>,
    parent: Option<EnvRef>,
}

type EnvRef = Rc<RefCell<EnvFrame>>;

#[derive(Clone)]
struct CaseLambdaClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: EnvRef,
}

type ApplyFn = fn(&Value, &[Value], Pos, &RefCell<String>) -> Result<Value, EvalError>;

type Kont = Rc<KontFrame>;

// Continuation frames for the CEK machine
#[derive(Clone)]
enum KontFrame {
    Halt,
    Seq { rest: Vec<Expr>, env: EnvRef, next: Kont },
    If { then_b: Expr, else_b: Option<Expr>, env: EnvRef, next: Kont },
    Define { name: String, env: EnvRef, next: Kont },
    Set { name: String, env: EnvRef, pos: Pos, next: Kont },
    // Evaluating the operator in a function application; arg_exprs in REVERSE order (right-to-left)
    EvalOp { arg_exprs_rev: Vec<Expr>, env: EnvRef, pos: Pos, next: Kont },
    // Evaluating arguments right-to-left; remaining in reverse order, done accumulates right-to-left
    EvalArgs { func: Value, done: Vec<Value>, remaining: Vec<Expr>, env: EnvRef, pos: Pos, next: Kont },
    And { rest: Vec<Expr>, env: EnvRef, next: Kont },
    Or { rest: Vec<Expr>, env: EnvRef, next: Kont },
    CondTest { body: Vec<Expr>, remaining_clauses: Vec<Vec<Expr>>, env: EnvRef, next: Kont },
    // Let: evaluating bindings in outer env. done collects (name, value) pairs.
    // remaining has init exprs not yet evaluated. current_name = name for the value being awaited.
    LetBind { current_name: String, done: Vec<(String, Value)>, remaining: Vec<(String, Expr)>,
              eval_env: EnvRef, parent_env: EnvRef, body: Vec<Expr>, named: Option<String>, next: Kont },
    // Let*: evaluating bindings in growing local env
    LetStarBind { current_name: String, remaining: Vec<(String, Expr)>,
                  local_env: EnvRef, body: Vec<Expr>, next: Kont },
    // Letrec/Letrec*: evaluating inits in local env
    LetrecBind { current_name: String, remaining: Vec<(String, Expr)>,
                 local_env: EnvRef, body: Vec<Expr>, next: Kont },
    CaseMatch { clauses: Vec<Expr>, env: EnvRef, pos: Pos, next: Kont },
    StringSetIdx { var_name: String, char_expr: Expr, env: EnvRef, pos: Pos, next: Kont },
    StringSetChar { var_name: String, idx: usize, env: EnvRef, pos: Pos, next: Kont },
    MapCall { func: Value, results: Vec<Value>, remaining: Vec<Vec<Value>>, pos: Pos, next: Kont },
    ForEachCall { func: Value, remaining: Vec<Vec<Value>>, pos: Pos, next: Kont },
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64),
    Boolean(bool),
    Char(char),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Pair(Rc<RefCell<(Value, Value)>>),
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
    Continuation(Kont),
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({n})"),
            Value::Float(v) => write!(f, "Float({v})"),
            Value::Rational(n, d) => write!(f, "Rational({n}/{d})"),
            Value::Boolean(b) => write!(f, "Boolean({b})"),
            Value::Char(c) => write!(f, "Char({c:?})"),
            Value::Str(s) => write!(f, "Str({s:?})"),
            Value::Symbol(s) => write!(f, "Symbol({s})"),
            Value::List(items) => write!(f, "List({items:?})"),
            Value::Pair(_) => write!(f, "Pair(...)"),
            Value::Lambda { .. } => write!(f, "Lambda(...)"),
            Value::Void => write!(f, "Void"),
            Value::Macro { .. } => write!(f, "Macro(...)"),
            Value::Record { type_id, .. } => write!(f, "Record({type_id})"),
            Value::RecordConstructor { type_id, .. } => write!(f, "RecordConstructor({type_id})"),
            Value::RecordPredicate { type_id } => write!(f, "RecordPredicate({type_id})"),
            Value::RecordAccessor { type_id, field_index } => write!(f, "RecordAccessor({type_id}, {field_index})"),
            Value::CaseLambda { .. } => write!(f, "CaseLambda(...)"),
            Value::Vector(_) => write!(f, "Vector(...)"),
            Value::Continuation(_) => write!(f, "Continuation(...)"),
        }
    }
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
            (Value::Pair(a), Value::Pair(b)) => {
                if Rc::ptr_eq(a, b) { return true; }
                let ab = a.borrow();
                let bb = b.borrow();
                ab.0 == bb.0 && ab.1 == bb.1
            }
            (Value::Void, Value::Void) => true,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::CaseLambda { .. }, Value::CaseLambda { .. }) => false,
            (Value::Macro { .. }, Value::Macro { .. }) => false,
            (Value::Record { type_id: a, fields: af }, Value::Record { type_id: b, fields: bf }) => a == b && af == bf,
            (Value::Vector(a), Value::Vector(b)) => *a.borrow() == *b.borrow(),
            (Value::Continuation(a), Value::Continuation(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

fn list_from_vec(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = val.clone();
            let mut seen = HashSet::new();
            loop {
                match current {
                    Value::Pair(ref p) => {
                        let ptr = Rc::as_ptr(p) as usize;
                        if !seen.insert(ptr) { return None; }
                        let (car, cdr) = {
                            let b = p.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        result.push(car);
                        current = cdr;
                    }
                    Value::List(ref items) if items.is_empty() => return Some(result),
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

impl Value {
    fn display_pair_impl(&self, seen: &mut HashSet<usize>, write_mode: bool) -> String {
        if let Value::Pair(p) = self {
            let ptr = Rc::as_ptr(p) as usize;
            if !seen.insert(ptr) {
                return "(...)".to_string();
            }
            let (car_val, cdr_val) = {
                let b = p.borrow();
                (b.0.clone(), b.1.clone())
            };
            let mut parts = vec![car_val.display_impl(seen, write_mode)];
            let mut current = cdr_val;
            loop {
                match current {
                    Value::Pair(ref p2) => {
                        let ptr2 = Rc::as_ptr(p2) as usize;
                        if !seen.insert(ptr2) {
                            break;
                        }
                        let (c, d) = {
                            let b = p2.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        parts.push(c.display_impl(seen, write_mode));
                        current = d;
                    }
                    Value::List(ref items) if items.is_empty() => break,
                    ref other => {
                        return format!("({} . {})", parts.join(" "), other.display_impl(seen, write_mode));
                    }
                }
            }
            format!("({})", parts.join(" "))
        } else {
            unreachable!()
        }
    }

    fn display_impl(&self, seen: &mut HashSet<usize>, write_mode: bool) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Float(f) => {
                let s = format!("{f}");
                if f.is_finite() && !s.contains('.') { format!("{f}.0") } else { s }
            }
            Value::Rational(n, d) => format!("{n}/{d}"),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Char(c) => {
                if write_mode {
                    match c {
                        ' ' => "#\\space".to_string(),
                        '\n' => "#\\newline".to_string(),
                        '\t' => "#\\tab".to_string(),
                        _ => format!("#\\{c}"),
                    }
                } else {
                    c.to_string()
                }
            }
            Value::Str(s) => {
                if write_mode { format!("\"{s}\"") } else { s.clone() }
            }
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_impl(seen, write_mode)).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => self.display_pair_impl(seen, write_mode),
            Value::Lambda { .. } | Value::CaseLambda { .. } => "#<procedure>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::Record { .. } => "#<record>".to_string(),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => "#<procedure>".to_string(),
            Value::Vector(v) => {
                let items: Vec<String> = v.borrow().iter().map(|v| v.display_impl(seen, write_mode)).collect();
                format!("#({})", items.join(" "))
            }
            Value::Void => "".to_string(),
            Value::Continuation(_) => "#<continuation>".to_string(),
        }
    }

    fn display_scheme(&self) -> String {
        let mut seen = HashSet::new();
        self.display_impl(&mut seen, true)
    }

    fn display_output(&self) -> String {
        let mut seen = HashSet::new();
        self.display_impl(&mut seen, false)
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// --- Environment ---

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

// --- Helpers ---

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
        ExprKind::Symbol(s) => (vec![], Some(s.clone())),
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

fn bind_args(
    params: &[String],
    rest_param: &Option<String>,
    args: &[Value],
    parent_env: &EnvRef,
    p: Pos,
) -> Result<EnvRef, EvalError> {
    if rest_param.is_some() {
        if args.len() < params.len() {
            return Err(EvalError::Arity(format!(
                "{p}: expected at least {} args, got {}", params.len(), args.len()
            )));
        }
    } else if args.len() != params.len() {
        return Err(EvalError::Arity(format!(
            "{p}: expected {} args, got {}", params.len(), args.len()
        )));
    }
    let local_env = new_env(Some(parent_env.clone()));
    for (param, arg) in params.iter().zip(args.iter()) {
        env_set(&local_env, param.clone(), arg.clone());
    }
    if let Some(rest) = rest_param {
        let rest_args = if args.len() > params.len() {
            args[params.len()..].to_vec()
        } else {
            vec![]
        };
        env_set(&local_env, rest.clone(), list_from_vec(rest_args));
    }
    Ok(local_env)
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
        ExprKind::List(items) => {
            if items.is_empty() {
                Value::List(vec![])
            } else {
                list_from_vec(items.iter().map(expr_to_value).collect())
            }
        }
    }
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

// --- Records ---

fn eval_define_record_type(args: &[Expr], env: &EnvRef, p: Pos) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Arity(format!("{p}: define-record-type requires at least 3 arguments")));
    }
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);
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
    let predicate_name = match &args[2].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type(format!("{p}: define-record-type: expected predicate name"))),
    };
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
    env_set(env, constructor_name, Value::RecordConstructor { type_id, num_fields: constructor_fields.len() });
    env_set(env, predicate_name, Value::RecordPredicate { type_id });
    for (field_name, accessor_name) in &field_specs {
        let field_index = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Type(format!("{p}: define-record-type: field '{field_name}' not in constructor")))?;
        env_set(env, accessor_name.clone(), Value::RecordAccessor { type_id, field_index });
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

fn match_pattern(pattern: &Expr, form: &Expr, literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
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

fn match_pattern_list(patterns: &[Expr], forms: &[Expr], literals: &[String], bindings: &mut HashMap<String, MacroBinding>) -> bool {
    let has_ellipsis = patterns.len() >= 2
        && matches!(&patterns[patterns.len() - 1].kind, ExprKind::Symbol(s) if s == "...");
    if has_ellipsis {
        let fixed = &patterns[..patterns.len() - 2];
        let ellipsis_pat = &patterns[patterns.len() - 2];
        if forms.len() < fixed.len() { return false; }
        for (pat, frm) in fixed.iter().zip(forms.iter()) {
            if !match_pattern(pat, frm, literals, bindings) { return false; }
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
        if patterns.len() != forms.len() { return false; }
        for (pat, frm) in patterns.iter().zip(forms.iter()) {
            if !match_pattern(pat, frm, literals, bindings) { return false; }
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
            for item in items { result.extend(find_many_vars_in_template(item, bindings)); }
        }
        _ => {}
    }
    result
}

fn expand_ellipsis(item: &Expr, bindings: &HashMap<String, MacroBinding>, gensym_map: &mut HashMap<String, String>) -> Option<Vec<Expr>> {
    if let ExprKind::Symbol(var) = &item.kind {
        if let Some(MacroBinding::Many(elems)) = bindings.get(var) {
            return Some(elems.clone());
        }
    }
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

fn expand_template(template: &Expr, bindings: &HashMap<String, MacroBinding>, gensym_map: &mut HashMap<String, String>) -> Expr {
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

fn expand_macro(
    items: &[Expr], literals: &[String], rules: &[(Expr, Expr)],
    def_env: &EnvRef, use_env: &EnvRef, p: Pos,
) -> Result<(Expr, EnvRef), EvalError> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        if let ExprKind::List(pat_items) = &pattern.kind {
            if match_pattern_list(&pat_items[1..], &items[1..], literals, &mut bindings) {
                let mut gensym_map = HashMap::new();
                let expanded = expand_template(template, &bindings, &mut gensym_map);
                let hyg_env = new_env(Some(use_env.clone()));
                for (gs, original) in &gensym_map {
                    if let Some(val) = env_get(def_env, original) {
                        env_set(&hyg_env, gs.clone(), val);
                    }
                }
                return Ok((expanded, hyg_env));
            }
        }
    }
    Err(EvalError::Type(format!("{p}: no matching syntax-rules pattern")))
}

// --- Desugar `do` to named let ---

fn desugar_do(args: &[Expr], p: Pos) -> Result<Expr, EvalError> {
    let var_specs = match &args[0].kind {
        ExprKind::List(items) => items,
        _ => return Err(EvalError::Type(format!("{p}: do: expected variable list"))),
    };
    let test_clause = match &args[1].kind {
        ExprKind::List(items) if !items.is_empty() => items,
        _ => return Err(EvalError::Type(format!("{p}: do: expected test clause"))),
    };
    let body = &args[2..];

    let loop_name = gensym("do");
    let mut bindings = Vec::new();
    let mut step_args = Vec::new();

    for spec in var_specs {
        let items = match &spec.kind {
            ExprKind::List(items) if items.len() >= 2 => items,
            _ => return Err(EvalError::Type(format!("{}: do: expected (var init step)", spec.pos))),
        };
        let var_name = items[0].clone();
        let init = items[1].clone();
        bindings.push(Expr::new(ExprKind::List(vec![var_name.clone(), init]), spec.pos));
        if items.len() >= 3 {
            step_args.push(items[2].clone());
        } else {
            step_args.push(var_name);
        }
    }

    let loop_sym = Expr::new(ExprKind::Symbol(loop_name.clone()), p);
    let mut loop_call_items: Vec<Expr> = vec![loop_sym.clone()];
    loop_call_items.extend(step_args);
    let loop_call = Expr::new(ExprKind::List(loop_call_items), p);

    let result_body = if test_clause.len() > 1 {
        let mut begin_items = vec![Expr::new(ExprKind::Symbol("begin".to_string()), p)];
        begin_items.extend_from_slice(&test_clause[1..]);
        Expr::new(ExprKind::List(begin_items), p)
    } else {
        Expr::new(ExprKind::List(vec![Expr::new(ExprKind::Symbol("begin".to_string()), p)]), p)
    };

    let mut else_items = vec![Expr::new(ExprKind::Symbol("begin".to_string()), p)];
    for b in body { else_items.push(b.clone()); }
    else_items.push(loop_call);
    let else_body = Expr::new(ExprKind::List(else_items), p);

    let if_expr = Expr::new(ExprKind::List(vec![
        Expr::new(ExprKind::Symbol("if".to_string()), p),
        test_clause[0].clone(),
        result_body,
        else_body,
    ]), p);

    let bindings_list = Expr::new(ExprKind::List(bindings), p);
    Ok(Expr::new(ExprKind::List(vec![
        Expr::new(ExprKind::Symbol("let".to_string()), p),
        loop_sym,
        bindings_list,
        if_expr,
    ]), p))
}

// --- CEK Machine ---

enum CekState {
    Eval(Expr, EnvRef),
    ApplyK(Value),
}

fn apply_func_unreachable(_: &Value, _: &[Value], _: Pos, _: &RefCell<String>) -> Result<Value, EvalError> {
    unreachable!("builtin callback should not be called in CEK mode")
}

/// Set up state + k for evaluating a body (sequence of expressions).
/// The last expression is in tail position (no extra frame).
fn body_state(body: &[Expr], env: EnvRef, k: Kont) -> (CekState, Kont) {
    if body.is_empty() {
        (CekState::ApplyK(Value::Void), k)
    } else if body.len() == 1 {
        (CekState::Eval(body[0].clone(), env), k)
    } else {
        let new_k = Rc::new(KontFrame::Seq {
            rest: body[1..].to_vec(),
            env: env.clone(),
            next: k,
        });
        (CekState::Eval(body[0].clone(), env), new_k)
    }
}

/// Apply a function to evaluated arguments. Returns new (state, k).
fn do_apply(func: Value, args: Vec<Value>, pos: Pos, k: Kont, out: &RefCell<String>) -> Result<(CekState, Kont), EvalError> {
    match func {
        Value::Lambda { params, rest_param, body, env } => {
            let local_env = bind_args(&params, &rest_param, &args, &env, pos)?;
            Ok(body_state(&body, local_env, k))
        }
        Value::CaseLambda { clauses } => {
            for clause in clauses {
                let m = if clause.rest_param.is_some() {
                    args.len() >= clause.params.len()
                } else {
                    args.len() == clause.params.len()
                };
                if !m { continue; }
                let local_env = bind_args(&clause.params, &clause.rest_param, &args, &clause.env, pos)?;
                return Ok(body_state(&clause.body, local_env, k));
            }
            Err(EvalError::Arity(format!("{pos}: case-lambda: no matching clause for {} args", args.len())))
        }
        Value::Continuation(captured_k) => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: continuation expects 1 argument, got {}", args.len())));
            }
            Ok((CekState::ApplyK(args.into_iter().next().unwrap()), captured_k))
        }
        Value::Symbol(ref s) if s == "call/cc" || s == "call-with-current-continuation" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: call/cc requires 1 argument")));
            }
            let cont_val = Value::Continuation(k.clone());
            let f = args.into_iter().next().unwrap();
            do_apply(f, vec![cont_val], pos, k, out)
        }
        Value::Symbol(ref s) if s == "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{pos}: map requires at least 2 arguments")));
            }
            let func = args[0].clone();
            let lists: Vec<Vec<Value>> = args[1..].iter()
                .map(|a| value_to_vec(a).ok_or_else(|| EvalError::Type(format!("{pos}: map: expected list"))))
                .collect::<Result<_, _>>()?;
            if lists.is_empty() || lists[0].is_empty() {
                return Ok((CekState::ApplyK(Value::List(vec![])), k));
            }
            let len = lists[0].len();
            for l in &lists[1..] {
                if l.len() != len {
                    return Err(EvalError::Type(format!("{pos}: map: lists must have same length")));
                }
            }
            // Build remaining arg groups (in reverse so we can pop from end)
            let mut remaining: Vec<Vec<Value>> = Vec::new();
            for i in (1..len).rev() {
                remaining.push(lists.iter().map(|l| l[i].clone()).collect());
            }
            let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
            let new_k = Rc::new(KontFrame::MapCall {
                func: func.clone(),
                results: vec![],
                remaining,
                pos,
                next: k,
            });
            do_apply(func, first_args, pos, new_k, out)
        }
        Value::Symbol(ref s) if s == "for-each" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{pos}: for-each requires at least 2 arguments")));
            }
            let func = args[0].clone();
            let lists: Vec<Vec<Value>> = args[1..].iter()
                .map(|a| value_to_vec(a).ok_or_else(|| EvalError::Type(format!("{pos}: for-each: expected list"))))
                .collect::<Result<_, _>>()?;
            if lists.is_empty() || lists[0].is_empty() {
                return Ok((CekState::ApplyK(Value::Void), k));
            }
            let len = lists[0].len();
            let mut remaining: Vec<Vec<Value>> = Vec::new();
            for i in (1..len).rev() {
                remaining.push(lists.iter().map(|l| l[i].clone()).collect());
            }
            let first_args: Vec<Value> = lists.iter().map(|l| l[0].clone()).collect();
            let new_k = Rc::new(KontFrame::ForEachCall {
                func: func.clone(),
                remaining,
                pos,
                next: k,
            });
            do_apply(func, first_args, pos, new_k, out)
        }
        Value::Symbol(ref s) if s == "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{pos}: apply requires at least 2 arguments")));
            }
            let f = args[0].clone();
            let last = &args[args.len() - 1];
            let tail = value_to_vec(last).ok_or_else(|| EvalError::Type(format!("{pos}: apply: last argument must be a list")))?;
            let mut final_args: Vec<Value> = args[1..args.len() - 1].to_vec();
            final_args.extend(tail);
            do_apply(f, final_args, pos, k, out)
        }
        Value::RecordConstructor { type_id, num_fields } => {
            if args.len() != num_fields {
                return Err(EvalError::Arity(format!("{pos}: record constructor expected {num_fields} args, got {}", args.len())));
            }
            Ok((CekState::ApplyK(Value::Record { type_id, fields: args }), k))
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: record predicate expects 1 argument")));
            }
            Ok((CekState::ApplyK(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if *tid == type_id))), k))
        }
        Value::RecordAccessor { type_id, field_index } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{pos}: record accessor expects 1 argument")));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields } if *tid == type_id => {
                    Ok((CekState::ApplyK(fields[field_index].clone()), k))
                }
                _ => Err(EvalError::Type(format!("{pos}: record accessor: wrong type"))),
            }
        }
        Value::Symbol(s) => {
            let val = apply_builtin_by_name(&s, &args, pos, out, apply_func_unreachable)?;
            Ok((CekState::ApplyK(val), k))
        }
        _ => Err(EvalError::Type(format!("{pos}: not a procedure: {}", func.display_scheme()))),
    }
}

/// Start processing cond clauses. Each clause is a Vec<Expr> (the items inside the clause parens).
fn start_cond(clauses: &[Vec<Expr>], env: &EnvRef, k: Kont) -> (CekState, Kont) {
    if clauses.is_empty() {
        return (CekState::ApplyK(Value::Void), k);
    }
    let clause = &clauses[0];
    let is_else = matches!(&clause[0].kind, ExprKind::Symbol(ref s) if s == "else");
    if is_else {
        body_state(&clause[1..], env.clone(), k)
    } else {
        let body = clause[1..].to_vec();
        let remaining: Vec<Vec<Expr>> = clauses[1..].to_vec();
        let new_k = Rc::new(KontFrame::CondTest {
            body,
            remaining_clauses: remaining,
            env: env.clone(),
            next: k,
        });
        (CekState::Eval(clause[0].clone(), env.clone()), new_k)
    }
}

/// Parse let bindings into (name, init_expr) pairs.
fn parse_let_bindings(bindings_expr: &[Expr], form: &str) -> Result<Vec<(String, Expr)>, EvalError> {
    let mut result = Vec::new();
    for b in bindings_expr {
        match &b.kind {
            ExprKind::List(pair) if pair.len() == 2 => {
                let ExprKind::Symbol(s) = &pair[0].kind else {
                    return Err(EvalError::Type(format!("{}: {form}: expected symbol in binding", pair[0].pos)));
                };
                result.push((s.clone(), pair[1].clone()));
            }
            _ => return Err(EvalError::Type(format!("{}: {form}: expected (var init) binding", b.pos))),
        }
    }
    Ok(result)
}

/// Start evaluating a let binding sequence.
/// For regular let: eval_env = outer env, parent_env = outer env.
/// For named let: same, but with `named` = loop name.
fn start_let_bindings(
    bindings: Vec<(String, Expr)>, eval_env: EnvRef, parent_env: EnvRef,
    body: Vec<Expr>, named: Option<String>, k: Kont, out: &RefCell<String>,
) -> Result<(CekState, Kont), EvalError> {
    if bindings.is_empty() {
        // No bindings: create local env and eval body
        let local_env = new_env(Some(parent_env));
        if let Some(name) = named {
            let lambda = Value::Lambda { params: vec![], rest_param: None, body: body.clone(), env: local_env.clone() };
            env_set(&local_env, name, lambda);
        }
        Ok(body_state(&body, local_env, k))
    } else {
        let mut iter = bindings.into_iter();
        let (first_name, first_expr) = iter.next().unwrap();
        let remaining: Vec<(String, Expr)> = iter.collect();
        let new_k = Rc::new(KontFrame::LetBind {
            current_name: first_name,
            done: vec![],
            remaining,
            eval_env: eval_env.clone(),
            parent_env,
            body,
            named,
            next: k,
        });
        let _ = out; // suppress unused warning
        Ok((CekState::Eval(first_expr, eval_env), new_k))
    }
}

fn cek_run(exprs: &[Expr], env: &EnvRef, out: &RefCell<String>) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Void);
    }

    let mut k: Kont = Rc::new(KontFrame::Halt);
    if exprs.len() > 1 {
        k = Rc::new(KontFrame::Seq {
            rest: exprs[1..].to_vec(),
            env: env.clone(),
            next: k,
        });
    }
    let mut state = CekState::Eval(exprs[0].clone(), env.clone());

    let halt: Kont = Rc::new(KontFrame::Halt);

    'main: loop {
        match state {
            CekState::Eval(expr, env) => {
                let p = expr.pos;
                match expr.kind {
                    ExprKind::Integer(n) => { state = CekState::ApplyK(Value::Integer(n)); }
                    ExprKind::Float(f) => { state = CekState::ApplyK(Value::Float(f)); }
                    ExprKind::Rational(n, d) => { state = CekState::ApplyK(builtins::make_rational(n, d)); }
                    ExprKind::Boolean(b) => { state = CekState::ApplyK(Value::Boolean(b)); }
                    ExprKind::Str(s) => { state = CekState::ApplyK(Value::Str(s)); }
                    ExprKind::Char(c) => { state = CekState::ApplyK(Value::Char(c)); }
                    ExprKind::Symbol(name) => {
                        let val = env_get(&env, &name)
                            .ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))?;
                        state = CekState::ApplyK(val);
                    }
                    ExprKind::List(mut items) => {
                        if items.is_empty() {
                            state = CekState::ApplyK(Value::List(vec![]));
                            continue 'main;
                        }

                        let head_sym: Option<String> = match &items[0].kind {
                            ExprKind::Symbol(s) => Some(s.clone()),
                            _ => None,
                        };

                        if let Some(ref op) = head_sym {
                            match op.as_str() {
                                "and" => {
                                    if items.len() <= 1 {
                                        state = CekState::ApplyK(Value::Boolean(true));
                                    } else if items.len() == 2 {
                                        state = CekState::Eval(items.swap_remove(1), env);
                                    } else {
                                        let first = items.remove(1);
                                        let rest: Vec<Expr> = items[1..].to_vec();
                                        k = Rc::new(KontFrame::And { rest, env: env.clone(), next: k });
                                        state = CekState::Eval(first, env);
                                    }
                                    continue 'main;
                                }
                                "or" => {
                                    if items.len() <= 1 {
                                        state = CekState::ApplyK(Value::Boolean(false));
                                    } else if items.len() == 2 {
                                        state = CekState::Eval(items.swap_remove(1), env);
                                    } else {
                                        let first = items.remove(1);
                                        let rest: Vec<Expr> = items[1..].to_vec();
                                        k = Rc::new(KontFrame::Or { rest, env: env.clone(), next: k });
                                        state = CekState::Eval(first, env);
                                    }
                                    continue 'main;
                                }
                                "if" => {
                                    if items.len() < 3 || items.len() > 4 {
                                        return Err(EvalError::Arity(format!("{p}: if requires 2 or 3 arguments")));
                                    }
                                    let then_b = items[2].clone();
                                    let else_b = if items.len() == 4 { Some(items[3].clone()) } else { None };
                                    k = Rc::new(KontFrame::If { then_b, else_b, env: env.clone(), next: k });
                                    state = CekState::Eval(items[1].clone(), env);
                                    continue 'main;
                                }
                                "begin" => {
                                    let (s, new_k) = body_state(&items[1..], env, k);
                                    state = s; k = new_k;
                                    continue 'main;
                                }
                                "cond" => {
                                    // Parse clauses
                                    let mut clauses: Vec<Vec<Expr>> = Vec::new();
                                    for item in items.iter().skip(1) {
                                        match &item.kind {
                                            ExprKind::List(citems) if !citems.is_empty() => {
                                                clauses.push(citems.clone());
                                            }
                                            _ => return Err(EvalError::Type(format!("{}: cond: expected clause list", item.pos))),
                                        }
                                    }
                                    let (s, new_k) = start_cond(&clauses, &env, k);
                                    state = s; k = new_k;
                                    continue 'main;
                                }
                                "let" => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity(format!("{p}: let requires bindings and body")));
                                    }
                                    // Check for named let
                                    if let ExprKind::Symbol(name) = &items[1].kind {
                                        let name = name.clone();
                                        if items.len() < 4 {
                                            return Err(EvalError::Arity(format!("{p}: named let requires bindings and body")));
                                        }
                                        let bindings_expr = match &items[2].kind {
                                            ExprKind::List(bs) => bs,
                                            _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
                                        };
                                        let bindings = parse_let_bindings(bindings_expr, "let")?;
                                        let body = items[3..].to_vec();
                                        let (s, new_k) = start_let_bindings(bindings, env.clone(), env, body, Some(name), k, out)?;
                                        state = s; k = new_k;
                                        continue 'main;
                                    }
                                    // Regular let
                                    let bindings_expr = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type(format!("{p}: let: expected bindings list"))),
                                    };
                                    let bindings = parse_let_bindings(bindings_expr, "let")?;
                                    let body = items[2..].to_vec();
                                    let (s, new_k) = start_let_bindings(bindings, env.clone(), env, body, None, k, out)?;
                                    state = s; k = new_k;
                                    continue 'main;
                                }
                                "let*" => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity(format!("{p}: let* requires bindings and body")));
                                    }
                                    let bindings_expr = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type(format!("{p}: let*: expected bindings list"))),
                                    };
                                    let bindings = parse_let_bindings(bindings_expr, "let*")?;
                                    let body = items[2..].to_vec();
                                    let local_env = new_env(Some(env));
                                    if bindings.is_empty() {
                                        let (s, new_k) = body_state(&body, local_env, k);
                                        state = s; k = new_k;
                                    } else {
                                        let mut iter = bindings.into_iter();
                                        let (first_name, first_expr) = iter.next().unwrap();
                                        let remaining: Vec<(String, Expr)> = iter.collect();
                                        k = Rc::new(KontFrame::LetStarBind {
                                            current_name: first_name,
                                            remaining,
                                            local_env: local_env.clone(),
                                            body,
                                            next: k,
                                        });
                                        state = CekState::Eval(first_expr, local_env);
                                    }
                                    continue 'main;
                                }
                                "letrec" | "letrec*" => {
                                    if items.len() < 3 {
                                        return Err(EvalError::Arity(format!("{p}: {op} requires bindings and body")));
                                    }
                                    let bindings_expr = match &items[1].kind {
                                        ExprKind::List(bs) => bs,
                                        _ => return Err(EvalError::Type(format!("{p}: {op}: expected bindings list"))),
                                    };
                                    let bindings = parse_let_bindings(bindings_expr, op)?;
                                    let body = items[2..].to_vec();
                                    let local_env = new_env(Some(env));
                                    // Bind all vars to Void first
                                    for (name, _) in &bindings {
                                        env_set(&local_env, name.clone(), Value::Void);
                                    }
                                    if bindings.is_empty() {
                                        let (s, new_k) = body_state(&body, local_env, k);
                                        state = s; k = new_k;
                                    } else {
                                        let mut iter = bindings.into_iter();
                                        let (first_name, first_expr) = iter.next().unwrap();
                                        let remaining: Vec<(String, Expr)> = iter.collect();
                                        k = Rc::new(KontFrame::LetrecBind {
                                            current_name: first_name,
                                            remaining,
                                            local_env: local_env.clone(),
                                            body,
                                            next: k,
                                        });
                                        state = CekState::Eval(first_expr, local_env);
                                    }
                                    continue 'main;
                                }
                                "define" => {
                                    if items.len() < 2 {
                                        return Err(EvalError::Arity(format!("{p}: define requires at least 2 arguments")));
                                    }
                                    match &items[1].kind {
                                        ExprKind::Symbol(name) => {
                                            if items.len() != 3 {
                                                return Err(EvalError::Arity(format!("{p}: define requires 2 arguments")));
                                            }
                                            k = Rc::new(KontFrame::Define { name: name.clone(), env: env.clone(), next: k });
                                            state = CekState::Eval(items[2].clone(), env);
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
                                            let body = items[2..].to_vec();
                                            let lambda = Value::Lambda { params, rest_param, body, env: env.clone() };
                                            env_set(&env, name, lambda);
                                            state = CekState::ApplyK(Value::Void);
                                        }
                                        _ => return Err(EvalError::Type(format!("{p}: define: expected symbol or list"))),
                                    }
                                    continue 'main;
                                }
                                "set!" => {
                                    if items.len() != 3 {
                                        return Err(EvalError::Arity(format!("{p}: set! requires 2 arguments")));
                                    }
                                    let name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type(format!("{p}: set!: expected symbol"))),
                                    };
                                    k = Rc::new(KontFrame::Set { name, env: env.clone(), pos: p, next: k });
                                    state = CekState::Eval(items[2].clone(), env);
                                    continue 'main;
                                }
                                "quote" => {
                                    if items.len() != 2 {
                                        return Err(EvalError::Arity(format!("{p}: quote requires 1 argument")));
                                    }
                                    state = CekState::ApplyK(expr_to_value(&items[1]));
                                    continue 'main;
                                }
                                "lambda" => {
                                    state = CekState::ApplyK(eval_lambda(&items[1..], &env, p)?);
                                    continue 'main;
                                }
                                "case-lambda" => {
                                    state = CekState::ApplyK(eval_case_lambda(&items[1..], &env, p)?);
                                    continue 'main;
                                }
                                "define-syntax" => {
                                    state = CekState::ApplyK(eval_define_syntax(&items[1..], &env, p)?);
                                    continue 'main;
                                }
                                "define-record-type" => {
                                    state = CekState::ApplyK(eval_define_record_type(&items[1..], &env, p)?);
                                    continue 'main;
                                }
                                "string-set!" => {
                                    if items.len() != 4 {
                                        return Err(EvalError::Arity(format!("{p}: string-set! requires 3 arguments")));
                                    }
                                    let var_name = match &items[1].kind {
                                        ExprKind::Symbol(s) => s.clone(),
                                        _ => return Err(EvalError::Type(format!("{p}: string-set!: first argument must be a variable"))),
                                    };
                                    k = Rc::new(KontFrame::StringSetIdx {
                                        var_name,
                                        char_expr: items[3].clone(),
                                        env: env.clone(),
                                        pos: p,
                                        next: k,
                                    });
                                    state = CekState::Eval(items[2].clone(), env);
                                    continue 'main;
                                }
                                "case" => {
                                    if items.len() < 2 {
                                        return Err(EvalError::Arity(format!("{p}: case requires key and clauses")));
                                    }
                                    k = Rc::new(KontFrame::CaseMatch {
                                        clauses: items[2..].to_vec(),
                                        env: env.clone(),
                                        pos: p,
                                        next: k,
                                    });
                                    state = CekState::Eval(items[1].clone(), env);
                                    continue 'main;
                                }
                                "do" => {
                                    let desugared = desugar_do(&items[1..], p)?;
                                    state = CekState::Eval(desugared, env);
                                    continue 'main;
                                }
                                _ => {
                                    // Check for macro
                                    if let Some(Value::Macro { literals, rules, def_env }) = env_get(&env, op) {
                                        let (expanded, hyg_env) = expand_macro(&items, &literals, &rules, &def_env, &env, p)?;
                                        state = CekState::Eval(expanded, hyg_env);
                                        continue 'main;
                                    }
                                    // Fall through to function application below
                                }
                            }
                        }

                        // Function application (including when special form match falls through)
                        // Evaluate operator first, then args RIGHT-TO-LEFT
                        let arg_exprs: Vec<Expr> = items[1..].iter().rev().cloned().collect();
                        k = Rc::new(KontFrame::EvalOp {
                            arg_exprs_rev: arg_exprs,
                            env: env.clone(),
                            pos: p,
                            next: k,
                        });
                        state = CekState::Eval(items[0].clone(), env);
                    }
                }
            }
            CekState::ApplyK(val) => {
                // Take ownership of continuation frame to avoid cloning when refcount == 1.
                // Only falls back to clone when the continuation is shared (captured by call/cc).
                let old_k = std::mem::replace(&mut k, halt.clone());
                let frame = match Rc::try_unwrap(old_k) {
                    Ok(f) => f,
                    Err(rc) => (*rc).clone(),
                };
                match frame {
                    KontFrame::Halt => return Ok(val),

                    KontFrame::Seq { mut rest, env, next } => {
                        if rest.len() == 1 {
                            state = CekState::Eval(rest.swap_remove(0), env);
                            k = next;
                        } else {
                            let first = rest.remove(0);
                            k = Rc::new(KontFrame::Seq {
                                rest,
                                env: env.clone(),
                                next,
                            });
                            state = CekState::Eval(first, env);
                        }
                    }

                    KontFrame::If { then_b, else_b, env, next } => {
                        k = next;
                        if val.is_truthy() {
                            state = CekState::Eval(then_b, env);
                        } else if let Some(eb) = else_b {
                            state = CekState::Eval(eb, env);
                        } else {
                            state = CekState::ApplyK(Value::Void);
                        }
                    }

                    KontFrame::Define { name, env, next } => {
                        k = next;
                        env_set(&env, name, val);
                        state = CekState::ApplyK(Value::Void);
                    }

                    KontFrame::Set { name, env, pos, next } => {
                        k = next;
                        if !env_set_existing(&env, &name, val) {
                            return Err(EvalError::UnboundVariable(format!("{pos}: {name}")));
                        }
                        state = CekState::ApplyK(Value::Void);
                    }

                    KontFrame::EvalOp { mut arg_exprs_rev, env, pos, next } => {
                        let func = val;
                        k = next;
                        if arg_exprs_rev.is_empty() {
                            let (s, new_k) = do_apply(func, vec![], pos, k, out)?;
                            state = s; k = new_k;
                        } else {
                            let first = arg_exprs_rev.remove(0);
                            k = Rc::new(KontFrame::EvalArgs {
                                func,
                                done: vec![],
                                remaining: arg_exprs_rev,
                                env: env.clone(),
                                pos,
                                next: k,
                            });
                            state = CekState::Eval(first, env);
                        }
                    }

                    KontFrame::EvalArgs { func, mut done, mut remaining, env, pos, next } => {
                        done.push(val);
                        if remaining.is_empty() {
                            done.reverse();
                            let (s, new_k) = do_apply(func, done, pos, next, out)?;
                            state = s; k = new_k;
                        } else {
                            let next_expr = remaining.remove(0);
                            k = Rc::new(KontFrame::EvalArgs {
                                func,
                                done,
                                remaining,
                                env: env.clone(),
                                pos,
                                next,
                            });
                            state = CekState::Eval(next_expr, env);
                        }
                    }

                    KontFrame::And { mut rest, env, next } => {
                        if !val.is_truthy() {
                            k = next;
                            state = CekState::ApplyK(val);
                        } else if rest.len() == 1 {
                            k = next;
                            state = CekState::Eval(rest.swap_remove(0), env);
                        } else {
                            let first = rest.remove(0);
                            k = Rc::new(KontFrame::And {
                                rest,
                                env: env.clone(),
                                next,
                            });
                            state = CekState::Eval(first, env);
                        }
                    }

                    KontFrame::Or { mut rest, env, next } => {
                        if val.is_truthy() {
                            k = next;
                            state = CekState::ApplyK(val);
                        } else if rest.len() == 1 {
                            k = next;
                            state = CekState::Eval(rest.swap_remove(0), env);
                        } else {
                            let first = rest.remove(0);
                            k = Rc::new(KontFrame::Or {
                                rest,
                                env: env.clone(),
                                next,
                            });
                            state = CekState::Eval(first, env);
                        }
                    }

                    KontFrame::CondTest { body, remaining_clauses, env, next } => {
                        if val.is_truthy() {
                            if body.is_empty() {
                                k = next;
                                state = CekState::ApplyK(val);
                            } else {
                                let (s, new_k) = body_state(&body, env, next);
                                state = s; k = new_k;
                            }
                        } else {
                            let (s, new_k) = start_cond(&remaining_clauses, &env, next);
                            state = s; k = new_k;
                        }
                    }

                    KontFrame::LetBind { current_name, mut done, remaining, eval_env, parent_env, body, named, next } => {
                        done.push((current_name, val));

                        if remaining.is_empty() {
                            let local_env = new_env(Some(parent_env));
                            if let Some(ref name) = named {
                                let params: Vec<String> = done.iter().map(|(n, _)| n.clone()).collect();
                                let lambda = Value::Lambda {
                                    params,
                                    rest_param: None,
                                    body: body.clone(),
                                    env: local_env.clone(),
                                };
                                env_set(&local_env, name.clone(), lambda);
                            }
                            for (n, v) in done {
                                env_set(&local_env, n, v);
                            }
                            let (s, new_k) = body_state(&body, local_env, next);
                            state = s; k = new_k;
                        } else {
                            let mut iter = remaining.into_iter();
                            let (next_name, next_expr) = iter.next().unwrap();
                            let rest: Vec<(String, Expr)> = iter.collect();
                            k = Rc::new(KontFrame::LetBind {
                                current_name: next_name,
                                done,
                                remaining: rest,
                                eval_env: eval_env.clone(),
                                parent_env,
                                body,
                                named,
                                next,
                            });
                            state = CekState::Eval(next_expr, eval_env);
                        }
                    }

                    KontFrame::LetStarBind { current_name, remaining, local_env, body, next } => {
                        env_set(&local_env, current_name, val);

                        if remaining.is_empty() {
                            let (s, new_k) = body_state(&body, local_env, next);
                            state = s; k = new_k;
                        } else {
                            let mut iter = remaining.into_iter();
                            let (next_name, next_expr) = iter.next().unwrap();
                            let rest: Vec<(String, Expr)> = iter.collect();
                            k = Rc::new(KontFrame::LetStarBind {
                                current_name: next_name,
                                remaining: rest,
                                local_env: local_env.clone(),
                                body,
                                next,
                            });
                            state = CekState::Eval(next_expr, local_env);
                        }
                    }

                    KontFrame::LetrecBind { current_name, remaining, local_env, body, next } => {
                        env_set(&local_env, current_name, val);

                        if remaining.is_empty() {
                            let (s, new_k) = body_state(&body, local_env, next);
                            state = s; k = new_k;
                        } else {
                            let mut iter = remaining.into_iter();
                            let (next_name, next_expr) = iter.next().unwrap();
                            let rest: Vec<(String, Expr)> = iter.collect();
                            k = Rc::new(KontFrame::LetrecBind {
                                current_name: next_name,
                                remaining: rest,
                                local_env: local_env.clone(),
                                body,
                                next,
                            });
                            state = CekState::Eval(next_expr, local_env);
                        }
                    }

                    KontFrame::CaseMatch { clauses, env, pos, next } => {
                        let key = val;
                        for clause in &clauses {
                            match &clause.kind {
                                ExprKind::List(citems) if !citems.is_empty() => {
                                    if let ExprKind::Symbol(s) = &citems[0].kind {
                                        if s == "else" {
                                            let (s, new_k) = body_state(&citems[1..], env.clone(), next.clone());
                                            state = s; k = new_k;
                                            continue 'main;
                                        }
                                    }
                                    let datums = match &citems[0].kind {
                                        ExprKind::List(ds) => ds,
                                        _ => return Err(EvalError::Type(format!("{}: case: expected datum list", citems[0].pos))),
                                    };
                                    for datum in datums {
                                        let datum_val = expr_to_value(datum);
                                        if eqv_match(&key, &datum_val) {
                                            let (s, new_k) = body_state(&citems[1..], env.clone(), next.clone());
                                            state = s; k = new_k;
                                            continue 'main;
                                        }
                                    }
                                }
                                _ => return Err(EvalError::Type(format!("{pos}: case: expected clause"))),
                            }
                        }
                        k = next;
                        state = CekState::ApplyK(Value::Void);
                    }

                    KontFrame::StringSetIdx { var_name, char_expr, env, pos, next } => {
                        let idx = match val {
                            Value::Integer(n) => n as usize,
                            _ => return Err(EvalError::Type(format!("{pos}: string-set!: expected integer index"))),
                        };
                        k = Rc::new(KontFrame::StringSetChar { var_name, idx, env: env.clone(), pos, next });
                        state = CekState::Eval(char_expr, env);
                    }

                    KontFrame::StringSetChar { var_name, idx, env, pos, next } => {
                        k = next;
                        let ch = match val {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type(format!("{pos}: string-set!: expected char"))),
                        };
                        let s = env_get(&env, &var_name)
                            .ok_or_else(|| EvalError::UnboundVariable(format!("{pos}: {var_name}")))?;
                        match s {
                            Value::Str(mut string) => {
                                if idx >= string.len() {
                                    return Err(EvalError::Type(format!("{pos}: string-set!: index out of range")));
                                }
                                unsafe { string.as_bytes_mut()[idx] = ch as u8; }
                                env_set_existing(&env, &var_name, Value::Str(string));
                                state = CekState::ApplyK(Value::Void);
                            }
                            _ => return Err(EvalError::Type(format!("{pos}: string-set!: expected string"))),
                        }
                    }

                    KontFrame::MapCall { func, mut results, mut remaining, pos, next } => {
                        results.push(val);
                        if remaining.is_empty() {
                            k = next;
                            state = CekState::ApplyK(list_from_vec(results));
                        } else {
                            let next_args = remaining.pop().unwrap();
                            let func_clone = func.clone();
                            k = Rc::new(KontFrame::MapCall {
                                func,
                                results,
                                remaining,
                                pos,
                                next,
                            });
                            let (s, new_k) = do_apply(func_clone, next_args, pos, k, out)?;
                            state = s; k = new_k;
                        }
                    }

                    KontFrame::ForEachCall { func, mut remaining, pos, next } => {
                        if remaining.is_empty() {
                            k = next;
                            state = CekState::ApplyK(Value::Void);
                        } else {
                            let next_args = remaining.pop().unwrap();
                            let func_clone = func.clone();
                            k = Rc::new(KontFrame::ForEachCall {
                                func,
                                remaining,
                                pos,
                                next,
                            });
                            let (s, new_k) = do_apply(func_clone, next_args, pos, k, out)?;
                            state = s; k = new_k;
                        }
                    }
                }
            }
        }
    }
}

// --- Public API ---

fn make_initial_env() -> EnvRef {
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
                  "vector-length", "vector?", "vector->list", "list->vector",
                  "set-car!", "set-cdr!",
                  "reverse", "memq", "memv", "member", "assq", "assv",
                  "gcd", "lcm", "round", "truncate",
                  "make-string", "string",
                  "string<=?", "string>=?", "string>?",
                  "caar", "cadr", "cdar", "cddr",
                  "caaar", "caadr", "cadar", "caddr",
                  "cdaar", "cdadr", "cddar", "cdddr",
                  "caaaar", "caaadr", "caadar", "caaddr",
                  "cadaar", "cadadr", "caddar", "cadddr",
                  "cdaaar", "cdaadr", "cdadar", "cdaddr",
                  "cddaar", "cddadr", "cdddar", "cddddr",
                  "call/cc", "call-with-current-continuation"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = make_initial_env();
    let out = RefCell::new(String::new());
    let result = cek_run(&exprs, &env, &out)?;
    Ok(result.display_scheme())
}

pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = make_initial_env();
    let out = RefCell::new(String::new());
    let result = cek_run(&exprs, &env, &out)?;
    let output = out.into_inner();
    Ok((result.display_scheme(), output))
}

#[cfg(test)]
mod tests;
