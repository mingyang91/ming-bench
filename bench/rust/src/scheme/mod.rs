mod builtins;
pub mod error;
mod macros;
mod parser;

pub use error::EvalError;
use error::Span;
use parser::{Parser, parse_params};
use builtins::{apply_builtin, format_float_value, values_eqv};
pub(crate) use builtins::make_rational;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

const DUMMY_SPAN: Span = Span { line: 0, col: 0 };

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);
static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(crate) fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{}__g{}", base, n)
}

pub(crate) type Output = Rc<RefCell<String>>;

/// A single lambda clause: (params, rest_param, body, closure_env).
pub(crate) type LambdaClause = (Vec<String>, Option<String>, Vec<Spanned>, Env);

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Value {
    Integer(i64),
    Float(f64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Spanned>),
    Pair(Box<Value>, Box<Value>), // improper pair (a . b) where b is not a list
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
    Void,
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
            Value::Pair(a, b) => format!("({} . {})", a.display_value(), b.display_value()),
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|val| val.display_value()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Lambda(..) | Value::CaseLambda(..) => "#<procedure>".into(),
            Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) => "#<procedure>".into(),
            Value::Record(..) => "#<record>".into(),
            Value::SyntaxRules { .. } => "#<syntax>".into(),
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
            Value::Pair(a, b) => format!("({} . {})", a.format_display(), b.format_display()),
            Value::Vector(v) => {
                let inner: Vec<String> = v.borrow().iter().map(|val| val.format_display()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Record(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) => self.display_value(),
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
}

// --- Environment ---

pub(crate) type Env = Rc<RefCell<EnvInner>>;

#[derive(Debug, PartialEq)]
pub(crate) struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

fn new_env(parent: Option<Env>) -> Env {
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

fn env_update(env: &Env, name: &str, val: Value) -> bool {
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

// --- Macros (syntax-rules) ---


fn apply_macro(
    items: &[Spanned],
    literals: &[String],
    rules: &[(Spanned, Spanned)],
    def_env: &Env,
    env: &Env,
    out: &Output,
    span: Span,
) -> Result<Value, EvalError> {
    let Some((expanded, renames)) = macros::try_expand(items, literals, rules) else {
        return Err(EvalError::Type("no matching pattern for macro".into(), span));
    };
    for (orig, gs) in &renames {
        if let Some(val) = env_get(def_env, orig) {
            env_set(env, gs.clone(), val);
        }
    }
    eval(&expanded, env, out)
}

// --- Evaluator ---

fn eval(expr: &Spanned, env: &Env, out: &Output) -> Result<Value, EvalError> {
    let span = expr.span;
    match &expr.val {
        Value::Integer(_) | Value::Float(_) | Value::Rational(..) | Value::Boolean(_) | Value::Str(_) | Value::Char(_) | Value::Pair(..) | Value::Lambda(..) | Value::CaseLambda(..) | Value::SyntaxRules { .. } | Value::Vector(..) | Value::Record(..) | Value::RecordConstructor(..) | Value::RecordPredicate(..) | Value::RecordAccessor(..) => Ok(expr.val.clone()),
        Value::Symbol(name) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(name.clone(), span))
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
                        return Ok(items[1].val.clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Arity("if requires 2 or 3 arguments".into(), span));
                        }
                        let cond = eval(&items[1], env, out)?;
                        if cond.is_truthy() {
                            return eval(&items[2], env, out);
                        } else if items.len() == 4 {
                            return eval(&items[3], env, out);
                        } else {
                            return Ok(Value::Void);
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
                                return Ok(Value::Void);
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
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("define: expected symbol or list".into(), span)),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Arity("lambda requires at least 2 arguments".into(), span));
                        }
                        let (params, rest) = match &items[1].val {
                            Value::List(param_list) => parse_params(param_list, "lambda", span)?,
                            Value::Symbol(s) => (vec![], Some(s.clone())), // (lambda args body)
                            _ => return Err(EvalError::Type("lambda: expected parameter list".into(), span)),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(params, rest, body, env.clone()));
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
                            let (params, rest) = match &parts[0].val {
                                Value::List(param_list) => parse_params(param_list, "case-lambda", span)?,
                                Value::Symbol(s) => (vec![], Some(s.clone())),
                                _ => return Err(EvalError::Type("case-lambda: expected parameter list".into(), span)),
                            };
                            let body = parts[1..].to_vec();
                            clauses.push((params, rest, body, env.clone()));
                        }
                        return Ok(Value::CaseLambda(clauses));
                    }
                    "let" => return eval_let(&items[1..], env, out, span),
                    "begin" => {
                        let mut result = Value::Void;
                        for expr in &items[1..] {
                            result = eval(expr, env, out)?;
                        }
                        return Ok(result);
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
                        return Ok(Value::Void);
                    }
                    "cond" => return eval_cond(&items[1..], env, out, span),
                    "and" => return eval_and(&items[1..], env, out),
                    "or" => return eval_or(&items[1..], env, out),
                    "string-set!" => {
                        if items.len() != 4 {
                            return Err(EvalError::Arity("string-set! requires 3 arguments".into(), span));
                        }
                        let Value::Symbol(var_name) = &items[1].val else {
                            return Err(EvalError::Type("string-set!: first argument must be a variable".into(), span));
                        };
                        let idx_val = eval(&items[2], env, out)?;
                        let Value::Integer(idx_n) = &idx_val else {
                            return Err(EvalError::Type("string-set!: index must be integer".into(), span));
                        };
                        let idx = *idx_n as usize;
                        let ch = match eval(&items[3], env, out)? {
                            Value::Char(c) => c,
                            _ => return Err(EvalError::Type("string-set!: third argument must be a char".into(), span)),
                        };
                        let s = env_get(env, var_name).ok_or_else(|| EvalError::UnboundVariable(var_name.clone(), span))?;
                        match s {
                            Value::Str(st) => {
                                let mut chars: Vec<char> = st.chars().collect();
                                if idx >= chars.len() {
                                    return Err(EvalError::Type("string-set!: index out of bounds".into(), span));
                                }
                                chars[idx] = ch;
                                env_update(env, var_name, Value::Str(chars.into_iter().collect()));
                                return Ok(Value::Void);
                            }
                            _ => return Err(EvalError::Type("string-set!: expected string".into(), span)),
                        }
                    }
                    "not" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity("not requires 1 argument".into(), span));
                        }
                        let v = eval(&items[1], env, out)?;
                        return Ok(Value::Boolean(!v.is_truthy()));
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
                        return Ok(Value::Void);
                    }
                    "letrec" => return eval_letrec(&items[1..], env, out, span),
                    "letrec*" => return eval_letrec(&items[1..], env, out, span),
                    "case" => return eval_case(&items[1..], env, out, span),
                    "do" => return eval_do(&items[1..], env, out, span),
                    "define-syntax" => {
                        if items.len() != 3 {
                            return Err(EvalError::Arity("define-syntax requires 2 arguments".into(), span));
                        }
                        let Value::Symbol(macro_name) = &items[1].val else {
                            return Err(EvalError::Type("define-syntax: expected symbol".into(), span));
                        };
                        let Value::List(sr_parts) = &items[2].val else {
                            return Err(EvalError::Type("define-syntax: expected syntax-rules".into(), span));
                        };
                        if sr_parts.is_empty() || !matches!(&sr_parts[0].val, Value::Symbol(s) if s == "syntax-rules") {
                            return Err(EvalError::Type("define-syntax: expected syntax-rules form".into(), span));
                        }
                        if sr_parts.len() < 2 {
                            return Err(EvalError::Arity("syntax-rules requires literals list".into(), span));
                        }
                        let Value::List(lit_list) = &sr_parts[1].val else {
                            return Err(EvalError::Type("syntax-rules: expected literals list".into(), span));
                        };
                        let literals: Vec<String> = lit_list.iter().filter_map(|l| {
                            if let Value::Symbol(s) = &l.val { Some(s.clone()) } else { None }
                        }).collect();
                        let mut rules = Vec::new();
                        for rule in &sr_parts[2..] {
                            let Value::List(parts) = &rule.val else {
                                return Err(EvalError::Type("syntax-rules: expected rule".into(), span));
                            };
                            if parts.len() != 2 {
                                return Err(EvalError::Arity("syntax-rules: rule needs pattern and template".into(), span));
                            }
                            rules.push((parts[0].clone(), parts[1].clone()));
                        }
                        let val = Value::SyntaxRules { literals, rules, def_env: env.clone() };
                        env_set(env, macro_name.clone(), val);
                        return Ok(Value::Void);
                    }
                    _ => {
                        // Check for macro application
                        if let Some(Value::SyntaxRules { ref literals, ref rules, ref def_env }) = env_get(env, name) {
                            return apply_macro(items, literals, rules, def_env, env, out, span);
                        }
                    }
                }
            }
            // Function application
            let func = eval(head, env, out)?;
            let args: Result<Vec<Value>, _> = items[1..].iter().map(|a| eval(a, env, out)).collect();
            let args = args?;
            apply(&func, &args, out, span)
        }
        Value::Void => Ok(Value::Void),
    }
}

fn apply(func: &Value, args: &[Value], out: &Output, span: Span) -> Result<Value, EvalError> {
    match func {
        Value::Lambda(params, rest, body, closure_env) => {
            if let Some(rest_name) = rest {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    ), span));
                }
                let local_env = new_env(Some(closure_env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let rest_list = args[params.len()..].iter()
                    .map(|a| Spanned::new(a.clone(), DUMMY_SPAN))
                    .collect();
                env_set(&local_env, rest_name.clone(), Value::List(rest_list));
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, out)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    ), span));
                }
                let local_env = new_env(Some(closure_env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, out)?;
                }
                Ok(result)
            }
        }
        Value::CaseLambda(clauses) => {
            for (params, rest, body, closure_env) in clauses {
                let matches = if rest.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    return apply(&Value::Lambda(params.clone(), rest.clone(), body.clone(), closure_env.clone()), args, out, span);
                }
            }
            Err(EvalError::Arity(format!("case-lambda: no matching clause for {} arguments", args.len()), span))
        }
        Value::RecordConstructor(type_id, field_count) => {
            if args.len() != *field_count {
                return Err(EvalError::Arity(format!("record constructor expects {} arguments, got {}", field_count, args.len()), span));
            }
            Ok(Value::Record(*type_id, args.to_vec()))
        }
        Value::RecordPredicate(type_id) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record predicate requires 1 argument".into(), span));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record(tid, _) if tid == type_id)))
        }
        Value::RecordAccessor(type_id, idx) => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor requires 1 argument".into(), span));
            }
            match &args[0] {
                Value::Record(tid, fields) if tid == type_id => Ok(fields[*idx].clone()),
                _ => Err(EvalError::Type("record accessor: wrong record type".into(), span)),
            }
        }
        Value::Symbol(name) => apply_builtin(name, args, out, span, apply),
        _ => Err(EvalError::Type("not a procedure".into(), span)),
    }
}

fn eval_and(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env, out)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Spanned], env: &Env, out: &Output) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for expr in exprs {
        let result = eval(expr, env, out)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_let(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
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
        let lambda = Value::Lambda(params.clone(), None, body, loop_env.clone());
        env_set(&loop_env, name.clone(), lambda);
        let call_env = new_env(Some(loop_env));
        for (p, v) in params.iter().zip(inits.iter()) {
            env_set(&call_env, p.clone(), v.clone());
        }
        let body_ref = &args[2..];
        let mut result = Value::Void;
        for expr in body_ref {
            result = eval(expr, &call_env, out)?;
        }
        return Ok(result);
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
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
    for clause in clauses {
        let Value::List(parts) = &clause.val else {
            return Err(EvalError::Type("cond: expected list clause".into(), span));
        };
        if parts.is_empty() {
            return Err(EvalError::Arity("cond: empty clause".into(), span));
        }
        if let Value::Symbol(s) = &parts[0].val {
            if s == "else" {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
        let test = eval(&parts[0], env, out)?;
        if test.is_truthy() {
            let mut result = test;
            for expr in &parts[1..] {
                result = eval(expr, env, out)?;
            }
            return Ok(result);
        }
    }
    Ok(Value::Void)
}

fn eval_letrec(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
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
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, out)?;
    }
    Ok(result)
}

fn eval_case(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
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
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
        // Datum list
        let Value::List(datums) = &parts[0].val else {
            return Err(EvalError::Type("case: expected datum list".into(), span));
        };
        for datum in datums {
            if values_eqv(&key, &datum.val) {
                let mut result = Value::Void;
                for expr in &parts[1..] {
                    result = eval(expr, env, out)?;
                }
                return Ok(result);
            }
        }
    }
    Ok(Value::Void)
}


fn eval_do(args: &[Spanned], env: &Env, out: &Output, span: Span) -> Result<Value, EvalError> {
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
                   "assq", "memq"] {
        env_set(&env, name.to_string(), Value::Symbol(name.to_string()));
    }
    env
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("no expressions".into(), Span { line: 1, col: 1 }));
    }
    let env = make_global_env();
    let out = Rc::new(RefCell::new(String::new()));
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &out)?;
    }
    let output = out.borrow().clone();
    Ok((last.display_value(), output))
}

#[cfg(test)]
mod tests;
