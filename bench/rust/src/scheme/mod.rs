pub mod error;
mod builtins;
mod macros;
mod parser;

use parser::{parse_all, Expr, Pos};

pub use error::EvalError;
use builtins::apply_builtin;
use macros::{eval_define_syntax, expand_macro};

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone)]
pub(super) struct CaseLambdaClause {
    params: Vec<String>,
    rest_param: Option<String>,
    body: Vec<Expr>,
    env: Env,
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
    Pair(Box<Value>, Box<Value>),
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

fn make_rational(n: i64, d: i64) -> Value {
    let (n, d) = if d < 0 { (-n, -d) } else { (n, d) };
    let g = gcd(n.abs(), d);
    let (n, d) = (n / g, d / g);
    if d == 1 {
        Value::Integer(n)
    } else {
        Value::Rational(n, d)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(v) => {
                let s = format!("{v}");
                if s.contains('.') || s.contains('e') || s.contains('E') || s.contains("inf") || s.contains("NaN") {
                    write!(f, "{s}")
                } else {
                    write!(f, "{s}.0")
                }
            }
            Value::Rational(n, d) => write!(f, "{n}/{d}"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, "{s}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Pair(a, d) => {
                write!(f, "({a}")?;
                let mut cur = d.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cd) => {
                            write!(f, " {ca}")?;
                            cur = cd.as_ref();
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        other => {
                            write!(f, " . {other}")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Value::Char(c) => match c {
                ' ' => write!(f, "#\\space"),
                '\n' => write!(f, "#\\newline"),
                '\t' => write!(f, "#\\tab"),
                _ => write!(f, "#\\{c}"),
            },
            Value::Lambda { .. } | Value::CaseLambda { .. } => write!(f, "#<procedure>"),
            Value::Builtin(name) => write!(f, "#<builtin:{name}>"),
            Value::Macro { .. } => write!(f, "#<macro>"),
            Value::Record { type_name, .. } => write!(f, "#<record:{type_name}>"),
            Value::RecordConstructor { .. }
            | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. } => write!(f, "#<procedure>"),
            Value::Void => write!(f, ""),
        }
    }
}

impl Value {
    /// Format for `display` — strings without quotes.
    fn display_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{s}"),
            Value::Char(c) => write!(f, "{c}"),
            Value::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Pair(a, d) => {
                write!(f, "(")?;
                a.display_fmt(f)?;
                let mut cur = d.as_ref();
                loop {
                    match cur {
                        Value::Pair(ca, cd) => {
                            write!(f, " ")?;
                            ca.display_fmt(f)?;
                            cur = cd.as_ref();
                        }
                        Value::List(elems) if elems.is_empty() => break,
                        other => {
                            write!(f, " . ")?;
                            other.display_fmt(f)?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
            Value::Vector(v) => {
                let elems = v.borrow();
                write!(f, "#(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    e.display_fmt(f)?;
                }
                write!(f, ")")
            }
            Value::Macro { .. } => write!(f, "#<macro>"),
            other => fmt::Display::fmt(other, f),
        }
    }
}

struct DisplayValue<'a>(&'a Value);
impl<'a> fmt::Display for DisplayValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display_fmt(f)
    }
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

fn new_env(parent: Option<Env>) -> Env {
    Rc::new(RefCell::new(EnvInner {
        bindings: HashMap::new(),
        parent,
    }))
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

fn is_truthy(v: &Value) -> bool {
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
    "procedure?",
    "eqv?",
    "vector", "make-vector", "vector-ref", "vector-set!", "vector-length", "vector?",
    "vector->list", "list->vector",
    "memq", "assq",
    "for-each",
];

fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let p = expr.pos();
    match expr {
        Expr::Integer(n, _) => Ok(Value::Integer(*n)),
        Expr::Float(f, _) => Ok(Value::Float(*f)),
        Expr::Rational(n, d, _) => Ok(make_rational(*n, *d)),
        Expr::Boolean(b, _) => Ok(Value::Boolean(*b)),
        Expr::Str(s, _) => Ok(Value::Str(s.clone())),
        Expr::Char(c, _) => Ok(Value::Char(*c)),
        Expr::Symbol(name, _) => {
            env_get(env, name).ok_or_else(|| EvalError::UnboundVariable(format!("{p}: {name}")))
        }
        Expr::List(elems, _) => {
            if elems.is_empty() {
                return Ok(Value::List(vec![]));
            }
            // Check for special forms
            if let Expr::Symbol(op, _) = &elems[0] {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], p, env, output),
                    "if" => return eval_if(&elems[1..], p, env, output),
                    "quote" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: quote expects 1 argument")));
                        }
                        return Ok(expr_to_value(&elems[1]));
                    }
                    "lambda" => return eval_lambda(&elems[1..], p, env),
                    "case-lambda" => return eval_case_lambda(&elems[1..], p, env),
                    "let" => return eval_let(&elems[1..], p, env, output),
                    "begin" => return eval_begin(&elems[1..], env, output),
                    "cond" => return eval_cond(&elems[1..], env, output),
                    "and" => return eval_and(&elems[1..], env, output),
                    "or" => return eval_or(&elems[1..], env, output),
                    "set!" => return eval_set_bang(&elems[1..], p, env, output),
                    "string-set!" => return eval_string_set(&elems[1..], p, env, output),
                    "not" => {
                        if elems.len() != 2 {
                            return Err(EvalError::Arity(format!("{p}: not expects 1 argument")));
                        }
                        let val = eval(&elems[1], env, output)?;
                        return Ok(Value::Boolean(!is_truthy(&val)));
                    }
                    "define-syntax" => return eval_define_syntax(&elems[1..], p, env),
                    "define-record-type" => return eval_define_record_type(&elems[1..], p, env),
                    "letrec" => return eval_letrec(&elems[1..], p, env, output),
                    "letrec*" => return eval_letrec_star(&elems[1..], p, env, output),
                    "case" => return eval_case(&elems[1..], p, env, output),
                    "do" => return eval_do(&elems[1..], p, env, output),
                    "let*" => return eval_let_star(&elems[1..], p, env, output),
                    "when" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Arity(format!("{p}: when requires test and body")));
                        }
                        let test = eval(&elems[1], env, output)?;
                        if is_truthy(&test) {
                            return eval_begin(&elems[2..], env, output);
                        }
                        return Ok(Value::Void);
                    }
                    "unless" => {
                        if elems.len() < 3 {
                            return Err(EvalError::Arity(format!("{p}: unless requires test and body")));
                        }
                        let test = eval(&elems[1], env, output)?;
                        if !is_truthy(&test) {
                            return eval_begin(&elems[2..], env, output);
                        }
                        return Ok(Value::Void);
                    }
                    _ => {
                        // Check for macro invocation
                        if let Some(Value::Macro { literals, rules, def_env }) = env_get(env, op) {
                            let (expanded, hygiene_bindings) = expand_macro(&literals, &rules, elems, p, &def_env)?;
                            if hygiene_bindings.is_empty() {
                                return eval(&expanded, env, output);
                            }
                            let hyg_env = new_env(Some(env.clone()));
                            for (name, val) in hygiene_bindings {
                                env_set(&hyg_env, name, val);
                            }
                            return eval(&expanded, &hyg_env, output);
                        }
                    }
                }
            }
            // Function call
            let func = eval(&elems[0], env, output)?;
            let args: Vec<Value> = elems[1..]
                .iter()
                .map(|e| eval(e, env, output))
                .collect::<Result<_, _>>()?;
            apply_func(&func, &args, p, output)
        }
    }
}

fn expr_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n, _) => Value::Integer(*n),
        Expr::Float(f, _) => Value::Float(*f),
        Expr::Rational(n, d, _) => make_rational(*n, *d),
        Expr::Boolean(b, _) => Value::Boolean(*b),
        Expr::Str(s, _) => Value::Str(s.clone()),
        Expr::Symbol(s, _) => Value::Symbol(s.clone()),
        Expr::Char(c, _) => Value::Char(*c),
        Expr::List(elems, _) => Value::List(elems.iter().map(expr_to_value).collect()),
    }
}

fn eval_define(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{call_pos}: define requires at least 2 arguments")));
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name, _) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: define requires exactly 2 arguments")));
            }
            let val = eval(&args[1], env, output)?;
            env_set(env, name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...) or (define (f params... . rest) body...)
        Expr::List(sig, _) => {
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("{call_pos}: define: empty signature")));
            }
            let name = match &sig[0] {
                Expr::Symbol(s, _) => s.clone(),
                _ => return Err(EvalError::Parse(format!("{call_pos}: define: expected function name"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], call_pos)?;
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: define: empty body")));
            }
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env_set(env, name, lambda);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Parse(format!("{call_pos}: define: expected symbol or list"))),
    }
}

fn eval_if(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("{call_pos}: if requires 2 or 3 arguments")));
    }
    let cond = eval(&args[0], env, output)?;
    if is_truthy(&cond) {
        eval(&args[1], env, output)
    } else if args.len() == 3 {
        eval(&args[2], env, output)
    } else {
        Ok(Value::Void)
    }
}

fn parse_params(param_exprs: &[Expr], call_pos: Pos) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_exprs.len() {
        match &param_exprs[i] {
            Expr::Symbol(s, _) if s == "." => {
                if i + 1 >= param_exprs.len() {
                    return Err(EvalError::Parse(format!("{call_pos}: expected rest parameter after .")));
                }
                rest_param = Some(match &param_exprs[i + 1] {
                    Expr::Symbol(s, _) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("{call_pos}: expected symbol for rest parameter"))),
                });
                break;
            }
            Expr::Symbol(s, _) => params.push(s.clone()),
            _ => return Err(EvalError::Parse(format!("{call_pos}: expected parameter name"))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn eval_lambda(args: &[Expr], call_pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: lambda requires params and body")));
    }
    let (params, rest_param) = match &args[0] {
        Expr::List(elems, _) => parse_params(elems, call_pos)?,
        _ => return Err(EvalError::Parse(format!("{call_pos}: lambda: expected parameter list"))),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_case_lambda(args: &[Expr], call_pos: Pos, env: &Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{call_pos}: case-lambda requires at least one clause")));
    }
    let mut clauses = Vec::new();
    for arg in args {
        match arg {
            Expr::List(clause_elems, _) => {
                if clause_elems.len() < 2 {
                    return Err(EvalError::Parse(format!("{call_pos}: case-lambda clause needs params and body")));
                }
                let (params, rest_param) = match &clause_elems[0] {
                    Expr::List(elems, _) => parse_params(elems, call_pos)?,
                    _ => return Err(EvalError::Parse(format!("{call_pos}: case-lambda: expected parameter list"))),
                };
                let body = clause_elems[1..].to_vec();
                clauses.push(CaseLambdaClause { params, rest_param, body, env: env.clone() });
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: case-lambda: expected clause list"))),
        }
    }
    Ok(Value::CaseLambda { clauses })
}

fn eval_and(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for e in exprs {
        result = eval(e, env, output)?;
        if !is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    for e in exprs {
        let result = eval(e, env, output)?;
        if is_truthy(&result) {
            return Ok(result);
        }
    }
    Ok(Value::Boolean(false))
}

fn eval_set_bang(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("{call_pos}: set! requires 2 arguments")));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => return Err(EvalError::Parse(format!("{call_pos}: set!: expected symbol"))),
    };
    let val = eval(&args[1], env, output)?;
    if !env_update(env, &name, val) {
        return Err(EvalError::UnboundVariable(format!("{call_pos}: {name}")));
    }
    Ok(Value::Void)
}

fn eval_string_set(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("{call_pos}: string-set! requires 3 arguments")));
    }
    let var_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: first argument must be a variable"))),
    };
    let idx_val = eval(&args[1], env, output)?;
    let idx = match &idx_val {
        Value::Integer(n) => *n as usize,
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected integer index"))),
    };
    let ch = match eval(&args[2], env, output)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected character"))),
    };
    let mut s = match env_get(env, &var_name) {
        Some(Value::Str(s)) => s,
        Some(_) => return Err(EvalError::Type(format!("{call_pos}: string-set!: expected string"))),
        None => return Err(EvalError::UnboundVariable(format!("{call_pos}: {var_name}"))),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type(format!("{call_pos}: string-set!: index out of range")));
    }
    chars[idx] = ch;
    s = chars.into_iter().collect();
    env_update(env, &var_name, Value::Str(s));
    Ok(Value::Void)
}

fn eval_let(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: let requires bindings and body")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let Expr::Symbol(name, _) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity(format!("{call_pos}: named let requires bindings and body")));
        }
        let bindings_expr = match &args[1] {
            Expr::List(b, _) => b,
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair, _) if pair.len() == 2 => {
                    if let Expr::Symbol(s, _) = &pair[0] {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env, output)?);
                    } else {
                        return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
            }
        }
        let body = args[2..].to_vec();
        let local_env = new_env(Some(env.clone()));
        let lambda = Value::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: local_env.clone(),
        };
        env_set(&local_env, name.clone(), lambda);
        for (param, init) in params.iter().zip(inits.iter()) {
            env_set(&local_env, param.clone(), init.clone());
        }
        let mut result = Value::Void;
        for expr in &args[2..] {
            result = eval(expr, &local_env, output)?;
        }
        return Ok(result);
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: let: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    let val = eval(&pair[1], env, output)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: let: expected symbol in binding")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: let: invalid binding"))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in args {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts, _) if !parts.is_empty() => {
                if let Expr::Symbol(s, _) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env, output)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env, output)?;
                if is_truthy(&test) {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env, output)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse("cond: invalid clause".into())),
        }
    }
    Ok(Value::Void)
}

// ---------- define-record-type ----------

fn eval_define_record_type(args: &[Expr], p: Pos, env: &Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field ...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity(format!("{p}: define-record-type requires at least 3 args")));
    }

    // Parse type name
    let type_name = match &args[0] {
        Expr::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected type name"))),
    };

    // Allocate a unique type ID
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Parse constructor: (make-name field ...)
    let (ctor_name, ctor_fields) = match &args[1] {
        Expr::List(elems, _) if !elems.is_empty() => {
            let name = match &elems[0] {
                Expr::Symbol(n, _) => n.clone(),
                _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected constructor name"))),
            };
            let fields: Vec<String> = elems[1..]
                .iter()
                .map(|e| match e {
                    Expr::Symbol(n, _) => Ok(n.clone()),
                    _ => Err(EvalError::Parse(format!("{p}: define-record-type: expected field name"))),
                })
                .collect::<Result<_, _>>()?;
            (name, fields)
        }
        _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected constructor"))),
    };

    // Parse predicate name
    let pred_name = match &args[2] {
        Expr::Symbol(name, _) => name.clone(),
        _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected predicate name"))),
    };

    // Parse field accessors: (field accessor) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match arg {
            Expr::List(elems, _) if elems.len() == 2 => {
                let field = match &elems[0] {
                    Expr::Symbol(n, _) => n.clone(),
                    _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected field name in accessor"))),
                };
                let accessor = match &elems[1] {
                    Expr::Symbol(n, _) => n.clone(),
                    _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected accessor name"))),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Parse(format!("{p}: define-record-type: expected (field accessor)"))),
        }
    }

    // Define constructor as a lambda that creates a Record value
    let ctor_fields_clone = ctor_fields.clone();
    let type_name_clone = type_name.clone();
    // We store the constructor as a lambda that builds the record
    // Using a closure-like approach: store type_id, type_name, and field names in the env
    // and use a Builtin-like approach. Actually, let's define the constructor and accessors
    // by inserting lambdas into the environment.

    // Constructor: takes N args, returns Record { type_id, fields: [(name, val), ...] }
    // We'll implement this by defining a special builtin-like value.
    // Simplest approach: define them as Lambda values that capture the type info.

    // Actually, the cleanest approach is to create the constructor and accessor functions
    // as actual Rust closures stored in the environment. But our Value doesn't support closures.
    // Instead, let's synthesize Scheme code and eval it.

    // Alternative: Create a special constructor entry in the env and handle it in apply_func.
    // Let's use a simpler approach: create synthetic lambdas using Expr.

    // Create constructor body that returns a record
    // We'll use a special internal form: the constructor is a Lambda whose body
    // we handle specially. Actually, the simplest approach is:
    // Store type metadata in the env and build lambdas that reference it.

    // Cleanest: add a RecordConstructor and RecordAccessor variant... but that's heavy.
    // Let's just use a different approach: store the record as a tagged list internally
    // but with a special tag that makes predicates work.

    // Actually, let's just add constructor/predicate/accessor as Lambda values
    // that we build from Expr nodes programmatically.

    // Simplest: build the record as Value::Record and create the functions via
    // special Value variants. Let me just add RecordConstructor/Predicate/Accessor
    // to keep it clean.

    // Actually, the absolute simplest approach: define them using synthetic code strings.
    // But that's fragile. Let me just handle it directly.

    // Store constructor info
    let ctor_type_id = type_id;
    let ctor_type_name_str = type_name_clone.clone();
    let ctor_field_names = ctor_fields_clone.clone();

    // Define constructor function
    env_set(env, ctor_name, Value::RecordConstructor {
        type_id: ctor_type_id,
        type_name: ctor_type_name_str,
        field_names: ctor_field_names,
    });

    // Define predicate
    env_set(env, pred_name, Value::RecordPredicate {
        type_id,
    });

    // Define accessors
    for (field_name, accessor_name) in &field_accessors {
        env_set(env, accessor_name.clone(), Value::RecordAccessor {
            type_id,
            field_name: field_name.clone(),
        });
    }

    Ok(Value::Void)
}

fn as_integer(v: &Value, call_pos: Pos) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::Type(format!("{call_pos}: expected integer, got {v}"))),
    }
}

fn value_to_f64(v: &Value, call_pos: Pos) -> Result<f64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
        _ => Err(EvalError::Type(format!("{call_pos}: expected number, got {v}"))),
    }
}

fn apply_func(func: &Value, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(name) => apply_builtin(name, args, call_pos, output),
        Value::Lambda {
            params, rest_param, body, env, ..
        } => {
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected at least {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                env_set(&local_env, rest.clone(), Value::List(args[params.len()..].to_vec()));
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, output)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "{call_pos}: expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }
                let local_env = new_env(Some(env.clone()));
                for (param, arg) in params.iter().zip(args.iter()) {
                    env_set(&local_env, param.clone(), arg.clone());
                }
                let mut result = Value::Void;
                for expr in body {
                    result = eval(expr, &local_env, output)?;
                }
                Ok(result)
            }
        }
        Value::CaseLambda { clauses } => {
            for clause in clauses {
                if let Some(rest) = &clause.rest_param {
                    if args.len() >= clause.params.len() {
                        let local_env = new_env(Some(clause.env.clone()));
                        for (param, arg) in clause.params.iter().zip(args.iter()) {
                            env_set(&local_env, param.clone(), arg.clone());
                        }
                        env_set(&local_env, rest.clone(), Value::List(args[clause.params.len()..].to_vec()));
                        let mut result = Value::Void;
                        for expr in &clause.body {
                            result = eval(expr, &local_env, output)?;
                        }
                        return Ok(result);
                    }
                } else if args.len() == clause.params.len() {
                    let local_env = new_env(Some(clause.env.clone()));
                    for (param, arg) in clause.params.iter().zip(args.iter()) {
                        env_set(&local_env, param.clone(), arg.clone());
                    }
                    let mut result = Value::Void;
                    for expr in &clause.body {
                        result = eval(expr, &local_env, output)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "{call_pos}: no matching case-lambda clause for {} arguments",
                args.len()
            )))
        }
        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{call_pos}: record constructor expects {} arguments, got {}",
                    field_names.len(),
                    args.len()
                )));
            }
            let fields: Vec<(String, Value)> = field_names
                .iter()
                .zip(args.iter())
                .map(|(n, v)| (n.clone(), v.clone()))
                .collect();
            Ok(Value::Record {
                type_id: *type_id,
                type_name: type_name.clone(),
                fields,
            })
        }
        Value::RecordPredicate { type_id } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: predicate expects 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
        }
        Value::RecordAccessor { type_id, field_name } => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: accessor expects 1 argument")));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if tid == type_id => {
                    fields
                        .iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, v)| v.clone())
                        .ok_or_else(|| EvalError::Type(format!("{call_pos}: no field {field_name}")))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: expected record, got {}", args[0]))),
            }
        }
        _ => Err(EvalError::Type(format!("{call_pos}: not a procedure: {func}"))),
    }
}

fn eval_letrec(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: letrec requires bindings and body")));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: letrec: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    // First, bind all variables to Void
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    names.push(s.clone());
                    init_exprs.push(&pair[1]);
                    env_set(&local_env, s.clone(), Value::Void);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: letrec: expected symbol")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: letrec: invalid binding"))),
        }
    }
    // Evaluate init expressions in the local env and assign
    for (name, init_expr) in names.iter().zip(init_exprs.iter()) {
        let val = eval(init_expr, &local_env, output)?;
        env_set(&local_env, name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: letrec* requires bindings and body")));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: letrec*: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    let val = eval(&pair[1], &local_env, output)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: letrec*: expected symbol")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: letrec*: invalid binding"))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_let_star(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: let* requires bindings and body")));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: let*: expected bindings list"))),
    };
    let local_env = new_env(Some(env.clone()));
    for b in bindings_expr {
        match b {
            Expr::List(pair, _) if pair.len() == 2 => {
                if let Expr::Symbol(s, _) = &pair[0] {
                    let val = eval(&pair[1], &local_env, output)?;
                    env_set(&local_env, s.clone(), val);
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: let*: expected symbol")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: let*: invalid binding"))),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_seq(exprs: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn case_clause_matches(key: &Value, parts: &[Expr]) -> bool {
    if let Expr::List(datums, _) = &parts[0] {
        datums.iter().any(|d| values_eqv(key, &expr_to_value(d)))
    } else {
        false
    }
}

fn eval_case(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity(format!("{call_pos}: case requires key and clauses")));
    }
    let key = eval(&args[0], env, output)?;
    for clause in &args[1..] {
        let Expr::List(parts, _) = clause else { continue };
        if parts.is_empty() { continue; }
        // else clause
        if matches!(&parts[0], Expr::Symbol(s, _) if s == "else") {
            return eval_seq(&parts[1..], env, output);
        }
        // datum clause
        if case_clause_matches(&key, parts) {
            return eval_seq(&parts[1..], env, output);
        }
    }
    Ok(Value::Void)
}

fn eval_do(args: &[Expr], call_pos: Pos, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity(format!("{call_pos}: do requires bindings and test")));
    }
    let bindings_expr = match &args[0] {
        Expr::List(b, _) => b,
        _ => return Err(EvalError::Parse(format!("{call_pos}: do: expected bindings list"))),
    };
    let test_clause = match &args[1] {
        Expr::List(parts, _) if !parts.is_empty() => parts,
        _ => return Err(EvalError::Parse(format!("{call_pos}: do: expected test clause"))),
    };
    let body = &args[2..];

    // Parse bindings: (var init step?)
    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<&Expr>> = Vec::new();
    let local_env = new_env(Some(env.clone()));

    for b in bindings_expr {
        match b {
            Expr::List(parts, _) if parts.len() >= 2 => {
                if let Expr::Symbol(name, _) = &parts[0] {
                    let init = eval(&parts[1], env, output)?;
                    env_set(&local_env, name.clone(), init);
                    var_names.push(name.clone());
                    step_exprs.push(if parts.len() >= 3 { Some(&parts[2]) } else { None });
                } else {
                    return Err(EvalError::Parse(format!("{call_pos}: do: expected variable name")));
                }
            }
            _ => return Err(EvalError::Parse(format!("{call_pos}: do: invalid binding"))),
        }
    }

    loop {
        // Test
        let test_val = eval(&test_clause[0], &local_env, output)?;
        if is_truthy(&test_val) {
            // Evaluate result expressions
            let mut result = Value::Void;
            for expr in &test_clause[1..] {
                result = eval(expr, &local_env, output)?;
            }
            return Ok(result);
        }
        // Evaluate body
        for expr in body {
            eval(expr, &local_env, output)?;
        }
        // Parallel step: evaluate all step expressions using current values
        let mut new_vals = Vec::new();
        for step in step_exprs.iter() {
            if let Some(step_expr) = step {
                new_vals.push(Some(eval(step_expr, &local_env, output)?));
            } else {
                new_vals.push(None);
            }
        }
        // Update variables
        for (i, name) in var_names.iter().enumerate() {
            if let Some(val) = new_vals[i].take() {
                env_set(&local_env, name.clone(), val);
            }
        }
    }
}

pub(super) fn values_eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        _ => false,
    }
}

pub(super) fn values_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => std::ptr::eq(a.as_str(), b.as_str()),
        (Value::List(a), Value::List(b)) => a.is_empty() && b.is_empty(),
        (Value::Vector(a), Value::Vector(b)) => Rc::ptr_eq(a, b),
        (Value::Void, Value::Void) => true,
        _ => false,
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => a == b,
        (Value::Rational(an, ad), Value::Rational(bn, bd)) => an == bn && ad == bd,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        (Value::Pair(a1, a2), Value::Pair(b1, b2)) => {
            values_equal(a1, b1) && values_equal(a2, b2)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| values_equal(x, y))
        }
        _ => false,
    }
}

fn is_proper_list(v: &Value) -> bool {
    match v {
        Value::List(_) => true,
        Value::Pair(_, d) => is_proper_list(d),
        _ => false,
    }
}

// ---------- Macros (syntax-rules) ----------

static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(super) const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "case-lambda", "let", "begin", "cond", "and", "or",
    "set!", "string-set!", "not", "define-syntax", "syntax-rules",
    "letrec", "letrec*", "case", "do", "let*", "when", "unless",
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
    }
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
    let mut result = Value::Void;
    for expr in &exprs {
        result = eval(expr, &env, &mut output)?;
    }
    Ok((result.to_string(), output))
}

#[cfg(test)]
mod tests;
