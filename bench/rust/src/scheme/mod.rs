pub mod error;
mod builtins;
mod macros;
mod parser;

pub use error::EvalError;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

// ---- AST (parsed code with source positions) ----

#[derive(Debug, Clone)]
pub(crate) struct Ast {
    pub(crate) kind: AstKind,
    pub(crate) line: usize,
    pub(crate) col: usize,
}

#[derive(Debug, Clone)]
pub(crate) enum AstKind {
    Integer(i64),
    Rational(i64, i64),
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Ast>),
}

// ---- Runtime values ----

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Rational(i64, i64), // numerator, denominator (always simplified, denom > 0)
    Float(f64),
    Boolean(bool),
    Str(String),
    Char(char),
    List(Vec<Value>),
    Symbol(String),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Ast>,
        env: Env,
    },
    Pair(Rc<RefCell<(Value, Value)>>),
    Builtin(fn(&[Value], &mut String) -> Result<Value, EvalError>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Ast, Ast)>,
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
        type_name: String,
        field_name: String,
        field_index: usize,
    },
    CaseLambda {
        clauses: Vec<(Vec<String>, Option<String>, Vec<Ast>)>,
        env: Env,
    },
    Vector(Rc<RefCell<Vec<Value>>>),
    Void,
}

pub(crate) fn gcd(a: i64, b: i64) -> i64 {
    let (mut a, mut b) = (a.abs(), b.abs());
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Create a rational or integer value, always simplified.
pub(crate) fn make_rational(n: i64, d: i64) -> Value {
    assert!(d != 0, "division by zero in make_rational");
    let sign = if (n < 0) ^ (d < 0) { -1 } else { 1 };
    let n = n.abs();
    let d = d.abs();
    let g = gcd(n, d);
    let n = sign * (n / g);
    let d = d / g;
    if d == 1 { Value::Integer(n) } else { Value::Rational(n, d) }
}

pub(crate) fn make_pair(car: Value, cdr: Value) -> Value {
    Value::Pair(Rc::new(RefCell::new((car, cdr))))
}

pub(crate) fn vec_to_list(items: Vec<Value>) -> Value {
    let mut result = Value::List(vec![]);
    for item in items.into_iter().rev() {
        result = make_pair(item, result);
    }
    result
}

pub(crate) fn value_to_vec(val: &Value) -> Option<Vec<Value>> {
    match val {
        Value::List(items) => Some(items.clone()),
        Value::Pair(_) => {
            let mut result = Vec::new();
            let mut current = val.clone();
            let mut limit = 10_000_000usize;
            loop {
                match current {
                    Value::List(ref items) if items.is_empty() => return Some(result),
                    Value::List(ref items) => {
                        result.extend(items.iter().cloned());
                        return Some(result);
                    }
                    Value::Pair(ref p) => {
                        limit = limit.checked_sub(1)?;
                        let (car, cdr) = {
                            let b = p.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        result.push(car);
                        current = cdr;
                    }
                    _ => return None,
                }
            }
        }
        _ => None,
    }
}

pub(crate) type Env = Rc<RefCell<Environment>>;

#[derive(Debug)]
pub(crate) struct Environment {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Environment {
    pub(crate) fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub(crate) fn with_parent(parent: &Env) -> Env {
        Rc::new(RefCell::new(Environment {
            bindings: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        }))
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub(crate) fn set(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    fn set_existing(&mut self, name: &str, val: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), val);
            true
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().set_existing(name, val)
        } else {
            false
        }
    }
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub(crate) fn as_integer(&self) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    /// Convert any numeric value to f64 for comparison.
    pub(crate) fn as_f64(&self) -> Result<f64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n as f64),
            Value::Rational(n, d) => Ok(*n as f64 / *d as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(EvalError::Type(format!("expected number, got {}", self.display_value()))),
        }
    }

    pub(crate) fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _) | Value::Float(_))
    }

    pub(crate) fn is_exact(&self) -> bool {
        matches!(self, Value::Integer(_) | Value::Rational(_, _))
    }

    pub(crate) fn display_value(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Rational(n, d) => format!("{}/{}", n, d),
            Value::Float(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{:.1}", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::Str(s) => format!("\"{s}\""),
            Value::Char(c) => match c {
                ' ' => "#\\space".into(),
                '\n' => "#\\newline".into(),
                '\t' => "#\\tab".into(),
                _ => format!("#\\{c}"),
            },
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => display_pair(self, false),
            Value::Lambda { .. } | Value::Builtin(_)
            | Value::RecordConstructor { .. } | Value::RecordPredicate { .. }
            | Value::RecordAccessor { .. }
            | Value::CaseLambda { .. } => "#<procedure>".into(),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display_value()).collect();
                format!("#({})", inner.join(" "))
            }
            Value::Macro { .. } => "#<macro>".into(),
            Value::Record { type_name, fields, .. } => {
                let inner: Vec<String> = fields.iter().map(|(k, v)| format!("{}: {}", k, v.display_value())).collect();
                format!("#<{} {}>", type_name, inner.join(", "))
            }
            Value::Void => "".into(),
        }
    }

    /// Format for `display` — no quotes on strings, no #\ on chars.
    pub(crate) fn display_repr(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            Value::Char(c) => c.to_string(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Pair(_) => display_pair(self, true),
            Value::Vector(v) => {
                let items = v.borrow();
                let inner: Vec<String> = items.iter().map(|v| v.display_repr()).collect();
                format!("#({})", inner.join(" "))
            }
            _ => self.display_value(),
        }
    }
}

fn display_pair(val: &Value, use_repr: bool) -> String {
    let mut parts = Vec::new();
    let mut current = val.clone();
    let mut seen = std::collections::HashSet::new();
    let mut improper_tail = None;
    loop {
        match current {
            Value::List(ref items) if items.is_empty() => break,
            Value::List(ref items) => {
                // Non-empty List in cdr position: extend with its elements
                for item in items {
                    parts.push(if use_repr { item.display_repr() } else { item.display_value() });
                }
                break;
            }
            Value::Pair(ref p) => {
                let addr = Rc::as_ptr(p) as usize;
                if !seen.insert(addr) {
                    break; // cycle detected
                }
                let (car, cdr) = {
                    let b = p.borrow();
                    (b.0.clone(), b.1.clone())
                };
                parts.push(if use_repr { car.display_repr() } else { car.display_value() });
                current = cdr;
            }
            other => {
                improper_tail = Some(if use_repr { other.display_repr() } else { other.display_value() });
                break;
            }
        }
    }
    if let Some(tail) = improper_tail {
        format!("({} . {})", parts.join(" "), tail)
    } else {
        format!("({})", parts.join(" "))
    }
}

/// Convert an AST node to a runtime Value (for quote).
fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
        AstKind::Rational(n, d) => Value::Rational(*n, *d),
        AstKind::Float(f) => Value::Float(*f),
        AstKind::Boolean(b) => Value::Boolean(*b),
        AstKind::Str(s) => Value::Str(s.clone()),
        AstKind::Char(c) => Value::Char(*c),
        AstKind::Symbol(s) => Value::Symbol(s.clone()),
        AstKind::List(items) => Value::List(items.iter().map(ast_to_value).collect()),
    }
}

use parser::Parser;

// ---- Evaluator ----

/// Evaluate an AST node, wrapping any error with source position.
pub(crate) fn eval(ast: &Ast, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    eval_inner(ast, env, output).map_err(|e| match e {
        EvalError::WithPosition(_, _, _) => e,
        _ => EvalError::WithPosition(Box::new(e), ast.line, ast.col),
    })
}

fn eval_inner(ast: &Ast, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut cur_ast = ast.clone();
    let mut cur_env = Rc::clone(env);

    loop {
        match &cur_ast.kind {
            AstKind::Integer(n) => return Ok(Value::Integer(*n)),
            AstKind::Rational(n, d) => return Ok(Value::Rational(*n, *d)),
            AstKind::Float(f) => return Ok(Value::Float(*f)),
            AstKind::Boolean(b) => return Ok(Value::Boolean(*b)),
            AstKind::Str(s) => return Ok(Value::Str(s.clone())),
            AstKind::Char(c) => return Ok(Value::Char(*c)),
            AstKind::Symbol(name) => {
                return cur_env.borrow().get(name).ok_or_else(|| EvalError::UnboundVariable(name.clone()));
            }
            AstKind::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::Parse("empty application".into()));
                }
                // Check for special forms
                if let AstKind::Symbol(op) = &items[0].kind {
                    match op.as_str() {
                        "define" => return eval_define(&items[1..], &cur_env, output),
                        "if" => {
                            let args = &items[1..];
                            if args.len() < 2 || args.len() > 3 {
                                return Err(EvalError::Arity("if requires 2 or 3 arguments".into()));
                            }
                            let cond = eval(&args[0], &cur_env, output)?;
                            if cond.is_truthy() {
                                cur_ast = args[1].clone();
                            } else if args.len() == 3 {
                                cur_ast = args[2].clone();
                            } else {
                                return Ok(Value::Void);
                            }
                            continue;
                        }
                        "quote" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity("quote requires exactly 1 argument".into()));
                            }
                            return Ok(ast_to_value(&items[1]));
                        }
                        "lambda" => return eval_lambda(&items[1..], &cur_env),
                        "let" => {
                            let args = &items[1..];
                            if args.len() < 2 {
                                return Err(EvalError::Arity("let requires bindings and body".into()));
                            }
                            let (tail, new_env) = if let AstKind::Symbol(name) = &args[0].kind {
                                eval_named_let(name, args, &cur_env, output)?
                            } else {
                                eval_regular_let(args, &cur_env, output)?
                            };
                            cur_ast = tail;
                            cur_env = new_env;
                            continue;
                        }
                        "begin" => {
                            if items.len() == 1 {
                                return Ok(Value::Void);
                            }
                            for expr in &items[1..items.len()-1] {
                                eval(expr, &cur_env, output)?;
                            }
                            cur_ast = items.last().expect("begin has at least one form").clone();
                            continue;
                        }
                        "cond" => {
                            match eval_cond_tail(&items[1..], &cur_env, output)? {
                                CondResult::TailExpr(tail) => {
                                    cur_ast = tail;
                                    continue;
                                }
                                CondResult::DirectValue(val) => return Ok(val),
                                CondResult::NoMatch => return Ok(Value::Void),
                            }
                        }
                        "and" => {
                            if items.len() == 1 {
                                return Ok(Value::Boolean(true));
                            }
                            if let Some(falsy) = eval_and_short_circuit(&items[1..items.len()-1], &cur_env, output)? {
                                return Ok(falsy);
                            }
                            cur_ast = items.last().expect("and has at least one form").clone();
                            continue;
                        }
                        "set!" => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity("set! requires exactly 2 arguments".into()));
                            }
                            let var_name = match &items[1].kind {
                                AstKind::Symbol(name) => name.clone(),
                                _ => return Err(EvalError::Type("set!: first argument must be a symbol".into())),
                            };
                            let val = eval(&items[2], &cur_env, output)?;
                            if !cur_env.borrow_mut().set_existing(&var_name, val) {
                                return Err(EvalError::UnboundVariable(var_name));
                            }
                            return Ok(Value::Void);
                        }
                        "string-set!" => return eval_string_set(&items[1..], &cur_env, output),
                        "or" => {
                            if items.len() == 1 {
                                return Ok(Value::Boolean(false));
                            }
                            if let Some(truthy) = eval_or_short_circuit(&items[1..items.len()-1], &cur_env, output)? {
                                return Ok(truthy);
                            }
                            cur_ast = items.last().expect("or has at least one form").clone();
                            continue;
                        }
                        "define-syntax" => return eval_define_syntax(&items[1..], &cur_env),
                        "define-record-type" => return eval_define_record_type(&items[1..], &cur_env),
                        "case-lambda" => return eval_case_lambda(&items[1..], &cur_env),
                        "letrec" => return eval_letrec(&items[1..], &cur_env, output),
                        "letrec*" => return eval_letrec_star(&items[1..], &cur_env, output),
                        "case" => return eval_case(&items[1..], &cur_env, output),
                        "do" => return eval_do(&items[1..], &cur_env, output),
                        "let*" => return eval_let_star(&items[1..], &cur_env, output),
                        "when" => return eval_when(&items[1..], &cur_env, output),
                        "unless" => return eval_unless(&items[1..], &cur_env, output),
                        _ => {
                            // Check for macro invocation
                            let maybe_macro = cur_env.borrow().get(op);
                            if let Some(Value::Macro { literals, rules, def_env }) = maybe_macro {
                                return expand_and_eval_macro(&literals, &rules, &def_env, items, &cur_env, output);
                            }
                        }
                    }
                }
                // Function application with TCO
                let func = eval(&items[0], &cur_env, output)?;
                let args: Vec<Value> = items[1..].iter().map(|a| eval(a, &cur_env, output)).collect::<Result<_, _>>()?;
                match func {
                    Value::Lambda { params, rest_param, body, env: lambda_env } => {
                        if rest_param.is_some() {
                            if args.len() < params.len() {
                                return Err(EvalError::Arity(format!(
                                    "expected at least {} arguments, got {}", params.len(), args.len()
                                )));
                            }
                        } else if args.len() != params.len() {
                            return Err(EvalError::Arity(format!(
                                "expected {} arguments, got {}", params.len(), args.len()
                            )));
                        }
                        let local_env = Environment::with_parent(&lambda_env);
                        for (p, a) in params.iter().zip(args.iter()) {
                            local_env.borrow_mut().set(p.clone(), a.clone());
                        }
                        if let Some(rest) = &rest_param {
                            let rest_args = args[params.len()..].to_vec();
                            local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
                        }
                        if body.is_empty() {
                            return Ok(Value::Void);
                        }
                        for expr in &body[..body.len()-1] {
                            eval(expr, &local_env, output)?;
                        }
                        cur_ast = body.last().expect("lambda body is non-empty").clone();
                        cur_env = local_env;
                        continue;
                    }
                    Value::CaseLambda { clauses, env: cl_env } => {
                        let matched = clauses.into_iter().find(|(params, rest_param, _body)| {
                            if rest_param.is_some() {
                                args.len() >= params.len()
                            } else {
                                args.len() == params.len()
                            }
                        });
                        if let Some((params, rest_param, body)) = matched {
                            let local_env = Environment::with_parent(&cl_env);
                            for (p, a) in params.iter().zip(args.iter()) {
                                local_env.borrow_mut().set(p.clone(), a.clone());
                            }
                            if let Some(rest) = &rest_param {
                                let rest_args = args[params.len()..].to_vec();
                                local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
                            }
                            if body.is_empty() {
                                return Ok(Value::Void);
                            }
                            for expr in &body[..body.len()-1] {
                                eval(expr, &local_env, output)?;
                            }
                            cur_ast = body.last().expect("case-lambda body is non-empty").clone();
                            cur_env = local_env;
                            continue;
                        }
                        return Err(EvalError::Arity(format!(
                            "case-lambda: no matching clause for {} arguments", args.len()
                        )));
                    }
                    _ => return apply(&func, &args, output),
                }
            }
        }
    }
}

/// Evaluate named let: (let name ((var init) ...) body...)
/// Returns the tail expression and environment for TCO.
fn eval_named_let(
    name: &str,
    args: &[Ast],
    env: &Env,
    output: &mut String,
) -> Result<(Ast, Env), EvalError> {
    if args.len() < 3 {
        return Err(EvalError::Arity("named let requires bindings and body".into()));
    }
    let bindings_list = match &args[1].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                match &pair[0].kind {
                    AstKind::Symbol(s) => params.push(s.clone()),
                    _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                }
                inits.push(eval(&pair[1], env, output)?);
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    let body = args[2..].to_vec();
    let local_env = Environment::with_parent(env);
    let lambda = Value::Lambda {
        params: params.clone(),
        rest_param: None,
        body: body.clone(),
        env: Rc::clone(&local_env),
    };
    local_env.borrow_mut().set(name.to_string(), lambda);
    for (p, v) in params.iter().zip(inits.iter()) {
        local_env.borrow_mut().set(p.clone(), v.clone());
    }
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env, output)?;
    }
    let tail = body.last().expect("named let body is non-empty").clone();
    Ok((tail, local_env))
}

/// Evaluate regular let: (let ((var init) ...) body...)
/// Returns the tail expression and environment for TCO.
fn eval_regular_let(
    args: &[Ast],
    env: &Env,
    output: &mut String,
) -> Result<(Ast, Env), EvalError> {
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let: expected bindings list".into())),
    };
    let local_env = Environment::with_parent(env);
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let bname = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], env, output)?;
                local_env.borrow_mut().set(bname, val);
            }
            _ => return Err(EvalError::Type("let: invalid binding".into())),
        }
    }
    let body = &args[1..];
    for expr in &body[..body.len() - 1] {
        eval(expr, &local_env, output)?;
    }
    let tail = body.last().expect("let body is non-empty").clone();
    Ok((tail, local_env))
}

/// Evaluate cond clauses, returning the tail expression for TCO if a clause matches.
enum CondResult {
    NoMatch,
    TailExpr(Ast),
    DirectValue(Value),
}

fn eval_cond_tail(
    clauses: &[Ast],
    env: &Env,
    output: &mut String,
) -> Result<CondResult, EvalError> {
    for clause in clauses {
        let citems = match &clause.kind {
            AstKind::List(citems) if !citems.is_empty() => citems,
            _ => return Err(EvalError::Type("cond: invalid clause".into())),
        };
        let is_else = matches!(&citems[0].kind, AstKind::Symbol(s) if s == "else");
        if is_else {
            if citems.len() < 2 {
                return Err(EvalError::Type("cond: else clause needs a body".into()));
            }
            for expr in &citems[1..citems.len() - 1] {
                eval(expr, env, output)?;
            }
            return Ok(CondResult::TailExpr(citems.last().expect("else clause non-empty").clone()));
        }
        let test_val = eval(&citems[0], env, output)?;
        if test_val.is_truthy() {
            if citems.len() == 1 {
                return Ok(CondResult::DirectValue(test_val));
            }
            for expr in &citems[1..citems.len() - 1] {
                eval(expr, env, output)?;
            }
            return Ok(CondResult::TailExpr(citems.last().expect("cond clause non-empty").clone()));
        }
    }
    Ok(CondResult::NoMatch)
}

/// Evaluate short-circuit `and` arguments (all but the last).
/// Returns Some(value) if a falsy value was found, None if all were truthy.
fn eval_and_short_circuit(
    args: &[Ast],
    env: &Env,
    output: &mut String,
) -> Result<Option<Value>, EvalError> {
    for a in args {
        let result = eval(a, env, output)?;
        if !result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

/// Evaluate short-circuit `or` arguments (all but the last).
/// Returns Some(value) if a truthy value was found, None if all were falsy.
fn eval_or_short_circuit(
    args: &[Ast],
    env: &Env,
    output: &mut String,
) -> Result<Option<Value>, EvalError> {
    for a in args {
        let result = eval(a, env, output)?;
        if result.is_truthy() {
            return Ok(Some(result));
        }
    }
    Ok(None)
}

fn eval_string_set(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity("string-set! requires 3 arguments".into()));
    }
    let var_name = match &args[0].kind {
        AstKind::Symbol(name) => name.clone(),
        _ => return Err(EvalError::Type("string-set!: strings are immutable".into())),
    };
    let idx = eval(&args[1], env, output)?.as_integer()? as usize;
    let ch = match eval(&args[2], env, output)? {
        Value::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };
    let current = env.borrow().get(&var_name)
        .ok_or_else(|| EvalError::UnboundVariable(var_name.clone()))?;
    let s = match current {
        Value::Str(ref s) => s.clone(),
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Type("string-set!: index out of bounds".into()));
    }
    chars[idx] = ch;
    let new_s: String = chars.into_iter().collect();
    env.borrow_mut().set_existing(&var_name, Value::Str(new_s));
    Ok(Value::Void)
}

fn eval_define(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("define requires at least 2 arguments".into()));
    }
    match &args[0].kind {
        // (define x expr)
        AstKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity("define requires exactly 2 arguments".into()));
            }
            let val = eval(&args[1], env, output)?;
            env.borrow_mut().set(name.clone(), val);
            Ok(Value::Void)
        }
        // (define (f params...) body...)
        AstKind::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse("define: empty signature".into()));
            }
            let name = match &sig[0].kind {
                AstKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define: expected symbol for function name".into())),
            };
            let (params, rest_param) = parse_params(&sig[1..])?;
            let body = args[1..].to_vec();
            let lambda = Value::Lambda {
                params,
                rest_param,
                body,
                env: Rc::clone(env),
            };
            env.borrow_mut().set(name, lambda);
            Ok(Value::Void)
        }
        AstKind::Integer(_) | AstKind::Rational(_, _) | AstKind::Float(_) | AstKind::Boolean(_) | AstKind::Str(_) | AstKind::Char(_) => {
            Err(EvalError::Type("define: expected symbol or list".into()))
        }
    }
}


fn eval_lambda(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("lambda requires at least 2 arguments".into()));
    }
    let (params, rest_param) = match &args[0].kind {
        AstKind::List(ps) => parse_params(ps)?,
        AstKind::Symbol(s) => (vec![], Some(s.clone())),
        _ => return Err(EvalError::Type("lambda: expected parameter list".into())),
    };
    let body = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        rest_param,
        body,
        env: Rc::clone(env),
    })
}

fn eval_case_lambda(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    let mut clauses = Vec::new();
    for clause in args {
        match &clause.kind {
            AstKind::List(items) if items.len() >= 2 => {
                let (params, rest_param) = match &items[0].kind {
                    AstKind::List(ps) => parse_params(ps)?,
                    AstKind::Symbol(s) => (vec![], Some(s.clone())),
                    _ => return Err(EvalError::Type("case-lambda: expected parameter list".into())),
                };
                let body = items[1..].to_vec();
                clauses.push((params, rest_param, body));
            }
            _ => return Err(EvalError::Type("case-lambda: expected clause (params body ...)".into())),
        }
    }
    Ok(Value::CaseLambda {
        clauses,
        env: Rc::clone(env),
    })
}

/// Parse a parameter list, handling optional dot notation for rest params.
/// E.g. `[x, y, ., rest]` -> `(["x", "y"], Some("rest"))`
fn parse_params(items: &[Ast]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < items.len() {
        match &items[i].kind {
            AstKind::Symbol(s) if s == "." => {
                if i + 1 != items.len() - 1 {
                    return Err(EvalError::Parse("malformed dotted parameter list".into()));
                }
                rest_param = Some(match &items[i + 1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("expected symbol after dot in parameters".into())),
                });
                break;
            }
            AstKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Type("expected symbol for parameter".into())),
        }
        i += 1;
    }
    Ok((params, rest_param))
}


// ---- L14: letrec, letrec*, case, do, let*, when, unless ----

fn eval_letrec(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec: expected bindings list".into())),
    };
    let local_env = Environment::with_parent(env);
    // First pass: bind all names to Void (so they're visible to each other)
    let mut names = Vec::new();
    let mut init_exprs = Vec::new();
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("letrec: expected symbol in binding".into())),
                };
                local_env.borrow_mut().set(name.clone(), Value::Void);
                names.push(name);
                init_exprs.push(&pair[1]);
            }
            _ => return Err(EvalError::Type("letrec: invalid binding".into())),
        }
    }
    // Second pass: evaluate inits in the local_env and update bindings
    for (name, init_expr) in names.iter().zip(init_exprs.iter()) {
        let val = eval(init_expr, &local_env, output)?;
        local_env.borrow_mut().set(name.clone(), val);
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_letrec_star(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("letrec* requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("letrec*: expected bindings list".into())),
    };
    let local_env = Environment::with_parent(env);
    // Bind all names to Void first
    for binding in bindings_list {
        if let AstKind::List(pair) = &binding.kind {
            if let AstKind::Symbol(s) = &pair[0].kind {
                local_env.borrow_mut().set(s.clone(), Value::Void);
            }
        }
    }
    // Evaluate sequentially — each init can see previous bindings
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("letrec*: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], &local_env, output)?;
                local_env.borrow_mut().set(name, val);
            }
            _ => return Err(EvalError::Type("letrec*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

pub(crate) fn eqv(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Rational(xn, xd), Value::Rational(yn, yd)) => xn == yn && xd == yd,
        (Value::Float(x), Value::Float(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
        (Value::Pair(a), Value::Pair(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn eval_body(exprs: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;
    for expr in exprs {
        result = eval(expr, env, output)?;
    }
    Ok(result)
}

fn eval_case(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity("case requires at least a key expression".into()));
    }
    let key = eval(&args[0], env, output)?;
    for clause in &args[1..] {
        let items = match &clause.kind {
            AstKind::List(items) if items.len() >= 2 => items,
            _ => return Err(EvalError::Type("case: invalid clause".into())),
        };
        // Check for else clause
        if matches!(&items[0].kind, AstKind::Symbol(s) if s == "else") {
            return eval_body(&items[1..], env, output);
        }
        // Check datums list
        if let AstKind::List(datums) = &items[0].kind {
            let matched = datums.iter().any(|d| eqv(&key, &ast_to_value(d)));
            if matched {
                return eval_body(&items[1..], env, output);
            }
        }
    }
    Ok(Value::Void)
}

fn eval_do(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    // (do ((var init step) ...) (test expr ...) body ...)
    if args.len() < 2 {
        return Err(EvalError::Arity("do requires variable bindings and test clause".into()));
    }
    let var_specs = match &args[0].kind {
        AstKind::List(v) => v,
        _ => return Err(EvalError::Type("do: expected variable list".into())),
    };
    let test_clause = match &args[1].kind {
        AstKind::List(t) if !t.is_empty() => t,
        _ => return Err(EvalError::Type("do: expected test clause".into())),
    };
    let body = &args[2..];

    // Parse variable specs
    struct DoVar {
        name: String,
        step: Option<usize>, // index into var_specs for the step expression
    }
    let mut vars: Vec<DoVar> = Vec::new();
    let mut step_exprs: Vec<Option<Ast>> = Vec::new();

    let local_env = Environment::with_parent(env);

    for spec in var_specs {
        match &spec.kind {
            AstKind::List(parts) if parts.len() >= 2 => {
                let name = match &parts[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("do: expected symbol for variable".into())),
                };
                let init = eval(&parts[1], env, output)?;
                local_env.borrow_mut().set(name.clone(), init);
                let step = if parts.len() >= 3 { Some(parts[2].clone()) } else { None };
                vars.push(DoVar { name, step: if step.is_some() { Some(step_exprs.len()) } else { None } });
                step_exprs.push(step);
            }
            _ => return Err(EvalError::Type("do: invalid variable spec".into())),
        }
    }

    loop {
        // Test
        let test_val = eval(&test_clause[0], &local_env, output)?;
        if test_val.is_truthy() {
            // Evaluate result expressions
            if test_clause.len() > 1 {
                let mut result = Value::Void;
                for expr in &test_clause[1..] {
                    result = eval(expr, &local_env, output)?;
                }
                return Ok(result);
            }
            return Ok(Value::Void);
        }

        // Execute body
        for expr in body {
            eval(expr, &local_env, output)?;
        }

        // Parallel step: evaluate all steps with current values, then update
        let new_vals: Vec<Option<Result<Value, EvalError>>> = vars.iter().map(|v| {
            if let Some(idx) = v.step {
                step_exprs[idx].as_ref().map(|step_expr| eval(step_expr, &local_env, output))
            } else {
                None
            }
        }).collect();

        for (var, new_val) in vars.iter().zip(new_vals.into_iter()) {
            if let Some(result) = new_val {
                local_env.borrow_mut().set(var.name.clone(), result?);
            }
        }
    }
}

fn eval_let_star(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("let* requires bindings and body".into()));
    }
    let bindings_list = match &args[0].kind {
        AstKind::List(b) => b,
        _ => return Err(EvalError::Type("let*: expected bindings list".into())),
    };
    let local_env = Environment::with_parent(env);
    for binding in bindings_list {
        match &binding.kind {
            AstKind::List(pair) if pair.len() == 2 => {
                let name = match &pair[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("let*: expected symbol in binding".into())),
                };
                let val = eval(&pair[1], &local_env, output)?;
                local_env.borrow_mut().set(name, val);
            }
            _ => return Err(EvalError::Type("let*: invalid binding".into())),
        }
    }
    let mut result = Value::Void;
    for expr in &args[1..] {
        result = eval(expr, &local_env, output)?;
    }
    Ok(result)
}

fn eval_when(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("when requires a test and body".into()));
    }
    let test = eval(&args[0], env, output)?;
    if test.is_truthy() {
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, env, output)?;
        }
        Ok(result)
    } else {
        Ok(Value::Void)
    }
}

fn eval_unless(args: &[Ast], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity("unless requires a test and body".into()));
    }
    let test = eval(&args[0], env, output)?;
    if !test.is_truthy() {
        let mut result = Value::Void;
        for expr in &args[1..] {
            result = eval(expr, env, output)?;
        }
        Ok(result)
    } else {
        Ok(Value::Void)
    }
}

// ---- Macro Support (L10) — see macros.rs ----

use macros::{expand_and_eval_macro, eval_define_syntax};

static RECORD_TYPE_COUNTER: AtomicU64 = AtomicU64::new(0);

fn eval_define_record_type(args: &[Ast], env: &Env) -> Result<Value, EvalError> {
    // (define-record-type <name> (constructor field...) predicate (field accessor) ...)
    if args.len() < 3 {
        return Err(EvalError::Arity("define-record-type requires at least 3 arguments".into()));
    }
    let _type_name = match &args[0].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected type name symbol".into())),
    };
    let type_id = RECORD_TYPE_COUNTER.fetch_add(1, Ordering::Relaxed);

    // Parse constructor: (constructor-name field-name ...)
    let (constructor_name, constructor_fields) = match &args[1].kind {
        AstKind::List(items) if !items.is_empty() => {
            let cname = match &items[0].kind {
                AstKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Type("define-record-type: expected constructor name".into())),
            };
            let fields: Vec<String> = items[1..].iter().map(|a| match &a.kind {
                AstKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Type("define-record-type: expected field name in constructor".into())),
            }).collect::<Result<_, _>>()?;
            (cname, fields)
        }
        _ => return Err(EvalError::Type("define-record-type: expected constructor spec".into())),
    };

    // Parse predicate name
    let predicate_name = match &args[2].kind {
        AstKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Type("define-record-type: expected predicate name".into())),
    };

    // Parse field accessors: (field-name accessor-name) ...
    let mut field_accessors: Vec<(String, String)> = Vec::new();
    for arg in &args[3..] {
        match &arg.kind {
            AstKind::List(items) if items.len() == 2 => {
                let field = match &items[0].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: expected field name".into())),
                };
                let accessor = match &items[1].kind {
                    AstKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Type("define-record-type: expected accessor name".into())),
                };
                field_accessors.push((field, accessor));
            }
            _ => return Err(EvalError::Type("define-record-type: invalid field spec".into())),
        }
    }

    // Register constructor
    env.borrow_mut().set(constructor_name, Value::RecordConstructor {
        type_id,
        type_name: _type_name.clone(),
        field_names: constructor_fields.clone(),
    });

    // Register predicate
    env.borrow_mut().set(predicate_name, Value::RecordPredicate { type_id });

    // Register accessors
    for (field_name, accessor_name) in &field_accessors {
        let field_index = constructor_fields.iter().position(|f| f == field_name)
            .ok_or_else(|| EvalError::Type(format!(
                "define-record-type: field {} not in constructor", field_name
            )))?;
        env.borrow_mut().set(accessor_name.clone(), Value::RecordAccessor {
            type_id,
            type_name: _type_name.clone(),
            field_name: accessor_name.clone(),
            field_index,
        });
    }

    Ok(Value::Void)
}

fn apply(func: &Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match func {
        Value::Builtin(f) => f(args, output),
        Value::Lambda { params, rest_param, body, env } => {
            if rest_param.is_some() {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
            } else if args.len() != params.len() {
                return Err(EvalError::Arity(format!(
                    "expected {} arguments, got {}", params.len(), args.len()
                )));
            }
            let local_env = Environment::with_parent(env);
            for (p, a) in params.iter().zip(args.iter()) {
                local_env.borrow_mut().set(p.clone(), a.clone());
            }
            if let Some(rest) = rest_param {
                let rest_args = args[params.len()..].to_vec();
                local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
            }
            let mut result = Value::Void;
            for expr in body {
                result = eval(expr, &local_env, output)?;
            }
            Ok(result)
        }
        Value::CaseLambda { clauses, env } => {
            for (params, rest_param, body) in clauses {
                let matches = if rest_param.is_some() {
                    args.len() >= params.len()
                } else {
                    args.len() == params.len()
                };
                if matches {
                    let local_env = Environment::with_parent(env);
                    for (p, a) in params.iter().zip(args.iter()) {
                        local_env.borrow_mut().set(p.clone(), a.clone());
                    }
                    if let Some(rest) = rest_param {
                        let rest_args = args[params.len()..].to_vec();
                        local_env.borrow_mut().set(rest.clone(), vec_to_list(rest_args));
                    }
                    let mut result = Value::Void;
                    for expr in body {
                        result = eval(expr, &local_env, output)?;
                    }
                    return Ok(result);
                }
            }
            Err(EvalError::Arity(format!(
                "case-lambda: no matching clause for {} arguments", args.len()
            )))
        }
        Value::RecordConstructor { type_id, type_name, field_names } => {
            if args.len() != field_names.len() {
                return Err(EvalError::Arity(format!(
                    "{} constructor expects {} arguments, got {}", type_name, field_names.len(), args.len()
                )));
            }
            let fields: Vec<(String, Value)> = field_names.iter().zip(args.iter())
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
                return Err(EvalError::Arity("record predicate expects 1 argument".into()));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Record { type_id: tid, .. } if tid == type_id)))
        }
        Value::RecordAccessor { type_id, type_name, field_name, field_index } => {
            if args.len() != 1 {
                return Err(EvalError::Arity("record accessor expects 1 argument".into()));
            }
            match &args[0] {
                Value::Record { type_id: tid, fields, .. } if tid == type_id => {
                    Ok(fields[*field_index].1.clone())
                }
                _ => Err(EvalError::Type(format!(
                    "{}: expected {}", field_name, type_name
                ))),
            }
        }
        Value::Integer(_)
        | Value::Rational(_, _)
        | Value::Float(_)
        | Value::Boolean(_)
        | Value::Str(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Pair(_)
        | Value::Symbol(_)
        | Value::Macro { .. }
        | Value::Record { .. }
        | Value::Vector(_)
        | Value::Void => Err(EvalError::Type(format!("not a procedure: {}", func.display_value()))),
    }
}


/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &mut output)?;
    }
    Ok(last.display_value())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = builtins::make_global_env();
    let mut output = String::new();
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr, &env, &mut output)?;
    }
    Ok((last.display_value(), output))
}

#[cfg(test)]
mod tests;
