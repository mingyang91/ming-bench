mod builtins;
pub mod error;
mod parser;

pub use error::EvalError;

use builtins::{call_builtin, is_builtin};
use parser::Parser;
use std::collections::HashMap;
use std::rc::Rc;

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Rc<[Expr]>,
        env: Rc<Env>,
    },
    Builtin(String),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::SchemeString(a), Value::SchemeString(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Builtin(a), Value::Builtin(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub(crate) fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::SchemeString(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Builtin(name) => format!("#<builtin:{name}>"),
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    pub(crate) fn as_integer(&self, context: &str) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::Type {
                message: format!("{context}: expected number, got {}", other.display()),
            }),
        }
    }
}

/// A parsed S-expression.
#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    Symbol(String),
    List(Vec<Expr>),
}

// --- Environment ---

#[derive(Debug, Clone)]
pub(crate) struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Rc<Env>>,
}

impl Env {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    fn child(parent: Rc<Env>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    fn define(&mut self, name: String, val: Value) {
        self.bindings.insert(name, val);
    }

    fn into_rc(self) -> Rc<Env> {
        Rc::new(self)
    }
}

// --- Evaluator ---

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::SchemeString(s) => Value::SchemeString(s.clone()),
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(items) => {
            Value::List(items.iter().map(quote_expr).collect())
        }
    }
}

fn eval(expr: &Expr, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Boolean(b) => Ok(Value::Boolean(*b)),
        Expr::SchemeString(s) => Ok(Value::SchemeString(s.clone())),
        Expr::Symbol(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else if is_builtin(name) {
                Ok(Value::Builtin(name.clone()))
            } else {
                Err(EvalError::UnboundVariable {
                    name: name.clone(),
                })
            }
        }
        Expr::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".to_string(),
                });
            }
            eval_application(items, env)
        }
    }
}

fn eval_application(items: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let head = &items[0];
    let args_exprs = &items[1..];

    // Special forms
    if let Expr::Symbol(name) = head {
        match name.as_str() {
            "define" => return eval_define(args_exprs, env),
            "if" => return eval_if(args_exprs, env),
            "quote" => return eval_quote(args_exprs),
            "lambda" => return eval_lambda(args_exprs, env),
            "and" => return eval_and(args_exprs, env),
            "or" => return eval_or(args_exprs, env),
            "let" => return eval_let(args_exprs, env),
            "begin" => return eval_begin(args_exprs, env),
            "cond" => return eval_cond(args_exprs, env),
            _ => {}
        }
    }

    // Evaluate head to get callable
    let callable = eval(head, env)?;

    // Evaluate arguments
    let args: Vec<Value> = args_exprs
        .iter()
        .map(|e| eval(e, env))
        .collect::<Result<_, _>>()?;

    // For named calls, pass the name so recursion works
    let name = if let Expr::Symbol(s) = head {
        Some(s.as_str())
    } else {
        None
    };
    apply(&callable, &args, name)
}

fn apply(callable: &Value, args: &[Value], name: Option<&str>) -> Result<Value, EvalError> {
    match callable {
        Value::Lambda { params, body, env } => {
            if args.len() != params.len() {
                return Err(EvalError::Arity {
                    message: format!(
                        "lambda expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ),
                });
            }
            let mut call_env = Env::child(env.clone().into_rc());
            // Inject self-reference for recursion
            if let Some(fn_name) = name {
                call_env.define(fn_name.to_string(), callable.clone());
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }
            // Separate internal defines from body expressions
            let mut internal_defines = Vec::new();
            let mut body_exprs = Vec::new();
            for expr in body {
                if let Expr::List(items) = expr {
                    if let Some(Expr::Symbol(s)) = items.first() {
                        if s == "define" {
                            if let Some(def_name) = extract_define_name(items) {
                                internal_defines.push(def_name);
                            }
                            // Still add to body_exprs — we'll eval defines first
                        }
                    }
                }
                body_exprs.push(expr);
            }
            // Pre-define all internal names as placeholders
            for def_name in &internal_defines {
                call_env.define(def_name.clone(), Value::Boolean(false));
            }
            // Evaluate all define forms
            for expr in &body_exprs {
                if let Expr::List(items) = expr {
                    if let Some(Expr::Symbol(s)) = items.first() {
                        if s == "define" {
                            eval(expr, &mut call_env)?;
                        }
                    }
                }
            }
            // Patch all defined lambdas with the complete env
            if !internal_defines.is_empty() {
                for def_name in &internal_defines {
                    if let Some(Value::Lambda {
                        params,
                        body: lbody,
                        ..
                    }) = call_env.get(def_name)
                    {
                        let patched = Value::Lambda {
                            params,
                            body: lbody,
                            env: call_env.clone(),
                        };
                        call_env.define(def_name.clone(), patched);
                    }
                }
            }
            // Evaluate non-define body expressions
            let mut result = Value::Boolean(false);
            for expr in &body_exprs {
                let is_define = if let Expr::List(items) = expr {
                    matches!(items.first(), Some(Expr::Symbol(s)) if s == "define")
                } else {
                    false
                };
                if !is_define {
                    result = eval(expr, &mut call_env)?;
                }
            }
            Ok(result)
        }
        Value::Builtin(bname) => call_builtin(bname, args),
        _ => Err(EvalError::Type {
            message: format!("not a procedure: {}", callable.display()),
        }),
    }
}

fn extract_define_name(items: &[Expr]) -> Option<String> {
    // items[0] is "define"
    match items.get(1) {
        Some(Expr::Symbol(name)) => Some(name.clone()),
        Some(Expr::List(sig)) => {
            if let Some(Expr::Symbol(name)) = sig.first() {
                Some(name.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn eval_define(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "define requires at least 2 arguments".to_string(),
        });
    }
    match &args[0] {
        // (define x expr)
        Expr::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity {
                    message: "define requires exactly 2 arguments".to_string(),
                });
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Value::Boolean(false)) // define returns unspecified
        }
        // (define (f params...) body...)
        Expr::List(sig) => {
            if sig.is_empty() {
                return Err(EvalError::Parse {
                    message: "define: empty signature".to_string(),
                });
            }
            let fname = match &sig[0] {
                Expr::Symbol(s) => s.clone(),
                _ => {
                    return Err(EvalError::Type {
                        message: "define: expected symbol as function name".to_string(),
                    })
                }
            };
            let mut params = Vec::new();
            for p in &sig[1..] {
                match p {
                    Expr::Symbol(s) => params.push(s.clone()),
                    _ => {
                        return Err(EvalError::Type {
                            message: "define: expected symbol as parameter".to_string(),
                        })
                    }
                }
            }
            let body: Vec<Expr> = args[1..].to_vec();
            // First define a placeholder so env contains the name
            let placeholder = Value::Lambda {
                params: params.clone(),
                body: body.clone(),
                env: env.clone(),
            };
            env.define(fname.clone(), placeholder);
            // Now re-create with env that includes fname (enables recursion)
            let recursive_lambda = Value::Lambda {
                params,
                body,
                env: env.clone(),
            };
            env.define(fname, recursive_lambda);
            Ok(Value::Boolean(false))
        }
        _ => Err(EvalError::Type {
            message: "define: expected symbol or list".to_string(),
        }),
    }
}

fn eval_if(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity {
            message: "if requires 2 or 3 arguments".to_string(),
        });
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Value::Boolean(false))
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            message: "quote requires exactly 1 argument".to_string(),
        });
    }
    Ok(quote_expr(&args[0]))
}

fn eval_lambda(args: &[Expr], env: &Env) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::Arity {
            message: "lambda requires at least 2 arguments".to_string(),
        });
    }
    let params = match &args[0] {
        Expr::List(param_exprs) => {
            let mut params = Vec::new();
            for p in param_exprs {
                match p {
                    Expr::Symbol(s) => params.push(s.clone()),
                    _ => {
                        return Err(EvalError::Type {
                            message: "lambda: expected symbol as parameter".to_string(),
                        })
                    }
                }
            }
            params
        }
        _ => {
            return Err(EvalError::Type {
                message: "lambda: expected parameter list".to_string(),
            })
        }
    };
    let body: Vec<Expr> = args[1..].to_vec();
    Ok(Value::Lambda {
        params,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if exprs.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for expr in exprs {
        result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Arity {
            message: "let requires at least 2 arguments".to_string(),
        });
    }
    // Named let: (let name ((var init) ...) body...)
    if let Expr::Symbol(name) = &args[0] {
        if args.len() < 3 {
            return Err(EvalError::Arity {
                message: "named let requires bindings and body".to_string(),
            });
        }
        let bindings_expr = match &args[1] {
            Expr::List(b) => b,
            _ => {
                return Err(EvalError::Type {
                    message: "let: expected bindings list".to_string(),
                })
            }
        };
        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for b in bindings_expr {
            match b {
                Expr::List(pair) if pair.len() == 2 => {
                    match &pair[0] {
                        Expr::Symbol(s) => params.push(s.clone()),
                        _ => {
                            return Err(EvalError::Type {
                                message: "let: expected symbol in binding".to_string(),
                            })
                        }
                    }
                    init_vals.push(eval(&pair[1], env)?);
                }
                _ => {
                    return Err(EvalError::Type {
                        message: "let: malformed binding".to_string(),
                    })
                }
            }
        }
        let body: Vec<Expr> = args[2..].to_vec();
        let lambda = Value::Lambda {
            params: params.clone(),
            body,
            env: env.clone(),
        };
        // Create env with the named function bound to itself
        let mut call_env = Env::child(env.clone().into_rc());
        call_env.define(name.clone(), lambda.clone());
        // Re-create lambda with env that contains self-reference
        let recursive_lambda = Value::Lambda {
            params,
            body: args[2..].to_vec(),
            env: call_env.clone(),
        };
        call_env.define(name.clone(), recursive_lambda.clone());
        // Apply with init values
        return apply(&recursive_lambda, &init_vals, Some(name));
    }
    // Regular let: (let ((var init) ...) body...)
    let bindings_expr = match &args[0] {
        Expr::List(b) => b,
        _ => {
            return Err(EvalError::Type {
                message: "let: expected bindings list".to_string(),
            })
        }
    };
    let mut let_env = Env::child(env.clone().into_rc());
    for b in bindings_expr {
        match b {
            Expr::List(pair) if pair.len() == 2 => {
                let name = match &pair[0] {
                    Expr::Symbol(s) => s.clone(),
                    _ => {
                        return Err(EvalError::Type {
                            message: "let: expected symbol in binding".to_string(),
                        })
                    }
                };
                let val = eval(&pair[1], env)?;
                let_env.define(name, val);
            }
            _ => {
                return Err(EvalError::Type {
                    message: "let: malformed binding".to_string(),
                })
            }
        }
    }
    let mut result = Value::Boolean(false);
    for expr in &args[1..] {
        result = eval(expr, &mut let_env)?;
    }
    Ok(result)
}

fn eval_begin(args: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_cond(clauses: &[Expr], env: &mut Env) -> Result<Value, EvalError> {
    for clause in clauses {
        match clause {
            Expr::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let Expr::Symbol(s) = &parts[0] {
                    if s == "else" {
                        let mut result = Value::Boolean(false);
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    if parts.len() == 1 {
                        return Ok(test);
                    }
                    let mut result = Value::Boolean(false);
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => {
                return Err(EvalError::Type {
                    message: "cond: malformed clause".to_string(),
                })
            }
        }
    }
    Ok(Value::Boolean(false))
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
        });
    }
    let mut env = Env::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
