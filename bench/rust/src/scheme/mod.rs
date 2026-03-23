mod builtins;
pub mod error;
mod parser;

pub use error::EvalError;

use builtins::{call_builtin, is_builtin};
use parser::Parser;
use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    SchemeString(String),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(String),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::SchemeString(a), Value::SchemeString(b)) => a == b,
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
    parent: Option<Box<Env>>,
}

impl Env {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: &Env) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
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
}

// --- Evaluator ---

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(n) => Value::Integer(*n),
        Expr::Boolean(b) => Value::Boolean(*b),
        Expr::SchemeString(s) => Value::SchemeString(s.clone()),
        Expr::Symbol(s) => Value::SchemeString(s.clone()),
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
            let mut call_env = Env::with_parent(env);
            // Inject self-reference for recursion
            if let Some(fn_name) = name {
                call_env.define(fn_name.to_string(), callable.clone());
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                call_env.define(param.clone(), arg.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut call_env)?;
            }
            Ok(result)
        }
        Value::Builtin(bname) => call_builtin(bname, args),
        _ => Err(EvalError::Type {
            message: format!("not a procedure: {}", callable.display()),
        }),
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
