mod builtins;
pub mod error;
mod forms;
mod parser;

pub use error::EvalError;

use builtins::{
    builtin_add, builtin_append, builtin_car, builtin_cdr, builtin_cmp,
    builtin_cons, builtin_div, builtin_length, builtin_list, builtin_mul, builtin_not,
    builtin_null, builtin_string_ops, builtin_sub, builtin_type_pred,
};
use forms::{apply_lambda, eval_and, eval_define, eval_lambda, eval_or, eval_string_set};
use parser::{parse_all, Span};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type Env = HashMap<String, Rc<RefCell<Value>>>;

/// A Scheme value.
#[derive(Debug, Clone)]
pub(crate) enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Char(char),
    Symbol(String, Span),
    List(Vec<Value>, Span),
    Void,
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Value>,
        closure_env: Env,
    },
}

/// Result of a form evaluation that may be a tail expression.
pub(crate) enum Tail {
    Done(Value),
    Expr(Value),
    Call { expr: Value, env: Env },
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self, context: &str) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                message: format!("{context}: expected number, got {}", other.type_name()),
                line: 0,
                col: 0,
            }),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Str(_) => "string",
            Value::Char(_) => "character",
            Value::Symbol(..) => "symbol",
            Value::List(..) => "list",
            Value::Void => "void",
            Value::Lambda { .. } => "procedure",
        }
    }

    /// `write`-style representation (strings get quotes).
    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("#\\{c}"),
            Value::Symbol(s, _) => s.clone(),
            Value::List(items, _) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "#<void>".to_string(),
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }

    /// `display`-style representation (strings without quotes).
    fn display_repr(&self) -> String {
        match self {
            Value::Str(s) => s.clone(),
            other => other.display(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Value::Symbol(_, span) | Value::List(_, span) => *span,
            Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_)
            | Value::Void | Value::Lambda { .. } => (0, 0),
        }
    }
}

// --- Evaluator ---

fn default_env() -> Env {
    Env::new()
}

/// Handle a `Tail` result inside the trampoline loop. Returns `Some(value)` for
/// `Done`, or `None` when the caller should `continue` the loop (after updating
/// `current` and optionally `owned_env`).
macro_rules! dispatch_tail {
    ($tail:expr, $current:ident, $owned_env:ident) => {
        match $tail {
            Tail::Done(val) => return Ok(val),
            Tail::Expr(expr) => {
                $current = expr;
                continue;
            }
            Tail::Call { expr, env: new_env } => {
                $current = expr;
                $owned_env = Some(new_env);
                continue;
            }
        }
    };
}

pub(crate) fn eval(expr: &Value, env: &mut Env, output: &mut String) -> Result<Value, EvalError> {
    let mut current = expr.clone();
    let mut owned_env: Option<Env> = None;

    loop {
        let active_env = owned_env.as_mut().unwrap_or(env);

        match current {
            val @ (Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Char(_)
            | Value::Void | Value::Lambda { .. }) => return Ok(val),

            Value::Symbol(ref name, span) => {
                return active_env
                    .get(name)
                    .map(|cell| cell.borrow().clone())
                    .ok_or_else(|| EvalError::UnboundVariable {
                        name: name.clone(),
                        line: span.0,
                        col: span.1,
                    });
            }

            Value::List(items, span) => {
                let (line, col) = span;
                if items.is_empty() {
                    return Err(EvalError::Parse {
                        message: "empty application".to_string(),
                        line,
                        col,
                    });
                }

                let op_name: Option<String> = match &items[0] {
                    Value::Symbol(s, _) => Some(s.clone()),
                    _ => None,
                };

                if let Some(ref op) = op_name {
                    match op.as_str() {
                        "quote" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity {
                                    procedure: "quote".to_string(),
                                    expected: "1".to_string(),
                                    got: items.len() - 1,
                                    line,
                                    col,
                                });
                            }
                            return Ok(items[1].clone());
                        }
                        "if" => {
                            if items.len() < 3 || items.len() > 4 {
                                return Err(EvalError::Parse {
                                    message: "if requires 2 or 3 arguments".to_string(),
                                    line,
                                    col,
                                });
                            }
                            let cond = eval(&items[1], active_env, output)?;
                            if cond.is_truthy() {
                                current = items[2].clone();
                                continue;
                            } else if items.len() == 4 {
                                current = items[3].clone();
                                continue;
                            }
                            return Ok(Value::Boolean(false));
                        }
                        "define" => return eval_define(&items, active_env, (line, col), output),
                        "set!" => {
                            if items.len() != 3 {
                                return Err(EvalError::Parse {
                                    message: "set! requires 2 arguments".to_string(),
                                    line,
                                    col,
                                });
                            }
                            let name = match &items[1] {
                                Value::Symbol(s, _) => s.clone(),
                                other => {
                                    return Err(EvalError::TypeError {
                                        message: format!(
                                            "set!: expected symbol, got {}",
                                            other.type_name()
                                        ),
                                        line,
                                        col,
                                    })
                                }
                            };
                            let val = eval(&items[2], active_env, output)?;
                            let cell = active_env.get(&name).ok_or_else(|| {
                                EvalError::UnboundVariable {
                                    name: name.clone(),
                                    line,
                                    col,
                                }
                            })?;
                            *cell.borrow_mut() = val;
                            return Ok(Value::Void);
                        }
                        "lambda" => return eval_lambda(&items, active_env, (line, col)),
                        "begin" => {
                            if items.len() <= 1 {
                                return Ok(Value::Boolean(false));
                            }
                            for item in &items[1..items.len() - 1] {
                                eval(item, active_env, output)?;
                            }
                            current = items[items.len() - 1].clone();
                            continue;
                        }
                        "and" => {
                            dispatch_tail!(eval_and(&items, active_env, output)?, current, owned_env);
                        }
                        "or" => {
                            dispatch_tail!(eval_or(&items, active_env, output)?, current, owned_env);
                        }
                        "cond" => {
                            dispatch_tail!(
                                forms::eval_cond(&items[1..], active_env, output)?,
                                current,
                                owned_env
                            );
                        }
                        "let" => {
                            dispatch_tail!(
                                forms::eval_let(&items[1..], active_env, (line, col), output)?,
                                current,
                                owned_env
                            );
                        }
                        "+" => return builtin_add(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "-" => return builtin_sub(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "*" => return builtin_mul(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "/" => return builtin_div(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "<" => return builtin_cmp(&items[1..], active_env, "<", output)
                            .map_err(|e| e.with_position(line, col)),
                        ">" => return builtin_cmp(&items[1..], active_env, ">", output)
                            .map_err(|e| e.with_position(line, col)),
                        "=" => return builtin_cmp(&items[1..], active_env, "=", output)
                            .map_err(|e| e.with_position(line, col)),
                        "<=" => return builtin_cmp(&items[1..], active_env, "<=", output)
                            .map_err(|e| e.with_position(line, col)),
                        ">=" => return builtin_cmp(&items[1..], active_env, ">=", output)
                            .map_err(|e| e.with_position(line, col)),
                        "not" => return builtin_not(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "cons" => return builtin_cons(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "car" => return builtin_car(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "cdr" => return builtin_cdr(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "null?" => return builtin_null(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "list" => return builtin_list(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "length" => return builtin_length(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "append" => return builtin_append(&items[1..], active_env, output)
                            .map_err(|e| e.with_position(line, col)),
                        "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?" => {
                            return builtin_type_pred(&items[1..], active_env, op.as_str(), output)
                                .map_err(|e| e.with_position(line, col));
                        }
                        "display" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity {
                                    procedure: "display".to_string(),
                                    expected: "1".to_string(),
                                    got: items.len() - 1,
                                    line,
                                    col,
                                });
                            }
                            let val = eval(&items[1], active_env, output)?;
                            output.push_str(&val.display_repr());
                            return Ok(Value::Void);
                        }
                        "write" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity {
                                    procedure: "write".to_string(),
                                    expected: "1".to_string(),
                                    got: items.len() - 1,
                                    line,
                                    col,
                                });
                            }
                            let val = eval(&items[1], active_env, output)?;
                            output.push_str(&val.display());
                            return Ok(Value::Void);
                        }
                        "newline" => {
                            if items.len() != 1 {
                                return Err(EvalError::Arity {
                                    procedure: "newline".to_string(),
                                    expected: "0".to_string(),
                                    got: items.len() - 1,
                                    line,
                                    col,
                                });
                            }
                            output.push('\n');
                            return Ok(Value::Void);
                        }
                        "string-append" | "string-length" | "substring"
                        | "string->number" | "number->string"
                        | "symbol->string" | "string->symbol" | "string-ref" => {
                            return builtin_string_ops(&items[1..], active_env, op.as_str(), output)
                                .map_err(|e| e.with_position(line, col));
                        }
                        "string-copy" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity {
                                    procedure: "string-copy".to_string(),
                                    expected: "1".to_string(),
                                    got: items.len() - 1,
                                    line,
                                    col,
                                });
                            }
                            let val = eval(&items[1], active_env, output)?;
                            match val {
                                Value::Str(s) => return Ok(Value::Str(s)),
                                other => return Err(EvalError::TypeError {
                                    message: format!(
                                        "string-copy: expected string, got {}",
                                        other.type_name()
                                    ),
                                    line,
                                    col,
                                }),
                            }
                        }
                        "string-set!" => {
                            return eval_string_set(&items, active_env, (line, col), output);
                        }
                        _ => {}
                    }
                }

                // Function application
                let func = eval(&items[0], active_env, output)?;
                let args = eval_args(&items[1..], active_env, output)?;
                dispatch_tail!(
                    apply_lambda(func, args, active_env, (line, col), output)?,
                    current,
                    owned_env
                );
            }
        }
    }
}

fn eval_args(args: &[Value], env: &mut Env, output: &mut String) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|a| eval(a, env, output)).collect()
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
            line: 0,
            col: 0,
        });
    }
    let mut env = default_env();
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
            line: 0,
            col: 0,
        });
    }
    let mut env = default_env();
    let mut output = String::new();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env, &mut output)?;
    }
    Ok((result.display(), output))
}

#[cfg(test)]
mod tests;
