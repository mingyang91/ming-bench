mod builtins;
pub mod env;
pub mod error;
mod list_ops;
pub mod parser;
mod special_forms;
pub mod value;

pub use error::EvalError;
use builtins::{apply_arithmetic, apply_comparison};
use env::Env;
use list_ops::{eval_car, eval_cdr, eval_cons, eval_length, eval_list, eval_null_q, eval_type_pred};
use special_forms::{
    eval_and, eval_cond, eval_define, eval_if, eval_lambda, eval_let, eval_not, eval_or,
};
use std::rc::Rc;
use value::Value;

/// Check if a value is truthy (everything except #f is truthy in Scheme).
fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Boolean(false))
}

/// Extract parameter names from a list of symbols.
fn extract_params(params: &[Value], form: &str) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|p| match p {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::BadSyntax { form: form.into() }),
        })
        .collect()
}

/// Apply a lambda to evaluated arguments.
fn apply_lambda(func: Value, args: &[Value]) -> Result<Value, EvalError> {
    let Value::Lambda { params, body, env } = func else {
        return Err(EvalError::NotAProcedure {
            value: format!("{func}"),
        });
    };
    if params.len() != args.len() {
        return Err(EvalError::WrongArgCount {
            expected: params.len().to_string(),
            got: args.len(),
        });
    }
    let child = Env::child(&env);
    for (param, arg) in params.iter().zip(args) {
        child.set(param.clone(), arg.clone());
    }
    eval_body(&body, &child)
}

/// Evaluate a sequence of expressions, returning the last result.
fn eval_body(exprs: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    exprs
        .iter()
        .try_fold(Value::Void, |_, expr| eval(expr, env))
}

/// Evaluate a single parsed Scheme value.
pub(crate) fn eval(expr: &Value, env: &Rc<Env>) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Void => Ok(expr.clone()),
        Value::Lambda { .. } => Ok(expr.clone()),
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Value::List(elements) => {
            let [operator, args @ ..] = elements.as_slice() else {
                return Err(EvalError::EmptyList);
            };
            match operator {
                Value::Symbol(op) if op == "quote" => match args {
                    [datum] => Ok(datum.clone()),
                    _ => Err(EvalError::BadSyntax {
                        form: "quote".into(),
                    }),
                },
                Value::Symbol(op) if op == "if" => eval_if(args, env),
                Value::Symbol(op) if op == "define" => eval_define(args, env),
                Value::Symbol(op) if op == "lambda" => eval_lambda(args, env),
                Value::Symbol(op) if matches!(op.as_str(), "+" | "-" | "*" | "/") => {
                    let evaluated: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_arithmetic(op, &evaluated)
                }
                Value::Symbol(op)
                    if matches!(op.as_str(), "<" | ">" | "=" | "<=" | ">=") =>
                {
                    let evaluated: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_comparison(op, &evaluated)
                }
                Value::Symbol(op) if op == "let" => eval_let(args, env),
                Value::Symbol(op) if op == "begin" => eval_body(args, env),
                Value::Symbol(op) if op == "cond" => eval_cond(args, env),
                Value::Symbol(op) if op == "not" => eval_not(args, env),
                Value::Symbol(op) if op == "and" => eval_and(args, env),
                Value::Symbol(op) if op == "or" => eval_or(args, env),
                Value::Symbol(op) if op == "cons" => eval_cons(args, env),
                Value::Symbol(op) if op == "car" => eval_car(args, env),
                Value::Symbol(op) if op == "cdr" => eval_cdr(args, env),
                Value::Symbol(op) if op == "null?" => eval_null_q(args, env),
                Value::Symbol(op) if op == "list" => eval_list(args, env),
                Value::Symbol(op) if op == "length" => eval_length(args, env),
                Value::Symbol(op)
                    if matches!(
                        op.as_str(),
                        "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
                    ) =>
                {
                    eval_type_pred(op, args, env)
                }
                _ => {
                    let func = eval(operator, env)?;
                    let evaluated_args: Vec<Value> =
                        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
                    apply_lambda(func, &evaluated_args)
                }
            }
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = parser::parse_spanned(input)?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let env = Env::new();
    let mut last = Value::Void;
    for (expr, span) in &expressions {
        last = eval(expr, &env).map_err(|e| EvalError::AtPosition {
            error: Box::new(e),
            line: span.line,
            col: span.col,
        })?;
    }
    match last {
        Value::Void => Err(EvalError::EmptyInput),
        val => Ok(val.to_string()),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
