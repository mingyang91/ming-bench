mod builtins;
pub mod env;
pub mod error;
mod list_ops;
pub mod parser;
mod special_forms;
mod string_ops;
pub mod value;

pub use error::EvalError;
use builtins::{apply_arithmetic, apply_comparison};
use env::Env;
use list_ops::{eval_car, eval_cdr, eval_cons, eval_length, eval_list, eval_null_q, eval_type_pred};
use string_ops::{
    eval_number_to_string, eval_string_append, eval_string_length, eval_string_ref,
    eval_string_to_number, eval_string_to_symbol, eval_substring, eval_symbol_to_string,
};
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
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_) | Value::Void => Ok(expr.clone()),
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
                        "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
                    ) =>
                {
                    eval_type_pred(op, args, env)
                }
                Value::Symbol(op) if op == "string-append" => eval_string_append(args, env),
                Value::Symbol(op) if op == "string-length" => eval_string_length(args, env),
                Value::Symbol(op) if op == "substring" => eval_substring(args, env),
                Value::Symbol(op) if op == "string->number" => eval_string_to_number(args, env),
                Value::Symbol(op) if op == "number->string" => eval_number_to_string(args, env),
                Value::Symbol(op) if op == "symbol->string" => eval_symbol_to_string(args, env),
                Value::Symbol(op) if op == "string->symbol" => eval_string_to_symbol(args, env),
                Value::Symbol(op) if op == "string-ref" => eval_string_ref(args, env),
                Value::Symbol(op) if op == "display" => eval_display(args, env),
                Value::Symbol(op) if op == "write" => eval_write(args, env),
                Value::Symbol(op) if op == "newline" => eval_newline(args, env),
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

fn eval_display(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.display_str());
    Ok(Value::Void)
}

fn eval_write(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    env.write_output(&val.write_str());
    Ok(Value::Void)
}

fn eval_newline(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    if !args.is_empty() {
        return Err(EvalError::WrongArgCount {
            expected: "0".into(),
            got: args.len(),
        });
    }
    env.write_output("\n");
    Ok(Value::Void)
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
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
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
    let output = env.take_output();
    let result = match last {
        Value::Void => String::new(),
        val => val.to_string(),
    };
    Ok((result, output))
}

#[cfg(test)]
mod tests;
