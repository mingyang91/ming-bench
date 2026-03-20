mod builtins;
pub mod env;
pub mod error;
mod io_ops;
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
    eval_char_to_integer, eval_integer_to_char, eval_list_to_string, eval_number_to_string,
    eval_string_append, eval_string_copy, eval_string_length, eval_string_ref, eval_string_set,
    eval_string_to_list, eval_string_to_number, eval_string_to_symbol, eval_substring,
    eval_symbol_to_string,
};
use io_ops::{eval_display, eval_map, eval_newline, eval_write};
use special_forms::{
    eval_and, eval_cond_tco, eval_define, eval_if_tco, eval_lambda, eval_let_tco, eval_not,
    eval_or,
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

/// Result of evaluating in a tail-position-aware manner.
/// `Bounce` signals a tail call that should be continued by the trampoline.
pub(crate) enum Trampoline {
    Done(Value),
    Bounce { expr: Value, env: Rc<Env> },
}

/// Evaluate a sequence of expressions. All but the last are evaluated for
/// side effects; the last is returned as a `Trampoline` for TCO.
pub(crate) fn eval_body_tco(exprs: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    let [init @ .., last] = exprs else {
        return Ok(Trampoline::Done(Value::Void));
    };
    for expr in init {
        eval(expr, env)?;
    }
    Ok(Trampoline::Bounce {
        expr: last.clone(),
        env: Rc::clone(env),
    })
}

/// Apply a lambda, returning a Trampoline for TCO.
fn apply_lambda_tco(func: Value, args: &[Value]) -> Result<Trampoline, EvalError> {
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
    eval_body_tco(&body, &child)
}

/// Apply a lambda to evaluated arguments (non-TCO, for use in non-tail contexts).
pub(crate) fn apply_lambda(func: Value, args: &[Value]) -> Result<Value, EvalError> {
    match apply_lambda_tco(func, args)? {
        Trampoline::Done(v) => Ok(v),
        Trampoline::Bounce { expr, env } => eval(&expr, &env),
    }
}

/// Evaluate a single parsed Scheme value.
/// Uses a trampoline loop for tail-call optimization.
pub(crate) fn eval(start_expr: &Value, start_env: &Rc<Env>) -> Result<Value, EvalError> {
    let mut current_expr = start_expr.clone();
    let mut current_env = Rc::clone(start_env);

    loop {
        let result = eval_inner(&current_expr, &current_env)?;
        match result {
            Trampoline::Done(val) => return Ok(val),
            Trampoline::Bounce { expr, env } => {
                current_expr = expr;
                current_env = env;
            }
        }
    }
}

/// Inner eval that returns Trampoline — tail positions return Bounce.
fn eval_inner(expr: &Value, env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::String(_) | Value::Char(_) | Value::Void => {
            Ok(Trampoline::Done(expr.clone()))
        }
        Value::Lambda { .. } => Ok(Trampoline::Done(expr.clone())),
        Value::Symbol(name) => env
            .get(name)
            .map(Trampoline::Done)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Value::List(elements) => eval_list_form(elements, env),
    }
}

/// Evaluate a list form (function application or special form).
fn eval_list_form(elements: &[Value], env: &Rc<Env>) -> Result<Trampoline, EvalError> {
    let [operator, args @ ..] = elements else {
        return Err(EvalError::EmptyList);
    };
    match operator {
        Value::Symbol(op) if op == "quote" => match args {
            [datum] => Ok(Trampoline::Done(datum.clone())),
            _ => Err(EvalError::BadSyntax {
                form: "quote".into(),
            }),
        },
        Value::Symbol(op) if op == "if" => eval_if_tco(args, env),
        Value::Symbol(op) if op == "define" => eval_define(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "lambda" => eval_lambda(args, env).map(Trampoline::Done),
        Value::Symbol(op) if matches!(op.as_str(), "+" | "-" | "*" | "/") => {
            let evaluated: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_arithmetic(op, &evaluated).map(Trampoline::Done)
        }
        Value::Symbol(op) if matches!(op.as_str(), "<" | ">" | "=" | "<=" | ">=") => {
            let evaluated: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_comparison(op, &evaluated).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "let" => eval_let_tco(args, env),
        Value::Symbol(op) if op == "begin" => eval_body_tco(args, env),
        Value::Symbol(op) if op == "cond" => eval_cond_tco(args, env),
        Value::Symbol(op) if op == "not" => eval_not(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "and" => eval_and(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "or" => eval_or(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "cons" => eval_cons(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "car" => eval_car(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "cdr" => eval_cdr(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "null?" => eval_null_q(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "list" => eval_list(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "length" => eval_length(args, env).map(Trampoline::Done),
        Value::Symbol(op)
            if matches!(
                op.as_str(),
                "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?"
            ) =>
        {
            eval_type_pred(op, args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string-append" => {
            eval_string_append(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string-length" => {
            eval_string_length(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "substring" => eval_substring(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "string->number" => {
            eval_string_to_number(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "number->string" => {
            eval_number_to_string(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "symbol->string" => {
            eval_symbol_to_string(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string->symbol" => {
            eval_string_to_symbol(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string-ref" => {
            eval_string_ref(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string-copy" => {
            eval_string_copy(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string-set!" => {
            eval_string_set(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "string->list" => {
            eval_string_to_list(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "list->string" => {
            eval_list_to_string(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "char->integer" => {
            eval_char_to_integer(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "integer->char" => {
            eval_integer_to_char(args, env).map(Trampoline::Done)
        }
        Value::Symbol(op) if op == "map" => eval_map(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "display" => eval_display(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "write" => eval_write(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "newline" => eval_newline(args, env).map(Trampoline::Done),
        _ => {
            let func = eval(operator, env)?;
            let evaluated_args: Vec<Value> =
                args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
            apply_lambda_tco(func, &evaluated_args)
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
