pub(crate) mod apply;
mod builtins;
mod continuation;
mod dispatch;
pub mod env;
pub mod error;
mod io_ops;
mod list_ops;
mod macros;
pub mod parser;
mod special_forms;
mod string_ops;
mod toplevel;
pub mod value;
mod vector_ops;

pub use error::EvalError;
use builtins::{apply_arithmetic, apply_comparison};
use dispatch::{apply_value_tco, eval_apply, register_builtins};
use env::Env;
use list_ops::{
    eval_car, eval_cdr, eval_cons,
    eval_length, eval_list, eval_null_q, eval_type_pred,
};
use string_ops::{
    eval_char_to_integer, eval_integer_to_char, eval_list_to_string, eval_number_to_string,
    eval_string_append, eval_string_copy, eval_string_length, eval_string_ref, eval_string_set,
    eval_string_to_list, eval_string_to_number, eval_string_to_symbol, eval_substring,
    eval_symbol_to_string,
};
use io_ops::{eval_display, eval_map, eval_newline, eval_write};
use special_forms::{
    eval_and_tco, eval_case_tco, eval_cond_tco, eval_define, eval_if_tco, eval_lambda,
    eval_let_tco, eval_letrec_star_tco, eval_letrec_tco, eval_not, eval_or_tco, eval_set,
};
use std::rc::Rc;
use value::Value;

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
        Value::Vector(_) | Value::Lambda { .. } | Value::Builtin(_) | Value::Continuation(_) | Value::Macro { .. } => {
            Ok(Trampoline::Done(expr.clone()))
        }
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
        Value::Symbol(op) if op == "letrec" => eval_letrec_tco(args, env),
        Value::Symbol(op) if op == "letrec*" => eval_letrec_star_tco(args, env),
        Value::Symbol(op) if op == "set!" => eval_set(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "begin" => eval_body_tco(args, env),
        Value::Symbol(op) if op == "cond" => eval_cond_tco(args, env),
        Value::Symbol(op) if op == "case" => eval_case_tco(args, env),
        Value::Symbol(op) if op == "not" => eval_not(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "and" => eval_and_tco(args, env),
        Value::Symbol(op) if op == "or" => eval_or_tco(args, env),
        Value::Symbol(op) if op == "cons" => eval_cons(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "car" => eval_car(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "cdr" => eval_cdr(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "null?" => eval_null_q(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "list" => eval_list(args, env).map(Trampoline::Done),
        Value::Symbol(op) if op == "length" => eval_length(args, env).map(Trampoline::Done),
        Value::Symbol(op)
            if matches!(
                op.as_str(),
                "string?" | "number?" | "boolean?" | "pair?" | "symbol?" | "char?" | "vector?"
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
        Value::Symbol(op) if op == "apply" => eval_apply(args, env),
        Value::Symbol(op) if op == "define-syntax" => {
            macros::eval_define_syntax(args, env).map(Trampoline::Done)
        }
        _ => eval_call_or_macro(elements, operator, args, env),
    }
}

/// Build an env for macro expansion, injecting free bindings from def-site.
fn env_with_free_bindings(env: &Rc<Env>, free_bindings: &[(String, Value)]) -> Rc<Env> {
    if free_bindings.is_empty() {
        return Rc::clone(env);
    }
    let child = Env::child(env);
    for (name, val) in free_bindings {
        child.set(name.clone(), val.clone());
    }
    child
}

/// Try macro expansion, then fall back to function application.
fn eval_call_or_macro(
    elements: &[Value],
    operator: &Value,
    args: &[Value],
    env: &Rc<Env>,
) -> Result<Trampoline, EvalError> {
    if let Some(result) = macros::try_expand(elements, env) {
        let (expanded, _def_env, free_bindings) = result?;
        let eval_env = env_with_free_bindings(env, &free_bindings);
        return Ok(Trampoline::Bounce {
            expr: expanded,
            env: eval_env,
        });
    }
    let func = eval(operator, env)?;
    let evaluated_args: Vec<Value> =
        args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    apply_value_tco(func, &evaluated_args)
}

use toplevel::eval_top_level;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let expressions = parser::parse_spanned(input)?;
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }
    let env = Env::new();
    register_builtins(&env);
    match eval_top_level(&expressions, &env)? {
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
    register_builtins(&env);
    let last = eval_top_level(&expressions, &env)?;
    let output = env.take_output();
    let result = match last {
        Value::Void => String::new(),
        val => val.to_string(),
    };
    Ok((result, output))
}

#[cfg(test)]
mod tests;
