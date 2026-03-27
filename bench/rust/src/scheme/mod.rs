mod builtins;
pub mod error;
mod macros;
mod number;
mod parser;
mod runtime;
mod text;

pub use error::EvalError;
use runtime::*;
use text::SchemeString;

use std::cell::RefCell;
use std::rc::Rc;

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parser::parse_program(input)?;
    let mut output = String::new();
    let value = eval_program(&exprs, builtins::default_env(), &mut output)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parser::parse_program(input)?;
    let mut output = String::new();
    let value = eval_program(&exprs, builtins::default_env(), &mut output)?;
    Ok((value.render(), output))
}

#[cfg(test)]
mod tests;

fn eval_program(exprs: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match eval_program_tail(exprs, env, output)? {
        TailEvalResult::Value(value) => Ok(value),
        TailEvalResult::Call {
            procedure,
            args,
            pos,
        } => with_position(builtins::apply_procedure(procedure, &args, output), pos),
    }
}

fn eval_program_tail(
    exprs: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = exprs.split_last() else {
        return Ok(TailEvalResult::Value(Value::Void));
    };

    for expr in prefix {
        let _ = eval(expr, env.clone(), output)?;
    }

    eval_tail(last, env, output)
}

fn eval_args(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<Vec<EvaluatedArg>, EvalError> {
    args.iter()
        .map(|expr| {
            eval(expr, env.clone(), output).map(|value| EvaluatedArg {
                value,
                pos: expr.pos(),
            })
        })
        .collect()
}

fn eval_symbol(name: &str, pos: Position, env: &EnvRef) -> Result<Value, EvalError> {
    match env.lookup(name) {
        Some(Value::Uninitialized) => Err(EvalError::UninitializedBinding {
            name: name.to_string(),
        }
        .with_position(pos.line, pos.col)),
        Some(value) => Ok(value),
        None => Err(EvalError::UnboundSymbol {
            name: name.to_string(),
        }
        .with_position(pos.line, pos.col)),
    }
}

fn eval_tail(expr: &Expr, env: EnvRef, output: &mut String) -> Result<TailEvalResult, EvalError> {
    match expr {
        Expr::Bool { value, .. } => Ok(TailEvalResult::Value(Value::Bool(*value))),
        Expr::Number { value, .. } => Ok(TailEvalResult::Value(Value::Number(*value))),
        Expr::Char { value, .. } => Ok(TailEvalResult::Value(Value::Char(*value))),
        Expr::String { value, .. } => Ok(TailEvalResult::Value(Value::String(
            SchemeString::immutable(value),
        ))),
        Expr::Symbol { name, pos } => eval_symbol(name, *pos, &env).map(TailEvalResult::Value),
        Expr::List { items, pos } => eval_tail_list(items, *pos, env, output),
    }
}

fn eval_tail_list(
    items: &[Expr],
    pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        }
        .with_position(pos.line, pos.col));
    };

    let head_pos = head.pos();
    match head {
        Expr::Symbol { name, .. } => {
            eval_tail_named_head(name, items, args, pos, head_pos, env, output)
        }
        _ => eval_tail_application(head, args, head_pos, env, output),
    }
}

fn eval_tail_named_head(
    name: &str,
    items: &[Expr],
    args: &[Expr],
    list_pos: Position,
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match name {
        "and" => eval_tail_and(args, env, output),
        "or" => eval_tail_or(args, env, output),
        "if" => eval_tail_if(args, head_pos, env, output),
        "quote" => with_position(eval_quote(args).map(TailEvalResult::Value), head_pos),
        "begin" => eval_program_tail(args, env, output),
        "cond" => eval_tail_cond(args, head_pos, env, output),
        "case" => eval_tail_case(args, head_pos, env, output),
        "let" => eval_tail_let(args, head_pos, env, output),
        "let*" => eval_tail_let_star(args, head_pos, env, output),
        "letrec" => eval_tail_letrec(args, head_pos, env, output, LetrecMode::Parallel),
        "letrec*" => eval_tail_letrec(args, head_pos, env, output, LetrecMode::Sequential),
        "lambda" | "case-lambda" | "define" | "define-record-type" | "define-syntax" | "set!"
        | "do" => with_position(
            eval_list(items, env, output).map(TailEvalResult::Value),
            list_pos,
        ),
        _ => eval_tail_symbol_application(name, items, args, head_pos, env, output),
    }
}

fn eval_tail_symbol_application(
    name: &str,
    items: &[Expr],
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if let Some(transformer) = env.lookup_macro(name) {
        let (expanded, macro_env) = with_position(
            macros::expand_macro_call(items, env.clone(), transformer),
            head_pos,
        )?;
        return eval_tail(&expanded, macro_env, output);
    }

    let procedure = eval_symbol(name, head_pos, &env)?;
    eval_tail_call(procedure, args, head_pos, env, output)
}

fn eval_tail_application(
    head: &Expr,
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let procedure = eval(head, env.clone(), output)?;
    eval_tail_call(procedure, args, head_pos, env, output)
}

fn eval_tail_call(
    procedure: Value,
    args: &[Expr],
    pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let values = eval_args(args, env, output)?;
    Ok(TailEvalResult::Call {
        procedure,
        args: values,
        pos,
    })
}

fn eval_tail_and(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailEvalResult::Value(Value::Bool(true)));
    };

    for expr in prefix {
        let value = eval(expr, env.clone(), output)?;
        if !value.is_truthy() {
            return Ok(TailEvalResult::Value(value));
        }
    }

    eval_tail(last, env, output)
}

fn eval_tail_or(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((last, prefix)) = args.split_last() else {
        return Ok(TailEvalResult::Value(Value::Bool(false)));
    };

    for expr in prefix {
        let value = eval(expr, env.clone(), output)?;
        if value.is_truthy() {
            return Ok(TailEvalResult::Value(value));
        }
    }

    eval_tail(last, env, output)
}

fn eval_tail_if(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match args {
        [condition, consequent] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval_tail(consequent, env, output)
            } else {
                Ok(TailEvalResult::Value(Value::Void))
            }
        }
        [condition, consequent, alternate] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval_tail(consequent, env, output)
            } else {
                eval_tail(alternate, env, output)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_tail_cond(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    for clause in args {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "cond clauses must be lists".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "cond clauses cannot be empty".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if matches!(test, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }

        let test_value = eval(test, env.clone(), output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(test_value))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }
    }

    Ok(TailEvalResult::Value(Value::Void))
}

fn eval_tail_case(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let key = eval(key_expr, env.clone(), output)?;
    for clause in clauses {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "case clauses must be lists".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "case clauses cannot be empty".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if matches!(datum_expr, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }

        let Expr::List { items: datums, .. } = datum_expr else {
            return Err(EvalError::ParseError {
                message: "case clause datums must be a list".to_string(),
            }
            .with_position(head_pos.line, head_pos.col));
        };

        if datums
            .iter()
            .map(builtins::quote_expr)
            .any(|datum| values_eq(&key, &datum))
        {
            return if body.is_empty() {
                Ok(TailEvalResult::Value(Value::Void))
            } else {
                eval_program_tail(body, env.clone(), output)
            };
        }
    }

    Ok(TailEvalResult::Value(Value::Void))
}

fn eval_tail_let(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            eval_tail_named_let(name, bindings_expr, body, head_pos, env, output)
        }
        [bindings_expr, body @ ..] => {
            eval_tail_plain_let(bindings_expr, body, head_pos, env, output)
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }
        .with_position(head_pos.line, head_pos.col)),
    }
}

fn eval_tail_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 3",
            got: 2,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let params = bindings
        .iter()
        .map(|(param, _)| param.clone())
        .collect::<Vec<_>>();
    let values = bindings
        .iter()
        .map(|(_, expr)| {
            eval(expr, env.clone(), output).map(|value| EvaluatedArg {
                value,
                pos: expr.pos(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let closure_env = Environment::new(Some(env));
    let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
        params: LambdaParams {
            fixed: params,
            rest: None,
        },
        body: body.to_vec(),
        env: closure_env.clone(),
    }));
    closure_env.define(name.to_string(), procedure.clone());
    Ok(TailEvalResult::Call {
        procedure,
        args: values,
        pos: head_pos,
    })
}

fn eval_tail_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let values = bindings
        .iter()
        .map(|(_, expr)| eval(expr, env.clone(), output))
        .collect::<Result<Vec<_>, _>>()?;

    let let_env = Environment::new(Some(env));
    for ((name, _), value) in bindings.iter().zip(values) {
        let_env.define(name.clone(), value);
    }

    eval_program_tail(body, let_env, output)
}

fn eval_tail_let_star(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
) -> Result<TailEvalResult, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let let_env = Environment::new(Some(env));
    for (name, expr) in bindings {
        let value = eval(&expr, let_env.clone(), output)?;
        let_env.define(name, value);
    }

    eval_program_tail(body, let_env, output)
}

fn eval_tail_letrec(
    args: &[Expr],
    head_pos: Position,
    env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<TailEvalResult, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: args.len(),
        }
        .with_position(head_pos.line, head_pos.col));
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: 1,
        }
        .with_position(head_pos.line, head_pos.col));
    }

    let bindings = with_position(macros::parse_let_bindings(bindings_expr), head_pos)?;
    let (letrec_env, binding_refs) = create_recursive_bindings(&bindings, env);
    initialize_recursive_bindings(&bindings, &binding_refs, letrec_env.clone(), output, mode)?;
    eval_program_tail(body, letrec_env, output)
}

fn eval(expr: &Expr, env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
        Expr::Number { value, .. } => Ok(Value::Number(*value)),
        Expr::Char { value, .. } => Ok(Value::Char(*value)),
        Expr::String { value, .. } => Ok(Value::String(SchemeString::immutable(value))),
        Expr::Symbol { name, pos } => eval_symbol(name, *pos, &env),
        Expr::List { items, pos } => with_position(eval_list(items, env, output), *pos),
    }
}

fn eval_list(items: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, args)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "cannot evaluate empty list".to_string(),
        });
    };

    let head_pos = head.pos();

    match head {
        Expr::Symbol { name, .. } if name == "and" => {
            with_position(eval_and(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "or" => {
            with_position(eval_or(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "if" => {
            with_position(eval_if(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "quote" => with_position(eval_quote(args), head_pos),
        Expr::Symbol { name, .. } if name == "begin" => {
            with_position(eval_begin(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "cond" => {
            with_position(eval_cond(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "case" => {
            with_position(eval_case(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "let" => {
            with_position(eval_let(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "let*" => {
            with_position(eval_let_star(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "letrec" => with_position(
            eval_letrec(args, env, output, LetrecMode::Parallel),
            head_pos,
        ),
        Expr::Symbol { name, .. } if name == "letrec*" => with_position(
            eval_letrec(args, env, output, LetrecMode::Sequential),
            head_pos,
        ),
        Expr::Symbol { name, .. } if name == "lambda" => {
            with_position(eval_lambda(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "case-lambda" => {
            with_position(eval_case_lambda(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define" => {
            with_position(eval_define(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define-record-type" => {
            with_position(eval_define_record_type(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "define-syntax" => {
            with_position(macros::define_syntax(args, env), head_pos)
        }
        Expr::Symbol { name, .. } if name == "set!" => {
            with_position(eval_set(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } if name == "do" => {
            with_position(eval_do(args, env, output), head_pos)
        }
        Expr::Symbol { name, .. } => {
            if let Some(transformer) = env.lookup_macro(name) {
                let (expanded, macro_env) = with_position(
                    macros::expand_macro_call(items, env.clone(), transformer),
                    head_pos,
                )?;
                eval(&expanded, macro_env, output)
            } else {
                let procedure = eval(head, env.clone(), output)?;
                let values = eval_args(args, env.clone(), output)?;
                with_position(
                    builtins::apply_procedure(procedure, &values, output),
                    head_pos,
                )
            }
        }
        _ => {
            let procedure = eval(head, env.clone(), output)?;
            let values = eval_args(args, env.clone(), output)?;
            with_position(
                builtins::apply_procedure(procedure, &values, output),
                head_pos,
            )
        }
    }
}

fn eval_and(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);
    for expr in args {
        last = eval(expr, env.clone(), output)?;
        if !last.is_truthy() {
            return Ok(last);
        }
    }
    Ok(last)
}

fn eval_or(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);
    for expr in args {
        let value = eval(expr, env.clone(), output)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }
    Ok(last)
}

fn eval_if(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, consequent] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval(consequent, env, output)
            } else {
                Ok(Value::Void)
            }
        }
        [condition, consequent, alternate] => {
            if eval(condition, env.clone(), output)?.is_truthy() {
                eval(consequent, env, output)
            } else {
                eval(alternate, env, output)
            }
        }
        _ => Err(EvalError::WrongArgCount {
            name: "if",
            expected: "2 or 3",
            got: args.len(),
        }),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [quoted] = args else {
        return Err(EvalError::WrongArgCount {
            name: "quote",
            expected: "exactly 1",
            got: args.len(),
        });
    };

    Ok(builtins::quote_expr(quoted))
}

fn eval_begin(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_program(args, env, output)
}

fn eval_cond(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for clause in args {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "cond clauses must be lists".to_string(),
            });
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "cond clauses cannot be empty".to_string(),
            });
        };

        if matches!(test, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }

        let test_value = eval(test, env.clone(), output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_program(body, env.clone(), output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_case(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 0,
        });
    };

    if clauses.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case",
            min: 2,
            got: 1,
        });
    }

    let key = eval(key_expr, env.clone(), output)?;
    for clause in clauses {
        let Expr::List { items, .. } = clause else {
            return Err(EvalError::ParseError {
                message: "case clauses must be lists".to_string(),
            });
        };

        let Some((datum_expr, body)) = items.split_first() else {
            return Err(EvalError::ParseError {
                message: "case clauses cannot be empty".to_string(),
            });
        };

        if matches!(datum_expr, Expr::Symbol { name, .. } if name == "else") {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }

        let Expr::List { items: datums, .. } = datum_expr else {
            return Err(EvalError::ParseError {
                message: "case clause datums must be a list".to_string(),
            });
        };

        if datums
            .iter()
            .map(builtins::quote_expr)
            .any(|datum| values_eq(&key, &datum))
        {
            return if body.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(body, env.clone(), output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 3",
                    got: 2,
                });
            }

            let bindings = macros::parse_let_bindings(bindings_expr)?;
            let params = bindings
                .iter()
                .map(|(param, _)| param.clone())
                .collect::<Vec<_>>();
            let values = bindings
                .iter()
                .map(|(_, expr)| {
                    eval(expr, env.clone(), output).map(|value| EvaluatedArg {
                        value,
                        pos: expr.pos(),
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let closure_env = Environment::new(Some(env));
            let procedure = Value::Procedure(Rc::new(Procedure::Lambda {
                params: LambdaParams {
                    fixed: params,
                    rest: None,
                },
                body: body.to_vec(),
                env: closure_env.clone(),
            }));
            closure_env.define(name.clone(), procedure.clone());
            builtins::apply_procedure(procedure, &values, output)
        }
        [bindings_expr, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::WrongArgCount {
                    name: "let",
                    expected: "at least 2",
                    got: 1,
                });
            }

            let bindings = macros::parse_let_bindings(bindings_expr)?;
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, env.clone(), output))
                .collect::<Result<Vec<_>, _>>()?;

            let let_env = Environment::new(Some(env));
            for ((name, _), value) in bindings.iter().zip(values.into_iter()) {
                let_env.define(name.clone(), value);
            }

            eval_program(body, let_env, output)
        }
        [] => Err(EvalError::WrongArgCount {
            name: "let",
            expected: "at least 2",
            got: 0,
        }),
    }
}

fn eval_let_star(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: args.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "let*",
            expected: "at least 2",
            got: 1,
        });
    }

    let bindings = macros::parse_let_bindings(bindings_expr)?;
    let let_env = Environment::new(Some(env));
    for (name, expr) in bindings {
        let value = eval(&expr, let_env.clone(), output)?;
        let_env.define(name, value);
    }

    eval_program(body, let_env, output)
}

fn eval_letrec(
    args: &[Expr],
    env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<Value, EvalError> {
    let [bindings_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: args.len(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: letrec_form_name(mode),
            expected: "at least 2",
            got: 1,
        });
    }

    let bindings = macros::parse_let_bindings(bindings_expr)?;
    let (letrec_env, binding_refs) = create_recursive_bindings(&bindings, env);
    initialize_recursive_bindings(&bindings, &binding_refs, letrec_env.clone(), output, mode)?;

    eval_program(body, letrec_env, output)
}

#[derive(Clone, Copy)]
enum LetrecMode {
    Parallel,
    Sequential,
}

fn letrec_form_name(mode: LetrecMode) -> &'static str {
    match mode {
        LetrecMode::Parallel => "letrec",
        LetrecMode::Sequential => "letrec*",
    }
}

fn create_recursive_bindings(
    bindings: &[(String, Expr)],
    env: EnvRef,
) -> (EnvRef, Vec<BindingRef>) {
    let letrec_env = Environment::new(Some(env));
    let binding_refs = bindings
        .iter()
        .map(|(name, _)| {
            let binding = Rc::new(RefCell::new(Value::Uninitialized));
            letrec_env.define_alias(name.clone(), binding.clone());
            binding
        })
        .collect::<Vec<_>>();

    (letrec_env, binding_refs)
}

fn initialize_recursive_bindings(
    bindings: &[(String, Expr)],
    binding_refs: &[BindingRef],
    letrec_env: EnvRef,
    output: &mut String,
    mode: LetrecMode,
) -> Result<(), EvalError> {
    match mode {
        LetrecMode::Parallel => {
            let values = bindings
                .iter()
                .map(|(_, expr)| eval(expr, letrec_env.clone(), output))
                .collect::<Result<Vec<_>, _>>()?;
            for (binding, value) in binding_refs.iter().zip(values) {
                *binding.borrow_mut() = value;
            }
        }
        LetrecMode::Sequential => {
            for ((_, expr), binding) in bindings.iter().zip(binding_refs.iter()) {
                let value = eval(expr, letrec_env.clone(), output)?;
                *binding.borrow_mut() = value;
            }
        }
    }

    Ok(())
}

fn eval_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 0,
        });
    };

    if body.is_empty() {
        return Err(EvalError::WrongArgCount {
            name: "lambda",
            expected: "at least 2",
            got: 1,
        });
    }

    let params = macros::parse_lambda_params(params_expr)?;
    Ok(Value::Procedure(Rc::new(Procedure::Lambda {
        params,
        body: body.to_vec(),
        env,
    })))
}

fn eval_case_lambda(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(EvalError::WrongArgCountAtLeast {
            name: "case-lambda",
            min: 1,
            got: 0,
        });
    }

    let clauses = args
        .iter()
        .map(parse_case_lambda_clause)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Procedure(Rc::new(Procedure::CaseLambda {
        clauses,
        env,
    })))
}

fn parse_case_lambda_clause(clause_expr: &Expr) -> Result<CaseLambdaClause, EvalError> {
    let Expr::List { items, .. } = clause_expr else {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses must be lists".to_string(),
        });
    };

    let Some((params_expr, body)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses cannot be empty".to_string(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::ParseError {
            message: "case-lambda clauses require a body".to_string(),
        });
    }

    Ok(CaseLambdaClause {
        params: macros::parse_lambda_params(params_expr)?,
        body: body.to_vec(),
    })
}

struct DoBinding {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

fn eval_do(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let [bindings_expr, end_expr, body @ ..] = args else {
        return Err(EvalError::WrongArgCount {
            name: "do",
            expected: "at least 2",
            got: args.len(),
        });
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let (test_expr, result_exprs) = parse_do_end_clause(end_expr)?;

    let init_values = bindings
        .iter()
        .map(|binding| eval(&binding.init, env.clone(), output))
        .collect::<Result<Vec<_>, _>>()?;

    let do_env = Environment::new(Some(env));
    let binding_refs = bindings
        .iter()
        .zip(init_values)
        .map(|(binding, value)| {
            let cell = Rc::new(RefCell::new(value));
            do_env.define_alias(binding.name.clone(), cell.clone());
            cell
        })
        .collect::<Vec<_>>();

    loop {
        if eval(&test_expr, do_env.clone(), output)?.is_truthy() {
            return if result_exprs.is_empty() {
                Ok(Value::Void)
            } else {
                eval_program(&result_exprs, do_env.clone(), output)
            };
        }

        if !body.is_empty() {
            let _ = eval_program(body, do_env.clone(), output)?;
        }

        let next_values = bindings
            .iter()
            .zip(binding_refs.iter())
            .map(|(binding, current)| match &binding.step {
                Some(step_expr) => eval(step_expr, do_env.clone(), output),
                None => Ok(current.borrow().clone()),
            })
            .collect::<Result<Vec<_>, _>>()?;

        for (binding, value) in binding_refs.iter().zip(next_values.into_iter()) {
            *binding.borrow_mut() = value;
        }
    }
}

fn parse_do_bindings(expr: &Expr) -> Result<Vec<DoBinding>, EvalError> {
    let Expr::List {
        items: bindings, ..
    } = expr
    else {
        return Err(EvalError::ParseError {
            message: "do bindings must be a list".to_string(),
        });
    };

    bindings
        .iter()
        .map(|binding| {
            let Expr::List { items, .. } = binding else {
                return Err(EvalError::ParseError {
                    message: "do bindings must be (name init [step]) lists".to_string(),
                });
            };

            match items.as_slice() {
                [Expr::Symbol { name, .. }, init] => Ok(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: None,
                }),
                [Expr::Symbol { name, .. }, init, step] => Ok(DoBinding {
                    name: name.clone(),
                    init: init.clone(),
                    step: Some(step.clone()),
                }),
                _ => Err(EvalError::ParseError {
                    message: "do bindings must be (name init [step]) lists".to_string(),
                }),
            }
        })
        .collect()
}

fn parse_do_end_clause(expr: &Expr) -> Result<(Expr, Vec<Expr>), EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "do termination clause must be a list".to_string(),
        });
    };

    let Some((test, result_exprs)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "do termination clause cannot be empty".to_string(),
        });
    };

    Ok((test.clone(), result_exprs.to_vec()))
}

fn eval_define(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, .. }, value_expr] => {
            let value = eval(value_expr, env.clone(), output)?;
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List {
            items: signature, ..
        }, body @ ..] => {
            if body.is_empty() {
                return Err(EvalError::ParseError {
                    message: "define requires a function body".to_string(),
                });
            }

            let Some((Expr::Symbol { name, .. }, params)) = signature.split_first() else {
                return Err(EvalError::ParseError {
                    message: "define requires a function name".to_string(),
                });
            };

            let params = macros::parse_lambda_param_items(params)?;
            env.define(
                name.clone(),
                Value::Procedure(Rc::new(Procedure::Lambda {
                    params,
                    body: body.to_vec(),
                    env: env.clone(),
                })),
            );
            Ok(Value::Void)
        }
        _ => Err(EvalError::ParseError {
            message: "invalid define form".to_string(),
        }),
    }
}

fn eval_define_record_type(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(EvalError::ParseError {
            message: "invalid define-record-type form".to_string(),
        });
    };

    let type_name = parse_symbol_name(type_name_expr, "record type name")?;
    let (constructor_name, constructor_fields) = parse_record_constructor(constructor_expr)?;
    let predicate_name = parse_symbol_name(predicate_expr, "record predicate name")?;
    let field_specs = parse_record_field_specs(field_exprs)?;
    let record_type = Rc::new(RecordType { name: type_name });

    env.define(
        constructor_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordConstructor {
            name: constructor_name,
            record_type: record_type.clone(),
            field_count: constructor_fields.len(),
        })),
    );
    env.define(
        predicate_name.clone(),
        Value::Procedure(Rc::new(Procedure::RecordPredicate {
            name: predicate_name,
            record_type: record_type.clone(),
        })),
    );

    for field_spec in field_specs {
        let Some(field_index) = constructor_fields
            .iter()
            .position(|field_name| field_name == &field_spec.field_name)
        else {
            return Err(EvalError::ParseError {
                message: format!(
                    "record field {} is not declared by the constructor",
                    field_spec.field_name
                ),
            });
        };

        env.define(
            field_spec.accessor_name.clone(),
            Value::Procedure(Rc::new(Procedure::RecordAccessor {
                name: field_spec.accessor_name,
                record_type: record_type.clone(),
                field_index,
            })),
        );

        if let Some(mutator_name) = field_spec.mutator_name {
            env.define(
                mutator_name.clone(),
                Value::Procedure(Rc::new(Procedure::RecordMutator {
                    name: mutator_name,
                    record_type: record_type.clone(),
                    field_index,
                })),
            );
        }
    }

    Ok(Value::Void)
}

fn parse_symbol_name(expr: &Expr, context: &str) -> Result<String, EvalError> {
    let Expr::Symbol { name, .. } = expr else {
        return Err(EvalError::ParseError {
            message: format!("{context} must be a symbol"),
        });
    };
    Ok(name.clone())
}

fn parse_record_constructor(expr: &Expr) -> Result<(String, Vec<String>), EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "record constructor spec must be a list".to_string(),
        });
    };

    let Some((constructor_name, field_exprs)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "record constructor spec cannot be empty".to_string(),
        });
    };

    let constructor_name = parse_symbol_name(constructor_name, "record constructor name")?;
    let field_names = field_exprs
        .iter()
        .map(|expr| parse_symbol_name(expr, "record constructor field"))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((constructor_name, field_names))
}

fn parse_record_field_specs(field_exprs: &[Expr]) -> Result<Vec<RecordFieldSpec>, EvalError> {
    field_exprs
        .iter()
        .map(|field_expr| {
            let Expr::List { items, .. } = field_expr else {
                return Err(EvalError::ParseError {
                    message: "record field specs must be lists".to_string(),
                });
            };

            match items.as_slice() {
                [field_name, accessor_name] => Ok(RecordFieldSpec {
                    field_name: parse_symbol_name(field_name, "record field name")?,
                    accessor_name: parse_symbol_name(accessor_name, "record accessor name")?,
                    mutator_name: None,
                }),
                [field_name, accessor_name, mutator_name] => Ok(RecordFieldSpec {
                    field_name: parse_symbol_name(field_name, "record field name")?,
                    accessor_name: parse_symbol_name(accessor_name, "record accessor name")?,
                    mutator_name: Some(parse_symbol_name(mutator_name, "record mutator name")?),
                }),
                _ => Err(EvalError::ParseError {
                    message:
                        "record field specs must be (field accessor) or (field accessor mutator)"
                            .to_string(),
                }),
            }
        })
        .collect()
}

fn eval_set(args: &[Expr], env: EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol { name, pos }, value_expr] => {
            let value = eval(value_expr, env.clone(), output)?;
            env.set(name, value)
                .map_err(|error| error.with_position(pos.line, pos.col))?;
            Ok(Value::Void)
        }
        [_, _] => Err(EvalError::ParseError {
            message: "set! target must be a symbol".to_string(),
        }),
        _ => Err(EvalError::WrongArgCount {
            name: "set!",
            expected: "exactly 2",
            got: args.len(),
        }),
    }
}
