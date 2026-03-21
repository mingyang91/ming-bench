use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::builtins::{apply_builtin, install_builtins};
use crate::scheme::environment::Environment;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::value::{Closure, Value};

enum EvalAction {
    Value(Value),
    Call {
        callable: Value,
        arguments: Vec<Value>,
        location: SourceLocation,
    },
}

pub fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    eval_program_with_output(expressions).map(|(value, _)| value)
}

pub fn eval_program_with_output(expressions: &[Expr]) -> Result<(Value, String), EvalError> {
    let Some((last_expression, prefix)) = expressions.split_last() else {
        return Err(EvalError::EmptyProgram);
    };

    let environment = Environment::new();
    install_builtins(&environment);
    let mut output = String::new();

    eval_discarded_expressions(prefix, &environment, &mut output)?;
    let value = eval_expr(last_expression, &environment, &mut output)?;

    Ok((value, output))
}

fn eval_expr(
    expression: &Expr,
    environment: &Environment,
    output: &mut String,
) -> Result<Value, EvalError> {
    resolve_action(eval_tail_expr(expression, environment, output)?, output)
}

fn resolve_action(mut action: EvalAction, output: &mut String) -> Result<Value, EvalError> {
    loop {
        action = match action {
            EvalAction::Value(value) => return Ok(value),
            EvalAction::Call {
                callable,
                arguments,
                location,
            } => apply_tail_callable(callable, arguments, location, output)?,
        };
    }
}

fn eval_tail_expr(
    expression: &Expr,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    match expression {
        Expr::Integer { value, .. } => Ok(EvalAction::Value(Value::Integer(*value))),
        Expr::Boolean { value, .. } => Ok(EvalAction::Value(Value::Boolean(*value))),
        Expr::String { value, .. } => Ok(EvalAction::Value(Value::immutable_string(value.clone()))),
        Expr::Character { value, .. } => Ok(EvalAction::Value(Value::Character(*value))),
        Expr::Symbol { name, location } => environment
            .lookup(name)
            .map(EvalAction::Value)
            .ok_or_else(|| EvalError::UnboundVariable {
                location: *location,
                name: name.clone(),
            }),
        Expr::List { items, location } => eval_tail_list(items, *location, environment, output),
    }
}

fn eval_tail_list(
    items: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::EmptyApplication { location });
    };

    if let Some(name) = operator.symbol_name() {
        if let Some(action) =
            eval_tail_special_form(name, arguments, operator.location(), environment, output)?
        {
            return Ok(action);
        }
    }

    let callable = eval_expr(operator, environment, output)?;
    let values = eval_arguments(arguments, environment, output)?;

    Ok(EvalAction::Call {
        callable,
        arguments: values,
        location: operator.location(),
    })
}

fn eval_tail_special_form(
    name: &str,
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<Option<EvalAction>, EvalError> {
    match name {
        "and" => eval_tail_and(arguments, environment, output).map(Some),
        "or" => eval_tail_or(arguments, environment, output).map(Some),
        "if" => eval_tail_if(arguments, location, environment, output).map(Some),
        "define" => eval_define(arguments, location, environment, output)
            .map(EvalAction::Value)
            .map(Some),
        "set!" => eval_set(arguments, location, environment, output)
            .map(EvalAction::Value)
            .map(Some),
        "quote" => eval_quote(arguments, location)
            .map(EvalAction::Value)
            .map(Some),
        "lambda" => eval_lambda(arguments, location, environment)
            .map(EvalAction::Value)
            .map(Some),
        "let" => eval_tail_let(arguments, location, environment, output).map(Some),
        "begin" => eval_tail_sequence(arguments, environment, output).map(Some),
        "cond" => eval_tail_cond(arguments, location, environment, output).map(Some),
        _ => Ok(None),
    }
}

fn eval_tail_and(
    arguments: &[Expr],
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let Some((last_argument, prefix)) = arguments.split_last() else {
        return Ok(EvalAction::Value(Value::Boolean(true)));
    };

    for argument in prefix {
        let value = eval_expr(argument, environment, output)?;
        if !value.is_truthy() {
            return Ok(EvalAction::Value(value));
        }
    }

    eval_tail_expr(last_argument, environment, output)
}

fn eval_tail_or(
    arguments: &[Expr],
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let Some((last_argument, prefix)) = arguments.split_last() else {
        return Ok(EvalAction::Value(Value::Boolean(false)));
    };

    for argument in prefix {
        let value = eval_expr(argument, environment, output)?;
        if value.is_truthy() {
            return Ok(EvalAction::Value(value));
        }
    }

    eval_tail_expr(last_argument, environment, output)
}

fn eval_tail_if(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "if",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let branch = if eval_expr(condition, environment, output)?.is_truthy() {
        consequent
    } else {
        alternate
    };

    eval_tail_expr(branch, environment, output)
}

fn eval_define(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<Value, EvalError> {
    match arguments {
        [Expr::Symbol { name, .. }, expression] => {
            let value = eval_expr(expression, environment, output)?;
            environment.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List {
            items: signature, ..
        }, body @ ..] => define_function(signature, body, location, environment),
        _ => Err(EvalError::MalformedSpecialForm {
            location,
            form: "define",
        }),
    }
}

fn eval_set(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<Value, EvalError> {
    let [Expr::Symbol { name, location: name_location }, expression] = arguments else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "set!",
        });
    };

    let value = eval_expr(expression, environment, output)?;
    if environment.set(name, value) {
        return Ok(Value::Void);
    }

    Err(EvalError::UnboundVariable {
        location: *name_location,
        name: name.clone(),
    })
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    let Some((Expr::Symbol { name, .. }, parameters)) = signature.split_first() else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "define",
        });
    };

    let closure = build_closure(
        Some(name.clone()),
        parameters,
        body,
        environment.clone(),
        "define",
        location,
    )?;
    environment.define(name.clone(), Value::Closure(closure));
    Ok(Value::Void)
}

fn eval_quote(arguments: &[Expr], location: SourceLocation) -> Result<Value, EvalError> {
    let [expression] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "quote",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(quote_expression(expression))
}

fn eval_lambda(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    let Some((parameters, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "lambda",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    let Expr::List {
        items: parameters, ..
    } = parameters
    else {
        return Err(EvalError::InvalidParameterList {
            location,
            form: "lambda",
        });
    };

    let closure = build_closure(
        None,
        parameters,
        body,
        environment.clone(),
        "lambda",
        location,
    )?;
    Ok(Value::Closure(closure))
}

fn eval_tail_let(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    match arguments {
        [] => Err(EvalError::WrongArgumentCount {
            location,
            procedure: "let",
            expected: ArgCount::AtLeast(2),
            got: 0,
        }),
        [Expr::Symbol { name, .. }, bindings, body @ ..] => {
            eval_named_let(name, bindings, body, location, environment, output)
        }
        [bindings, body @ ..] => eval_standard_let(bindings, body, location, environment, output),
    }
}

fn eval_standard_let(
    bindings_expression: &Expr,
    body: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let evaluated_bindings = let_binding_items(bindings_expression, location)?
        .iter()
        .map(|binding| eval_let_binding(binding, environment, output))
        .collect::<Result<Vec<_>, _>>()?;

    let scope = environment.child();
    for (name, value) in evaluated_bindings {
        scope.define(name, value);
    }

    eval_required_sequence_tail(body, &scope, "let", location, output)
}

fn eval_named_let(
    name: &str,
    bindings_expression: &Expr,
    body: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let evaluated_bindings = let_binding_items(bindings_expression, location)?
        .iter()
        .map(|binding| eval_let_binding(binding, environment, output))
        .collect::<Result<Vec<_>, _>>()?;

    let closure_environment = environment.child();
    let parameters = evaluated_bindings
        .iter()
        .map(|(parameter, _)| parameter.clone())
        .collect();
    let arguments = evaluated_bindings
        .into_iter()
        .map(|(_, value)| value)
        .collect();
    let closure = build_closure_from_names(
        Some(name.to_string()),
        parameters,
        body,
        closure_environment.clone(),
        "let",
        location,
    )?;
    let callable = Value::Closure(closure);
    closure_environment.define(name.to_string(), callable.clone());

    Ok(EvalAction::Call {
        callable,
        arguments,
        location,
    })
}

fn let_binding_items(
    bindings_expression: &Expr,
    location: SourceLocation,
) -> Result<&[Expr], EvalError> {
    let Expr::List { items, .. } = bindings_expression else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "let",
        });
    };

    Ok(items)
}

fn eval_let_binding(
    binding: &Expr,
    environment: &Environment,
    output: &mut String,
) -> Result<(String, Value), EvalError> {
    let location = binding.location();
    let Expr::List { items, .. } = binding else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "let",
        });
    };
    let [Expr::Symbol { name, .. }, expression] = items.as_slice() else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "let",
        });
    };

    Ok((name.clone(), eval_expr(expression, environment, output)?))
}

fn eval_tail_cond(
    clauses: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    match clauses {
        [] => Ok(EvalAction::Value(Value::Void)),
        [clause, rest @ ..] => eval_tail_cond_clause(clause, rest, location, environment, output),
    }
}

fn eval_tail_cond_clause(
    clause: &Expr,
    remaining_clauses: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let clause_location = clause.location();
    let Expr::List { items, .. } = clause else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "cond",
        });
    };
    let Some((test, body)) = items.split_first() else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "cond",
        });
    };

    if matches!(test, Expr::Symbol { name, .. } if name == "else") {
        if !remaining_clauses.is_empty() {
            return Err(EvalError::MalformedSpecialForm {
                location: test.location(),
                form: "cond",
            });
        }
        return eval_required_sequence_tail(body, environment, "cond", clause_location, output);
    }

    let test_value = eval_expr(test, environment, output)?;
    if !test_value.is_truthy() {
        return eval_tail_cond(remaining_clauses, location, environment, output);
    }

    if body.is_empty() {
        return Ok(EvalAction::Value(test_value));
    }

    eval_tail_sequence(body, environment, output)
}

fn build_closure(
    name: Option<String>,
    parameters: &[Expr],
    body: &[Expr],
    environment: Environment,
    form: &'static str,
    location: SourceLocation,
) -> Result<Closure, EvalError> {
    let parameters = parameters
        .iter()
        .map(|parameter| match parameter {
            Expr::Symbol { name, .. } => Ok(name.clone()),
            _ => Err(EvalError::NonSymbolParameter {
                location: parameter.location(),
                form,
            }),
        })
        .collect::<Result<_, _>>()?;

    build_closure_from_names(name, parameters, body, environment, form, location)
}

fn build_closure_from_names(
    name: Option<String>,
    parameters: Vec<String>,
    body: &[Expr],
    environment: Environment,
    form: &'static str,
    location: SourceLocation,
) -> Result<Closure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

    Ok(Closure::new(name, parameters, body.to_vec(), environment))
}

fn quote_expression(expression: &Expr) -> Value {
    match expression {
        Expr::Integer { value, .. } => Value::Integer(*value),
        Expr::Boolean { value, .. } => Value::Boolean(*value),
        Expr::String { value, .. } => Value::immutable_string(value.clone()),
        Expr::Character { value, .. } => Value::Character(*value),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::Pair(Box::new(quote_expression(item)), Box::new(tail))
        }),
    }
}

fn eval_arguments(
    arguments: &[Expr],
    environment: &Environment,
    output: &mut String,
) -> Result<Vec<Value>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_expr(argument, environment, output))
        .collect()
}

pub(crate) fn apply_callable(
    callable: Value,
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    resolve_action(
        EvalAction::Call {
            callable,
            arguments: arguments.to_vec(),
            location,
        },
        output,
    )
}

fn apply_tail_callable(
    callable: Value,
    arguments: Vec<Value>,
    location: SourceLocation,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    match callable {
        Value::Builtin(procedure) => {
            apply_builtin(procedure, &arguments, location, output).map(EvalAction::Value)
        }
        Value::Closure(closure) => apply_tail_closure(closure, arguments, location, output),
        other => Err(EvalError::NotCallable {
            location,
            expression: other.render(),
        }),
    }
}

fn apply_tail_closure(
    closure: Closure,
    arguments: Vec<Value>,
    location: SourceLocation,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    if arguments.len() != closure.parameters.len() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "lambda",
            expected: ArgCount::Exactly(closure.parameters.len()),
            got: arguments.len(),
        });
    }

    let call_environment = closure.environment.child();
    for (name, value) in closure.parameters.iter().cloned().zip(arguments) {
        call_environment.define(name, value);
    }

    eval_required_sequence_tail(&closure.body, &call_environment, "lambda", location, output)
}

fn eval_required_sequence_tail(
    body: &[Expr],
    environment: &Environment,
    form: &'static str,
    location: SourceLocation,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

    eval_tail_sequence(body, environment, output)
}

fn eval_tail_sequence(
    body: &[Expr],
    environment: &Environment,
    output: &mut String,
) -> Result<EvalAction, EvalError> {
    let Some((last_expression, prefix)) = body.split_last() else {
        return Ok(EvalAction::Value(Value::Void));
    };

    eval_discarded_expressions(prefix, environment, output)?;
    eval_tail_expr(last_expression, environment, output)
}

fn eval_discarded_expressions(
    expressions: &[Expr],
    environment: &Environment,
    output: &mut String,
) -> Result<(), EvalError> {
    expressions
        .iter()
        .try_for_each(|expression| eval_expr(expression, environment, output).map(|_| ()))
}
