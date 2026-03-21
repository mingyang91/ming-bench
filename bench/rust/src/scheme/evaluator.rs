use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::builtins::apply_builtin;
use crate::scheme::builtins::install_builtins;
use crate::scheme::environment::Environment;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::value::{Closure, Value};

pub fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    let Some((last_expression, prefix)) = expressions.split_last() else {
        return Err(EvalError::EmptyProgram);
    };

    let environment = Environment::new();
    install_builtins(&environment);

    prefix
        .iter()
        .try_for_each(|expression| eval_expr(expression, &environment).map(|_| ()))?;

    eval_expr(last_expression, &environment)
}

fn eval_expr(expression: &Expr, environment: &Environment) -> Result<Value, EvalError> {
    match expression {
        Expr::Integer { value, .. } => Ok(Value::Integer(*value)),
        Expr::Boolean { value, .. } => Ok(Value::Boolean(*value)),
        Expr::String { value, .. } => Ok(Value::String(value.clone())),
        Expr::Symbol { name, location } => {
            environment
                .lookup(name)
                .ok_or_else(|| EvalError::UnboundVariable {
                    location: *location,
                    name: name.clone(),
                })
        }
        Expr::List { items, location } => eval_list(items, *location, environment),
    }
}

fn eval_list(
    items: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::EmptyApplication { location });
    };

    if let Some(name) = operator.symbol_name() {
        if let Some(value) = eval_special_form(name, arguments, operator.location(), environment)? {
            return Ok(value);
        }
    }

    let callable = eval_expr(operator, environment)?;
    let values = eval_arguments(arguments, environment)?;
    apply_callable(callable, &values, operator.location())
}

fn eval_special_form(
    name: &str,
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Option<Value>, EvalError> {
    match name {
        "and" => eval_and(arguments, environment).map(Some),
        "or" => eval_or(arguments, environment).map(Some),
        "if" => eval_if(arguments, location, environment).map(Some),
        "define" => eval_define(arguments, location, environment).map(Some),
        "quote" => eval_quote(arguments, location).map(Some),
        "lambda" => eval_lambda(arguments, location, environment).map(Some),
        "let" => eval_let(arguments, location, environment).map(Some),
        "begin" => eval_begin(arguments, environment).map(Some),
        "cond" => eval_cond(arguments, location, environment).map(Some),
        _ => Ok(None),
    }
}

fn eval_and(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(true);

    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let mut last_value = Value::Boolean(false);

    for argument in arguments {
        let value = eval_expr(argument, environment)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last_value = value;
    }

    Ok(last_value)
}

fn eval_if(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "if",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let branch = if eval_expr(condition, environment)?.is_truthy() {
        consequent
    } else {
        alternate
    };

    eval_expr(branch, environment)
}

fn eval_define(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    match arguments {
        [Expr::Symbol { name, .. }, expression] => {
            let value = eval_expr(expression, environment)?;
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

fn eval_let(
    arguments: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    let Some((bindings, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "let",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    let Expr::List {
        items: bindings, ..
    } = bindings
    else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "let",
        });
    };

    let evaluated_bindings = bindings
        .iter()
        .map(|binding| eval_let_binding(binding, environment))
        .collect::<Result<Vec<_>, _>>()?;

    let scope = environment.child();
    for (name, value) in evaluated_bindings {
        scope.define(name, value);
    }

    eval_required_sequence(body, &scope, "let", location)
}

fn eval_let_binding(
    binding: &Expr,
    environment: &Environment,
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

    Ok((name.clone(), eval_expr(expression, environment)?))
}

fn eval_begin(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    eval_sequence(arguments, environment)
}

fn eval_cond(
    clauses: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
    match clauses {
        [] => Ok(Value::Void),
        [clause, rest @ ..] => eval_cond_clause(clause, rest, location, environment),
    }
}

fn eval_cond_clause(
    clause: &Expr,
    remaining_clauses: &[Expr],
    location: SourceLocation,
    environment: &Environment,
) -> Result<Value, EvalError> {
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
        return eval_required_sequence(body, environment, "cond", clause_location);
    }

    let test_value = eval_expr(test, environment)?;
    if !test_value.is_truthy() {
        return eval_cond(remaining_clauses, location, environment);
    }

    if body.is_empty() {
        return Ok(test_value);
    }

    eval_sequence(body, environment)
}

fn build_closure(
    name: Option<String>,
    parameters: &[Expr],
    body: &[Expr],
    environment: Environment,
    form: &'static str,
    location: SourceLocation,
) -> Result<Closure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

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

    Ok(Closure::new(name, parameters, body.to_vec(), environment))
}

fn quote_expression(expression: &Expr) -> Value {
    match expression {
        Expr::Integer { value, .. } => Value::Integer(*value),
        Expr::Boolean { value, .. } => Value::Boolean(*value),
        Expr::String { value, .. } => Value::String(value.clone()),
        Expr::Symbol { name, .. } => Value::Symbol(name.clone()),
        Expr::List { items, .. } => items.iter().rev().fold(Value::EmptyList, |tail, item| {
            Value::Pair(Box::new(quote_expression(item)), Box::new(tail))
        }),
    }
}

fn eval_arguments(arguments: &[Expr], environment: &Environment) -> Result<Vec<Value>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval_expr(argument, environment))
        .collect()
}

fn apply_callable(
    callable: Value,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(procedure) => apply_builtin(procedure, arguments, location),
        Value::Closure(closure) => apply_closure(&closure, arguments, location),
        other => Err(EvalError::NotCallable {
            location,
            expression: other.render(),
        }),
    }
}

fn apply_closure(
    closure: &Closure,
    arguments: &[Value],
    location: SourceLocation,
) -> Result<Value, EvalError> {
    if arguments.len() != closure.parameters.len() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "lambda",
            expected: ArgCount::Exactly(closure.parameters.len()),
            got: arguments.len(),
        });
    }

    let call_environment = closure.environment.child();
    for (name, value) in closure
        .parameters
        .iter()
        .cloned()
        .zip(arguments.iter().cloned())
    {
        call_environment.define(name, value);
    }

    eval_required_sequence(&closure.body, &call_environment, "lambda", location)
}

fn eval_required_sequence(
    body: &[Expr],
    environment: &Environment,
    form: &'static str,
    location: SourceLocation,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

    eval_sequence(body, environment)
}

fn eval_sequence(body: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let Some((last_expression, prefix)) = body.split_last() else {
        return Ok(Value::Void);
    };

    prefix
        .iter()
        .try_for_each(|expression| eval_expr(expression, environment).map(|_| ()))?;

    eval_expr(last_expression, environment)
}
