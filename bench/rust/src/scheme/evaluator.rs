use crate::scheme::ast::Expr;
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
        Expr::Integer(value) => Ok(Value::Integer(*value)),
        Expr::Boolean(value) => Ok(Value::Boolean(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Symbol(name) => environment
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() }),
        Expr::List(items) => eval_list(items, environment),
    }
}

fn eval_list(items: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::EmptyApplication);
    };

    if let Expr::Symbol(name) = operator {
        if let Some(value) = eval_special_form(name, arguments, environment)? {
            return Ok(value);
        }
    }

    let callable = eval_expr(operator, environment)?;
    let values = eval_arguments(arguments, environment)?;
    apply_callable(callable, &values)
}

fn eval_special_form(
    name: &str,
    arguments: &[Expr],
    environment: &Environment,
) -> Result<Option<Value>, EvalError> {
    match name {
        "and" => eval_and(arguments, environment).map(Some),
        "or" => eval_or(arguments, environment).map(Some),
        "if" => eval_if(arguments, environment).map(Some),
        "define" => eval_define(arguments, environment).map(Some),
        "quote" => eval_quote(arguments).map(Some),
        "lambda" => eval_lambda(arguments, environment).map(Some),
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

fn eval_if(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = arguments else {
        return Err(EvalError::WrongArgumentCount {
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

fn eval_define(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    match arguments {
        [Expr::Symbol(name), expression] => {
            let value = eval_expr(expression, environment)?;
            environment.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature), body @ ..] => define_function(signature, body, environment),
        _ => Err(EvalError::MalformedSpecialForm { form: "define" }),
    }
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    environment: &Environment,
) -> Result<Value, EvalError> {
    let Some((Expr::Symbol(name), parameters)) = signature.split_first() else {
        return Err(EvalError::MalformedSpecialForm { form: "define" });
    };

    let closure = build_closure(
        Some(name.clone()),
        parameters,
        body,
        environment.clone(),
        "define",
    )?;
    environment.define(name.clone(), Value::Closure(closure));
    Ok(Value::Void)
}

fn eval_quote(arguments: &[Expr]) -> Result<Value, EvalError> {
    let [expression] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            procedure: "quote",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(quote_expression(expression))
}

fn eval_lambda(arguments: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let Some((parameters, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            procedure: "lambda",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    let Expr::List(parameters) = parameters else {
        return Err(EvalError::InvalidParameterList { form: "lambda" });
    };

    let closure = build_closure(None, parameters, body, environment.clone(), "lambda")?;
    Ok(Value::Closure(closure))
}

fn build_closure(
    name: Option<String>,
    parameters: &[Expr],
    body: &[Expr],
    environment: Environment,
    form: &'static str,
) -> Result<Closure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { form });
    }

    let parameters = parameters
        .iter()
        .map(|parameter| match parameter {
            Expr::Symbol(name) => Ok(name.clone()),
            _ => Err(EvalError::NonSymbolParameter { form }),
        })
        .collect::<Result<_, _>>()?;

    Ok(Closure::new(name, parameters, body.to_vec(), environment))
}

fn quote_expression(expression: &Expr) -> Value {
    match expression {
        Expr::Integer(value) => Value::Integer(*value),
        Expr::Boolean(value) => Value::Boolean(*value),
        Expr::String(value) => Value::String(value.clone()),
        Expr::Symbol(value) => Value::Symbol(value.clone()),
        Expr::List(items) => items.iter().rev().fold(Value::EmptyList, |tail, item| {
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

fn apply_callable(callable: Value, arguments: &[Value]) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(procedure) => apply_builtin(procedure, arguments),
        Value::Closure(closure) => apply_closure(closure, arguments),
        other => Err(EvalError::NotCallable {
            expression: other.render(),
        }),
    }
}

fn apply_closure(closure: Closure, arguments: &[Value]) -> Result<Value, EvalError> {
    if arguments.len() != closure.parameters.len() {
        return Err(EvalError::WrongArgumentCount {
            procedure: "lambda",
            expected: ArgCount::Exactly(closure.parameters.len()),
            got: arguments.len(),
        });
    }

    let call_environment = closure.environment.child();
    closure
        .parameters
        .iter()
        .cloned()
        .zip(arguments.iter().cloned())
        .for_each(|(name, value)| call_environment.define(name, value));

    eval_body(&closure.body, &call_environment)
}

fn eval_body(body: &[Expr], environment: &Environment) -> Result<Value, EvalError> {
    let Some((last_expression, prefix)) = body.split_last() else {
        return Err(EvalError::MissingBody { form: "lambda" });
    };

    prefix
        .iter()
        .try_for_each(|expression| eval_expr(expression, environment).map(|_| ()))?;

    eval_expr(last_expression, environment)
}
