use std::ops::ControlFlow;
use std::rc::Rc;

use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::builtins::{apply_builtin, install_builtins};
use crate::scheme::continuation::{
    CapturedContinuation, DynamicWind, ExceptionHandler, Frame, RaisedException,
};
use crate::scheme::environment::Environment;
use crate::scheme::equality::is_eqv;
use crate::scheme::error::{ArgCount, EvalError};
use crate::scheme::syntax::MacroEnvironment;
use crate::scheme::value::{list_from_values, Closure, Value};

type ContinuationFrames = Vec<Frame>;

enum State {
    Eval {
        expression: Expr,
        environment: Environment,
        continuation: ContinuationFrames,
    },
    Apply {
        callable: Value,
        arguments: Vec<Value>,
        location: SourceLocation,
        continuation: ContinuationFrames,
    },
    Raise {
        exception: RaisedException,
        continuation: ContinuationFrames,
    },
    Return {
        value: Value,
        continuation: ContinuationFrames,
    },
}

struct ParsedFormals {
    required_parameters: Vec<String>,
    rest_parameter: Option<String>,
}

struct StandardLetProgress {
    names: Vec<String>,
    pending: Vec<Expr>,
    evaluated_rev: Vec<Value>,
    body: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
}

struct NamedLetProgress {
    name: String,
    parameters: Vec<String>,
    pending: Vec<Expr>,
    evaluated_rev: Vec<Value>,
    body: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
}

struct RecursiveLetProgress {
    pending_rev: Vec<(String, Expr)>,
    body: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
    form: &'static str,
}

pub(crate) fn eval_program(expressions: &[Expr]) -> Result<Value, EvalError> {
    eval_program_with_output(expressions).map(|(value, _)| value)
}

pub(crate) fn eval_program_with_output(expressions: &[Expr]) -> Result<(Value, String), EvalError> {
    if expressions.is_empty() {
        return Err(EvalError::EmptyProgram);
    }

    let environment = Environment::new();
    let macro_environment = MacroEnvironment::new();
    install_builtins(&environment);
    install_runtime_procedures(&environment);

    let mut output = String::new();
    let value = run(
        eval_sequence(expressions.to_vec(), environment, Vec::new())?,
        &mut output,
        &macro_environment,
    )?;

    Ok((value, output))
}

pub(crate) fn apply_callable(
    callable: Value,
    arguments: &[Value],
    location: SourceLocation,
    output: &mut String,
) -> Result<Value, EvalError> {
    run(
        State::Apply {
            callable,
            arguments: arguments.to_vec(),
            location,
            continuation: Vec::new(),
        },
        output,
        &MacroEnvironment::new(),
    )
}

fn install_runtime_procedures(environment: &Environment) {
    environment.define("call/cc", Value::CallWithCurrentContinuation);
    environment.define(
        "call-with-current-continuation",
        Value::CallWithCurrentContinuation,
    );
    environment.define("dynamic-wind", Value::DynamicWind);
    environment.define("raise", Value::Raise);
    environment.define("with-exception-handler", Value::WithExceptionHandler);
}

fn run(
    mut state: State,
    output: &mut String,
    macro_environment: &MacroEnvironment,
) -> Result<Value, EvalError> {
    loop {
        state = match state {
            State::Eval {
                expression,
                environment,
                continuation,
            } => eval_expression(expression, environment, continuation, macro_environment)?,
            State::Apply {
                callable,
                arguments,
                location,
                continuation,
            } => apply_value(callable, arguments, location, continuation, output)?,
            State::Raise {
                exception,
                continuation,
            } => raise_exception(exception, continuation)?,
            State::Return {
                value,
                continuation,
            } => match continue_return(value, continuation, macro_environment)? {
                ControlFlow::Break(value) => return Ok(value),
                ControlFlow::Continue(state) => state,
            },
        };
    }
}

fn continue_return(
    value: Value,
    mut continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<ControlFlow<Value, State>, EvalError> {
    let Some(frame) = continuation.pop() else {
        return Ok(ControlFlow::Break(value));
    };

    continue_with_frame(frame, value, continuation, macro_environment).map(ControlFlow::Continue)
}

fn eval_expression(
    expression: Expr,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    match expression {
        Expr::Integer { value, .. } => Ok(State::Return {
            value: Value::Integer(value),
            continuation,
        }),
        Expr::Boolean { value, .. } => Ok(State::Return {
            value: Value::Boolean(value),
            continuation,
        }),
        Expr::String { value, .. } => Ok(State::Return {
            value: Value::immutable_string(value),
            continuation,
        }),
        Expr::Character { value, .. } => Ok(State::Return {
            value: Value::Character(value),
            continuation,
        }),
        Expr::Symbol { name, location } => match environment.lookup(&name) {
            Some(Value::Uninitialized) => Err(EvalError::UninitializedVariable { location, name }),
            Some(value) => Ok(State::Return {
                value,
                continuation,
            }),
            None => Err(EvalError::UnboundVariable { location, name }),
        },
        Expr::List { items, location } => eval_list_expression(
            items,
            location,
            environment,
            continuation,
            macro_environment,
        ),
    }
}

fn eval_list_expression(
    items: Vec<Expr>,
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    let expression = Expr::list(items.clone(), location);
    let Some((operator, arguments)) = split_first(items) else {
        return Err(EvalError::EmptyApplication { location });
    };

    if let Some(name) = operator.symbol_name() {
        if let Some(state) = eval_special_form(
            name,
            &arguments,
            operator.location(),
            environment.clone(),
            continuation.clone(),
            macro_environment,
        )? {
            return Ok(state);
        }

        if macro_environment.is_macro(name) {
            let expanded = macro_environment.expand_expression(&expression)?;
            return Ok(State::Eval {
                expression: expanded,
                environment,
                continuation,
            });
        }
    }

    let operator_location = operator.location();
    let mut next_continuation = continuation;
    next_continuation.push(Frame::ApplyOperator {
        arguments,
        environment: environment.clone(),
        location: operator_location,
    });

    Ok(State::Eval {
        expression: operator,
        environment,
        continuation: next_continuation,
    })
}

fn eval_special_form(
    name: &str,
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<Option<State>, EvalError> {
    match name {
        "and" => eval_and(arguments.to_vec(), environment, continuation).map(Some),
        "or" => eval_or(arguments.to_vec(), environment, continuation).map(Some),
        "if" => eval_if(arguments, location, environment, continuation).map(Some),
        "define" => eval_define(
            arguments,
            location,
            environment,
            continuation,
            macro_environment,
        )
        .map(Some),
        "define-syntax" => eval_define_syntax(
            arguments,
            location,
            environment,
            continuation,
            macro_environment,
        )
        .map(Some),
        "set!" => eval_set(arguments, location, environment, continuation).map(Some),
        "quote" => eval_quote(arguments, location, continuation).map(Some),
        "lambda" => eval_lambda(
            arguments,
            location,
            environment,
            continuation,
            macro_environment,
        )
        .map(Some),
        "let" => eval_let(
            arguments,
            location,
            environment,
            continuation,
            macro_environment,
        )
        .map(Some),
        "letrec" => {
            eval_recursive_let(arguments, location, environment, continuation, "letrec").map(Some)
        }
        "letrec*" => {
            eval_recursive_let(arguments, location, environment, continuation, "letrec*").map(Some)
        }
        "begin" => eval_sequence(arguments.to_vec(), environment, continuation).map(Some),
        "cond" => eval_cond(arguments.to_vec(), environment, continuation).map(Some),
        "case" => eval_case(arguments, location, environment, continuation).map(Some),
        "guard" => eval_guard(arguments, location, environment, continuation).map(Some),
        _ => Ok(None),
    }
}

fn eval_and(
    arguments: Vec<Expr>,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if arguments.is_empty() {
        return Ok(State::Return {
            value: Value::Boolean(true),
            continuation,
        });
    }

    let mut remaining_rev = arguments;
    remaining_rev.reverse();
    eval_next_and(remaining_rev, environment, continuation)
}

fn eval_next_and(
    mut remaining_rev: Vec<Expr>,
    environment: Environment,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let expression = remaining_rev
        .pop()
        .expect("and should have at least one expression");
    if !remaining_rev.is_empty() {
        continuation.push(Frame::And {
            remaining_rev,
            environment: environment.clone(),
        });
    }

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn eval_or(
    arguments: Vec<Expr>,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if arguments.is_empty() {
        return Ok(State::Return {
            value: Value::Boolean(false),
            continuation,
        });
    }

    let mut remaining_rev = arguments;
    remaining_rev.reverse();
    eval_next_or(remaining_rev, environment, continuation)
}

fn eval_next_or(
    mut remaining_rev: Vec<Expr>,
    environment: Environment,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let expression = remaining_rev
        .pop()
        .expect("or should have at least one expression");
    if !remaining_rev.is_empty() {
        continuation.push(Frame::Or {
            remaining_rev,
            environment: environment.clone(),
        });
    }

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn eval_if(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let (condition, consequent, alternate) = match arguments {
        [condition, consequent] => (condition, consequent, None),
        [condition, consequent, alternate] => (condition, consequent, Some(alternate.clone())),
        _ => {
            return Err(EvalError::WrongArgumentCount {
                location,
                procedure: "if",
                expected: ArgCount::AtLeast(2),
                got: arguments.len(),
            });
        }
    };

    let mut next_continuation = continuation;
    next_continuation.push(Frame::If {
        consequent: consequent.clone(),
        alternate,
        environment: environment.clone(),
    });

    Ok(State::Eval {
        expression: condition.clone(),
        environment,
        continuation: next_continuation,
    })
}

fn eval_recursive_let(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    form: &'static str,
) -> Result<State, EvalError> {
    let [bindings_expression, body @ ..] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: form,
            expected: ArgCount::AtLeast(2),
            got: arguments.len(),
        });
    };

    let bindings = parse_bindings(bindings_expression, form, location)?;
    let scope = environment.child();
    bindings.iter().for_each(|(name, _)| {
        scope.define(name.clone(), Value::Uninitialized);
    });

    let mut pending_rev = bindings;
    pending_rev.reverse();
    start_recursive_let(
        RecursiveLetProgress {
            pending_rev,
            body: body.to_vec(),
            environment: scope,
            location,
            form,
        },
        continuation,
    )
}

fn start_recursive_let(
    mut progress: RecursiveLetProgress,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some((binding_name, expression)) = progress.pending_rev.pop() else {
        return eval_required_sequence(
            progress.body,
            progress.environment,
            progress.form,
            progress.location,
            continuation,
        );
    };

    continuation.push(Frame::RecursiveLet {
        binding_name,
        pending_rev: progress.pending_rev,
        body: progress.body,
        environment: progress.environment.clone(),
        location: progress.location,
        form: progress.form,
    });

    Ok(State::Eval {
        expression,
        environment: progress.environment,
        continuation,
    })
}

fn eval_define(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    match arguments {
        [Expr::Symbol { name, .. }, expression] => {
            let mut next_continuation = continuation;
            next_continuation.push(Frame::Define {
                name: name.clone(),
                environment: environment.clone(),
            });
            Ok(State::Eval {
                expression: expression.clone(),
                environment,
                continuation: next_continuation,
            })
        }
        [Expr::List {
            items: signature, ..
        }, body @ ..] => {
            define_function(signature, body, location, &environment, macro_environment).map(
                |value| State::Return {
                    value,
                    continuation,
                },
            )
        }
        _ => Err(EvalError::MalformedSpecialForm {
            location,
            form: "define",
        }),
    }
}

fn eval_define_syntax(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    macro_environment.define_syntax(arguments, location, &environment)?;

    Ok(State::Return {
        value: Value::Void,
        continuation,
    })
}

fn eval_set(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [Expr::Symbol {
        name,
        location: name_location,
    }, expression] = arguments
    else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "set!",
        });
    };

    let mut next_continuation = continuation;
    next_continuation.push(Frame::Set {
        name: name.clone(),
        name_location: *name_location,
        environment: environment.clone(),
    });

    Ok(State::Eval {
        expression: expression.clone(),
        environment,
        continuation: next_continuation,
    })
}

fn eval_quote(
    arguments: &[Expr],
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [expression] = arguments else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "quote",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(State::Return {
        value: quote_expression(expression),
        continuation,
    })
}

fn eval_lambda(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    let Some((parameters, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "lambda",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };

    let formals = parse_lambda_formals(parameters, "lambda", location)?;
    let closure = build_closure(
        None,
        formals,
        body,
        environment,
        "lambda",
        location,
        macro_environment,
    )?;

    Ok(State::Return {
        value: Value::Closure(closure),
        continuation,
    })
}

fn eval_let(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    match arguments {
        [] => Err(EvalError::WrongArgumentCount {
            location,
            procedure: "let",
            expected: ArgCount::AtLeast(2),
            got: 0,
        }),
        [Expr::Symbol { name, .. }, bindings, body @ ..] => eval_named_let(
            name,
            bindings,
            body,
            location,
            environment,
            continuation,
            macro_environment,
        ),
        [bindings, body @ ..] => {
            eval_standard_let(bindings, body, location, environment, continuation)
        }
    }
}

fn eval_standard_let(
    bindings_expression: &Expr,
    body: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let bindings = parse_bindings(bindings_expression, "let", location)?;
    let (names, expressions): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    start_standard_let(
        names,
        expressions,
        body.to_vec(),
        environment,
        location,
        continuation,
    )
}

fn start_standard_let(
    names: Vec<String>,
    mut expressions: Vec<Expr>,
    body: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some(expression) = expressions.pop() else {
        let scope = environment.child();
        return bind_let_values_and_eval_body(
            names,
            Vec::new(),
            body,
            scope,
            location,
            continuation,
        );
    };

    continuation.push(Frame::StandardLet {
        names,
        pending: expressions,
        evaluated_rev: Vec::new(),
        body,
        environment: environment.clone(),
        location,
    });

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn bind_let_values_and_eval_body(
    names: Vec<String>,
    values: Vec<Value>,
    body: Vec<Expr>,
    scope: Environment,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    names
        .into_iter()
        .zip(values)
        .for_each(|(name, value)| scope.define(name, value));

    eval_required_sequence(body, scope, "let", location, continuation)
}

fn eval_named_let(
    name: &str,
    bindings_expression: &Expr,
    body: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    let bindings = parse_bindings(bindings_expression, "let", location)?;
    let (parameters, expressions): (Vec<_>, Vec<_>) = bindings.into_iter().unzip();
    start_named_let(
        NamedLetProgress {
            name: name.to_string(),
            parameters,
            pending: expressions,
            evaluated_rev: Vec::new(),
            body: body.to_vec(),
            environment,
            location,
        },
        continuation,
        macro_environment,
    )
}

fn start_named_let(
    mut progress: NamedLetProgress,
    mut continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    let Some(expression) = progress.pending.pop() else {
        return finish_named_let(progress, continuation, macro_environment);
    };

    continuation.push(Frame::NamedLet {
        name: progress.name,
        parameters: progress.parameters,
        pending: progress.pending,
        evaluated_rev: progress.evaluated_rev,
        body: progress.body,
        environment: progress.environment.clone(),
        location: progress.location,
    });

    Ok(State::Eval {
        expression,
        environment: progress.environment,
        continuation,
    })
}

fn finish_named_let(
    progress: NamedLetProgress,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    let NamedLetProgress {
        name,
        parameters,
        pending: _,
        mut evaluated_rev,
        body,
        environment,
        location,
    } = progress;
    evaluated_rev.reverse();

    let closure_environment = environment.child();
    let closure = build_closure(
        Some(name.clone()),
        ParsedFormals {
            required_parameters: parameters,
            rest_parameter: None,
        },
        &body,
        closure_environment.clone(),
        "let",
        location,
        macro_environment,
    )?;
    let callable = Value::Closure(closure);
    closure_environment.define(name, callable.clone());

    Ok(State::Apply {
        callable,
        arguments: evaluated_rev,
        location,
        continuation,
    })
}

fn eval_cond(
    clauses: Vec<Expr>,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let mut remaining_clauses_rev = clauses;
    remaining_clauses_rev.reverse();
    eval_reversed_cond_clauses(remaining_clauses_rev, environment, continuation)
}

fn eval_reversed_cond_clauses(
    mut remaining_clauses_rev: Vec<Expr>,
    environment: Environment,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some(clause) = remaining_clauses_rev.pop() else {
        return Ok(State::Return {
            value: Value::Void,
            continuation,
        });
    };

    let clause_location = clause.location();
    let Expr::List { items, .. } = clause else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "cond",
        });
    };
    let Some((test, body)) = split_first(items) else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "cond",
        });
    };

    if matches!(&test, Expr::Symbol { name, .. } if name == "else") {
        if !remaining_clauses_rev.is_empty() {
            return Err(EvalError::MalformedSpecialForm {
                location: test.location(),
                form: "cond",
            });
        }
        return eval_required_sequence(body, environment, "cond", clause_location, continuation);
    }

    continuation.push(Frame::CondClause {
        body,
        remaining_clauses_rev,
        environment: environment.clone(),
    });

    Ok(State::Eval {
        expression: test,
        environment,
        continuation,
    })
}

fn eval_case(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some((key, clauses)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "case",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };
    if clauses.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "case",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    }

    let mut clauses_rev = clauses.to_vec();
    clauses_rev.reverse();
    let mut next_continuation = continuation;
    next_continuation.push(Frame::Case {
        clauses_rev,
        environment: environment.clone(),
    });

    Ok(State::Eval {
        expression: key.clone(),
        environment,
        continuation: next_continuation,
    })
}

fn eval_guard(
    arguments: &[Expr],
    location: SourceLocation,
    environment: Environment,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some((specification, body)) = arguments.split_first() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "guard",
            expected: ArgCount::AtLeast(2),
            got: 0,
        });
    };
    if body.is_empty() {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "guard",
            expected: ArgCount::AtLeast(2),
            got: 1,
        });
    }

    let (variable, clauses) = parse_guard_spec(specification, location)?;
    continuation.push(Frame::ExceptionHandler {
        handler: ExceptionHandler::Guard {
            variable,
            clauses,
            environment: environment.clone(),
        },
    });

    eval_required_sequence(body.to_vec(), environment, "guard", location, continuation)
}

fn eval_sequence(
    expressions: Vec<Expr>,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let mut remaining_rev = expressions;
    remaining_rev.reverse();
    eval_reversed_sequence(remaining_rev, environment, continuation)
}

fn eval_reversed_sequence(
    mut remaining_rev: Vec<Expr>,
    environment: Environment,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some(expression) = remaining_rev.pop() else {
        return Ok(State::Return {
            value: Value::Void,
            continuation,
        });
    };

    if !remaining_rev.is_empty() {
        continuation.push(Frame::Sequence {
            remaining_rev,
            environment: environment.clone(),
        });
    }

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn eval_required_sequence(
    body: Vec<Expr>,
    environment: Environment,
    form: &'static str,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

    eval_sequence(body, environment, continuation)
}

fn eval_guard_handler(
    variable: String,
    clauses: Vec<Expr>,
    environment: Environment,
    exception: RaisedException,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let scope = environment.child();
    scope.define(variable, exception.value().clone());

    let mut remaining_clauses_rev = clauses;
    remaining_clauses_rev.reverse();
    eval_reversed_guard_clauses(remaining_clauses_rev, scope, exception, continuation)
}

fn eval_reversed_guard_clauses(
    mut remaining_clauses_rev: Vec<Expr>,
    environment: Environment,
    exception: RaisedException,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some(clause) = remaining_clauses_rev.pop() else {
        return Ok(State::Raise {
            exception,
            continuation,
        });
    };

    let clause_location = clause.location();
    let Expr::List { items, .. } = clause else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "guard",
        });
    };
    let Some((test, body)) = split_first(items) else {
        return Err(EvalError::MalformedSpecialForm {
            location: clause_location,
            form: "guard",
        });
    };

    if matches!(&test, Expr::Symbol { name, .. } if name == "else") {
        if !remaining_clauses_rev.is_empty() {
            return Err(EvalError::MalformedSpecialForm {
                location: test.location(),
                form: "guard",
            });
        }
        return eval_required_sequence(body, environment, "guard", clause_location, continuation);
    }

    continuation.push(Frame::GuardClause {
        body,
        remaining_clauses_rev,
        environment: environment.clone(),
        exception,
    });

    Ok(State::Eval {
        expression: test,
        environment,
        continuation,
    })
}

fn continue_with_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    match frame {
        Frame::Sequence { .. }
        | Frame::If { .. }
        | Frame::Define { .. }
        | Frame::Set { .. }
        | Frame::And { .. }
        | Frame::Or { .. } => continue_control_frame(frame, value, continuation),
        Frame::ApplyOperator { .. } | Frame::ApplyArgument { .. } => {
            continue_apply_frame(frame, value, continuation)
        }
        Frame::StandardLet { .. } | Frame::NamedLet { .. } | Frame::RecursiveLet { .. } => {
            continue_binding_frame(frame, value, continuation, macro_environment)
        }
        Frame::CondClause { .. } | Frame::GuardClause { .. } | Frame::Case { .. } => {
            continue_branch_frame(frame, value, continuation)
        }
        Frame::DynamicWindBefore { .. }
        | Frame::DynamicWindExit { .. }
        | Frame::DynamicWindAfter { .. }
        | Frame::DynamicWindContext { .. }
        | Frame::ExceptionHandler { .. }
        | Frame::ExceptionTransition { .. }
        | Frame::ContinuationTransition { .. } => continue_effect_frame(frame, value, continuation),
    }
}

fn continue_control_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    match frame {
        Frame::Sequence {
            remaining_rev,
            environment,
        } => continue_sequence(remaining_rev, environment, continuation),
        Frame::If {
            consequent,
            alternate,
            environment,
        } => continue_if(consequent, alternate, environment, value, continuation),
        Frame::Define { name, environment } => {
            continue_define(name, environment, value, continuation)
        }
        Frame::Set {
            name,
            name_location,
            environment,
        } => continue_set(name, name_location, environment, value, continuation),
        Frame::And {
            remaining_rev,
            environment,
        } => continue_and(remaining_rev, environment, value, continuation),
        Frame::Or {
            remaining_rev,
            environment,
        } => continue_or(remaining_rev, environment, value, continuation),
        _ => unreachable!("non-control frame routed to continue_control_frame"),
    }
}

fn continue_apply_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    match frame {
        Frame::ApplyOperator {
            arguments,
            environment,
            location,
        } => continue_apply_operator(arguments, environment, location, value, continuation),
        Frame::ApplyArgument {
            callable,
            pending,
            evaluated_rev,
            environment,
            location,
        } => continue_apply_argument(
            callable,
            pending,
            evaluated_rev,
            environment,
            location,
            value,
            continuation,
        ),
        _ => unreachable!("non-apply frame routed to continue_apply_frame"),
    }
}

fn continue_binding_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    match frame {
        Frame::StandardLet {
            names,
            pending,
            evaluated_rev,
            body,
            environment,
            location,
        } => continue_standard_let(
            StandardLetProgress {
                names,
                pending,
                evaluated_rev,
                body,
                environment,
                location,
            },
            value,
            continuation,
        ),
        Frame::NamedLet {
            name,
            parameters,
            pending,
            evaluated_rev,
            body,
            environment,
            location,
        } => continue_named_let(
            NamedLetProgress {
                name,
                parameters,
                pending,
                evaluated_rev,
                body,
                environment,
                location,
            },
            value,
            continuation,
            macro_environment,
        ),
        Frame::RecursiveLet {
            binding_name,
            pending_rev,
            body,
            environment,
            location,
            form,
        } => continue_recursive_let(
            binding_name,
            RecursiveLetProgress {
                pending_rev,
                body,
                environment,
                location,
                form,
            },
            value,
            continuation,
        ),
        _ => unreachable!("non-binding frame routed to continue_binding_frame"),
    }
}

fn continue_branch_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    match frame {
        Frame::CondClause {
            body,
            remaining_clauses_rev,
            environment,
        } => continue_cond_clause(
            body,
            remaining_clauses_rev,
            environment,
            value,
            continuation,
        ),
        Frame::GuardClause {
            body,
            remaining_clauses_rev,
            environment,
            exception,
        } => continue_guard_clause(
            body,
            remaining_clauses_rev,
            environment,
            exception,
            value,
            continuation,
        ),
        Frame::Case {
            clauses_rev,
            environment,
        } => continue_case(clauses_rev, environment, value, continuation),
        _ => unreachable!("non-branch frame routed to continue_branch_frame"),
    }
}

fn continue_effect_frame(
    frame: Frame,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    match frame {
        Frame::DynamicWindBefore { body, wind } => {
            continue_dynamic_wind_before(body, wind, continuation)
        }
        Frame::DynamicWindExit { wind } => continue_dynamic_wind_exit(wind, value, continuation),
        Frame::DynamicWindAfter { value } => continue_dynamic_wind_after(value, continuation),
        Frame::DynamicWindContext { .. } => continue_dynamic_wind_context(value, continuation),
        Frame::ExceptionHandler { .. } => continue_exception_handler(value, continuation),
        Frame::ExceptionTransition {
            active_winds,
            exit_winds,
            target_continuation,
            handler,
            exception,
        } => continue_exception_transition(
            active_winds,
            exit_winds,
            target_continuation,
            handler,
            exception,
        ),
        Frame::ContinuationTransition {
            active_winds,
            exit_winds,
            enter_winds,
            target_continuation,
            value,
        } => continue_continuation_transition(
            active_winds,
            exit_winds,
            enter_winds,
            target_continuation,
            value,
        ),
        _ => unreachable!("non-effect frame routed to continue_effect_frame"),
    }
}

fn continue_sequence(
    remaining_rev: Vec<Expr>,
    environment: Environment,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    eval_reversed_sequence(remaining_rev, environment, continuation)
}

fn continue_if(
    consequent: Expr,
    alternate: Option<Expr>,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if value.is_truthy() {
        return Ok(State::Eval {
            expression: consequent,
            environment,
            continuation,
        });
    }

    let Some(alternate) = alternate else {
        return Ok(State::Return {
            value: Value::Void,
            continuation,
        });
    };

    Ok(State::Eval {
        expression: alternate,
        environment,
        continuation,
    })
}

fn continue_define(
    name: String,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    environment.define(name, value);
    Ok(State::Return {
        value: Value::Void,
        continuation,
    })
}

fn continue_set(
    name: String,
    name_location: SourceLocation,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if environment.set(&name, value) {
        return Ok(State::Return {
            value: Value::Void,
            continuation,
        });
    }

    Err(EvalError::UnboundVariable {
        location: name_location,
        name,
    })
}

fn continue_and(
    remaining_rev: Vec<Expr>,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if !value.is_truthy() {
        return Ok(State::Return {
            value,
            continuation,
        });
    }

    eval_next_and(remaining_rev, environment, continuation)
}

fn continue_or(
    remaining_rev: Vec<Expr>,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if value.is_truthy() {
        return Ok(State::Return {
            value,
            continuation,
        });
    }

    eval_next_or(remaining_rev, environment, continuation)
}

fn continue_apply_operator(
    arguments: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
    callable: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    start_argument_evaluation(callable, arguments, environment, location, continuation)
}

fn start_argument_evaluation(
    callable: Value,
    mut arguments: Vec<Expr>,
    environment: Environment,
    location: SourceLocation,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some(expression) = arguments.pop() else {
        return Ok(State::Apply {
            callable,
            arguments: Vec::new(),
            location,
            continuation,
        });
    };

    continuation.push(Frame::ApplyArgument {
        callable,
        pending: arguments,
        evaluated_rev: Vec::new(),
        environment: environment.clone(),
        location,
    });

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn continue_apply_argument(
    callable: Value,
    mut pending: Vec<Expr>,
    mut evaluated_rev: Vec<Value>,
    environment: Environment,
    location: SourceLocation,
    value: Value,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    evaluated_rev.push(value);
    let Some(expression) = pending.pop() else {
        evaluated_rev.reverse();
        return Ok(State::Apply {
            callable,
            arguments: evaluated_rev,
            location,
            continuation,
        });
    };

    continuation.push(Frame::ApplyArgument {
        callable,
        pending,
        evaluated_rev,
        environment: environment.clone(),
        location,
    });

    Ok(State::Eval {
        expression,
        environment,
        continuation,
    })
}

fn continue_standard_let(
    mut progress: StandardLetProgress,
    value: Value,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    progress.evaluated_rev.push(value);
    let Some(expression) = progress.pending.pop() else {
        progress.evaluated_rev.reverse();
        let scope = progress.environment.child();
        return bind_let_values_and_eval_body(
            progress.names,
            progress.evaluated_rev,
            progress.body,
            scope,
            progress.location,
            continuation,
        );
    };

    continuation.push(Frame::StandardLet {
        names: progress.names,
        pending: progress.pending,
        evaluated_rev: progress.evaluated_rev,
        body: progress.body,
        environment: progress.environment.clone(),
        location: progress.location,
    });

    Ok(State::Eval {
        expression,
        environment: progress.environment,
        continuation,
    })
}

fn continue_named_let(
    mut progress: NamedLetProgress,
    value: Value,
    mut continuation: ContinuationFrames,
    macro_environment: &MacroEnvironment,
) -> Result<State, EvalError> {
    progress.evaluated_rev.push(value);
    let Some(expression) = progress.pending.pop() else {
        return finish_named_let(progress, continuation, macro_environment);
    };

    continuation.push(Frame::NamedLet {
        name: progress.name,
        parameters: progress.parameters,
        pending: progress.pending,
        evaluated_rev: progress.evaluated_rev,
        body: progress.body,
        environment: progress.environment.clone(),
        location: progress.location,
    });

    Ok(State::Eval {
        expression,
        environment: progress.environment,
        continuation,
    })
}

fn continue_cond_clause(
    body: Vec<Expr>,
    remaining_clauses_rev: Vec<Expr>,
    environment: Environment,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if !value.is_truthy() {
        return eval_reversed_cond_clauses(remaining_clauses_rev, environment, continuation);
    }

    if body.is_empty() {
        return Ok(State::Return {
            value,
            continuation,
        });
    }

    eval_sequence(body, environment, continuation)
}

fn continue_guard_clause(
    body: Vec<Expr>,
    remaining_clauses_rev: Vec<Expr>,
    environment: Environment,
    exception: RaisedException,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if !value.is_truthy() {
        return eval_reversed_guard_clauses(
            remaining_clauses_rev,
            environment,
            exception,
            continuation,
        );
    }

    if body.is_empty() {
        return Ok(State::Return {
            value,
            continuation,
        });
    }

    eval_sequence(body, environment, continuation)
}

fn continue_dynamic_wind_before(
    body: Value,
    wind: Rc<DynamicWind>,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    continuation.push(Frame::DynamicWindExit { wind: wind.clone() });
    Ok(State::Apply {
        callable: body,
        arguments: Vec::new(),
        location: wind.location(),
        continuation,
    })
}

fn continue_dynamic_wind_exit(
    wind: Rc<DynamicWind>,
    value: Value,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    continuation.push(Frame::DynamicWindAfter { value });
    Ok(State::Apply {
        callable: wind.after(),
        arguments: Vec::new(),
        location: wind.location(),
        continuation,
    })
}

fn continue_dynamic_wind_after(
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    Ok(State::Return {
        value,
        continuation,
    })
}

fn continue_dynamic_wind_context(
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    Ok(State::Return {
        value,
        continuation,
    })
}

fn continue_exception_handler(
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    Ok(State::Return {
        value,
        continuation,
    })
}

fn continue_exception_transition(
    mut active_winds: Vec<Rc<DynamicWind>>,
    mut exit_winds: Vec<Rc<DynamicWind>>,
    target_continuation: ContinuationFrames,
    handler: ExceptionHandler,
    exception: RaisedException,
) -> Result<State, EvalError> {
    if let Some(wind) = exit_winds.first().cloned() {
        let popped = active_winds
            .pop()
            .expect("exception transition should only exit active winds");
        debug_assert!(
            Rc::ptr_eq(&popped, &wind),
            "exception transition should unwind from innermost wind"
        );
        exit_winds.remove(0);
        let continuation = transition_continuation(
            &active_winds,
            Frame::ExceptionTransition {
                active_winds: active_winds.clone(),
                exit_winds,
                target_continuation,
                handler,
                exception,
            },
        );
        return Ok(State::Apply {
            callable: wind.after(),
            arguments: Vec::new(),
            location: wind.location(),
            continuation,
        });
    }

    invoke_exception_handler(handler, exception, target_continuation)
}

fn continue_continuation_transition(
    mut active_winds: Vec<Rc<DynamicWind>>,
    mut exit_winds: Vec<Rc<DynamicWind>>,
    mut enter_winds: Vec<Rc<DynamicWind>>,
    target_continuation: ContinuationFrames,
    value: Value,
) -> Result<State, EvalError> {
    if let Some(wind) = exit_winds.first().cloned() {
        let popped = active_winds
            .pop()
            .expect("continuation transition should only exit active winds");
        debug_assert!(
            Rc::ptr_eq(&popped, &wind),
            "continuation transition should unwind from innermost wind"
        );
        exit_winds.remove(0);
        let continuation = transition_continuation(
            &active_winds,
            Frame::ContinuationTransition {
                active_winds: active_winds.clone(),
                exit_winds,
                enter_winds,
                target_continuation,
                value,
            },
        );
        return Ok(State::Apply {
            callable: wind.after(),
            arguments: Vec::new(),
            location: wind.location(),
            continuation,
        });
    }

    if let Some(wind) = enter_winds.first().cloned() {
        let mut next_active = active_winds.clone();
        next_active.push(wind.clone());
        enter_winds.remove(0);
        let continuation = transition_continuation(
            &active_winds,
            Frame::ContinuationTransition {
                active_winds: next_active,
                exit_winds,
                enter_winds,
                target_continuation,
                value,
            },
        );
        return Ok(State::Apply {
            callable: wind.before(),
            arguments: Vec::new(),
            location: wind.location(),
            continuation,
        });
    }

    Ok(State::Return {
        value,
        continuation: target_continuation,
    })
}

fn transition_continuation(active_winds: &[Rc<DynamicWind>], frame: Frame) -> ContinuationFrames {
    let mut continuation = active_winds
        .iter()
        .cloned()
        .map(|wind| Frame::DynamicWindContext { wind })
        .collect::<Vec<_>>();
    continuation.push(frame);
    continuation
}

fn continue_recursive_let(
    binding_name: String,
    progress: RecursiveLetProgress,
    value: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let did_set = progress.environment.set(&binding_name, value);
    debug_assert!(
        did_set,
        "recursive let binding should exist before evaluation"
    );
    start_recursive_let(progress, continuation)
}

fn continue_case(
    clauses_rev: Vec<Expr>,
    environment: Environment,
    key: Value,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some((body, clause_location)) = find_matching_case_clause(&key, clauses_rev)? else {
        return Ok(State::Return {
            value: Value::Void,
            continuation,
        });
    };

    eval_required_sequence(body, environment, "case", clause_location, continuation)
}

fn apply_value(
    callable: Value,
    arguments: Vec<Value>,
    location: SourceLocation,
    continuation: ContinuationFrames,
    output: &mut String,
) -> Result<State, EvalError> {
    match callable {
        Value::Builtin(procedure) => {
            apply_builtin(procedure, &arguments, location, output).map(|value| State::Return {
                value,
                continuation,
            })
        }
        Value::Closure(closure) => apply_closure(closure, arguments, location, continuation),
        Value::CallWithCurrentContinuation => {
            apply_call_with_current_continuation(arguments, location, continuation)
        }
        Value::DynamicWind => apply_dynamic_wind(arguments, location, continuation),
        Value::Raise => apply_raise(arguments, location, continuation),
        Value::WithExceptionHandler => {
            apply_with_exception_handler(arguments, location, continuation)
        }
        Value::Continuation(captured) => {
            apply_continuation(captured, arguments, location, continuation)
        }
        other => Err(EvalError::NotCallable {
            location,
            expression: other.render(),
        }),
    }
}

fn apply_closure(
    closure: Closure,
    arguments: Vec<Value>,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    if !closure_accepts_argument_count(&closure, arguments.len()) {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "lambda",
            expected: closure_expected_arg_count(&closure),
            got: arguments.len(),
        });
    }

    let call_environment = closure.environment.child();
    let (required_arguments, rest_arguments) = arguments.split_at(closure.parameters.len());

    closure
        .parameters
        .iter()
        .cloned()
        .zip(required_arguments.iter().cloned())
        .for_each(|(name, value)| call_environment.define(name, value));
    if let Some(rest_parameter) = &closure.rest_parameter {
        call_environment.define(rest_parameter.clone(), list_from_values(rest_arguments));
    }

    eval_required_sequence(
        closure.body,
        call_environment,
        "lambda",
        location,
        continuation,
    )
}

fn apply_call_with_current_continuation(
    arguments: Vec<Value>,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [callable] = arguments.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "call/cc",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(State::Apply {
        callable: callable.clone(),
        arguments: vec![Value::Continuation(Rc::new(CapturedContinuation::new(
            continuation.clone(),
        )))],
        location,
        continuation,
    })
}

fn apply_dynamic_wind(
    arguments: Vec<Value>,
    location: SourceLocation,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [before, body, after] = arguments.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "dynamic-wind",
            expected: ArgCount::Exactly(3),
            got: arguments.len(),
        });
    };

    let wind = Rc::new(DynamicWind::new(before.clone(), after.clone(), location));
    continuation.push(Frame::DynamicWindBefore {
        body: body.clone(),
        wind,
    });

    Ok(State::Apply {
        callable: before.clone(),
        arguments: Vec::new(),
        location,
        continuation,
    })
}

fn apply_raise(
    arguments: Vec<Value>,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [value] = arguments.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "raise",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    Ok(State::Raise {
        exception: RaisedException::new(value.clone(), location),
        continuation,
    })
}

fn apply_with_exception_handler(
    arguments: Vec<Value>,
    location: SourceLocation,
    mut continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [handler, thunk] = arguments.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "with-exception-handler",
            expected: ArgCount::Exactly(2),
            got: arguments.len(),
        });
    };

    continuation.push(Frame::ExceptionHandler {
        handler: ExceptionHandler::Procedure {
            callable: handler.clone(),
            location,
        },
    });

    Ok(State::Apply {
        callable: thunk.clone(),
        arguments: Vec::new(),
        location,
        continuation,
    })
}

fn apply_continuation(
    captured: Rc<CapturedContinuation>,
    arguments: Vec<Value>,
    location: SourceLocation,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let [value] = arguments.as_slice() else {
        return Err(EvalError::WrongArgumentCount {
            location,
            procedure: "continuation",
            expected: ArgCount::Exactly(1),
            got: arguments.len(),
        });
    };

    resume_continuation(
        continuation,
        normalize_resumed_continuation(captured.frames(), value),
        value.clone(),
    )
}

fn raise_exception(
    exception: RaisedException,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    let Some((handler, target_continuation)) =
        split_continuation_at_exception_handler(continuation.clone())
    else {
        return Err(EvalError::UncaughtException {
            location: exception.location(),
            value: exception.value().render(),
        });
    };

    let current_winds = continuation_winds(&continuation);
    let target_winds = continuation_winds(&target_continuation);
    let shared_prefix = shared_wind_prefix(&current_winds, &target_winds);

    continue_exception_transition(
        current_winds.clone(),
        current_winds[shared_prefix..]
            .iter()
            .rev()
            .cloned()
            .collect(),
        target_continuation,
        handler,
        exception,
    )
}

fn split_continuation_at_exception_handler(
    mut continuation: ContinuationFrames,
) -> Option<(ExceptionHandler, ContinuationFrames)> {
    while let Some(frame) = continuation.pop() {
        if let Frame::ExceptionHandler { handler } = frame {
            return Some((handler, continuation));
        }
    }

    None
}

fn invoke_exception_handler(
    handler: ExceptionHandler,
    exception: RaisedException,
    continuation: ContinuationFrames,
) -> Result<State, EvalError> {
    match handler {
        ExceptionHandler::Procedure { callable, location } => Ok(State::Apply {
            callable,
            arguments: vec![exception.value().clone()],
            location,
            continuation,
        }),
        ExceptionHandler::Guard {
            variable,
            clauses,
            environment,
        } => eval_guard_handler(variable, clauses, environment, exception, continuation),
    }
}

fn resume_continuation(
    current_continuation: ContinuationFrames,
    target_continuation: ContinuationFrames,
    value: Value,
) -> Result<State, EvalError> {
    let current_winds = continuation_winds(&current_continuation);
    let target_winds = continuation_winds(&target_continuation);
    let shared_prefix = shared_wind_prefix(&current_winds, &target_winds);

    continue_continuation_transition(
        current_winds.clone(),
        current_winds[shared_prefix..]
            .iter()
            .rev()
            .cloned()
            .collect(),
        target_winds[shared_prefix..].to_vec(),
        target_continuation,
        value,
    )
}

fn continuation_winds(continuation: &[Frame]) -> Vec<Rc<DynamicWind>> {
    continuation
        .iter()
        .filter_map(active_dynamic_wind)
        .collect()
}

fn active_dynamic_wind(frame: &Frame) -> Option<Rc<DynamicWind>> {
    match frame {
        Frame::DynamicWindExit { wind } | Frame::DynamicWindContext { wind } => Some(wind.clone()),
        _ => None,
    }
}

fn shared_wind_prefix(left: &[Rc<DynamicWind>], right: &[Rc<DynamicWind>]) -> usize {
    left.iter()
        .zip(right)
        .take_while(|(left, right)| Rc::ptr_eq(left, right))
        .count()
}

fn normalize_resumed_continuation(
    mut continuation: ContinuationFrames,
    value: &Value,
) -> ContinuationFrames {
    if !matches!(value, Value::Boolean(false)) {
        return continuation;
    }

    if continuation
        .last()
        .is_some_and(replays_single_pending_call_cc)
    {
        continuation.pop();
    }

    continuation
}

fn replays_single_pending_call_cc(frame: &Frame) -> bool {
    let Frame::Sequence { remaining_rev, .. } = frame else {
        return false;
    };

    remaining_rev.len() == 1
        && remaining_rev
            .last()
            .is_some_and(is_call_with_current_continuation_expression)
}

fn is_call_with_current_continuation_expression(expression: &Expr) -> bool {
    matches!(
        expression,
        Expr::List { items, .. }
            if matches!(
                items.first(),
                Some(Expr::Symbol { name, .. })
                    if name == "call/cc" || name == "call-with-current-continuation"
            )
    )
}

fn parse_bindings(
    bindings_expression: &Expr,
    form: &'static str,
    location: SourceLocation,
) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List { items, .. } = bindings_expression else {
        return Err(EvalError::MalformedSpecialForm { location, form });
    };

    items
        .iter()
        .map(|binding| parse_binding(binding, form))
        .collect()
}

fn parse_binding(binding: &Expr, form: &'static str) -> Result<(String, Expr), EvalError> {
    let Expr::List { items, .. } = binding else {
        return Err(EvalError::MalformedSpecialForm {
            location: binding.location(),
            form,
        });
    };
    let [Expr::Symbol { name, .. }, expression] = items.as_slice() else {
        return Err(EvalError::MalformedSpecialForm {
            location: binding.location(),
            form,
        });
    };

    Ok((name.clone(), expression.clone()))
}

fn find_matching_case_clause(
    key: &Value,
    mut clauses_rev: Vec<Expr>,
) -> Result<Option<(Vec<Expr>, SourceLocation)>, EvalError> {
    while let Some(clause) = clauses_rev.pop() {
        let (datum_expression, body, clause_location) = parse_case_clause(clause)?;

        if matches!(&datum_expression, Expr::Symbol { name, .. } if name == "else") {
            validate_case_else_position(&datum_expression, clauses_rev.is_empty())?;
            return Ok(Some((body, clause_location)));
        }

        if case_clause_matches(key, &datum_expression, clause_location)? {
            return Ok(Some((body, clause_location)));
        }
    }

    Ok(None)
}

fn parse_case_clause(clause: Expr) -> Result<(Expr, Vec<Expr>, SourceLocation), EvalError> {
    let clause_location = clause.location();
    let Expr::List { items, .. } = clause else {
        return Err(malformed_case(clause_location));
    };
    let Some((datum_expression, body)) = split_first(items) else {
        return Err(malformed_case(clause_location));
    };

    Ok((datum_expression, body, clause_location))
}

fn validate_case_else_position(datum_expression: &Expr, is_last: bool) -> Result<(), EvalError> {
    if is_last {
        return Ok(());
    }

    Err(malformed_case(datum_expression.location()))
}

fn case_clause_matches(
    key: &Value,
    datum_expression: &Expr,
    clause_location: SourceLocation,
) -> Result<bool, EvalError> {
    let Expr::List { items: datums, .. } = datum_expression else {
        return Err(malformed_case(clause_location));
    };

    Ok(datums
        .iter()
        .any(|datum| is_eqv(key, &quote_expression(datum))))
}

fn malformed_case(location: SourceLocation) -> EvalError {
    EvalError::MalformedSpecialForm {
        location,
        form: "case",
    }
}

fn parse_guard_spec(
    specification: &Expr,
    location: SourceLocation,
) -> Result<(String, Vec<Expr>), EvalError> {
    let Expr::List { items, .. } = specification else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "guard",
        });
    };
    let Some((variable, clauses)) = items.split_first() else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "guard",
        });
    };
    let Expr::Symbol { name, .. } = variable else {
        return Err(EvalError::MalformedSpecialForm {
            location: variable.location(),
            form: "guard",
        });
    };

    Ok((name.clone(), clauses.to_vec()))
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    location: SourceLocation,
    environment: &Environment,
    macro_environment: &MacroEnvironment,
) -> Result<Value, EvalError> {
    let Some((Expr::Symbol { name, .. }, parameters)) = signature.split_first() else {
        return Err(EvalError::MalformedSpecialForm {
            location,
            form: "define",
        });
    };

    let formals = parse_formals(parameters, "define")?;
    let closure = build_closure(
        Some(name.clone()),
        formals,
        body,
        environment.clone(),
        "define",
        location,
        macro_environment,
    )?;

    environment.define(name.clone(), Value::Closure(closure));
    Ok(Value::Void)
}

fn build_closure(
    name: Option<String>,
    formals: ParsedFormals,
    body: &[Expr],
    environment: Environment,
    form: &'static str,
    location: SourceLocation,
    macro_environment: &MacroEnvironment,
) -> Result<Closure, EvalError> {
    if body.is_empty() {
        return Err(EvalError::MissingBody { location, form });
    }

    let expanded_body = expand_expressions(body, macro_environment)?;

    Ok(Closure::new(
        name,
        formals.required_parameters,
        formals.rest_parameter,
        expanded_body,
        environment,
    ))
}

fn expand_expressions(
    expressions: &[Expr],
    macro_environment: &MacroEnvironment,
) -> Result<Vec<Expr>, EvalError> {
    expressions
        .iter()
        .map(|expression| macro_environment.expand_expression(expression))
        .collect()
}

fn parse_lambda_formals(
    parameters: &Expr,
    form: &'static str,
    location: SourceLocation,
) -> Result<ParsedFormals, EvalError> {
    match parameters {
        Expr::Symbol { .. } => Ok(ParsedFormals {
            required_parameters: Vec::new(),
            rest_parameter: Some(parse_parameter_name(parameters, form)?),
        }),
        Expr::List { items, .. } => parse_formals(items, form),
        _ => Err(EvalError::InvalidParameterList { location, form }),
    }
}

fn parse_formals(parameters: &[Expr], form: &'static str) -> Result<ParsedFormals, EvalError> {
    let Some(dot_index) = parameters.iter().position(is_dot_parameter) else {
        return parameters
            .iter()
            .map(|parameter| parse_parameter_name(parameter, form))
            .collect::<Result<Vec<_>, _>>()
            .map(|required_parameters| ParsedFormals {
                required_parameters,
                rest_parameter: None,
            });
    };
    let (required_parameters, dotted_tail) = parameters.split_at(dot_index);
    let [_, rest_parameter] = dotted_tail else {
        return Err(EvalError::InvalidParameterList {
            location: parameters[dot_index].location(),
            form,
        });
    };

    Ok(ParsedFormals {
        required_parameters: required_parameters
            .iter()
            .map(|parameter| parse_parameter_name(parameter, form))
            .collect::<Result<_, _>>()?,
        rest_parameter: Some(parse_parameter_name(rest_parameter, form)?),
    })
}

fn parse_parameter_name(parameter: &Expr, form: &'static str) -> Result<String, EvalError> {
    match parameter {
        Expr::Symbol { name, location } if name == "." => Err(EvalError::InvalidParameterList {
            location: *location,
            form,
        }),
        Expr::Symbol { name, .. } => Ok(name.clone()),
        _ => Err(EvalError::NonSymbolParameter {
            location: parameter.location(),
            form,
        }),
    }
}

fn is_dot_parameter(parameter: &Expr) -> bool {
    matches!(parameter, Expr::Symbol { name, .. } if name == ".")
}

fn closure_accepts_argument_count(closure: &Closure, provided: usize) -> bool {
    closure
        .rest_parameter
        .as_ref()
        .map_or(provided == closure.parameters.len(), |_| {
            provided >= closure.parameters.len()
        })
}

fn closure_expected_arg_count(closure: &Closure) -> ArgCount {
    closure
        .rest_parameter
        .as_ref()
        .map_or(ArgCount::Exactly(closure.parameters.len()), |_| {
            ArgCount::AtLeast(closure.parameters.len())
        })
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

fn split_first<T>(items: Vec<T>) -> Option<(T, Vec<T>)> {
    let mut iter = items.into_iter();
    let first = iter.next()?;
    Some((first, iter.collect()))
}
