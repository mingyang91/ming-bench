use std::collections::HashSet;
use std::rc::Rc;

use crate::scheme::builtin::{Builtin, BUILTINS};
use crate::scheme::continuation::{Continuation, Frame, LetBinding};
use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::procedure::{Parameters, Procedure};
use crate::scheme::value::Value;

enum MachineState {
    Expression(Expr, Environment),
    Value(Value),
}

pub(crate) fn eval_str(input: &str) -> Result<String, SchemeError> {
    let program = parser::parse_program(input)?;
    let value = eval_program(&program)?;
    Ok(value.to_string())
}

fn eval_program(program: &[Expr]) -> Result<Value, SchemeError> {
    let environment = Environment::new();
    define_builtins(&environment);
    eval_sequence(program, &environment)
}

fn define_builtins(environment: &Environment) {
    for (name, builtin) in BUILTINS {
        environment.define(name, Value::Builtin(builtin));
    }
}

fn eval_sequence(expressions: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let mut stack = Vec::new();
    let mut state = start_sequence(expressions.to_vec(), environment.clone(), &mut stack)?;

    loop {
        state = match state {
            MachineState::Expression(expression, environment) => {
                eval_expression(expression, environment, &mut stack)?
            }
            MachineState::Value(value) => match stack.pop() {
                Some(frame) => continue_from_frame(frame, value, &mut stack)?,
                None => return Ok(value),
            },
        };
    }
}

fn start_sequence(
    expressions: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let Some((first_expression, remaining)) = expressions.split_first() else {
        unreachable!("sequences are validated before evaluation");
    };

    if !remaining.is_empty() {
        stack.push(Frame::Sequence {
            remaining: remaining.to_vec(),
            environment: environment.clone(),
        });
    }

    Ok(MachineState::Expression(
        first_expression.clone(),
        environment,
    ))
}

fn eval_expression(
    expression: Expr,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if let Some(value) = Value::from_literal(&expression) {
        return Ok(MachineState::Value(value));
    }

    match expression {
        Expr::Symbol(name) => Ok(MachineState::Value(
            environment
                .get(&name)
                .ok_or(SchemeError::UnboundSymbol { name })?,
        )),
        Expr::List(expressions) => eval_application(expressions, environment, stack),
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => {
            unreachable!("literal expressions are handled before matching")
        }
    }
}

fn eval_application(
    expressions: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let Some((operator, operands)) = expressions.split_first() else {
        return Err(SchemeError::EmptyApplication);
    };

    if let Expr::Symbol(name) = operator {
        if let Some(state) = eval_special_form(name, operands, &environment, stack)? {
            return Ok(state);
        }
    }

    stack.push(Frame::ApplyOperator {
        operands: operands.to_vec(),
        environment: environment.clone(),
    });

    Ok(MachineState::Expression(operator.clone(), environment))
}

fn eval_special_form(
    operator: &str,
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<Option<MachineState>, SchemeError> {
    match operator {
        "define" => eval_define(operands, environment, stack).map(Some),
        "set!" => eval_set(operands, environment, stack).map(Some),
        "if" => eval_if(operands, environment, stack).map(Some),
        "quote" => eval_quote(operands).map(|value| Some(MachineState::Value(value))),
        "lambda" => {
            eval_lambda(operands, environment).map(|value| Some(MachineState::Value(value)))
        }
        "and" => start_and(operands.to_vec(), environment.clone(), stack).map(Some),
        "or" => start_or(operands.to_vec(), environment.clone(), stack).map(Some),
        "begin" => eval_begin(operands, environment, stack).map(Some),
        "cond" => eval_cond(operands, environment, stack).map(Some),
        "let" => eval_let(operands, environment, stack).map(Some),
        _ => Ok(None),
    }
}

fn eval_define(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            stack.push(Frame::Define {
                name: name.clone(),
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(
                value_expression.clone(),
                environment.clone(),
            ))
        }
        [Expr::Symbol(_), ..] => Err(SchemeError::WrongArgumentCount {
            operator: "define",
            expected: 2,
            actual: operands.len(),
        }),
        [Expr::List(signature), body @ ..] if !body.is_empty() => {
            define_function(signature, body, environment)
        }
        [Expr::List(_)] => Err(SchemeError::TooFewArguments {
            operator: "define",
            min: 2,
            actual: operands.len(),
        }),
        [target, _] | [target, ..] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        _ => Err(SchemeError::TooFewArguments {
            operator: "define",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_set(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            stack.push(Frame::Set {
                name: name.clone(),
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(
                value_expression.clone(),
                environment.clone(),
            ))
        }
        [Expr::Symbol(_), ..] => Err(SchemeError::WrongArgumentCount {
            operator: "set!",
            expected: 2,
            actual: operands.len(),
        }),
        [target, ..] => Err(SchemeError::InvalidAssignmentTarget {
            found: expression_kind(target),
        }),
        [] => Err(SchemeError::WrongArgumentCount {
            operator: "set!",
            expected: 2,
            actual: 0,
        }),
    }
}

fn define_function(
    signature: &[Expr],
    body: &[Expr],
    environment: &Environment,
) -> Result<MachineState, SchemeError> {
    let Some((name, parameters)) = signature.split_first() else {
        return Err(SchemeError::InvalidDefinitionTarget { found: "list" });
    };

    let Expr::Symbol(name) = name else {
        return Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(name),
        });
    };

    let parameters = parse_parameters(parameters, "define")?;
    let procedure = Value::procedure(parameters, body.to_vec(), environment.clone());
    environment.define(name, procedure);
    Ok(MachineState::Value(Value::Void))
}

fn eval_if(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match operands {
        [condition, consequent, alternative] => {
            stack.push(Frame::If {
                consequent: consequent.clone(),
                alternative: alternative.clone(),
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(
                condition.clone(),
                environment.clone(),
            ))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "if",
            expected: 3,
            actual: operands.len(),
        }),
    }
}

fn eval_quote(operands: &[Expr]) -> Result<Value, SchemeError> {
    match operands {
        [expression] => Ok(Value::from_quoted_expr(expression)),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "quote",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_lambda(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [parameters, body @ ..] if !body.is_empty() => {
            let parameters = parse_parameter_list(parameters, "lambda")?;
            Ok(Value::procedure(
                parameters,
                body.to_vec(),
                environment.clone(),
            ))
        }
        _ => Err(SchemeError::TooFewArguments {
            operator: "lambda",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_begin(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if operands.is_empty() {
        Err(SchemeError::TooFewArguments {
            operator: "begin",
            min: 1,
            actual: 0,
        })
    } else {
        start_sequence(operands.to_vec(), environment.clone(), stack)
    }
}

fn eval_cond(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if operands.is_empty() {
        return Err(SchemeError::TooFewArguments {
            operator: "cond",
            min: 1,
            actual: 0,
        });
    }

    validate_cond_clauses(operands)?;
    continue_cond(operands.to_vec(), environment.clone(), stack)
}

fn continue_cond(
    clauses: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let Some((clause, remaining_clauses)) = clauses.split_first() else {
        return Ok(MachineState::Value(Value::Void));
    };

    let Expr::List(parts) = clause else {
        unreachable!("cond clauses are validated before evaluation");
    };
    let Some((predicate, body)) = parts.split_first() else {
        unreachable!("cond clauses are validated before evaluation");
    };

    if is_else_symbol(predicate) {
        return start_sequence(body.to_vec(), environment, stack);
    }

    stack.push(Frame::Cond {
        body: body.to_vec(),
        remaining_clauses: remaining_clauses.to_vec(),
        environment: environment.clone(),
    });
    Ok(MachineState::Expression(predicate.clone(), environment))
}

fn validate_cond_clauses(clauses: &[Expr]) -> Result<(), SchemeError> {
    let Some((last_clause, preceding_clauses)) = clauses.split_last() else {
        return Ok(());
    };

    for clause in preceding_clauses {
        validate_cond_clause(clause, false)?;
    }

    validate_cond_clause(last_clause, true)
}

fn validate_cond_clause(clause: &Expr, is_last: bool) -> Result<(), SchemeError> {
    let Expr::List(parts) = clause else {
        return Err(SchemeError::InvalidCondClause {
            found: expression_kind(clause),
        });
    };

    let Some((predicate, body)) = parts.split_first() else {
        return Err(SchemeError::TooFewArguments {
            operator: "cond clause",
            min: 1,
            actual: 0,
        });
    };

    if is_else_symbol(predicate) {
        if !is_last {
            return Err(SchemeError::CondElseNotLast);
        }

        if body.is_empty() {
            return Err(SchemeError::TooFewArguments {
                operator: "cond else",
                min: 1,
                actual: 0,
            });
        }
    }

    Ok(())
}

fn eval_let(
    operands: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match operands {
        [Expr::Symbol(name), bindings_expression, body @ ..] if !body.is_empty() => {
            eval_named_let(name, bindings_expression, body, environment, stack)
        }
        [bindings_expression, body @ ..] if !body.is_empty() => {
            eval_let_body(bindings_expression, body, environment, stack)
        }
        [Expr::Symbol(_), ..] => Err(SchemeError::TooFewArguments {
            operator: "let",
            min: 3,
            actual: operands.len(),
        }),
        _ => Err(SchemeError::TooFewArguments {
            operator: "let",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_let_body(
    bindings_expression: &Expr,
    body: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let mut bindings = parse_let_bindings(bindings_expression)?;
    let Some(last_binding) = bindings.pop() else {
        let let_environment = environment.child();
        return start_sequence(body.to_vec(), let_environment, stack);
    };

    let (current_name, expression) = last_binding.into_parts();
    stack.push(Frame::Let {
        current_name,
        evaluated_rev: Vec::new(),
        remaining: bindings,
        body: body.to_vec(),
        environment: environment.clone(),
    });
    Ok(MachineState::Expression(expression, environment.clone()))
}

fn eval_named_let(
    name: &str,
    bindings_expression: &Expr,
    body: &[Expr],
    environment: &Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let bindings = parse_let_bindings(bindings_expression)?;
    let parameters = bindings
        .iter()
        .map(|binding| binding.name().to_owned())
        .collect();
    let arguments = bindings
        .into_iter()
        .map(LetBinding::into_parts)
        .map(|(_, expression)| expression)
        .collect();
    let let_environment = environment.child();
    let procedure = Value::procedure(
        Parameters::new(parameters, None),
        body.to_vec(),
        let_environment.clone(),
    );

    let_environment.define(name, procedure.clone());
    start_argument_evaluation(procedure, arguments, environment.clone(), stack)
}

fn parse_let_bindings(bindings_expression: &Expr) -> Result<Vec<LetBinding>, SchemeError> {
    let Expr::List(bindings) = bindings_expression else {
        return Err(SchemeError::InvalidBindingList {
            operator: "let",
            found: expression_kind(bindings_expression),
        });
    };

    let mut parsed_bindings = Vec::with_capacity(bindings.len());
    let mut seen = HashSet::with_capacity(bindings.len());

    for binding in bindings {
        let binding = parse_let_binding(binding)?;

        if !seen.insert(binding.name().to_owned()) {
            return Err(SchemeError::DuplicateBinding {
                operator: "let",
                name: binding.name().to_owned(),
            });
        }

        parsed_bindings.push(binding);
    }

    Ok(parsed_bindings)
}

fn parse_let_binding(binding: &Expr) -> Result<LetBinding, SchemeError> {
    let Expr::List(parts) = binding else {
        return Err(SchemeError::InvalidBinding {
            operator: "let",
            found: expression_kind(binding),
        });
    };

    match parts.as_slice() {
        [Expr::Symbol(name), value_expression] => {
            Ok(LetBinding::new(name.clone(), value_expression.clone()))
        }
        [Expr::Symbol(_), ..] => Err(SchemeError::WrongArgumentCount {
            operator: "let binding",
            expected: 2,
            actual: parts.len(),
        }),
        [name, ..] => Err(SchemeError::InvalidBindingName {
            operator: "let",
            found: expression_kind(name),
        }),
        [] => Err(SchemeError::WrongArgumentCount {
            operator: "let binding",
            expected: 2,
            actual: 0,
        }),
    }
}

fn parse_parameter_list(
    parameters: &Expr,
    operator: &'static str,
) -> Result<Parameters, SchemeError> {
    match parameters {
        Expr::List(parameters) => parse_parameters(parameters, operator),
        Expr::Symbol(name) if name != "." => Ok(Parameters::new(Vec::new(), Some(name.clone()))),
        _ => Err(SchemeError::InvalidParameterList {
            operator,
            found: expression_kind(parameters),
        }),
    }
}

fn parse_parameters(
    parameters: &[Expr],
    operator: &'static str,
) -> Result<Parameters, SchemeError> {
    let mut dotted_indices = parameters
        .iter()
        .enumerate()
        .filter_map(|(index, parameter)| {
            matches!(parameter, Expr::Symbol(name) if name == ".").then_some(index)
        });

    let Some(dotted_index) = dotted_indices.next() else {
        return parse_fixed_parameters(parameters, operator)
            .map(|fixed| Parameters::new(fixed, None));
    };

    if dotted_indices.next().is_some() {
        return Err(SchemeError::InvalidDottedParameterList { operator });
    }

    parse_variadic_parameters(parameters, operator, dotted_index)
}

fn parse_variadic_parameters(
    parameters: &[Expr],
    operator: &'static str,
    dotted_index: usize,
) -> Result<Parameters, SchemeError> {
    if dotted_index + 2 != parameters.len() {
        return Err(SchemeError::InvalidDottedParameterList { operator });
    }

    let fixed = parse_fixed_parameters(&parameters[..dotted_index], operator)?;
    let rest_parameter = parse_rest_parameter(&parameters[dotted_index + 1], operator)?;

    if fixed.iter().any(|parameter| parameter == &rest_parameter) {
        return Err(SchemeError::DuplicateParameter {
            operator,
            name: rest_parameter,
        });
    }

    Ok(Parameters::new(fixed, Some(rest_parameter)))
}

fn parse_fixed_parameters(
    parameters: &[Expr],
    operator: &'static str,
) -> Result<Vec<String>, SchemeError> {
    let mut names = Vec::with_capacity(parameters.len());
    let mut seen = HashSet::with_capacity(parameters.len());

    for parameter in parameters {
        let Expr::Symbol(name) = parameter else {
            return Err(SchemeError::InvalidParameterName {
                operator,
                found: expression_kind(parameter),
            });
        };

        if name == "." {
            return Err(SchemeError::InvalidDottedParameterList { operator });
        }

        if !seen.insert(name.clone()) {
            return Err(SchemeError::DuplicateParameter {
                operator,
                name: name.clone(),
            });
        }

        names.push(name.clone());
    }

    Ok(names)
}

fn parse_rest_parameter(parameter: &Expr, operator: &'static str) -> Result<String, SchemeError> {
    let Expr::Symbol(name) = parameter else {
        return Err(SchemeError::InvalidParameterName {
            operator,
            found: expression_kind(parameter),
        });
    };

    if name == "." {
        Err(SchemeError::InvalidDottedParameterList { operator })
    } else {
        Ok(name.clone())
    }
}

fn continue_from_frame(
    frame: Frame,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match frame {
        Frame::Sequence {
            remaining,
            environment,
        } => start_sequence(remaining, environment, stack),
        Frame::Define { name, environment } => finish_define(name, environment, value),
        Frame::Set { name, environment } => finish_set(name, environment, value),
        Frame::If {
            consequent,
            alternative,
            environment,
        } => finish_if(consequent, alternative, environment, value),
        Frame::And {
            remaining,
            environment,
        } => finish_and(remaining, environment, value, stack),
        Frame::Or {
            remaining,
            environment,
        } => finish_or(remaining, environment, value, stack),
        Frame::Cond {
            body,
            remaining_clauses,
            environment,
        } => finish_cond(body, remaining_clauses, environment, value, stack),
        Frame::ApplyOperator {
            operands,
            environment,
        } => start_argument_evaluation(value, operands, environment, stack),
        Frame::ApplyArguments {
            callable,
            evaluated_rev,
            remaining,
            environment,
        } => finish_apply_arguments(
            callable,
            evaluated_rev,
            remaining,
            environment,
            value,
            stack,
        ),
        Frame::Let {
            current_name,
            evaluated_rev,
            remaining,
            body,
            environment,
        } => finish_let_bindings(
            current_name,
            evaluated_rev,
            remaining,
            body,
            environment,
            value,
            stack,
        ),
    }
}

fn finish_define(
    name: String,
    environment: Environment,
    value: Value,
) -> Result<MachineState, SchemeError> {
    environment.define(&name, value);
    Ok(MachineState::Value(Value::Void))
}

fn finish_set(
    name: String,
    environment: Environment,
    value: Value,
) -> Result<MachineState, SchemeError> {
    if environment.set(&name, value) {
        Ok(MachineState::Value(Value::Void))
    } else {
        Err(SchemeError::UnboundSymbol { name })
    }
}

fn finish_if(
    consequent: Expr,
    alternative: Expr,
    environment: Environment,
    value: Value,
) -> Result<MachineState, SchemeError> {
    if value.is_truthy() {
        Ok(MachineState::Expression(consequent, environment))
    } else {
        Ok(MachineState::Expression(alternative, environment))
    }
}

fn finish_and(
    remaining: Vec<Expr>,
    environment: Environment,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if value.is_truthy() {
        start_and(remaining, environment, stack)
    } else {
        Ok(MachineState::Value(value))
    }
}

fn finish_or(
    remaining: Vec<Expr>,
    environment: Environment,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if value.is_truthy() {
        Ok(MachineState::Value(value))
    } else {
        start_or(remaining, environment, stack)
    }
}

fn finish_cond(
    body: Vec<Expr>,
    remaining_clauses: Vec<Expr>,
    environment: Environment,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    if !value.is_truthy() {
        return continue_cond(remaining_clauses, environment, stack);
    }

    if body.is_empty() {
        return Ok(MachineState::Value(value));
    }

    start_sequence(body, environment, stack)
}

fn finish_apply_arguments(
    callable: Value,
    mut evaluated_rev: Vec<Value>,
    mut remaining: Vec<Expr>,
    environment: Environment,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    evaluated_rev.push(value);

    match remaining.pop() {
        Some(next_operand) => {
            stack.push(Frame::ApplyArguments {
                callable,
                evaluated_rev,
                remaining,
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(next_operand, environment))
        }
        None => {
            evaluated_rev.reverse();
            apply_value_callable(callable, evaluated_rev, stack)
        }
    }
}

fn finish_let_bindings(
    current_name: String,
    mut evaluated_rev: Vec<(String, Value)>,
    mut remaining: Vec<LetBinding>,
    body: Vec<Expr>,
    environment: Environment,
    value: Value,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    evaluated_rev.push((current_name, value));

    match remaining.pop() {
        Some(binding) => {
            let (current_name, expression) = binding.into_parts();
            stack.push(Frame::Let {
                current_name,
                evaluated_rev,
                remaining,
                body,
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(expression, environment))
        }
        None => finish_let_body(evaluated_rev, body, environment, stack),
    }
}

fn finish_let_body(
    evaluated_rev: Vec<(String, Value)>,
    body: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let let_environment = environment.child();

    for (name, value) in evaluated_rev.into_iter().rev() {
        let_environment.define(&name, value);
    }

    start_sequence(body, let_environment, stack)
}

fn start_and(
    operands: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let Some((first_operand, remaining)) = operands.split_first() else {
        return Ok(MachineState::Value(Value::Boolean(true)));
    };

    if !remaining.is_empty() {
        stack.push(Frame::And {
            remaining: remaining.to_vec(),
            environment: environment.clone(),
        });
    }

    Ok(MachineState::Expression(first_operand.clone(), environment))
}

fn start_or(
    operands: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let Some((first_operand, remaining)) = operands.split_first() else {
        return Ok(MachineState::Value(Value::Boolean(false)));
    };

    if !remaining.is_empty() {
        stack.push(Frame::Or {
            remaining: remaining.to_vec(),
            environment: environment.clone(),
        });
    }

    Ok(MachineState::Expression(first_operand.clone(), environment))
}

fn start_argument_evaluation(
    callable: Value,
    mut operands: Vec<Expr>,
    environment: Environment,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match operands.pop() {
        Some(operand) => {
            stack.push(Frame::ApplyArguments {
                callable,
                evaluated_rev: Vec::new(),
                remaining: operands,
                environment: environment.clone(),
            });
            Ok(MachineState::Expression(operand, environment))
        }
        None => apply_value_callable(callable, Vec::new(), stack),
    }
}

fn apply_value_callable(
    callable: Value,
    arguments: Vec<Value>,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, arguments, stack),
        Value::Procedure(procedure) => apply_procedure(procedure, arguments, stack),
        Value::Continuation(continuation) => apply_continuation(continuation, arguments, stack),
        value => Err(SchemeError::NonCallable { kind: value.kind() }),
    }
}

fn apply_builtin(
    builtin: Builtin,
    arguments: Vec<Value>,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match builtin {
        Builtin::Apply => eval_apply(arguments, stack),
        Builtin::CallCc => eval_callcc(arguments, stack),
        _ => apply_builtin_value(builtin, arguments).map(MachineState::Value),
    }
}

fn apply_procedure(
    procedure: Rc<Procedure>,
    arguments: Vec<Value>,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    let parameters = procedure.parameters();
    if !parameters.accepts_argument_count(arguments.len()) {
        if parameters.has_rest() {
            return Err(SchemeError::TooFewProcedureArguments {
                min: parameters.minimum_arity(),
                actual: arguments.len(),
            });
        }

        return Err(SchemeError::WrongProcedureArgumentCount {
            expected: parameters.minimum_arity(),
            actual: arguments.len(),
        });
    }

    let call_environment = procedure.environment().child();
    bind_procedure_arguments(&call_environment, parameters, arguments);
    start_sequence(procedure.body().to_vec(), call_environment, stack)
}

fn bind_procedure_arguments(
    environment: &Environment,
    parameters: &Parameters,
    arguments: Vec<Value>,
) {
    let mut arguments = arguments.into_iter();

    for parameter in parameters.fixed() {
        let Some(argument) = arguments.next() else {
            unreachable!("procedure arity is validated before binding");
        };
        environment.define(parameter, argument);
    }

    if let Some(rest_parameter) = parameters.rest() {
        environment.define(rest_parameter, Value::list(arguments.collect()));
    }
}

fn apply_continuation(
    continuation: Rc<Continuation>,
    arguments: Vec<Value>,
    stack: &mut Vec<Frame>,
) -> Result<MachineState, SchemeError> {
    match arguments.as_slice() {
        [value] => {
            *stack = continuation.cloned_frames();
            Ok(MachineState::Value(value.clone()))
        }
        _ => Err(SchemeError::WrongProcedureArgumentCount {
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_apply(arguments: Vec<Value>, stack: &mut Vec<Frame>) -> Result<MachineState, SchemeError> {
    if arguments.len() < 2 {
        return Err(SchemeError::TooFewArguments {
            operator: "apply",
            min: 2,
            actual: arguments.len(),
        });
    }

    let Some((rest_arguments, leading_arguments)) = arguments.split_last() else {
        unreachable!("argument length is checked before destructuring");
    };
    let Some((callable, prefix_arguments)) = leading_arguments.split_first() else {
        unreachable!("argument length is checked before destructuring");
    };

    let mut applied_arguments = prefix_arguments.to_vec();
    let rest_arguments = rest_arguments
        .clone()
        .into_list_elements()
        .map_err(|found| SchemeError::ExpectedList {
            operator: "apply",
            found,
        })?;
    applied_arguments.extend(rest_arguments);

    apply_value_callable(callable.clone(), applied_arguments, stack)
}

fn eval_callcc(arguments: Vec<Value>, stack: &mut Vec<Frame>) -> Result<MachineState, SchemeError> {
    match arguments.as_slice() {
        [callable] => {
            let continuation = Value::continuation(Continuation::new(stack.clone()));
            apply_value_callable(callable.clone(), vec![continuation], stack)
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "call/cc",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn apply_builtin_value(builtin: Builtin, arguments: Vec<Value>) -> Result<Value, SchemeError> {
    match builtin {
        Builtin::Add => eval_addition(&arguments),
        Builtin::Subtract => eval_subtraction(&arguments),
        Builtin::Multiply => eval_multiplication(&arguments),
        Builtin::Divide => eval_division(&arguments),
        Builtin::LessThan => eval_comparison("<", &arguments, |lhs, rhs| lhs < rhs),
        Builtin::GreaterThan => eval_comparison(">", &arguments, |lhs, rhs| lhs > rhs),
        Builtin::Equal => eval_comparison("=", &arguments, |lhs, rhs| lhs == rhs),
        Builtin::LessEqual => eval_comparison("<=", &arguments, |lhs, rhs| lhs <= rhs),
        Builtin::Not => eval_not(&arguments),
        Builtin::Cons => eval_cons(&arguments),
        Builtin::Car => eval_car(&arguments),
        Builtin::Cdr => eval_cdr(&arguments),
        Builtin::Null => eval_null(&arguments),
        Builtin::List => eval_list(arguments),
        Builtin::Length => eval_length(&arguments),
        Builtin::StringPredicate => eval_string_predicate(&arguments),
        Builtin::NumberPredicate => eval_number_predicate(&arguments),
        Builtin::BooleanPredicate => eval_boolean_predicate(&arguments),
        Builtin::PairPredicate => eval_pair_predicate(&arguments),
        Builtin::SymbolPredicate => eval_symbol_predicate(&arguments),
        Builtin::Apply | Builtin::CallCc => {
            unreachable!("non-primitive builtins are handled before primitive dispatch")
        }
    }
}

fn eval_addition(arguments: &[Value]) -> Result<Value, SchemeError> {
    let sum = eval_numbers("+", arguments)?.into_iter().sum();
    Ok(Value::Integer(sum))
}

fn eval_subtraction(arguments: &[Value]) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("-", arguments)?;

    match numbers.as_slice() {
        [] => Err(SchemeError::TooFewArguments {
            operator: "-",
            min: 1,
            actual: 0,
        }),
        [value] => Ok(Value::Integer(-*value)),
        [first, rest @ ..] => Ok(Value::Integer(
            rest.iter().fold(*first, |acc, value| acc - value),
        )),
    }
}

fn eval_multiplication(arguments: &[Value]) -> Result<Value, SchemeError> {
    let product = eval_numbers("*", arguments)?.into_iter().product();
    Ok(Value::Integer(product))
}

fn eval_division(arguments: &[Value]) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("/", arguments)?;

    match numbers.as_slice() {
        [] | [_] => Err(SchemeError::TooFewArguments {
            operator: "/",
            min: 2,
            actual: numbers.len(),
        }),
        [first, rest @ ..] => {
            let quotient = rest
                .iter()
                .try_fold(*first, |acc, value| divide_numbers(acc, *value))?;
            Ok(Value::Integer(quotient))
        }
    }
}

fn eval_comparison<F>(
    operator: &'static str,
    arguments: &[Value],
    compare: F,
) -> Result<Value, SchemeError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_numbers(operator, arguments)?;

    match numbers.as_slice() {
        [] | [_] => Err(SchemeError::TooFewArguments {
            operator,
            min: 2,
            actual: numbers.len(),
        }),
        _ => Ok(Value::Boolean(
            numbers.windows(2).all(|pair| compare(pair[0], pair[1])),
        )),
    }
}

fn eval_not(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [value] => Ok(Value::Boolean(!value.is_truthy())),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "not",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_cons(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [car, cdr] => Ok(Value::pair(car.clone(), cdr.clone())),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cons",
            expected: 2,
            actual: arguments.len(),
        }),
    }
}

fn eval_car(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [value] => match value {
            Value::Pair(pair) => Ok(pair.car().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "car",
                found: found.clone(),
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "car",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_cdr(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [value] => match value {
            Value::Pair(pair) => Ok(pair.cdr().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "cdr",
                found: found.clone(),
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cdr",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_null(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [value] => Ok(Value::Boolean(value.is_null())),
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "null?",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_list(arguments: Vec<Value>) -> Result<Value, SchemeError> {
    Ok(Value::list(arguments))
}

fn eval_length(arguments: &[Value]) -> Result<Value, SchemeError> {
    match arguments {
        [value] => {
            let length = value
                .list_length()
                .map_err(|found| SchemeError::ExpectedList {
                    operator: "length",
                    found,
                })?;
            Ok(Value::Integer(length as i64))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "length",
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_string_predicate(arguments: &[Value]) -> Result<Value, SchemeError> {
    eval_type_predicate("string?", arguments, Value::is_string)
}

fn eval_number_predicate(arguments: &[Value]) -> Result<Value, SchemeError> {
    eval_type_predicate("number?", arguments, Value::is_number)
}

fn eval_boolean_predicate(arguments: &[Value]) -> Result<Value, SchemeError> {
    eval_type_predicate("boolean?", arguments, Value::is_boolean)
}

fn eval_pair_predicate(arguments: &[Value]) -> Result<Value, SchemeError> {
    eval_type_predicate("pair?", arguments, Value::is_pair)
}

fn eval_symbol_predicate(arguments: &[Value]) -> Result<Value, SchemeError> {
    eval_type_predicate("symbol?", arguments, Value::is_symbol)
}

fn eval_type_predicate(
    operator: &'static str,
    arguments: &[Value],
    predicate: fn(&Value) -> bool,
) -> Result<Value, SchemeError> {
    match arguments {
        [value] => Ok(Value::Boolean(predicate(value))),
        _ => Err(SchemeError::WrongArgumentCount {
            operator,
            expected: 1,
            actual: arguments.len(),
        }),
    }
}

fn eval_numbers(operator: &'static str, arguments: &[Value]) -> Result<Vec<i64>, SchemeError> {
    arguments
        .iter()
        .cloned()
        .map(|value| expect_number(operator, value))
        .collect()
}

fn expect_number(operator: &'static str, value: Value) -> Result<i64, SchemeError> {
    match value {
        Value::Integer(number) => Ok(number),
        _ => Err(SchemeError::ExpectedNumber {
            operator,
            found: value,
        }),
    }
}

fn divide_numbers(lhs: i64, rhs: i64) -> Result<i64, SchemeError> {
    if rhs == 0 {
        Err(SchemeError::DivisionByZero)
    } else {
        Ok(lhs / rhs)
    }
}

fn is_else_symbol(expression: &Expr) -> bool {
    matches!(expression, Expr::Symbol(symbol) if symbol == "else")
}

fn expression_kind(expression: &Expr) -> &'static str {
    match expression {
        Expr::Integer(_) => "number",
        Expr::Boolean(_) => "boolean",
        Expr::String(_) => "string",
        Expr::Symbol(_) => "symbol",
        Expr::List(_) => "list",
    }
}
