use std::collections::HashSet;
use std::rc::Rc;

use crate::scheme::builtin::{Builtin, BUILTINS};
use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::procedure::{Parameters, Procedure};
use crate::scheme::value::Value;

enum EvalStep<'expr> {
    Value(Value),
    Expression(&'expr Expr, Environment),
    Procedure(Rc<Procedure>, Environment),
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
    let mut last_value = None;

    for expression in expressions {
        last_value = Some(eval_expr(expression, environment)?);
    }

    last_value.ok_or(SchemeError::EmptyInput)
}

fn eval_expr(expression: &Expr, environment: &Environment) -> Result<Value, SchemeError> {
    let mut current_expression = expression;
    let mut current_environment = environment.clone();
    let mut procedure_anchor = Vec::new();

    loop {
        if let Some(value) = Value::from_literal(current_expression) {
            return Ok(value);
        }

        let step = match current_expression {
            Expr::Symbol(name) => EvalStep::Value(
                current_environment
                    .get(name)
                    .ok_or_else(|| SchemeError::UnboundSymbol { name: name.clone() })?,
            ),
            Expr::List(expressions) => eval_application(expressions, &current_environment)?,
            Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => unreachable!(),
        };

        match step {
            EvalStep::Value(value) => return Ok(value),
            EvalStep::Expression(expression, environment) => {
                current_expression = expression;
                current_environment = environment;
            }
            EvalStep::Procedure(procedure, environment) => {
                current_environment = environment;
                procedure_anchor.clear();
                procedure_anchor.push(procedure);
                current_expression = current_procedure_expression(&procedure_anchor);
            }
        }
    }
}

fn current_procedure_expression(procedures: &[Rc<Procedure>]) -> &Expr {
    let Some(procedure) = procedures.last() else {
        unreachable!("procedure anchors are stored before use");
    };
    let Some(expression) = procedure.body().last() else {
        unreachable!("procedures are created with non-empty bodies");
    };
    expression
}

fn eval_application<'expr>(
    expressions: &'expr [Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    let (operator, operands) = expressions
        .split_first()
        .ok_or(SchemeError::EmptyApplication)?;

    if let Expr::Symbol(name) = operator {
        if let Some(value) = eval_special_form(name, operands, environment)? {
            return Ok(value);
        }
    }

    let callable = eval_expr(operator, environment)?;
    apply_callable(callable, operands, environment)
}

fn eval_special_form<'expr>(
    operator: &str,
    operands: &'expr [Expr],
    environment: &Environment,
) -> Result<Option<EvalStep<'expr>>, SchemeError> {
    match operator {
        "define" => eval_define(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "set!" => eval_set(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "if" => eval_if(operands, environment).map(Some),
        "quote" => eval_quote(operands).map(|value| Some(EvalStep::Value(value))),
        "lambda" => eval_lambda(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "and" => eval_and(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "or" => eval_or(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "begin" => eval_begin(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "cond" => eval_cond(operands, environment).map(|value| Some(EvalStep::Value(value))),
        "let" => eval_let(operands, environment).map(|value| Some(EvalStep::Value(value))),
        _ => Ok(None),
    }
}

fn eval_define(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            let value = eval_expr(value_expression, environment)?;
            environment.define(name, value);
            Ok(Value::Void)
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
        [target, _] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        [target, ..] => Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(target),
        }),
        _ => Err(SchemeError::TooFewArguments {
            operator: "define",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_set(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [Expr::Symbol(name), value_expression] => {
            let value = eval_expr(value_expression, environment)?;

            if environment.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(SchemeError::UnboundSymbol { name: name.clone() })
            }
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
) -> Result<Value, SchemeError> {
    let (name, parameters) = signature
        .split_first()
        .ok_or(SchemeError::InvalidDefinitionTarget { found: "list" })?;

    let Expr::Symbol(name) = name else {
        return Err(SchemeError::InvalidDefinitionTarget {
            found: expression_kind(name),
        });
    };

    let parameters = parse_parameters(parameters, "define")?;
    let procedure = Value::procedure(parameters, body.to_vec(), environment.clone());
    environment.define(name, procedure);
    Ok(Value::Void)
}

fn eval_if<'expr>(
    operands: &'expr [Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    match operands {
        [condition, consequent, alternative] => {
            let value = eval_expr(condition, environment)?;
            if value.is_truthy() {
                Ok(EvalStep::Expression(consequent, environment.clone()))
            } else {
                Ok(EvalStep::Expression(alternative, environment.clone()))
            }
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

fn eval_begin(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    if operands.is_empty() {
        Err(SchemeError::TooFewArguments {
            operator: "begin",
            min: 1,
            actual: 0,
        })
    } else {
        eval_sequence(operands, environment)
    }
}

fn eval_cond(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    if operands.is_empty() {
        return Err(SchemeError::TooFewArguments {
            operator: "cond",
            min: 1,
            actual: 0,
        });
    }

    validate_cond_clauses(operands)?;

    for clause in operands {
        if let Some(value) = eval_cond_clause(clause, environment)? {
            return Ok(value);
        }
    }

    Ok(Value::Void)
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

fn eval_cond_clause(
    clause: &Expr,
    environment: &Environment,
) -> Result<Option<Value>, SchemeError> {
    let Expr::List(parts) = clause else {
        unreachable!("cond clauses are validated before evaluation");
    };
    let Some((predicate, body)) = parts.split_first() else {
        unreachable!("cond clauses are validated before evaluation");
    };

    if is_else_symbol(predicate) {
        return eval_cond_else(body, environment).map(Some);
    }

    let predicate_value = eval_expr(predicate, environment)?;
    if !predicate_value.is_truthy() {
        return Ok(None);
    }

    if body.is_empty() {
        Ok(Some(predicate_value))
    } else {
        eval_sequence(body, environment).map(Some)
    }
}

fn eval_cond_else(body: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    if body.is_empty() {
        return Err(SchemeError::TooFewArguments {
            operator: "cond else",
            min: 1,
            actual: 0,
        });
    }

    eval_sequence(body, environment)
}

fn eval_let(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [bindings_expression, body @ ..] if !body.is_empty() => {
            let bindings = eval_let_bindings(bindings_expression, environment)?;
            let let_environment = environment.child();

            for (name, value) in bindings {
                let_environment.define(&name, value);
            }

            eval_sequence(body, &let_environment)
        }
        _ => Err(SchemeError::TooFewArguments {
            operator: "let",
            min: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_let_bindings(
    bindings_expression: &Expr,
    environment: &Environment,
) -> Result<Vec<(String, Value)>, SchemeError> {
    let bindings = parse_let_bindings(bindings_expression)?;

    bindings
        .into_iter()
        .map(|(name, value_expression)| {
            eval_expr(value_expression, environment).map(|value| (name.to_owned(), value))
        })
        .collect()
}

fn parse_let_bindings(bindings_expression: &Expr) -> Result<Vec<(&str, &Expr)>, SchemeError> {
    let Expr::List(bindings) = bindings_expression else {
        return Err(SchemeError::InvalidBindingList {
            operator: "let",
            found: expression_kind(bindings_expression),
        });
    };

    let mut parsed_bindings = Vec::with_capacity(bindings.len());
    let mut seen = HashSet::with_capacity(bindings.len());

    for binding in bindings {
        let (name, value_expression) = parse_let_binding(binding)?;

        if !seen.insert(name.to_owned()) {
            return Err(SchemeError::DuplicateBinding {
                operator: "let",
                name: name.to_owned(),
            });
        }

        parsed_bindings.push((name, value_expression));
    }

    Ok(parsed_bindings)
}

fn parse_let_binding(binding: &Expr) -> Result<(&str, &Expr), SchemeError> {
    let Expr::List(parts) = binding else {
        return Err(SchemeError::InvalidBinding {
            operator: "let",
            found: expression_kind(binding),
        });
    };

    match parts.as_slice() {
        [Expr::Symbol(name), value_expression] => Ok((name.as_str(), value_expression)),
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

fn apply_builtin<'expr>(
    builtin: Builtin,
    operands: &[Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    let arguments = eval_values(operands, environment)?;
    apply_value_builtin(builtin, arguments)
}

fn apply_value_builtin<'expr>(
    builtin: Builtin,
    arguments: Vec<Value>,
) -> Result<EvalStep<'expr>, SchemeError> {
    match builtin {
        Builtin::Apply => eval_apply(arguments),
        _ => apply_builtin_value(builtin, arguments).map(EvalStep::Value),
    }
}

fn apply_callable<'expr>(
    callable: Value,
    operands: &'expr [Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, operands, environment),
        Value::Procedure(procedure) => {
            apply_procedure(procedure, eval_values(operands, environment)?)
        }
        value => Err(SchemeError::NonCallable { kind: value.kind() }),
    }
}

fn apply_procedure<'expr>(
    procedure: Rc<Procedure>,
    arguments: Vec<Value>,
) -> Result<EvalStep<'expr>, SchemeError> {
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

    eval_leading_expressions(procedure.body(), &call_environment)?;
    Ok(EvalStep::Procedure(procedure, call_environment))
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

fn eval_apply<'expr>(arguments: Vec<Value>) -> Result<EvalStep<'expr>, SchemeError> {
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

    apply_value_callable(callable.clone(), applied_arguments)
}

fn apply_value_callable<'expr>(
    callable: Value,
    arguments: Vec<Value>,
) -> Result<EvalStep<'expr>, SchemeError> {
    match callable {
        Value::Builtin(builtin) => apply_value_builtin(builtin, arguments),
        Value::Procedure(procedure) => apply_procedure(procedure, arguments),
        value => Err(SchemeError::NonCallable { kind: value.kind() }),
    }
}

fn eval_leading_expressions(
    expressions: &[Expr],
    environment: &Environment,
) -> Result<(), SchemeError> {
    let Some((_, leading_expressions)) = expressions.split_last() else {
        unreachable!("procedures are created with non-empty bodies");
    };

    for expression in leading_expressions {
        let _ = eval_expr(expression, environment)?;
    }

    Ok(())
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
        Builtin::Apply => unreachable!("apply is handled before builtin value dispatch"),
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

fn eval_and(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let mut last_value = Value::Boolean(true);

    for operand in operands {
        let value = eval_expr(operand, environment)?;

        if !value.is_truthy() {
            return Ok(value);
        }

        last_value = value;
    }

    Ok(last_value)
}

fn eval_or(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    for operand in operands {
        let value = eval_expr(operand, environment)?;

        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
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

fn eval_values(operands: &[Expr], environment: &Environment) -> Result<Vec<Value>, SchemeError> {
    let mut values = Vec::with_capacity(operands.len());

    for operand in operands {
        values.push(eval_expr(operand, environment)?);
    }

    Ok(values)
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
