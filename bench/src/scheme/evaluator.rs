use std::collections::HashSet;
use std::rc::Rc;

use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::{self, Expr};
use crate::scheme::procedure::Procedure;
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
    eval_sequence(program, &environment)
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

        if is_builtin(name) {
            return apply_builtin(name, operands, environment).map(EvalStep::Value);
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
        "if" => eval_if(operands, environment).map(Some),
        "quote" => eval_quote(operands).map(|value| Some(EvalStep::Value(value))),
        "lambda" => eval_lambda(operands, environment).map(|value| Some(EvalStep::Value(value))),
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

    let parameters = parse_parameter_names(parameters, "define")?;
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
) -> Result<Vec<String>, SchemeError> {
    match parameters {
        Expr::List(parameters) => parse_parameter_names(parameters, operator),
        _ => Err(SchemeError::InvalidParameterList {
            operator,
            found: expression_kind(parameters),
        }),
    }
}

fn parse_parameter_names(
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

fn apply_builtin(
    operator: &str,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    match operator {
        "+" => eval_addition(operands, environment),
        "-" => eval_subtraction(operands, environment),
        "*" => eval_multiplication(operands, environment),
        "/" => eval_division(operands, environment),
        "<" => eval_comparison("<", operands, environment, |lhs, rhs| lhs < rhs),
        ">" => eval_comparison(">", operands, environment, |lhs, rhs| lhs > rhs),
        "=" => eval_comparison("=", operands, environment, |lhs, rhs| lhs == rhs),
        "<=" => eval_comparison("<=", operands, environment, |lhs, rhs| lhs <= rhs),
        "not" => eval_not(operands, environment),
        "and" => eval_and(operands, environment),
        "or" => eval_or(operands, environment),
        "cons" => eval_cons(operands, environment),
        "car" => eval_car(operands, environment),
        "cdr" => eval_cdr(operands, environment),
        "null?" => eval_null(operands, environment),
        "list" => eval_list(operands, environment),
        "length" => eval_length(operands, environment),
        "string?" => eval_string_predicate(operands, environment),
        "number?" => eval_number_predicate(operands, environment),
        "boolean?" => eval_boolean_predicate(operands, environment),
        "pair?" => eval_pair_predicate(operands, environment),
        "symbol?" => eval_symbol_predicate(operands, environment),
        _ => Err(SchemeError::UnboundSymbol {
            name: operator.to_owned(),
        }),
    }
}

fn apply_callable<'expr>(
    callable: Value,
    operands: &'expr [Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    match callable {
        Value::Procedure(procedure) => apply_procedure(procedure, operands, environment),
        value => Err(SchemeError::NonCallable { kind: value.kind() }),
    }
}

fn apply_procedure<'expr>(
    procedure: Rc<Procedure>,
    operands: &'expr [Expr],
    environment: &Environment,
) -> Result<EvalStep<'expr>, SchemeError> {
    if operands.len() != procedure.parameters().len() {
        return Err(SchemeError::WrongProcedureArgumentCount {
            expected: procedure.parameters().len(),
            actual: operands.len(),
        });
    }

    let arguments = eval_values(operands, environment)?;
    let call_environment = procedure.environment().child();

    for (parameter, argument) in procedure.parameters().iter().zip(arguments) {
        call_environment.define(parameter, argument);
    }

    eval_leading_expressions(procedure.body(), &call_environment)?;
    Ok(EvalStep::Procedure(procedure, call_environment))
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

fn eval_addition(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let sum = eval_numbers("+", operands, environment)?.into_iter().sum();
    Ok(Value::Integer(sum))
}

fn eval_subtraction(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("-", operands, environment)?;

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

fn eval_multiplication(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let product = eval_numbers("*", operands, environment)?
        .into_iter()
        .product();
    Ok(Value::Integer(product))
}

fn eval_division(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    let numbers = eval_numbers("/", operands, environment)?;

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
    operands: &[Expr],
    environment: &Environment,
    compare: F,
) -> Result<Value, SchemeError>
where
    F: Fn(i64, i64) -> bool,
{
    let numbers = eval_numbers(operator, operands, environment)?;

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

fn eval_not(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            Ok(Value::Boolean(!value.is_truthy()))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "not",
            expected: 1,
            actual: operands.len(),
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

fn eval_cons(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [car_expression, cdr_expression] => {
            let car = eval_expr(car_expression, environment)?;
            let cdr = eval_expr(cdr_expression, environment)?;
            Ok(Value::pair(car, cdr))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cons",
            expected: 2,
            actual: operands.len(),
        }),
    }
}

fn eval_car(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => match eval_expr(operand, environment)? {
            Value::Pair(pair) => Ok(pair.car().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "car",
                found,
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "car",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_cdr(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => match eval_expr(operand, environment)? {
            Value::Pair(pair) => Ok(pair.cdr().clone()),
            found => Err(SchemeError::ExpectedPair {
                operator: "cdr",
                found,
            }),
        },
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "cdr",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_null(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            Ok(Value::Boolean(value.is_null()))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator: "null?",
            expected: 1,
            actual: operands.len(),
        }),
    }
}

fn eval_list(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    Ok(Value::list(eval_values(operands, environment)?))
}

fn eval_length(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
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
            actual: operands.len(),
        }),
    }
}

fn eval_string_predicate(
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    eval_type_predicate("string?", operands, environment, Value::is_string)
}

fn eval_number_predicate(
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    eval_type_predicate("number?", operands, environment, Value::is_number)
}

fn eval_boolean_predicate(
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    eval_type_predicate("boolean?", operands, environment, Value::is_boolean)
}

fn eval_pair_predicate(operands: &[Expr], environment: &Environment) -> Result<Value, SchemeError> {
    eval_type_predicate("pair?", operands, environment, Value::is_pair)
}

fn eval_symbol_predicate(
    operands: &[Expr],
    environment: &Environment,
) -> Result<Value, SchemeError> {
    eval_type_predicate("symbol?", operands, environment, Value::is_symbol)
}

fn eval_type_predicate(
    operator: &'static str,
    operands: &[Expr],
    environment: &Environment,
    predicate: fn(&Value) -> bool,
) -> Result<Value, SchemeError> {
    match operands {
        [operand] => {
            let value = eval_expr(operand, environment)?;
            Ok(Value::Boolean(predicate(&value)))
        }
        _ => Err(SchemeError::WrongArgumentCount {
            operator,
            expected: 1,
            actual: operands.len(),
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

fn eval_numbers(
    operator: &'static str,
    operands: &[Expr],
    environment: &Environment,
) -> Result<Vec<i64>, SchemeError> {
    eval_values(operands, environment)?
        .into_iter()
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

fn is_builtin(operator: &str) -> bool {
    matches!(
        operator,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | "not"
            | "and"
            | "or"
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "string?"
            | "number?"
            | "boolean?"
            | "pair?"
            | "symbol?"
    )
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
