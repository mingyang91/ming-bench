use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::expr::Expr;
use crate::scheme::reader::read_all;
use crate::scheme::source::SourcePos;
use crate::scheme::value::{Builtin, Value};

pub fn evaluate_program(input: &str) -> Result<(Value, String), EvalError> {
    let expressions = read_all(input)?;
    let env = Env::with_builtins();
    let mut output = String::new();
    let value = eval_sequence(&expressions, &env, &mut output)?;
    Ok((value, output))
}

fn eval_sequence(expressions: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut current = Value::Void;

    for expr in expressions {
        current = eval_sequence_expr(expr, env, output)?;
    }

    Ok(current)
}

fn eval_sequence_expr(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if let Some((arguments, position)) = define_form(expr) {
        eval_define(arguments, env, output, position)?;
        Ok(Value::Void)
    } else {
        eval(expr, env, output)
    }
}

fn define_form(expr: &Expr) -> Option<(&[Expr], SourcePos)> {
    let Expr::List(items, position) = expr else {
        return None;
    };

    match items.as_slice() {
        [Expr::Symbol(name, _), arguments @ ..] if name == "define" => Some((arguments, *position)),
        _ => None,
    }
}

fn eval(expr: &Expr, env: &Env, output: &mut String) -> Result<Value, EvalError> {
    match expr {
        Expr::Literal(value, _) => Ok(value.clone()),
        Expr::Symbol(name, position) => env
            .lookup(name)
            .ok_or_else(|| EvalError::at(*position, format!("unbound symbol '{name}'"))),
        Expr::List(items, position) => eval_list(items, env, output, *position),
    }
}

fn eval_list(
    items: &[Expr],
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::at(position, "cannot evaluate empty list"));
    };

    match operator {
        Expr::Symbol(name, _) if name == "if" => eval_if(arguments, env, output, position),
        Expr::Symbol(name, _) if name == "quote" => eval_quote(arguments, position),
        Expr::Symbol(name, _) if name == "lambda" => eval_lambda(arguments, env, position),
        Expr::Symbol(name, _) if name == "and" => eval_and(arguments, env, output),
        Expr::Symbol(name, _) if name == "or" => eval_or(arguments, env, output),
        Expr::Symbol(name, _) if name == "let" => eval_let(arguments, env, output, position),
        Expr::Symbol(name, _) if name == "begin" => eval_sequence(arguments, env, output),
        Expr::Symbol(name, _) if name == "cond" => eval_cond(arguments, env, output, position),
        Expr::Symbol(name, _) if name == "define" => {
            Err(EvalError::at(position, "define is only allowed in a sequence"))
        }
        _ => {
            let procedure = eval(operator, env, output)?;
            let evaluated_arguments = eval_arguments(arguments, env, output)?;
            apply_procedure(procedure, evaluated_arguments, position, output)
        }
    }
}

fn eval_define(
    arguments: &[Expr],
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<(), EvalError> {
    match arguments {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval(value_expr, env, output)?;
            env.define(name.clone(), value);
            Ok(())
        }
        [Expr::List(signature, _), body @ ..] if !body.is_empty() => {
            let Some((Expr::Symbol(name, _), parameters)) = signature.split_first() else {
                return Err(EvalError::at(position, "invalid define form"));
            };

            let closure = Value::Closure {
                parameters: read_parameters(parameters)?,
                body: body.to_vec(),
                env: env.clone(),
            };
            env.define(name.clone(), closure);
            Ok(())
        }
        _ => Err(EvalError::at(position, "invalid define form")),
    }
}

fn eval_if(
    arguments: &[Expr],
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                eval(else_branch, env, output)
            }
        }
        _ => Err(EvalError::at(position, "'if' expects exactly 3 arguments")),
    }
}

fn eval_quote(arguments: &[Expr], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [datum] => Ok(quote_to_value(datum)),
        _ => Err(EvalError::at(position, "'quote' expects exactly 1 argument")),
    }
}

fn eval_lambda(arguments: &[Expr], env: &Env, position: SourcePos) -> Result<Value, EvalError> {
    let Some((parameter_expr, body)) = arguments.split_first() else {
        return Err(EvalError::at(position, "invalid lambda form"));
    };

    if body.is_empty() {
        return Err(EvalError::at(position, "invalid lambda form"));
    }

    let Expr::List(parameters, _) = parameter_expr else {
        return Err(EvalError::at(position, "lambda parameters must be a list"));
    };

    Ok(Value::Closure {
        parameters: read_parameters(parameters)?,
        body: body.to_vec(),
        env: env.clone(),
    })
}

fn eval_and(arguments: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut current = Value::Bool(true);

    for argument in arguments {
        current = eval(argument, env, output)?;
        if !current.is_truthy() {
            return Ok(current);
        }
    }

    Ok(current)
}

fn eval_or(arguments: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    let mut current = Value::Bool(false);

    for argument in arguments {
        current = eval(argument, env, output)?;
        if current.is_truthy() {
            return Ok(current);
        }
    }

    Ok(current)
}

fn eval_let(
    arguments: &[Expr],
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<Value, EvalError> {
    let Some((binding_expr, body)) = arguments.split_first() else {
        return Err(EvalError::at(position, "invalid let form"));
    };

    if body.is_empty() {
        return Err(EvalError::at(position, "invalid let form"));
    }

    let Expr::List(bindings, _) = binding_expr else {
        return Err(EvalError::at(position, "let bindings must be a list"));
    };

    let child = Env::child(env);

    for binding in bindings {
        match binding {
            Expr::List(items, binding_pos) => match items.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    let value = eval(value_expr, env, output)?;
                    child.define(name.clone(), value);
                }
                _ => return Err(EvalError::at(*binding_pos, "invalid let binding")),
            },
            _ => return Err(EvalError::at(binding.position(), "invalid let binding")),
        }
    }

    eval_sequence(body, &child, output)
}

fn eval_cond(
    arguments: &[Expr],
    env: &Env,
    output: &mut String,
    _position: SourcePos,
) -> Result<Value, EvalError> {
    for clause in arguments {
        let Expr::List(items, clause_pos) = clause else {
            return Err(EvalError::at(clause.position(), "cond clause must be a list"));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::at(*clause_pos, "cond clause cannot be empty"));
        };

        if matches!(test, Expr::Symbol(name, _) if name == "else") {
            return eval_cond_body(body, env, output);
        }

        let test_value = eval(test, env, output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(test_value)
            } else {
                eval_cond_body(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_cond_body(body: &[Expr], env: &Env, output: &mut String) -> Result<Value, EvalError> {
    if body.is_empty() {
        Ok(Value::Void)
    } else {
        eval_sequence(body, env, output)
    }
}

fn quote_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Literal(value, _) => value.clone(),
        Expr::Symbol(name, _) => Value::Symbol(name.clone()),
        Expr::List(items, _) => Value::from_list(items.iter().map(quote_to_value).collect()),
    }
}

fn apply_procedure(
    procedure: Value,
    arguments: Vec<(Value, SourcePos)>,
    position: SourcePos,
    output: &mut String,
) -> Result<Value, EvalError> {
    match procedure {
        Value::Builtin(name) => apply_builtin(name, &arguments, position, output),
        Value::Closure {
            parameters,
            body,
            env,
        } => {
            if parameters.len() != arguments.len() {
                return Err(EvalError::at(
                    position,
                    format!(
                        "wrong argument count: expected {}, got {}",
                        parameters.len(),
                        arguments.len()
                    ),
                ));
            }

            let child = Env::child(&env);
            for (parameter, (value, _)) in parameters.into_iter().zip(arguments.into_iter()) {
                child.define(parameter, value);
            }

            eval_sequence(&body, &child, output)
        }
        _ => Err(EvalError::at(position, "attempted to call a non-procedure")),
    }
}

fn apply_builtin(
    builtin: Builtin,
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    output: &mut String,
) -> Result<Value, EvalError> {
    match builtin {
        Builtin::Add => Ok(Value::Number(expect_numbers(arguments)?.into_iter().sum())),
        Builtin::Mul => Ok(Value::Number(
            expect_numbers(arguments)?.into_iter().product::<i64>(),
        )),
        Builtin::Sub => eval_sub(arguments, position),
        Builtin::Div => eval_div(arguments, position),
        Builtin::LessThan => eval_compare(arguments, position, |left, right| left < right),
        Builtin::GreaterThan => eval_compare(arguments, position, |left, right| left > right),
        Builtin::Equal => eval_compare(arguments, position, |left, right| left == right),
        Builtin::LessEqual => eval_compare(arguments, position, |left, right| left <= right),
        Builtin::Not => eval_not(arguments, position),
        Builtin::Cons => eval_cons(arguments, position),
        Builtin::Car => eval_car(arguments, position),
        Builtin::Cdr => eval_cdr(arguments, position),
        Builtin::NullPred => eval_null_pred(arguments, position),
        Builtin::List => Ok(Value::from_list(
            arguments.iter().map(|(value, _)| value.clone()).collect(),
        )),
        Builtin::Length => eval_length(arguments, position),
        Builtin::StringPred => eval_predicate(arguments, position, |value| matches!(value, Value::Str(_))),
        Builtin::NumberPred => eval_predicate(arguments, position, |value| matches!(value, Value::Number(_))),
        Builtin::BooleanPred => {
            eval_predicate(arguments, position, |value| matches!(value, Value::Bool(_)))
        }
        Builtin::PairPred => eval_predicate(arguments, position, |value| matches!(value, Value::Pair(_, _))),
        Builtin::SymbolPred => {
            eval_predicate(arguments, position, |value| matches!(value, Value::Symbol(_)))
        }
        Builtin::Display => eval_display(arguments, position, output),
        Builtin::Write => eval_write(arguments, position, output),
        Builtin::Newline => eval_newline(arguments, position, output),
        Builtin::StringAppend => eval_string_append(arguments),
        Builtin::StringLength => eval_string_length(arguments, position),
        Builtin::Substring => eval_substring(arguments, position),
        Builtin::StringToNumber => eval_string_to_number(arguments, position),
        Builtin::NumberToString => eval_number_to_string(arguments, position),
        Builtin::SymbolToString => eval_symbol_to_string(arguments, position),
        Builtin::StringToSymbol => eval_string_to_symbol(arguments, position),
        Builtin::StringRef => eval_string_ref(arguments, position),
        Builtin::CharPred => eval_predicate(arguments, position, |value| matches!(value, Value::Char(_))),
    }
}

fn eval_arguments(
    arguments: &[Expr],
    env: &Env,
    output: &mut String,
) -> Result<Vec<(Value, SourcePos)>, EvalError> {
    arguments
        .iter()
        .map(|argument| eval(argument, env, output).map(|value| (value, argument.position())))
        .collect()
}

fn read_parameters(parameters: &[Expr]) -> Result<Vec<String>, EvalError> {
    parameters
        .iter()
        .map(|parameter| match parameter {
            Expr::Symbol(name, _) => Ok(name.clone()),
            _ => Err(EvalError::at(
                parameter.position(),
                "parameter must be a symbol",
            )),
        })
        .collect()
}

fn expect_numbers(arguments: &[(Value, SourcePos)]) -> Result<Vec<i64>, EvalError> {
    arguments
        .iter()
        .map(|(value, position)| match value {
            Value::Number(number) => Ok(*number),
            _ => Err(EvalError::at(*position, "expected number")),
        })
        .collect()
}

fn expect_string(value: &Value, position: SourcePos) -> Result<String, EvalError> {
    match value {
        Value::Str(text) => Ok(text.clone()),
        _ => Err(EvalError::at(position, "expected string")),
    }
}

fn expect_symbol(value: &Value, position: SourcePos) -> Result<String, EvalError> {
    match value {
        Value::Symbol(name) => Ok(name.clone()),
        _ => Err(EvalError::at(position, "expected symbol")),
    }
}

fn expect_index(value: &Value, position: SourcePos) -> Result<usize, EvalError> {
    match value {
        Value::Number(number) if *number >= 0 => usize::try_from(*number)
            .map_err(|_| EvalError::at(position, "index is out of range")),
        Value::Number(_) => Err(EvalError::at(position, "expected non-negative index")),
        _ => Err(EvalError::at(position, "expected number")),
    }
}

fn eval_sub(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    let numbers = expect_numbers(arguments)?;

    match numbers.as_slice() {
        [] => Err(EvalError::at(position, "'-' expects at least 1 argument")),
        [single] => Ok(Value::Number(-single)),
        [head, tail @ ..] => Ok(Value::Number(
            tail.iter().fold(*head, |current, number| current - number),
        )),
    }
}

fn eval_div(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    let numbers = expect_numbers(arguments)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::at(position, "'/' expects at least 2 arguments")),
        [head, tail @ ..] => {
            let mut current = *head;
            for number in tail {
                if *number == 0 {
                    return Err(EvalError::at(position, "division by zero"));
                }
                current /= number;
            }
            Ok(Value::Number(current))
        }
    }
}

fn eval_compare(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    predicate: fn(i64, i64) -> bool,
) -> Result<Value, EvalError> {
    let numbers = expect_numbers(arguments)?;

    match numbers.as_slice() {
        [] | [_] => Err(EvalError::at(
            position,
            "comparison expects at least 2 arguments",
        )),
        [first, rest @ ..] => {
            let mut previous = *first;
            for current in rest {
                if !predicate(previous, *current) {
                    return Ok(Value::Bool(false));
                }
                previous = *current;
            }
            Ok(Value::Bool(true))
        }
    }
}

fn eval_not(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(value, _)] => Ok(Value::Bool(!value.is_truthy())),
        _ => Err(EvalError::at(position, "'not' expects exactly 1 argument")),
    }
}

fn eval_cons(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(car, _), (cdr, _)] => Ok(Value::Pair(Box::new(car.clone()), Box::new(cdr.clone()))),
        _ => Err(EvalError::at(position, "'cons' expects exactly 2 arguments")),
    }
}

fn eval_car(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(Value::Pair(car, _), _)] => Ok((**car).clone()),
        [(_, arg_position)] => Err(EvalError::at(*arg_position, "expected pair")),
        _ => Err(EvalError::at(position, "'car' expects exactly 1 argument")),
    }
}

fn eval_cdr(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(Value::Pair(_, cdr), _)] => Ok((**cdr).clone()),
        [(_, arg_position)] => Err(EvalError::at(*arg_position, "expected pair")),
        _ => Err(EvalError::at(position, "'cdr' expects exactly 1 argument")),
    }
}

fn eval_null_pred(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(value, _)] => Ok(Value::Bool(matches!(value, Value::EmptyList))),
        _ => Err(EvalError::at(position, "'null?' expects exactly 1 argument")),
    }
}

fn eval_length(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(value, arg_position)] => Ok(Value::Number(proper_list_len(value, *arg_position)? as i64)),
        _ => Err(EvalError::at(position, "'length' expects exactly 1 argument")),
    }
}

fn eval_predicate(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    predicate: fn(&Value) -> bool,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, _)] => Ok(Value::Bool(predicate(value))),
        _ => Err(EvalError::at(position, "predicate expects exactly 1 argument")),
    }
}

fn eval_display(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    output: &mut String,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, _)] => {
            output.push_str(&value.render_display());
            Ok(Value::Void)
        }
        _ => Err(EvalError::at(position, "'display' expects exactly 1 argument")),
    }
}

fn eval_write(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    output: &mut String,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, _)] => {
            output.push_str(&value.render());
            Ok(Value::Void)
        }
        _ => Err(EvalError::at(position, "'write' expects exactly 1 argument")),
    }
}

fn eval_newline(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
    output: &mut String,
) -> Result<Value, EvalError> {
    match arguments {
        [] => {
            output.push('\n');
            Ok(Value::Void)
        }
        _ => Err(EvalError::at(position, "'newline' expects exactly 0 arguments")),
    }
}

fn eval_string_append(arguments: &[(Value, SourcePos)]) -> Result<Value, EvalError> {
    let mut result = String::new();

    for (value, position) in arguments {
        result.push_str(&expect_string(value, *position)?);
    }

    Ok(Value::Str(result))
}

fn eval_string_length(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, arg_position)] => Ok(Value::Number(
            expect_string(value, *arg_position)?.chars().count() as i64,
        )),
        _ => Err(EvalError::at(
            position,
            "'string-length' expects exactly 1 argument",
        )),
    }
}

fn eval_substring(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(value, value_position), (start, start_position), (end, end_position)] => {
            let text = expect_string(value, *value_position)?;
            let start = expect_index(start, *start_position)?;
            let end = expect_index(end, *end_position)?;
            let chars: Vec<char> = text.chars().collect();

            if start > end || end > chars.len() {
                return Err(EvalError::at(position, "substring indices are out of bounds"));
            }

            Ok(Value::Str(chars[start..end].iter().collect()))
        }
        _ => Err(EvalError::at(position, "'substring' expects exactly 3 arguments")),
    }
}

fn eval_string_to_number(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, arg_position)] => {
            let text = expect_string(value, *arg_position)?;
            match text.parse::<i64>() {
                Ok(number) => Ok(Value::Number(number)),
                Err(_) => Ok(Value::Bool(false)),
            }
        }
        _ => Err(EvalError::at(
            position,
            "'string->number' expects exactly 1 argument",
        )),
    }
}

fn eval_number_to_string(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [(Value::Number(number), _)] => Ok(Value::Str(number.to_string())),
        [(_, arg_position)] => Err(EvalError::at(*arg_position, "expected number")),
        _ => Err(EvalError::at(
            position,
            "'number->string' expects exactly 1 argument",
        )),
    }
}

fn eval_symbol_to_string(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, arg_position)] => Ok(Value::Str(expect_symbol(value, *arg_position)?)),
        _ => Err(EvalError::at(
            position,
            "'symbol->string' expects exactly 1 argument",
        )),
    }
}

fn eval_string_to_symbol(
    arguments: &[(Value, SourcePos)],
    position: SourcePos,
) -> Result<Value, EvalError> {
    match arguments {
        [(value, arg_position)] => Ok(Value::Symbol(expect_string(value, *arg_position)?)),
        _ => Err(EvalError::at(
            position,
            "'string->symbol' expects exactly 1 argument",
        )),
    }
}

fn eval_string_ref(arguments: &[(Value, SourcePos)], position: SourcePos) -> Result<Value, EvalError> {
    match arguments {
        [(value, value_position), (index, index_position)] => {
            let text = expect_string(value, *value_position)?;
            let index = expect_index(index, *index_position)?;
            let chars: Vec<char> = text.chars().collect();
            let Some(ch) = chars.get(index) else {
                return Err(EvalError::at(position, "string index out of bounds"));
            };
            Ok(Value::Char(*ch))
        }
        _ => Err(EvalError::at(position, "'string-ref' expects exactly 2 arguments")),
    }
}

fn proper_list_len(value: &Value, position: SourcePos) -> Result<usize, EvalError> {
    let mut current = value;
    let mut length = 0;

    loop {
        match current {
            Value::EmptyList => return Ok(length),
            Value::Pair(_, cdr) => {
                length += 1;
                current = cdr.as_ref();
            }
            _ => return Err(EvalError::at(position, "expected proper list")),
        }
    }
}
