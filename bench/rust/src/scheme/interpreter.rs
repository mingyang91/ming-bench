use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::expr::Expr;
use crate::scheme::reader::read_all;
use crate::scheme::source::SourcePos;
use crate::scheme::value::{Builtin, Value};
use std::ptr::NonNull;
use std::rc::Rc;

enum ExprOwner<'a> {
    Borrowed(&'a Expr),
    Body(Rc<[Expr]>),
}

struct CurrentExpr<'a> {
    owner: ExprOwner<'a>,
    ptr: NonNull<Expr>,
}

impl<'a> CurrentExpr<'a> {
    fn borrowed(expr: &'a Expr) -> Self {
        Self {
            owner: ExprOwner::Borrowed(expr),
            ptr: NonNull::from(expr),
        }
    }

    fn from_body(body: Rc<[Expr]>, index: usize) -> Self {
        Self {
            ptr: NonNull::from(&body[index]),
            owner: ExprOwner::Body(body),
        }
    }

    fn child(&self, expr: &Expr) -> Self {
        let owner = match &self.owner {
            ExprOwner::Borrowed(root) => ExprOwner::Borrowed(root),
            ExprOwner::Body(body) => ExprOwner::Body(body.clone()),
        };

        Self {
            owner,
            ptr: NonNull::from(expr),
        }
    }

    fn expr(&self) -> &Expr {
        // SAFETY: `ptr` is only created from expressions owned by `owner`
        // and the AST is immutable, so the pointed-to node stays valid.
        unsafe { self.ptr.as_ref() }
    }
}

enum EvalControl<'a> {
    Value(Value),
    Tail { expr: CurrentExpr<'a>, env: Env },
}

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
    let mut current = CurrentExpr::borrowed(expr);
    let mut current_env = env.clone();

    loop {
        match current.expr() {
            Expr::Literal(value, _) => return Ok(value.clone()),
            Expr::Symbol(name, position) => {
                return current_env
                    .lookup(name)
                    .ok_or_else(|| EvalError::at(*position, format!("unbound symbol '{name}'")));
            }
            Expr::List(items, position) => {
                match eval_list(items, &current, &current_env, output, *position)? {
                    EvalControl::Value(value) => return Ok(value),
                    EvalControl::Tail { expr, env } => {
                        current = expr;
                        current_env = env;
                    }
                }
            }
        }
    }
}

fn eval_list<'a>(
    items: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<EvalControl<'a>, EvalError> {
    let Some((operator, arguments)) = items.split_first() else {
        return Err(EvalError::at(position, "cannot evaluate empty list"));
    };

    match operator {
        Expr::Symbol(name, _) if name == "if" => eval_if(arguments, current, env, output, position),
        Expr::Symbol(name, _) if name == "quote" => {
            Ok(EvalControl::Value(eval_quote(arguments, position)?))
        }
        Expr::Symbol(name, _) if name == "lambda" => {
            Ok(EvalControl::Value(eval_lambda(arguments, env, position)?))
        }
        Expr::Symbol(name, _) if name == "and" => eval_and(arguments, current, env, output),
        Expr::Symbol(name, _) if name == "or" => eval_or(arguments, current, env, output),
        Expr::Symbol(name, _) if name == "let" => eval_let(arguments, current, env, output, position),
        Expr::Symbol(name, _) if name == "begin" => eval_tail_sequence(arguments, current, env, output),
        Expr::Symbol(name, _) if name == "cond" => eval_cond(arguments, current, env, output),
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
                body: body.to_vec().into(),
                env: env.clone(),
            };
            env.define(name.clone(), closure);
            Ok(())
        }
        _ => Err(EvalError::at(position, "invalid define form")),
    }
}

fn eval_if<'a>(
    arguments: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<EvalControl<'a>, EvalError> {
    match arguments {
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                Ok(EvalControl::Tail {
                    expr: current.child(then_branch),
                    env: env.clone(),
                })
            } else {
                Ok(EvalControl::Tail {
                    expr: current.child(else_branch),
                    env: env.clone(),
                })
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
        body: body.to_vec().into(),
        env: env.clone(),
    })
}

fn eval_and<'a>(
    arguments: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    let Some((last, prefix)) = arguments.split_last() else {
        return Ok(EvalControl::Value(Value::Bool(true)));
    };

    for argument in prefix {
        let value = eval(argument, env, output)?;
        if !value.is_truthy() {
            return Ok(EvalControl::Value(value));
        }
    }

    Ok(EvalControl::Tail {
        expr: current.child(last),
        env: env.clone(),
    })
}

fn eval_or<'a>(
    arguments: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    let Some((last, prefix)) = arguments.split_last() else {
        return Ok(EvalControl::Value(Value::Bool(false)));
    };

    for argument in prefix {
        let value = eval(argument, env, output)?;
        if value.is_truthy() {
            return Ok(EvalControl::Value(value));
        }
    }

    Ok(EvalControl::Tail {
        expr: current.child(last),
        env: env.clone(),
    })
}

fn eval_let<'a>(
    arguments: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<EvalControl<'a>, EvalError> {
    match arguments {
        [Expr::Symbol(name, _), binding_expr, body @ ..] => {
            eval_named_let(name, binding_expr, body, env, output, position)
        }
        [binding_expr, body @ ..] => eval_plain_let(binding_expr, body, current, env, output, position),
        _ => Err(EvalError::at(position, "invalid let form")),
    }
}

fn eval_plain_let<'a>(
    binding_expr: &Expr,
    body: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<EvalControl<'a>, EvalError> {
    if body.is_empty() {
        return Err(EvalError::at(position, "invalid let form"));
    }

    let Expr::List(bindings, _) = binding_expr else {
        return Err(EvalError::at(position, "let bindings must be a list"));
    };

    let child = Env::child(env);
    for (name, value) in read_let_bindings(bindings, env, output)? {
        child.define(name, value);
    }

    eval_tail_sequence(body, current, &child, output)
}

fn eval_named_let<'a>(
    name: &str,
    binding_expr: &Expr,
    body: &[Expr],
    env: &Env,
    output: &mut String,
    position: SourcePos,
) -> Result<EvalControl<'a>, EvalError> {
    if body.is_empty() {
        return Err(EvalError::at(position, "invalid let form"));
    }

    let Expr::List(bindings, _) = binding_expr else {
        return Err(EvalError::at(position, "let bindings must be a list"));
    };

    let values = read_let_bindings(bindings, env, output)?;
    let parameters = values.iter().map(|(parameter, _)| parameter.clone()).collect();
    let body: Rc<[Expr]> = body.to_vec().into();
    let named_env = Env::child(env);

    named_env.define(
        name.to_string(),
        Value::Closure {
            parameters,
            body: body.clone(),
            env: named_env.clone(),
        },
    );

    let child = Env::child(&named_env);
    for (parameter, value) in values {
        child.define(parameter, value);
    }

    eval_tail_body(body, &child, output)
}

fn eval_cond<'a>(
    arguments: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    for clause in arguments {
        let Expr::List(items, clause_pos) = clause else {
            return Err(EvalError::at(clause.position(), "cond clause must be a list"));
        };

        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::at(*clause_pos, "cond clause cannot be empty"));
        };

        if matches!(test, Expr::Symbol(name, _) if name == "else") {
            return eval_tail_sequence(body, current, env, output);
        }

        let test_value = eval(test, env, output)?;
        if test_value.is_truthy() {
            return if body.is_empty() {
                Ok(EvalControl::Value(test_value))
            } else {
                eval_tail_sequence(body, current, env, output)
            };
        }
    }

    Ok(EvalControl::Value(Value::Void))
}

fn eval_tail_sequence<'a>(
    expressions: &[Expr],
    current: &CurrentExpr<'a>,
    env: &Env,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    let Some((last, prefix)) = expressions.split_last() else {
        return Ok(EvalControl::Value(Value::Void));
    };

    for expr in prefix {
        eval_sequence_expr(expr, env, output)?;
    }

    if let Some((arguments, position)) = define_form(last) {
        eval_define(arguments, env, output, position)?;
        Ok(EvalControl::Value(Value::Void))
    } else {
        Ok(EvalControl::Tail {
            expr: current.child(last),
            env: env.clone(),
        })
    }
}

fn eval_tail_body<'a>(
    body: Rc<[Expr]>,
    env: &Env,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    if body.is_empty() {
        return Ok(EvalControl::Value(Value::Void));
    }

    let last_index = body.len() - 1;
    for expr in &body[..last_index] {
        eval_sequence_expr(expr, env, output)?;
    }

    let last = &body[last_index];
    if let Some((arguments, position)) = define_form(last) {
        eval_define(arguments, env, output, position)?;
        Ok(EvalControl::Value(Value::Void))
    } else {
        Ok(EvalControl::Tail {
            expr: CurrentExpr::from_body(body, last_index),
            env: env.clone(),
        })
    }
}

fn quote_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Literal(value, _) => value.clone(),
        Expr::Symbol(name, _) => Value::Symbol(name.clone()),
        Expr::List(items, _) => Value::from_list(items.iter().map(quote_to_value).collect()),
    }
}

fn apply_procedure<'a>(
    procedure: Value,
    arguments: Vec<(Value, SourcePos)>,
    position: SourcePos,
    output: &mut String,
) -> Result<EvalControl<'a>, EvalError> {
    match procedure {
        Value::Builtin(name) => Ok(EvalControl::Value(apply_builtin(
            name,
            &arguments,
            position,
            output,
        )?)),
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

            eval_tail_body(body, &child, output)
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

fn read_let_bindings(
    bindings: &[Expr],
    env: &Env,
    output: &mut String,
) -> Result<Vec<(String, Value)>, EvalError> {
    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List(items, binding_pos) => match items.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    eval(value_expr, env, output).map(|value| (name.clone(), value))
                }
                _ => Err(EvalError::at(*binding_pos, "invalid let binding")),
            },
            _ => Err(EvalError::at(binding.position(), "invalid let binding")),
        })
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
