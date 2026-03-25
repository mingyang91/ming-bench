use super::builtins::default_env;
use super::core::{
    make_lambda, quote_expr, EnvRef, Environment, Expr, LambdaProcedure, Procedure, Runtime, Value,
};
use super::error::EvalError;

#[derive(Clone, Copy)]
enum SpecialForm {
    Define,
    If,
    Quote,
    Lambda,
    And,
    Or,
    Let,
    Begin,
    Cond,
}

impl SpecialForm {
    fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "define" => Some(Self::Define),
            "if" => Some(Self::If),
            "quote" => Some(Self::Quote),
            "lambda" => Some(Self::Lambda),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "let" => Some(Self::Let),
            "begin" => Some(Self::Begin),
            "cond" => Some(Self::Cond),
            _ => None,
        }
    }

    fn eval(self, args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
        match self {
            Self::Define => eval_define(args, env, runtime),
            Self::If => eval_if(args, env, runtime),
            Self::Quote => eval_quote(args),
            Self::Lambda => eval_lambda(args, env),
            Self::And => eval_and(args, env, runtime),
            Self::Or => eval_or(args, env, runtime),
            Self::Let => eval_let(args, env, runtime),
            Self::Begin => eval_begin(args, env, runtime),
            Self::Cond => eval_cond(args, env, runtime),
        }
    }
}

pub(crate) fn eval_program(
    expressions: &[Expr],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if expressions.is_empty() {
        return Err(EvalError::EmptyInput);
    }

    let env = default_env();
    eval_sequence(expressions, &env, runtime)
}

pub(crate) fn eval_sequence(
    expressions: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    let mut last = Value::Void;

    for expression in expressions {
        last = eval_expr(expression, env, runtime)?;
    }

    Ok(last)
}

fn eval_expr(expr: &Expr, env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    match expr {
        Expr::Bool(value, _) => Ok(Value::Bool(*value)),
        Expr::Int(value, _) => Ok(Value::Int(*value)),
        Expr::String(value, _) => Ok(super::core::make_string(value.clone())),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, pos) => Environment::lookup(env, name)
            .ok_or_else(|| pos.attach(EvalError::UnboundSymbol { name: name.clone() })),
        Expr::List(items, pos) => eval_list(items, env, runtime).map_err(|error| pos.attach(error)),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or_else(empty_list_error)?;

    if let Expr::Symbol(name, _) = head {
        if let Some(special_form) = SpecialForm::from_symbol(name) {
            return special_form.eval(args, env, runtime);
        }
    }

    let operator = eval_expr(head, env, runtime)?;
    let values = args
        .iter()
        .map(|arg| eval_expr(arg, env, runtime))
        .collect::<Result<Vec<_>, _>>()?;
    apply_procedure(operator, &values, runtime)
}

fn eval_define(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((target, rest)) = args.split_first() else {
        return Err(wrong_arg_count("define", "at least 2", 0));
    };

    match target {
        Expr::Symbol(name, _) => eval_variable_define(name, rest, env, runtime, args.len()),
        Expr::List(signature, _) => eval_function_define(signature, rest, env, args.len()),
        Expr::Bool(_, _) | Expr::Int(_, _) | Expr::String(_, _) | Expr::Char(_, _) => Err(
            positioned_syntax_error(target, "define requires a symbol or function signature"),
        ),
    }
}

fn eval_variable_define(
    name: &str,
    rest: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
    actual: usize,
) -> Result<Value, EvalError> {
    let [value_expr] = rest else {
        return Err(wrong_arg_count("define", "exactly 2", actual));
    };

    let value = eval_expr(value_expr, env, runtime)?;
    Environment::define(env, name.to_string(), value);
    Ok(Value::Void)
}

fn eval_function_define(
    signature: &[Expr],
    rest: &[Expr],
    env: &EnvRef,
    actual: usize,
) -> Result<Value, EvalError> {
    let Some((name_expr, params_exprs)) = signature.split_first() else {
        return Err(syntax_error("define requires a function name"));
    };

    if rest.is_empty() {
        return Err(wrong_arg_count("define", "at least 2", actual));
    }

    let name = expect_symbol_expr(name_expr, "define function name")?;
    let params = parse_params(params_exprs)?;
    let lambda = make_lambda(Some(name.clone()), params, rest.to_vec(), env);
    Environment::define(env, name, lambda);
    Ok(Value::Void)
}

fn eval_if(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [condition, consequent, alternate] = args else {
        return Err(wrong_arg_count("if", "exactly 3", args.len()));
    };

    if eval_expr(condition, env, runtime)?.is_truthy() {
        eval_expr(consequent, env, runtime)
    } else {
        eval_expr(alternate, env, runtime)
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    let [expr] = args else {
        return Err(wrong_arg_count("quote", "exactly 1", args.len()));
    };

    Ok(quote_expr(expr))
}

fn eval_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let Some((params_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count("lambda", "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count("lambda", "at least 2", 1));
    }

    let params = match params_expr {
        Expr::List(params, _) => parse_params(params)?,
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                params_expr,
                "lambda parameters must be a list",
            ));
        }
    };

    Ok(make_lambda(None, params, body.to_vec(), env))
}

fn eval_and(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut last = Value::Bool(true);

    for arg in args {
        let value = eval_expr(arg, env, runtime)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let mut last = Value::Bool(false);

    for arg in args {
        let value = eval_expr(arg, env, runtime)?;
        if value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_begin(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    eval_sequence(args, env, runtime)
}

fn eval_let(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((head, tail)) = args.split_first() else {
        return Err(wrong_arg_count("let", "at least 2", 0));
    };

    match head {
        Expr::Symbol(name, _) => eval_named_let_form(name, tail, env, runtime, args.len()),
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _) => {
            let bindings = parse_let_bindings(head)?;
            eval_plain_let(&bindings, tail, env, runtime)
        }
    }
}

fn eval_named_let_form(
    name: &str,
    tail: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
    actual: usize,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = tail.split_first() else {
        return Err(wrong_arg_count("let", "at least 3", actual));
    };

    let bindings = parse_let_bindings(bindings_expr)?;
    eval_named_let(name, &bindings, body, env, runtime)
}

fn eval_plain_let(
    bindings: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(wrong_arg_count("let", "at least 2", 1));
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env, runtime))
        .collect::<Result<Vec<_>, _>>()?;
    let local_env = Environment::new(Some(env.clone()));

    for ((name, _), value) in bindings.iter().zip(values) {
        Environment::define(&local_env, name.clone(), value);
    }

    eval_sequence(body, &local_env, runtime)
}

fn eval_named_let(
    name: &str,
    bindings: &[(String, Expr)],
    body: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(wrong_arg_count("let", "at least 3", 2));
    }

    let values = bindings
        .iter()
        .map(|(_, expr)| eval_expr(expr, env, runtime))
        .collect::<Result<Vec<_>, _>>()?;
    let params = bindings
        .iter()
        .map(|(binding_name, _)| binding_name.clone())
        .collect();
    let recursive_env = Environment::new(Some(env.clone()));
    let procedure = make_lambda(
        Some(name.to_string()),
        params,
        body.to_vec(),
        &recursive_env,
    );

    Environment::define(&recursive_env, name.to_string(), procedure.clone());
    apply_procedure(procedure, &values, runtime)
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                bindings_expr,
                "let bindings must be a list",
            ));
        }
    };

    bindings
        .iter()
        .map(parse_let_binding)
        .collect::<Result<Vec<_>, _>>()
}

fn parse_let_binding(binding: &Expr) -> Result<(String, Expr), EvalError> {
    match binding {
        Expr::List(parts, _) if parts.len() == 2 => Ok((
            expect_symbol_expr(&parts[0], "let binding name")?,
            parts[1].clone(),
        )),
        Expr::List(_, _) => Err(positioned_syntax_error(
            binding,
            "let bindings must contain exactly a name and value",
        )),
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            binding,
            "let binding must be a list",
        )),
    }
}

fn eval_cond(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    for clause in args {
        let parts = cond_clause_parts(clause)?;

        if is_else_clause(parts) {
            return eval_cond_clause_body(&parts[1..], env, Value::Bool(true), runtime);
        }

        let test_value = eval_expr(&parts[0], env, runtime)?;
        if test_value.is_truthy() {
            return eval_cond_clause_body(&parts[1..], env, test_value, runtime);
        }
    }

    Ok(Value::Void)
}

fn cond_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause must be a list",
        )),
    }
}

fn is_else_clause(parts: &[Expr]) -> bool {
    matches!(&parts[0], Expr::Symbol(symbol, _) if symbol == "else")
}

fn eval_cond_clause_body(
    args: &[Expr],
    env: &EnvRef,
    fallback: Value,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if args.is_empty() {
        Ok(fallback)
    } else {
        eval_sequence(args, env, runtime)
    }
}

fn expect_symbol_expr(expr: &Expr, context: &str) -> Result<String, EvalError> {
    match expr {
        Expr::Symbol(name, _) => Ok(name.clone()),
        Expr::Bool(_, _)
        | Expr::Int(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            format!("{context} must be a symbol"),
        )),
    }
}

fn parse_params(params: &[Expr]) -> Result<Vec<String>, EvalError> {
    params
        .iter()
        .map(|param| expect_symbol_expr(param, "parameter"))
        .collect()
}

fn apply_procedure(
    operator: Value,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => (builtin.func)(args, runtime),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args, runtime),
        },
        Value::Bool(_)
        | Value::Int(_)
        | Value::String(_)
        | Value::Symbol(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Void => Err(EvalError::NotAProcedure {
            found: operator.render_for_error(),
        }),
    }
}

fn apply_lambda(
    lambda: &LambdaProcedure,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if args.len() != lambda.params.len() {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected: format!("exactly {}", lambda.params.len()),
            actual: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().zip(args.iter()) {
        Environment::define(&call_env, param.clone(), value.clone());
    }

    eval_sequence(&lambda.body, &call_env, runtime)
}

fn empty_list_error() -> EvalError {
    syntax_error("cannot evaluate an empty list")
}

fn syntax_error(message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        message: message.into(),
    }
}

fn positioned_syntax_error(expr: &Expr, message: impl Into<String>) -> EvalError {
    expr.pos().attach(syntax_error(message))
}

fn wrong_arg_count(name: &str, expected: impl Into<String>, actual: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        actual,
    }
}
