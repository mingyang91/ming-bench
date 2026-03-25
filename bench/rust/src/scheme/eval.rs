use std::cell::RefCell;
use std::rc::Rc;

use super::builtins::{default_env, eqv_value};
use super::core::{
    make_case_lambda, make_lambda, make_record, make_record_accessor, make_record_constructor,
    make_record_predicate, make_record_type, quote_expr, BindingRef, CaseLambdaProcedure, EnvRef,
    Environment, Expr, LambdaProcedure, Procedure, RecordAccessorProcedure,
    RecordConstructorProcedure, RecordPredicateProcedure, Runtime, Value,
};
use super::error::EvalError;
use super::macros::{expand_macro_call, parse_macro_definition};

struct ParsedParams {
    params: Vec<String>,
    rest_param: Option<String>,
}

struct RecordConstructorSpec {
    name: String,
    field_names: Vec<String>,
}

struct RecordFieldSpec {
    field_name: String,
    accessor_name: String,
}

struct DoBindingSpec {
    name: String,
    init: Expr,
    step: Option<Expr>,
}

#[derive(Clone, Copy)]
enum SpecialForm {
    Define,
    DefineRecordType,
    DefineSyntax,
    Set,
    If,
    Quote,
    Lambda,
    CaseLambda,
    And,
    Or,
    Let,
    Letrec,
    LetrecStar,
    Begin,
    Cond,
    Case,
    Do,
}

impl SpecialForm {
    fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "define" => Some(Self::Define),
            "define-record-type" => Some(Self::DefineRecordType),
            "define-syntax" => Some(Self::DefineSyntax),
            "set!" => Some(Self::Set),
            "if" => Some(Self::If),
            "quote" => Some(Self::Quote),
            "lambda" => Some(Self::Lambda),
            "case-lambda" => Some(Self::CaseLambda),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            "let" => Some(Self::Let),
            "letrec" => Some(Self::Letrec),
            "letrec*" => Some(Self::LetrecStar),
            "begin" => Some(Self::Begin),
            "cond" => Some(Self::Cond),
            "case" => Some(Self::Case),
            "do" => Some(Self::Do),
            _ => None,
        }
    }

    fn eval(self, args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
        match self {
            Self::Define => eval_define(args, env, runtime),
            Self::DefineRecordType => eval_define_record_type(args, env),
            Self::DefineSyntax => eval_define_syntax(args, env, runtime),
            Self::Set => eval_set(args, env, runtime),
            Self::If => eval_if(args, env, runtime),
            Self::Quote => eval_quote(args),
            Self::Lambda => eval_lambda(args, env),
            Self::CaseLambda => eval_case_lambda(args, env),
            Self::And => eval_and(args, env, runtime),
            Self::Or => eval_or(args, env, runtime),
            Self::Let => eval_let(args, env, runtime),
            Self::Letrec => eval_letrec(args, env, runtime),
            Self::LetrecStar => eval_letrec_star(args, env, runtime),
            Self::Begin => eval_begin(args, env, runtime),
            Self::Cond => eval_cond(args, env, runtime),
            Self::Case => eval_case(args, env, runtime),
            Self::Do => eval_do(args, env, runtime),
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
        Expr::Number(value, _) => Ok(Value::Number(*value)),
        Expr::String(value, _) => Ok(super::core::make_immutable_string(value.clone())),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, pos) => match Environment::lookup(env, name) {
            Some(Value::Uninitialized) => {
                Err(pos.attach(EvalError::UninitializedBinding { name: name.clone() }))
            }
            Some(value) => Ok(value),
            None => Err(pos.attach(EvalError::UnboundSymbol { name: name.clone() })),
        },
        Expr::List(items, pos) => eval_list(items, env, runtime).map_err(|error| pos.attach(error)),
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (head, args) = items.split_first().ok_or_else(empty_list_error)?;

    if let Expr::Symbol(name, _) = head {
        if let Some(special_form) = SpecialForm::from_symbol(name) {
            return special_form.eval(args, env, runtime);
        }

        if let Some(transformer) = runtime.lookup_macro(name) {
            let (expanded, expansion_env) = expand_macro_call(&transformer, items, env, runtime)?;
            return eval_expr(&expanded, &expansion_env, runtime);
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
        Expr::Bool(_, _) | Expr::Number(_, _) | Expr::String(_, _) | Expr::Char(_, _) => Err(
            positioned_syntax_error(target, "define requires a symbol or function signature"),
        ),
    }
}

fn eval_define_record_type(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    let [type_name_expr, constructor_expr, predicate_expr, field_exprs @ ..] = args else {
        return Err(wrong_arg_count(
            "define-record-type",
            "at least 3",
            args.len(),
        ));
    };

    let type_name = expect_symbol_expr(type_name_expr, "record type name")?;
    let constructor = parse_record_constructor_spec(constructor_expr)?;
    let predicate_name = expect_symbol_expr(predicate_expr, "record predicate")?;
    let fields = field_exprs
        .iter()
        .map(parse_record_field_spec)
        .collect::<Result<Vec<_>, _>>()?;

    if constructor.field_names.len() != fields.len() {
        return Err(positioned_syntax_error(
            constructor_expr,
            "record constructor fields must match accessor fields",
        ));
    }

    if constructor
        .field_names
        .iter()
        .zip(fields.iter())
        .any(|(constructor_field, field)| constructor_field != &field.field_name)
    {
        return Err(positioned_syntax_error(
            constructor_expr,
            "record constructor fields must match accessor fields",
        ));
    }

    let record_type = make_record_type(type_name, constructor.field_names.clone());
    Environment::define(
        env,
        constructor.name.clone(),
        make_record_constructor(constructor.name.clone(), &record_type),
    );
    Environment::define(
        env,
        predicate_name.clone(),
        make_record_predicate(predicate_name, &record_type),
    );

    for (field_index, field) in fields.iter().enumerate() {
        Environment::define(
            env,
            field.accessor_name.clone(),
            make_record_accessor(field.accessor_name.clone(), &record_type, field_index),
        );
    }

    Ok(Value::Void)
}

fn eval_define_syntax(
    args: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    let [target, transformer_expr] = args else {
        return Err(wrong_arg_count("define-syntax", "exactly 2", args.len()));
    };

    let name = expect_symbol_expr(target, "define-syntax name")?;
    let transformer = parse_macro_definition(&name, transformer_expr, env)?;
    runtime.define_macro(name, transformer);
    Ok(Value::Void)
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
    let lambda = make_lambda(
        Some(name.clone()),
        params.params,
        params.rest_param,
        rest.to_vec(),
        env,
    );
    Environment::define(env, name, lambda);
    Ok(Value::Void)
}

fn eval_if(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let (condition, consequent, alternate) = match args {
        [condition, consequent] => (condition, consequent, None),
        [condition, consequent, alternate] => (condition, consequent, Some(alternate)),
        _ => return Err(wrong_arg_count("if", "exactly 2 or 3", args.len())),
    };

    if eval_expr(condition, env, runtime)?.is_truthy() {
        eval_expr(consequent, env, runtime)
    } else if let Some(alternate) = alternate {
        eval_expr(alternate, env, runtime)
    } else {
        Ok(Value::Void)
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let [target, value_expr] = args else {
        return Err(wrong_arg_count("set!", "exactly 2", args.len()));
    };

    let name = match target {
        Expr::Symbol(name, _) => name,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _) => {
            return Err(positioned_syntax_error(
                target,
                "set! target must be a symbol",
            ));
        }
    };

    let value = eval_expr(value_expr, env, runtime)?;
    if Environment::set(env, name, value) {
        Ok(Value::Void)
    } else {
        Err(target
            .pos()
            .attach(EvalError::UnboundSymbol { name: name.clone() }))
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

    let params = parse_formals(params_expr)?;

    Ok(make_lambda(
        None,
        params.params,
        params.rest_param,
        body.to_vec(),
        env,
    ))
}

fn eval_case_lambda(args: &[Expr], env: &EnvRef) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Err(wrong_arg_count("case-lambda", "at least 1", 0));
    }

    let clauses = args
        .iter()
        .map(|clause| parse_case_lambda_clause(clause, env))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(make_case_lambda(None, clauses))
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
        | Expr::Number(_, _)
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
        None,
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
        | Expr::Number(_, _)
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
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            binding,
            "let binding must be a list",
        )),
    }
}

fn eval_letrec(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    eval_recursive_let("letrec", args, env, runtime, false)
}

fn eval_letrec_star(
    args: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    eval_recursive_let("letrec*", args, env, runtime, true)
}

fn eval_recursive_let(
    name: &str,
    args: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
    sequential: bool,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(wrong_arg_count(name, "at least 2", 0));
    };

    if body.is_empty() {
        return Err(wrong_arg_count(name, "at least 2", 1));
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let local_env = Environment::new(Some(env.clone()));
    let cells = create_recursive_bindings(&local_env, &bindings);

    if sequential {
        for ((_, expr), cell) in bindings.iter().zip(cells.iter()) {
            let value = eval_expr(expr, &local_env, runtime)?;
            *cell.borrow_mut() = value;
        }
    } else {
        let values = bindings
            .iter()
            .map(|(_, expr)| eval_expr(expr, &local_env, runtime))
            .collect::<Result<Vec<_>, _>>()?;

        for (cell, value) in cells.iter().zip(values) {
            *cell.borrow_mut() = value;
        }
    }

    eval_sequence(body, &local_env, runtime)
}

fn create_recursive_bindings(env: &EnvRef, bindings: &[(String, Expr)]) -> Vec<BindingRef> {
    bindings
        .iter()
        .map(|(binding_name, _)| {
            let cell = Rc::new(RefCell::new(Value::Uninitialized));
            Environment::define_cell(env, binding_name.clone(), cell.clone());
            cell
        })
        .collect()
}

fn parse_do_bindings(bindings_expr: &Expr) -> Result<Vec<DoBindingSpec>, EvalError> {
    let bindings = match bindings_expr {
        Expr::List(bindings, _) => bindings,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                bindings_expr,
                "do bindings must be a list",
            ));
        }
    };

    bindings
        .iter()
        .map(parse_do_binding)
        .collect::<Result<Vec<_>, _>>()
}

fn parse_do_binding(binding: &Expr) -> Result<DoBindingSpec, EvalError> {
    match binding {
        Expr::List(parts, _) if (2..=3).contains(&parts.len()) => Ok(DoBindingSpec {
            name: expect_symbol_expr(&parts[0], "do binding name")?,
            init: parts[1].clone(),
            step: parts.get(2).cloned(),
        }),
        Expr::List(_, _) => Err(positioned_syntax_error(
            binding,
            "do bindings must contain a name, init, and optional step",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            binding,
            "do binding must be a list",
        )),
    }
}

fn parse_record_constructor_spec(expr: &Expr) -> Result<RecordConstructorSpec, EvalError> {
    let parts = match expr {
        Expr::List(parts, _) if !parts.is_empty() => parts,
        Expr::List(_, _) => {
            return Err(positioned_syntax_error(
                expr,
                "record constructor must include a name",
            ));
        }
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                expr,
                "record constructor must be a list",
            ));
        }
    };

    let name = expect_symbol_expr(&parts[0], "record constructor name")?;
    let field_names = parts[1..]
        .iter()
        .map(|field| expect_symbol_expr(field, "record constructor field"))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(RecordConstructorSpec { name, field_names })
}

fn parse_record_field_spec(expr: &Expr) -> Result<RecordFieldSpec, EvalError> {
    match expr {
        Expr::List(parts, _) if parts.len() == 2 => Ok(RecordFieldSpec {
            field_name: expect_symbol_expr(&parts[0], "record field name")?,
            accessor_name: expect_symbol_expr(&parts[1], "record accessor name")?,
        }),
        Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            "record field must contain exactly a field name and accessor name",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(expr, "record field must be a list")),
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

fn eval_case(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((key_expr, clauses)) = args.split_first() else {
        return Err(wrong_arg_count("case", "at least 2", 0));
    };

    if clauses.is_empty() {
        return Err(wrong_arg_count("case", "at least 2", 1));
    }

    let key = eval_expr(key_expr, env, runtime)?;

    for (index, clause) in clauses.iter().enumerate() {
        if let Some(result) = eval_case_clause(clause, index, clauses.len(), &key, env, runtime)? {
            return Ok(result);
        }
    }

    Ok(Value::Void)
}

fn eval_case_clause(
    clause: &Expr,
    index: usize,
    clause_count: usize,
    key: &Value,
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Option<Value>, EvalError> {
    let parts = case_clause_parts(clause)?;
    if is_else_clause(parts) {
        validate_case_else_clause(clause, parts, index, clause_count)?;
        return Ok(Some(eval_sequence(&parts[1..], env, runtime)?));
    }

    ensure_case_clause_has_body(clause, parts, "case clause must have a body")?;
    if !case_clause_matches_key(key, &parts[0])? {
        return Ok(None);
    }

    Ok(Some(eval_sequence(&parts[1..], env, runtime)?))
}

fn validate_case_else_clause(
    clause: &Expr,
    parts: &[Expr],
    index: usize,
    clause_count: usize,
) -> Result<(), EvalError> {
    if index + 1 != clause_count {
        return Err(positioned_syntax_error(
            clause,
            "case else clause must be last",
        ));
    }

    ensure_case_clause_has_body(clause, parts, "case else clause must have a body")
}

fn ensure_case_clause_has_body(
    clause: &Expr,
    parts: &[Expr],
    message: &str,
) -> Result<(), EvalError> {
    if parts.len() > 1 {
        return Ok(());
    }

    Err(positioned_syntax_error(clause, message))
}

fn case_clause_matches_key(key: &Value, datums_expr: &Expr) -> Result<bool, EvalError> {
    let datums = case_clause_datums(datums_expr)?;
    Ok(datums
        .iter()
        .any(|datum| eqv_value(key, &quote_expr(datum))))
}

fn eval_do(args: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<Value, EvalError> {
    let Some((bindings_expr, tail)) = args.split_first() else {
        return Err(wrong_arg_count("do", "at least 2", 0));
    };

    let Some((test_clause, body)) = tail.split_first() else {
        return Err(wrong_arg_count("do", "at least 2", 1));
    };

    let bindings = parse_do_bindings(bindings_expr)?;
    let test_parts = do_termination_clause_parts(test_clause)?;
    let (test_expr, result_exprs) = test_parts
        .split_first()
        .expect("do termination clause must contain a test expression");
    let init_values = bindings
        .iter()
        .map(|binding| eval_expr(&binding.init, env, runtime))
        .collect::<Result<Vec<_>, _>>()?;
    let local_env = Environment::new(Some(env.clone()));

    for (binding, value) in bindings.iter().zip(init_values) {
        Environment::define(&local_env, binding.name.clone(), value);
    }

    loop {
        if let Some(result) = eval_do_termination(test_expr, result_exprs, &local_env, runtime)? {
            return Ok(result);
        }

        eval_do_body(body, &local_env, runtime)?;
        let next_values = collect_do_next_values(&bindings, &local_env, runtime)?;
        update_do_bindings(&bindings, &local_env, next_values);
    }
}

fn eval_do_termination(
    test_expr: &Expr,
    result_exprs: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Option<Value>, EvalError> {
    if !eval_expr(test_expr, env, runtime)?.is_truthy() {
        return Ok(None);
    }

    Ok(Some(eval_do_result(result_exprs, env, runtime)?))
}

fn eval_do_result(
    result_exprs: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if result_exprs.is_empty() {
        return Ok(Value::Void);
    }

    eval_sequence(result_exprs, env, runtime)
}

fn eval_do_body(body: &[Expr], env: &EnvRef, runtime: &mut Runtime) -> Result<(), EvalError> {
    if body.is_empty() {
        return Ok(());
    }

    eval_sequence(body, env, runtime).map(|_| ())
}

fn collect_do_next_values(
    bindings: &[DoBindingSpec],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Vec<Value>, EvalError> {
    bindings
        .iter()
        .map(|binding| eval_do_step(binding, env, runtime))
        .collect()
}

fn eval_do_step(
    binding: &DoBindingSpec,
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    match &binding.step {
        Some(step) => eval_expr(step, env, runtime),
        None => Ok(
            Environment::lookup(env, &binding.name).expect("do binding should remain available")
        ),
    }
}

fn update_do_bindings(bindings: &[DoBindingSpec], env: &EnvRef, next_values: Vec<Value>) {
    for (binding, value) in bindings.iter().zip(next_values) {
        let updated = Environment::set(env, &binding.name, value);
        debug_assert!(
            updated,
            "do binding should be mutable in its local environment"
        );
    }
}

fn cond_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            clause,
            "cond clause must be a list",
        )),
    }
}

fn case_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "case clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            clause,
            "case clause must be a list",
        )),
    }
}

fn case_clause_datums(expr: &Expr) -> Result<&[Expr], EvalError> {
    match expr {
        Expr::List(datums, _) if !datums.is_empty() => Ok(datums),
        Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            "case clause must include at least one datum",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            expr,
            "case clause datums must be a list",
        )),
    }
}

fn do_termination_clause_parts(clause: &Expr) -> Result<&[Expr], EvalError> {
    match clause {
        Expr::List(parts, _) if !parts.is_empty() => Ok(parts),
        Expr::List(_, _) => Err(positioned_syntax_error(
            clause,
            "do termination clause cannot be empty",
        )),
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => Err(positioned_syntax_error(
            clause,
            "do termination clause must be a list",
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
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::List(_, _) => Err(positioned_syntax_error(
            expr,
            format!("{context} must be a symbol"),
        )),
    }
}

fn parse_formals(params_expr: &Expr) -> Result<ParsedParams, EvalError> {
    match params_expr {
        Expr::List(params, _) => parse_params(params),
        Expr::Symbol(name, _) if name != "." => Ok(ParsedParams {
            params: Vec::new(),
            rest_param: Some(name.clone()),
        }),
        Expr::Symbol(_, _)
        | Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _) => Err(positioned_syntax_error(
            params_expr,
            "lambda parameters must be a list or symbol",
        )),
    }
}

fn parse_case_lambda_clause(clause: &Expr, env: &EnvRef) -> Result<LambdaProcedure, EvalError> {
    let parts = match clause {
        Expr::List(parts, _) => parts,
        Expr::Bool(_, _)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                clause,
                "case-lambda clause must be a list",
            ));
        }
    };

    let Some((params_expr, body)) = parts.split_first() else {
        return Err(positioned_syntax_error(
            clause,
            "case-lambda clause cannot be empty",
        ));
    };

    if body.is_empty() {
        return Err(positioned_syntax_error(
            clause,
            "case-lambda clause must have a body",
        ));
    }

    let params = parse_formals(params_expr)?;
    Ok(LambdaProcedure {
        name: None,
        params: params.params,
        rest_param: params.rest_param,
        body: body.to_vec(),
        env: env.clone(),
    })
}

fn parse_params(params: &[Expr]) -> Result<ParsedParams, EvalError> {
    let mut parsed = ParsedParams {
        params: Vec::new(),
        rest_param: None,
    };
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(symbol, _) if symbol == "." => {
                parsed.rest_param = Some(parse_rest_param(params, index)?);
                return Ok(parsed);
            }
            param => parsed.params.push(expect_param_name(param)?),
        }

        index += 1;
    }

    Ok(parsed)
}

fn parse_rest_param(params: &[Expr], index: usize) -> Result<String, EvalError> {
    let dot = &params[index];
    let Some(rest_expr) = params.get(index + 1) else {
        return Err(positioned_syntax_error(
            dot,
            "parameter list is missing a rest parameter name",
        ));
    };

    if index + 2 != params.len() {
        return Err(positioned_syntax_error(
            dot,
            "parameter list allows only one rest parameter",
        ));
    }

    expect_param_name(rest_expr)
}

fn expect_param_name(expr: &Expr) -> Result<String, EvalError> {
    let name = expect_symbol_expr(expr, "parameter")?;
    if name == "." {
        Err(positioned_syntax_error(
            expr,
            "parameter name cannot be '.'",
        ))
    } else {
        Ok(name)
    }
}

pub(crate) fn apply_procedure(
    operator: Value,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    match operator {
        Value::Procedure(procedure) => match procedure.as_ref() {
            Procedure::Builtin(builtin) => (builtin.func)(args, runtime),
            Procedure::Lambda(lambda) => apply_lambda(lambda, args, runtime),
            Procedure::CaseLambda(case_lambda) => apply_case_lambda(case_lambda, args, runtime),
            Procedure::RecordConstructor(constructor) => {
                apply_record_constructor(constructor, args)
            }
            Procedure::RecordPredicate(predicate) => apply_record_predicate(predicate, args),
            Procedure::RecordAccessor(accessor) => apply_record_accessor(accessor, args),
        },
        Value::Bool(_)
        | Value::Number(_)
        | Value::String(_)
        | Value::Symbol(_)
        | Value::Char(_)
        | Value::List(_)
        | Value::Pair(_)
        | Value::Vector(_)
        | Value::Record(_)
        | Value::Uninitialized
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
    let required_len = lambda.params.len();
    let rest_param = lambda.rest_param.as_ref();

    if !lambda_accepts_arity(lambda, args.len()) {
        return Err(EvalError::WrongArgCount {
            name: lambda.name.clone().unwrap_or_else(|| "lambda".into()),
            expected: lambda_expected_arity(lambda),
            actual: args.len(),
        });
    }

    let call_env = Environment::new(Some(lambda.env.clone()));

    for (param, value) in lambda.params.iter().zip(args.iter()) {
        Environment::define(&call_env, param.clone(), value.clone());
    }

    if let Some(rest_param) = rest_param {
        Environment::define(
            &call_env,
            rest_param.clone(),
            Value::List(args[required_len..].to_vec()),
        );
    }

    eval_sequence(&lambda.body, &call_env, runtime)
}

fn apply_case_lambda(
    case_lambda: &CaseLambdaProcedure,
    args: &[Value],
    runtime: &mut Runtime,
) -> Result<Value, EvalError> {
    if let Some(clause) = case_lambda
        .clauses
        .iter()
        .find(|clause| lambda_accepts_arity(clause, args.len()))
    {
        return apply_lambda(clause, args, runtime);
    }

    Err(EvalError::WrongArgCount {
        name: case_lambda
            .name
            .clone()
            .unwrap_or_else(|| "case-lambda".into()),
        expected: case_lambda_expected(case_lambda),
        actual: args.len(),
    })
}

fn lambda_accepts_arity(lambda: &LambdaProcedure, actual: usize) -> bool {
    actual >= lambda.params.len() && (lambda.rest_param.is_some() || actual == lambda.params.len())
}

fn lambda_expected_arity(lambda: &LambdaProcedure) -> String {
    if lambda.rest_param.is_some() {
        format!("at least {}", lambda.params.len())
    } else {
        format!("exactly {}", lambda.params.len())
    }
}

fn case_lambda_expected(case_lambda: &CaseLambdaProcedure) -> String {
    let mut expected = Vec::with_capacity(case_lambda.clauses.len());

    for clause in &case_lambda.clauses {
        let signature = lambda_expected_arity(clause);
        if !expected.contains(&signature) {
            expected.push(signature);
        }
    }

    match expected.len() {
        0 => "no matching clause".into(),
        1 => expected.pop().expect("single expected clause"),
        _ => format!("one of {}", expected.join(", ")),
    }
}

fn apply_record_constructor(
    constructor: &RecordConstructorProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let field_count = constructor.record_type.as_ref().field_names.len();
    if args.len() != field_count {
        return Err(EvalError::WrongArgCount {
            name: constructor.name.clone(),
            expected: format!("exactly {field_count}"),
            actual: args.len(),
        });
    }

    Ok(make_record(&constructor.record_type, args.to_vec()))
}

fn apply_record_predicate(
    predicate: &RecordPredicateProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: predicate.name.clone(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    };

    Ok(Value::Bool(matches!(
        value,
        Value::Record(record)
            if Rc::ptr_eq(&record.as_ref().record_type, &predicate.record_type)
    )))
}

fn apply_record_accessor(
    accessor: &RecordAccessorProcedure,
    args: &[Value],
) -> Result<Value, EvalError> {
    let [value] = args else {
        return Err(EvalError::WrongArgCount {
            name: accessor.name.clone(),
            expected: "exactly 1".into(),
            actual: args.len(),
        });
    };

    match value {
        Value::Record(record)
            if Rc::ptr_eq(&record.as_ref().record_type, &accessor.record_type) =>
        {
            Ok(record.as_ref().fields[accessor.field_index].clone())
        }
        _ => Err(EvalError::TypeMismatch {
            expected: format!("{} record", accessor.record_type.name),
            found: record_found_type(value),
        }),
    }
}

fn record_found_type(value: &Value) -> String {
    match value {
        Value::Record(record) => record.as_ref().record_type.name.clone(),
        _ => value.type_name().into(),
    }
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
