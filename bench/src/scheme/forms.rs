use std::collections::HashSet;

use crate::scheme::ast::{Expr, ExprRef, Identifier};
use crate::scheme::error::EvalError;
use crate::scheme::runtime::{BindingName, CondClause, LetKind, Parameters};

#[derive(Clone, Debug)]
pub enum DefineForm {
    Variable { target: BindingName, value: ExprRef },
    Function {
        target: BindingName,
        params: Parameters,
        body: Vec<ExprRef>,
    },
}

#[derive(Clone, Debug)]
pub struct LetForm {
    pub kind: LetKind,
    pub inits: Vec<ExprRef>,
    pub body: Vec<ExprRef>,
}

pub fn parse_define(args: &[ExprRef]) -> Result<DefineForm, EvalError> {
    if args.len() < 2 {
        return invalid_form("define", "expected a target and a value");
    }
    match args[0].as_ref() {
        Expr::Symbol(_) => parse_variable_define(args),
        Expr::List(items) => parse_function_define(items, &args[1..]),
        Expr::DottedList(items, tail) => parse_dotted_define(items, tail, &args[1..]),
        _ => invalid_form("define", "target must be a symbol or signature"),
    }
}

pub fn parse_lambda(args: &[ExprRef]) -> Result<(Parameters, Vec<ExprRef>), EvalError> {
    if args.len() < 2 {
        return invalid_form("lambda", "expected parameters and a body");
    }
    let params = parse_parameters(&args[0], "lambda")?;
    Ok((params, args[1..].to_vec()))
}

pub fn parse_let(args: &[ExprRef]) -> Result<LetForm, EvalError> {
    if args.len() < 2 {
        return invalid_form("let", "expected bindings and a body");
    }
    if matches!(args[0].as_ref(), Expr::Symbol(_)) {
        parse_named_let(args)
    } else {
        parse_plain_let(args)
    }
}

pub fn parse_cond(args: &[ExprRef]) -> Result<Vec<CondClause>, EvalError> {
    if args.is_empty() {
        return Ok(Vec::new());
    }
    let mut clauses = Vec::new();
    for (index, clause) in args.iter().enumerate() {
        clauses.push(parse_cond_clause(clause, index + 1 == args.len())?);
    }
    Ok(clauses)
}

fn parse_variable_define(args: &[ExprRef]) -> Result<DefineForm, EvalError> {
    if args.len() != 2 {
        return invalid_form("define", "variable definitions take exactly one value");
    }
    Ok(DefineForm::Variable {
        target: binding_name(&args[0], "define")?,
        value: args[1].clone(),
    })
}

fn parse_function_define(items: &[ExprRef], body: &[ExprRef]) -> Result<DefineForm, EvalError> {
    if items.is_empty() {
        return invalid_form("define", "function name is missing");
    }
    let params = parse_parameters_from_parts(&items[1..], None, "define")?;
    Ok(DefineForm::Function {
        target: binding_name(&items[0], "define")?,
        params,
        body: body.to_vec(),
    })
}

fn parse_dotted_define(
    items: &[ExprRef],
    tail: &ExprRef,
    body: &[ExprRef],
) -> Result<DefineForm, EvalError> {
    if items.is_empty() {
        return invalid_form("define", "function name is missing");
    }
    let params = parse_parameters_from_parts(&items[1..], Some(tail), "define")?;
    Ok(DefineForm::Function {
        target: binding_name(&items[0], "define")?,
        params,
        body: body.to_vec(),
    })
}

fn parse_parameters(expr: &ExprRef, context: &str) -> Result<Parameters, EvalError> {
    match expr.as_ref() {
        Expr::Symbol(_) => parse_parameters_from_parts(&[], Some(expr), context),
        Expr::List(items) => parse_parameters_from_parts(items, None, context),
        Expr::DottedList(items, tail) => parse_parameters_from_parts(items, Some(tail), context),
        _ => Err(EvalError::InvalidSyntax {
            form: context.to_owned(),
            detail: "parameter list must be a symbol or list".to_owned(),
        }),
    }
}

fn parse_parameters_from_parts(
    items: &[ExprRef],
    tail: Option<&ExprRef>,
    context: &str,
) -> Result<Parameters, EvalError> {
    let fixed = collect_binding_names(items, context)?;
    let parameters = match tail {
        Some(rest) => Parameters::Variadic {
            fixed,
            rest: binding_name(rest, context)?,
        },
        None => Parameters::Fixed(fixed),
    };
    reject_duplicate_parameters(&parameters)?;
    Ok(parameters)
}

fn parse_named_let(args: &[ExprRef]) -> Result<LetForm, EvalError> {
    if args.len() < 3 {
        return invalid_form("let", "named let requires bindings and a body");
    }
    let name = binding_name(&args[0], "let")?;
    let (params, inits) = parse_let_bindings(&args[1], "let")?;
    Ok(LetForm {
        kind: LetKind::Named { name, params },
        inits,
        body: args[2..].to_vec(),
    })
}

fn parse_plain_let(args: &[ExprRef]) -> Result<LetForm, EvalError> {
    let (params, inits) = parse_let_bindings(&args[0], "let")?;
    Ok(LetForm {
        kind: LetKind::Plain(params),
        inits,
        body: args[1..].to_vec(),
    })
}

fn parse_let_bindings(
    expr: &ExprRef,
    context: &str,
) -> Result<(Vec<BindingName>, Vec<ExprRef>), EvalError> {
    let Expr::List(bindings) = expr.as_ref() else {
        return invalid_form(context, "bindings must be a list");
    };
    let mut params = Vec::new();
    let mut inits = Vec::new();
    for binding in bindings {
        let Expr::List(parts) = binding.as_ref() else {
            return invalid_form(context, "each binding must be a two-element list");
        };
        if parts.len() != 2 {
            return invalid_form(context, "each binding must have exactly two elements");
        }
        params.push(binding_name(&parts[0], context)?);
        inits.push(parts[1].clone());
    }
    reject_duplicate_names(&params)?;
    Ok((params, inits))
}

fn parse_cond_clause(expr: &ExprRef, is_last: bool) -> Result<CondClause, EvalError> {
    let Expr::List(items) = expr.as_ref() else {
        return invalid_form("cond", "each clause must be a list");
    };
    let Some((test, body)) = items.split_first() else {
        return invalid_form("cond", "clauses may not be empty");
    };
    if test.as_ref().is_symbol_named("else") {
        if !is_last {
            return invalid_form("cond", "`else` must be last");
        }
        return Ok(CondClause {
            test: None,
            body: body.to_vec(),
        });
    }
    let body_items = if body.is_empty() {
        vec![test.clone()]
    } else {
        body.to_vec()
    };
    Ok(CondClause {
        test: Some(test.clone()),
        body: body_items,
    })
}

fn binding_name(expr: &ExprRef, context: &str) -> Result<BindingName, EvalError> {
    let Expr::Symbol(identifier) = expr.as_ref() else {
        return Err(EvalError::InvalidBindingTarget {
            context: context.to_owned(),
        });
    };
    let Some(key) = bindable_key(identifier) else {
        return Err(EvalError::InvalidBindingTarget {
            context: context.to_owned(),
        });
    };
    Ok(BindingName {
        display: identifier.name().to_owned(),
        key,
    })
}

fn collect_binding_names(
    items: &[ExprRef],
    context: &str,
) -> Result<Vec<BindingName>, EvalError> {
    items.iter().map(|expr| binding_name(expr, context)).collect()
}

fn bindable_key(identifier: &Identifier) -> Option<crate::scheme::ast::BindingKey> {
    identifier.binding_key()
}

fn reject_duplicate_parameters(parameters: &Parameters) -> Result<(), EvalError> {
    let mut names = HashSet::new();
    for binding in parameter_names(parameters) {
        if !names.insert(binding.display.clone()) {
            return Err(EvalError::DuplicateParameter {
                name: binding.display.clone(),
            });
        }
    }
    Ok(())
}

fn reject_duplicate_names(names: &[BindingName]) -> Result<(), EvalError> {
    let mut seen = HashSet::new();
    for name in names {
        if !seen.insert(name.display.clone()) {
            return Err(EvalError::DuplicateParameter {
                name: name.display.clone(),
            });
        }
    }
    Ok(())
}

fn parameter_names(parameters: &Parameters) -> Vec<&BindingName> {
    match parameters {
        Parameters::Fixed(names) => names.iter().collect(),
        Parameters::Variadic { fixed, rest } => {
            let mut result: Vec<&BindingName> = fixed.iter().collect();
            result.push(rest);
            result
        }
    }
}

fn invalid_form<T>(form: &str, detail: &str) -> Result<T, EvalError> {
    Err(EvalError::InvalidSyntax {
        form: form.to_owned(),
        detail: detail.to_owned(),
    })
}
