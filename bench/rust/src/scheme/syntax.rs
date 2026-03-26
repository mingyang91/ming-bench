use super::{
    apply_procedure, eval, eval_sequence, expr_plain_symbol_name, expr_symbol_name,
    is_ellipsis_expr, list_from_values, list_to_vec, render_string, Env, EnvRef,
    EvalContext, EvalError, Expr, ExprKind, SourcePos, Value,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub(super) type MacroRef = Rc<MacroBinding>;

pub(super) enum MacroBinding {
    SyntaxRules(SyntaxRules),
    Transformer(Value),
}

pub(super) fn parse_syntax_rules(
    name: &str,
    expr: &Expr,
    env: EnvRef,
) -> Result<MacroRef, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define-syntax requires a syntax-rules transformer".to_string(),
        });
    };

    let Some(head) = items.first() else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules form cannot be empty".to_string(),
        });
    };

    if expr_symbol_name(head) != Some("syntax-rules") {
        return Err(EvalError::InvalidSyntax {
            message: "only syntax-rules transformers are supported".to_string(),
        });
    }

    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules requires literals and at least one rule".to_string(),
        });
    }

    let literals = parse_literal_identifiers(&items[1])?;
    let rules = items[2..]
        .iter()
        .map(parse_rule)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Rc::new(MacroBinding::SyntaxRules(SyntaxRules {
        name: name.to_string(),
        literals,
        rules,
        env,
    })))
}

pub(super) fn make_transformer_macro(transformer: Value) -> MacroRef {
    Rc::new(MacroBinding::Transformer(transformer))
}

pub(super) fn expand_macro_call(
    pos: SourcePos,
    items: &[Expr],
    syntax: MacroRef,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    match syntax.as_ref() {
        MacroBinding::SyntaxRules(rules) => expand_syntax_rules_call(pos, items, rules, ctx),
        MacroBinding::Transformer(transformer) => {
            expand_transformer_call(pos, items, transformer.clone(), ctx)
        }
    }
}

pub(super) fn eval_syntax_form(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            name: "syntax",
            expected: "exactly 1",
            got: args.len(),
        });
    }

    let expanded = expand_template_from_env(&args[0], &env, ctx)?;
    Ok(Value::Syntax(Box::new(expanded)))
}

pub(super) fn eval_syntax_case_form(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    if args.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-case requires an input, literals, and at least one clause"
                .to_string(),
        });
    }

    let input = eval(&args[0], env.clone(), ctx)?;
    let input_expr = syntax_value_to_expr(&input, "syntax-case")?;
    let literals = parse_literal_identifiers(&args[1])?;

    for clause in &args[2..] {
        let (pattern, fender, body) = parse_syntax_case_clause(clause)?;
        let mut bindings = HashMap::new();
        if !match_pattern(pattern, &input_expr, &literals, None, &mut bindings)? {
            continue;
        }

        let clause_env = bind_match_bindings(env.clone(), bindings);
        if let Some(fender) = fender {
            if !eval(fender, clause_env.clone(), ctx)?.is_truthy() {
                continue;
            }
        }

        return eval(body, clause_env, ctx);
    }

    Err(EvalError::InvalidSyntax {
        message: "syntax-case: no matching pattern".to_string(),
    })
}

pub(super) fn eval_with_syntax_form(
    args: &[Expr],
    env: EnvRef,
    ctx: &EvalContext,
) -> Result<Value, EvalError> {
    let Some((bindings_expr, body)) = args.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "with-syntax requires bindings and a body".to_string(),
        });
    };
    if body.is_empty() {
        return Err(EvalError::InvalidSyntax {
            message: "with-syntax requires a body".to_string(),
        });
    }

    let ExprKind::List(specs) = &bindings_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "with-syntax bindings must be a list".to_string(),
        });
    };

    let mut merged = HashMap::new();
    for spec in specs {
        let ExprKind::List(parts) = &spec.kind else {
            return Err(EvalError::InvalidSyntax {
                message: "with-syntax bindings must be lists".to_string(),
            });
        };
        if parts.len() != 2 {
            return Err(EvalError::InvalidSyntax {
                message: "with-syntax bindings must contain a pattern and expression".to_string(),
            });
        }

        if let Some(name) = expr_plain_symbol_name(&parts[0]) {
            let binding = value_to_match_binding(&eval(&parts[1], env.clone(), ctx)?, "with-syntax")?;
            merge_binding(name, binding, &mut merged)?;
            continue;
        }

        let value = eval(&parts[1], env.clone(), ctx)?;
        let expr = syntax_value_to_expr(&value, "with-syntax")?;
        let mut bindings = HashMap::new();
        if !match_pattern(&parts[0], &expr, &HashSet::new(), None, &mut bindings)? {
            return Err(EvalError::InvalidSyntax {
                message: "with-syntax pattern did not match the produced syntax".to_string(),
            });
        }

        for (name, binding) in bindings {
            merge_binding(&name, binding, &mut merged)?;
        }
    }

    let frame = bind_match_bindings(env, merged);
    eval_sequence(body, frame, ctx)
}

pub(super) fn syntax_to_datum_value(
    value: &Value,
    name: &'static str,
) -> Result<Value, EvalError> {
    match value {
        Value::Syntax(expr) => expr_to_datum(expr),
        Value::SyntaxList(exprs) => exprs
            .iter()
            .map(expr_to_datum)
            .collect::<Result<Vec<_>, _>>()
            .map(list_from_values),
        other => Err(EvalError::ExpectedSyntax {
            name,
            found: other.render(),
        }),
    }
}

pub(super) fn datum_to_syntax_value(
    context: &Value,
    datum: &Value,
    name: &'static str,
) -> Result<Value, EvalError> {
    let context_expr = match context {
        Value::Syntax(expr) => Some(expr.as_ref()),
        other => {
            return Err(EvalError::ExpectedSyntax {
                name,
                found: other.render(),
            });
        }
    };

    Ok(Value::Syntax(Box::new(datum_to_expr(
        datum,
        context_expr,
        name,
    )?)))
}

pub(super) struct SyntaxRules {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    env: EnvRef,
}

struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
enum MatchBinding {
    One(Expr),
    Many(Vec<Expr>),
}

fn expand_syntax_rules_call(
    pos: SourcePos,
    items: &[Expr],
    syntax: &SyntaxRules,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    let call = Expr::new(ExprKind::List(items.to_vec()), pos);

    for rule in &syntax.rules {
        let mut bindings = HashMap::new();
        if match_pattern(
            &rule.pattern,
            &call,
            &syntax.literals,
            Some(&syntax.name),
            &mut bindings,
        )? {
            let renames = HashMap::new();
            return expand_template(&rule.template, &bindings, &syntax.env, &renames, ctx, None);
        }
    }

    Err(EvalError::InvalidSyntax {
        message: format!("{}: no matching syntax-rules pattern", syntax.name),
    })
}

fn expand_transformer_call(
    pos: SourcePos,
    items: &[Expr],
    transformer: Value,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    let syntax = Value::Syntax(Box::new(Expr::new(ExprKind::List(items.to_vec()), pos)));
    let value = apply_procedure(transformer, &[syntax], ctx)?;
    syntax_value_to_expr(&value, "macro transformer")
}

fn parse_literal_identifiers(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let ExprKind::List(items) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules literals must be a list".to_string(),
        });
    };

    let mut literals = HashSet::with_capacity(items.len());
    for item in items {
        let Some(name) = expr_plain_symbol_name(item) else {
            return Err(EvalError::InvalidSyntax {
                message: "syntax-rules literals must be identifiers".to_string(),
            });
        };
        literals.insert(name.to_string());
    }
    Ok(literals)
}

fn parse_rule(expr: &Expr) -> Result<SyntaxRule, EvalError> {
    let ExprKind::List(rule_parts) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules clauses must be lists".to_string(),
        });
    };

    if rule_parts.len() != 2 {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-rules clauses must contain a pattern and template".to_string(),
        });
    }

    Ok(SyntaxRule {
        pattern: rule_parts[0].clone(),
        template: rule_parts[1].clone(),
    })
}

fn parse_syntax_case_clause(expr: &Expr) -> Result<(&Expr, Option<&Expr>, &Expr), EvalError> {
    let ExprKind::List(parts) = &expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "syntax-case clauses must be lists".to_string(),
        });
    };

    match parts.as_slice() {
        [pattern, body] => Ok((pattern, None, body)),
        [pattern, fender, body] => Ok((pattern, Some(fender), body)),
        _ => Err(EvalError::InvalidSyntax {
            message: "syntax-case clauses must contain a pattern, optional fender, and body"
                .to_string(),
        }),
    }
}

fn match_pattern(
    pattern: &Expr,
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match &pattern.kind {
        ExprKind::Number(expected) => {
            Ok(matches!(&value.kind, ExprKind::Number(found) if found == expected))
        }
        ExprKind::Boolean(expected) => {
            Ok(matches!(&value.kind, ExprKind::Boolean(found) if found == expected))
        }
        ExprKind::Char(expected) => {
            Ok(matches!(&value.kind, ExprKind::Char(found) if found == expected))
        }
        ExprKind::String(expected) => {
            Ok(matches!(&value.kind, ExprKind::String(found) if found == expected))
        }
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if name == "_" {
                return Ok(true);
            }

            if macro_name.is_some_and(|macro_name| name == macro_name) || literals.contains(name) {
                return Ok(expr_symbol_name(value).is_some_and(|found| found == name));
            }

            bind_single(name, value, bindings)
        }
        ExprKind::List(pattern_items) => {
            match_list_pattern(pattern_items, value, literals, macro_name, bindings)
        }
        ExprKind::Vector(pattern_items) => {
            match_vector_pattern(pattern_items, value, literals, macro_name, bindings)
        }
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    let ExprKind::List(value_items) = &value.kind else {
        return Ok(false);
    };

    match_sequence_pattern(pattern_items, value_items, literals, macro_name, bindings)
}

fn match_vector_pattern(
    pattern_items: &[Expr],
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    let ExprKind::Vector(value_items) = &value.kind else {
        return Ok(false);
    };

    match_sequence_pattern(pattern_items, value_items, literals, macro_name, bindings)
}

fn match_sequence_pattern(
    pattern_items: &[Expr],
    value_items: &[Expr],
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {

    if let Some((prefix, repeated)) = pattern_items.split_last_chunk::<2>() {
        if is_ellipsis_expr(&repeated[1]) {
            return match_ellipsis_pattern(
                &repeated[0],
                prefix,
                value_items,
                literals,
                macro_name,
                bindings,
            );
        }
    }

    if pattern_items.len() != value_items.len() {
        return Ok(false);
    }

    for (pattern_item, value_item) in pattern_items.iter().zip(value_items) {
        if !match_pattern(pattern_item, value_item, literals, macro_name, bindings)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn match_ellipsis_pattern(
    repeated_pattern: &Expr,
    prefix: &[Expr],
    value_items: &[Expr],
    literals: &HashSet<String>,
    macro_name: Option<&str>,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    if value_items.len() < prefix.len() {
        return Ok(false);
    }

    for (pattern_item, value_item) in prefix.iter().zip(value_items) {
        if !match_pattern(pattern_item, value_item, literals, macro_name, bindings)? {
            return Ok(false);
        }
    }

    match_repeated_pattern(
        repeated_pattern,
        &value_items[prefix.len()..],
        bindings,
        literals,
        macro_name,
    )
}

fn match_repeated_pattern(
    pattern: &Expr,
    values: &[Expr],
    bindings: &mut HashMap<String, MatchBinding>,
    literals: &HashSet<String>,
    macro_name: Option<&str>,
) -> Result<bool, EvalError> {
    if let Some(name) = expr_symbol_name(pattern) {
        if name == "_" {
            return Ok(true);
        }
        if macro_name.is_some_and(|macro_name| name == macro_name) || literals.contains(name) {
            return Ok(values
                .iter()
                .all(|value| expr_symbol_name(value).is_some_and(|found| found == name)));
        }
        return bind_many(name, values, bindings);
    }

    for value in values {
        if !match_pattern(pattern, value, literals, macro_name, bindings)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn bind_single(
    name: &str,
    value: &Expr,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match bindings.get(name) {
        None => {
            bindings.insert(name.to_string(), MatchBinding::One(value.clone()));
            Ok(true)
        }
        Some(MatchBinding::One(existing)) => Ok(expr_syntax_eq(existing, value)),
        Some(MatchBinding::Many(_)) => Err(EvalError::InvalidSyntax {
            message: format!("pattern variable {name} used as both repeated and non-repeated"),
        }),
    }
}

fn bind_many(
    name: &str,
    values: &[Expr],
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    match bindings.get(name) {
        None => {
            bindings.insert(name.to_string(), MatchBinding::Many(values.to_vec()));
            Ok(true)
        }
        Some(MatchBinding::Many(existing)) => {
            if existing.len() != values.len() {
                return Ok(false);
            }
            Ok(existing
                .iter()
                .zip(values)
                .all(|(left, right)| expr_syntax_eq(left, right)))
        }
        Some(MatchBinding::One(_)) => Err(EvalError::InvalidSyntax {
            message: format!("pattern variable {name} used as both repeated and non-repeated"),
        }),
    }
}

fn expr_syntax_eq(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => left == right,
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::CapturedSymbol(left, left_env), ExprKind::CapturedSymbol(right, right_env)) => {
            left == right && Rc::ptr_eq(left_env, right_env)
        }
        (ExprKind::Symbol(left), ExprKind::CapturedSymbol(right, _))
        | (ExprKind::CapturedSymbol(left, _), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| expr_syntax_eq(left, right))
        }
        (ExprKind::Vector(left), ExprKind::Vector(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| expr_syntax_eq(left, right))
        }
        _ => false,
    }
}

fn expand_template_from_env(
    template: &Expr,
    env: &EnvRef,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    let mut bindings = HashMap::new();
    collect_template_bindings(template, env, &mut bindings)?;
    let renames = HashMap::new();
    expand_template(template, &bindings, env, &renames, ctx, None)
}

fn collect_template_bindings(
    template: &Expr,
    env: &EnvRef,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if name != "..." && !bindings.contains_key(name) {
                if let Some(binding) = lookup_template_binding(name, env)? {
                    bindings.insert(name.clone(), binding);
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_template_bindings(item, env, bindings)?;
            }
        }
        ExprKind::Vector(items) => {
            for item in items {
                collect_template_bindings(item, env, bindings)?;
            }
        }
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {}
    }

    Ok(())
}

fn lookup_template_binding(
    name: &str,
    env: &EnvRef,
) -> Result<Option<MatchBinding>, EvalError> {
    let Some(value) = env.lookup(name) else {
        return Ok(None);
    };
    match value {
        Value::Syntax(expr) => Ok(Some(MatchBinding::One((*expr).clone()))),
        Value::SyntaxList(exprs) => Ok(Some(MatchBinding::Many(exprs))),
        _ => Ok(None),
    }
}

fn value_to_match_binding(
    value: &Value,
    name: &'static str,
) -> Result<MatchBinding, EvalError> {
    match value {
        Value::Syntax(expr) => Ok(MatchBinding::One((**expr).clone())),
        Value::SyntaxList(exprs) => Ok(MatchBinding::Many(exprs.clone())),
        other => Err(EvalError::ExpectedSyntax {
            name,
            found: other.render(),
        }),
    }
}

fn bind_match_bindings(parent: EnvRef, bindings: HashMap<String, MatchBinding>) -> EnvRef {
    let frame = Env::new(Some(parent));
    for (name, binding) in bindings {
        match binding {
            MatchBinding::One(expr) => frame.define(name, Value::Syntax(Box::new(expr))),
            MatchBinding::Many(exprs) => frame.define(name, Value::SyntaxList(exprs)),
        }
    }
    frame
}

fn merge_binding(
    name: &str,
    binding: MatchBinding,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<(), EvalError> {
    match bindings.get(name) {
        None => {
            bindings.insert(name.to_string(), binding);
            Ok(())
        }
        Some(MatchBinding::One(existing)) => match binding {
            MatchBinding::One(found) if expr_syntax_eq(existing, &found) => Ok(()),
            MatchBinding::One(_) => Err(EvalError::InvalidSyntax {
                message: format!("duplicate with-syntax binding for {name}"),
            }),
            MatchBinding::Many(_) => Err(EvalError::InvalidSyntax {
                message: format!("binding {name} used as both repeated and non-repeated"),
            }),
        },
        Some(MatchBinding::Many(existing)) => match binding {
            MatchBinding::Many(found)
                if existing.len() == found.len()
                    && existing
                        .iter()
                        .zip(found.iter())
                        .all(|(left, right)| expr_syntax_eq(left, right)) =>
            {
                Ok(())
            }
            MatchBinding::Many(_) => Err(EvalError::InvalidSyntax {
                message: format!("duplicate with-syntax binding for {name}"),
            }),
            MatchBinding::One(_) => Err(EvalError::InvalidSyntax {
                message: format!("binding {name} used as both repeated and non-repeated"),
            }),
        },
    }
}

fn syntax_value_to_expr(value: &Value, name: &'static str) -> Result<Expr, EvalError> {
    match value {
        Value::Syntax(expr) => Ok((**expr).clone()),
        other => Err(EvalError::ExpectedSyntax {
            name,
            found: other.render(),
        }),
    }
}

fn expr_to_datum(expr: &Expr) -> Result<Value, EvalError> {
    super::quote_expr_value(expr)
}

fn datum_to_expr(
    datum: &Value,
    context: Option<&Expr>,
    name: &'static str,
) -> Result<Expr, EvalError> {
    let pos = context.map_or(SourcePos::new(0, 0), |expr| expr.pos);

    match datum {
        Value::Number(value) => Ok(Expr::new(ExprKind::Number(*value), pos)),
        Value::Boolean(value) => Ok(Expr::new(ExprKind::Boolean(*value), pos)),
        Value::Char(value) => Ok(Expr::new(ExprKind::Char(*value), pos)),
        Value::String(value) => Ok(Expr::new(ExprKind::String(render_string(value)), pos)),
        Value::Symbol(value) => {
            if let Some(env) = capture_env_from_context(context) {
                Ok(Expr::new(ExprKind::CapturedSymbol(value.clone(), env), pos))
            } else {
                Ok(Expr::new(ExprKind::Symbol(value.clone()), pos))
            }
        }
        Value::Nil => Ok(Expr::new(ExprKind::List(Vec::new()), pos)),
        Value::Pair(_) => list_to_vec(datum, name)?
            .iter()
            .map(|value| datum_to_expr(value, context, name))
            .collect::<Result<Vec<_>, _>>()
            .map(|items| Expr::new(ExprKind::List(items), pos)),
        Value::Vector(vector) => vector
            .borrow()
            .iter()
            .map(|value| datum_to_expr(value, context, name))
            .collect::<Result<Vec<_>, _>>()
            .map(|items| Expr::new(ExprKind::Vector(items), pos)),
        Value::Syntax(expr) => Ok((**expr).clone()),
        other => Err(EvalError::InvalidSyntax {
            message: format!("{name}: invalid datum {}", other.render()),
        }),
    }
}

fn capture_env_from_context(context: Option<&Expr>) -> Option<EnvRef> {
    match context?.kind.clone() {
        ExprKind::CapturedSymbol(_, env) => Some(env),
        _ => None,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Number(value) => Ok(Expr::new(ExprKind::Number(*value), template.pos)),
        ExprKind::Boolean(value) => Ok(Expr::new(ExprKind::Boolean(*value), template.pos)),
        ExprKind::Char(value) => Ok(Expr::new(ExprKind::Char(*value), template.pos)),
        ExprKind::String(value) => Ok(Expr::new(ExprKind::String(value.clone()), template.pos)),
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => expand_template_symbol(
            name,
            template,
            bindings,
            definition_env,
            renames,
            repeat_index,
        ),
        ExprKind::List(items) => expand_template_list(
            template,
            items,
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        ),
        ExprKind::Vector(items) => expand_template_vector(
            template,
            items,
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        ),
    }
}

fn expand_template_symbol(
    name: &str,
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(binding) = bindings.get(name) {
        return match binding {
            MatchBinding::One(expr) => Ok(expr.clone()),
            MatchBinding::Many(exprs) => expand_repeated_binding(name, exprs, repeat_index),
        };
    }

    if let Some(rename) = renames.get(name) {
        return Ok(Expr::new(ExprKind::Symbol(rename.clone()), template.pos));
    }

    Ok(Expr::new(
        ExprKind::CapturedSymbol(name.to_string(), definition_env.clone()),
        template.pos,
    ))
}

fn expand_repeated_binding(
    name: &str,
    exprs: &[Expr],
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some(index) = repeat_index else {
        return Err(EvalError::InvalidSyntax {
            message: format!("template uses repeated pattern variable {name} outside ellipsis"),
        });
    };

    exprs
        .get(index)
        .cloned()
        .ok_or_else(|| EvalError::InvalidSyntax {
            message: format!("template repetition index {index} out of bounds for {name}"),
        })
}

fn expand_template_list(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(name) = items.first().and_then(expr_plain_symbol_name) {
        if !bindings.contains_key(name) && name == "let" {
            return expand_let_template(
                template,
                items,
                bindings,
                definition_env,
                renames,
                ctx,
                repeat_index,
            );
        }
        if !bindings.contains_key(name) && name == "lambda" {
            return expand_lambda_template(
                template,
                items,
                bindings,
                definition_env,
                renames,
                ctx,
                repeat_index,
            );
        }
        if !bindings.contains_key(name) && name == "guard" {
            return expand_guard_template(
                template,
                items,
                bindings,
                definition_env,
                renames,
                ctx,
                repeat_index,
            );
        }
        if !bindings.contains_key(name)
            && name == "define"
            && items
                .get(1)
                .is_some_and(|expr| matches!(&expr.kind, ExprKind::List(_)))
        {
            let mut scoped_renames = renames.clone();
            return expand_define_template(
                template,
                items,
                bindings,
                definition_env,
                &mut scoped_renames,
                ctx,
                repeat_index,
            );
        }
    }

    let expanded_items =
        expand_template_items(items, bindings, definition_env, renames, ctx, repeat_index)?;
    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn expand_template_vector(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let expanded_items =
        expand_template_items(items, bindings, definition_env, renames, ctx, repeat_index)?;
    Ok(Expr::new(ExprKind::Vector(expanded_items), template.pos))
}

fn expand_body_expr(
    body: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let ExprKind::List(items) = &body.kind else {
        return expand_template(
            body,
            bindings,
            definition_env,
            scoped_renames,
            ctx,
            repeat_index,
        );
    };

    if items.first().and_then(expr_plain_symbol_name) == Some("define")
        && items
            .get(1)
            .is_some_and(|expr| matches!(&expr.kind, ExprKind::List(_)))
    {
        return expand_define_template(
            body,
            items,
            bindings,
            definition_env,
            scoped_renames,
            ctx,
            repeat_index,
        );
    }

    expand_template(
        body,
        bindings,
        definition_env,
        scoped_renames,
        ctx,
        repeat_index,
    )
}

fn expand_template_items(
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis_expr(&items[index + 1]) {
            let repeat_count = template_repeat_count(&items[index], bindings)?;
            for current in 0..repeat_count {
                expanded.push(expand_template(
                    &items[index],
                    bindings,
                    definition_env,
                    renames,
                    ctx,
                    Some(current),
                )?);
            }
            index += 2;
            continue;
        }

        expanded.push(expand_template(
            &items[index],
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(expanded)
}

fn expand_let_template(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "let template requires bindings and a body".to_string(),
        });
    }

    let head = expand_template(
        &items[0],
        bindings,
        definition_env,
        renames,
        ctx,
        repeat_index,
    )?;

    let mut scoped_renames = renames.clone();
    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);

    let (bindings_expr, body) = if items
        .get(1)
        .and_then(expr_symbol_name)
        .is_some_and(|name| !bindings.contains_key(name))
    {
        if items.len() < 4 {
            return Err(EvalError::InvalidSyntax {
                message: "named let template requires bindings and a body".to_string(),
            });
        }

        let loop_name = expand_param_name(
            &items[1],
            bindings,
            definition_env,
            renames,
            &mut scoped_renames,
            ctx,
            repeat_index,
        )?;
        expanded_items.push(loop_name);
        (&items[2], &items[3..])
    } else {
        (&items[1], &items[2..])
    };

    let ExprKind::List(binding_items) = &bindings_expr.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let template bindings must be a list".to_string(),
        });
    };

    let expanded_bindings = binding_items
        .iter()
        .map(|binding| {
            expand_let_binding(
                binding,
                bindings,
                definition_env,
                renames,
                &mut scoped_renames,
                ctx,
                repeat_index,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    expanded_items.push(Expr::new(
        ExprKind::List(expanded_bindings),
        bindings_expr.pos,
    ));
    for body in body {
        expanded_items.push(expand_body_expr(
            body,
            bindings,
            definition_env,
            &mut scoped_renames,
            ctx,
            repeat_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn expand_lambda_template(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "lambda template requires parameters and a body".to_string(),
        });
    }

    let head = expand_template(
        &items[0],
        bindings,
        definition_env,
        renames,
        ctx,
        repeat_index,
    )?;

    let mut scoped_renames = renames.clone();
    let params = expand_param_list_template(
        &items[1],
        bindings,
        definition_env,
        renames,
        &mut scoped_renames,
        ctx,
        repeat_index,
    )?;

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);
    expanded_items.push(params);
    for body in &items[2..] {
        expanded_items.push(expand_body_expr(
            body,
            bindings,
            definition_env,
            &mut scoped_renames,
            ctx,
            repeat_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn expand_guard_template(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "guard template requires a spec and a body".to_string(),
        });
    }

    let head = expand_template(
        &items[0],
        bindings,
        definition_env,
        renames,
        ctx,
        repeat_index,
    )?;

    let ExprKind::List(spec_items) = &items[1].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "guard template spec must be a list".to_string(),
        });
    };
    let Some((name_expr, clauses)) = spec_items.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "guard template spec requires a variable".to_string(),
        });
    };

    let mut scoped_renames = renames.clone();
    let expanded_name = expand_param_name(
        name_expr,
        bindings,
        definition_env,
        renames,
        &mut scoped_renames,
        ctx,
        repeat_index,
    )?;
    let expanded_clauses = clauses
        .iter()
        .map(|clause| {
            expand_template(
                clause,
                bindings,
                definition_env,
                &scoped_renames,
                ctx,
                repeat_index,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut spec = Vec::with_capacity(spec_items.len());
    spec.push(expanded_name);
    spec.extend(expanded_clauses);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);
    expanded_items.push(Expr::new(ExprKind::List(spec), items[1].pos));
    for body in &items[2..] {
        expanded_items.push(expand_body_expr(
            body,
            bindings,
            definition_env,
            &mut scoped_renames,
            ctx,
            repeat_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn expand_define_template(
    template: &Expr,
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return Err(EvalError::InvalidSyntax {
            message: "define template requires a function signature and body".to_string(),
        });
    }

    let ExprKind::List(signature) = &items[1].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "define template function signatures must be lists".to_string(),
        });
    };
    let Some((name_expr, params)) = signature.split_first() else {
        return Err(EvalError::InvalidSyntax {
            message: "define template requires a function name".to_string(),
        });
    };

    let head = expand_template(
        &items[0],
        bindings,
        definition_env,
        scoped_renames,
        ctx,
        repeat_index,
    )?;

    let rename_snapshot = scoped_renames.clone();
    let expanded_name = expand_param_name(
        name_expr,
        bindings,
        definition_env,
        &rename_snapshot,
        scoped_renames,
        ctx,
        repeat_index,
    )?;
    let mut definition_renames = scoped_renames.clone();
    let mut expanded_signature = Vec::with_capacity(signature.len());
    expanded_signature.push(expanded_name);
    expanded_signature.extend(expand_param_items(
        params,
        bindings,
        definition_env,
        scoped_renames,
        &mut definition_renames,
        ctx,
        repeat_index,
    )?);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);
    expanded_items.push(Expr::new(ExprKind::List(expanded_signature), items[1].pos));
    for body in &items[2..] {
        expanded_items.push(expand_body_expr(
            body,
            bindings,
            definition_env,
            &mut definition_renames,
            ctx,
            repeat_index,
        )?);
    }

    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
}

fn expand_let_binding(
    binding: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let ExprKind::List(parts) = &binding.kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let template bindings must be pairs".to_string(),
        });
    };

    if parts.len() != 2 {
        return Err(EvalError::InvalidSyntax {
            message: "let template bindings must contain exactly 2 items".to_string(),
        });
    }

    let binding_name = expand_let_binding_name(
        &parts[0],
        bindings,
        definition_env,
        renames,
        scoped_renames,
        ctx,
        repeat_index,
    )?;
    let binding_value = expand_template(
        &parts[1],
        bindings,
        definition_env,
        renames,
        ctx,
        repeat_index,
    )?;

    Ok(Expr::new(
        ExprKind::List(vec![binding_name, binding_value]),
        binding.pos,
    ))
}

fn expand_let_binding_name(
    binding_name: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some(name) = expr_plain_symbol_name(binding_name) else {
        return Err(EvalError::InvalidSyntax {
            message: "let template binding names must be identifiers".to_string(),
        });
    };

    if bindings.contains_key(name) {
        return expand_template(
            binding_name,
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        );
    }

    let fresh = ctx.fresh_name(name);
    scoped_renames.insert(name.to_string(), fresh.clone());
    Ok(Expr::new(ExprKind::Symbol(fresh), binding_name.pos))
}

fn expand_param_list_template(
    params: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match &params.kind {
        ExprKind::List(items) => Ok(Expr::new(
            ExprKind::List(expand_param_items(
                items,
                bindings,
                definition_env,
                renames,
                scoped_renames,
                ctx,
                repeat_index,
            )?),
            params.pos,
        )),
        ExprKind::Symbol(_) | ExprKind::CapturedSymbol(_, _) => expand_param_name(
            params,
            bindings,
            definition_env,
            renames,
            scoped_renames,
            ctx,
            repeat_index,
        ),
        ExprKind::Number(_)
        | ExprKind::Boolean(_)
        | ExprKind::Char(_)
        | ExprKind::String(_) => Err(EvalError::InvalidSyntax {
            message: "lambda template parameters must be identifiers or lists".to_string(),
        }),
        ExprKind::Vector(_) => Err(EvalError::InvalidSyntax {
            message: "lambda template parameters must be identifiers or lists".to_string(),
        }),
    }
}

fn expand_param_items(
    items: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::with_capacity(items.len());
    let mut index = 0;

    while let Some(param) = items.get(index) {
        if expr_symbol_name(param).is_some_and(|name| name == ".") {
            let Some(rest) = items.get(index + 1) else {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter dot must be followed by a name".to_string(),
                });
            };
            if index + 2 != items.len() {
                return Err(EvalError::InvalidSyntax {
                    message: "rest parameter must be the final parameter".to_string(),
                });
            }

            expanded.push(Expr::new(ExprKind::Symbol(".".to_string()), param.pos));
            expanded.push(expand_param_name(
                rest,
                bindings,
                definition_env,
                renames,
                scoped_renames,
                ctx,
                repeat_index,
            )?);
            return Ok(expanded);
        }

        expanded.push(expand_param_name(
            param,
            bindings,
            definition_env,
            renames,
            scoped_renames,
            ctx,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(expanded)
}

fn expand_param_name(
    param: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    definition_env: &EnvRef,
    renames: &HashMap<String, String>,
    scoped_renames: &mut HashMap<String, String>,
    ctx: &EvalContext,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some(name) = expr_symbol_name(param) else {
        return Err(EvalError::InvalidSyntax {
            message: "parameter names must be identifiers".to_string(),
        });
    };

    if bindings.contains_key(name) {
        return expand_template(
            param,
            bindings,
            definition_env,
            renames,
            ctx,
            repeat_index,
        );
    }

    let fresh = ctx.fresh_name(name);
    scoped_renames.insert(name.to_string(), fresh.clone());
    Ok(Expr::new(ExprKind::Symbol(fresh), param.pos))
}

fn template_repeat_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
) -> Result<usize, EvalError> {
    let mut count = None;
    collect_template_repeat_count(template, bindings, &mut count)?;
    count.ok_or_else(|| EvalError::InvalidSyntax {
        message: "ellipsis template must reference a repeated pattern variable".to_string(),
    })
}

fn collect_template_repeat_count(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) | ExprKind::CapturedSymbol(name, _) => {
            if let Some(MatchBinding::Many(values)) = bindings.get(name) {
                match count {
                    None => *count = Some(values.len()),
                    Some(existing) if *existing == values.len() => {}
                    Some(_) => {
                        return Err(EvalError::InvalidSyntax {
                            message: "repeated template variables must have the same length"
                                .to_string(),
                        });
                    }
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if is_ellipsis_expr(item) {
                    continue;
                }
                collect_template_repeat_count(item, bindings, count)?;
            }
        }
        ExprKind::Vector(items) => {
            for item in items {
                if is_ellipsis_expr(item) {
                    continue;
                }
                collect_template_repeat_count(item, bindings, count)?;
            }
        }
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {}
    }

    Ok(())
}
