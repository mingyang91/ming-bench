use super::{
    expr_plain_symbol_name, expr_symbol_name, is_ellipsis_expr, EnvRef, EvalContext, EvalError,
    Expr, ExprKind, SourcePos,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub(super) type MacroRef = Rc<SyntaxRules>;

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

    Ok(Rc::new(SyntaxRules {
        name: name.to_string(),
        literals,
        rules,
        env,
    }))
}

pub(super) fn expand_macro_call(
    pos: SourcePos,
    items: &[Expr],
    syntax: MacroRef,
    ctx: &EvalContext,
) -> Result<Expr, EvalError> {
    let call = Expr::new(ExprKind::List(items.to_vec()), pos);

    for rule in &syntax.rules {
        let mut bindings = HashMap::new();
        if match_pattern(
            &rule.pattern,
            &call,
            &syntax.literals,
            &syntax.name,
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

enum MatchBinding {
    One(Expr),
    Many(Vec<Expr>),
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

fn match_pattern(
    pattern: &Expr,
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
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

            if name == macro_name || literals.contains(name) {
                return Ok(expr_symbol_name(value).is_some_and(|found| found == name));
            }

            bind_single(name, value, bindings)
        }
        ExprKind::List(pattern_items) => {
            match_list_pattern(pattern_items, value, literals, macro_name, bindings)
        }
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    value: &Expr,
    literals: &HashSet<String>,
    macro_name: &str,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Result<bool, EvalError> {
    let ExprKind::List(value_items) = &value.kind else {
        return Ok(false);
    };

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
    macro_name: &str,
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
    macro_name: &str,
) -> Result<bool, EvalError> {
    if let Some(name) = expr_symbol_name(pattern) {
        if name == "_" {
            return Ok(true);
        }
        if name == macro_name || literals.contains(name) {
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
        _ => false,
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
    }

    let expanded_items =
        expand_template_items(items, bindings, definition_env, renames, ctx, repeat_index)?;
    Ok(Expr::new(ExprKind::List(expanded_items), template.pos))
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

    let ExprKind::List(binding_items) = &items[1].kind else {
        return Err(EvalError::InvalidSyntax {
            message: "let template bindings must be a list".to_string(),
        });
    };

    let mut scoped_renames = renames.clone();
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

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head);
    expanded_items.push(Expr::new(ExprKind::List(expanded_bindings), items[1].pos));
    for body in &items[2..] {
        expanded_items.push(expand_template(
            body,
            bindings,
            definition_env,
            &scoped_renames,
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
        ExprKind::Number(_) | ExprKind::Boolean(_) | ExprKind::Char(_) | ExprKind::String(_) => {}
    }

    Ok(())
}
