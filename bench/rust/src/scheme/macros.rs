use super::{
    EnvRef, Environment, EvalError, Expr, LambdaParams, MacroBinding, MacroExpansionContext,
    MacroRef, MacroRule, MacroTransformer, Position, Value,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

static MACRO_GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

pub(super) fn define_syntax(args: &[Expr], env: EnvRef) -> Result<Value, EvalError> {
    let [Expr::Symbol { name, .. }, rules_expr] = args else {
        return Err(EvalError::ParseError {
            message: "invalid define-syntax form".to_string(),
        });
    };

    let transformer = parse_syntax_rules(rules_expr, env.clone())?;
    env.define_macro(name.clone(), Rc::new(transformer));
    Ok(Value::Void)
}

pub(super) fn expand_macro_call(
    items: &[Expr],
    env: EnvRef,
    transformer: MacroRef,
) -> Result<(Expr, EnvRef), EvalError> {
    for rule in &transformer.rules {
        if let Some(bindings) = match_macro_rule(rule, items, &transformer.literals) {
            let macro_env = Environment::new(Some(env));
            let mut context = MacroExpansionContext {
                bindings,
                macro_env: macro_env.clone(),
                def_env: transformer.def_env.clone(),
                free_names: HashMap::new(),
            };
            let expanded = expand_macro_template(&rule.template, &mut context, &HashMap::new())?;
            return Ok((expanded, macro_env));
        }
    }

    Err(EvalError::ParseError {
        message: "macro invocation did not match any syntax-rules clause".to_string(),
    })
}

pub(super) fn parse_lambda_params(expr: &Expr) -> Result<LambdaParams, EvalError> {
    match expr {
        Expr::List { items, .. } => parse_lambda_param_items(items),
        Expr::Symbol { name, .. } => Ok(LambdaParams {
            fixed: Vec::new(),
            rest: Some(name.clone()),
        }),
        _ => Err(EvalError::ParseError {
            message: "lambda parameters must be a list or symbol".to_string(),
        }),
    }
}

pub(super) fn parse_lambda_param_items(items: &[Expr]) -> Result<LambdaParams, EvalError> {
    let mut fixed = Vec::new();

    for (index, expr) in items.iter().enumerate() {
        match expr {
            Expr::Symbol { name, .. } if name == "." => {
                let [rest] = &items[index + 1..] else {
                    return Err(EvalError::ParseError {
                        message: "invalid dotted parameter list".to_string(),
                    });
                };

                let Expr::Symbol { name, .. } = rest else {
                    return Err(EvalError::ParseError {
                        message: "parameter names must be symbols".to_string(),
                    });
                };

                return Ok(LambdaParams {
                    fixed,
                    rest: Some(name.clone()),
                });
            }
            Expr::Symbol { name, .. } => fixed.push(name.clone()),
            _ => {
                return Err(EvalError::ParseError {
                    message: "parameter names must be symbols".to_string(),
                });
            }
        }
    }

    Ok(LambdaParams { fixed, rest: None })
}

pub(super) fn parse_let_bindings(expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List {
        items: bindings, ..
    } = expr
    else {
        return Err(EvalError::ParseError {
            message: "let bindings must be a list".to_string(),
        });
    };

    bindings
        .iter()
        .map(|binding| match binding {
            Expr::List { items: parts, .. } => match parts.as_slice() {
                [Expr::Symbol { name, .. }, value] => Ok((name.clone(), value.clone())),
                _ => Err(EvalError::ParseError {
                    message: "let bindings must be (name value) pairs".to_string(),
                }),
            },
            _ => Err(EvalError::ParseError {
                message: "let bindings must be (name value) pairs".to_string(),
            }),
        })
        .collect()
}

fn parse_syntax_rules(expr: &Expr, env: EnvRef) -> Result<MacroTransformer, EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "define-syntax requires syntax-rules".to_string(),
        });
    };

    let Some((Expr::Symbol { name, .. }, rest)) = items.split_first() else {
        return Err(EvalError::ParseError {
            message: "define-syntax requires syntax-rules".to_string(),
        });
    };

    if name != "syntax-rules" {
        return Err(EvalError::ParseError {
            message: "define-syntax requires syntax-rules".to_string(),
        });
    }

    let [literal_expr, rules @ ..] = rest else {
        return Err(EvalError::ParseError {
            message: "syntax-rules requires literals and at least one rule".to_string(),
        });
    };

    if rules.is_empty() {
        return Err(EvalError::ParseError {
            message: "syntax-rules requires at least one rule".to_string(),
        });
    }

    let literals = parse_syntax_rule_literals(literal_expr)?;
    let mut parsed_rules = Vec::with_capacity(rules.len());
    for rule_expr in rules {
        let Expr::List { items: parts, .. } = rule_expr else {
            return Err(EvalError::ParseError {
                message: "syntax-rules clauses must be (pattern template) pairs".to_string(),
            });
        };

        let [pattern, template] = parts.as_slice() else {
            return Err(EvalError::ParseError {
                message: "syntax-rules clauses must be (pattern template) pairs".to_string(),
            });
        };

        let Expr::List {
            items: pattern_items,
            ..
        } = pattern
        else {
            return Err(EvalError::ParseError {
                message: "syntax-rules patterns must be lists".to_string(),
            });
        };

        parsed_rules.push(MacroRule {
            pattern_items: pattern_items.clone(),
            template: template.clone(),
        });
    }

    Ok(MacroTransformer {
        literals,
        rules: parsed_rules,
        def_env: env,
    })
}

fn parse_syntax_rule_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List { items, .. } = expr else {
        return Err(EvalError::ParseError {
            message: "syntax-rules literals must be a list".to_string(),
        });
    };

    let mut literals = HashSet::with_capacity(items.len());
    for item in items {
        let Expr::Symbol { name, .. } = item else {
            return Err(EvalError::ParseError {
                message: "syntax-rules literals must be symbols".to_string(),
            });
        };
        literals.insert(name.clone());
    }
    Ok(literals)
}

fn match_macro_rule(
    rule: &MacroRule,
    items: &[Expr],
    literals: &HashSet<String>,
) -> Option<HashMap<String, MacroBinding>> {
    let [_macro_name, pattern_args @ ..] = rule.pattern_items.as_slice() else {
        return None;
    };
    let [_head, input_args @ ..] = items else {
        return None;
    };

    let mut bindings = HashMap::new();
    if match_pattern_sequence(pattern_args, input_args, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_pattern_sequence(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    if patterns.is_empty() {
        return inputs.is_empty();
    }

    if patterns.len() >= 2 && is_ellipsis(&patterns[1]) {
        let min_rest = minimum_pattern_inputs(&patterns[2..]);
        if inputs.len() < min_rest {
            return false;
        }

        let max_repeat = inputs.len() - min_rest;
        for repeat_count in 0..=max_repeat {
            let mut candidate = bindings.clone();
            if !match_repeated_pattern(
                &patterns[0],
                &inputs[..repeat_count],
                literals,
                &mut candidate,
            ) {
                continue;
            }

            if match_pattern_sequence(
                &patterns[2..],
                &inputs[repeat_count..],
                literals,
                &mut candidate,
            ) {
                *bindings = candidate;
                return true;
            }
        }

        return false;
    }

    let Some((input_head, input_tail)) = inputs.split_first() else {
        return false;
    };

    if !match_single_pattern(&patterns[0], input_head, literals, bindings) {
        return false;
    }

    match_pattern_sequence(&patterns[1..], input_tail, literals, bindings)
}

fn minimum_pattern_inputs(patterns: &[Expr]) -> usize {
    let mut count = 0;
    let mut index = 0;
    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis(&patterns[index + 1]) {
            index += 2;
        } else {
            count += 1;
            index += 1;
        }
    }
    count
}

fn match_repeated_pattern(
    pattern: &Expr,
    inputs: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match pattern {
        Expr::Symbol { name, .. } if name != "..." && !literals.contains(name) => {
            match bindings.get(name) {
                Some(MacroBinding::Repeated(existing)) => existing == inputs,
                Some(MacroBinding::Single(_)) => false,
                None => {
                    bindings.insert(name.clone(), MacroBinding::Repeated(inputs.to_vec()));
                    true
                }
            }
        }
        _ => inputs
            .iter()
            .all(|input| match_single_pattern(pattern, input, literals, bindings)),
    }
}

fn match_single_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match pattern {
        Expr::Bool { value, .. } => {
            matches!(input, Expr::Bool { value: other, .. } if other == value)
        }
        Expr::Number { value, .. } => {
            matches!(input, Expr::Number { value: other, .. } if other == value)
        }
        Expr::Char { value, .. } => {
            matches!(input, Expr::Char { value: other, .. } if other == value)
        }
        Expr::String { value, .. } => {
            matches!(input, Expr::String { value: other, .. } if other == value)
        }
        Expr::Symbol { name, .. } if name == "..." => false,
        Expr::Symbol { name, .. } if literals.contains(name) => {
            matches!(input, Expr::Symbol { name: other, .. } if other == name)
        }
        Expr::Symbol { name, .. } => match bindings.get(name) {
            Some(MacroBinding::Single(existing)) => existing == input,
            Some(MacroBinding::Repeated(_)) => false,
            None => {
                bindings.insert(name.clone(), MacroBinding::Single(input.clone()));
                true
            }
        },
        Expr::List {
            items: pattern_items,
            ..
        } => match input {
            Expr::List {
                items: input_items, ..
            } => match_pattern_sequence(pattern_items, input_items, literals, bindings),
            _ => false,
        },
    }
}

fn expand_macro_template(
    expr: &Expr,
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    expand_macro_template_at(expr, context, scope, None)
}

fn expand_macro_template_at(
    expr: &Expr,
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match expr {
        Expr::Bool { .. } | Expr::Number { .. } | Expr::Char { .. } | Expr::String { .. } => {
            Ok(expr.clone())
        }
        Expr::Symbol { name, pos } => {
            expand_macro_symbol(name, *pos, context, scope, repetition_index)
        }
        Expr::List { items, pos } => {
            expand_macro_list(items, *pos, context, scope, repetition_index)
        }
    }
}

fn expand_macro_symbol(
    name: &str,
    pos: Position,
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(binding) = context.bindings.get(name) {
        return match binding {
            MacroBinding::Single(expr) => Ok(expr.clone()),
            MacroBinding::Repeated(values) => repetition_index
                .and_then(|index| values.get(index).cloned())
                .ok_or_else(|| {
                    EvalError::ParseError {
                        message: format!("missing repetition for template variable {name}"),
                    }
                    .with_position(pos.line, pos.col)
                }),
        };
    }

    if let Some(mapped) = scope.get(name) {
        return Ok(Expr::Symbol {
            name: mapped.clone(),
            pos,
        });
    }

    if is_special_form_keyword(name) || name == "..." {
        return Ok(Expr::Symbol {
            name: name.to_string(),
            pos,
        });
    }

    let mapped = ensure_hygienic_free_name(context, name);
    Ok(Expr::Symbol { name: mapped, pos })
}

fn ensure_hygienic_free_name(context: &mut MacroExpansionContext, name: &str) -> String {
    if let Some(existing) = context.free_names.get(name) {
        return existing.clone();
    }

    let fresh = fresh_macro_identifier(name);
    if let Some(binding) = context.def_env.lookup_binding(name) {
        context.macro_env.define_alias(fresh.clone(), binding);
    }
    if let Some(transformer) = context.def_env.lookup_macro(name) {
        context
            .macro_env
            .define_macro_alias(fresh.clone(), transformer);
    }
    context.free_names.insert(name.to_string(), fresh.clone());
    fresh
}

fn expand_macro_list(
    items: &[Expr],
    pos: Position,
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(Expr::Symbol { name, .. }) = items.first() {
        if !context.bindings.contains_key(name) {
            if name == "let" {
                if let Some(expanded) = expand_macro_let(items, context, scope, repetition_index)? {
                    return Ok(expanded);
                }
            }
            if name == "lambda" {
                if let Some(expanded) =
                    expand_macro_lambda(items, context, scope, repetition_index)?
                {
                    return Ok(expanded);
                }
            }
            if name == "case-lambda" {
                if let Some(expanded) =
                    expand_macro_case_lambda(items, context, scope, repetition_index)?
                {
                    return Ok(expanded);
                }
            }
        }
    }

    let mut expanded = Vec::new();
    let mut index = 0;
    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            let repeat_len = template_repeat_length(&items[index], context)?;
            for repeat_index in 0..repeat_len {
                expanded.push(expand_macro_template_at(
                    &items[index],
                    context,
                    scope,
                    Some(repeat_index),
                )?);
            }
            index += 2;
        } else {
            expanded.push(expand_macro_template_at(
                &items[index],
                context,
                scope,
                repetition_index,
            )?);
            index += 1;
        }
    }

    Ok(Expr::List {
        items: expanded,
        pos,
    })
}

fn expand_macro_let(
    items: &[Expr],
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, bindings_expr, body @ ..] = items else {
        return Ok(None);
    };

    if body.is_empty() {
        return Ok(None);
    }

    let Expr::List {
        items: bindings,
        pos,
    } = bindings_expr
    else {
        return Ok(None);
    };

    let mut expanded_bindings = Vec::with_capacity(bindings.len());
    let mut body_scope = scope.clone();

    for binding in bindings {
        let Expr::List {
            items: pair,
            pos: pair_pos,
        } = binding
        else {
            return Err(EvalError::ParseError {
                message: "macro-generated let bindings must be (name value) pairs".to_string(),
            }
            .with_position(pos.line, pos.col));
        };

        let [Expr::Symbol { name, .. }, value_expr] = pair.as_slice() else {
            return Err(EvalError::ParseError {
                message: "macro-generated let bindings must be (name value) pairs".to_string(),
            }
            .with_position(pair_pos.line, pair_pos.col));
        };

        let expanded_value =
            expand_macro_template_at(value_expr, context, scope, repetition_index)?;
        let renamed = fresh_macro_identifier(name);
        body_scope.insert(name.clone(), renamed.clone());
        expanded_bindings.push(Expr::List {
            items: vec![
                Expr::Symbol {
                    name: renamed,
                    pos: value_expr.pos(),
                },
                expanded_value,
            ],
            pos: *pair_pos,
        });
    }

    let mut expanded_items = vec![
        head.clone(),
        Expr::List {
            items: expanded_bindings,
            pos: *pos,
        },
    ];
    for expr in body {
        expanded_items.push(expand_macro_template_at(
            expr,
            context,
            &body_scope,
            repetition_index,
        )?);
    }

    Ok(Some(Expr::List {
        items: expanded_items,
        pos: head.pos(),
    }))
}

fn expand_macro_lambda(
    items: &[Expr],
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, params_expr, body @ ..] = items else {
        return Ok(None);
    };

    if body.is_empty() {
        return Ok(None);
    }

    let (expanded_params, body_scope) = rename_macro_params(params_expr, scope)?;
    let mut expanded_items = vec![head.clone(), expanded_params];
    for expr in body {
        expanded_items.push(expand_macro_template_at(
            expr,
            context,
            &body_scope,
            repetition_index,
        )?);
    }

    Ok(Some(Expr::List {
        items: expanded_items,
        pos: head.pos(),
    }))
}

fn expand_macro_case_lambda(
    items: &[Expr],
    context: &mut MacroExpansionContext,
    scope: &HashMap<String, String>,
    repetition_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, clauses @ ..] = items else {
        return Ok(None);
    };

    if clauses.is_empty() {
        return Ok(None);
    }

    let mut expanded_items = vec![head.clone()];
    for clause in clauses {
        let Expr::List {
            items: clause_items,
            pos,
        } = clause
        else {
            return Err(EvalError::ParseError {
                message: "macro-generated case-lambda clauses must be lists".to_string(),
            }
            .with_position(clause.pos().line, clause.pos().col));
        };

        let Some((params_expr, body)) = clause_items.split_first() else {
            return Err(EvalError::ParseError {
                message: "macro-generated case-lambda clauses cannot be empty".to_string(),
            }
            .with_position(pos.line, pos.col));
        };

        if body.is_empty() {
            return Err(EvalError::ParseError {
                message: "macro-generated case-lambda clauses require a body".to_string(),
            }
            .with_position(pos.line, pos.col));
        }

        let (expanded_params, body_scope) = rename_macro_params(params_expr, scope)?;
        let mut expanded_clause = vec![expanded_params];
        for expr in body {
            expanded_clause.push(expand_macro_template_at(
                expr,
                context,
                &body_scope,
                repetition_index,
            )?);
        }

        expanded_items.push(Expr::List {
            items: expanded_clause,
            pos: *pos,
        });
    }

    Ok(Some(Expr::List {
        items: expanded_items,
        pos: head.pos(),
    }))
}

fn rename_macro_params(
    params_expr: &Expr,
    scope: &HashMap<String, String>,
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    match params_expr {
        Expr::Symbol { name, pos } => {
            let renamed = fresh_macro_identifier(name);
            let mut body_scope = scope.clone();
            body_scope.insert(name.clone(), renamed.clone());
            Ok((
                Expr::Symbol {
                    name: renamed,
                    pos: *pos,
                },
                body_scope,
            ))
        }
        Expr::List { items, pos } => {
            let mut renamed_items = Vec::with_capacity(items.len());
            let mut body_scope = scope.clone();
            for item in items {
                match item {
                    Expr::Symbol { name, pos } if name == "." => {
                        renamed_items.push(Expr::Symbol {
                            name: name.clone(),
                            pos: *pos,
                        });
                    }
                    Expr::Symbol { name, pos } => {
                        let renamed = fresh_macro_identifier(name);
                        body_scope.insert(name.clone(), renamed.clone());
                        renamed_items.push(Expr::Symbol {
                            name: renamed,
                            pos: *pos,
                        });
                    }
                    _ => {
                        return Err(EvalError::ParseError {
                            message: "macro-generated lambda parameters must be symbols"
                                .to_string(),
                        }
                        .with_position(pos.line, pos.col));
                    }
                }
            }
            Ok((
                Expr::List {
                    items: renamed_items,
                    pos: *pos,
                },
                body_scope,
            ))
        }
        _ => Err(EvalError::ParseError {
            message: "macro-generated lambda parameters must be symbols".to_string(),
        }
        .with_position(params_expr.pos().line, params_expr.pos().col)),
    }
}

fn template_repeat_length(
    expr: &Expr,
    context: &MacroExpansionContext,
) -> Result<usize, EvalError> {
    let mut length = None;
    collect_template_repeat_length(expr, &context.bindings, &mut length)?;
    Ok(length.unwrap_or(0))
}

fn collect_template_repeat_length(
    expr: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    length: &mut Option<usize>,
) -> Result<(), EvalError> {
    match expr {
        Expr::Symbol { name, .. } => {
            if let Some(MacroBinding::Repeated(values)) = bindings.get(name) {
                match length {
                    Some(existing) if *existing != values.len() => {
                        return Err(EvalError::ParseError {
                            message: "mismatched template repetition lengths".to_string(),
                        });
                    }
                    Some(_) => {}
                    None => *length = Some(values.len()),
                }
            }
        }
        Expr::List { items, .. } => {
            for item in items {
                collect_template_repeat_length(item, bindings, length)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn is_special_form_keyword(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "if"
            | "quote"
            | "begin"
            | "cond"
            | "let"
            | "lambda"
            | "case-lambda"
            | "define"
            | "define-syntax"
            | "set!"
            | "else"
            | "."
    )
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol { name, .. } if name == "...")
}

fn fresh_macro_identifier(name: &str) -> String {
    let suffix = MACRO_GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__ming_macro_{suffix}_{name}")
}
