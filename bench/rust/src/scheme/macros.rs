use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::error::{EvalError, SourcePos};
use super::model::{
    expr_datum_eq, fresh_identifier, is_core_syntax, is_ellipsis, Env, EnvRef, ExpansionState,
    Expr, MacroExpansion, MacroRef, MacroTransformer, PatternBindings, SyntaxRule,
};

pub(super) fn parse_syntax_rules(expr: &Expr, env: &EnvRef) -> Result<MacroRef, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules form".into(),
        });
    };

    let [Expr::Symbol(keyword, _), Expr::List(literal_exprs, _), rules @ ..] = items.as_slice()
    else {
        return Err(EvalError::Syntax {
            message: "define-syntax: invalid syntax-rules form".into(),
        });
    };

    if keyword != "syntax-rules" {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected syntax-rules".into(),
        });
    }

    if rules.is_empty() {
        return Err(EvalError::Syntax {
            message: "define-syntax: expected at least one syntax-rules clause".into(),
        });
    }

    let mut literals = HashSet::new();
    for literal in literal_exprs {
        match literal {
            Expr::Symbol(name, _) if name != "..." => {
                literals.insert(name.clone());
            }
            _ => {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: expected literal identifier".into(),
                });
            }
        }
    }

    let mut parsed_rules = Vec::with_capacity(rules.len());
    for rule in rules {
        let Expr::List(parts, _) = rule else {
            return Err(EvalError::Syntax {
                message: "syntax-rules: expected rule".into(),
            });
        };

        match parts.as_slice() {
            [pattern, template] => parsed_rules.push(SyntaxRule {
                pattern: pattern.clone(),
                template: template.clone(),
            }),
            _ => {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: expected (pattern template)".into(),
                });
            }
        }
    }

    Ok(Rc::new(MacroTransformer {
        literals,
        rules: parsed_rules,
        env: env.clone(),
    }))
}

pub(super) fn env_with_expansion_aliases(env: &EnvRef, expansion: &MacroExpansion) -> EnvRef {
    if expansion.value_aliases.is_empty() && expansion.macro_aliases.is_empty() {
        return env.clone();
    }

    let expanded_env = Env::new(Some(env.clone()));
    for (name, cell) in &expansion.value_aliases {
        expanded_env.define_alias(name.clone(), cell.clone());
    }
    for (name, transformer) in &expansion.macro_aliases {
        expanded_env.define_macro(name.clone(), transformer.clone());
    }
    expanded_env
}

pub(super) fn expand_macro_call(
    items: &[Expr],
    transformer: &MacroRef,
) -> Result<MacroExpansion, EvalError> {
    let call_expr = Expr::List(items.to_vec(), items[0].pos());

    for rule in &transformer.rules {
        let mut bindings = PatternBindings::default();
        if match_macro_rule(rule, &call_expr, &transformer.literals, &mut bindings)? {
            let mut state = ExpansionState::new(bindings, &transformer.env);
            let expr = expand_template_expr(&rule.template, &mut state, &HashMap::new(), None)?;
            return Ok(MacroExpansion {
                expr,
                value_aliases: state.value_aliases,
                macro_aliases: state.macro_aliases,
            });
        }
    }

    Err(EvalError::Syntax {
        message: "syntax-rules: no matching clause".into(),
    })
}

fn match_macro_rule(
    rule: &SyntaxRule,
    call_expr: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) -> Result<bool, EvalError> {
    let Expr::List(pattern_items, _) = &rule.pattern else {
        return Err(EvalError::Syntax {
            message: "syntax-rules: expected list pattern".into(),
        });
    };
    let Expr::List(call_items, _) = call_expr else {
        return Ok(false);
    };

    if pattern_items.is_empty() {
        return Err(EvalError::Syntax {
            message: "syntax-rules: expected macro name in pattern".into(),
        });
    }

    if call_items.is_empty() {
        return Ok(false);
    }

    match_list_pattern(
        &pattern_items[1..],
        &call_items[1..],
        literals,
        bindings,
        false,
    )
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> Result<bool, EvalError> {
    match pattern {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(expr_datum_eq(pattern, input))
        }
        Expr::Symbol(name, _) => {
            if name == "..." {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: invalid ellipsis pattern".into(),
                });
            }

            if literals.contains(name) {
                return Ok(matches!(input, Expr::Symbol(other, _) if other == name));
            }

            Ok(if repeated {
                bindings.bind_repeated(name, input)
            } else {
                bindings.bind_single(name, input)
            })
        }
        Expr::List(pattern_items, _) => match input {
            Expr::List(input_items, _) => {
                match_list_pattern(pattern_items, input_items, literals, bindings, repeated)
            }
            _ => Ok(false),
        },
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    input_items: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
    repeated: bool,
) -> Result<bool, EvalError> {
    let Some(ellipsis_index) = find_ellipsis_index(pattern_items)? else {
        if pattern_items.len() != input_items.len() {
            return Ok(false);
        }

        for (pattern, input) in pattern_items.iter().zip(input_items.iter()) {
            if !match_pattern(pattern, input, literals, bindings, repeated)? {
                return Ok(false);
            }
        }

        return Ok(true);
    };

    let repeated_pattern = &pattern_items[ellipsis_index - 1];
    let prefix = &pattern_items[..ellipsis_index - 1];
    let suffix = &pattern_items[ellipsis_index + 1..];

    if input_items.len() < prefix.len() + suffix.len() {
        return Ok(false);
    }

    for (pattern, input) in prefix.iter().zip(input_items.iter()) {
        if !match_pattern(pattern, input, literals, bindings, repeated)? {
            return Ok(false);
        }
    }

    let repeat_count = input_items.len() - prefix.len() - suffix.len();
    seed_repeated_bindings(repeated_pattern, literals, bindings)?;
    for input in &input_items[prefix.len()..prefix.len() + repeat_count] {
        if !match_pattern(repeated_pattern, input, literals, bindings, true)? {
            return Ok(false);
        }
    }

    for (pattern, input) in suffix
        .iter()
        .zip(input_items[prefix.len() + repeat_count..].iter())
    {
        if !match_pattern(pattern, input, literals, bindings, repeated)? {
            return Ok(false);
        }
    }

    Ok(true)
}

fn seed_repeated_bindings(
    pattern: &Expr,
    literals: &HashSet<String>,
    bindings: &mut PatternBindings,
) -> Result<(), EvalError> {
    match pattern {
        Expr::Symbol(name, _) => {
            if name == "..." {
                return Err(EvalError::Syntax {
                    message: "syntax-rules: invalid ellipsis pattern".into(),
                });
            }

            if !literals.contains(name) {
                bindings.seed_repeated(name);
            }
        }
        Expr::List(items, _) => {
            for item in items {
                if !is_ellipsis(item) {
                    seed_repeated_bindings(item, literals, bindings)?;
                }
            }
        }
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }

    Ok(())
}

fn find_ellipsis_index(items: &[Expr]) -> Result<Option<usize>, EvalError> {
    let mut ellipsis_index = None;

    for (index, item) in items.iter().enumerate() {
        if !is_ellipsis(item) {
            continue;
        }

        if index == 0 {
            return Err(EvalError::Syntax {
                message: "syntax-rules: ellipsis must follow a pattern".into(),
            });
        }

        if ellipsis_index.is_some() {
            return Err(EvalError::Syntax {
                message: "syntax-rules: multiple ellipses at one list level are unsupported".into(),
            });
        }

        ellipsis_index = Some(index);
    }

    Ok(ellipsis_index)
}

fn expand_template_expr(
    template: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) => expand_template_symbol(name, *pos, state, scope, repeat_index),
        Expr::List(items, pos) => {
            if let Some(Expr::Symbol(name, _)) = items.first() {
                match name.as_str() {
                    "let" => {
                        if let Some(expanded) =
                            expand_let_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    "letrec" | "letrec*" => {
                        if let Some(expanded) =
                            expand_let_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    "lambda" => {
                        if let Some(expanded) =
                            expand_lambda_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    "case-lambda" => {
                        if let Some(expanded) =
                            expand_case_lambda_template(items, *pos, state, scope, repeat_index)?
                        {
                            return Ok(expanded);
                        }
                    }
                    _ => {}
                }
            }

            Ok(Expr::List(
                expand_template_items(items, state, scope, repeat_index)?,
                *pos,
            ))
        }
    }
}

fn expand_template_items(
    items: &[Expr],
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    let mut expanded = Vec::with_capacity(items.len());
    let mut index = 0;

    while index < items.len() {
        if is_ellipsis(&items[index]) {
            return Err(EvalError::Syntax {
                message: "template: unexpected ellipsis".into(),
            });
        }

        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            let repeat_count = repetition_len_for_template(&items[index], &state.bindings, scope)?;
            for repeated_index in 0..repeat_count {
                expanded.push(expand_template_expr(
                    &items[index],
                    state,
                    scope,
                    Some(repeated_index),
                )?);
            }
            index += 2;
            continue;
        }

        expanded.push(expand_template_expr(
            &items[index],
            state,
            scope,
            repeat_index,
        )?);
        index += 1;
    }

    Ok(expanded)
}

fn expand_template_symbol(
    name: &str,
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(expr) = state.bindings.substitute(name, repeat_index)? {
        return Ok(expr);
    }

    if let Some(renamed) = scope.get(name) {
        return Ok(Expr::Symbol(renamed.clone(), pos));
    }

    if name == "..." || is_core_syntax(name) {
        return Ok(Expr::Symbol(name.into(), pos));
    }

    if let Some(alias) = state.alias_names.get(name) {
        return Ok(Expr::Symbol(alias.clone(), pos));
    }

    let mut captured_any = false;
    let alias = fresh_identifier(name);

    if let Some(cell) = state.definition_env.lookup_cell(name) {
        state.value_aliases.push((alias.clone(), cell));
        captured_any = true;
    }

    if let Some(transformer) = state.definition_env.lookup_macro(name) {
        state.macro_aliases.push((alias.clone(), transformer));
        captured_any = true;
    }

    if captured_any {
        state.alias_names.insert(name.into(), alias.clone());
        Ok(Expr::Symbol(alias, pos))
    } else {
        Ok(Expr::Symbol(name.into(), pos))
    }
}

fn expand_let_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, bindings_expr, body @ ..] = items else {
        return Ok(None);
    };

    let Expr::List(bindings, bindings_pos) = bindings_expr else {
        return Ok(None);
    };

    let mut introduced = HashMap::new();
    let mut expanded_bindings = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts, binding_pos) = binding else {
            return Ok(None);
        };

        let [name_expr, value_expr] = parts.as_slice() else {
            return Ok(None);
        };

        let expanded_name =
            expand_binding_target(name_expr, state, scope, repeat_index, &mut introduced)?;
        let expanded_value = expand_template_expr(value_expr, state, scope, repeat_index)?;
        expanded_bindings.push(Expr::List(
            vec![expanded_name, expanded_value],
            *binding_pos,
        ));
    }

    let mut body_scope = scope.clone();
    body_scope.extend(introduced);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());
    expanded_items.push(Expr::List(expanded_bindings, *bindings_pos));
    for expr in body {
        expanded_items.push(expand_template_expr(
            expr,
            state,
            &body_scope,
            repeat_index,
        )?);
    }

    Ok(Some(Expr::List(expanded_items, pos)))
}

fn expand_lambda_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, params_expr, body @ ..] = items else {
        return Ok(None);
    };

    let (expanded_params, introduced) =
        expand_parameter_list(params_expr, state, scope, repeat_index)?;
    let mut body_scope = scope.clone();
    body_scope.extend(introduced);

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());
    expanded_items.push(expanded_params);
    for expr in body {
        expanded_items.push(expand_template_expr(
            expr,
            state,
            &body_scope,
            repeat_index,
        )?);
    }

    Ok(Some(Expr::List(expanded_items, pos)))
}

fn expand_case_lambda_template(
    items: &[Expr],
    pos: SourcePos,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    let [head, clauses @ ..] = items else {
        return Ok(None);
    };

    let mut expanded_items = Vec::with_capacity(items.len());
    expanded_items.push(head.clone());

    for clause in clauses {
        let Expr::List(parts, clause_pos) = clause else {
            return Ok(None);
        };
        let [params_expr, body @ ..] = parts.as_slice() else {
            return Ok(None);
        };

        let (expanded_params, introduced) =
            expand_parameter_list(params_expr, state, scope, repeat_index)?;
        let mut body_scope = scope.clone();
        body_scope.extend(introduced);

        let mut expanded_clause = Vec::with_capacity(parts.len());
        expanded_clause.push(expanded_params);
        for expr in body {
            expanded_clause.push(expand_template_expr(
                expr,
                state,
                &body_scope,
                repeat_index,
            )?);
        }
        expanded_items.push(Expr::List(expanded_clause, *clause_pos));
    }

    Ok(Some(Expr::List(expanded_items, pos)))
}

fn expand_parameter_list(
    params_expr: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    let Expr::List(params, pos) = params_expr else {
        return Ok((
            expand_template_expr(params_expr, state, scope, repeat_index)?,
            HashMap::new(),
        ));
    };

    let mut introduced = HashMap::new();
    let mut expanded = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                expanded.push(params[index].clone());
                index += 1;
            }
            param => {
                expanded.push(expand_binding_target(
                    param,
                    state,
                    scope,
                    repeat_index,
                    &mut introduced,
                )?);
                index += 1;
            }
        }
    }

    Ok((Expr::List(expanded, *pos), introduced))
}

fn expand_binding_target(
    target: &Expr,
    state: &mut ExpansionState,
    scope: &HashMap<String, String>,
    repeat_index: Option<usize>,
    introduced: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match target {
        Expr::Symbol(name, pos) => {
            if let Some(expr) = state.bindings.substitute(name, repeat_index)? {
                return Ok(expr);
            }

            let fresh = fresh_identifier(name);
            introduced.insert(name.clone(), fresh.clone());
            Ok(Expr::Symbol(fresh, *pos))
        }
        _ => expand_template_expr(target, state, scope, repeat_index),
    }
}

fn repetition_len_for_template(
    template: &Expr,
    bindings: &PatternBindings,
    scope: &HashMap<String, String>,
) -> Result<usize, EvalError> {
    let mut repetition_len = None;
    collect_repetition_lens(template, bindings, scope, &mut repetition_len)?;

    repetition_len.ok_or_else(|| EvalError::Syntax {
        message: "template: ellipsis without repeated pattern variable".into(),
    })
}

fn collect_repetition_lens(
    expr: &Expr,
    bindings: &PatternBindings,
    scope: &HashMap<String, String>,
    repetition_len: &mut Option<usize>,
) -> Result<(), EvalError> {
    match expr {
        Expr::Symbol(name, _) => {
            if scope.contains_key(name) {
                return Ok(());
            }

            if let Some(current_len) = bindings.repetition_len(name) {
                match repetition_len {
                    Some(existing_len) if *existing_len != current_len => {
                        return Err(EvalError::Syntax {
                            message: "template: inconsistent ellipsis lengths".into(),
                        });
                    }
                    Some(_) => {}
                    None => *repetition_len = Some(current_len),
                }
            }
        }
        Expr::List(items, _) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_repetition_lens(item, bindings, scope, repetition_len)?;
                }
            }
        }
        Expr::Number(_, _) | Expr::Boolean(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {}
    }

    Ok(())
}
