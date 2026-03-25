use std::collections::{HashMap, HashSet};

use super::core::{EnvRef, Environment, Expr, Position, Runtime};
use super::error::EvalError;

const ELLIPSIS: &str = "...";
const PROTECTED_IDENTIFIERS: &[&str] = &[
    ".",
    "and",
    "begin",
    "cond",
    "define",
    "define-syntax",
    "else",
    "if",
    "lambda",
    "let",
    "or",
    "quote",
    "set!",
    "syntax-rules",
];

#[derive(Clone)]
pub(crate) struct MacroTransformer {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    definition_env: EnvRef,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
    pattern_vars: HashSet<String>,
}

#[derive(Clone)]
enum BindingMatch {
    Scalar(Expr),
    Repeated(Vec<Expr>),
}

type Bindings = HashMap<String, BindingMatch>;

struct ExpansionState<'a> {
    bindings: &'a Bindings,
    pattern_vars: &'a HashSet<String>,
    definition_env: &'a EnvRef,
    expansion_env: EnvRef,
    captured_aliases: HashMap<String, String>,
    protected_identifiers: HashSet<String>,
    runtime: &'a mut Runtime,
}

pub(crate) fn parse_macro_definition(
    name: &str,
    spec: &Expr,
    env: &EnvRef,
) -> Result<MacroTransformer, EvalError> {
    let Expr::List(items, _) = spec else {
        return Err(positioned_syntax_error(
            spec,
            "define-syntax requires a syntax-rules transformer",
        ));
    };

    let Some((head, rest)) = items.split_first() else {
        return Err(positioned_syntax_error(
            spec,
            "define-syntax requires a syntax-rules transformer",
        ));
    };

    match head {
        Expr::Symbol(symbol, _) if symbol == "syntax-rules" => {}
        _ => {
            return Err(positioned_syntax_error(
                head,
                "define-syntax requires a syntax-rules transformer",
            ));
        }
    }

    let Some((literals_expr, rules)) = rest.split_first() else {
        return Err(positioned_syntax_error(
            spec,
            "syntax-rules requires a literals list and at least one rule",
        ));
    };

    if rules.is_empty() {
        return Err(positioned_syntax_error(
            spec,
            "syntax-rules requires at least one rule",
        ));
    }

    let literals = parse_literal_identifiers(literals_expr)?;
    let parsed_rules = rules
        .iter()
        .map(|rule| parse_syntax_rule(rule, name, &literals))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(MacroTransformer {
        name: name.to_string(),
        literals,
        rules: parsed_rules,
        definition_env: env.clone(),
    })
}

pub(crate) fn expand_macro_call(
    transformer: &MacroTransformer,
    call: &[Expr],
    env: &EnvRef,
    runtime: &mut Runtime,
) -> Result<(Expr, EnvRef), EvalError> {
    let Some(first) = call.first() else {
        return Err(syntax_error("macro call cannot be empty"));
    };
    let call_expr = Expr::List(call.to_vec(), first.pos());
    let match_literals = transformer.match_literals();

    for rule in &transformer.rules {
        let Some(bindings) = match_pattern(
            &rule.pattern,
            &call_expr,
            &match_literals,
            0,
            &Bindings::new(),
        )?
        else {
            continue;
        };

        let protected_identifiers = build_protected_identifiers(runtime);
        let expansion_env = Environment::new(Some(env.clone()));
        let mut state = ExpansionState {
            bindings: &bindings,
            pattern_vars: &rule.pattern_vars,
            definition_env: &transformer.definition_env,
            expansion_env: expansion_env.clone(),
            captured_aliases: HashMap::new(),
            protected_identifiers,
            runtime,
        };
        let expanded = expand_template(&rule.template, &mut state, None, &HashMap::new())?;
        return Ok((expanded, expansion_env));
    }

    Err(first.pos().attach(syntax_error(format!(
        "no syntax-rules clause matched macro '{}'",
        transformer.name
    ))))
}

impl MacroTransformer {
    fn match_literals(&self) -> HashSet<String> {
        let mut literals = self.literals.clone();
        literals.insert(self.name.clone());
        literals
    }
}

fn parse_literal_identifiers(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules literals must be a list",
        ));
    };

    let mut literals = HashSet::new();
    for item in items {
        match item {
            Expr::Symbol(name, _) if name != ELLIPSIS => {
                literals.insert(name.clone());
            }
            Expr::Symbol(_, _) => {
                return Err(positioned_syntax_error(
                    item,
                    "ellipsis cannot be used as a literal identifier",
                ));
            }
            _ => {
                return Err(positioned_syntax_error(
                    item,
                    "syntax-rules literals must be identifiers",
                ));
            }
        }
    }

    Ok(literals)
}

fn parse_syntax_rule(
    expr: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Result<SyntaxRule, EvalError> {
    let Expr::List(items, _) = expr else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules clauses must be (pattern template) pairs",
        ));
    };

    let [pattern, template] = items.as_slice() else {
        return Err(positioned_syntax_error(
            expr,
            "syntax-rules clauses must be (pattern template) pairs",
        ));
    };

    let Expr::List(pattern_items, _) = pattern else {
        return Err(positioned_syntax_error(
            pattern,
            "syntax-rules patterns must be lists",
        ));
    };

    let Some(head) = pattern_items.first() else {
        return Err(positioned_syntax_error(
            pattern,
            "syntax-rules patterns cannot be empty",
        ));
    };

    match head {
        Expr::Symbol(name, _) if name == macro_name => {}
        Expr::Symbol(_, _) => {
            return Err(positioned_syntax_error(
                head,
                "syntax-rules pattern must start with the macro name",
            ));
        }
        _ => {
            return Err(positioned_syntax_error(
                head,
                "syntax-rules pattern must start with an identifier",
            ));
        }
    }

    let mut pattern_vars = HashSet::new();
    let mut match_literals = literals.clone();
    match_literals.insert(macro_name.to_string());
    collect_pattern_variables(pattern, &match_literals, &mut pattern_vars);

    Ok(SyntaxRule {
        pattern: pattern.clone(),
        template: template.clone(),
        pattern_vars,
    })
}

fn collect_pattern_variables(
    expr: &Expr,
    literals: &HashSet<String>,
    pattern_vars: &mut HashSet<String>,
) {
    match expr {
        Expr::Symbol(name, _) if name != ELLIPSIS && !literals.contains(name) => {
            pattern_vars.insert(name.clone());
        }
        Expr::List(items, _) => {
            for item in items {
                collect_pattern_variables(item, literals, pattern_vars);
            }
        }
        _ => {}
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    match pattern {
        Expr::Bool(value, _) => Ok(match input {
            Expr::Bool(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Number(value, _) => Ok(match input {
            Expr::Number(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::String(value, _) => Ok(match input {
            Expr::String(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Char(value, _) => Ok(match input {
            Expr::Char(other, _) if value == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Symbol(name, _) if name == ELLIPSIS => {
            Err(syntax_error("misplaced ellipsis in macro pattern"))
        }
        Expr::Symbol(name, _) if literals.contains(name) => Ok(match input {
            Expr::Symbol(other, _) if name == other => Some(bindings.clone()),
            _ => None,
        }),
        Expr::Symbol(name, _) => bind_pattern_variable(name, input, ellipsis_depth, bindings),
        Expr::List(patterns, _) => match input {
            Expr::List(inputs, _) => {
                match_list(patterns, inputs, literals, ellipsis_depth, bindings)
            }
            _ => Ok(None),
        },
    }
}

fn match_list(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    match_list_from(patterns, 0, inputs, 0, literals, ellipsis_depth, bindings)
}

fn match_list_from(
    patterns: &[Expr],
    pattern_index: usize,
    inputs: &[Expr],
    input_index: usize,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    if pattern_index == patterns.len() {
        return Ok((input_index == inputs.len()).then(|| bindings.clone()));
    }

    if pattern_index + 1 < patterns.len() && is_ellipsis(&patterns[pattern_index + 1]) {
        let repeated = &patterns[pattern_index];
        let min_remaining = minimum_inputs_required(&patterns[pattern_index + 2..]);
        if inputs.len() < input_index + min_remaining {
            return Ok(None);
        }

        let max_repeat = inputs.len() - input_index - min_remaining;
        for repeat in (0..=max_repeat).rev() {
            let mut branch = bindings.clone();
            if repeat == 0 {
                ensure_empty_repeated_bindings(
                    repeated,
                    literals,
                    ellipsis_depth + 1,
                    &mut branch,
                )?;
            }

            let mut matched = true;
            for input in &inputs[input_index..input_index + repeat] {
                let Some(next) =
                    match_pattern(repeated, input, literals, ellipsis_depth + 1, &branch)?
                else {
                    matched = false;
                    break;
                };
                branch = next;
            }

            if !matched {
                continue;
            }

            if let Some(done) = match_list_from(
                patterns,
                pattern_index + 2,
                inputs,
                input_index + repeat,
                literals,
                ellipsis_depth,
                &branch,
            )? {
                return Ok(Some(done));
            }
        }

        Ok(None)
    } else {
        let Some(input) = inputs.get(input_index) else {
            return Ok(None);
        };
        let Some(next) = match_pattern(
            &patterns[pattern_index],
            input,
            literals,
            ellipsis_depth,
            bindings,
        )?
        else {
            return Ok(None);
        };
        match_list_from(
            patterns,
            pattern_index + 1,
            inputs,
            input_index + 1,
            literals,
            ellipsis_depth,
            &next,
        )
    }
}

fn bind_pattern_variable(
    name: &str,
    input: &Expr,
    ellipsis_depth: usize,
    bindings: &Bindings,
) -> Result<Option<Bindings>, EvalError> {
    if ellipsis_depth > 1 {
        return Err(syntax_error("nested ellipsis is not supported"));
    }

    let mut next = bindings.clone();
    match ellipsis_depth {
        0 => match next.get(name) {
            None => {
                next.insert(name.to_string(), BindingMatch::Scalar(input.clone()));
                Ok(Some(next))
            }
            Some(BindingMatch::Scalar(existing)) if existing == input => Ok(Some(next)),
            Some(BindingMatch::Scalar(_)) | Some(BindingMatch::Repeated(_)) => Ok(None),
        },
        1 => {
            match next.entry(name.to_string()) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(BindingMatch::Repeated(vec![input.clone()]));
                }
                std::collections::hash_map::Entry::Occupied(mut entry) => match entry.get_mut() {
                    BindingMatch::Repeated(values) => values.push(input.clone()),
                    BindingMatch::Scalar(_) => return Ok(None),
                },
            }
            Ok(Some(next))
        }
        _ => unreachable!("nested ellipsis handled above"),
    }
}

fn ensure_empty_repeated_bindings(
    pattern: &Expr,
    literals: &HashSet<String>,
    ellipsis_depth: usize,
    bindings: &mut Bindings,
) -> Result<(), EvalError> {
    if ellipsis_depth > 1 {
        return Err(syntax_error("nested ellipsis is not supported"));
    }

    match pattern {
        Expr::Symbol(name, _) if name != ELLIPSIS && !literals.contains(name) => {
            if ellipsis_depth == 1 {
                bindings
                    .entry(name.clone())
                    .or_insert_with(|| BindingMatch::Repeated(Vec::new()));
            }
            Ok(())
        }
        Expr::List(items, _) => {
            let mut index = 0;
            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    ensure_empty_repeated_bindings(
                        &items[index],
                        literals,
                        ellipsis_depth + 1,
                        bindings,
                    )?;
                    index += 2;
                } else {
                    ensure_empty_repeated_bindings(
                        &items[index],
                        literals,
                        ellipsis_depth,
                        bindings,
                    )?;
                    index += 1;
                }
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn minimum_inputs_required(patterns: &[Expr]) -> usize {
    let mut required = 0;
    let mut index = 0;
    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis(&patterns[index + 1]) {
            index += 2;
        } else {
            required += 1;
            index += 1;
        }
    }
    required
}

fn expand_template(
    template: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Bool(_, _) | Expr::Number(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) => {
            expand_template_symbol(name, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_quote_form(items, state.pattern_vars) => {
            let datum = expand_quoted_template(&items[1], state, repetition_index)?;
            Ok(Expr::List(
                vec![Expr::Symbol("quote".into(), items[0].pos()), datum],
                *pos,
            ))
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "let") => {
            expand_template_let(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "lambda") => {
            expand_template_lambda(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) if is_template_head(items, state.pattern_vars, "define") => {
            expand_template_define(items, *pos, state, repetition_index, local_renames)
        }
        Expr::List(items, pos) => {
            expand_plain_list(items, *pos, state, repetition_index, local_renames)
        }
    }
}

fn expand_template_symbol(
    name: &str,
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if name == ELLIPSIS {
        return Err(pos.attach(syntax_error("unexpected ellipsis in macro template")));
    }

    if state.pattern_vars.contains(name) {
        return expand_pattern_variable(name, pos, state, repetition_index);
    }

    if let Some(rename) = local_renames.get(name) {
        return Ok(Expr::Symbol(rename.clone(), pos));
    }

    if state.protected_identifiers.contains(name) {
        return Ok(Expr::Symbol(name.to_string(), pos));
    }

    Ok(capture_free_identifier(name, pos, state))
}

fn expand_pattern_variable(
    name: &str,
    pos: Position,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match state.bindings.get(name) {
        Some(BindingMatch::Scalar(expr)) => Ok(expr.clone()),
        Some(BindingMatch::Repeated(values)) => {
            let Some(index) = repetition_index else {
                return Err(pos.attach(syntax_error(format!(
                    "pattern variable '{name}' requires ellipsis in the template"
                ))));
            };

            values.get(index).cloned().ok_or_else(|| {
                pos.attach(syntax_error(format!(
                    "pattern variable '{name}' is missing repetition {index}"
                )))
            })
        }
        None => Err(pos.attach(syntax_error(format!(
            "unbound pattern variable '{name}' in macro template"
        )))),
    }
}

fn expand_plain_list(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            if repetition_index.is_some() {
                return Err(items[index + 1]
                    .pos()
                    .attach(syntax_error("nested ellipsis is not supported")));
            }

            let repeat_count = template_repeat_count(&items[index], state)?;
            for repeat in 0..repeat_count {
                expanded.push(expand_template(
                    &items[index],
                    state,
                    Some(repeat),
                    local_renames,
                )?);
            }
            index += 2;
        } else {
            expanded.push(expand_template(
                &items[index],
                state,
                repetition_index,
                local_renames,
            )?);
            index += 1;
        }
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_template_let(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let Expr::List(bindings, bindings_pos) = &items[1] else {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    };

    let mut body_renames = local_renames.clone();
    let mut expanded_bindings = Vec::with_capacity(bindings.len());

    for binding in bindings {
        let Expr::List(parts, binding_pos) = binding else {
            return Err(positioned_syntax_error(
                binding,
                "macro-generated let bindings must be lists",
            ));
        };
        let [name_expr, value_expr] = parts.as_slice() else {
            return Err(positioned_syntax_error(
                binding,
                "macro-generated let bindings must contain a name and value",
            ));
        };

        let expanded_name = expand_binding_identifier(
            name_expr,
            state,
            repetition_index,
            local_renames,
            &mut body_renames,
        )?;
        let expanded_value = expand_template(value_expr, state, repetition_index, local_renames)?;
        expanded_bindings.push(Expr::List(
            vec![expanded_name, expanded_value],
            *binding_pos,
        ));
    }

    let mut expanded = vec![
        Expr::Symbol("let".into(), items[0].pos()),
        Expr::List(expanded_bindings, *bindings_pos),
    ];
    for body in &items[2..] {
        expanded.push(expand_template(
            body,
            state,
            repetition_index,
            &body_renames,
        )?);
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_template_lambda(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let mut body_renames = local_renames.clone();
    let params = expand_formals(
        &items[1],
        state,
        repetition_index,
        local_renames,
        &mut body_renames,
    )?;

    let mut expanded = vec![Expr::Symbol("lambda".into(), items[0].pos()), params];
    for body in &items[2..] {
        expanded.push(expand_template(
            body,
            state,
            repetition_index,
            &body_renames,
        )?);
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_template_define(
    items: &[Expr],
    pos: Position,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return expand_plain_list(items, pos, state, repetition_index, local_renames);
    }

    let mut expanded = vec![Expr::Symbol("define".into(), items[0].pos())];
    match &items[1] {
        Expr::Symbol(_, _) => expanded.push(expand_binding_identifier(
            &items[1],
            state,
            repetition_index,
            local_renames,
            &mut local_renames.clone(),
        )?),
        Expr::List(signature, signature_pos) if !signature.is_empty() => {
            let mut body_renames = local_renames.clone();
            let name = expand_binding_identifier(
                &signature[0],
                state,
                repetition_index,
                local_renames,
                &mut body_renames,
            )?;
            let mut expanded_signature = vec![name];
            for param in &signature[1..] {
                expanded_signature.push(expand_binding_identifier(
                    param,
                    state,
                    repetition_index,
                    local_renames,
                    &mut body_renames,
                )?);
            }
            expanded.push(Expr::List(expanded_signature, *signature_pos));
            for body in &items[2..] {
                expanded.push(expand_template(
                    body,
                    state,
                    repetition_index,
                    &body_renames,
                )?);
            }
            return Ok(Expr::List(expanded, pos));
        }
        _ => return expand_plain_list(items, pos, state, repetition_index, local_renames),
    }

    for value in &items[2..] {
        expanded.push(expand_template(
            value,
            state,
            repetition_index,
            local_renames,
        )?);
    }

    Ok(Expr::List(expanded, pos))
}

fn expand_formals(
    formals: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match formals {
        Expr::List(params, pos) => {
            let mut expanded = Vec::with_capacity(params.len());
            for param in params {
                match param {
                    Expr::Symbol(name, param_pos) if name == "." => {
                        expanded.push(Expr::Symbol(name.clone(), *param_pos));
                    }
                    _ => expanded.push(expand_binding_identifier(
                        param,
                        state,
                        repetition_index,
                        local_renames,
                        body_renames,
                    )?),
                }
            }
            Ok(Expr::List(expanded, *pos))
        }
        Expr::Symbol(name, pos) if name != "." => expand_binding_identifier(
            formals,
            state,
            repetition_index,
            local_renames,
            body_renames,
        ),
        _ => expand_template(formals, state, repetition_index, local_renames),
    }
}

fn expand_binding_identifier(
    template: &Expr,
    state: &mut ExpansionState<'_>,
    repetition_index: Option<usize>,
    local_renames: &HashMap<String, String>,
    body_renames: &mut HashMap<String, String>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Symbol(name, pos) if !state.pattern_vars.contains(name) && name != "." => {
            let fresh = state.runtime.fresh_symbol(name);
            body_renames.insert(name.clone(), fresh.clone());
            Ok(Expr::Symbol(fresh, *pos))
        }
        _ => expand_template(template, state, repetition_index, local_renames),
    }
}

fn expand_quoted_template(
    template: &Expr,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Expr::Bool(_, _) | Expr::Number(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
            Ok(template.clone())
        }
        Expr::Symbol(name, pos) if name == ELLIPSIS => {
            Err(pos.attach(syntax_error("unexpected ellipsis in quoted macro template")))
        }
        Expr::Symbol(name, pos) if state.pattern_vars.contains(name) => {
            expand_pattern_variable(name, *pos, state, repetition_index)
        }
        Expr::Symbol(_, _) => Ok(template.clone()),
        Expr::List(items, pos) => expand_quoted_list(items, *pos, state, repetition_index),
    }
}

fn expand_quoted_list(
    items: &[Expr],
    pos: Position,
    state: &ExpansionState<'_>,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let mut expanded = Vec::new();
    let mut index = 0;

    while index < items.len() {
        if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
            if repetition_index.is_some() {
                return Err(items[index + 1]
                    .pos()
                    .attach(syntax_error("nested ellipsis is not supported")));
            }

            let repeat_count = template_repeat_count(&items[index], state)?;
            for repeat in 0..repeat_count {
                expanded.push(expand_quoted_template(&items[index], state, Some(repeat))?);
            }
            index += 2;
        } else {
            expanded.push(expand_quoted_template(
                &items[index],
                state,
                repetition_index,
            )?);
            index += 1;
        }
    }

    Ok(Expr::List(expanded, pos))
}

fn template_repeat_count(template: &Expr, state: &ExpansionState<'_>) -> Result<usize, EvalError> {
    let mut counts = Vec::new();
    collect_repeat_counts(template, state.pattern_vars, state.bindings, &mut counts);
    let Some(first) = counts.first().copied() else {
        return Err(positioned_syntax_error(
            template,
            "ellipsis in macro template requires a repeated pattern variable",
        ));
    };

    if counts.iter().any(|count| *count != first) {
        return Err(positioned_syntax_error(
            template,
            "all repeated pattern variables in a template must have the same length",
        ));
    }

    Ok(first)
}

fn collect_repeat_counts(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    bindings: &Bindings,
    counts: &mut Vec<usize>,
) {
    match template {
        Expr::Symbol(name, _) if pattern_vars.contains(name) => {
            if let Some(BindingMatch::Repeated(values)) = bindings.get(name) {
                counts.push(values.len());
            }
        }
        Expr::List(items, _) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_repeat_counts(item, pattern_vars, bindings, counts);
                }
            }
        }
        _ => {}
    }
}

fn capture_free_identifier(name: &str, pos: Position, state: &mut ExpansionState<'_>) -> Expr {
    if let Some(alias) = state.captured_aliases.get(name) {
        return Expr::Symbol(alias.clone(), pos);
    }

    let Some(binding) = Environment::lookup_cell(state.definition_env, name) else {
        return Expr::Symbol(name.to_string(), pos);
    };

    let alias = state.runtime.fresh_symbol(name);
    Environment::define_cell(&state.expansion_env, alias.clone(), binding);
    state
        .captured_aliases
        .insert(name.to_string(), alias.clone());
    Expr::Symbol(alias, pos)
}

fn build_protected_identifiers(runtime: &Runtime) -> HashSet<String> {
    let mut identifiers = PROTECTED_IDENTIFIERS
        .iter()
        .map(|name| (*name).to_string())
        .collect::<HashSet<_>>();
    identifiers.extend(runtime.macro_names());
    identifiers
}

fn is_template_head(items: &[Expr], pattern_vars: &HashSet<String>, name: &str) -> bool {
    matches!(items.first(), Some(Expr::Symbol(symbol, _)) if symbol == name && !pattern_vars.contains(symbol))
}

fn is_quote_form(items: &[Expr], pattern_vars: &HashSet<String>) -> bool {
    items.len() == 2 && is_template_head(items, pattern_vars, "quote")
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == ELLIPSIS)
}

fn syntax_error(message: impl Into<String>) -> EvalError {
    EvalError::SyntaxError {
        message: message.into(),
    }
}

fn positioned_syntax_error(expr: &Expr, message: impl Into<String>) -> EvalError {
    expr.pos().attach(syntax_error(message))
}
