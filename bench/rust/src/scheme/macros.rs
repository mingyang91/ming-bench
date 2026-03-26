use super::{env_lookup_binding, EnvRef, EvalError, Expr, SourcePos, Value};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

type MatchEnv = HashMap<String, MatchValue>;
type MacroRef = Rc<SyntaxRulesMacro>;
pub(super) type MacroEnvRef = Rc<RefCell<MacroEnvironment>>;

static MACRO_GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub(super) struct MacroEnvironment {
    parent: Option<MacroEnvRef>,
    bindings: HashMap<String, MacroRef>,
}

struct SyntaxRulesMacro {
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    env: EnvRef,
}

struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
enum MatchValue {
    Single(Expr),
    Repetition(Vec<MatchValue>),
}

#[derive(Clone)]
enum SyntaxExpr {
    UseSite(Expr),
    Symbol(String, SourcePos),
    List(Vec<SyntaxExpr>, SourcePos),
}

impl MacroEnvironment {
    pub(super) fn new(parent: Option<MacroEnvRef>) -> MacroEnvRef {
        Rc::new(RefCell::new(Self {
            parent,
            bindings: HashMap::new(),
        }))
    }
}

pub(super) fn eval_define_syntax(
    args: &[Expr],
    env: &EnvRef,
    macro_env: &MacroEnvRef,
) -> Result<Value, EvalError> {
    let [Expr::Symbol(name, _), transformer_spec] = args else {
        return Err(EvalError::SyntaxError {
            message: "invalid define-syntax".into(),
        });
    };

    let transformer = parse_syntax_rules(transformer_spec, env)?;
    macro_env_define(macro_env, name.clone(), Rc::new(transformer));
    Ok(Value::Void)
}

pub(super) fn expand_macro_call(
    items: &[Expr],
    position: SourcePos,
    macro_env: &MacroEnvRef,
) -> Result<Option<Expr>, EvalError> {
    let Some(Expr::Symbol(name, _)) = items.first() else {
        return Ok(None);
    };
    let Some(transformer) = macro_env_lookup(macro_env, name) else {
        return Ok(None);
    };

    let invocation = Expr::List(items.to_vec(), position);
    let expanded = transformer.expand(&invocation)?;
    Ok(Some(expanded))
}

fn macro_env_define(macro_env: &MacroEnvRef, name: String, transformer: MacroRef) {
    macro_env.borrow_mut().bindings.insert(name, transformer);
}

fn macro_env_lookup(macro_env: &MacroEnvRef, name: &str) -> Option<MacroRef> {
    let (binding, parent) = {
        let borrowed = macro_env.borrow();
        (
            borrowed.bindings.get(name).cloned(),
            borrowed.parent.clone(),
        )
    };

    binding.or_else(|| parent.and_then(|parent| macro_env_lookup(&parent, name)))
}

fn parse_syntax_rules(
    transformer_spec: &Expr,
    env: &EnvRef,
) -> Result<SyntaxRulesMacro, EvalError> {
    let Expr::List(items, _) = transformer_spec else {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a syntax-rules transformer".into(),
        });
    };

    let Some((Expr::Symbol(keyword, _), rest)) = items.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a syntax-rules transformer".into(),
        });
    };

    if keyword != "syntax-rules" {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a syntax-rules transformer".into(),
        });
    }

    let Some((literal_list, rule_exprs)) = rest.split_first() else {
        return Err(EvalError::SyntaxError {
            message: "syntax-rules requires literals and at least one rule".into(),
        });
    };

    let Expr::List(literal_items, _) = literal_list else {
        return Err(EvalError::SyntaxError {
            message: "syntax-rules literal list must be a list".into(),
        });
    };

    if rule_exprs.is_empty() {
        return Err(EvalError::SyntaxError {
            message: "syntax-rules requires at least one rule".into(),
        });
    }

    let literals = literal_items
        .iter()
        .map(|literal| match literal {
            Expr::Symbol(name, _) if name != "..." => Ok(name.clone()),
            _ => Err(EvalError::SyntaxError {
                message: "syntax-rules literals must be symbols".into(),
            }),
        })
        .collect::<Result<HashSet<_>, _>>()?;

    let mut rules = Vec::with_capacity(rule_exprs.len());
    for rule_expr in rule_exprs {
        let Expr::List(rule_items, _) = rule_expr else {
            return Err(EvalError::SyntaxError {
                message: "syntax-rules clauses must be (pattern template) pairs".into(),
            });
        };

        let [pattern, template] = rule_items.as_slice() else {
            return Err(EvalError::SyntaxError {
                message: "syntax-rules clauses must be (pattern template) pairs".into(),
            });
        };

        rules.push(SyntaxRule {
            pattern: pattern.clone(),
            template: template.clone(),
        });
    }

    Ok(SyntaxRulesMacro {
        literals,
        rules,
        env: Rc::clone(env),
    })
}

impl SyntaxRulesMacro {
    fn expand(&self, invocation: &Expr) -> Result<Expr, EvalError> {
        for rule in &self.rules {
            let Some(bindings) = match_pattern(&rule.pattern, invocation, &self.literals)? else {
                continue;
            };

            let syntax = expand_template(&rule.template, &bindings, &[])?;
            return Ok(hygienize_syntax(&syntax, &self.env, &HashMap::new()));
        }

        Err(EvalError::SyntaxError {
            message: "no matching syntax-rules clause".into(),
        })
    }
}

fn match_pattern(
    pattern: &Expr,
    expr: &Expr,
    literals: &HashSet<String>,
) -> Result<Option<MatchEnv>, EvalError> {
    match pattern {
        Expr::Integer(pattern_value, _) => Ok(match expr {
            Expr::Integer(value, _) if value == pattern_value => Some(HashMap::new()),
            _ => None,
        }),
        Expr::Boolean(pattern_value, _) => Ok(match expr {
            Expr::Boolean(value, _) if value == pattern_value => Some(HashMap::new()),
            _ => None,
        }),
        Expr::String(pattern_value, _) => Ok(match expr {
            Expr::String(value, _) if value == pattern_value => Some(HashMap::new()),
            _ => None,
        }),
        Expr::Char(pattern_value, _) => Ok(match expr {
            Expr::Char(value, _) if value == pattern_value => Some(HashMap::new()),
            _ => None,
        }),
        Expr::Symbol(name, _) => match name.as_str() {
            "_" => Ok(Some(HashMap::new())),
            "..." => Err(EvalError::SyntaxError {
                message: "invalid use of ellipsis in syntax pattern".into(),
            }),
            _ if literals.contains(name) => Ok(symbol_name(expr)
                .filter(|actual| *actual == name)
                .map(|_| HashMap::new())),
            _ => {
                let mut env = HashMap::new();
                env.insert(name.clone(), MatchValue::Single(expr.clone()));
                Ok(Some(env))
            }
        },
        Expr::CapturedSymbol(_, _, _) => Err(EvalError::SyntaxError {
            message: "invalid syntax-rules pattern".into(),
        }),
        Expr::List(pattern_items, _) => {
            let Expr::List(expr_items, _) = expr else {
                return Ok(None);
            };
            match_list_pattern(pattern_items, expr_items, literals)
        }
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    expr_items: &[Expr],
    literals: &HashSet<String>,
) -> Result<Option<MatchEnv>, EvalError> {
    match_list_pattern_from(pattern_items, expr_items, 0, 0, literals)
}

fn match_list_pattern_from(
    pattern_items: &[Expr],
    expr_items: &[Expr],
    pattern_index: usize,
    expr_index: usize,
    literals: &HashSet<String>,
) -> Result<Option<MatchEnv>, EvalError> {
    if pattern_index == pattern_items.len() {
        return Ok((expr_index == expr_items.len()).then(HashMap::new));
    }

    if pattern_index + 1 < pattern_items.len() && is_ellipsis(&pattern_items[pattern_index + 1]) {
        let repeated_pattern = &pattern_items[pattern_index];
        let repeated_vars = pattern_variables(repeated_pattern, literals);

        for repeat_count in 0..=expr_items.len().saturating_sub(expr_index) {
            let mut repeated_matches = Vec::with_capacity(repeat_count);
            let mut matched = true;

            for actual in &expr_items[expr_index..expr_index + repeat_count] {
                match match_pattern(repeated_pattern, actual, literals)? {
                    Some(bindings) => repeated_matches.push(bindings),
                    None => {
                        matched = false;
                        break;
                    }
                }
            }

            if !matched {
                continue;
            }

            let Some(rest) = match_list_pattern_from(
                pattern_items,
                expr_items,
                pattern_index + 2,
                expr_index + repeat_count,
                literals,
            )?
            else {
                continue;
            };

            let repeated = collect_repetition_bindings(&repeated_vars, &repeated_matches);
            if let Some(merged) = merge_match_envs(repeated, rest) {
                return Ok(Some(merged));
            }
        }

        return Ok(None);
    }

    if expr_index >= expr_items.len() {
        return Ok(None);
    }

    let Some(current) = match_pattern(
        &pattern_items[pattern_index],
        &expr_items[expr_index],
        literals,
    )?
    else {
        return Ok(None);
    };
    let Some(rest) = match_list_pattern_from(
        pattern_items,
        expr_items,
        pattern_index + 1,
        expr_index + 1,
        literals,
    )?
    else {
        return Ok(None);
    };

    Ok(merge_match_envs(current, rest))
}

fn collect_repetition_bindings(
    repeated_vars: &HashSet<String>,
    repeated_matches: &[MatchEnv],
) -> MatchEnv {
    let mut bindings = HashMap::with_capacity(repeated_vars.len());

    for name in repeated_vars {
        let mut values = Vec::with_capacity(repeated_matches.len());
        for repeated_match in repeated_matches {
            if let Some(value) = repeated_match.get(name) {
                values.push(value.clone());
            }
        }
        bindings.insert(name.clone(), MatchValue::Repetition(values));
    }

    bindings
}

fn merge_match_envs(mut left: MatchEnv, right: MatchEnv) -> Option<MatchEnv> {
    for (name, value) in right {
        match left.get(&name) {
            Some(existing) if !match_values_equal(existing, &value) => return None,
            Some(_) => {}
            None => {
                left.insert(name, value);
            }
        }
    }

    Some(left)
}

fn match_values_equal(left: &MatchValue, right: &MatchValue) -> bool {
    match (left, right) {
        (MatchValue::Single(left), MatchValue::Single(right)) => expr_datum_equal(left, right),
        (MatchValue::Repetition(left), MatchValue::Repetition(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| match_values_equal(left, right))
        }
        _ => false,
    }
}

fn expr_datum_equal(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Integer(left, _), Expr::Integer(right, _)) => left == right,
        (Expr::Boolean(left, _), Expr::Boolean(right, _)) => left == right,
        (Expr::String(left, _), Expr::String(right, _)) => left == right,
        (Expr::Char(left, _), Expr::Char(right, _)) => left == right,
        (Expr::Symbol(left, _), Expr::Symbol(right, _))
        | (Expr::Symbol(left, _), Expr::CapturedSymbol(right, _, _))
        | (Expr::CapturedSymbol(left, _, _), Expr::Symbol(right, _))
        | (Expr::CapturedSymbol(left, _, _), Expr::CapturedSymbol(right, _, _)) => left == right,
        (Expr::List(left, _), Expr::List(right, _)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_datum_equal(left, right))
        }
        _ => false,
    }
}

fn pattern_variables(pattern: &Expr, literals: &HashSet<String>) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_pattern_variables(pattern, literals, &mut vars);
    vars
}

fn collect_pattern_variables(
    pattern: &Expr,
    literals: &HashSet<String>,
    vars: &mut HashSet<String>,
) {
    match pattern {
        Expr::Symbol(name, _) if name != "_" && name != "..." && !literals.contains(name) => {
            vars.insert(name.clone());
        }
        Expr::List(items, _) => {
            for item in items {
                collect_pattern_variables(item, literals, vars);
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &MatchEnv,
    repetition_indices: &[usize],
) -> Result<SyntaxExpr, EvalError> {
    match template {
        Expr::Symbol(name, position) => {
            expand_template_symbol(name, *position, bindings, repetition_indices)
        }
        Expr::Integer(_, _)
        | Expr::Boolean(_, _)
        | Expr::String(_, _)
        | Expr::Char(_, _)
        | Expr::CapturedSymbol(_, _, _) => Ok(SyntaxExpr::UseSite(template.clone())),
        Expr::List(items, position) => {
            let mut expanded = Vec::new();
            let mut index = 0;

            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    let repeat_count =
                        template_repeat_count(&items[index], bindings, repetition_indices)?;
                    for repeat_index in 0..repeat_count {
                        let mut nested_indices = repetition_indices.to_vec();
                        nested_indices.push(repeat_index);
                        expanded.push(expand_template(&items[index], bindings, &nested_indices)?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(
                    &items[index],
                    bindings,
                    repetition_indices,
                )?);
                index += 1;
            }

            Ok(SyntaxExpr::List(expanded, *position))
        }
    }
}

fn expand_template_symbol(
    name: &str,
    position: SourcePos,
    bindings: &MatchEnv,
    repetition_indices: &[usize],
) -> Result<SyntaxExpr, EvalError> {
    let Some(binding) = bindings.get(name) else {
        return Ok(SyntaxExpr::Symbol(name.into(), position));
    };

    match project_match_value(binding, repetition_indices)? {
        MatchValue::Single(expr) => Ok(SyntaxExpr::UseSite(expr.clone())),
        MatchValue::Repetition(_) => Err(EvalError::SyntaxError {
            message: format!("missing ellipsis for template variable: {name}"),
        }),
    }
}

fn template_repeat_count(
    template: &Expr,
    bindings: &MatchEnv,
    repetition_indices: &[usize],
) -> Result<usize, EvalError> {
    let template_vars = template_variables(template, bindings);
    let mut repeat_count = None;

    for name in template_vars {
        let Some(binding) = bindings.get(&name) else {
            continue;
        };

        if let MatchValue::Repetition(values) = project_match_value(binding, repetition_indices)? {
            match repeat_count {
                Some(existing) if existing != values.len() => {
                    return Err(EvalError::SyntaxError {
                        message: "inconsistent ellipsis expansion".into(),
                    })
                }
                Some(_) => {}
                None => repeat_count = Some(values.len()),
            }
        }
    }

    repeat_count.ok_or_else(|| EvalError::SyntaxError {
        message: "ellipsis template must reference a repeated pattern variable".into(),
    })
}

fn template_variables(template: &Expr, bindings: &MatchEnv) -> HashSet<String> {
    let mut vars = HashSet::new();
    collect_template_variables(template, bindings, &mut vars);
    vars
}

fn collect_template_variables(template: &Expr, bindings: &MatchEnv, vars: &mut HashSet<String>) {
    match template {
        Expr::Symbol(name, _) if name != "..." && bindings.contains_key(name) => {
            vars.insert(name.clone());
        }
        Expr::List(items, _) => {
            for item in items {
                collect_template_variables(item, bindings, vars);
            }
        }
        _ => {}
    }
}

fn project_match_value<'a>(
    value: &'a MatchValue,
    repetition_indices: &[usize],
) -> Result<&'a MatchValue, EvalError> {
    let mut current = value;

    for &index in repetition_indices {
        let MatchValue::Repetition(values) = current else {
            return Err(EvalError::SyntaxError {
                message: "too many ellipses in template".into(),
            });
        };
        current = values.get(index).ok_or_else(|| EvalError::SyntaxError {
            message: "ellipsis expansion index out of bounds".into(),
        })?;
    }

    Ok(current)
}

fn hygienize_syntax(
    expr: &SyntaxExpr,
    definition_env: &EnvRef,
    rename_map: &HashMap<String, String>,
) -> Expr {
    match expr {
        SyntaxExpr::UseSite(expr) => expr.clone(),
        SyntaxExpr::Symbol(name, position) => {
            if let Some(fresh_name) = rename_map.get(name) {
                Expr::Symbol(fresh_name.clone(), *position)
            } else if is_core_syntax(name) {
                Expr::Symbol(name.clone(), *position)
            } else if let Some(binding) = env_lookup_binding(definition_env, name) {
                Expr::CapturedSymbol(name.clone(), binding, *position)
            } else {
                Expr::Symbol(name.clone(), *position)
            }
        }
        SyntaxExpr::List(items, position) => {
            if let Some(keyword) = introduced_symbol(items.first()) {
                match keyword {
                    "lambda" => {
                        return hygienize_lambda(items, *position, definition_env, rename_map)
                    }
                    "let" => return hygienize_let(items, *position, definition_env, rename_map),
                    "define" => {
                        return hygienize_define(items, *position, definition_env, rename_map)
                    }
                    _ => {}
                }
            }

            Expr::List(
                items
                    .iter()
                    .map(|item| hygienize_syntax(item, definition_env, rename_map))
                    .collect(),
                *position,
            )
        }
    }
}

fn hygienize_lambda(
    items: &[SyntaxExpr],
    position: SourcePos,
    definition_env: &EnvRef,
    rename_map: &HashMap<String, String>,
) -> Expr {
    if items.len() < 2 {
        return Expr::List(
            items
                .iter()
                .map(|item| hygienize_syntax(item, definition_env, rename_map))
                .collect(),
            position,
        );
    }

    let head = hygienize_syntax(&items[0], definition_env, rename_map);
    let (params, body_map) = hygienize_formals(&items[1], rename_map);

    let mut result = vec![head, params];
    for body_expr in &items[2..] {
        result.push(hygienize_syntax(body_expr, definition_env, &body_map));
    }

    Expr::List(result, position)
}

fn hygienize_let(
    items: &[SyntaxExpr],
    position: SourcePos,
    definition_env: &EnvRef,
    rename_map: &HashMap<String, String>,
) -> Expr {
    if items.len() < 2 {
        return Expr::List(
            items
                .iter()
                .map(|item| hygienize_syntax(item, definition_env, rename_map))
                .collect(),
            position,
        );
    }

    let head = hygienize_syntax(&items[0], definition_env, rename_map);

    let mut result = vec![head];
    let mut body_map = rename_map.clone();
    let mut body_start = 2;

    if items.len() >= 3 && !matches!(items[1], SyntaxExpr::List(_, _)) {
        let (name_expr, name_mapping) = hygienize_binding_identifier(&items[1]);
        extend_scope(&mut body_map, name_mapping);
        result.push(name_expr);
        body_start = 3;
    }

    if body_start <= items.len() {
        let binding_index = body_start - 1;
        if let Some(bindings_expr) = items.get(binding_index) {
            let (bindings, binding_scope) =
                hygienize_let_bindings(bindings_expr, definition_env, rename_map);
            extend_scope(&mut body_map, binding_scope);
            result.push(bindings);
        }
    }

    for body_expr in &items[body_start..] {
        result.push(hygienize_syntax(body_expr, definition_env, &body_map));
    }

    Expr::List(result, position)
}

fn hygienize_define(
    items: &[SyntaxExpr],
    position: SourcePos,
    definition_env: &EnvRef,
    rename_map: &HashMap<String, String>,
) -> Expr {
    if items.len() < 3 {
        return Expr::List(
            items
                .iter()
                .map(|item| hygienize_syntax(item, definition_env, rename_map))
                .collect(),
            position,
        );
    }

    let head = hygienize_syntax(&items[0], definition_env, rename_map);
    let mut result = vec![head];

    match &items[1] {
        SyntaxExpr::List(signature_items, signature_pos) if !signature_items.is_empty() => {
            let (name_expr, name_mapping) = hygienize_binding_identifier(&signature_items[0]);
            let mut body_map = rename_map.clone();
            extend_scope(&mut body_map, name_mapping);

            let params_syntax = SyntaxExpr::List(signature_items[1..].to_vec(), *signature_pos);
            let (params_expr, body_map) = hygienize_formals(&params_syntax, &body_map);
            let Expr::List(param_items, _) = params_expr else {
                return Expr::List(
                    items
                        .iter()
                        .map(|item| hygienize_syntax(item, definition_env, rename_map))
                        .collect(),
                    position,
                );
            };

            let mut signature = Vec::with_capacity(param_items.len() + 1);
            signature.push(name_expr);
            signature.extend(param_items);
            result.push(Expr::List(signature, *signature_pos));

            for body_expr in &items[2..] {
                result.push(hygienize_syntax(body_expr, definition_env, &body_map));
            }
        }
        name => {
            let (name_expr, _) = hygienize_binding_identifier(name);
            result.push(name_expr);
            result.push(hygienize_syntax(&items[2], definition_env, rename_map));
            for expr in &items[3..] {
                result.push(hygienize_syntax(expr, definition_env, rename_map));
            }
        }
    }

    Expr::List(result, position)
}

fn hygienize_formals(
    formals: &SyntaxExpr,
    rename_map: &HashMap<String, String>,
) -> (Expr, HashMap<String, String>) {
    let SyntaxExpr::List(items, position) = formals else {
        return (hygienize_binding_reference(formals), rename_map.clone());
    };

    let mut fresh_scope = HashMap::new();
    let mut params = Vec::with_capacity(items.len());

    for item in items {
        if matches!(item, SyntaxExpr::Symbol(name, _) if name == ".") {
            params.push(hygienize_binding_reference(item));
            continue;
        }

        let (param, mapping) = hygienize_binding_identifier(item);
        extend_scope(&mut fresh_scope, mapping);
        params.push(param);
    }

    let mut body_map = rename_map.clone();
    extend_scope(&mut body_map, fresh_scope);
    (Expr::List(params, *position), body_map)
}

fn hygienize_let_bindings(
    bindings: &SyntaxExpr,
    definition_env: &EnvRef,
    rename_map: &HashMap<String, String>,
) -> (Expr, HashMap<String, String>) {
    let SyntaxExpr::List(items, position) = bindings else {
        return (
            hygienize_syntax(bindings, definition_env, rename_map),
            HashMap::new(),
        );
    };

    let mut fresh_scope = HashMap::new();
    let mut binding_exprs = Vec::with_capacity(items.len());

    for item in items {
        let SyntaxExpr::List(pair_items, pair_position) = item else {
            binding_exprs.push(hygienize_syntax(item, definition_env, rename_map));
            continue;
        };

        if pair_items.len() != 2 {
            binding_exprs.push(hygienize_syntax(item, definition_env, rename_map));
            continue;
        }

        let (name_expr, mapping) = hygienize_binding_identifier(&pair_items[0]);
        extend_scope(&mut fresh_scope, mapping);
        let value_expr = hygienize_syntax(&pair_items[1], definition_env, rename_map);
        binding_exprs.push(Expr::List(vec![name_expr, value_expr], *pair_position));
    }

    (Expr::List(binding_exprs, *position), fresh_scope)
}

fn hygienize_binding_identifier(expr: &SyntaxExpr) -> (Expr, HashMap<String, String>) {
    match expr {
        SyntaxExpr::Symbol(name, position) => {
            if name == "." {
                return (Expr::Symbol(name.clone(), *position), HashMap::new());
            }

            let fresh_name = fresh_symbol(name);
            let mut mapping = HashMap::new();
            mapping.insert(name.clone(), fresh_name.clone());
            (Expr::Symbol(fresh_name, *position), mapping)
        }
        _ => (hygienize_binding_reference(expr), HashMap::new()),
    }
}

fn hygienize_binding_reference(expr: &SyntaxExpr) -> Expr {
    match expr {
        SyntaxExpr::UseSite(Expr::Symbol(name, position))
        | SyntaxExpr::UseSite(Expr::CapturedSymbol(name, _, position)) => {
            Expr::Symbol(name.clone(), *position)
        }
        SyntaxExpr::UseSite(expr) => expr.clone(),
        SyntaxExpr::Symbol(name, position) => Expr::Symbol(name.clone(), *position),
        SyntaxExpr::List(items, position) => Expr::List(
            items.iter().map(hygienize_binding_reference).collect(),
            *position,
        ),
    }
}

fn introduced_symbol(expr: Option<&SyntaxExpr>) -> Option<&str> {
    match expr {
        Some(SyntaxExpr::Symbol(name, _)) => Some(name),
        _ => None,
    }
}

fn is_core_syntax(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "if"
            | "quote"
            | "lambda"
            | "set!"
            | "and"
            | "or"
            | "begin"
            | "cond"
            | "let"
            | "else"
    )
}

fn extend_scope(scope: &mut HashMap<String, String>, additions: HashMap<String, String>) {
    for (name, fresh_name) in additions {
        scope.insert(name, fresh_name);
    }
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Symbol(name, _) | Expr::CapturedSymbol(name, _, _) => Some(name),
        _ => None,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name, _) if name == "...")
}

fn fresh_symbol(name: &str) -> String {
    let suffix = MACRO_GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__macro_{}_{}", sanitize_symbol(name), suffix)
}

fn sanitize_symbol(name: &str) -> String {
    let mut sanitized = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            sanitized.push(ch);
        } else {
            sanitized.push('_');
        }
    }

    if sanitized.is_empty() {
        sanitized.push_str("sym");
    }

    sanitized
}
