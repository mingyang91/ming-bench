use super::*;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub(super) struct MacroTransformer {
    pub(super) name: String,
    literals: Vec<String>,
    rules: Vec<MacroRule>,
    pub(super) env: EnvRef,
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone, PartialEq, Eq)]
enum SyntaxExpr {
    Number(Number),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(SyntaxSymbol),
    List(Vec<SyntaxExpr>),
}

impl SyntaxExpr {
    fn from_expr(expr: &Expr, origin: SyntaxOrigin) -> Self {
        match expr {
            Expr::Number(value) => Self::Number(*value),
            Expr::Boolean(value) => Self::Boolean(*value),
            Expr::String(value) => Self::String(value.clone()),
            Expr::Char(value) => Self::Char(*value),
            Expr::Symbol(name) => Self::Symbol(SyntaxSymbol {
                name: name.clone(),
                origin,
            }),
            Expr::List(items) => Self::List(
                items
                    .iter()
                    .map(|item| Self::from_expr(item, origin))
                    .collect(),
            ),
        }
    }

    fn symbol_name(&self) -> Option<&str> {
        match self {
            Self::Symbol(symbol) => Some(symbol.name.as_str()),
            _ => None,
        }
    }

    fn as_symbol(&self) -> Option<&SyntaxSymbol> {
        match self {
            Self::Symbol(symbol) => Some(symbol),
            _ => None,
        }
    }

    fn as_list(&self) -> Option<&[SyntaxExpr]> {
        match self {
            Self::List(items) => Some(items),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct SyntaxSymbol {
    name: String,
    origin: SyntaxOrigin,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SyntaxOrigin {
    CallSite,
    Template,
}

#[derive(Clone, Default)]
struct PatternBindings {
    values: HashMap<String, PatternBinding>,
}

impl PatternBindings {
    fn bind_one(&mut self, name: &str, value: SyntaxExpr) -> bool {
        match self.values.get(name) {
            Some(PatternBinding::One(existing)) => existing == &value,
            Some(PatternBinding::Many(_)) => false,
            None => {
                self.values
                    .insert(name.to_string(), PatternBinding::One(value));
                true
            }
        }
    }

    fn bind_many(&mut self, name: &str, values: Vec<SyntaxExpr>) -> bool {
        match self.values.get(name) {
            Some(PatternBinding::Many(existing)) => existing == &values,
            Some(PatternBinding::One(_)) => false,
            None => {
                self.values
                    .insert(name.to_string(), PatternBinding::Many(values));
                true
            }
        }
    }

    fn get(&self, name: &str) -> Option<&PatternBinding> {
        self.values.get(name)
    }
}

#[derive(Clone)]
enum PatternBinding {
    One(SyntaxExpr),
    Many(Vec<SyntaxExpr>),
}

#[derive(Default)]
struct AliasCache {
    value_aliases: HashMap<String, String>,
    macro_aliases: HashMap<String, String>,
}

pub(super) fn parse_macro_transformer(
    name: &str,
    expr: &Expr,
    env: EnvRef,
) -> Result<Rc<MacroTransformer>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a syntax-rules transformer".to_string(),
        });
    };

    let Some(Expr::Symbol(keyword)) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "define-syntax requires a syntax-rules transformer".to_string(),
        });
    };

    if keyword != "syntax-rules" {
        return Err(EvalError::SyntaxError {
            message: "only syntax-rules transformers are supported".to_string(),
        });
    }

    if items.len() < 3 {
        return Err(EvalError::SyntaxError {
            message: "syntax-rules requires literals and at least one rule".to_string(),
        });
    }

    let literals = parse_literals(&items[1])?;
    let mut rules = Vec::with_capacity(items.len().saturating_sub(2));
    for rule in &items[2..] {
        let Expr::List(parts) = rule else {
            return Err(EvalError::SyntaxError {
                message: "syntax-rules clauses must be lists".to_string(),
            });
        };

        if parts.len() != 2 {
            return Err(EvalError::SyntaxError {
                message: "syntax-rules clauses must contain a pattern and template".to_string(),
            });
        }

        rules.push(MacroRule {
            pattern: parts[0].clone(),
            template: parts[1].clone(),
        });
    }

    Ok(Rc::new(MacroTransformer {
        name: name.to_string(),
        literals,
        rules,
        env,
    }))
}

pub(super) fn expand_macro_call(
    transformer: Rc<MacroTransformer>,
    items: &[Expr],
    call_env: EnvRef,
    ctx: &mut EvalContext,
) -> Result<(Expr, EnvRef), EvalError> {
    let invocation = SyntaxExpr::List(
        items
            .iter()
            .map(|item| SyntaxExpr::from_expr(item, SyntaxOrigin::CallSite))
            .collect(),
    );

    let Some((rule, bindings)) = transformer.rules.iter().find_map(|rule| {
        match_rule(rule, transformer.as_ref(), &invocation).map(|bindings| (rule, bindings))
    }) else {
        return Err(macro_error(
            &transformer.name,
            "no syntax-rules clause matched the macro call",
        ));
    };

    let expanded = expand_template(&rule.template, &bindings, None, &transformer.name)?;
    let expansion_env = Env::child(call_env);
    let mut aliases = AliasCache::default();
    let rewritten = hygienize_expr(
        &expanded,
        &transformer.env,
        &expansion_env,
        ctx,
        &HashMap::new(),
        &mut aliases,
        false,
    )?;

    Ok((rewritten, expansion_env))
}

fn parse_literals(expr: &Expr) -> Result<Vec<String>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::SyntaxError {
            message: "syntax-rules literals must be a list".to_string(),
        });
    };

    let mut literals = Vec::with_capacity(items.len());
    for item in items {
        let Expr::Symbol(name) = item else {
            return Err(EvalError::SyntaxError {
                message: "syntax-rules literals must be symbols".to_string(),
            });
        };
        literals.push(name.clone());
    }

    Ok(literals)
}

fn match_rule(
    rule: &MacroRule,
    transformer: &MacroTransformer,
    invocation: &SyntaxExpr,
) -> Option<PatternBindings> {
    let invoked_as = invocation
        .as_list()
        .and_then(|items| items.first())
        .and_then(SyntaxExpr::symbol_name)
        .unwrap_or(transformer.name.as_str());
    let mut bindings = PatternBindings::default();
    match_pattern(
        &rule.pattern,
        invocation,
        transformer,
        invoked_as,
        &mut bindings,
    )
    .then_some(bindings)
}

fn match_pattern(
    pattern: &Expr,
    value: &SyntaxExpr,
    transformer: &MacroTransformer,
    invoked_as: &str,
    bindings: &mut PatternBindings,
) -> bool {
    match pattern {
        Expr::Number(expected) => matches!(value, SyntaxExpr::Number(actual) if actual == expected),
        Expr::Boolean(expected) => {
            matches!(value, SyntaxExpr::Boolean(actual) if actual == expected)
        }
        Expr::String(expected) => matches!(value, SyntaxExpr::String(actual) if actual == expected),
        Expr::Char(expected) => matches!(value, SyntaxExpr::Char(actual) if actual == expected),
        Expr::Symbol(name) => {
            if is_pattern_literal(name, transformer) {
                let expected = if name == &transformer.name {
                    invoked_as
                } else {
                    name.as_str()
                };
                matches!(value, SyntaxExpr::Symbol(symbol) if symbol.name == expected)
            } else {
                bindings.bind_one(name, value.clone())
            }
        }
        Expr::List(pattern_items) => {
            let SyntaxExpr::List(value_items) = value else {
                return false;
            };
            let Some(result) = match_list_pattern(
                pattern_items,
                value_items,
                transformer,
                invoked_as,
                bindings,
            ) else {
                return false;
            };
            *bindings = result;
            true
        }
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    value_items: &[SyntaxExpr],
    transformer: &MacroTransformer,
    invoked_as: &str,
    bindings: &PatternBindings,
) -> Option<PatternBindings> {
    if pattern_items.is_empty() {
        return value_items.is_empty().then(|| bindings.clone());
    }

    if pattern_items.len() >= 2 && is_ellipsis(&pattern_items[1]) {
        let repeated = &pattern_items[0];
        let rest = &pattern_items[2..];

        for count in 0..=value_items.len() {
            let trial = bindings.clone();
            let Some(trial) = match_repeated_pattern(
                repeated,
                &value_items[..count],
                transformer,
                invoked_as,
                &trial,
            ) else {
                continue;
            };
            if let Some(result) =
                match_list_pattern(rest, &value_items[count..], transformer, invoked_as, &trial)
            {
                return Some(result);
            }
        }

        return None;
    }

    let (value_head, value_tail) = value_items.split_first()?;
    let mut trial = bindings.clone();
    if !match_pattern(
        &pattern_items[0],
        value_head,
        transformer,
        invoked_as,
        &mut trial,
    ) {
        return None;
    }

    match_list_pattern(
        &pattern_items[1..],
        value_tail,
        transformer,
        invoked_as,
        &trial,
    )
}

fn match_repeated_pattern(
    pattern: &Expr,
    values: &[SyntaxExpr],
    transformer: &MacroTransformer,
    invoked_as: &str,
    bindings: &PatternBindings,
) -> Option<PatternBindings> {
    let mut names = HashSet::new();
    collect_pattern_variables(pattern, transformer, &mut names);

    let mut aggregated: HashMap<String, Vec<SyntaxExpr>> = names
        .iter()
        .map(|name| (name.clone(), Vec::new()))
        .collect();

    for value in values {
        let mut trial = PatternBindings::default();
        if !match_pattern(pattern, value, transformer, invoked_as, &mut trial) {
            return None;
        }

        for (name, binding) in trial.values {
            let PatternBinding::One(expr) = binding else {
                return None;
            };
            aggregated.entry(name).or_default().push(expr);
        }
    }

    let mut result = bindings.clone();
    for name in names {
        let captured = aggregated.remove(&name).unwrap_or_default();
        if !result.bind_many(&name, captured) {
            return None;
        }
    }

    Some(result)
}

fn collect_pattern_variables(
    pattern: &Expr,
    transformer: &MacroTransformer,
    names: &mut HashSet<String>,
) {
    match pattern {
        Expr::Symbol(name) if !is_pattern_literal(name, transformer) => {
            names.insert(name.clone());
        }
        Expr::List(items) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_pattern_variables(item, transformer, names);
                }
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &PatternBindings,
    repetition: Option<usize>,
    macro_name: &str,
) -> Result<SyntaxExpr, EvalError> {
    match template {
        Expr::Number(value) => Ok(SyntaxExpr::Number(*value)),
        Expr::Boolean(value) => Ok(SyntaxExpr::Boolean(*value)),
        Expr::String(value) => Ok(SyntaxExpr::String(value.clone())),
        Expr::Char(value) => Ok(SyntaxExpr::Char(*value)),
        Expr::Symbol(name) => match bindings.get(name) {
            Some(PatternBinding::One(value)) => Ok(value.clone()),
            Some(PatternBinding::Many(values)) => {
                let Some(index) = repetition else {
                    return Err(macro_error(
                        macro_name,
                        "repeated pattern variables must appear under ellipsis in templates",
                    ));
                };

                values.get(index).cloned().ok_or_else(|| {
                    macro_error(macro_name, "template repetition index was out of bounds")
                })
            }
            None => Ok(SyntaxExpr::Symbol(SyntaxSymbol {
                name: name.clone(),
                origin: SyntaxOrigin::Template,
            })),
        },
        Expr::List(items) => {
            let mut out = Vec::new();
            let mut index = 0;

            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    let mut repeated_names = HashSet::new();
                    collect_repeated_template_names(&items[index], bindings, &mut repeated_names);

                    if repeated_names.is_empty() {
                        return Err(macro_error(
                            macro_name,
                            "ellipsis in templates requires at least one repeated pattern variable",
                        ));
                    }

                    let mut repeat_count = None;
                    for name in repeated_names {
                        let Some(PatternBinding::Many(values)) = bindings.get(&name) else {
                            continue;
                        };

                        match repeat_count {
                            Some(expected) if expected != values.len() => {
                                return Err(macro_error(
                                    macro_name,
                                    "template repetition variables had mismatched lengths",
                                ));
                            }
                            Some(_) => {}
                            None => repeat_count = Some(values.len()),
                        }
                    }

                    for repetition_index in 0..repeat_count.unwrap_or(0) {
                        out.push(expand_template(
                            &items[index],
                            bindings,
                            Some(repetition_index),
                            macro_name,
                        )?);
                    }

                    index += 2;
                    continue;
                }

                out.push(expand_template(
                    &items[index],
                    bindings,
                    repetition,
                    macro_name,
                )?);
                index += 1;
            }

            Ok(SyntaxExpr::List(out))
        }
    }
}

fn collect_repeated_template_names(
    template: &Expr,
    bindings: &PatternBindings,
    names: &mut HashSet<String>,
) {
    match template {
        Expr::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatternBinding::Many(_))) {
                names.insert(name.clone());
            }
        }
        Expr::List(items) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_repeated_template_names(item, bindings, names);
                }
            }
        }
        _ => {}
    }
}

fn hygienize_expr(
    expr: &SyntaxExpr,
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
    prefer_macro: bool,
) -> Result<Expr, EvalError> {
    match expr {
        SyntaxExpr::Number(value) => Ok(Expr::Number(*value)),
        SyntaxExpr::Boolean(value) => Ok(Expr::Boolean(*value)),
        SyntaxExpr::String(value) => Ok(Expr::String(value.clone())),
        SyntaxExpr::Char(value) => Ok(Expr::Char(*value)),
        SyntaxExpr::Symbol(symbol) => hygienize_symbol(
            symbol,
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
            prefer_macro,
        ),
        SyntaxExpr::List(items) => {
            hygienize_list(items, definition_env, expansion_env, ctx, scope, aliases)
        }
    }
}

fn hygienize_list(
    items: &[SyntaxExpr],
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<Expr, EvalError> {
    if let Some(operator) = items.first().and_then(SyntaxExpr::symbol_name) {
        match operator {
            "quote" => return Ok(lower_quoted_list(items)),
            "lambda" => {
                return hygienize_lambda(items, definition_env, expansion_env, ctx, scope, aliases);
            }
            "let" => {
                return hygienize_let(items, definition_env, expansion_env, ctx, scope, aliases);
            }
            "define" => {
                return hygienize_define(items, definition_env, expansion_env, ctx, scope, aliases);
            }
            _ => {}
        }
    }

    let mut out = Vec::with_capacity(items.len());
    for (index, item) in items.iter().enumerate() {
        out.push(hygienize_expr(
            item,
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
            index == 0,
        )?);
    }
    Ok(Expr::List(out))
}

fn hygienize_symbol(
    symbol: &SyntaxSymbol,
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
    prefer_macro: bool,
) -> Result<Expr, EvalError> {
    if symbol.origin == SyntaxOrigin::CallSite {
        return Ok(Expr::Symbol(symbol.name.clone()));
    }

    if let Some(renamed) = scope.get(&symbol.name) {
        return Ok(Expr::Symbol(renamed.clone()));
    }

    if symbol.name == "." || is_special_form_name(&symbol.name) {
        return Ok(Expr::Symbol(symbol.name.clone()));
    }

    if prefer_macro {
        if let Some(alias) = aliases.macro_aliases.get(&symbol.name) {
            return Ok(Expr::Symbol(alias.clone()));
        }

        if let Some(transformer) = lookup_macro(definition_env, &symbol.name) {
            let alias = ctx.fresh_identifier(&symbol.name);
            expansion_env
                .borrow_mut()
                .macros
                .insert(alias.clone(), transformer);
            aliases
                .macro_aliases
                .insert(symbol.name.clone(), alias.clone());
            return Ok(Expr::Symbol(alias));
        }
    }

    if let Some(alias) = aliases.value_aliases.get(&symbol.name) {
        return Ok(Expr::Symbol(alias.clone()));
    }

    if let Some(cell) = lookup_binding_cell_opt(definition_env, &symbol.name) {
        let alias = ctx.fresh_identifier(&symbol.name);
        expansion_env
            .borrow_mut()
            .bindings
            .insert(alias.clone(), cell);
        aliases
            .value_aliases
            .insert(symbol.name.clone(), alias.clone());
        return Ok(Expr::Symbol(alias));
    }

    Ok(Expr::Symbol(symbol.name.clone()))
}

fn hygienize_lambda(
    items: &[SyntaxExpr],
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return hygienize_fallback_list(items, definition_env, expansion_env, ctx, scope, aliases);
    }

    let (params, param_scope) = hygienize_parameter_list(&items[1], ctx)?;
    let body_scope = extend_scope(scope, param_scope);
    let mut out = vec![Expr::Symbol("lambda".to_string()), params];

    for item in &items[2..] {
        out.push(hygienize_expr(
            item,
            definition_env,
            expansion_env,
            ctx,
            &body_scope,
            aliases,
            false,
        )?);
    }

    Ok(Expr::List(out))
}

fn hygienize_let(
    items: &[SyntaxExpr],
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return hygienize_fallback_list(items, definition_env, expansion_env, ctx, scope, aliases);
    }

    let mut out = vec![Expr::Symbol("let".to_string())];
    let mut body_scope = scope.clone();
    let body_start;

    if let Some(name_symbol) = items[1].as_symbol() {
        let (name_expr, renamed) = rename_binding_symbol(name_symbol, ctx);
        out.push(name_expr);
        if let Some((original, fresh)) = renamed {
            body_scope.insert(original, fresh);
        }

        let (bindings, binding_scope) = hygienize_let_bindings(
            items
                .get(2)
                .ok_or_else(|| macro_error("let", "named let requires bindings"))?,
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
        )?;
        out.push(bindings);
        body_scope.extend(binding_scope);
        body_start = 3;
    } else {
        let (bindings, binding_scope) = hygienize_let_bindings(
            &items[1],
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
        )?;
        out.push(bindings);
        body_scope.extend(binding_scope);
        body_start = 2;
    }

    for item in &items[body_start..] {
        out.push(hygienize_expr(
            item,
            definition_env,
            expansion_env,
            ctx,
            &body_scope,
            aliases,
            false,
        )?);
    }

    Ok(Expr::List(out))
}

fn hygienize_define(
    items: &[SyntaxExpr],
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<Expr, EvalError> {
    if items.len() < 3 {
        return hygienize_fallback_list(items, definition_env, expansion_env, ctx, scope, aliases);
    }

    let mut out = vec![Expr::Symbol("define".to_string())];

    match &items[1] {
        SyntaxExpr::Symbol(symbol) => {
            let (name_expr, _) = rename_binding_symbol(symbol, ctx);
            out.push(name_expr);
            out.push(hygienize_expr(
                &items[2],
                definition_env,
                expansion_env,
                ctx,
                scope,
                aliases,
                false,
            )?);
        }
        SyntaxExpr::List(signature) if !signature.is_empty() => {
            let Some(name_symbol) = signature[0].as_symbol() else {
                return Err(macro_error(
                    "define",
                    "function definitions require a symbol name",
                ));
            };

            let (name_expr, renamed_name) = rename_binding_symbol(name_symbol, ctx);
            let params_syntax = SyntaxExpr::List(signature[1..].to_vec());
            let (params, param_scope) = hygienize_parameter_list(&params_syntax, ctx)?;
            let mut signature_exprs = vec![name_expr];
            let Expr::List(param_exprs) = params else {
                unreachable!("parameter list always lowers to a list");
            };
            signature_exprs.extend(param_exprs);
            out.push(Expr::List(signature_exprs));

            let mut body_scope = scope.clone();
            if let Some((original, fresh)) = renamed_name {
                body_scope.insert(original, fresh);
            }
            body_scope.extend(param_scope);

            for item in &items[2..] {
                out.push(hygienize_expr(
                    item,
                    definition_env,
                    expansion_env,
                    ctx,
                    &body_scope,
                    aliases,
                    false,
                )?);
            }
        }
        _ => {
            return Err(macro_error(
                "define",
                "macro expansion produced an invalid define form",
            ));
        }
    }

    Ok(Expr::List(out))
}

fn hygienize_let_bindings(
    expr: &SyntaxExpr,
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    let SyntaxExpr::List(bindings) = expr else {
        return Err(macro_error(
            "let",
            "macro expansion produced invalid let bindings",
        ));
    };

    let mut out = Vec::with_capacity(bindings.len());
    let mut renamed = HashMap::new();

    for binding in bindings {
        let SyntaxExpr::List(parts) = binding else {
            return Err(macro_error("let", "let bindings must be pairs"));
        };

        if parts.len() != 2 {
            return Err(macro_error(
                "let",
                "let bindings must contain a name and value",
            ));
        }

        let Some(name_symbol) = parts[0].as_symbol() else {
            return Err(macro_error("let", "let binding names must be symbols"));
        };

        let (name_expr, binding_rename) = rename_binding_symbol(name_symbol, ctx);
        if let Some((original, fresh)) = binding_rename {
            renamed.insert(original, fresh);
        }

        let value_expr = hygienize_expr(
            &parts[1],
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
            false,
        )?;
        out.push(Expr::List(vec![name_expr, value_expr]));
    }

    Ok((Expr::List(out), renamed))
}

fn hygienize_parameter_list(
    expr: &SyntaxExpr,
    ctx: &mut EvalContext,
) -> Result<(Expr, HashMap<String, String>), EvalError> {
    let SyntaxExpr::List(params) = expr else {
        return Err(macro_error(
            "lambda",
            "macro expansion produced an invalid parameter list",
        ));
    };

    let mut out = Vec::with_capacity(params.len());
    let mut renamed = HashMap::new();

    for param in params {
        let Some(symbol) = param.as_symbol() else {
            return Err(macro_error("lambda", "parameter names must be symbols"));
        };

        if symbol.name == "." {
            out.push(Expr::Symbol(".".to_string()));
            continue;
        }

        let (param_expr, rename) = rename_binding_symbol(symbol, ctx);
        if let Some((original, fresh)) = rename {
            renamed.insert(original, fresh);
        }
        out.push(param_expr);
    }

    Ok((Expr::List(out), renamed))
}

fn rename_binding_symbol(
    symbol: &SyntaxSymbol,
    ctx: &mut EvalContext,
) -> (Expr, Option<(String, String)>) {
    if symbol.origin == SyntaxOrigin::Template {
        let fresh = ctx.fresh_identifier(&symbol.name);
        (
            Expr::Symbol(fresh.clone()),
            Some((symbol.name.clone(), fresh)),
        )
    } else {
        (Expr::Symbol(symbol.name.clone()), None)
    }
}

fn hygienize_fallback_list(
    items: &[SyntaxExpr],
    definition_env: &EnvRef,
    expansion_env: &EnvRef,
    ctx: &mut EvalContext,
    scope: &HashMap<String, String>,
    aliases: &mut AliasCache,
) -> Result<Expr, EvalError> {
    let mut out = Vec::with_capacity(items.len());
    for (index, item) in items.iter().enumerate() {
        out.push(hygienize_expr(
            item,
            definition_env,
            expansion_env,
            ctx,
            scope,
            aliases,
            index == 0,
        )?);
    }
    Ok(Expr::List(out))
}

fn lower_quoted_list(items: &[SyntaxExpr]) -> Expr {
    let mut out = Vec::with_capacity(items.len());
    for (index, item) in items.iter().enumerate() {
        if index == 0 {
            out.push(Expr::Symbol("quote".to_string()));
        } else {
            out.push(lower_datum(item));
        }
    }
    Expr::List(out)
}

fn lower_datum(expr: &SyntaxExpr) -> Expr {
    match expr {
        SyntaxExpr::Number(value) => Expr::Number(*value),
        SyntaxExpr::Boolean(value) => Expr::Boolean(*value),
        SyntaxExpr::String(value) => Expr::String(value.clone()),
        SyntaxExpr::Char(value) => Expr::Char(*value),
        SyntaxExpr::Symbol(symbol) => Expr::Symbol(symbol.name.clone()),
        SyntaxExpr::List(items) => Expr::List(items.iter().map(lower_datum).collect()),
    }
}

fn extend_scope(
    base: &HashMap<String, String>,
    additions: HashMap<String, String>,
) -> HashMap<String, String> {
    let mut scope = base.clone();
    scope.extend(additions);
    scope
}

fn is_pattern_literal(name: &str, transformer: &MacroTransformer) -> bool {
    name == "..."
        || name == transformer.name
        || transformer.literals.iter().any(|literal| literal == name)
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(name) if name == "...")
}

fn macro_error(name: &str, message: &str) -> EvalError {
    EvalError::MacroError {
        name: name.to_string(),
        message: message.to_string(),
    }
}
