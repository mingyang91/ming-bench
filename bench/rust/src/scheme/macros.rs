use super::*;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub(super) struct MacroTransformer {
    pub(super) name: String,
    kind: MacroTransformerKind,
    pub(super) env: EnvRef,
}

#[derive(Clone)]
enum MacroTransformerKind {
    SyntaxRules {
        literals: Vec<String>,
        rules: Vec<MacroRule>,
    },
    SyntaxCase {
        literals: Vec<String>,
        clauses: Vec<SyntaxCaseClause>,
    },
}

impl MacroTransformer {
    fn literals(&self) -> &[String] {
        match &self.kind {
            MacroTransformerKind::SyntaxRules { literals, .. }
            | MacroTransformerKind::SyntaxCase { literals, .. } => literals,
        }
    }
}

#[derive(Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone)]
struct SyntaxCaseClause {
    pattern: Expr,
    fender: Option<Expr>,
    body: Expr,
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

struct PatternContext<'a> {
    macro_name: &'a str,
    literals: &'a [String],
    invoked_as: &'a str,
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

    match keyword.as_str() {
        "syntax-rules" => parse_syntax_rules_transformer(name, items, env),
        "lambda" => parse_syntax_case_transformer(name, items, env),
        _ => Err(EvalError::SyntaxError {
            message: "define-syntax requires syntax-rules or a syntax-case transformer lambda"
                .to_string(),
        }),
    }
}

fn parse_syntax_rules_transformer(
    name: &str,
    items: &[Expr],
    env: EnvRef,
) -> Result<Rc<MacroTransformer>, EvalError> {
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
        kind: MacroTransformerKind::SyntaxRules { literals, rules },
        env,
    }))
}

fn parse_syntax_case_transformer(
    name: &str,
    items: &[Expr],
    env: EnvRef,
) -> Result<Rc<MacroTransformer>, EvalError> {
    if items.len() != 3 {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer lambdas must contain exactly one body expression"
                .to_string(),
        });
    }

    let Expr::List(params) = &items[1] else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer lambdas require a single parameter".to_string(),
        });
    };

    if params.len() != 1 {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer lambdas require a single parameter".to_string(),
        });
    }

    let Expr::Symbol(parameter) = &params[0] else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer parameter must be a symbol".to_string(),
        });
    };

    parse_syntax_case_body(name, parameter, &items[2], env)
}

fn parse_syntax_case_body(
    name: &str,
    parameter: &str,
    body: &Expr,
    env: EnvRef,
) -> Result<Rc<MacroTransformer>, EvalError> {
    let Expr::List(items) = body else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer body must be a syntax-case form".to_string(),
        });
    };

    let Some(Expr::Symbol(keyword)) = items.first() else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer body must be a syntax-case form".to_string(),
        });
    };

    if keyword != "syntax-case" {
        return Err(EvalError::SyntaxError {
            message: "syntax-case transformer body must be a syntax-case form".to_string(),
        });
    }

    if items.len() < 4 {
        return Err(EvalError::SyntaxError {
            message: "syntax-case requires literals and at least one clause".to_string(),
        });
    }

    let Expr::Symbol(target) = &items[1] else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case must inspect the transformer parameter".to_string(),
        });
    };

    if target != parameter {
        return Err(EvalError::SyntaxError {
            message: "syntax-case must inspect the transformer parameter".to_string(),
        });
    }

    let literals = parse_literals(&items[2])?;
    let clauses = items[3..]
        .iter()
        .map(parse_syntax_case_clause)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Rc::new(MacroTransformer {
        name: name.to_string(),
        kind: MacroTransformerKind::SyntaxCase { literals, clauses },
        env,
    }))
}

fn parse_syntax_case_clause(expr: &Expr) -> Result<SyntaxCaseClause, EvalError> {
    let Expr::List(parts) = expr else {
        return Err(EvalError::SyntaxError {
            message: "syntax-case clauses must be lists".to_string(),
        });
    };

    match parts.len() {
        2 => Ok(SyntaxCaseClause {
            pattern: parts[0].clone(),
            fender: None,
            body: parts[1].clone(),
        }),
        3 => Ok(SyntaxCaseClause {
            pattern: parts[0].clone(),
            fender: Some(parts[1].clone()),
            body: parts[2].clone(),
        }),
        _ => Err(EvalError::SyntaxError {
            message: "syntax-case clauses must contain a pattern, an optional fender, and a body"
                .to_string(),
        }),
    }
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

    let expanded = match &transformer.kind {
        MacroTransformerKind::SyntaxRules { rules, .. } => {
            let Some((rule, bindings)) = rules.iter().find_map(|rule| {
                match_rule(rule, transformer.as_ref(), &invocation).map(|bindings| (rule, bindings))
            }) else {
                return Err(macro_error(
                    &transformer.name,
                    "no syntax-rules clause matched the macro call",
                ));
            };

            expand_template(&rule.template, &bindings, None, &transformer.name)?
        }
        MacroTransformerKind::SyntaxCase { clauses, .. } => {
            expand_syntax_case_call(transformer.as_ref(), clauses, &invocation)?
        }
    };
    let expansion_env = Env::macro_expansion(call_env);
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

fn expand_syntax_case_call(
    transformer: &MacroTransformer,
    clauses: &[SyntaxCaseClause],
    invocation: &SyntaxExpr,
) -> Result<SyntaxExpr, EvalError> {
    let invoked_as = invocation
        .as_list()
        .and_then(|items| items.first())
        .and_then(SyntaxExpr::symbol_name)
        .unwrap_or(transformer.name.as_str());
    let pattern_ctx = PatternContext {
        macro_name: transformer.name.as_str(),
        literals: transformer.literals(),
        invoked_as,
    };

    for clause in clauses {
        let mut bindings = PatternBindings::default();
        if !match_pattern(&clause.pattern, invocation, &pattern_ctx, &mut bindings) {
            continue;
        }

        if let Some(fender) = &clause.fender {
            let value = eval_transformer_expr(fender, &bindings, &transformer.name)?;
            if !value.is_truthy() {
                continue;
            }
        }

        return eval_transformer_syntax_expr(&clause.body, &bindings, &transformer.name);
    }

    Err(macro_error(
        &transformer.name,
        "no syntax-case clause matched the macro call",
    ))
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
    let pattern_ctx = PatternContext {
        macro_name: transformer.name.as_str(),
        literals: transformer.literals(),
        invoked_as,
    };
    let mut bindings = PatternBindings::default();
    match_pattern(&rule.pattern, invocation, &pattern_ctx, &mut bindings).then_some(bindings)
}

fn match_pattern(
    pattern: &Expr,
    value: &SyntaxExpr,
    pattern_ctx: &PatternContext<'_>,
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
            if is_pattern_literal(name, pattern_ctx) {
                let expected = if name == pattern_ctx.macro_name {
                    pattern_ctx.invoked_as
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
            let Some(result) =
                match_list_pattern(pattern_items, value_items, pattern_ctx, bindings)
            else {
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
    pattern_ctx: &PatternContext<'_>,
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
            let Some(trial) =
                match_repeated_pattern(repeated, &value_items[..count], pattern_ctx, &trial)
            else {
                continue;
            };
            if let Some(result) =
                match_list_pattern(rest, &value_items[count..], pattern_ctx, &trial)
            {
                return Some(result);
            }
        }

        return None;
    }

    let (value_head, value_tail) = value_items.split_first()?;
    let mut trial = bindings.clone();
    if !match_pattern(&pattern_items[0], value_head, pattern_ctx, &mut trial) {
        return None;
    }

    match_list_pattern(&pattern_items[1..], value_tail, pattern_ctx, &trial)
}

fn match_repeated_pattern(
    pattern: &Expr,
    values: &[SyntaxExpr],
    pattern_ctx: &PatternContext<'_>,
    bindings: &PatternBindings,
) -> Option<PatternBindings> {
    let mut names = HashSet::new();
    collect_pattern_variables(pattern, pattern_ctx, &mut names);

    let mut aggregated: HashMap<String, Vec<SyntaxExpr>> = names
        .iter()
        .map(|name| (name.clone(), Vec::new()))
        .collect();

    for value in values {
        let mut trial = PatternBindings::default();
        if !match_pattern(pattern, value, pattern_ctx, &mut trial) {
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
    pattern_ctx: &PatternContext<'_>,
    names: &mut HashSet<String>,
) {
    match pattern {
        Expr::Symbol(name) if !is_pattern_literal(name, pattern_ctx) => {
            names.insert(name.clone());
        }
        Expr::List(items) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_pattern_variables(item, pattern_ctx, names);
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

#[derive(Clone, PartialEq, Eq)]
enum TransformerValue {
    Number(Number),
    Boolean(bool),
    String(String),
    Char(char),
    Symbol(String),
    List(Vec<TransformerValue>),
    Syntax(SyntaxExpr),
}

impl TransformerValue {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Boolean(false))
    }

    fn type_name(&self) -> &'static str {
        match self {
            Self::Number(_) => "number",
            Self::Boolean(_) => "boolean",
            Self::String(_) => "string",
            Self::Char(_) => "char",
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::Syntax(_) => "syntax object",
        }
    }
}

fn eval_transformer_syntax_expr(
    expr: &Expr,
    bindings: &PatternBindings,
    macro_name: &str,
) -> Result<SyntaxExpr, EvalError> {
    let value = eval_transformer_expr(expr, bindings, macro_name)?;
    match value {
        TransformerValue::Syntax(syntax) => Ok(syntax),
        other => Err(macro_error(
            macro_name,
            &format!(
                "syntax-case clauses must produce a syntax object, got {}",
                other.type_name()
            ),
        )),
    }
}

fn eval_transformer_expr(
    expr: &Expr,
    bindings: &PatternBindings,
    macro_name: &str,
) -> Result<TransformerValue, EvalError> {
    match expr {
        Expr::Number(value) => Ok(TransformerValue::Number(*value)),
        Expr::Boolean(value) => Ok(TransformerValue::Boolean(*value)),
        Expr::String(value) => Ok(TransformerValue::String(value.clone())),
        Expr::Char(value) => Ok(TransformerValue::Char(*value)),
        Expr::Symbol(name) => match bindings.get(name) {
            Some(PatternBinding::One(value)) => Ok(TransformerValue::Syntax(value.clone())),
            Some(PatternBinding::Many(values)) => Ok(TransformerValue::List(
                values
                    .iter()
                    .cloned()
                    .map(TransformerValue::Syntax)
                    .collect(),
            )),
            None => Err(macro_error(
                macro_name,
                &format!("unbound transformer variable: {name}"),
            )),
        },
        Expr::List(items) => {
            if items.is_empty() {
                return Ok(TransformerValue::List(Vec::new()));
            }

            if let Expr::Symbol(operator) = &items[0] {
                match operator.as_str() {
                    "quote" => {
                        if items.len() != 2 {
                            return Err(macro_error(
                                macro_name,
                                "quote expects exactly one argument",
                            ));
                        }
                        return Ok(quote_transformer_datum(&items[1]));
                    }
                    "syntax" => {
                        if items.len() != 2 {
                            return Err(macro_error(
                                macro_name,
                                "syntax expects exactly one argument",
                            ));
                        }
                        return Ok(TransformerValue::Syntax(expand_template(
                            &items[1], bindings, None, macro_name,
                        )?));
                    }
                    "with-syntax" => {
                        return eval_transformer_with_syntax(items, bindings, macro_name);
                    }
                    "if" => {
                        if !(3..=4).contains(&items.len()) {
                            return Err(macro_error(
                                macro_name,
                                "if expects a test, a consequent, and an optional alternate",
                            ));
                        }

                        let condition = eval_transformer_expr(&items[1], bindings, macro_name)?;
                        if condition.is_truthy() {
                            return eval_transformer_expr(&items[2], bindings, macro_name);
                        }

                        return if items.len() == 4 {
                            eval_transformer_expr(&items[3], bindings, macro_name)
                        } else {
                            Ok(TransformerValue::Boolean(false))
                        };
                    }
                    "begin" => {
                        return eval_transformer_sequence(&items[1..], bindings, macro_name);
                    }
                    "and" => {
                        let mut last = TransformerValue::Boolean(true);
                        for item in &items[1..] {
                            let value = eval_transformer_expr(item, bindings, macro_name)?;
                            if !value.is_truthy() {
                                return Ok(value);
                            }
                            last = value;
                        }
                        return Ok(last);
                    }
                    "or" => {
                        for item in &items[1..] {
                            let value = eval_transformer_expr(item, bindings, macro_name)?;
                            if value.is_truthy() {
                                return Ok(value);
                            }
                        }
                        return Ok(TransformerValue::Boolean(false));
                    }
                    _ => {}
                }
            }

            let Expr::Symbol(operator) = &items[0] else {
                return Err(macro_error(
                    macro_name,
                    "transformer applications require a symbol operator",
                ));
            };

            let args = items[1..]
                .iter()
                .map(|item| eval_transformer_expr(item, bindings, macro_name))
                .collect::<Result<Vec<_>, _>>()?;
            apply_transformer_builtin(operator, &args, macro_name)
        }
    }
}

fn eval_transformer_with_syntax(
    items: &[Expr],
    bindings: &PatternBindings,
    macro_name: &str,
) -> Result<TransformerValue, EvalError> {
    if items.len() < 3 {
        return Err(macro_error(
            macro_name,
            "with-syntax requires bindings and a body",
        ));
    }

    let Expr::List(binding_specs) = &items[1] else {
        return Err(macro_error(
            macro_name,
            "with-syntax bindings must be a list",
        ));
    };

    let mut evaluated = Vec::with_capacity(binding_specs.len());
    for binding in binding_specs {
        let Expr::List(parts) = binding else {
            return Err(macro_error(
                macro_name,
                "with-syntax bindings must be pattern/expression pairs",
            ));
        };

        if parts.len() != 2 {
            return Err(macro_error(
                macro_name,
                "with-syntax bindings must contain a pattern and expression",
            ));
        }

        let syntax = eval_transformer_syntax_expr(&parts[1], bindings, macro_name)?;
        evaluated.push((parts[0].clone(), syntax));
    }

    let mut scoped_bindings = bindings.clone();
    for (pattern, syntax) in evaluated {
        scoped_bindings = match_syntax_binding(&pattern, &syntax, &scoped_bindings, macro_name)?;
    }

    eval_transformer_sequence(&items[2..], &scoped_bindings, macro_name)
}

fn eval_transformer_sequence(
    exprs: &[Expr],
    bindings: &PatternBindings,
    macro_name: &str,
) -> Result<TransformerValue, EvalError> {
    let mut last = TransformerValue::Boolean(false);
    for expr in exprs {
        last = eval_transformer_expr(expr, bindings, macro_name)?;
    }
    Ok(last)
}

fn match_syntax_binding(
    pattern: &Expr,
    syntax: &SyntaxExpr,
    bindings: &PatternBindings,
    macro_name: &str,
) -> Result<PatternBindings, EvalError> {
    let pattern_ctx = PatternContext {
        macro_name: "",
        literals: &[],
        invoked_as: "",
    };
    let mut scoped = bindings.clone();
    if match_pattern(pattern, syntax, &pattern_ctx, &mut scoped) {
        Ok(scoped)
    } else {
        Err(macro_error(
            macro_name,
            "with-syntax binding pattern did not match",
        ))
    }
}

fn quote_transformer_datum(expr: &Expr) -> TransformerValue {
    match expr {
        Expr::Number(value) => TransformerValue::Number(*value),
        Expr::Boolean(value) => TransformerValue::Boolean(*value),
        Expr::String(value) => TransformerValue::String(value.clone()),
        Expr::Char(value) => TransformerValue::Char(*value),
        Expr::Symbol(name) => TransformerValue::Symbol(name.clone()),
        Expr::List(items) => {
            TransformerValue::List(items.iter().map(quote_transformer_datum).collect())
        }
    }
}

fn syntax_to_datum(expr: &SyntaxExpr) -> TransformerValue {
    match expr {
        SyntaxExpr::Number(value) => TransformerValue::Number(*value),
        SyntaxExpr::Boolean(value) => TransformerValue::Boolean(*value),
        SyntaxExpr::String(value) => TransformerValue::String(value.clone()),
        SyntaxExpr::Char(value) => TransformerValue::Char(*value),
        SyntaxExpr::Symbol(symbol) => TransformerValue::Symbol(symbol.name.clone()),
        SyntaxExpr::List(items) => {
            TransformerValue::List(items.iter().map(syntax_to_datum).collect())
        }
    }
}

fn datum_to_syntax(
    datum: &TransformerValue,
    origin: SyntaxOrigin,
    macro_name: &str,
) -> Result<SyntaxExpr, EvalError> {
    match datum {
        TransformerValue::Number(value) => Ok(SyntaxExpr::Number(*value)),
        TransformerValue::Boolean(value) => Ok(SyntaxExpr::Boolean(*value)),
        TransformerValue::String(value) => Ok(SyntaxExpr::String(value.clone())),
        TransformerValue::Char(value) => Ok(SyntaxExpr::Char(*value)),
        TransformerValue::Symbol(name) => Ok(SyntaxExpr::Symbol(SyntaxSymbol {
            name: name.clone(),
            origin,
        })),
        TransformerValue::List(items) => Ok(SyntaxExpr::List(
            items
                .iter()
                .map(|item| datum_to_syntax(item, origin, macro_name))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        TransformerValue::Syntax(_) => Err(macro_error(
            macro_name,
            "datum->syntax expects a plain datum as its second argument",
        )),
    }
}

fn syntax_origin(expr: &SyntaxExpr) -> SyntaxOrigin {
    match expr {
        SyntaxExpr::Symbol(symbol) => symbol.origin,
        SyntaxExpr::List(items) => items
            .first()
            .map(syntax_origin)
            .unwrap_or(SyntaxOrigin::CallSite),
        _ => SyntaxOrigin::CallSite,
    }
}

fn expect_transformer_arity(
    name: &str,
    args: &[TransformerValue],
    expected: usize,
    macro_name: &str,
) -> Result<(), EvalError> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(macro_error(
            macro_name,
            &format!(
                "{name} expects {expected} argument{}, got {}",
                if expected == 1 { "" } else { "s" },
                args.len()
            ),
        ))
    }
}

fn expect_transformer_syntax<'a>(
    value: &'a TransformerValue,
    name: &str,
    macro_name: &str,
) -> Result<&'a SyntaxExpr, EvalError> {
    match value {
        TransformerValue::Syntax(syntax) => Ok(syntax),
        other => Err(macro_error(
            macro_name,
            &format!("{name} expects a syntax object, got {}", other.type_name()),
        )),
    }
}

fn expect_transformer_string<'a>(
    value: &'a TransformerValue,
    name: &str,
    macro_name: &str,
) -> Result<&'a str, EvalError> {
    match value {
        TransformerValue::String(text) => Ok(text),
        other => Err(macro_error(
            macro_name,
            &format!("{name} expects a string, got {}", other.type_name()),
        )),
    }
}

fn expect_transformer_symbol<'a>(
    value: &'a TransformerValue,
    name: &str,
    macro_name: &str,
) -> Result<&'a str, EvalError> {
    match value {
        TransformerValue::Symbol(symbol) => Ok(symbol),
        other => Err(macro_error(
            macro_name,
            &format!("{name} expects a symbol, got {}", other.type_name()),
        )),
    }
}

fn expect_transformer_number(
    value: &TransformerValue,
    name: &str,
    macro_name: &str,
) -> Result<Number, EvalError> {
    match value {
        TransformerValue::Number(number) => Ok(*number),
        other => Err(macro_error(
            macro_name,
            &format!("{name} expects a number, got {}", other.type_name()),
        )),
    }
}

fn expect_transformer_list<'a>(
    value: &'a TransformerValue,
    name: &str,
    macro_name: &str,
) -> Result<&'a [TransformerValue], EvalError> {
    match value {
        TransformerValue::List(items) => Ok(items),
        other => Err(macro_error(
            macro_name,
            &format!("{name} expects a list, got {}", other.type_name()),
        )),
    }
}

fn apply_transformer_builtin(
    name: &str,
    args: &[TransformerValue],
    macro_name: &str,
) -> Result<TransformerValue, EvalError> {
    match name {
        "syntax->datum" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(syntax_to_datum(expect_transformer_syntax(
                &args[0], name, macro_name,
            )?))
        }
        "datum->syntax" => {
            expect_transformer_arity(name, args, 2, macro_name)?;
            let context = expect_transformer_syntax(&args[0], name, macro_name)?;
            Ok(TransformerValue::Syntax(datum_to_syntax(
                &args[1],
                syntax_origin(context),
                macro_name,
            )?))
        }
        "string-append" => {
            let mut out = String::new();
            for arg in args {
                out.push_str(expect_transformer_string(arg, name, macro_name)?);
            }
            Ok(TransformerValue::String(out))
        }
        "string->symbol" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Symbol(
                expect_transformer_string(&args[0], name, macro_name)?.to_string(),
            ))
        }
        "symbol->string" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::String(
                expect_transformer_symbol(&args[0], name, macro_name)?.to_string(),
            ))
        }
        "number->string" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::String(
                expect_transformer_number(&args[0], name, macro_name)?.render(),
            ))
        }
        "identifier?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::Syntax(SyntaxExpr::Symbol(_))
            )))
        }
        "number?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::Number(_)
            )))
        }
        "boolean?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::Boolean(_)
            )))
        }
        "string?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::String(_)
            )))
        }
        "char?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::Char(_)
            )))
        }
        "symbol?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::Symbol(_)
            )))
        }
        "pair?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::List(items) if !items.is_empty()
            )))
        }
        "null?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::List(items) if items.is_empty()
            )))
        }
        "list?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(matches!(
                &args[0],
                TransformerValue::List(_)
            )))
        }
        "not" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(!args[0].is_truthy()))
        }
        "zero?" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            Ok(TransformerValue::Boolean(
                expect_transformer_number(&args[0], name, macro_name)?.is_zero(),
            ))
        }
        "list" => Ok(TransformerValue::List(args.to_vec())),
        "cons" => {
            expect_transformer_arity(name, args, 2, macro_name)?;
            let mut out = vec![args[0].clone()];
            out.extend(
                expect_transformer_list(&args[1], name, macro_name)?
                    .iter()
                    .cloned(),
            );
            Ok(TransformerValue::List(out))
        }
        "car" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            let list = expect_transformer_list(&args[0], name, macro_name)?;
            list.first()
                .cloned()
                .ok_or_else(|| macro_error(macro_name, "car expects a non-empty list"))
        }
        "cdr" => {
            expect_transformer_arity(name, args, 1, macro_name)?;
            let list = expect_transformer_list(&args[0], name, macro_name)?;
            if list.is_empty() {
                return Err(macro_error(macro_name, "cdr expects a non-empty list"));
            }
            Ok(TransformerValue::List(list[1..].to_vec()))
        }
        "eq?" | "eqv?" | "equal?" => {
            expect_transformer_arity(name, args, 2, macro_name)?;
            Ok(TransformerValue::Boolean(transformer_value_equal(
                &args[0], &args[1],
            )))
        }
        "=" | "<" | ">" | "<=" | ">=" => {
            if args.len() < 2 {
                return Ok(TransformerValue::Boolean(true));
            }

            let numbers = args
                .iter()
                .map(|arg| expect_transformer_number(arg, name, macro_name))
                .collect::<Result<Vec<_>, _>>()?;
            let ok = numbers.windows(2).all(|pair| match name {
                "=" => pair[0].numeric_eq(pair[1]),
                "<" => pair[0].compare(pair[1]).is_lt(),
                ">" => pair[0].compare(pair[1]).is_gt(),
                "<=" => !pair[0].compare(pair[1]).is_gt(),
                ">=" => !pair[0].compare(pair[1]).is_lt(),
                _ => unreachable!("comparison operator is matched above"),
            });
            Ok(TransformerValue::Boolean(ok))
        }
        "+" => {
            let mut total = Number::integer(0);
            for arg in args {
                total = total.add(expect_transformer_number(arg, name, macro_name)?)?;
            }
            Ok(TransformerValue::Number(total))
        }
        "-" => {
            if args.is_empty() {
                return Err(macro_error(macro_name, "- expects at least one argument"));
            }

            let mut total = expect_transformer_number(&args[0], name, macro_name)?;
            if args.len() == 1 {
                total = total.negate()?;
            } else {
                for arg in &args[1..] {
                    total = total.sub(expect_transformer_number(arg, name, macro_name)?)?;
                }
            }
            Ok(TransformerValue::Number(total))
        }
        _ => Err(macro_error(
            macro_name,
            &format!("unsupported transformer procedure: {name}"),
        )),
    }
}

fn transformer_value_equal(left: &TransformerValue, right: &TransformerValue) -> bool {
    match (left, right) {
        (TransformerValue::Number(left), TransformerValue::Number(right)) => {
            left.numeric_eq(*right)
        }
        (TransformerValue::Boolean(left), TransformerValue::Boolean(right)) => left == right,
        (TransformerValue::String(left), TransformerValue::String(right)) => left == right,
        (TransformerValue::Char(left), TransformerValue::Char(right)) => left == right,
        (TransformerValue::Symbol(left), TransformerValue::Symbol(right)) => left == right,
        (TransformerValue::List(left), TransformerValue::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| transformer_value_equal(left, right))
        }
        (TransformerValue::Syntax(left), TransformerValue::Syntax(right)) => left == right,
        _ => false,
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
    let mut body_scope = extend_scope(scope, param_scope);
    body_scope.extend(collect_body_define_scope(&items[2..], ctx));
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
            let (name_expr, _) = rename_binding_symbol_in_scope(symbol, scope, ctx);
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

            let (name_expr, renamed_name) = rename_binding_symbol_in_scope(name_symbol, scope, ctx);
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

fn collect_body_define_scope(
    body: &[SyntaxExpr],
    ctx: &mut EvalContext,
) -> HashMap<String, String> {
    let mut renamed = HashMap::new();

    for expr in body {
        let Some((original, fresh)) = body_define_rename(expr, ctx) else {
            continue;
        };
        renamed.entry(original).or_insert(fresh);
    }

    renamed
}

fn body_define_rename(expr: &SyntaxExpr, ctx: &mut EvalContext) -> Option<(String, String)> {
    let SyntaxExpr::List(items) = expr else {
        return None;
    };

    let Some(operator) = items.first().and_then(SyntaxExpr::symbol_name) else {
        return None;
    };

    if operator != "define" {
        return None;
    }

    match items.get(1) {
        Some(SyntaxExpr::Symbol(symbol)) => binding_rename_pair(symbol, ctx),
        Some(SyntaxExpr::List(signature)) if !signature.is_empty() => signature[0]
            .as_symbol()
            .and_then(|symbol| binding_rename_pair(symbol, ctx)),
        _ => None,
    }
}

fn binding_rename_pair(symbol: &SyntaxSymbol, ctx: &mut EvalContext) -> Option<(String, String)> {
    if symbol.origin == SyntaxOrigin::Template {
        Some((symbol.name.clone(), ctx.fresh_identifier(&symbol.name)))
    } else {
        None
    }
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

fn rename_binding_symbol_in_scope(
    symbol: &SyntaxSymbol,
    scope: &HashMap<String, String>,
    ctx: &mut EvalContext,
) -> (Expr, Option<(String, String)>) {
    if let Some(existing) = scope.get(&symbol.name) {
        return (Expr::Symbol(existing.clone()), None);
    }

    rename_binding_symbol(symbol, ctx)
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

fn is_pattern_literal(name: &str, pattern_ctx: &PatternContext<'_>) -> bool {
    name == "..."
        || (!pattern_ctx.macro_name.is_empty() && name == pattern_ctx.macro_name)
        || pattern_ctx.literals.iter().any(|literal| literal == name)
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
