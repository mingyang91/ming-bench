use std::collections::{HashMap, HashSet};

use crate::scheme::error::{EvalError, EvalResult};
use crate::scheme::parser::{Expr, ExprKind, Span};

#[derive(Clone)]
pub(crate) struct SyntaxRulesMacro {
    name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone, Default)]
struct Bindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

pub(crate) fn parse_define_syntax(items: &[Expr], span: Span) -> EvalResult<(String, SyntaxRulesMacro)> {
    if items.len() != 3 {
        return Err(runtime_error(
            "define-syntax expects a name and transformer",
            span,
        ));
    }

    let name = expect_symbol(&items[1], "define-syntax name must be a symbol")?;
    let ExprKind::List(transformer) = &items[2].kind else {
        return Err(runtime_error(
            "define-syntax transformer must be a list",
            items[2].span,
        ));
    };
    if transformer.len() < 3 || symbol_name(&transformer[0]) != Some("syntax-rules") {
        return Err(runtime_error(
            "define-syntax only supports syntax-rules",
            items[2].span,
        ));
    }

    let ExprKind::List(literal_exprs) = &transformer[1].kind else {
        return Err(runtime_error(
            "syntax-rules literals must be a list",
            transformer[1].span,
        ));
    };

    let mut literals = HashSet::with_capacity(literal_exprs.len());
    for literal in literal_exprs {
        literals.insert(expect_symbol(literal, "syntax-rules literals must be symbols")?);
    }

    let mut rules = Vec::with_capacity(transformer.len() - 2);
    for rule_expr in &transformer[2..] {
        let ExprKind::List(rule_parts) = &rule_expr.kind else {
            return Err(runtime_error("syntax-rules clauses must be lists", rule_expr.span));
        };
        if rule_parts.len() != 2 {
            return Err(runtime_error(
                "syntax-rules clause expects a pattern and template",
                rule_expr.span,
            ));
        }
        rules.push(SyntaxRule {
            pattern: rule_parts[0].clone(),
            template: rule_parts[1].clone(),
        });
    }

    if rules.is_empty() {
        return Err(runtime_error(
            "syntax-rules expects at least one clause",
            items[2].span,
        ));
    }

    Ok((
        name.clone(),
        SyntaxRulesMacro {
            name,
            literals,
            rules,
        },
    ))
}

impl SyntaxRulesMacro {
    pub(crate) fn expand(&self, items: &[Expr], span: Span) -> EvalResult<Expr> {
        let invocation = Expr {
            kind: ExprKind::List(items.to_vec()),
            span,
        };

        for rule in &self.rules {
            let mut bindings = Bindings::default();
            if match_pattern(&rule.pattern, &invocation, self, &mut bindings) {
                return expand_template(&rule.template, &bindings, None);
            }
        }

        Err(runtime_error(
            format!("no matching syntax-rules clause for {}", self.name),
            span,
        ))
    }

    fn is_literal(&self, name: &str) -> bool {
        name == self.name || self.literals.contains(name)
    }
}

fn match_pattern(
    pattern: &Expr,
    value: &Expr,
    macro_rules: &SyntaxRulesMacro,
    bindings: &mut Bindings,
) -> bool {
    match (&pattern.kind, &value.kind) {
        (ExprKind::Symbol(name), ExprKind::Symbol(actual)) if macro_rules.is_literal(name) => {
            name == actual
        }
        (ExprKind::Symbol(name), _) if name != "..." => bindings.bind_single(name, value.clone()),
        (ExprKind::List(pattern_items), ExprKind::List(value_items)) => {
            match_list_pattern(pattern_items, value_items, macro_rules, bindings)
        }
        (ExprKind::Bool(left), ExprKind::Bool(right)) => left == right,
        (ExprKind::Int(left), ExprKind::Int(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::Quote(left), ExprKind::Quote(right)) => exprs_equal(left, right),
        _ => false,
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    value_items: &[Expr],
    macro_rules: &SyntaxRulesMacro,
    bindings: &mut Bindings,
) -> bool {
    let mut pattern_index = 0;
    let mut value_index = 0;

    while pattern_index < pattern_items.len() {
        let pattern_item = &pattern_items[pattern_index];
        if pattern_index + 1 < pattern_items.len() && is_ellipsis(&pattern_items[pattern_index + 1]) {
            if pattern_index + 2 != pattern_items.len() {
                return false;
            }
            return match_repeated_pattern(
                pattern_item,
                &value_items[value_index..],
                macro_rules,
                bindings,
            );
        }

        let Some(value_item) = value_items.get(value_index) else {
            return false;
        };
        if !match_pattern(pattern_item, value_item, macro_rules, bindings) {
            return false;
        }

        pattern_index += 1;
        value_index += 1;
    }

    value_index == value_items.len()
}

fn match_repeated_pattern(
    pattern: &Expr,
    values: &[Expr],
    macro_rules: &SyntaxRulesMacro,
    bindings: &mut Bindings,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(name) if !macro_rules.is_literal(name) && name != "..." => {
            bindings.bind_repeated(name, values.to_vec())
        }
        _ => false,
    }
}

fn expand_template(
    template: &Expr,
    bindings: &Bindings,
    repeat_index: Option<usize>,
) -> EvalResult<Expr> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(index) = repeat_index {
                if let Some(values) = bindings.repeated.get(name) {
                    return values.get(index).cloned().ok_or_else(|| {
                        runtime_error("macro repetition index out of range", template.span)
                    });
                }
            }

            if let Some(value) = bindings.single.get(name) {
                return Ok(value.clone());
            }

            if let Some(values) = bindings.repeated.get(name) {
                return match values.as_slice() {
                    [only] => Ok(only.clone()),
                    [] => Ok(template.clone()),
                    _ => Err(runtime_error(
                        format!("macro variable {name} used without ellipsis"),
                        template.span,
                    )),
                };
            }

            Ok(template.clone())
        }
        ExprKind::List(items) => {
            let mut expanded = Vec::new();
            let mut index = 0;
            while index < items.len() {
                if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                    let repetitions = repetition_count(&items[index], bindings, template.span)?;
                    for repetition in 0..repetitions {
                        expanded.push(expand_template(
                            &items[index],
                            bindings,
                            Some(repetition),
                        )?);
                    }
                    index += 2;
                    continue;
                }

                expanded.push(expand_template(&items[index], bindings, repeat_index)?);
                index += 1;
            }

            Ok(Expr {
                kind: ExprKind::List(expanded),
                span: template.span,
            })
        }
        ExprKind::Quote(inner) => Ok(Expr {
            kind: ExprKind::Quote(Box::new(expand_template(inner, bindings, repeat_index)?)),
            span: template.span,
        }),
        _ => Ok(template.clone()),
    }
}

impl Bindings {
    fn bind_single(&mut self, name: &str, value: Expr) -> bool {
        if let Some(existing) = self.single.get(name) {
            return exprs_equal(existing, &value);
        }
        self.single.insert(name.to_string(), value);
        true
    }

    fn bind_repeated(&mut self, name: &str, values: Vec<Expr>) -> bool {
        if let Some(existing) = self.repeated.get(name) {
            return existing.len() == values.len()
                && existing.iter().zip(&values).all(|(left, right)| exprs_equal(left, right));
        }
        self.repeated.insert(name.to_string(), values);
        true
    }
}

fn repetition_count(template: &Expr, bindings: &Bindings, span: Span) -> EvalResult<usize> {
    let mut names = Vec::new();
    collect_repeated_names(template, bindings, &mut names);
    names.sort();
    names.dedup();

    let Some(first) = names.first() else {
        return Err(runtime_error(
            "ellipsis template requires a repeated pattern variable",
            span,
        ));
    };

    let expected = bindings
        .repeated
        .get(first)
        .map(Vec::len)
        .unwrap_or_default();
    for name in &names[1..] {
        let len = bindings
            .repeated
            .get(name)
            .map(Vec::len)
            .unwrap_or_default();
        if len != expected {
            return Err(runtime_error(
                "macro repetition variables must have the same length",
                span,
            ));
        }
    }
    Ok(expected)
}

fn collect_repeated_names(template: &Expr, bindings: &Bindings, names: &mut Vec<String>) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if bindings.repeated.contains_key(name) {
                names.push(name.clone());
            }
        }
        ExprKind::List(items) => {
            for item in items {
                collect_repeated_names(item, bindings, names);
            }
        }
        ExprKind::Quote(inner) => collect_repeated_names(inner, bindings, names),
        _ => {}
    }
}

fn expect_symbol(expr: &Expr, message: &str) -> EvalResult<String> {
    let Some(name) = symbol_name(expr) else {
        return Err(runtime_error(message, expr.span));
    };
    Ok(name.to_string())
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name.as_str()),
        _ => None,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    symbol_name(expr) == Some("...")
}

fn exprs_equal(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Bool(left), ExprKind::Bool(right)) => left == right,
        (ExprKind::Int(left), ExprKind::Int(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Char(left), ExprKind::Char(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| exprs_equal(left, right))
        }
        (ExprKind::Quote(left), ExprKind::Quote(right)) => exprs_equal(left, right),
        _ => false,
    }
}

fn runtime_error(message: impl Into<String>, span: Span) -> EvalError {
    EvalError::with_position(message, span.line, span.col)
}
