use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::scheme::ast::{Expr, SourceLocation};
use crate::scheme::environment::Environment;
use crate::scheme::error::EvalError;

type Scope = HashMap<String, String>;

#[derive(Clone, Default)]
pub struct MacroEnvironment(Rc<RefCell<MacroState>>);

#[derive(Default)]
struct MacroState {
    transformers: HashMap<String, SyntaxRulesTransformer>,
    next_identifier: usize,
}

#[derive(Clone)]
struct SyntaxRulesTransformer {
    rules: Vec<SyntaxRule>,
    captured_aliases: HashMap<String, String>,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Pattern,
    template: Template,
}

#[derive(Clone)]
enum Pattern {
    Literal(Expr),
    Variable(String),
    List { items: Vec<PatternItem> },
}

#[derive(Clone)]
struct PatternItem {
    pattern: Pattern,
    repeated: bool,
}

#[derive(Clone)]
enum Template {
    Datum(Expr),
    Identifier {
        name: String,
        location: SourceLocation,
    },
    PatternVariable(String),
    List {
        items: Vec<TemplateItem>,
        location: SourceLocation,
    },
}

#[derive(Clone)]
struct TemplateItem {
    template: Template,
    repeated: bool,
}

#[derive(Default)]
struct MatchBindings {
    singles: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

impl MacroEnvironment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_macro(&self, name: &str) -> bool {
        self.0.borrow().transformers.contains_key(name)
    }

    pub fn define_syntax(
        &self,
        arguments: &[Expr],
        location: SourceLocation,
        environment: &Environment,
    ) -> Result<(), EvalError> {
        let [Expr::Symbol { name, .. }, transformer_expression] = arguments else {
            return Err(EvalError::MalformedSpecialForm {
                location,
                form: "define-syntax",
            });
        };

        let transformer =
            SyntaxRulesTransformer::compile(name, transformer_expression, environment, self)?;
        self.0
            .borrow_mut()
            .transformers
            .insert(name.clone(), transformer);
        Ok(())
    }

    pub fn expand_expression(&self, expression: &Expr) -> Result<Expr, EvalError> {
        expand_expression_recursive(expression, self)
    }

    fn expand_macro_call(&self, expression: &Expr) -> Result<Expr, EvalError> {
        let Expr::List { items, location } = expression else {
            return Ok(expression.clone());
        };
        let Some(Expr::Symbol { name, .. }) = items.first() else {
            return Ok(expression.clone());
        };

        let transformer = self
            .0
            .borrow()
            .transformers
            .get(name)
            .cloned()
            .ok_or_else(|| EvalError::MacroNoMatchingRule {
                location: *location,
                name: name.clone(),
            })?;
        transformer.expand(expression, self)
    }

    fn fresh_identifier(&self, source_name: &str) -> String {
        let mut state = self.0.borrow_mut();
        let index = state.next_identifier;
        state.next_identifier += 1;
        format!("__macro_{index}_{source_name}")
    }
}

impl SyntaxRulesTransformer {
    fn compile(
        macro_name: &str,
        expression: &Expr,
        environment: &Environment,
        macros: &MacroEnvironment,
    ) -> Result<Self, EvalError> {
        let Expr::List { items, location } = expression else {
            return Err(invalid_syntax_rules(
                expression.location(),
                "syntax-rules form must be a list",
            ));
        };
        let Some((head, tail)) = items.split_first() else {
            return Err(invalid_syntax_rules(
                *location,
                "syntax-rules form is empty",
            ));
        };
        let Expr::Symbol { name, .. } = head else {
            return Err(invalid_syntax_rules(
                head.location(),
                "syntax-rules form must start with syntax-rules",
            ));
        };
        if name != "syntax-rules" {
            return Err(invalid_syntax_rules(
                head.location(),
                "define-syntax requires syntax-rules",
            ));
        }
        let Some((literal_expression, rules)) = tail.split_first() else {
            return Err(invalid_syntax_rules(
                *location,
                "syntax-rules requires literals and at least one rule",
            ));
        };
        if rules.is_empty() {
            return Err(invalid_syntax_rules(
                *location,
                "syntax-rules requires at least one rule",
            ));
        }

        let mut literal_identifiers = parse_literal_identifiers(literal_expression)?;
        literal_identifiers.insert(macro_name.to_string());

        let compiled_rules = rules
            .iter()
            .map(|rule| compile_syntax_rule(rule, &literal_identifiers))
            .collect::<Result<Vec<_>, _>>()?;
        let captured_aliases =
            capture_definition_site_identifiers(&compiled_rules, environment, macros);

        Ok(Self {
            rules: compiled_rules,
            captured_aliases,
        })
    }

    fn expand(&self, expression: &Expr, macros: &MacroEnvironment) -> Result<Expr, EvalError> {
        let Some(bindings) = self.rules.iter().find_map(|rule| {
            match_pattern(&rule.pattern, expression).map(|bindings| (bindings, &rule.template))
        }) else {
            let location = expression.location();
            let name = expression
                .symbol_name()
                .map(str::to_string)
                .unwrap_or_else(|| "<macro>".into());
            return Err(EvalError::MacroNoMatchingRule { location, name });
        };
        let (bindings, template) = bindings;

        expand_template(
            template,
            &bindings,
            &self.captured_aliases,
            &HashMap::new(),
            macros,
            None,
        )
    }
}

impl Template {
    fn location(&self) -> SourceLocation {
        match self {
            Self::Datum(expression) => expression.location(),
            Self::Identifier { location, .. } | Self::List { location, .. } => *location,
            Self::PatternVariable(_) => SourceLocation::new(1, 1),
        }
    }
}

impl MatchBindings {
    fn bind_single(&mut self, name: &str, expression: &Expr) -> bool {
        let Some(existing) = self.singles.get(name) else {
            self.singles.insert(name.to_string(), expression.clone());
            return true;
        };

        same_datum(existing, expression)
    }

    fn merge_non_repeated(&mut self, other: Self) -> bool {
        if !other
            .singles
            .into_iter()
            .all(|(name, expression)| self.bind_single(&name, &expression))
        {
            return false;
        }

        other
            .repeated
            .into_iter()
            .all(|(name, expressions)| self.merge_repeated_binding(name, expressions))
    }

    fn merge_repetition(&mut self, other: Self) {
        other.singles.into_iter().for_each(|(name, expression)| {
            self.repeated.entry(name).or_default().push(expression);
        });
        other.repeated.into_iter().for_each(|(name, expressions)| {
            self.repeated.entry(name).or_default().extend(expressions);
        });
    }

    fn merge_repeated_binding(&mut self, name: String, expressions: Vec<Expr>) -> bool {
        let Some(existing) = self.repeated.get(&name) else {
            self.repeated.insert(name, expressions);
            return true;
        };

        same_datum_list(existing, &expressions)
    }
}

fn expand_expression_recursive(
    expression: &Expr,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    match expression {
        Expr::Integer { .. }
        | Expr::Boolean { .. }
        | Expr::String { .. }
        | Expr::Character { .. }
        | Expr::Symbol { .. } => Ok(expression.clone()),
        Expr::List { items, location } => {
            expand_list_expression_recursive(items, *location, macros)
        }
    }
}

fn expand_list_expression_recursive(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let Some(operator) = items.first() else {
        return Ok(Expr::list(Vec::new(), location));
    };
    let Some(name) = operator.symbol_name() else {
        return expand_application_items(items, location, macros);
    };

    if name == "quote" || name == "define-syntax" {
        return Ok(Expr::list(items.to_vec(), location));
    }
    if is_core_special_form(name) {
        return expand_special_form_expression(name, items, location, macros);
    }
    if macros.is_macro(name) {
        let expanded = macros.expand_macro_call(&Expr::list(items.to_vec(), location))?;
        return expand_expression_recursive(&expanded, macros);
    }

    expand_application_items(items, location, macros)
}

fn expand_special_form_expression(
    name: &str,
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    match name {
        "lambda" => expand_lambda_expression(items, location, macros),
        "define" => expand_define_expression(items, location, macros),
        "let" => expand_let_expression(items, location, macros),
        "cond" => expand_cond_expression(items, location, macros),
        "set!" => expand_set_expression(items, location, macros),
        "if" | "begin" | "and" | "or" => expand_operator_and_tail(items, location, macros),
        _ => expand_application_items(items, location, macros),
    }
}

fn expand_application_items(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    items
        .iter()
        .map(|item| expand_expression_recursive(item, macros))
        .collect::<Result<Vec<_>, _>>()
        .map(|items| Expr::list(items, location))
}

fn expand_operator_and_tail(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let Some((operator, tail)) = items.split_first() else {
        return Ok(Expr::list(Vec::new(), location));
    };
    tail.iter()
        .map(|item| expand_expression_recursive(item, macros))
        .collect::<Result<Vec<_>, _>>()
        .map(|mut expanded_tail| {
            let mut expanded_items = vec![operator.clone()];
            expanded_items.append(&mut expanded_tail);
            Expr::list(expanded_items, location)
        })
}

fn expand_lambda_expression(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let [operator, parameters, body @ ..] = items else {
        return Ok(Expr::list(items.to_vec(), location));
    };

    body.iter()
        .map(|expression| expand_expression_recursive(expression, macros))
        .collect::<Result<Vec<_>, _>>()
        .map(|expanded_body| {
            let mut expanded_items = vec![operator.clone(), parameters.clone()];
            expanded_items.extend(expanded_body);
            Expr::list(expanded_items, location)
        })
}

fn expand_define_expression(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    match items {
        [operator, name @ Expr::Symbol { .. }, expression] => {
            expand_expression_recursive(expression, macros).map(|expanded| {
                Expr::list(vec![operator.clone(), name.clone(), expanded], location)
            })
        }
        [operator, signature @ Expr::List { .. }, body @ ..] => body
            .iter()
            .map(|expression| expand_expression_recursive(expression, macros))
            .collect::<Result<Vec<_>, _>>()
            .map(|expanded_body| {
                let mut expanded_items = vec![operator.clone(), signature.clone()];
                expanded_items.extend(expanded_body);
                Expr::list(expanded_items, location)
            }),
        _ => Ok(Expr::list(items.to_vec(), location)),
    }
}

fn expand_let_expression(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    match items {
        [operator, name @ Expr::Symbol { .. }, bindings, body @ ..] => {
            let expanded_bindings = expand_let_bindings_expression(bindings, macros)?;
            let expanded_body = body
                .iter()
                .map(|expression| expand_expression_recursive(expression, macros))
                .collect::<Result<Vec<_>, _>>()?;
            let mut expanded_items = vec![operator.clone(), name.clone(), expanded_bindings];
            expanded_items.extend(expanded_body);
            Ok(Expr::list(expanded_items, location))
        }
        [operator, bindings, body @ ..] => {
            let expanded_bindings = expand_let_bindings_expression(bindings, macros)?;
            let expanded_body = body
                .iter()
                .map(|expression| expand_expression_recursive(expression, macros))
                .collect::<Result<Vec<_>, _>>()?;
            let mut expanded_items = vec![operator.clone(), expanded_bindings];
            expanded_items.extend(expanded_body);
            Ok(Expr::list(expanded_items, location))
        }
        _ => Ok(Expr::list(items.to_vec(), location)),
    }
}

fn expand_let_bindings_expression(
    bindings: &Expr,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let Expr::List { items, location } = bindings else {
        return Ok(bindings.clone());
    };

    items
        .iter()
        .map(|binding| expand_let_binding_expression(binding, macros))
        .collect::<Result<Vec<_>, _>>()
        .map(|bindings| Expr::list(bindings, *location))
}

fn expand_let_binding_expression(
    binding: &Expr,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let Expr::List { items, location } = binding else {
        return Ok(binding.clone());
    };
    let [name, expression] = items.as_slice() else {
        return Ok(binding.clone());
    };

    expand_expression_recursive(expression, macros)
        .map(|expanded| Expr::list(vec![name.clone(), expanded], *location))
}

fn expand_cond_expression(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let Some((operator, clauses)) = items.split_first() else {
        return Ok(Expr::list(Vec::new(), location));
    };
    let expanded_clauses = clauses
        .iter()
        .map(|clause| expand_cond_clause(clause, macros))
        .collect::<Result<Vec<_>, _>>()?;

    let mut expanded_items = vec![operator.clone()];
    expanded_items.extend(expanded_clauses);
    Ok(Expr::list(expanded_items, location))
}

fn expand_cond_clause(clause: &Expr, macros: &MacroEnvironment) -> Result<Expr, EvalError> {
    let Expr::List { items, location } = clause else {
        return Ok(clause.clone());
    };

    items
        .iter()
        .map(|item| expand_expression_recursive(item, macros))
        .collect::<Result<Vec<_>, _>>()
        .map(|items| Expr::list(items, *location))
}

fn expand_set_expression(
    items: &[Expr],
    location: SourceLocation,
    macros: &MacroEnvironment,
) -> Result<Expr, EvalError> {
    let [operator, name, expression] = items else {
        return Ok(Expr::list(items.to_vec(), location));
    };

    expand_expression_recursive(expression, macros)
        .map(|expanded| Expr::list(vec![operator.clone(), name.clone(), expanded], location))
}

fn compile_syntax_rule(
    rule_expression: &Expr,
    literal_identifiers: &HashSet<String>,
) -> Result<SyntaxRule, EvalError> {
    let Expr::List { items, location } = rule_expression else {
        return Err(invalid_syntax_rules(
            rule_expression.location(),
            "syntax-rules rule must be a list",
        ));
    };
    let [pattern_expression, template_expression] = items.as_slice() else {
        return Err(invalid_syntax_rules(
            *location,
            "syntax-rules rule must contain a pattern and template",
        ));
    };

    let mut pattern_variables = HashSet::new();
    let pattern = compile_pattern(
        pattern_expression,
        literal_identifiers,
        &mut pattern_variables,
    )?;
    let template = compile_template(template_expression, &pattern_variables)?;

    Ok(SyntaxRule { pattern, template })
}

fn compile_pattern(
    expression: &Expr,
    literal_identifiers: &HashSet<String>,
    pattern_variables: &mut HashSet<String>,
) -> Result<Pattern, EvalError> {
    if is_quote_form(expression) {
        return Ok(Pattern::Literal(expression.clone()));
    }

    match expression {
        Expr::Integer { .. }
        | Expr::Boolean { .. }
        | Expr::String { .. }
        | Expr::Character { .. } => Ok(Pattern::Literal(expression.clone())),
        Expr::Symbol { name, .. } if name == "..." => Err(invalid_syntax_rules(
            expression.location(),
            "ellipsis must follow a pattern",
        )),
        Expr::Symbol { name, .. } if literal_identifiers.contains(name) => {
            Ok(Pattern::Literal(expression.clone()))
        }
        Expr::Symbol { name, .. } => {
            pattern_variables.insert(name.clone());
            Ok(Pattern::Variable(name.clone()))
        }
        Expr::List { items, location } => {
            compile_pattern_items(items, *location, literal_identifiers, pattern_variables)
        }
    }
}

fn compile_pattern_items(
    items: &[Expr],
    location: SourceLocation,
    literal_identifiers: &HashSet<String>,
    pattern_variables: &mut HashSet<String>,
) -> Result<Pattern, EvalError> {
    compile_repeated_items(items, location, |expression| {
        compile_pattern(expression, literal_identifiers, pattern_variables)
    })
    .map(|items| Pattern::List {
        items: items.into_iter().map(Into::into).collect(),
    })
}

fn compile_template(
    expression: &Expr,
    pattern_variables: &HashSet<String>,
) -> Result<Template, EvalError> {
    if is_quote_form(expression) {
        return Ok(Template::Datum(expression.clone()));
    }

    match expression {
        Expr::Integer { .. }
        | Expr::Boolean { .. }
        | Expr::String { .. }
        | Expr::Character { .. } => Ok(Template::Datum(expression.clone())),
        Expr::Symbol { name, .. } if name == "..." => Err(invalid_syntax_rules(
            expression.location(),
            "ellipsis must follow a template",
        )),
        Expr::Symbol { name, .. } if pattern_variables.contains(name) => {
            Ok(Template::PatternVariable(name.clone()))
        }
        Expr::Symbol { name, location } => Ok(Template::Identifier {
            name: name.clone(),
            location: *location,
        }),
        Expr::List { items, location } => {
            compile_template_items(items, *location, pattern_variables)
        }
    }
}

fn compile_template_items(
    items: &[Expr],
    location: SourceLocation,
    pattern_variables: &HashSet<String>,
) -> Result<Template, EvalError> {
    compile_repeated_items(items, location, |expression| {
        compile_template(expression, pattern_variables)
    })
    .map(|items| Template::List {
        items: items.into_iter().map(Into::into).collect(),
        location,
    })
}

fn compile_repeated_items<CompiledItem>(
    items: &[Expr],
    location: SourceLocation,
    mut compile: impl FnMut(&Expr) -> Result<CompiledItem, EvalError>,
) -> Result<Vec<CompiledItemWithRepeat<CompiledItem>>, EvalError> {
    let mut index = 0usize;
    let mut compiled_items = Vec::new();

    while let Some(item) = items.get(index) {
        if is_ellipsis(item) {
            return Err(invalid_syntax_rules(
                item.location(),
                "ellipsis must follow another form",
            ));
        }

        let repeated = items.get(index + 1).is_some_and(is_ellipsis);
        compiled_items.push(CompiledItemWithRepeat {
            item: compile(item)?,
            repeated,
        });
        index += if repeated { 2 } else { 1 };
    }

    if compiled_items.is_empty() && !items.is_empty() {
        return Err(invalid_syntax_rules(
            location,
            "invalid repeated form structure",
        ));
    }

    Ok(compiled_items)
}

struct CompiledItemWithRepeat<T> {
    item: T,
    repeated: bool,
}

impl From<CompiledItemWithRepeat<Pattern>> for PatternItem {
    fn from(value: CompiledItemWithRepeat<Pattern>) -> Self {
        Self {
            pattern: value.item,
            repeated: value.repeated,
        }
    }
}

impl From<CompiledItemWithRepeat<Template>> for TemplateItem {
    fn from(value: CompiledItemWithRepeat<Template>) -> Self {
        Self {
            template: value.item,
            repeated: value.repeated,
        }
    }
}

fn capture_definition_site_identifiers(
    rules: &[SyntaxRule],
    environment: &Environment,
    macros: &MacroEnvironment,
) -> HashMap<String, String> {
    let mut free_identifiers = HashSet::new();
    rules.iter().for_each(|rule| {
        collect_free_identifiers(&rule.template, &HashSet::new(), &mut free_identifiers);
    });

    free_identifiers
        .into_iter()
        .filter(|name| !is_core_special_form(name))
        .filter(|name| environment.lookup(name).is_some())
        .map(|name| {
            let alias = macros.fresh_identifier(&name);
            let aliased = environment.define_alias(alias.clone(), &name);
            debug_assert!(
                aliased,
                "captured identifier should exist at definition site"
            );
            (name, alias)
        })
        .collect()
}

fn collect_free_identifiers(
    template: &Template,
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    match template {
        Template::Datum(_) | Template::PatternVariable(_) => {}
        Template::Identifier { name, .. } => {
            if !bound_identifiers.contains(name) && !is_core_special_form(name) {
                free_identifiers.insert(name.clone());
            }
        }
        Template::List { items, .. } => {
            collect_free_identifiers_from_list(items, bound_identifiers, free_identifiers);
        }
    }
}

fn collect_free_identifiers_from_list(
    items: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    let Some(operator) = items.first() else {
        return;
    };
    let Template::Identifier { name, .. } = &operator.template else {
        collect_free_identifiers_in_items(items, bound_identifiers, free_identifiers);
        return;
    };

    match name.as_str() {
        "lambda" => collect_lambda_identifiers(items, bound_identifiers, free_identifiers),
        "let" => collect_let_identifiers(items, bound_identifiers, free_identifiers),
        "define" => collect_define_identifiers(items, bound_identifiers, free_identifiers),
        _ => collect_free_identifiers_in_items(items, bound_identifiers, free_identifiers),
    }
}

fn collect_free_identifiers_in_items(
    items: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    items.iter().for_each(|item| {
        collect_free_identifiers(&item.template, bound_identifiers, free_identifiers);
    });
}

fn collect_lambda_identifiers(
    items: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    let [_, parameters, body @ ..] = items else {
        collect_free_identifiers_in_items(items, bound_identifiers, free_identifiers);
        return;
    };

    let mut body_bindings = bound_identifiers.clone();
    collect_binder_names(&parameters.template, &mut body_bindings);
    collect_free_identifiers_in_items(body, &body_bindings, free_identifiers);
}

fn collect_let_identifiers(
    items: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    match items {
        [_, name, bindings, body @ ..] if !matches!(name.template, Template::List { .. }) => {
            collect_free_identifiers(&bindings.template, bound_identifiers, free_identifiers);
            let mut body_bindings = bound_identifiers.clone();
            collect_binder_names(&name.template, &mut body_bindings);
            collect_let_binding_names(
                &bindings.template,
                &mut body_bindings,
                free_identifiers,
                bound_identifiers,
            );
            collect_free_identifiers_in_items(body, &body_bindings, free_identifiers);
        }
        [_, bindings, body @ ..] => {
            let mut body_bindings = bound_identifiers.clone();
            collect_let_binding_names(
                &bindings.template,
                &mut body_bindings,
                free_identifiers,
                bound_identifiers,
            );
            collect_free_identifiers_in_items(body, &body_bindings, free_identifiers);
        }
        _ => collect_free_identifiers_in_items(items, bound_identifiers, free_identifiers),
    }
}

fn collect_let_binding_names(
    bindings: &Template,
    body_bindings: &mut HashSet<String>,
    free_identifiers: &mut HashSet<String>,
    outer_bindings: &HashSet<String>,
) {
    let Template::List { items, .. } = bindings else {
        collect_free_identifiers(bindings, outer_bindings, free_identifiers);
        return;
    };

    items.iter().for_each(|binding| {
        collect_let_binding_name(binding, body_bindings, free_identifiers, outer_bindings);
    });
}

fn collect_let_binding_name(
    binding: &TemplateItem,
    body_bindings: &mut HashSet<String>,
    free_identifiers: &mut HashSet<String>,
    outer_bindings: &HashSet<String>,
) {
    let Template::List { items, .. } = &binding.template else {
        collect_free_identifiers(&binding.template, outer_bindings, free_identifiers);
        return;
    };
    let [name, expression] = items.as_slice() else {
        collect_free_identifiers_in_items(items, outer_bindings, free_identifiers);
        return;
    };

    collect_binder_names(&name.template, body_bindings);
    collect_free_identifiers(&expression.template, outer_bindings, free_identifiers);
}

fn collect_define_identifiers(
    items: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    match items {
        [_, name, expression] => collect_define_expression_identifiers(
            &name.template,
            &expression.template,
            bound_identifiers,
            free_identifiers,
        ),
        [_, name, body @ ..] if matches!(name.template, Template::List { .. }) => {
            collect_define_body_identifiers(
                &name.template,
                body,
                bound_identifiers,
                free_identifiers,
            );
        }
        _ => collect_free_identifiers_in_items(items, bound_identifiers, free_identifiers),
    }
}

fn collect_define_expression_identifiers(
    name: &Template,
    expression: &Template,
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    let Template::List {
        items: signature, ..
    } = name
    else {
        collect_free_identifiers(expression, bound_identifiers, free_identifiers);
        return;
    };
    let Some(function_bindings) = collect_function_bindings(signature, bound_identifiers) else {
        collect_free_identifiers(name, bound_identifiers, free_identifiers);
        collect_free_identifiers(expression, bound_identifiers, free_identifiers);
        return;
    };

    collect_free_identifiers(expression, &function_bindings, free_identifiers);
}

fn collect_define_body_identifiers(
    name: &Template,
    body: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
    free_identifiers: &mut HashSet<String>,
) {
    let Template::List {
        items: signature, ..
    } = name
    else {
        return;
    };
    let Some(function_bindings) = collect_function_bindings(signature, bound_identifiers) else {
        return;
    };

    collect_free_identifiers_in_items(body, &function_bindings, free_identifiers);
}

fn collect_function_bindings(
    signature: &[TemplateItem],
    bound_identifiers: &HashSet<String>,
) -> Option<HashSet<String>> {
    let (function_name, parameters) = signature.split_first()?;
    let mut function_bindings = bound_identifiers.clone();
    collect_binder_names(&function_name.template, &mut function_bindings);
    parameters.iter().for_each(|parameter| {
        collect_binder_names(&parameter.template, &mut function_bindings);
    });
    Some(function_bindings)
}

fn collect_binder_names(template: &Template, bound_identifiers: &mut HashSet<String>) {
    match template {
        Template::Identifier { name, .. } if name != "." => {
            bound_identifiers.insert(name.clone());
        }
        Template::List { items, .. } => {
            items.iter().filter(|item| !item.repeated).for_each(|item| {
                collect_binder_names(&item.template, bound_identifiers);
            });
        }
        Template::Datum(_) | Template::PatternVariable(_) | Template::Identifier { .. } => {}
    }
}

fn parse_literal_identifiers(expression: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List { items, .. } = expression else {
        return Err(invalid_syntax_rules(
            expression.location(),
            "syntax-rules literals must be a list",
        ));
    };

    items
        .iter()
        .map(|item| match item {
            Expr::Symbol { name, .. } => Ok(name.clone()),
            _ => Err(invalid_syntax_rules(
                item.location(),
                "syntax-rules literals must be identifiers",
            )),
        })
        .collect()
}

fn match_pattern(pattern: &Pattern, expression: &Expr) -> Option<MatchBindings> {
    match pattern {
        Pattern::Literal(literal) => same_datum(literal, expression).then(MatchBindings::default),
        Pattern::Variable(name) => {
            let mut bindings = MatchBindings::default();
            bindings.bind_single(name, expression).then_some(bindings)
        }
        Pattern::List { items } => {
            let Expr::List {
                items: expressions, ..
            } = expression
            else {
                return None;
            };
            match_pattern_items(items, expressions)
        }
    }
}

fn match_pattern_items(items: &[PatternItem], expressions: &[Expr]) -> Option<MatchBindings> {
    match items {
        [] => expressions.is_empty().then(MatchBindings::default),
        [PatternItem {
            pattern,
            repeated: false,
        }, rest @ ..] => {
            let (expression, remaining_expressions) = expressions.split_first()?;
            let mut bindings = match_pattern(pattern, expression)?;
            let remaining_bindings = match_pattern_items(rest, remaining_expressions)?;
            bindings
                .merge_non_repeated(remaining_bindings)
                .then_some(bindings)
        }
        [PatternItem {
            pattern,
            repeated: true,
        }, rest @ ..] => match_repeated_pattern_item(pattern, rest, expressions),
    }
}

fn match_repeated_pattern_item(
    pattern: &Pattern,
    remaining_items: &[PatternItem],
    expressions: &[Expr],
) -> Option<MatchBindings> {
    let minimum_remaining = minimum_pattern_items(remaining_items);
    if expressions.len() < minimum_remaining {
        return None;
    }

    (0..=expressions.len() - minimum_remaining).find_map(|repetition_count| {
        let (repeated_expressions, remaining_expressions) = expressions.split_at(repetition_count);
        let mut bindings = MatchBindings::default();
        initialize_repeated_bindings(pattern, &mut bindings);
        match_repeated_expressions(pattern, repeated_expressions, &mut bindings)?;

        let remaining_bindings = match_pattern_items(remaining_items, remaining_expressions)?;
        bindings
            .merge_non_repeated(remaining_bindings)
            .then_some(bindings)
    })
}

fn match_repeated_expressions(
    pattern: &Pattern,
    repeated_expressions: &[Expr],
    bindings: &mut MatchBindings,
) -> Option<()> {
    repeated_expressions.iter().try_for_each(|expression| {
        let local_bindings = match_pattern(pattern, expression)?;
        bindings.merge_repetition(local_bindings);
        Some(())
    })
}

fn minimum_pattern_items(items: &[PatternItem]) -> usize {
    items.iter().filter(|item| !item.repeated).count()
}

fn initialize_repeated_bindings(pattern: &Pattern, bindings: &mut MatchBindings) {
    repeated_pattern_variables(pattern)
        .into_iter()
        .for_each(|name| {
            bindings.repeated.entry(name).or_default();
        });
}

fn repeated_pattern_variables(pattern: &Pattern) -> HashSet<String> {
    let mut variables = HashSet::new();
    collect_pattern_variables(pattern, &mut variables);
    variables
}

fn collect_pattern_variables(pattern: &Pattern, variables: &mut HashSet<String>) {
    match pattern {
        Pattern::Literal(_) => {}
        Pattern::Variable(name) => {
            variables.insert(name.clone());
        }
        Pattern::List { items } => {
            items.iter().for_each(|item| {
                collect_pattern_variables(&item.pattern, variables);
            });
        }
    }
}

fn expand_template(
    template: &Template,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match template {
        Template::Datum(expression) => Ok(expression.clone()),
        Template::PatternVariable(name) => {
            expand_pattern_variable(name, bindings, template.location(), repetition_index)
        }
        Template::Identifier { name, location } => {
            let expanded_name = scope
                .get(name)
                .cloned()
                .or_else(|| captured_aliases.get(name).cloned())
                .unwrap_or_else(|| name.clone());
            Ok(Expr::symbol(expanded_name, *location))
        }
        Template::List { items, location } => expand_template_list(
            items,
            *location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        ),
    }
}

fn expand_pattern_variable(
    name: &str,
    bindings: &MatchBindings,
    location: SourceLocation,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    if let Some(index) = repetition_index {
        let Some(values) = bindings.repeated.get(name) else {
            return Err(invalid_syntax_rules(
                location,
                "ellipsis template requires a repeated pattern variable",
            ));
        };
        return Ok(values[index].clone());
    }

    bindings.singles.get(name).cloned().ok_or_else(|| {
        invalid_syntax_rules(
            location,
            "pattern variable used outside its ellipsis context",
        )
    })
}

fn expand_template_list(
    items: &[TemplateItem],
    location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some(operator) = items.first() else {
        return Ok(Expr::list(Vec::new(), location));
    };
    let Template::Identifier { name, .. } = &operator.template else {
        return expand_general_template_list(
            items,
            location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        );
    };

    match name.as_str() {
        "lambda" => expand_lambda_template(
            items,
            location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        ),
        "let" => expand_let_template(
            items,
            location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        ),
        "define" => expand_define_template(
            items,
            location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        ),
        _ => expand_general_template_list(
            items,
            location,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        ),
    }
}

fn expand_general_template_list(
    items: &[TemplateItem],
    location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    expand_template_items(
        items,
        bindings,
        captured_aliases,
        scope,
        macros,
        repetition_index,
    )
    .map(|items| Expr::list(items, location))
}

fn expand_template_items(
    items: &[TemplateItem],
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    items.iter().try_fold(Vec::new(), |mut expressions, item| {
        expressions.extend(expand_template_item(
            item,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        )?);
        Ok(expressions)
    })
}

fn expand_template_item(
    item: &TemplateItem,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Vec<Expr>, EvalError> {
    if !item.repeated {
        return expand_template(
            &item.template,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        )
        .map(|expression| vec![expression]);
    }

    let repetition_count = template_repetition_count(&item.template, bindings)?;
    (0..repetition_count)
        .map(|index| {
            expand_template(
                &item.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                Some(index),
            )
        })
        .collect::<Result<Vec<_>, _>>()
}

fn template_repetition_count(
    template: &Template,
    bindings: &MatchBindings,
) -> Result<usize, EvalError> {
    let mut repetition_count = None;
    collect_template_repetition_count(template, bindings, &mut repetition_count)?;
    repetition_count.ok_or_else(|| {
        invalid_syntax_rules(
            template.location(),
            "ellipsis template must include a repeated pattern variable",
        )
    })
}

fn collect_template_repetition_count(
    template: &Template,
    bindings: &MatchBindings,
    repetition_count: &mut Option<usize>,
) -> Result<(), EvalError> {
    match template {
        Template::PatternVariable(name) => {
            let Some(values) = bindings.repeated.get(name) else {
                return Ok(());
            };
            merge_repetition_count(repetition_count, values.len(), template.location())
        }
        Template::List { items, .. } => items.iter().try_for_each(|item| {
            collect_template_repetition_count(&item.template, bindings, repetition_count)
        }),
        Template::Datum(_) | Template::Identifier { .. } => Ok(()),
    }
}

fn merge_repetition_count(
    repetition_count: &mut Option<usize>,
    next_count: usize,
    location: SourceLocation,
) -> Result<(), EvalError> {
    match repetition_count {
        Some(existing) if *existing != next_count => Err(invalid_syntax_rules(
            location,
            "repeated template variables must have the same length",
        )),
        Some(_) => Ok(()),
        None => {
            *repetition_count = Some(next_count);
            Ok(())
        }
    }
}

fn expand_lambda_template(
    items: &[TemplateItem],
    location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let [operator, parameters, body @ ..] = items else {
        return Err(invalid_syntax_rules(
            location,
            "lambda template requires parameters and a body",
        ));
    };

    let operator = expand_template(
        &operator.template,
        bindings,
        captured_aliases,
        scope,
        macros,
        repetition_index,
    )?;
    let (parameters, parameter_scope) = expand_formals_template(
        &parameters.template,
        bindings,
        captured_aliases,
        scope,
        macros,
        repetition_index,
    )?;
    let body_scope = merge_scopes(scope, parameter_scope);
    let expanded_body = expand_template_items(
        body,
        bindings,
        captured_aliases,
        &body_scope,
        macros,
        repetition_index,
    )?;

    Ok(Expr::list(
        std::iter::once(operator)
            .chain(std::iter::once(parameters))
            .chain(expanded_body)
            .collect(),
        location,
    ))
}

fn expand_let_template(
    items: &[TemplateItem],
    location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match items {
        [operator, name, let_bindings, body @ ..]
            if !matches!(name.template, Template::List { .. }) =>
        {
            let operator = expand_template(
                &operator.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            let (name, name_scope) =
                expand_binder_template(&name.template, bindings, scope, macros, repetition_index)?;
            let (let_bindings, binding_scope) = expand_let_bindings_template(
                &let_bindings.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            let body_scope = merge_scopes(&merge_scopes(scope, name_scope), binding_scope);
            let expanded_body = expand_template_items(
                body,
                bindings,
                captured_aliases,
                &body_scope,
                macros,
                repetition_index,
            )?;

            Ok(Expr::list(
                std::iter::once(operator)
                    .chain(std::iter::once(name))
                    .chain(std::iter::once(let_bindings))
                    .chain(expanded_body)
                    .collect(),
                location,
            ))
        }
        [operator, let_bindings, body @ ..] => {
            let operator = expand_template(
                &operator.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            let (let_bindings, binding_scope) = expand_let_bindings_template(
                &let_bindings.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            let body_scope = merge_scopes(scope, binding_scope);
            let expanded_body = expand_template_items(
                body,
                bindings,
                captured_aliases,
                &body_scope,
                macros,
                repetition_index,
            )?;

            Ok(Expr::list(
                std::iter::once(operator)
                    .chain(std::iter::once(let_bindings))
                    .chain(expanded_body)
                    .collect(),
                location,
            ))
        }
        _ => Err(invalid_syntax_rules(location, "let template is malformed")),
    }
}

fn expand_define_template(
    items: &[TemplateItem],
    location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    let Some(operator) = items.first() else {
        return Err(invalid_syntax_rules(location, "define template is empty"));
    };
    let operator = expand_template(
        &operator.template,
        bindings,
        captured_aliases,
        scope,
        macros,
        repetition_index,
    )?;

    match items {
        [_, name, expression] => {
            if let Template::List {
                items: signature_items,
                location: signature_location,
            } = &name.template
            {
                let (signature, body_scope) = expand_define_signature(
                    signature_items,
                    *signature_location,
                    bindings,
                    captured_aliases,
                    scope,
                    macros,
                    repetition_index,
                )?;
                let body = expand_template(
                    &expression.template,
                    bindings,
                    captured_aliases,
                    &body_scope,
                    macros,
                    repetition_index,
                )?;

                return Ok(Expr::list(vec![operator, signature, body], location));
            }

            let (name, _) =
                expand_binder_template(&name.template, bindings, scope, macros, repetition_index)?;
            let expression = expand_template(
                &expression.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            Ok(Expr::list(vec![operator, name, expression], location))
        }
        [_, name, body @ ..] if matches!(name.template, Template::List { .. }) => {
            let Template::List {
                items: signature_items,
                location: signature_location,
            } = &name.template
            else {
                unreachable!("function definition signature must be a list");
            };
            let (signature, body_scope) = expand_define_signature(
                signature_items,
                *signature_location,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            let body = expand_template_items(
                body,
                bindings,
                captured_aliases,
                &body_scope,
                macros,
                repetition_index,
            )?;

            Ok(Expr::list(
                std::iter::once(operator)
                    .chain(std::iter::once(signature))
                    .chain(body)
                    .collect(),
                location,
            ))
        }
        _ => Err(invalid_syntax_rules(
            location,
            "define template is malformed",
        )),
    }
}

fn expand_define_signature(
    signature_items: &[TemplateItem],
    signature_location: SourceLocation,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<(Expr, Scope), EvalError> {
    let Some((function_name, parameters)) = signature_items.split_first() else {
        return Err(invalid_syntax_rules(
            signature_location,
            "function definition signature cannot be empty",
        ));
    };
    let (function_name, function_scope) = expand_binder_template(
        &function_name.template,
        bindings,
        scope,
        macros,
        repetition_index,
    )?;
    let (parameters, parameter_scope) = expand_formals_items(
        parameters,
        bindings,
        captured_aliases,
        scope,
        macros,
        repetition_index,
    )?;
    let signature = Expr::list(
        std::iter::once(function_name).chain(parameters).collect(),
        signature_location,
    );
    let body_scope = merge_scopes(&merge_scopes(scope, function_scope), parameter_scope);
    Ok((signature, body_scope))
}

fn expand_formals_template(
    template: &Template,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<(Expr, Scope), EvalError> {
    match template {
        Template::List { items, location } => {
            let (items, scope) = expand_formals_items(
                items,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            Ok((Expr::list(items, *location), scope))
        }
        _ => expand_binder_template(template, bindings, scope, macros, repetition_index),
    }
}

fn expand_formals_items(
    items: &[TemplateItem],
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<(Vec<Expr>, Scope), EvalError> {
    let mut expanded_items = Vec::new();
    let mut bindings_scope = HashMap::new();

    for item in items {
        if item.repeated {
            return Err(invalid_syntax_rules(
                item.template.location(),
                "ellipsis in lambda formals is not supported in this level",
            ));
        }

        let (expression, binding_scope) =
            expand_binder_template(&item.template, bindings, scope, macros, repetition_index)?;
        bindings_scope.extend(binding_scope);
        expanded_items.push(expression);
    }

    let _ = captured_aliases;
    Ok((expanded_items, bindings_scope))
}

fn expand_let_bindings_template(
    template: &Template,
    bindings: &MatchBindings,
    captured_aliases: &HashMap<String, String>,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<(Expr, Scope), EvalError> {
    let Template::List { items, location } = template else {
        return expand_template(
            template,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        )
        .map(|expression| (expression, HashMap::new()));
    };

    let mut expanded_bindings = Vec::new();
    let mut binding_scope = HashMap::new();

    for item in items {
        if item.repeated {
            return Err(invalid_syntax_rules(
                item.template.location(),
                "ellipsis in let bindings is not supported in this level",
            ));
        }

        let Template::List {
            items: binding_items,
            location: binding_location,
        } = &item.template
        else {
            let expression = expand_template(
                &item.template,
                bindings,
                captured_aliases,
                scope,
                macros,
                repetition_index,
            )?;
            expanded_bindings.push(expression);
            continue;
        };
        let [name, expression] = binding_items.as_slice() else {
            return Err(invalid_syntax_rules(
                *binding_location,
                "let binding must contain a name and value",
            ));
        };

        let (name, local_scope) =
            expand_binder_template(&name.template, bindings, scope, macros, repetition_index)?;
        let expression = expand_template(
            &expression.template,
            bindings,
            captured_aliases,
            scope,
            macros,
            repetition_index,
        )?;
        binding_scope.extend(local_scope);
        expanded_bindings.push(Expr::list(vec![name, expression], *binding_location));
    }

    Ok((Expr::list(expanded_bindings, *location), binding_scope))
}

fn expand_binder_template(
    template: &Template,
    bindings: &MatchBindings,
    scope: &Scope,
    macros: &MacroEnvironment,
    repetition_index: Option<usize>,
) -> Result<(Expr, Scope), EvalError> {
    match template {
        Template::Identifier { name, location } if name == "." => {
            Ok((Expr::symbol(".".into(), *location), HashMap::new()))
        }
        Template::Identifier { name, location } => {
            let fresh_name = macros.fresh_identifier(name);
            Ok((
                Expr::symbol(fresh_name.clone(), *location),
                HashMap::from([(name.clone(), fresh_name)]),
            ))
        }
        Template::PatternVariable(name) => {
            expand_pattern_variable(name, bindings, template.location(), repetition_index)
                .map(|expression| (expression, HashMap::new()))
        }
        Template::Datum(expression) => Ok((expression.clone(), HashMap::new())),
        Template::List { location, .. } => Err(invalid_syntax_rules(
            *location,
            "binder position requires an identifier",
        )),
    }
    .map(|(expression, bindings_scope)| {
        let _ = scope;
        (expression, bindings_scope)
    })
}

fn merge_scopes(left: &Scope, right: Scope) -> Scope {
    let mut merged = left.clone();
    merged.extend(right);
    merged
}

fn same_datum(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Integer { value: left, .. }, Expr::Integer { value: right, .. }) => left == right,
        (Expr::Boolean { value: left, .. }, Expr::Boolean { value: right, .. }) => left == right,
        (Expr::String { value: left, .. }, Expr::String { value: right, .. }) => left == right,
        (Expr::Character { value: left, .. }, Expr::Character { value: right, .. }) => {
            left == right
        }
        (Expr::Symbol { name: left, .. }, Expr::Symbol { name: right, .. }) => left == right,
        (Expr::List { items: left, .. }, Expr::List { items: right, .. }) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same_datum(left, right))
        }
        _ => false,
    }
}

fn same_datum_list(left: &[Expr], right: &[Expr]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| same_datum(left, right))
}

fn invalid_syntax_rules(location: SourceLocation, detail: &'static str) -> EvalError {
    EvalError::InvalidSyntaxRules { location, detail }
}

fn is_ellipsis(expression: &Expr) -> bool {
    matches!(expression, Expr::Symbol { name, .. } if name == "...")
}

fn is_quote_form(expression: &Expr) -> bool {
    matches!(
        expression,
        Expr::List { items, .. }
            if matches!(
                items.first(),
                Some(Expr::Symbol { name, .. }) if name == "quote"
            )
    )
}

fn is_core_special_form(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "or"
            | "if"
            | "define"
            | "set!"
            | "quote"
            | "lambda"
            | "let"
            | "begin"
            | "cond"
            | "define-syntax"
    )
}
