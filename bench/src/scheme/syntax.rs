use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::scheme::environment::Environment;
use crate::scheme::error::SchemeError;
use crate::scheme::parser::Expr;

static HYGIENE_COUNTER: AtomicUsize = AtomicUsize::new(0);

type Bindings = HashMap<String, MatchValue>;
type Renames = HashMap<String, String>;

#[derive(Debug)]
pub(crate) struct SyntaxRules {
    macro_name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    environment: Environment,
}

#[derive(Debug)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Debug, Clone)]
enum MatchValue {
    Single(Expr),
    Repeated(Vec<MatchValue>),
}

#[derive(Clone, Copy)]
enum SequenceItem<'a> {
    Single(&'a Expr),
    Repeated(&'a Expr),
}

struct TemplateContext<'a> {
    bindings: &'a Bindings,
    indices: Vec<usize>,
    definition_environment: &'a Environment,
}

impl<'a> TemplateContext<'a> {
    fn nested(&self, index: usize) -> Self {
        let mut indices = self.indices.clone();
        indices.push(index);
        Self {
            bindings: self.bindings,
            indices,
            definition_environment: self.definition_environment,
        }
    }
}

impl SyntaxRules {
    pub(crate) fn parse(
        macro_name: &str,
        transformer: &Expr,
        environment: Environment,
    ) -> Result<Rc<Self>, SchemeError> {
        let Expr::List(parts) = transformer else {
            return Err(SchemeError::InvalidSyntaxTransformer {
                found: expression_kind(transformer),
            });
        };

        let Some((head, rest)) = parts.split_first() else {
            return Err(SchemeError::TooFewArguments {
                operator: "syntax-rules",
                min: 2,
                actual: 0,
            });
        };

        if !head.is_symbol_named("syntax-rules") {
            return Err(SchemeError::InvalidSyntaxTransformer {
                found: expression_kind(transformer),
            });
        }

        parse_syntax_rules(macro_name, rest, environment)
    }

    pub(crate) fn expand(&self, form: &[Expr]) -> Result<Expr, SchemeError> {
        let input = Expr::List(form.to_vec());

        self.rules
            .iter()
            .find_map(|rule| self.expand_rule(rule, &input))
            .unwrap_or_else(|| {
                Err(SchemeError::UnboundSymbol {
                    name: self.macro_name.clone(),
                })
            })
    }

    fn expand_rule(&self, rule: &SyntaxRule, input: &Expr) -> Option<Result<Expr, SchemeError>> {
        let bindings = match_pattern(&rule.pattern, input, &self.macro_name, &self.literals)?;
        Some(expand_rule_template(
            &rule.template,
            &bindings,
            &self.environment,
        ))
    }
}

fn parse_syntax_rules(
    macro_name: &str,
    operands: &[Expr],
    environment: Environment,
) -> Result<Rc<SyntaxRules>, SchemeError> {
    let [literals, rules @ ..] = operands else {
        return Err(SchemeError::TooFewArguments {
            operator: "syntax-rules",
            min: 2,
            actual: operands.len(),
        });
    };

    if rules.is_empty() {
        return Err(SchemeError::TooFewArguments {
            operator: "syntax-rules",
            min: 2,
            actual: operands.len(),
        });
    }

    let literals = parse_literal_identifiers(literals)?;
    let rules = parse_rules(rules, macro_name)?;

    Ok(Rc::new(SyntaxRules {
        macro_name: macro_name.to_owned(),
        literals,
        rules,
        environment,
    }))
}

fn parse_literal_identifiers(literals: &Expr) -> Result<HashSet<String>, SchemeError> {
    let Expr::List(identifiers) = literals else {
        return Err(SchemeError::InvalidSyntaxLiteralList {
            found: expression_kind(literals),
        });
    };

    let mut parsed = HashSet::with_capacity(identifiers.len());

    for identifier in identifiers {
        let Some(name) = identifier.symbol_name() else {
            return Err(SchemeError::InvalidSyntaxLiteral {
                found: expression_kind(identifier),
            });
        };
        let _ = parsed.insert(name.to_owned());
    }

    Ok(parsed)
}

fn parse_rules(rules: &[Expr], macro_name: &str) -> Result<Vec<SyntaxRule>, SchemeError> {
    let mut parsed = Vec::with_capacity(rules.len());

    for rule in rules {
        parsed.push(parse_rule(rule, macro_name)?);
    }

    Ok(parsed)
}

fn parse_rule(rule: &Expr, macro_name: &str) -> Result<SyntaxRule, SchemeError> {
    let Expr::List(parts) = rule else {
        return Err(SchemeError::InvalidSyntaxRule {
            found: expression_kind(rule),
        });
    };

    let [pattern, template] = parts.as_slice() else {
        return Err(SchemeError::InvalidSyntaxRuleArity {
            actual: parts.len(),
        });
    };

    validate_rule_pattern(pattern, macro_name)?;
    Ok(SyntaxRule {
        pattern: pattern.clone(),
        template: template.clone(),
    })
}

fn validate_rule_pattern(pattern: &Expr, macro_name: &str) -> Result<(), SchemeError> {
    let Expr::List(parts) = pattern else {
        return Err(SchemeError::InvalidSyntaxRule {
            found: expression_kind(pattern),
        });
    };

    let Some(name) = parts.first().and_then(Expr::symbol_name) else {
        return Err(SchemeError::InvalidSyntaxRule {
            found: expression_kind(pattern),
        });
    };

    if name == macro_name {
        Ok(())
    } else {
        Err(SchemeError::UnexpectedMacroPatternName {
            expected: macro_name.to_owned(),
            actual: name.to_owned(),
        })
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    match pattern {
        Expr::Integer(value) => match_literal_value(*value, input),
        Expr::Boolean(value) => match_literal_boolean(*value, input),
        Expr::String(value) => match_literal_string(value, input),
        Expr::Symbol(name) | Expr::ScopedSymbol { name, .. } => {
            match_symbol_pattern(name, input, macro_name, literals)
        }
        Expr::List(patterns) => match_list_pattern(patterns, input, macro_name, literals),
    }
}

fn match_literal_value(value: i64, input: &Expr) -> Option<Bindings> {
    matches!(input, Expr::Integer(other) if *other == value).then(HashMap::new)
}

fn match_literal_boolean(value: bool, input: &Expr) -> Option<Bindings> {
    matches!(input, Expr::Boolean(other) if *other == value).then(HashMap::new)
}

fn match_literal_string(value: &str, input: &Expr) -> Option<Bindings> {
    matches!(input, Expr::String(other) if other == value).then(HashMap::new)
}

fn match_symbol_pattern(
    name: &str,
    input: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    if name == "..." {
        return None;
    }

    if name == macro_name || literals.contains(name) {
        return input.is_symbol_named(name).then(HashMap::new);
    }

    Some(HashMap::from([(
        name.to_owned(),
        MatchValue::Single(input.clone()),
    )]))
}

fn match_list_pattern(
    patterns: &[Expr],
    input: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    let Expr::List(inputs) = input else {
        return None;
    };

    let items = parse_sequence(patterns).ok()?;
    match_sequence(&items, inputs, macro_name, literals)
}

fn match_sequence(
    items: &[SequenceItem<'_>],
    inputs: &[Expr],
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    let Some((first, rest)) = items.split_first() else {
        return inputs.is_empty().then(HashMap::new);
    };

    match first {
        SequenceItem::Single(pattern) => {
            let (input, remaining) = inputs.split_first()?;
            let bindings = match_pattern(pattern, input, macro_name, literals)?;
            let remaining = match_sequence(rest, remaining, macro_name, literals)?;
            merge_bindings(bindings, remaining)
        }
        SequenceItem::Repeated(pattern) => {
            match_repeated_sequence(pattern, rest, inputs, macro_name, literals)
        }
    }
}

fn match_repeated_sequence(
    pattern: &Expr,
    rest: &[SequenceItem<'_>],
    inputs: &[Expr],
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    let minimum_remaining = minimum_inputs_needed(rest);
    let maximum = inputs.len().checked_sub(minimum_remaining)?;

    for count in (0..=maximum).rev() {
        let repeated = match_repeated_pattern(pattern, &inputs[..count], macro_name, literals)?;
        let remaining = match_sequence(rest, &inputs[count..], macro_name, literals)?;
        let Some(bindings) = merge_bindings(repeated, remaining) else {
            continue;
        };
        return Some(bindings);
    }

    None
}

fn minimum_inputs_needed(items: &[SequenceItem<'_>]) -> usize {
    items.iter().fold(0, |count, item| match item {
        SequenceItem::Single(_) => count + 1,
        SequenceItem::Repeated(_) => count,
    })
}

fn match_repeated_pattern(
    pattern: &Expr,
    inputs: &[Expr],
    macro_name: &str,
    literals: &HashSet<String>,
) -> Option<Bindings> {
    let variable_names = collect_pattern_variables(pattern, macro_name, literals);
    let mut bindings = empty_repeated_bindings(&variable_names);

    for input in inputs {
        let iteration = match_pattern(pattern, input, macro_name, literals)?;
        append_repeated_bindings(&mut bindings, iteration)?;
    }

    Some(bindings)
}

fn collect_pattern_variables(
    pattern: &Expr,
    macro_name: &str,
    literals: &HashSet<String>,
) -> HashSet<String> {
    match pattern {
        Expr::Symbol(name) | Expr::ScopedSymbol { name, .. }
            if name != "..." && name != macro_name && !literals.contains(name) =>
        {
            HashSet::from([name.clone()])
        }
        Expr::List(items) => collect_pattern_variables_from_sequence(items, macro_name, literals),
        Expr::Integer(_)
        | Expr::Boolean(_)
        | Expr::String(_)
        | Expr::Symbol(_)
        | Expr::ScopedSymbol { .. } => HashSet::new(),
    }
}

fn collect_pattern_variables_from_sequence(
    items: &[Expr],
    macro_name: &str,
    literals: &HashSet<String>,
) -> HashSet<String> {
    let Ok(sequence) = parse_sequence(items) else {
        return HashSet::new();
    };

    let mut variables = HashSet::new();

    for item in sequence {
        let pattern = match item {
            SequenceItem::Single(pattern) | SequenceItem::Repeated(pattern) => pattern,
        };
        variables.extend(collect_pattern_variables(pattern, macro_name, literals));
    }

    variables
}

fn empty_repeated_bindings(variable_names: &HashSet<String>) -> Bindings {
    let mut bindings = HashMap::with_capacity(variable_names.len());

    for name in variable_names {
        let _ = bindings.insert(name.clone(), MatchValue::Repeated(Vec::new()));
    }

    bindings
}

fn append_repeated_bindings(bindings: &mut Bindings, iteration: Bindings) -> Option<()> {
    for (name, value) in iteration {
        match bindings.get_mut(&name) {
            Some(MatchValue::Repeated(values)) => values.push(value),
            Some(MatchValue::Single(_)) => return None,
            None => {
                let _ = bindings.insert(name, MatchValue::Repeated(vec![value]));
            }
        }
    }

    Some(())
}

fn merge_bindings(mut lhs: Bindings, rhs: Bindings) -> Option<Bindings> {
    for (name, value) in rhs {
        match lhs.get(&name) {
            Some(existing) if !match_values_equal(existing, &value) => return None,
            Some(_) => {}
            None => {
                let _ = lhs.insert(name, value);
            }
        }
    }

    Some(lhs)
}

fn match_values_equal(lhs: &MatchValue, rhs: &MatchValue) -> bool {
    match (lhs, rhs) {
        (MatchValue::Single(lhs), MatchValue::Single(rhs)) => same_expr(lhs, rhs),
        (MatchValue::Repeated(lhs), MatchValue::Repeated(rhs)) => {
            lhs.len() == rhs.len()
                && lhs
                    .iter()
                    .zip(rhs)
                    .all(|(lhs_value, rhs_value)| match_values_equal(lhs_value, rhs_value))
        }
        _ => false,
    }
}

fn same_expr(lhs: &Expr, rhs: &Expr) -> bool {
    match (lhs, rhs) {
        (Expr::Integer(lhs), Expr::Integer(rhs)) => lhs == rhs,
        (Expr::Boolean(lhs), Expr::Boolean(rhs)) => lhs == rhs,
        (Expr::String(lhs), Expr::String(rhs)) => lhs == rhs,
        (Expr::Symbol(lhs), Expr::Symbol(rhs)) => lhs == rhs,
        (
            Expr::ScopedSymbol {
                name: lhs_name,
                environment: lhs_environment,
            },
            Expr::ScopedSymbol {
                name: rhs_name,
                environment: rhs_environment,
            },
        ) => lhs_name == rhs_name && lhs_environment.ptr_eq(rhs_environment),
        (Expr::List(lhs), Expr::List(rhs)) => {
            lhs.len() == rhs.len() && lhs.iter().zip(rhs).all(|(lhs, rhs)| same_expr(lhs, rhs))
        }
        _ => false,
    }
}

fn expand_rule_template(
    template: &Expr,
    bindings: &Bindings,
    definition_environment: &Environment,
) -> Result<Expr, SchemeError> {
    let context = TemplateContext {
        bindings,
        indices: Vec::new(),
        definition_environment,
    };
    let expanded = expand_template(template, &context)?;
    hygienize_expansion(&expanded, definition_environment, &HashMap::new()).map(|(expr, _)| expr)
}

fn expand_template(template: &Expr, context: &TemplateContext<'_>) -> Result<Expr, SchemeError> {
    match template {
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => Ok(template.clone()),
        Expr::Symbol(name) => expand_template_symbol(name, context),
        Expr::ScopedSymbol { name, .. } => expand_template_symbol(name, context),
        Expr::List(items) => expand_list_template(items, context),
    }
}

fn expand_template_symbol(name: &str, context: &TemplateContext<'_>) -> Result<Expr, SchemeError> {
    if let Some(binding) = context.bindings.get(name) {
        return resolve_template_binding(name, binding, &context.indices);
    }

    Ok(Expr::scoped_symbol(
        name,
        context.definition_environment.clone(),
    ))
}

fn expand_list_template(
    items: &[Expr],
    context: &TemplateContext<'_>,
) -> Result<Expr, SchemeError> {
    let mut expanded = Vec::new();

    for item in parse_sequence(items)? {
        match item {
            SequenceItem::Single(template) => expanded.push(expand_template(template, context)?),
            SequenceItem::Repeated(template) => {
                expanded.extend(expand_repeated_template(template, context)?);
            }
        }
    }

    Ok(Expr::List(expanded))
}

fn expand_repeated_template(
    template: &Expr,
    context: &TemplateContext<'_>,
) -> Result<Vec<Expr>, SchemeError> {
    let repetitions = template_repetition_length(template, context)?;
    let mut expanded = Vec::with_capacity(repetitions);

    for index in 0..repetitions {
        expanded.push(expand_template(template, &context.nested(index))?);
    }

    Ok(expanded)
}

fn template_repetition_length(
    template: &Expr,
    context: &TemplateContext<'_>,
) -> Result<usize, SchemeError> {
    let mut repetition_length = None;

    for name in collect_template_variables(template, context.bindings) {
        let Some(binding) = context.bindings.get(&name) else {
            continue;
        };
        let Some(length) = repetition_len(binding, &context.indices) else {
            continue;
        };
        repetition_length = repetition_length.or(Some(length));
    }

    repetition_length.ok_or(SchemeError::EllipsisWithoutPatternVariable {
        context: "syntax-rules template",
    })
}

fn collect_template_variables(template: &Expr, bindings: &Bindings) -> HashSet<String> {
    match template {
        Expr::Symbol(name) | Expr::ScopedSymbol { name, .. } if bindings.contains_key(name) => {
            HashSet::from([name.clone()])
        }
        Expr::List(items) => collect_template_variables_from_sequence(items, bindings),
        Expr::Integer(_)
        | Expr::Boolean(_)
        | Expr::String(_)
        | Expr::Symbol(_)
        | Expr::ScopedSymbol { .. } => HashSet::new(),
    }
}

fn collect_template_variables_from_sequence(
    items: &[Expr],
    bindings: &Bindings,
) -> HashSet<String> {
    let Ok(sequence) = parse_sequence(items) else {
        return HashSet::new();
    };

    let mut variables = HashSet::new();

    for item in sequence {
        let pattern = match item {
            SequenceItem::Single(pattern) | SequenceItem::Repeated(pattern) => pattern,
        };
        variables.extend(collect_template_variables(pattern, bindings));
    }

    variables
}

fn repetition_len(binding: &MatchValue, indices: &[usize]) -> Option<usize> {
    let current = resolve_match_value(binding, indices)?;

    match current {
        MatchValue::Repeated(values) => Some(values.len()),
        MatchValue::Single(_) => None,
    }
}

fn resolve_template_binding(
    name: &str,
    binding: &MatchValue,
    indices: &[usize],
) -> Result<Expr, SchemeError> {
    match resolve_match_value(binding, indices) {
        Some(MatchValue::Single(expression)) => Ok(expression.clone()),
        Some(MatchValue::Repeated(_)) | None => Err(SchemeError::TemplateVariableOutOfContext {
            name: name.to_owned(),
        }),
    }
}

fn resolve_match_value<'a>(binding: &'a MatchValue, indices: &[usize]) -> Option<&'a MatchValue> {
    let mut current = binding;

    for index in indices {
        match current {
            MatchValue::Repeated(values) => current = values.get(*index)?,
            MatchValue::Single(_) => break,
        }
    }

    Some(current)
}

fn hygienize_expansion(
    expression: &Expr,
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    match expression {
        Expr::List(items) => hygienize_list(items, definition_environment, renames),
        Expr::Symbol(_) | Expr::ScopedSymbol { .. } => Ok((
            rename_symbol_if_needed(expression, definition_environment, renames),
            HashMap::new(),
        )),
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => {
            Ok((expression.clone(), HashMap::new()))
        }
    }
}

fn hygienize_list(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    match items.first().and_then(Expr::symbol_name) {
        Some("begin") => hygienize_begin(items, definition_environment, renames),
        Some("lambda") => hygienize_lambda(items, definition_environment, renames),
        Some("let") => hygienize_let(items, definition_environment, renames),
        Some("define") => hygienize_define(items, definition_environment, renames),
        _ => hygienize_generic_list(items, definition_environment, renames),
    }
}

fn hygienize_generic_list(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let mut hygienized = Vec::with_capacity(items.len());

    for item in items {
        hygienized.push(hygienize_expansion(item, definition_environment, renames)?.0);
    }

    Ok((Expr::List(hygienized), HashMap::new()))
}

fn hygienize_begin(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let Some((head, body)) = items.split_first() else {
        return Ok((Expr::List(Vec::new()), HashMap::new()));
    };

    let mut expressions = vec![hygienize_expansion(head, definition_environment, renames)?.0];
    expressions.extend(hygienize_sequence(body, definition_environment, renames)?);
    Ok((Expr::List(expressions), HashMap::new()))
}

fn hygienize_lambda(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    if items.len() < 3 {
        return hygienize_generic_list(items, definition_environment, renames);
    }

    let head = hygienize_expansion(&items[0], definition_environment, renames)?.0;
    let (parameters, parameter_renames) =
        hygienize_parameter_list(&items[1], definition_environment)?;
    let body_renames = extend_renames(renames, parameter_renames);
    let body = hygienize_sequence(&items[2..], definition_environment, &body_renames)?;
    let mut expressions = vec![head, parameters];
    expressions.extend(body);
    Ok((Expr::List(expressions), HashMap::new()))
}

fn hygienize_parameter_list(
    parameters: &Expr,
    definition_environment: &Environment,
) -> Result<(Expr, Renames), SchemeError> {
    match parameters {
        Expr::List(parameters) => hygienize_parameters(parameters, definition_environment),
        Expr::Symbol(_) | Expr::ScopedSymbol { .. } => {
            let (parameter, introduced) = fresh_binding(parameters, definition_environment);
            let renames: Renames = introduced.into_iter().collect();
            Ok((parameter, renames))
        }
        Expr::Integer(_) | Expr::Boolean(_) | Expr::String(_) => {
            Ok((parameters.clone(), HashMap::new()))
        }
    }
}

fn hygienize_parameters(
    parameters: &[Expr],
    definition_environment: &Environment,
) -> Result<(Expr, Renames), SchemeError> {
    let mut hygienized = Vec::with_capacity(parameters.len());
    let mut renames = HashMap::new();

    for parameter in parameters {
        let (parameter, introduced) = if parameter.is_symbol_named(".") {
            (parameter.clone(), None)
        } else {
            fresh_binding(parameter, definition_environment)
        };

        hygienized.push(parameter);
        if let Some((name, renamed)) = introduced {
            let _ = renames.insert(name, renamed);
        }
    }

    Ok((Expr::List(hygienized), renames))
}

fn hygienize_let(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    if items.len() >= 4 && items[1].symbol_name().is_some() {
        return hygienize_named_let(items, definition_environment, renames);
    }

    hygienize_regular_let(items, definition_environment, renames)
}

fn hygienize_regular_let(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    if items.len() < 3 {
        return hygienize_generic_list(items, definition_environment, renames);
    }

    let head = hygienize_expansion(&items[0], definition_environment, renames)?.0;
    let (bindings, binding_renames) =
        hygienize_let_bindings(&items[1], definition_environment, renames)?;
    let body_renames = extend_renames(renames, binding_renames);
    let body = hygienize_sequence(&items[2..], definition_environment, &body_renames)?;
    let mut expressions = vec![head, bindings];
    expressions.extend(body);
    Ok((Expr::List(expressions), HashMap::new()))
}

fn hygienize_named_let(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let head = hygienize_expansion(&items[0], definition_environment, renames)?.0;
    let (name, introduced) = fresh_binding(&items[1], definition_environment);
    let function_renames = introduced.into_iter().collect();
    let (bindings, binding_renames) =
        hygienize_let_bindings(&items[2], definition_environment, renames)?;
    let body_renames = extend_renames(renames, function_renames);
    let body_renames = extend_renames(&body_renames, binding_renames);
    let body = hygienize_sequence(&items[3..], definition_environment, &body_renames)?;
    let mut expressions = vec![head, name, bindings];
    expressions.extend(body);
    Ok((Expr::List(expressions), HashMap::new()))
}

fn hygienize_let_bindings(
    bindings: &Expr,
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let Expr::List(bindings) = bindings else {
        return Ok((
            hygienize_expansion(bindings, definition_environment, renames)?.0,
            HashMap::new(),
        ));
    };

    let mut hygienized = Vec::with_capacity(bindings.len());
    let mut binding_renames = HashMap::new();

    for binding in bindings {
        let (binding, introduced) =
            hygienize_single_let_binding(binding, definition_environment, renames)?;
        hygienized.push(binding);
        binding_renames.extend(introduced);
    }

    Ok((Expr::List(hygienized), binding_renames))
}

fn hygienize_single_let_binding(
    binding: &Expr,
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let Expr::List(parts) = binding else {
        return Ok((
            hygienize_expansion(binding, definition_environment, renames)?.0,
            HashMap::new(),
        ));
    };

    let Some((name, rest)) = parts.split_first() else {
        return Ok((Expr::List(Vec::new()), HashMap::new()));
    };

    let (name, introduced) = fresh_binding(name, definition_environment);
    let mut hygienized = vec![name];

    for expression in rest {
        hygienized.push(hygienize_expansion(expression, definition_environment, renames)?.0);
    }

    let introduced = introduced.into_iter().collect();
    Ok((Expr::List(hygienized), introduced))
}

fn hygienize_define(
    items: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    match items {
        [_, Expr::List(signature), body @ ..] if !body.is_empty() => {
            hygienize_function_define(items, signature, body, definition_environment, renames)
        }
        [_, target, value] => {
            hygienize_variable_define(items, target, value, definition_environment, renames)
        }
        _ => hygienize_generic_list(items, definition_environment, renames),
    }
}

fn hygienize_variable_define(
    items: &[Expr],
    target: &Expr,
    value: &Expr,
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let head = hygienize_expansion(&items[0], definition_environment, renames)?.0;
    let (target, introduced) = fresh_binding(target, definition_environment);
    let local_renames: Renames = introduced.into_iter().collect();
    let value_renames = extend_renames(renames, local_renames.clone());
    let value = hygienize_expansion(value, definition_environment, &value_renames)?.0;
    Ok((Expr::List(vec![head, target, value]), local_renames))
}

fn hygienize_function_define(
    items: &[Expr],
    signature: &[Expr],
    body: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<(Expr, Renames), SchemeError> {
    let Some((name, parameters)) = signature.split_first() else {
        return hygienize_generic_list(items, definition_environment, renames);
    };

    let head = hygienize_expansion(&items[0], definition_environment, renames)?.0;
    let (name, introduced) = fresh_binding(name, definition_environment);
    let local_renames: Renames = introduced.into_iter().collect();
    let (parameters, parameter_renames) = hygienize_parameters(parameters, definition_environment)?;
    let body_renames = extend_renames(renames, local_renames.clone());
    let body_renames = extend_renames(&body_renames, parameter_renames);
    let body = hygienize_sequence(body, definition_environment, &body_renames)?;
    let signature = build_function_signature(name, parameters);
    let mut expressions = vec![head, signature];
    expressions.extend(body);
    Ok((Expr::List(expressions), local_renames))
}

fn build_function_signature(name: Expr, parameters: Expr) -> Expr {
    match parameters {
        Expr::List(mut parameters) => {
            let mut signature = vec![name];
            signature.append(&mut parameters);
            Expr::List(signature)
        }
        parameters => Expr::List(vec![name, parameters]),
    }
}

fn hygienize_sequence(
    expressions: &[Expr],
    definition_environment: &Environment,
    renames: &Renames,
) -> Result<Vec<Expr>, SchemeError> {
    let mut current_renames = renames.clone();
    let mut hygienized = Vec::with_capacity(expressions.len());

    for expression in expressions {
        let (expression, introduced) =
            hygienize_expansion(expression, definition_environment, &current_renames)?;
        current_renames = extend_renames(&current_renames, introduced);
        hygienized.push(expression);
    }

    Ok(hygienized)
}

fn fresh_binding(
    identifier: &Expr,
    definition_environment: &Environment,
) -> (Expr, Option<(String, String)>) {
    match identifier {
        Expr::ScopedSymbol { name, environment } if environment.ptr_eq(definition_environment) => {
            let renamed = fresh_binding_name(name);
            (Expr::symbol(renamed.clone()), Some((name.clone(), renamed)))
        }
        _ => (identifier.clone(), None),
    }
}

fn fresh_binding_name(base: &str) -> String {
    let counter = HYGIENE_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__syntax_{counter}_{base}")
}

fn extend_renames(base: &Renames, additions: Renames) -> Renames {
    let mut combined = base.clone();
    combined.extend(additions);
    combined
}

fn rename_symbol_if_needed(
    expression: &Expr,
    definition_environment: &Environment,
    renames: &Renames,
) -> Expr {
    match expression {
        Expr::ScopedSymbol { name, environment } if environment.ptr_eq(definition_environment) => {
            match renames.get(name) {
                Some(renamed) => Expr::symbol(renamed.clone()),
                None => expression.clone(),
            }
        }
        _ => expression.clone(),
    }
}

fn parse_sequence(items: &[Expr]) -> Result<Vec<SequenceItem<'_>>, SchemeError> {
    let mut parsed = Vec::new();
    let mut index = 0;

    while index < items.len() {
        let expression = &items[index];

        if expression.is_symbol_named("...") {
            return Err(SchemeError::InvalidEllipsisPlacement {
                context: "syntax-rules",
            });
        }

        let repeated = items
            .get(index + 1)
            .is_some_and(|expression| expression.is_symbol_named("..."));
        parsed.push(if repeated {
            SequenceItem::Repeated(expression)
        } else {
            SequenceItem::Single(expression)
        });
        index += 1 + usize::from(repeated);
    }

    Ok(parsed)
}

fn expression_kind(expression: &Expr) -> &'static str {
    match expression {
        Expr::Integer(_) => "number",
        Expr::Boolean(_) => "boolean",
        Expr::String(_) => "string",
        Expr::Symbol(_) | Expr::ScopedSymbol { .. } => "symbol",
        Expr::List(_) => "list",
    }
}
