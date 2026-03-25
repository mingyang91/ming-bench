use super::{
    builtin_name, env_define, env_define_alias, env_define_macro, env_lookup_cell,
    env_lookup_macro, syntax_error, EnvRef, EvalContext, EvalError, Expr, ExprKind, MacroRef,
    MacroRule, MacroTransformer, PatternBinding, Procedure, SourcePos, Value, START_POS,
};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub(super) fn parse_syntax_rules(
    name: &str,
    expr: &Expr,
    env: &EnvRef,
) -> Result<MacroRef, EvalError> {
    let items = expr.list_items().ok_or_else(|| {
        syntax_error(
            expr.pos,
            "define-syntax requires a syntax-rules transformer",
        )
    })?;

    let (head, rest) = items.split_first().ok_or_else(|| {
        syntax_error(
            expr.pos,
            "define-syntax requires a syntax-rules transformer",
        )
    })?;

    if head.symbol_name() != Some("syntax-rules") {
        return Err(syntax_error(
            head.pos,
            "define-syntax requires a syntax-rules transformer",
        ));
    }

    let (literals_expr, rules) = rest.split_first().ok_or_else(|| {
        syntax_error(
            expr.pos,
            "syntax-rules requires a literals list and at least one rule",
        )
    })?;

    if rules.is_empty() {
        return Err(syntax_error(
            expr.pos,
            "syntax-rules requires at least one rule",
        ));
    }

    let literal_items = literals_expr
        .list_items()
        .ok_or_else(|| syntax_error(literals_expr.pos, "syntax-rules literals must be a list"))?;
    let mut literals = HashSet::new();

    for literal in literal_items {
        let literal_name = literal
            .symbol_name()
            .ok_or_else(|| syntax_error(literal.pos, "syntax-rules literals must be symbols"))?;
        literals.insert(literal_name.to_string());
    }

    let mut parsed_rules = Vec::with_capacity(rules.len());
    for rule in rules {
        let rule_items = rule
            .list_items()
            .ok_or_else(|| syntax_error(rule.pos, "syntax-rules clauses must be lists"))?;

        match rule_items {
            [pattern, template] => parsed_rules.push(MacroRule {
                pattern: pattern.clone(),
                template: template.clone(),
            }),
            _ => {
                return Err(syntax_error(
                    rule.pos,
                    "each syntax-rules clause must contain a pattern and template",
                ))
            }
        }
    }

    Ok(Rc::new(MacroTransformer {
        name: name.to_string(),
        literals,
        rules: parsed_rules,
        env: Rc::downgrade(env),
    }))
}

pub(super) fn expand_macro_call(
    transformer: &MacroRef,
    items: &[Expr],
    pos: SourcePos,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Expr, EvalError> {
    let mut normalized_items = items.to_vec();
    if let Some(head) = normalized_items.first_mut() {
        *head = Expr::new(head.pos, ExprKind::Symbol(transformer.name.clone()));
    }
    let call_expr = Expr::new(pos, ExprKind::List(normalized_items));

    for rule in &transformer.rules {
        let mut bindings = HashMap::new();
        if !match_pattern(&rule.pattern, &call_expr, transformer, &mut bindings)? {
            continue;
        }

        let scope_renames = HashMap::new();
        let mut free_renames = HashMap::new();
        let mut instantiator =
            TemplateInstantiator::new(transformer, env, &bindings, &mut free_renames, context);
        return instantiator.instantiate(&rule.template, &scope_renames, None);
    }

    Err(syntax_error(
        pos,
        format!("no matching syntax-rules pattern for {}", transformer.name),
    ))
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    transformer: &MacroTransformer,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    match (&pattern.kind, &input.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => Ok(left == right),
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => Ok(left == right),
        (ExprKind::Character(left), ExprKind::Character(right)) => Ok(left == right),
        (ExprKind::String(left), ExprKind::String(right)) => Ok(left == right),
        (ExprKind::Symbol(name), _) => {
            match_pattern_symbol(name, pattern.pos, input, transformer, bindings)
        }
        (ExprKind::List(pattern_items), ExprKind::List(input_items)) => {
            match_pattern_list(pattern_items, input_items, transformer, bindings)
        }
        _ => Ok(false),
    }
}

fn match_pattern_symbol(
    name: &str,
    pos: SourcePos,
    input: &Expr,
    transformer: &MacroTransformer,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    if name == "..." {
        return Err(syntax_error(
            pos,
            "ellipsis cannot appear by itself in a pattern",
        ));
    }

    if name == transformer.name || transformer.literals.contains(name) {
        return Ok(input.symbol_name() == Some(name));
    }

    Ok(bind_single_pattern(bindings, name, input))
}

fn match_pattern_list(
    pattern_items: &[Expr],
    input_items: &[Expr],
    transformer: &MacroTransformer,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    let ellipsis_count = pattern_items
        .iter()
        .filter(|expr| expr.symbol_name() == Some("..."))
        .count();

    if ellipsis_count == 0 {
        if pattern_items.len() != input_items.len() {
            return Ok(false);
        }

        for (pattern, input) in pattern_items.iter().zip(input_items.iter()) {
            if !match_pattern(pattern, input, transformer, bindings)? {
                return Ok(false);
            }
        }

        return Ok(true);
    }

    if ellipsis_count != 1 {
        return Err(syntax_error(
            pattern_items
                .iter()
                .find(|expr| expr.symbol_name() == Some("..."))
                .map_or(START_POS, |expr| expr.pos),
            "only one ellipsis is supported in a syntax-rules pattern",
        ));
    }

    let ellipsis_index = pattern_items
        .iter()
        .position(|expr| expr.symbol_name() == Some("..."))
        .expect("ellipsis_count checked above");

    if ellipsis_index == 0 || ellipsis_index + 1 != pattern_items.len() {
        return Err(syntax_error(
            pattern_items[ellipsis_index].pos,
            "ellipsis must appear at the end of a pattern",
        ));
    }

    let prefix = &pattern_items[..ellipsis_index - 1];
    if input_items.len() < prefix.len() {
        return Ok(false);
    }

    for (pattern, input) in prefix.iter().zip(input_items.iter()) {
        if !match_pattern(pattern, input, transformer, bindings)? {
            return Ok(false);
        }
    }

    let repeated_pattern = &pattern_items[ellipsis_index - 1];
    let repeated_name = pattern_variable_name(repeated_pattern, transformer).ok_or_else(|| {
        syntax_error(
            repeated_pattern.pos,
            "only symbol ellipsis patterns are supported",
        )
    })?;

    Ok(bind_repeated_pattern(
        bindings,
        repeated_name,
        &input_items[prefix.len()..],
    ))
}

fn pattern_variable_name<'a>(pattern: &'a Expr, transformer: &MacroTransformer) -> Option<&'a str> {
    let name = pattern.symbol_name()?;
    if name == "..." || name == transformer.name || transformer.literals.contains(name) {
        None
    } else {
        Some(name)
    }
}

fn bind_single_pattern(
    bindings: &mut HashMap<String, PatternBinding>,
    name: &str,
    expr: &Expr,
) -> bool {
    match bindings.get(name) {
        Some(PatternBinding::Single(existing)) => expr_equal(existing, expr),
        Some(PatternBinding::Repeated(_)) => false,
        None => {
            bindings.insert(name.to_string(), PatternBinding::Single(expr.clone()));
            true
        }
    }
}

fn bind_repeated_pattern(
    bindings: &mut HashMap<String, PatternBinding>,
    name: &str,
    exprs: &[Expr],
) -> bool {
    match bindings.get(name) {
        Some(PatternBinding::Repeated(existing)) => {
            existing.len() == exprs.len()
                && existing
                    .iter()
                    .zip(exprs.iter())
                    .all(|(left, right)| expr_equal(left, right))
        }
        Some(PatternBinding::Single(_)) => false,
        None => {
            bindings.insert(name.to_string(), PatternBinding::Repeated(exprs.to_vec()));
            true
        }
    }
}

struct TemplateInstantiator<'a> {
    transformer: &'a MacroTransformer,
    env: &'a EnvRef,
    bindings: &'a HashMap<String, PatternBinding>,
    free_renames: &'a mut HashMap<String, String>,
    context: &'a mut EvalContext,
}

impl<'a> TemplateInstantiator<'a> {
    fn new(
        transformer: &'a MacroTransformer,
        env: &'a EnvRef,
        bindings: &'a HashMap<String, PatternBinding>,
        free_renames: &'a mut HashMap<String, String>,
        context: &'a mut EvalContext,
    ) -> Self {
        Self {
            transformer,
            env,
            bindings,
            free_renames,
            context,
        }
    }

    fn instantiate(
        &mut self,
        template: &Expr,
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        match &template.kind {
            ExprKind::Symbol(name) => {
                self.instantiate_symbol(template.pos, name, scope_renames, repetition_index)
            }
            ExprKind::List(items) => {
                self.instantiate_list_template(template.pos, items, scope_renames, repetition_index)
            }
            _ => Ok(template.clone()),
        }
    }

    fn instantiate_symbol(
        &mut self,
        pos: SourcePos,
        name: &str,
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        if name == "..." {
            return Err(syntax_error(
                pos,
                "ellipsis cannot appear by itself in a template",
            ));
        }

        if let Some(binding) = self.bindings.get(name) {
            return instantiate_pattern_binding(name, pos, binding, repetition_index);
        }

        if let Some(renamed) = scope_renames.get(name) {
            return Ok(Expr::new(pos, ExprKind::Symbol(renamed.clone())));
        }

        if is_core_syntax_keyword(name) {
            return Ok(Expr::new(pos, ExprKind::Symbol(name.to_string())));
        }

        let renamed = self.ensure_free_identifier(name);
        Ok(Expr::new(pos, ExprKind::Symbol(renamed)))
    }

    fn instantiate_list_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        if let Some(head_name) = items
            .first()
            .and_then(Expr::symbol_name)
            .filter(|name| !self.bindings.contains_key(*name))
        {
            return match head_name {
                "let" => self.instantiate_let_template(pos, items, scope_renames, repetition_index),
                "lambda" => {
                    self.instantiate_lambda_template(pos, items, scope_renames, repetition_index)
                }
                _ => self.instantiate_plain_list_template(
                    pos,
                    items,
                    scope_renames,
                    repetition_index,
                ),
            };
        }

        self.instantiate_plain_list_template(pos, items, scope_renames, repetition_index)
    }

    fn instantiate_plain_list_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        let mut expanded = Vec::new();
        let mut index = 0;

        while index < items.len() {
            if items[index].symbol_name() == Some("...") {
                return Err(syntax_error(
                    items[index].pos,
                    "ellipsis must follow a template expression",
                ));
            }

            if index + 1 < items.len() && items[index + 1].symbol_name() == Some("...") {
                let repeat_count = template_repetition_count(&items[index], self.bindings)?;
                for repeat_index in 0..repeat_count {
                    expanded.push(self.instantiate(
                        &items[index],
                        scope_renames,
                        Some(repeat_index),
                    )?);
                }
                index += 2;
                continue;
            }

            expanded.push(self.instantiate(&items[index], scope_renames, repetition_index)?);
            index += 1;
        }

        Ok(Expr::new(pos, ExprKind::List(expanded)))
    }

    fn instantiate_let_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        let Some(bindings_items) = parse_bindings_template(items) else {
            return self.instantiate_plain_list_template(
                pos,
                items,
                scope_renames,
                repetition_index,
            );
        };

        let mut body_renames = scope_renames.clone();
        let mut expanded_bindings = Vec::with_capacity(bindings_items.len());

        for binding in bindings_items {
            let binding_items = binding.list_items().ok_or_else(|| {
                syntax_error(binding.pos, "macro-generated let bindings must be lists")
            })?;

            let [name_expr, value_expr] = binding_items else {
                return Err(syntax_error(
                    binding.pos,
                    "macro-generated let bindings must contain a name and value",
                ));
            };

            let expanded_value = self.instantiate(value_expr, scope_renames, repetition_index)?;
            let expanded_name =
                self.instantiate_binding_name(name_expr, &mut body_renames, repetition_index)?;

            expanded_bindings.push(Expr::new(
                binding.pos,
                ExprKind::List(vec![expanded_name, expanded_value]),
            ));
        }

        let mut expanded_items = Vec::with_capacity(items.len());
        expanded_items.push(Expr::new(items[0].pos, ExprKind::Symbol("let".into())));
        expanded_items.push(Expr::new(items[1].pos, ExprKind::List(expanded_bindings)));

        for body_expr in &items[2..] {
            expanded_items.push(self.instantiate(body_expr, &body_renames, repetition_index)?);
        }

        Ok(Expr::new(pos, ExprKind::List(expanded_items)))
    }

    fn instantiate_lambda_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        let Some(params_items) = parse_params_template(items) else {
            return self.instantiate_plain_list_template(
                pos,
                items,
                scope_renames,
                repetition_index,
            );
        };

        let mut body_renames = scope_renames.clone();
        let mut expanded_params = Vec::with_capacity(params_items.len());

        for param in params_items {
            if param.symbol_name() == Some(".") {
                expanded_params.push(param.clone());
                continue;
            }

            expanded_params.push(self.instantiate_binding_name(
                param,
                &mut body_renames,
                repetition_index,
            )?);
        }

        let mut expanded_items = Vec::with_capacity(items.len());
        expanded_items.push(Expr::new(items[0].pos, ExprKind::Symbol("lambda".into())));
        expanded_items.push(Expr::new(items[1].pos, ExprKind::List(expanded_params)));

        for body_expr in &items[2..] {
            expanded_items.push(self.instantiate(body_expr, &body_renames, repetition_index)?);
        }

        Ok(Expr::new(pos, ExprKind::List(expanded_items)))
    }

    fn instantiate_binding_name(
        &mut self,
        template: &Expr,
        body_renames: &mut HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        if let Some(name) = template.symbol_name() {
            if let Some(binding) = self.bindings.get(name) {
                return instantiate_pattern_binding(name, template.pos, binding, repetition_index);
            }

            let fresh = fresh_identifier(self.context, name);
            body_renames.insert(name.to_string(), fresh.clone());
            return Ok(Expr::new(template.pos, ExprKind::Symbol(fresh)));
        }

        self.instantiate(template, body_renames, repetition_index)
    }

    fn ensure_free_identifier(&mut self, name: &str) -> String {
        if let Some(existing) = self.free_renames.get(name) {
            return existing.clone();
        }

        let fresh = fresh_identifier(self.context, name);
        let definition_env = self
            .transformer
            .env
            .upgrade()
            .expect("macro definition environment should still exist");

        if let Some(cell) = env_lookup_cell(&definition_env, name) {
            env_define_alias(self.env, fresh.clone(), cell);
        } else if let Some(mac) = env_lookup_macro(&definition_env, name) {
            env_define_macro(self.env, fresh.clone(), mac);
        } else if let Some(builtin) = builtin_name(name) {
            env_define(
                self.env,
                fresh.clone(),
                Value::Procedure(Procedure::Builtin(builtin)),
            );
        }

        self.free_renames.insert(name.to_string(), fresh.clone());
        fresh
    }
}

fn instantiate_pattern_binding(
    name: &str,
    pos: SourcePos,
    binding: &PatternBinding,
    repetition_index: Option<usize>,
) -> Result<Expr, EvalError> {
    match binding {
        PatternBinding::Single(expr) => Ok(expr.clone()),
        PatternBinding::Repeated(exprs) => repetition_index
            .and_then(|index| exprs.get(index).cloned())
            .ok_or_else(|| {
                syntax_error(
                    pos,
                    format!("pattern variable {name} used outside ellipsis"),
                )
            }),
    }
}

fn parse_bindings_template(items: &[Expr]) -> Option<&[Expr]> {
    let bindings_expr = items.get(1)?;
    items.get(2)?;
    bindings_expr.list_items()
}

fn parse_params_template(items: &[Expr]) -> Option<&[Expr]> {
    let params_expr = items.get(1)?;
    items.get(2)?;
    params_expr.list_items()
}

fn fresh_identifier(context: &mut EvalContext, hint: &str) -> String {
    let id = context.next_fresh;
    context.next_fresh += 1;
    format!("__macro_{id}_{hint}")
}

fn is_core_syntax_keyword(name: &str) -> bool {
    matches!(
        name,
        "." | "and"
            | "begin"
            | "cond"
            | "define"
            | "define-syntax"
            | "else"
            | "if"
            | "lambda"
            | "let"
            | "or"
            | "quote"
            | "set!"
            | "syntax-rules"
    )
}

fn template_repetition_count(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
) -> Result<usize, EvalError> {
    let mut expected = None;
    collect_template_repetition_count(template, bindings, &mut expected)?;
    expected.ok_or_else(|| {
        syntax_error(
            template.pos,
            "ellipsis template must reference a repeated pattern variable",
        )
    })
}

fn collect_template_repetition_count(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    expected: &mut Option<usize>,
) -> Result<(), EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(PatternBinding::Repeated(values)) = bindings.get(name) {
                match expected {
                    Some(current) if *current != values.len() => {
                        return Err(syntax_error(
                            template.pos,
                            "repeated pattern variables must have the same length",
                        ))
                    }
                    Some(_) => {}
                    None => *expected = Some(values.len()),
                }
            }
        }
        ExprKind::List(items) => {
            for item in items {
                if item.symbol_name() == Some("...") {
                    continue;
                }
                collect_template_repetition_count(item, bindings, expected)?;
            }
        }
        _ => {}
    }

    Ok(())
}

fn expr_equal(left: &Expr, right: &Expr) -> bool {
    match (&left.kind, &right.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => left == right,
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => left == right,
        (ExprKind::Character(left), ExprKind::Character(right)) => left == right,
        (ExprKind::String(left), ExprKind::String(right)) => left == right,
        (ExprKind::Symbol(left), ExprKind::Symbol(right)) => left == right,
        (ExprKind::List(left), ExprKind::List(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_equal(left, right))
        }
        _ => false,
    }
}
