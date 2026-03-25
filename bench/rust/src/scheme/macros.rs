use super::{
    builtins::builtin_name, env_define, env_define_alias, env_define_macro, env_lookup_cell,
    env_lookup_macro, machine, make_list_value, syntax_error, type_mismatch, wrong_arg_count, Env,
    EnvRef, EnvWeak, EvalContext, EvalError, Expr, ExprKind, MacroKind, MacroRef, MacroRule,
    MacroTransformer, PatternBinding, Procedure, SourcePos, SyntaxObject, Value, START_POS,
};
use std::collections::{HashMap, HashSet};
use std::mem;
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
        kind: MacroKind::SyntaxRules {
            literals,
            rules: parsed_rules,
            env: Rc::downgrade(env),
        },
    }))
}

pub(super) fn expand_macro_call(
    transformer: &MacroRef,
    items: &[Expr],
    pos: SourcePos,
    env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Expr, EvalError> {
    match &transformer.kind {
        MacroKind::SyntaxRules {
            literals,
            rules,
            env: definition_env,
        } => SyntaxRulesExpander::new(&transformer.name, literals, rules, definition_env, env)
            .expand(items, pos, context),
        MacroKind::Procedure { transformer } => {
            expand_procedure_macro(transformer, items, pos, env, context)
        }
    }
}

pub(super) fn eval_syntax(
    args: &[Expr],
    _env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let [template] = args else {
        return Err(wrong_arg_count(
            pos,
            "syntax",
            "exactly 1 argument",
            args.len(),
        ));
    };

    if context.macro_expansions.is_empty() {
        return Ok(Value::Syntax(SyntaxObject::new(template.clone())));
    }

    let (definition_env, use_env, bindings, mut free_renames) = {
        let expansion = context
            .macro_expansions
            .last_mut()
            .expect("checked for active macro expansion");
        (
            Rc::clone(&expansion.definition_env),
            Rc::clone(&expansion.use_env),
            merged_pattern_bindings(&expansion.binding_scopes),
            mem::take(&mut expansion.free_renames),
        )
    };

    let expanded = instantiate_template(
        template,
        &definition_env,
        &use_env,
        &bindings,
        &mut free_renames,
        context,
    );

    context
        .macro_expansions
        .last_mut()
        .expect("active macro expansion must still exist")
        .free_renames = free_renames;

    expanded.map(|expr| Value::Syntax(SyntaxObject::new(expr)))
}

pub(super) fn eval_syntax_case(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (target_expr, rest) = args.split_first().ok_or_else(|| {
        syntax_error(pos, "syntax-case requires a target and at least one clause")
    })?;
    let (literals_expr, clauses) = rest.split_first().ok_or_else(|| {
        syntax_error(
            pos,
            "syntax-case requires a literals list and at least one clause",
        )
    })?;

    if clauses.is_empty() {
        return Err(syntax_error(
            pos,
            "syntax-case requires at least one clause",
        ));
    }

    let target = machine::eval_expr(target_expr, env, context)?;
    let target = expect_syntax("syntax-case", &target, target_expr.pos)?;

    let literal_items = literals_expr
        .list_items()
        .ok_or_else(|| syntax_error(literals_expr.pos, "syntax-case literals must be a list"))?;
    let mut literals = HashSet::new();
    for literal in literal_items {
        let literal_name = literal
            .symbol_name()
            .ok_or_else(|| syntax_error(literal.pos, "syntax-case literals must be symbols"))?;
        literals.insert(literal_name.to_string());
    }

    for clause in clauses {
        let items = clause
            .list_items()
            .ok_or_else(|| syntax_error(clause.pos, "syntax-case clauses must be lists"))?;

        let (pattern, fender, body) =
            match items {
                [pattern, body] => (pattern, None, body),
                [pattern, fender, body] => (pattern, Some(fender), body),
                _ => return Err(syntax_error(
                    clause.pos,
                    "syntax-case clauses must contain a pattern, an optional fender, and a body",
                )),
            };

        let mut bindings = HashMap::new();
        if !match_pattern(pattern, &target.expr, &literals, None, &mut bindings)? {
            continue;
        }

        let clause_env = Env::new_child(env);
        bind_pattern_values(&clause_env, &bindings);
        push_binding_scope(context, bindings);

        let matched = match fender {
            Some(fender) => machine::eval_expr(fender, &clause_env, context)?.is_truthy(),
            None => true,
        };

        if matched {
            let result = machine::eval_expr(body, &clause_env, context);
            pop_binding_scope(context);
            return result;
        }

        pop_binding_scope(context);
    }

    Err(syntax_error(pos, "no matching syntax-case clause"))
}

pub(super) fn eval_with_syntax(
    args: &[Expr],
    env: &EnvRef,
    pos: SourcePos,
    context: &mut EvalContext,
) -> Result<Value, EvalError> {
    let (bindings_expr, body) = args
        .split_first()
        .ok_or_else(|| syntax_error(pos, "with-syntax requires bindings and a body"))?;

    if body.is_empty() {
        return Err(syntax_error(pos, "with-syntax requires a body"));
    }

    let binding_specs = bindings_expr
        .list_items()
        .ok_or_else(|| syntax_error(bindings_expr.pos, "with-syntax bindings must be a list"))?;
    let with_env = Env::new_child(env);
    let mut all_bindings = HashMap::new();

    for binding in binding_specs {
        let items = binding
            .list_items()
            .ok_or_else(|| syntax_error(binding.pos, "with-syntax bindings must be lists"))?;

        let [pattern, value_expr] = items else {
            return Err(syntax_error(
                binding.pos,
                "each with-syntax binding must contain a pattern and a value",
            ));
        };

        let value = machine::eval_expr(value_expr, &with_env, context)?;
        let value = expect_syntax("with-syntax", &value, value_expr.pos)?;

        let mut bindings = HashMap::new();
        if !match_pattern(pattern, &value.expr, &HashSet::new(), None, &mut bindings)? {
            return Err(syntax_error(
                pattern.pos,
                "with-syntax pattern did not match the provided syntax object",
            ));
        }

        merge_binding_maps(&mut all_bindings, bindings.clone(), binding.pos)?;
        bind_pattern_values(&with_env, &bindings);
    }

    push_binding_scope(context, all_bindings);
    let result = machine::eval_sequence(body, &with_env, pos, context);
    pop_binding_scope(context);
    result
}

fn expand_procedure_macro(
    transformer: &Value,
    items: &[Expr],
    pos: SourcePos,
    use_env: &EnvRef,
    context: &mut EvalContext,
) -> Result<Expr, EvalError> {
    let call_expr = Expr::new(pos, ExprKind::List(items.to_vec()));
    let definition_env = transformer_definition_env(transformer, use_env);
    context.macro_expansions.push(super::MacroExpansion {
        definition_env,
        use_env: Rc::clone(use_env),
        binding_scopes: Vec::new(),
        free_renames: HashMap::new(),
    });

    let result = machine::apply_value(
        transformer.clone(),
        vec![Value::Syntax(SyntaxObject::new(call_expr))],
        pos,
        context,
    );

    context
        .macro_expansions
        .pop()
        .expect("macro expansion stack should not be empty");

    match result? {
        Value::Syntax(syntax) => Ok(syntax.expr.clone()),
        other => Err(type_mismatch(
            pos,
            "define-syntax",
            "syntax",
            other.type_name(),
        )),
    }
}

fn transformer_definition_env(transformer: &Value, fallback: &EnvRef) -> EnvRef {
    match transformer {
        Value::Procedure(Procedure::Lambda(lambda)) => Rc::clone(&lambda.env),
        Value::Procedure(Procedure::CaseLambda(case_lambda)) => case_lambda
            .clauses
            .first()
            .map(|clause| Rc::clone(&clause.env))
            .unwrap_or_else(|| Rc::clone(fallback)),
        _ => Rc::clone(fallback),
    }
}

struct SyntaxRulesExpander<'a> {
    name: &'a str,
    literals: &'a HashSet<String>,
    rules: &'a [MacroRule],
    definition_env: EnvRef,
    use_env: &'a EnvRef,
}

impl<'a> SyntaxRulesExpander<'a> {
    fn new(
        name: &'a str,
        literals: &'a HashSet<String>,
        rules: &'a [MacroRule],
        definition_env: &EnvWeak,
        use_env: &'a EnvRef,
    ) -> Self {
        Self {
            name,
            literals,
            rules,
            definition_env: definition_env
                .upgrade()
                .expect("macro definition environment should still exist"),
            use_env,
        }
    }

    fn expand(
        &self,
        items: &[Expr],
        pos: SourcePos,
        context: &mut EvalContext,
    ) -> Result<Expr, EvalError> {
        let call_expr = self.normalize_call(items, pos);

        for rule in self.rules {
            let mut bindings = HashMap::new();
            if !match_pattern(
                &rule.pattern,
                &call_expr,
                self.literals,
                Some(self.name),
                &mut bindings,
            )? {
                continue;
            }

            let mut free_renames = HashMap::new();
            return instantiate_template(
                &rule.template,
                &self.definition_env,
                self.use_env,
                &bindings,
                &mut free_renames,
                context,
            );
        }

        Err(syntax_error(
            pos,
            format!("no matching syntax-rules pattern for {}", self.name),
        ))
    }

    fn normalize_call(&self, items: &[Expr], pos: SourcePos) -> Expr {
        let mut normalized_items = items.to_vec();
        if let Some(head) = normalized_items.first_mut() {
            *head = Expr::new(head.pos, ExprKind::Symbol(self.name.to_string()));
        }
        Expr::new(pos, ExprKind::List(normalized_items))
    }
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
    head_literal: Option<&str>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    match (&pattern.kind, &input.kind) {
        (ExprKind::Number(left), ExprKind::Number(right)) => Ok(left == right),
        (ExprKind::Boolean(left), ExprKind::Boolean(right)) => Ok(left == right),
        (ExprKind::Character(left), ExprKind::Character(right)) => Ok(left == right),
        (ExprKind::String(left), ExprKind::String(right)) => Ok(left == right),
        (ExprKind::Symbol(name), _) => {
            match_pattern_symbol(name, pattern.pos, input, literals, head_literal, bindings)
        }
        (ExprKind::List(pattern_items), ExprKind::List(input_items)) => {
            match_pattern_list(pattern_items, input_items, literals, head_literal, bindings)
        }
        (ExprKind::List(_), ExprKind::DottedList(_, _))
        | (ExprKind::DottedList(_, _), ExprKind::List(_))
        | (ExprKind::DottedList(_, _), ExprKind::DottedList(_, _)) => {
            let (pattern_items, pattern_tail) = pattern
                .list_parts()
                .expect("list-like pattern must provide list parts");
            let (input_items, input_tail) = input
                .list_parts()
                .expect("list-like input must provide list parts");
            match_pattern_pair_like(
                pattern_items,
                pattern_tail,
                input_items,
                input_tail,
                literals,
                head_literal,
                bindings,
            )
        }
        (ExprKind::Vector(pattern_items), ExprKind::Vector(input_items)) => {
            match_pattern_list(pattern_items, input_items, literals, head_literal, bindings)
        }
        _ => Ok(false),
    }
}

fn match_pattern_symbol(
    name: &str,
    pos: SourcePos,
    input: &Expr,
    literals: &HashSet<String>,
    head_literal: Option<&str>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    if name == "..." {
        return Err(syntax_error(
            pos,
            "ellipsis cannot appear by itself in a pattern",
        ));
    }

    if head_literal == Some(name) || literals.contains(name) {
        return Ok(input.symbol_name() == Some(name));
    }

    Ok(bind_single_pattern(bindings, name, input))
}

fn match_pattern_list(
    pattern_items: &[Expr],
    input_items: &[Expr],
    literals: &HashSet<String>,
    head_literal: Option<&str>,
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
            if !match_pattern(pattern, input, literals, head_literal, bindings)? {
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
            "only one ellipsis is supported in a pattern",
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
        if !match_pattern(pattern, input, literals, head_literal, bindings)? {
            return Ok(false);
        }
    }

    let repeated_pattern = &pattern_items[ellipsis_index - 1];
    let repeated_name = pattern_variable_name(repeated_pattern, literals, head_literal)
        .ok_or_else(|| {
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

fn match_pattern_pair_like(
    pattern_items: &[Expr],
    pattern_tail: Option<&Expr>,
    input_items: &[Expr],
    input_tail: Option<&Expr>,
    literals: &HashSet<String>,
    head_literal: Option<&str>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> Result<bool, EvalError> {
    if pattern_items.is_empty() {
        return match pattern_tail {
            Some(pattern_tail) => {
                let input_expr = build_list_like_expr(input_items, input_tail);
                match_pattern(pattern_tail, &input_expr, literals, head_literal, bindings)
            }
            None => Ok(input_items.is_empty() && input_tail.is_none()),
        };
    }

    let Some((pattern_first, pattern_rest)) = pattern_items.split_first() else {
        return Ok(false);
    };
    let Some((input_first, input_rest)) = input_items.split_first() else {
        return match input_tail {
            Some(input_tail) => {
                let pattern_expr = build_list_like_expr(pattern_items, pattern_tail);
                match_pattern(&pattern_expr, input_tail, literals, head_literal, bindings)
            }
            None => Ok(false),
        };
    };

    if !match_pattern(pattern_first, input_first, literals, head_literal, bindings)? {
        return Ok(false);
    }

    let pattern_rest_expr = build_list_like_expr(pattern_rest, pattern_tail);
    let input_rest_expr = build_list_like_expr(input_rest, input_tail);
    match_pattern(
        &pattern_rest_expr,
        &input_rest_expr,
        literals,
        head_literal,
        bindings,
    )
}

fn build_list_like_expr(items: &[Expr], tail: Option<&Expr>) -> Expr {
    match (items, tail) {
        ([], None) => Expr::new(START_POS, ExprKind::List(Vec::new())),
        ([], Some(tail)) => tail.clone(),
        (_, None) => Expr::new(START_POS, ExprKind::List(items.to_vec())),
        (_, Some(tail)) => match tail.list_parts() {
            Some((tail_items, None)) => {
                let mut combined = items.to_vec();
                combined.extend_from_slice(tail_items);
                Expr::new(START_POS, ExprKind::List(combined))
            }
            Some((tail_items, Some(tail_tail))) => {
                let mut combined = items.to_vec();
                combined.extend_from_slice(tail_items);
                Expr::new(
                    START_POS,
                    ExprKind::DottedList(combined, Box::new(tail_tail.clone())),
                )
            }
            None => Expr::new(
                START_POS,
                ExprKind::DottedList(items.to_vec(), Box::new(tail.clone())),
            ),
        },
    }
}

fn pattern_variable_name<'a>(
    pattern: &'a Expr,
    literals: &HashSet<String>,
    head_literal: Option<&str>,
) -> Option<&'a str> {
    let name = pattern.symbol_name()?;
    if name == "..." || head_literal == Some(name) || literals.contains(name) {
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

fn instantiate_template(
    template: &Expr,
    definition_env: &EnvRef,
    use_env: &EnvRef,
    bindings: &HashMap<String, PatternBinding>,
    free_renames: &mut HashMap<String, String>,
    context: &mut EvalContext,
) -> Result<Expr, EvalError> {
    let scope_renames = HashMap::new();
    let mut instantiator =
        TemplateInstantiator::new(definition_env, use_env, bindings, free_renames, context);
    instantiator.instantiate(template, &scope_renames, None)
}

struct TemplateInstantiator<'a> {
    definition_env: &'a EnvRef,
    use_env: &'a EnvRef,
    bindings: &'a HashMap<String, PatternBinding>,
    free_renames: &'a mut HashMap<String, String>,
    context: &'a mut EvalContext,
}

impl<'a> TemplateInstantiator<'a> {
    fn new(
        definition_env: &'a EnvRef,
        use_env: &'a EnvRef,
        bindings: &'a HashMap<String, PatternBinding>,
        free_renames: &'a mut HashMap<String, String>,
        context: &'a mut EvalContext,
    ) -> Self {
        Self {
            definition_env,
            use_env,
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
            ExprKind::DottedList(items, tail) => self.instantiate_dotted_list_template(
                template.pos,
                items,
                tail,
                scope_renames,
                repetition_index,
            ),
            ExprKind::Vector(items) => self.instantiate_vector_template(
                template.pos,
                items,
                scope_renames,
                repetition_index,
            ),
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
                "quote" => Ok(Expr::new(pos, ExprKind::List(items.to_vec()))),
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
        self.instantiate_sequence_items(items, scope_renames, repetition_index)
            .map(|expanded| Expr::new(pos, ExprKind::List(expanded)))
    }

    fn instantiate_vector_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        self.instantiate_sequence_items(items, scope_renames, repetition_index)
            .map(|expanded| Expr::new(pos, ExprKind::Vector(expanded)))
    }

    fn instantiate_dotted_list_template(
        &mut self,
        pos: SourcePos,
        items: &[Expr],
        tail: &Expr,
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Expr, EvalError> {
        let expanded_items =
            self.instantiate_sequence_items(items, scope_renames, repetition_index)?;
        let expanded_tail = self.instantiate(tail, scope_renames, repetition_index)?;
        let mut expanded = build_list_like_expr(&expanded_items, Some(&expanded_tail));
        expanded.pos = pos;
        Ok(expanded)
    }

    fn instantiate_sequence_items(
        &mut self,
        items: &[Expr],
        scope_renames: &HashMap<String, String>,
        repetition_index: Option<usize>,
    ) -> Result<Vec<Expr>, EvalError> {
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

        Ok(expanded)
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
        let Some((params_items, tail)) = parse_params_template(items) else {
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

        let params_expr = match tail {
            Some(tail) => {
                let expanded_tail =
                    self.instantiate_binding_name(tail, &mut body_renames, repetition_index)?;
                let mut expanded = build_list_like_expr(&expanded_params, Some(&expanded_tail));
                expanded.pos = items[1].pos;
                expanded
            }
            None => Expr::new(items[1].pos, ExprKind::List(expanded_params)),
        };

        let mut expanded_items = Vec::with_capacity(items.len());
        expanded_items.push(Expr::new(items[0].pos, ExprKind::Symbol("lambda".into())));
        expanded_items.push(params_expr);

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

        if let Some(cell) = env_lookup_cell(self.definition_env, name) {
            env_define_alias(self.use_env, fresh.clone(), cell);
        } else if let Some(mac) = env_lookup_macro(self.definition_env, name) {
            env_define_macro(self.use_env, fresh.clone(), mac);
        } else if let Some(builtin) = builtin_name(name) {
            env_define(
                self.use_env,
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

fn parse_params_template(items: &[Expr]) -> Option<(&[Expr], Option<&Expr>)> {
    let params_expr = items.get(1)?;
    items.get(2)?;
    params_expr.list_parts()
}

fn push_binding_scope(context: &mut EvalContext, bindings: HashMap<String, PatternBinding>) {
    context
        .macro_expansions
        .last_mut()
        .expect("syntax bindings require an active macro expansion")
        .binding_scopes
        .push(bindings);
}

fn pop_binding_scope(context: &mut EvalContext) {
    context
        .macro_expansions
        .last_mut()
        .expect("syntax bindings require an active macro expansion")
        .binding_scopes
        .pop()
        .expect("syntax binding stack should not be empty");
}

fn merged_pattern_bindings(
    scopes: &[HashMap<String, PatternBinding>],
) -> HashMap<String, PatternBinding> {
    let mut merged = HashMap::new();
    for scope in scopes {
        for (name, binding) in scope {
            merged.insert(name.clone(), binding.clone());
        }
    }
    merged
}

fn bind_pattern_values(env: &EnvRef, bindings: &HashMap<String, PatternBinding>) {
    for (name, binding) in bindings {
        let value = match binding {
            PatternBinding::Single(expr) => Value::Syntax(SyntaxObject::new(expr.clone())),
            PatternBinding::Repeated(exprs) => make_list_value(
                exprs
                    .iter()
                    .cloned()
                    .map(SyntaxObject::new)
                    .map(Value::Syntax)
                    .collect(),
            ),
        };
        env_define(env, name.clone(), value);
    }
}

fn merge_binding_maps(
    target: &mut HashMap<String, PatternBinding>,
    bindings: HashMap<String, PatternBinding>,
    pos: SourcePos,
) -> Result<(), EvalError> {
    for (name, binding) in bindings {
        match target.get(&name) {
            Some(existing) if !pattern_binding_equal(existing, &binding) => {
                return Err(syntax_error(
                    pos,
                    format!("conflicting pattern binding for {name}"),
                ))
            }
            Some(_) => {}
            None => {
                target.insert(name, binding);
            }
        }
    }
    Ok(())
}

fn pattern_binding_equal(left: &PatternBinding, right: &PatternBinding) -> bool {
    match (left, right) {
        (PatternBinding::Single(left), PatternBinding::Single(right)) => expr_equal(left, right),
        (PatternBinding::Repeated(left), PatternBinding::Repeated(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_equal(left, right))
        }
        _ => false,
    }
}

fn expect_syntax(name: &str, value: &Value, pos: SourcePos) -> Result<Rc<SyntaxObject>, EvalError> {
    match value {
        Value::Syntax(syntax) => Ok(Rc::clone(syntax)),
        other => Err(type_mismatch(pos, name, "syntax", other.type_name())),
    }
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
            | "case"
            | "case-lambda"
            | "cond"
            | "define"
            | "define-record-type"
            | "define-syntax"
            | "do"
            | "else"
            | "=>"
            | "guard"
            | "if"
            | "lambda"
            | "let"
            | "let*"
            | "letrec"
            | "letrec*"
            | "or"
            | "quasiquote"
            | "quote"
            | "set!"
            | "syntax"
            | "syntax-case"
            | "syntax-rules"
            | "unquote"
            | "unquote-splicing"
            | "with-syntax"
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
        ExprKind::DottedList(items, tail) => {
            for item in items {
                if item.symbol_name() == Some("...") {
                    continue;
                }
                collect_template_repetition_count(item, bindings, expected)?;
            }
            collect_template_repetition_count(tail, bindings, expected)?;
        }
        ExprKind::Vector(items) => {
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
        (ExprKind::List(_), ExprKind::DottedList(_, _))
        | (ExprKind::DottedList(_, _), ExprKind::List(_))
        | (ExprKind::DottedList(_, _), ExprKind::DottedList(_, _)) => {
            let (left_items, left_tail) = left
                .list_parts()
                .expect("list-like expression must provide list parts");
            let (right_items, right_tail) = right
                .list_parts()
                .expect("list-like expression must provide list parts");
            list_like_expr_equal(left_items, left_tail, right_items, right_tail)
        }
        (ExprKind::Vector(left), ExprKind::Vector(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right.iter())
                    .all(|(left, right)| expr_equal(left, right))
        }
        _ => false,
    }
}

fn list_like_expr_equal(
    left_items: &[Expr],
    left_tail: Option<&Expr>,
    right_items: &[Expr],
    right_tail: Option<&Expr>,
) -> bool {
    if left_items.is_empty() {
        return match left_tail {
            Some(left_tail) => {
                expr_equal(left_tail, &build_list_like_expr(right_items, right_tail))
            }
            None => right_items.is_empty() && right_tail.is_none(),
        };
    }

    let Some((left_first, left_rest)) = left_items.split_first() else {
        return false;
    };
    let Some((right_first, right_rest)) = right_items.split_first() else {
        return match right_tail {
            Some(right_tail) => {
                expr_equal(&build_list_like_expr(left_items, left_tail), right_tail)
            }
            None => false,
        };
    };

    expr_equal(left_first, right_first)
        && list_like_expr_equal(left_rest, left_tail, right_rest, right_tail)
}
