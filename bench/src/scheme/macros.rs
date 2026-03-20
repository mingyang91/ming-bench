use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::scheme::ast::{expr_ref, list_expr, Expr, ExprRef, Identifier};
use crate::scheme::env::{snapshot_plain_bindings, CellRef, EnvRef};
use crate::scheme::error::EvalError;

pub type MacroRef = Rc<SyntaxRules>;
pub type MacroBindings = HashMap<String, MacroRef>;

#[derive(Clone, Debug)]
pub struct SyntaxRules {
    name: String,
    literals: HashSet<String>,
    rules: Vec<MacroRule>,
    value_bindings: HashMap<String, CellRef>,
    macro_bindings: MacroBindings,
}

#[derive(Clone, Debug)]
struct MacroRule {
    pattern: ExprRef,
    template: ExprRef,
}

#[derive(Debug, Default)]
pub struct CaptureStore {
    next_id: u64,
    values: HashMap<u64, CellRef>,
    macros: HashMap<u64, MacroRef>,
}

#[derive(Clone, Debug)]
enum MatchValue {
    One(ExprRef),
    Many(Vec<ExprRef>),
}

type MatchMap = HashMap<String, MatchValue>;

struct ExpansionState<'a> {
    rule_set: &'a SyntaxRules,
    bindings: &'a MatchMap,
    captures: &'a mut CaptureStore,
    introduced: HashMap<String, Identifier>,
}

pub fn is_define_syntax(expr: &ExprRef) -> bool {
    expr.as_ref()
        .list_items()
        .and_then(|items| items.first())
        .is_some_and(|head| head.as_ref().is_symbol_named("define-syntax"))
}

pub fn install_macro(
    expr: ExprRef,
    env: EnvRef,
    macros: &MacroBindings,
) -> Result<MacroBindings, EvalError> {
    let items = list_items(&expr, "define-syntax")?;
    if items.len() != 3 {
        return invalid_macro("define-syntax", "expected a name and transformer");
    }
    let name = symbol_name(&items[1], "define-syntax")?;
    let rule_set = parse_syntax_rules(name.clone(), &items[2], env, macros)?;
    let mut updated = macros.clone();
    updated.insert(name, Rc::new(rule_set));
    Ok(updated)
}

pub fn expand(
    expr: ExprRef,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    match expr.clone().as_ref() {
        Expr::List(items) => expand_list(expr, items, macros, captures),
        Expr::DottedList(items, tail) => expand_dotted(items, tail, macros, captures),
        _ => Ok(expr),
    }
}

impl CaptureStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn capture_value(&mut self, name: &str, cell: CellRef) -> Identifier {
        let id = self.next_identifier();
        self.values.insert(id, cell);
        Identifier::CapturedValue {
            name: name.to_owned(),
            id,
        }
    }

    pub fn capture_macro(&mut self, name: &str, rule_set: MacroRef) -> Identifier {
        let id = self.next_identifier();
        self.macros.insert(id, rule_set);
        Identifier::CapturedMacro {
            name: name.to_owned(),
            id,
        }
    }

    pub fn fresh_gensym(&mut self, name: &str) -> Identifier {
        Identifier::Gensym {
            name: name.to_owned(),
            id: self.next_identifier(),
        }
    }

    pub fn lookup_value(&self, id: u64) -> Option<CellRef> {
        self.values.get(&id).cloned()
    }

    pub fn lookup_macro(&self, id: u64) -> Option<MacroRef> {
        self.macros.get(&id).cloned()
    }

    fn next_identifier(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

fn parse_syntax_rules(
    name: String,
    expr: &ExprRef,
    env: EnvRef,
    macros: &MacroBindings,
) -> Result<SyntaxRules, EvalError> {
    let items = list_items(expr, "syntax-rules")?;
    if items.len() < 3 || !items[0].as_ref().is_symbol_named("syntax-rules") {
        return invalid_macro("define-syntax", "transformer must be syntax-rules");
    }
    let literals = parse_literals(&items[1], &name)?;
    let rules = parse_rules(&items[2..])?;
    Ok(SyntaxRules {
        name,
        literals,
        rules,
        value_bindings: snapshot_plain_bindings(&env),
        macro_bindings: macros.clone(),
    })
}

fn parse_literals(expr: &ExprRef, name: &str) -> Result<HashSet<String>, EvalError> {
    let items = list_items(expr, "syntax-rules")?;
    let mut literals = HashSet::from([name.to_owned()]);
    for item in items {
        literals.insert(symbol_name(item, "syntax-rules")?);
    }
    Ok(literals)
}

fn parse_rules(items: &[ExprRef]) -> Result<Vec<MacroRule>, EvalError> {
    items.iter().map(parse_rule).collect()
}

fn parse_rule(expr: &ExprRef) -> Result<MacroRule, EvalError> {
    let items = list_items(expr, "syntax-rules rule")?;
    if items.len() != 2 {
        return invalid_macro("syntax-rules", "each rule must have a pattern and template");
    }
    Ok(MacroRule {
        pattern: items[0].clone(),
        template: items[1].clone(),
    })
}

fn expand_list(
    expr: ExprRef,
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.is_empty() {
        return Ok(expr);
    }
    if let Some(rule_set) = resolve_macro(&items[0], macros, captures) {
        let expanded = apply_macro(rule_set, &expr, captures)?;
        return expand(expanded, macros, captures);
    }
    match items[0].as_ref().symbol_name() {
        Some("quote") => Ok(expr),
        Some("if") => expand_if(items, macros, captures),
        Some("begin") => expand_tail(items, 1, macros, captures),
        Some("lambda") => expand_lambda(items, macros, captures),
        Some("define") => expand_define(items, macros, captures),
        Some("set!") => expand_set(items, macros, captures),
        Some("let") => expand_let(items, macros, captures),
        Some("cond") => expand_cond(items, macros, captures),
        _ => expand_all(items, macros, captures),
    }
}

fn expand_dotted(
    items: &[ExprRef],
    tail: &ExprRef,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let head = expand_many(items, macros, captures)?;
    let expanded_tail = expand(tail.clone(), macros, captures)?;
    Ok(expr_ref(Expr::DottedList(head, expanded_tail)))
}

fn expand_if(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.len() < 2 {
        return Ok(list_expr(items.to_vec()));
    }
    let mut expanded = vec![items[0].clone(), expand(items[1].clone(), macros, captures)?];
    expanded.extend(expand_many(&items[2..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_lambda(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.len() < 2 {
        return Ok(list_expr(items.to_vec()));
    }
    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.extend(expand_many(&items[2..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_define(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.len() < 2 {
        return Ok(list_expr(items.to_vec()));
    }
    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.extend(expand_many(&items[2..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_set(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.len() != 3 {
        return Ok(list_expr(items.to_vec()));
    }
    Ok(list_expr(vec![
        items[0].clone(),
        items[1].clone(),
        expand(items[2].clone(), macros, captures)?,
    ]))
}

fn expand_let(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    if items.len() < 2 {
        return Ok(list_expr(items.to_vec()));
    }
    if items.len() >= 3 && matches!(items[1].as_ref(), Expr::Symbol(_)) {
        return expand_named_let(items, macros, captures);
    }
    expand_plain_let(items, macros, captures)
}

fn expand_named_let(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let mut expanded = vec![items[0].clone(), items[1].clone()];
    expanded.push(expand_binding_list(&items[2], macros, captures)?);
    expanded.extend(expand_many(&items[3..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_plain_let(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let mut expanded = vec![items[0].clone()];
    expanded.push(expand_binding_list(&items[1], macros, captures)?);
    expanded.extend(expand_many(&items[2..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_binding_list(
    expr: &ExprRef,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let Expr::List(bindings) = expr.as_ref() else {
        return Ok(expr.clone());
    };
    let expanded = bindings
        .iter()
        .map(|binding| expand_binding(binding, macros, captures))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(list_expr(expanded))
}

fn expand_binding(
    expr: &ExprRef,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let Expr::List(parts) = expr.as_ref() else {
        return Ok(expr.clone());
    };
    if parts.len() != 2 {
        return Ok(expr.clone());
    }
    Ok(list_expr(vec![
        parts[0].clone(),
        expand(parts[1].clone(), macros, captures)?,
    ]))
}

fn expand_cond(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let mut expanded = vec![items[0].clone()];
    for clause in &items[1..] {
        expanded.push(expand_cond_clause(clause, macros, captures)?);
    }
    Ok(list_expr(expanded))
}

fn expand_cond_clause(
    expr: &ExprRef,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let Expr::List(items) = expr.as_ref() else {
        return Ok(expr.clone());
    };
    let Some((test, body)) = items.split_first() else {
        return Ok(expr.clone());
    };
    let expanded_test = if test.as_ref().is_symbol_named("else") {
        test.clone()
    } else {
        expand(test.clone(), macros, captures)?
    };
    let mut expanded = vec![expanded_test];
    expanded.extend(expand_many(body, macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_tail(
    items: &[ExprRef],
    start: usize,
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    let mut expanded = items[..start].to_vec();
    expanded.extend(expand_many(&items[start..], macros, captures)?);
    Ok(list_expr(expanded))
}

fn expand_all(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    Ok(list_expr(expand_many(items, macros, captures)?))
}

fn expand_many(
    items: &[ExprRef],
    macros: &MacroBindings,
    captures: &mut CaptureStore,
) -> Result<Vec<ExprRef>, EvalError> {
    items
        .iter()
        .map(|item| expand(item.clone(), macros, captures))
        .collect()
}

fn resolve_macro(
    expr: &ExprRef,
    macros: &MacroBindings,
    captures: &CaptureStore,
) -> Option<MacroRef> {
    match expr.as_ref() {
        Expr::Symbol(identifier) => identifier
            .captured_macro_id()
            .and_then(|id| captures.lookup_macro(id))
            .or_else(|| macros.get(identifier.name()).cloned()),
        _ => None,
    }
}

fn apply_macro(
    rule_set: MacroRef,
    invocation: &ExprRef,
    captures: &mut CaptureStore,
) -> Result<ExprRef, EvalError> {
    for rule in &rule_set.rules {
        if let Some(bindings) = match_rule(&rule.pattern, invocation, &rule_set.literals) {
            let mut state = ExpansionState {
                rule_set: &rule_set,
                bindings: &bindings,
                captures,
                introduced: HashMap::new(),
            };
            return instantiate(&rule.template, &mut state, None);
        }
    }
    Err(EvalError::MacroNoMatch {
        name: rule_set.name.clone(),
    })
}

fn match_rule(pattern: &ExprRef, expr: &ExprRef, literals: &HashSet<String>) -> Option<MatchMap> {
    let mut bindings = HashMap::new();
    if match_node(pattern, expr, literals, &mut bindings, false) {
        Some(bindings)
    } else {
        None
    }
}

fn match_node(
    pattern: &ExprRef,
    expr: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MatchMap,
    repeated: bool,
) -> bool {
    match pattern.as_ref() {
        Expr::Symbol(identifier) => match_symbol(identifier, expr, literals, bindings, repeated),
        Expr::Number(left) => matches!(expr.as_ref(), Expr::Number(right) if left == right),
        Expr::Bool(left) => matches!(expr.as_ref(), Expr::Bool(right) if left == right),
        Expr::String(left) => matches!(expr.as_ref(), Expr::String(right) if left == right),
        Expr::List(items) => match_list(items, expr, literals, bindings),
        Expr::DottedList(items, tail) => match_dotted(items, tail, expr, literals, bindings),
    }
}

fn match_symbol(
    identifier: &Identifier,
    expr: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MatchMap,
    repeated: bool,
) -> bool {
    let name = identifier.name();
    if name == "..." {
        return false;
    }
    if literals.contains(name) {
        return expr.as_ref().symbol_name().is_some_and(|symbol| symbol == name);
    }
    bind_pattern_variable(name, expr, bindings, repeated);
    true
}

fn match_list(
    patterns: &[ExprRef],
    expr: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MatchMap,
) -> bool {
    let Expr::List(items) = expr.as_ref() else {
        return false;
    };
    if let Some(repeated_pattern) = tail_ellipsis(patterns) {
        return match_list_with_tail(patterns, repeated_pattern, items, literals, bindings);
    }
    patterns.len() == items.len()
        && patterns
            .iter()
            .zip(items)
            .all(|(pattern, value)| match_node(pattern, value, literals, bindings, false))
}

fn match_dotted(
    patterns: &[ExprRef],
    tail_pattern: &ExprRef,
    expr: &ExprRef,
    literals: &HashSet<String>,
    bindings: &mut MatchMap,
) -> bool {
    let Expr::DottedList(items, tail) = expr.as_ref() else {
        return false;
    };
    patterns.len() == items.len()
        && patterns
            .iter()
            .zip(items)
            .all(|(pattern, value)| match_node(pattern, value, literals, bindings, false))
        && match_node(tail_pattern, tail, literals, bindings, false)
}

fn match_list_with_tail(
    patterns: &[ExprRef],
    repeated_pattern: &ExprRef,
    items: &[ExprRef],
    literals: &HashSet<String>,
    bindings: &mut MatchMap,
) -> bool {
    let prefix = &patterns[..patterns.len() - 2];
    if items.len() < prefix.len() {
        return false;
    }
    if !prefix
        .iter()
        .zip(items)
        .all(|(pattern, value)| match_node(pattern, value, literals, bindings, false))
    {
        return false;
    }
    items[prefix.len()..]
        .iter()
        .all(|value| match_node(repeated_pattern, value, literals, bindings, true))
}

fn bind_pattern_variable(name: &str, expr: &ExprRef, bindings: &mut MatchMap, repeated: bool) {
    if repeated {
        match bindings.get_mut(name) {
            Some(MatchValue::Many(values)) => values.push(expr.clone()),
            _ => {
                bindings.insert(name.to_owned(), MatchValue::Many(vec![expr.clone()]));
            }
        }
        return;
    }
    bindings
        .entry(name.to_owned())
        .or_insert_with(|| MatchValue::One(expr.clone()));
}

fn instantiate(
    template: &ExprRef,
    state: &mut ExpansionState<'_>,
    index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    match template.as_ref() {
        Expr::Symbol(identifier) => instantiate_symbol(identifier, state, index),
        Expr::Number(_) | Expr::Bool(_) | Expr::String(_) => Ok(template.clone()),
        Expr::List(items) => instantiate_list(items, state, index),
        Expr::DottedList(items, tail) => instantiate_dotted(items, tail, state, index),
    }
}

fn instantiate_symbol(
    identifier: &Identifier,
    state: &mut ExpansionState<'_>,
    index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    if let Some(value) = instantiate_match(identifier.name(), state.bindings, index) {
        return Ok(value);
    }
    if identifier.name() == "..." {
        return invalid_macro("syntax-rules", "ellipsis may only appear after a template");
    }
    Ok(expr_ref(Expr::Symbol(introduced_identifier(
        identifier.name(),
        state,
    ))))
}

fn instantiate_list(
    items: &[ExprRef],
    state: &mut ExpansionState<'_>,
    index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    if let Some(ellipsis_index) = ellipsis_position(items) {
        return instantiate_repeated_list(items, ellipsis_index, state, index);
    }
    let expanded = items
        .iter()
        .map(|item| instantiate(item, state, index))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(list_expr(expanded))
}

fn instantiate_dotted(
    items: &[ExprRef],
    tail: &ExprRef,
    state: &mut ExpansionState<'_>,
    index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    let expanded_items = items
        .iter()
        .map(|item| instantiate(item, state, index))
        .collect::<Result<Vec<_>, _>>()?;
    let expanded_tail = instantiate(tail, state, index)?;
    Ok(expr_ref(Expr::DottedList(expanded_items, expanded_tail)))
}

fn instantiate_repeated_list(
    items: &[ExprRef],
    ellipsis_index: usize,
    state: &mut ExpansionState<'_>,
    index: Option<usize>,
) -> Result<ExprRef, EvalError> {
    let repeated = &items[ellipsis_index - 1];
    let mut expanded = items[..ellipsis_index - 1]
        .iter()
        .map(|item| instantiate(item, state, index))
        .collect::<Result<Vec<_>, _>>()?;
    let count = repetition_len(repeated, state.bindings).unwrap_or(0);
    for item_index in 0..count {
        expanded.push(instantiate(repeated, state, Some(item_index))?);
    }
    for item in &items[ellipsis_index + 1..] {
        expanded.push(instantiate(item, state, index)?);
    }
    Ok(list_expr(expanded))
}

fn instantiate_match(name: &str, bindings: &MatchMap, index: Option<usize>) -> Option<ExprRef> {
    match bindings.get(name) {
        Some(MatchValue::One(value)) => Some(value.clone()),
        Some(MatchValue::Many(values)) => index.and_then(|position| values.get(position).cloned()),
        None => None,
    }
}

fn introduced_identifier(name: &str, state: &mut ExpansionState<'_>) -> Identifier {
    let key = introduced_key(name, state.rule_set);
    if let Some(identifier) = state.introduced.get(&key) {
        return identifier.clone();
    }
    let identifier = capture_identifier(name, state);
    state.introduced.insert(key, identifier.clone());
    identifier
}

fn introduced_key(name: &str, rule_set: &SyntaxRules) -> String {
    if name == rule_set.name {
        return format!("s:{name}");
    }
    if core_keyword(name) {
        return format!("k:{name}");
    }
    if rule_set.macro_bindings.contains_key(name) {
        return format!("m:{name}");
    }
    if rule_set.value_bindings.contains_key(name) {
        return format!("v:{name}");
    }
    format!("g:{name}")
}

fn capture_identifier(name: &str, state: &mut ExpansionState<'_>) -> Identifier {
    if name == state.rule_set.name {
        return Identifier::Plain(name.to_owned());
    }
    if core_keyword(name) || state.rule_set.literals.contains(name) {
        return Identifier::Plain(name.to_owned());
    }
    if let Some(rule_set) = state.rule_set.macro_bindings.get(name) {
        return state.captures.capture_macro(name, rule_set.clone());
    }
    if let Some(cell) = state.rule_set.value_bindings.get(name) {
        return state.captures.capture_value(name, cell.clone());
    }
    state.captures.fresh_gensym(name)
}

fn repetition_len(template: &ExprRef, bindings: &MatchMap) -> Option<usize> {
    match template.as_ref() {
        Expr::Symbol(identifier) => match bindings.get(identifier.name()) {
            Some(MatchValue::Many(values)) => Some(values.len()),
            _ => None,
        },
        Expr::List(items) => items.iter().find_map(|item| repetition_len(item, bindings)),
        Expr::DottedList(items, tail) => items
            .iter()
            .find_map(|item| repetition_len(item, bindings))
            .or_else(|| repetition_len(tail, bindings)),
        Expr::Number(_) | Expr::Bool(_) | Expr::String(_) => None,
    }
}

fn tail_ellipsis(items: &[ExprRef]) -> Option<&ExprRef> {
    let last = items.last()?;
    if !last.as_ref().is_symbol_named("...") || items.len() < 2 {
        return None;
    }
    items.get(items.len() - 2)
}

fn ellipsis_position(items: &[ExprRef]) -> Option<usize> {
    items.iter()
        .position(|item| item.as_ref().is_symbol_named("..."))
        .filter(|index| *index > 0)
}

fn core_keyword(name: &str) -> bool {
    matches!(
        name,
        "if"
            | "begin"
            | "lambda"
            | "quote"
            | "set!"
            | "define"
            | "let"
            | "cond"
            | "and"
            | "or"
            | "else"
            | "define-syntax"
            | "syntax-rules"
    )
}

fn list_items<'a>(expr: &'a ExprRef, form: &str) -> Result<&'a [ExprRef], EvalError> {
    expr.as_ref().list_items().ok_or_else(|| EvalError::InvalidSyntax {
        form: form.to_owned(),
        detail: "expected a list".to_owned(),
    })
}

fn symbol_name(expr: &ExprRef, form: &str) -> Result<String, EvalError> {
    expr.as_ref()
        .symbol_name()
        .map(str::to_owned)
        .ok_or_else(|| EvalError::InvalidSyntax {
            form: form.to_owned(),
            detail: "expected a symbol".to_owned(),
        })
}

fn invalid_macro<T>(form: &str, detail: &str) -> Result<T, EvalError> {
    Err(EvalError::InvalidSyntax {
        form: form.to_owned(),
        detail: detail.to_owned(),
    })
}
