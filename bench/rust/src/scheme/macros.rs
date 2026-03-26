use super::{number::Number, EnvRef, EvalError, Expr, Value};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub(crate) type MacroEnv = HashMap<String, Rc<MacroDef>>;

#[derive(Debug, Clone)]
pub(crate) struct MacroDef {
    pub(crate) name: String,
    pub(crate) kind: MacroDefKind,
    pub(crate) def_env: EnvRef,
}

#[derive(Debug, Clone)]
pub(crate) enum MacroDefKind {
    SyntaxRules {
        literals: HashSet<String>,
        rules: Vec<MacroRule>,
    },
    Transformer {
        procedure: Value,
    },
}

#[derive(Debug, Clone)]
struct MacroRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Debug, Clone)]
pub(crate) enum PatternBinding {
    Single(Expr),
    Repeated(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum SyntaxExpr {
    Number(Number),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(SyntaxSymbol),
    List(Vec<SyntaxExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SyntaxSymbol {
    pub(crate) name: String,
    pub(crate) origin: SymbolOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SymbolOrigin {
    UseSite,
    Template,
}

struct PatternContext<'a> {
    macro_name: &'a str,
    literals: &'a HashSet<String>,
}

pub(crate) fn register_macro_definition(
    expr: &Expr,
    env: &EnvRef,
    macros: &mut MacroEnv,
) -> Result<bool, EvalError> {
    let Expr::List(items) = expr else {
        return Ok(false);
    };

    let Some(Expr::Symbol(keyword)) = items.first() else {
        return Ok(false);
    };

    if keyword != "define-syntax" {
        return Ok(false);
    }

    if items.len() != 3 {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a name and transformer",
        });
    }

    let Expr::Symbol(name) = &items[1] else {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a macro name",
        });
    };

    let macro_def = parse_transformer(name.clone(), &items[2], env.clone())?;
    macros.insert(name.clone(), Rc::new(macro_def));
    Ok(true)
}

pub(crate) fn transformer_macro(name: String, transformer: Value, def_env: EnvRef) -> MacroDef {
    MacroDef {
        name,
        kind: MacroDefKind::Transformer {
            procedure: transformer,
        },
        def_env,
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct MacroExpander {
    next_id: usize,
}

impl MacroExpander {
    pub(crate) fn expand_expr(
        &mut self,
        expr: &Expr,
        macros: &MacroEnv,
    ) -> Result<Expr, EvalError> {
        match expr {
            Expr::List(items) => self.expand_list(items, macros),
            _ => Ok(expr.clone()),
        }
    }

    fn expand_list(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let Some(head) = items.first() else {
            return Ok(Expr::List(Vec::new()));
        };

        if let Expr::Symbol(name) = head {
            if let Some(macro_def) = macros.get(name) {
                return self.expand_macro_call(&Expr::List(items.to_vec()), macro_def, macros);
            }

            match name.as_str() {
                "quote" | "syntax" => return Ok(Expr::List(items.to_vec())),
                "define" => return self.expand_define(items, macros),
                "define-syntax" => return Ok(Expr::List(items.to_vec())),
                "lambda" => return self.expand_lambda(items, macros),
                "let" => return self.expand_let(items, macros),
                "set!" => return self.expand_set(items, macros),
                "if" | "and" | "or" | "begin" => return self.expand_all(items, macros),
                "cond" => return self.expand_cond(items, macros),
                _ => {}
            }
        }

        self.expand_all(items, macros)
    }

    fn expand_macro_call(
        &mut self,
        invocation: &Expr,
        macro_def: &MacroDef,
        macros: &MacroEnv,
    ) -> Result<Expr, EvalError> {
        match &macro_def.kind {
            MacroDefKind::SyntaxRules { literals, rules } => {
                for rule in rules {
                    if let Some(bindings) = match_pattern(
                        &rule.pattern,
                        invocation,
                        &PatternContext {
                            macro_name: &macro_def.name,
                            literals,
                        },
                    ) {
                        let template =
                            self.expand_template(&rule.template, &bindings, None, &macro_def.name)?;
                        let hygienic =
                            self.hygienize(template, macro_def, macros, &mut Vec::new())?;
                        return self.expand_expr(&expr_from_syntax(hygienic), macros);
                    }
                }

                Err(EvalError::NoMatchingSyntaxRule {
                    name: macro_def.name.clone(),
                })
            }
            MacroDefKind::Transformer { procedure } => {
                let template = super::run_transformer_macro(invocation, procedure)?;
                let hygienic = self.hygienize(template, macro_def, macros, &mut Vec::new())?;
                self.expand_expr(&expr_from_syntax(hygienic), macros)
            }
        }
    }

    fn expand_define(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        expanded.push(items[0].clone());

        if let Some(target) = items.get(1) {
            expanded.push(target.clone());
        }

        for expr in items.iter().skip(2) {
            expanded.push(self.expand_expr(expr, macros)?);
        }

        Ok(Expr::List(expanded))
    }

    fn expand_lambda(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        expanded.push(items[0].clone());

        if let Some(params) = items.get(1) {
            expanded.push(params.clone());
        }

        for expr in items.iter().skip(2) {
            expanded.push(self.expand_expr(expr, macros)?);
        }

        Ok(Expr::List(expanded))
    }

    fn expand_let(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        expanded.push(items[0].clone());

        match items.get(1) {
            Some(Expr::List(_)) => {
                expanded.push(self.expand_bindings(&items[1], macros)?);
                for expr in &items[2..] {
                    expanded.push(self.expand_expr(expr, macros)?);
                }
            }
            Some(Expr::Symbol(name)) if items.len() >= 3 => {
                expanded.push(Expr::Symbol(name.clone()));
                expanded.push(self.expand_bindings(&items[2], macros)?);
                for expr in &items[3..] {
                    expanded.push(self.expand_expr(expr, macros)?);
                }
            }
            _ => return self.expand_all(items, macros),
        }

        Ok(Expr::List(expanded))
    }

    fn expand_bindings(&mut self, expr: &Expr, macros: &MacroEnv) -> Result<Expr, EvalError> {
        let Expr::List(bindings) = expr else {
            return self.expand_expr(expr, macros);
        };

        let mut expanded = Vec::with_capacity(bindings.len());
        for binding in bindings {
            match binding {
                Expr::List(items) if items.len() == 2 => expanded.push(Expr::List(vec![
                    items[0].clone(),
                    self.expand_expr(&items[1], macros)?,
                ])),
                _ => expanded.push(self.expand_expr(binding, macros)?),
            }
        }

        Ok(Expr::List(expanded))
    }

    fn expand_set(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        expanded.push(items[0].clone());

        if let Some(target) = items.get(1) {
            expanded.push(target.clone());
        }

        if let Some(value) = items.get(2) {
            expanded.push(self.expand_expr(value, macros)?);
        }

        for extra in items.iter().skip(3) {
            expanded.push(self.expand_expr(extra, macros)?);
        }

        Ok(Expr::List(expanded))
    }

    fn expand_all(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        Ok(Expr::List(
            items
                .iter()
                .map(|expr| self.expand_expr(expr, macros))
                .collect::<Result<Vec<_>, EvalError>>()?,
        ))
    }

    fn expand_cond(&mut self, items: &[Expr], macros: &MacroEnv) -> Result<Expr, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        expanded.push(items[0].clone());

        for clause in &items[1..] {
            match clause {
                Expr::List(parts) if !parts.is_empty() => {
                    let mut expanded_clause = Vec::with_capacity(parts.len());
                    if matches!(parts.first(), Some(Expr::Symbol(symbol)) if symbol == "else") {
                        expanded_clause.push(parts[0].clone());
                        for expr in &parts[1..] {
                            expanded_clause.push(self.expand_expr(expr, macros)?);
                        }
                    } else {
                        for expr in parts {
                            expanded_clause.push(self.expand_expr(expr, macros)?);
                        }
                    }
                    expanded.push(Expr::List(expanded_clause));
                }
                other => expanded.push(self.expand_expr(other, macros)?),
            }
        }

        Ok(Expr::List(expanded))
    }

    fn expand_template(
        &mut self,
        template: &Expr,
        bindings: &HashMap<String, PatternBinding>,
        repetition_index: Option<usize>,
        macro_name: &str,
    ) -> Result<SyntaxExpr, EvalError> {
        Ok(match template {
            Expr::Number(value) => SyntaxExpr::Number(value.clone()),
            Expr::Boolean(value) => SyntaxExpr::Boolean(*value),
            Expr::Char(value) => SyntaxExpr::Char(*value),
            Expr::String(value) => SyntaxExpr::String(value.clone()),
            Expr::Symbol(symbol) => match bindings.get(symbol) {
                Some(PatternBinding::Single(expr)) => syntax_from_use_expr(expr),
                Some(PatternBinding::Repeated(values)) => {
                    let Some(index) = repetition_index else {
                        return Err(EvalError::InvalidMacroTemplate {
                            name: macro_name.to_string(),
                            message: "ellipsis variables must appear under ellipsis in templates",
                        });
                    };

                    let Some(expr) = values.get(index) else {
                        return Err(EvalError::InvalidMacroTemplate {
                            name: macro_name.to_string(),
                            message: "ellipsis repetitions had inconsistent lengths",
                        });
                    };

                    syntax_from_use_expr(expr)
                }
                None => SyntaxExpr::Symbol(SyntaxSymbol {
                    name: symbol.clone(),
                    origin: SymbolOrigin::Template,
                }),
            },
            Expr::List(items) => {
                let mut expanded = Vec::new();
                let mut index = 0;

                while index < items.len() {
                    if matches!(items.get(index + 1), Some(Expr::Symbol(symbol)) if symbol == "...")
                    {
                        let repeat_count = repetition_count(&items[index], bindings, macro_name)?;
                        for repeated_index in 0..repeat_count {
                            expanded.push(self.expand_template(
                                &items[index],
                                bindings,
                                Some(repeated_index),
                                macro_name,
                            )?);
                        }
                        index += 2;
                    } else {
                        expanded.push(self.expand_template(
                            &items[index],
                            bindings,
                            repetition_index,
                            macro_name,
                        )?);
                        index += 1;
                    }
                }

                SyntaxExpr::List(expanded)
            }
        })
    }

    fn hygienize(
        &mut self,
        expr: SyntaxExpr,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &mut Vec<HashMap<String, String>>,
    ) -> Result<SyntaxExpr, EvalError> {
        match expr {
            SyntaxExpr::List(items) => self.hygienize_list(items, macro_def, macros, scopes),
            SyntaxExpr::Symbol(symbol) => Ok(SyntaxExpr::Symbol(
                self.resolve_symbol(symbol, macro_def, macros, scopes),
            )),
            other => Ok(other),
        }
    }

    fn hygienize_list(
        &mut self,
        items: Vec<SyntaxExpr>,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &mut Vec<HashMap<String, String>>,
    ) -> Result<SyntaxExpr, EvalError> {
        let Some(head_name) = template_head_name(&items) else {
            return self.generic_hygienize_list(items, macro_def, macros, scopes);
        };

        match head_name {
            "quote" => Ok(SyntaxExpr::List(items)),
            "lambda" => self.hygienize_lambda(items, macro_def, macros, scopes),
            "let" => self.hygienize_let(items, macro_def, macros, scopes),
            _ => self.generic_hygienize_list(items, macro_def, macros, scopes),
        }
    }

    fn generic_hygienize_list(
        &mut self,
        items: Vec<SyntaxExpr>,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &mut Vec<HashMap<String, String>>,
    ) -> Result<SyntaxExpr, EvalError> {
        Ok(SyntaxExpr::List(
            items
                .into_iter()
                .map(|item| self.hygienize(item, macro_def, macros, scopes))
                .collect::<Result<Vec<_>, EvalError>>()?,
        ))
    }

    fn hygienize_lambda(
        &mut self,
        mut items: Vec<SyntaxExpr>,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &mut Vec<HashMap<String, String>>,
    ) -> Result<SyntaxExpr, EvalError> {
        if items.len() < 3 {
            return self.generic_hygienize_list(items, macro_def, macros, scopes);
        }

        let head = self.hygienize(items.remove(0), macro_def, macros, scopes)?;
        let params = items.remove(0);
        let (params, renamed) = self.rename_lambda_formals(params);

        let mut expanded = vec![head, params];
        scopes.push(renamed);
        for body in items {
            expanded.push(self.hygienize(body, macro_def, macros, scopes)?);
        }
        scopes.pop();

        Ok(SyntaxExpr::List(expanded))
    }

    fn hygienize_let(
        &mut self,
        mut items: Vec<SyntaxExpr>,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &mut Vec<HashMap<String, String>>,
    ) -> Result<SyntaxExpr, EvalError> {
        if items.len() < 3 {
            return self.generic_hygienize_list(items, macro_def, macros, scopes);
        }

        let head = self.hygienize(items.remove(0), macro_def, macros, scopes)?;
        let bindings = items.remove(0);
        let SyntaxExpr::List(bindings) = bindings else {
            let mut rebuilt = vec![head, self.hygienize(bindings, macro_def, macros, scopes)?];
            for body in items {
                rebuilt.push(self.hygienize(body, macro_def, macros, scopes)?);
            }
            return Ok(SyntaxExpr::List(rebuilt));
        };

        let mut renamed = HashMap::new();
        let mut expanded_bindings = Vec::with_capacity(bindings.len());
        for binding in bindings {
            match binding {
                SyntaxExpr::List(pair) if pair.len() == 2 => {
                    let mut pair = pair;
                    let init = self.hygienize(pair.remove(1), macro_def, macros, scopes)?;
                    let binder = pair.remove(0);
                    let binder = match binder {
                        SyntaxExpr::Symbol(symbol)
                            if symbol.origin == SymbolOrigin::Template && symbol.name != "." =>
                        {
                            let fresh = self.fresh_symbol(&symbol.name);
                            renamed.insert(symbol.name, fresh.clone());
                            SyntaxExpr::Symbol(SyntaxSymbol {
                                name: fresh,
                                origin: SymbolOrigin::Template,
                            })
                        }
                        other => other,
                    };
                    expanded_bindings.push(SyntaxExpr::List(vec![binder, init]));
                }
                other => expanded_bindings.push(self.hygienize(other, macro_def, macros, scopes)?),
            }
        }

        let mut expanded = vec![head, SyntaxExpr::List(expanded_bindings)];
        scopes.push(renamed);
        for body in items {
            expanded.push(self.hygienize(body, macro_def, macros, scopes)?);
        }
        scopes.pop();

        Ok(SyntaxExpr::List(expanded))
    }

    fn rename_lambda_formals(
        &mut self,
        params: SyntaxExpr,
    ) -> (SyntaxExpr, HashMap<String, String>) {
        match params {
            SyntaxExpr::Symbol(symbol)
                if symbol.origin == SymbolOrigin::Template && symbol.name != "." =>
            {
                let fresh = self.fresh_symbol(&symbol.name);
                let mut renamed = HashMap::new();
                renamed.insert(symbol.name, fresh.clone());
                (
                    SyntaxExpr::Symbol(SyntaxSymbol {
                        name: fresh,
                        origin: SymbolOrigin::Template,
                    }),
                    renamed,
                )
            }
            SyntaxExpr::List(items) => {
                let mut renamed = HashMap::new();
                let mut expanded = Vec::with_capacity(items.len());
                for item in items {
                    match item {
                        SyntaxExpr::Symbol(symbol)
                            if symbol.origin == SymbolOrigin::Template && symbol.name != "." =>
                        {
                            let fresh = self.fresh_symbol(&symbol.name);
                            renamed.insert(symbol.name, fresh.clone());
                            expanded.push(SyntaxExpr::Symbol(SyntaxSymbol {
                                name: fresh,
                                origin: SymbolOrigin::Template,
                            }));
                        }
                        other => expanded.push(other),
                    }
                }
                (SyntaxExpr::List(expanded), renamed)
            }
            other => (other, HashMap::new()),
        }
    }

    fn resolve_symbol(
        &mut self,
        symbol: SyntaxSymbol,
        macro_def: &MacroDef,
        macros: &MacroEnv,
        scopes: &[HashMap<String, String>],
    ) -> SyntaxSymbol {
        if symbol.origin == SymbolOrigin::UseSite {
            return symbol;
        }

        if let Some(name) = scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(&symbol.name))
            .cloned()
        {
            return SyntaxSymbol {
                name,
                origin: SymbolOrigin::Template,
            };
        }

        if is_special_form_name(&symbol.name) || macros.contains_key(&symbol.name) {
            return symbol;
        }

        if let Some(cell) = macro_def.def_env.lookup_cell(&symbol.name) {
            let alias = self.fresh_symbol(&symbol.name);
            macro_def
                .def_env
                .define_cell(alias.clone(), Rc::clone(&cell));
            return SyntaxSymbol {
                name: alias,
                origin: SymbolOrigin::Template,
            };
        }

        symbol
    }

    fn fresh_symbol(&mut self, base: &str) -> String {
        self.next_id += 1;
        let sanitized = base
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect::<String>();
        format!("__macro_{}_{}", sanitized, self.next_id)
    }
}

pub(crate) fn parse_transformer(
    name: String,
    expr: &Expr,
    def_env: EnvRef,
) -> Result<MacroDef, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a syntax-rules transformer",
        });
    };

    if !matches!(items.first(), Some(Expr::Symbol(symbol)) if symbol == "syntax-rules") {
        return Err(EvalError::InvalidForm {
            name: "define-syntax",
            message: "expected a syntax-rules transformer",
        });
    }

    if items.len() < 3 {
        return Err(EvalError::InvalidForm {
            name: "syntax-rules",
            message: "expected a literal list and at least one rule",
        });
    }

    let literals = parse_literals(&items[1])?;
    let rules = items[2..]
        .iter()
        .map(parse_rule)
        .collect::<Result<Vec<_>, EvalError>>()?;

    Ok(MacroDef {
        name,
        kind: MacroDefKind::SyntaxRules { literals, rules },
        def_env,
    })
}

fn parse_literals(expr: &Expr) -> Result<HashSet<String>, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "syntax-rules",
            message: "expected a literal identifier list",
        });
    };

    items
        .iter()
        .map(|item| match item {
            Expr::Symbol(symbol) => Ok(symbol.clone()),
            _ => Err(EvalError::InvalidForm {
                name: "syntax-rules",
                message: "expected literal identifiers to be symbols",
            }),
        })
        .collect()
}

fn parse_rule(expr: &Expr) -> Result<MacroRule, EvalError> {
    let Expr::List(items) = expr else {
        return Err(EvalError::InvalidForm {
            name: "syntax-rules",
            message: "expected each rule to be a pattern/template pair",
        });
    };

    if items.len() != 2 {
        return Err(EvalError::InvalidForm {
            name: "syntax-rules",
            message: "expected each rule to contain a pattern and template",
        });
    }

    Ok(MacroRule {
        pattern: items[0].clone(),
        template: items[1].clone(),
    })
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    context: &PatternContext<'_>,
) -> Option<HashMap<String, PatternBinding>> {
    match (pattern, input) {
        (Expr::Number(left), Expr::Number(right)) if left == right => Some(HashMap::new()),
        (Expr::Boolean(left), Expr::Boolean(right)) if left == right => Some(HashMap::new()),
        (Expr::Char(left), Expr::Char(right)) if left == right => Some(HashMap::new()),
        (Expr::String(left), Expr::String(right)) if left == right => Some(HashMap::new()),
        (Expr::Symbol(symbol), Expr::Symbol(input_symbol))
            if !is_pattern_variable(symbol, context) =>
        {
            (symbol == input_symbol).then(HashMap::new)
        }
        (Expr::Symbol(symbol), _) if is_pattern_variable(symbol, context) && symbol != "..." => {
            let mut bindings = HashMap::new();
            bindings.insert(symbol.clone(), PatternBinding::Single(input.clone()));
            Some(bindings)
        }
        (Expr::List(patterns), Expr::List(inputs)) => match_list_pattern(patterns, inputs, context),
        _ => None,
    }
}

pub(crate) fn match_syntax_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &HashSet<String>,
) -> Option<HashMap<String, PatternBinding>> {
    match_pattern(
        pattern,
        input,
        &PatternContext {
            macro_name: "",
            literals,
        },
    )
}

fn match_list_pattern(
    patterns: &[Expr],
    inputs: &[Expr],
    context: &PatternContext<'_>,
) -> Option<HashMap<String, PatternBinding>> {
    if let Some((prefix, repeated)) = split_tail_ellipsis(patterns) {
        if inputs.len() < prefix.len() {
            return None;
        }

        let mut bindings = HashMap::new();
        for (pattern, input) in prefix.iter().zip(inputs.iter()) {
            merge_single_bindings(&mut bindings, match_pattern(pattern, input, context)?)?;
        }

        let mut repeated_bindings: HashMap<String, Vec<Expr>> = HashMap::new();
        for input in &inputs[prefix.len()..] {
            let nested = match_pattern(repeated, input, context)?;
            for (name, value) in nested {
                match value {
                    PatternBinding::Single(expr) => {
                        repeated_bindings.entry(name).or_default().push(expr);
                    }
                    PatternBinding::Repeated(_) => return None,
                }
            }
        }

        let mut repeated_vars = HashSet::new();
        collect_pattern_variables(repeated, context, &mut repeated_vars);
        for variable in repeated_vars {
            bindings.insert(
                variable.clone(),
                PatternBinding::Repeated(repeated_bindings.remove(&variable).unwrap_or_default()),
            );
        }

        Some(bindings)
    } else {
        if patterns.len() != inputs.len() {
            return None;
        }

        let mut bindings = HashMap::new();
        for (pattern, input) in patterns.iter().zip(inputs.iter()) {
            merge_single_bindings(&mut bindings, match_pattern(pattern, input, context)?)?;
        }

        Some(bindings)
    }
}

fn split_tail_ellipsis(patterns: &[Expr]) -> Option<(&[Expr], &Expr)> {
    match patterns {
        [prefix @ .., repeated, Expr::Symbol(symbol)] if symbol == "..." => {
            Some((prefix, repeated))
        }
        _ => None,
    }
}

fn merge_single_bindings(
    target: &mut HashMap<String, PatternBinding>,
    bindings: HashMap<String, PatternBinding>,
) -> Option<()> {
    for (name, value) in bindings {
        match (target.get(&name), value) {
            (None, value) => {
                target.insert(name, value);
            }
            (Some(PatternBinding::Single(existing)), PatternBinding::Single(candidate))
                if *existing == candidate => {}
            _ => return None,
        }
    }

    Some(())
}

fn collect_pattern_variables(
    pattern: &Expr,
    context: &PatternContext<'_>,
    variables: &mut HashSet<String>,
) {
    match pattern {
        Expr::Symbol(symbol) if is_pattern_variable(symbol, context) && symbol != "..." => {
            variables.insert(symbol.clone());
        }
        Expr::List(items) => {
            if let Some((prefix, repeated)) = split_tail_ellipsis(items) {
                for item in prefix {
                    collect_pattern_variables(item, context, variables);
                }
                collect_pattern_variables(repeated, context, variables);
            } else {
                for item in items {
                    collect_pattern_variables(item, context, variables);
                }
            }
        }
        _ => {}
    }
}

fn is_pattern_variable(symbol: &str, context: &PatternContext<'_>) -> bool {
    symbol != context.macro_name && !context.literals.contains(symbol)
}

fn repetition_count(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    macro_name: &str,
) -> Result<usize, EvalError> {
    let mut repeated = Vec::new();
    collect_repeated_bindings(template, bindings, &mut repeated);

    let Some((first, rest)) = repeated.split_first() else {
        return Err(EvalError::InvalidMacroTemplate {
            name: macro_name.to_string(),
            message: "ellipsis must repeat at least one pattern variable",
        });
    };

    if rest.iter().any(|count| *count != *first) {
        return Err(EvalError::InvalidMacroTemplate {
            name: macro_name.to_string(),
            message: "ellipsis repetitions had inconsistent lengths",
        });
    }

    Ok(*first)
}

fn collect_repeated_bindings(
    expr: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    repeated: &mut Vec<usize>,
) {
    match expr {
        Expr::Symbol(symbol) => {
            if let Some(PatternBinding::Repeated(values)) = bindings.get(symbol) {
                repeated.push(values.len());
            }
        }
        Expr::List(items) => {
            for item in items {
                collect_repeated_bindings(item, bindings, repeated);
            }
        }
        _ => {}
    }
}

pub(crate) fn syntax_from_use_expr(expr: &Expr) -> SyntaxExpr {
    match expr {
        Expr::Number(value) => SyntaxExpr::Number(value.clone()),
        Expr::Boolean(value) => SyntaxExpr::Boolean(*value),
        Expr::Char(value) => SyntaxExpr::Char(*value),
        Expr::String(value) => SyntaxExpr::String(value.clone()),
        Expr::Symbol(symbol) => SyntaxExpr::Symbol(SyntaxSymbol {
            name: symbol.clone(),
            origin: SymbolOrigin::UseSite,
        }),
        Expr::List(items) => SyntaxExpr::List(items.iter().map(syntax_from_use_expr).collect()),
    }
}

pub(crate) fn expr_from_syntax(expr: SyntaxExpr) -> Expr {
    match expr {
        SyntaxExpr::Number(value) => Expr::Number(value),
        SyntaxExpr::Boolean(value) => Expr::Boolean(value),
        SyntaxExpr::Char(value) => Expr::Char(value),
        SyntaxExpr::String(value) => Expr::String(value),
        SyntaxExpr::Symbol(symbol) => Expr::Symbol(symbol.name),
        SyntaxExpr::List(items) => Expr::List(items.into_iter().map(expr_from_syntax).collect()),
    }
}

fn template_head_name(items: &[SyntaxExpr]) -> Option<&str> {
    match items.first() {
        Some(SyntaxExpr::Symbol(symbol)) if symbol.origin == SymbolOrigin::Template => {
            Some(symbol.name.as_str())
        }
        _ => None,
    }
}

fn is_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "begin"
            | "cond"
            | "define"
            | "define-syntax"
            | "if"
            | "lambda"
            | "let"
            | "or"
            | "quote"
            | "set!"
            | "syntax"
            | "syntax-case"
            | "syntax-rules"
            | "with-syntax"
    )
}
