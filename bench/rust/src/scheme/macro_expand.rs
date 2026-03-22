use std::collections::HashMap;

use crate::scheme::error::{EvalError, EvalErrorKind, Span};
use crate::scheme::parser::{Expr, ExprKind};

/// A syntax-rules macro definition.
#[derive(Debug, Clone)]
pub struct SyntaxRulesDef {
    pub literals: Vec<String>,
    pub rules: Vec<SyntaxRule>,
}

/// A single (pattern template) rule.
#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pub pattern: Expr,
    pub template: Expr,
}

/// A matched binding from pattern matching.
#[derive(Debug, Clone)]
enum Binding {
    Single(Expr),
    List(Vec<Expr>),
}

/// Parse `(syntax-rules (literals...) (pattern template) ...)` from the args
/// after the `syntax-rules` keyword.
pub fn parse_syntax_rules(args: &[Expr], span: &Span) -> Result<SyntaxRulesDef, EvalError> {
    if args.is_empty() {
        return Err(EvalErrorKind::Parse {
            message: "syntax-rules requires literals and at least one rule".into(),
        }
        .at(span));
    }

    let literals = match &args[0].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalErrorKind::Parse {
                    message: "syntax-rules: literals must be symbols".into(),
                }
                .at(span)),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalErrorKind::Parse {
                message: "syntax-rules: expected literal list".into(),
            }
            .at(span))
        }
    };

    let mut rules = Vec::new();
    for rule_expr in &args[1..] {
        match &rule_expr.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push(SyntaxRule {
                    pattern: parts[0].clone(),
                    template: parts[1].clone(),
                });
            }
            _ => {
                return Err(EvalErrorKind::Parse {
                    message: "syntax-rules: each rule must be (pattern template)".into(),
                }
                .at(span))
            }
        }
    }

    Ok(SyntaxRulesDef { literals, rules })
}

/// Expand a macro use-site form against the syntax-rules definition.
/// Returns the expanded expression and a list of (gensym, original_name) pairs
/// for hygiene bindings.
pub fn expand_macro(
    syntax_rules: &SyntaxRulesDef,
    use_elements: &[Expr],
    hygiene_id: u64,
    span: &Span,
) -> Result<(Expr, Vec<(String, String)>), EvalError> {
    let use_args = &use_elements[1..]; // skip macro name

    for rule in &syntax_rules.rules {
        let mut bindings = HashMap::new();
        if match_pattern(
            &rule.pattern,
            use_args,
            &syntax_rules.literals,
            &mut bindings,
        ) {
            let mut introduced = Vec::new();
            let expanded = instantiate_template(
                &rule.template,
                &bindings,
                hygiene_id,
                &mut introduced,
            )?;
            return Ok((expanded, introduced));
        }
    }

    Err(EvalErrorKind::Parse {
        message: "no matching syntax-rules pattern".into(),
    }
    .at(span))
}

/// Match a pattern (which includes the macro name as first element) against
/// the use-site arguments (macro name already stripped).
fn match_pattern(
    pattern: &Expr,
    use_args: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match &pattern.kind {
        ExprKind::List(pat_elems) if !pat_elems.is_empty() => {
            // Skip the macro name (first element of pattern)
            let pat_args = &pat_elems[1..];
            match_elements(pat_args, use_args, literals, bindings)
        }
        _ => false,
    }
}

/// Match a sequence of pattern elements against a sequence of form elements.
fn match_elements(
    pat: &[Expr],
    form: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    // Find ellipsis position
    let ellipsis_pos = pat
        .iter()
        .position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));

    match ellipsis_pos {
        Some(pos) if pos > 0 => {
            let before_ellipsis = &pat[..pos - 1];
            let ellipsis_var = &pat[pos - 1];
            let after_ellipsis = &pat[pos + 1..];

            if form.len() < before_ellipsis.len() + after_ellipsis.len() {
                return false;
            }

            // Match fixed elements before the ellipsis variable
            for (p, f) in before_ellipsis.iter().zip(form.iter()) {
                if !match_single(p, f, literals, bindings) {
                    return false;
                }
            }

            // Match fixed elements after ellipsis
            let after_start = form.len() - after_ellipsis.len();
            for (p, f) in after_ellipsis.iter().zip(form[after_start..].iter()) {
                if !match_single(p, f, literals, bindings) {
                    return false;
                }
            }

            // The ellipsis variable matches everything in between
            let ellipsis_forms = &form[before_ellipsis.len()..after_start];
            match &ellipsis_var.kind {
                ExprKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                    bindings.insert(name.clone(), Binding::List(ellipsis_forms.to_vec()));
                    true
                }
                _ => false,
            }
        }
        _ => {
            // No ellipsis — exact element count
            if pat.len() != form.len() {
                return false;
            }
            for (p, f) in pat.iter().zip(form.iter()) {
                if !match_single(p, f, literals, bindings) {
                    return false;
                }
            }
            true
        }
    }
}

/// Match a single pattern element against a single form element.
fn match_single(
    pattern: &Expr,
    form: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(name) if name == "_" => true,
        ExprKind::Symbol(name) if literals.contains(name) => {
            matches!(&form.kind, ExprKind::Symbol(s) if s == name)
        }
        ExprKind::Symbol(name) => {
            bindings.insert(name.clone(), Binding::Single(form.clone()));
            true
        }
        ExprKind::List(pat_elems) => match &form.kind {
            ExprKind::List(form_elems) => {
                match_elements(pat_elems, form_elems, literals, bindings)
            }
            _ => false,
        },
        _ => pattern.kind == form.kind,
    }
}

/// Instantiate a template, substituting pattern variables and renaming
/// template-introduced identifiers for hygiene.
fn instantiate_template(
    template: &Expr,
    bindings: &HashMap<String, Binding>,
    hygiene_id: u64,
    introduced: &mut Vec<(String, String)>,
) -> Result<Expr, EvalError> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    Binding::Single(expr) => Ok(expr.clone()),
                    Binding::List(exprs) => {
                        // Bare ellipsis var without ...; return first if available
                        if let Some(first) = exprs.first() {
                            Ok(first.clone())
                        } else {
                            Ok(Expr {
                                kind: ExprKind::List(vec![]),
                                span: template.span.clone(),
                            })
                        }
                    }
                }
            } else if is_keyword(name) {
                Ok(template.clone())
            } else {
                // Template-introduced symbol: rename for hygiene
                let gensym = format!("{name}##h{hygiene_id}");
                introduced.push((gensym.clone(), name.clone()));
                Ok(Expr {
                    kind: ExprKind::Symbol(gensym),
                    span: template.span.clone(),
                })
            }
        }
        ExprKind::List(elements) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elements.len() {
                // Check if next element is `...`
                if i + 1 < elements.len() {
                    if let ExprKind::Symbol(s) = &elements[i + 1].kind {
                        if s == "..." {
                            let expanded =
                                expand_ellipsis(&elements[i], bindings, hygiene_id, introduced)?;
                            result.extend(expanded);
                            i += 2;
                            continue;
                        }
                    }
                }
                result.push(instantiate_template(
                    &elements[i],
                    bindings,
                    hygiene_id,
                    introduced,
                )?);
                i += 1;
            }
            Ok(Expr {
                kind: ExprKind::List(result),
                span: template.span.clone(),
            })
        }
        _ => Ok(template.clone()),
    }
}

/// Expand a template element followed by `...` using the ellipsis-bound variable.
fn expand_ellipsis(
    template_elem: &Expr,
    bindings: &HashMap<String, Binding>,
    hygiene_id: u64,
    introduced: &mut Vec<(String, String)>,
) -> Result<Vec<Expr>, EvalError> {
    let ellipsis_var = find_ellipsis_var(template_elem, bindings);

    match ellipsis_var {
        Some(var_name) => {
            if let Some(Binding::List(items)) = bindings.get(&var_name) {
                let mut result = Vec::new();
                for item in items {
                    let mut new_bindings = bindings.clone();
                    new_bindings.insert(var_name.clone(), Binding::Single(item.clone()));
                    result.push(instantiate_template(
                        template_elem,
                        &new_bindings,
                        hygiene_id,
                        introduced,
                    )?);
                }
                Ok(result)
            } else {
                Ok(Vec::new())
            }
        }
        None => Ok(Vec::new()),
    }
}

/// Find a pattern variable with a List binding in a template expression.
fn find_ellipsis_var(template: &Expr, bindings: &HashMap<String, Binding>) -> Option<String> {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if let Some(Binding::List(_)) = bindings.get(name) {
                Some(name.clone())
            } else {
                None
            }
        }
        ExprKind::List(elements) => {
            for elem in elements {
                if let Some(var) = find_ellipsis_var(elem, bindings) {
                    return Some(var);
                }
            }
            None
        }
        _ => None,
    }
}

/// Returns true if the symbol is a language keyword or builtin that should
/// not be renamed during hygiene.
fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        // special forms
        "if" | "define"
            | "quote"
            | "lambda"
            | "and"
            | "or"
            | "not"
            | "let"
            | "begin"
            | "cond"
            | "set!"
            | "string-set!"
            | "call/cc"
            | "call-with-current-continuation"
            | "define-syntax"
            | "syntax-rules"
            | "else"
            | "..."
            | "_"
            // builtins
            | "+"
            | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "pair?"
            | "string?"
            | "number?"
            | "boolean?"
            | "symbol?"
            | "char?"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
            | "string-copy"
            | "apply"
    )
}
