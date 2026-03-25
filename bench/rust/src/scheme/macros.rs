use std::collections::HashMap;

use super::{
    gensym, CekState, Env, Expr, ExprKind, KFrame, Span, Val, EvalError,
    enter_body_cek,
};

#[derive(Clone, Debug)]
pub(crate) enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

pub(super) const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "begin",
    "let", "let*", "cond", "set!", "string-set!", "set-car!", "set-cdr!",
    "define-syntax", "syntax-rules", "syntax-case", "syntax", "with-syntax",
    "define-record-type", "case-lambda", "letrec", "letrec*", "case", "do",
    "call/cc", "call-with-current-continuation", "dynamic-wind",
];

pub(super) fn is_ellipsis(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(s) if s == "...")
}

fn match_pattern_list(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let ellipsis_pos = pattern.iter().position(
        |e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."),
    );
    if let Some(epos) = ellipsis_pos {
        let fixed_before = &pattern[..epos - 1];
        let ellipsis_pat = &pattern[epos - 1];
        let fixed_after = &pattern[epos + 1..];
        let min_len = fixed_before.len() + fixed_after.len();
        if input.len() < min_len {
            return false;
        }
        for (p, e) in fixed_before.iter().zip(input.iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        let after_start = input.len() - fixed_after.len();
        for (p, e) in fixed_after.iter().zip(input[after_start..].iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        let ellipsis_input = &input[fixed_before.len()..after_start];
        match &ellipsis_pat.kind {
            ExprKind::Symbol(s) if !literals.contains(s) && s != "_" => {
                bindings.insert(s.clone(), MacroBinding::Many(ellipsis_input.to_vec()));
                true
            }
            _ => ellipsis_input.is_empty(),
        }
    } else {
        if pattern.len() != input.len() {
            return false;
        }
        for (p, e) in pattern.iter().zip(input.iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn match_pattern_single(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&input.kind, ExprKind::Symbol(s2) if s2 == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        _ => false,
    }
}

fn find_ellipsis_var(expr: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Many(_))) {
                return Some(s.clone());
            }
            None
        }
        ExprKind::List(elems) => {
            for e in elems {
                if let Some(v) = find_ellipsis_var(e, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn collect_template_free_vars(
    expr: &Expr,
    pattern_vars: &[String],
    out: &mut Vec<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(s) if s != "..." => {
            if !pattern_vars.contains(s) && !out.contains(s) {
                out.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_template_free_vars(e, pattern_vars, out);
            }
        }
        _ => {}
    }
}

fn expand_ellipsis_element(
    sub: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
    result: &mut Vec<Expr>,
) {
    let Some(var_name) = find_ellipsis_var(sub, bindings) else { return };
    let Some(MacroBinding::Many(exprs)) = bindings.get(&var_name) else { return };
    if matches!(&sub.kind, ExprKind::Symbol(sn) if sn == &var_name) {
        result.extend(exprs.iter().cloned());
    } else {
        for expr in exprs {
            let mut sub_bindings = bindings.clone();
            sub_bindings.insert(var_name.clone(), MacroBinding::Single(expr.clone()));
            result.push(expand_template(sub, &sub_bindings, renames));
        }
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(e) => e.clone(),
                    MacroBinding::Many(_) => template.clone(),
                }
            } else if let Some(new_name) = renames.get(s) {
                Expr::new(ExprKind::Symbol(new_name.clone()), template.span)
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    expand_ellipsis_element(&elems[i], bindings, renames, &mut result);
                    i += 2;
                    continue;
                }
                result.push(expand_template(&elems[i], bindings, renames));
                i += 1;
            }
            Expr::new(ExprKind::List(result), template.span)
        }
        _ => template.clone(),
    }
}

/// Expand a macro call and return the expanded expression + environment.
pub(super) fn expand_macro(
    elems: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    span: Span,
) -> Result<(Expr, Env), EvalError> {
    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => continue,
        };
        let mut bindings = HashMap::new();
        if match_pattern_list(&pat_elems[1..], &elems[1..], literals, &mut bindings) {
            let pattern_vars: Vec<String> = bindings.keys().cloned().collect();
            let mut free_vars = Vec::new();
            collect_template_free_vars(template, &pattern_vars, &mut free_vars);

            let mut renames = HashMap::new();
            for sym in &free_vars {
                if SPECIAL_FORMS.contains(&sym.as_str()) {
                    continue;
                }
                if def_env.get(sym).is_some() {
                    renames.insert(sym.clone(), gensym(sym));
                }
            }

            let expanded = expand_template(template, &bindings, &renames);

            if renames.is_empty() {
                return Ok((expanded, use_env.clone()));
            } else {
                let new_env = use_env.push();
                for (orig, gs) in &renames {
                    if let Some(val) = def_env.get(orig) {
                        new_env.define(gs.clone(), val);
                    }
                }
                return Ok((expanded, new_env));
            }
        }
    }
    Err(EvalError::Runtime(format!("no matching syntax rule at {span}")))
}

// ---------------------------------------------------------------------------
//  syntax-case support
// ---------------------------------------------------------------------------

/// Evaluate `(syntax template)` — expand template using SyntaxObject bindings in env.
pub(super) fn eval_syntax_form(template: &Expr, env: &Env) -> Val {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(val) = env.get(s) {
                if matches!(&val, Val::SyntaxObject(_)) {
                    return val;
                }
            }
            Val::SyntaxObject(Box::new(template.clone()))
        }
        ExprKind::List(_) => {
            let expanded = expand_syntax_template(template, env);
            Val::SyntaxObject(Box::new(expanded))
        }
        _ => Val::SyntaxObject(Box::new(template.clone())),
    }
}

/// Expand a syntax template, substituting SyntaxObject bindings from env.
fn expand_syntax_template(template: &Expr, env: &Env) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(Val::SyntaxObject(e)) = env.get(s) {
                *e
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    // Ellipsis expansion
                    expand_syntax_ellipsis(&elems[i], env, &mut result);
                    i += 2;
                } else {
                    result.push(expand_syntax_template(&elems[i], env));
                    i += 1;
                }
            }
            Expr::new(ExprKind::List(result), template.span)
        }
        _ => template.clone(),
    }
}

/// Find the ellipsis variable name in a sub-template, checking against the env for list bindings.
fn find_syntax_ellipsis_var(expr: &Expr, env: &Env) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if let Some(Val::List(items)) = env.get(s) {
                if items.iter().all(|v| matches!(v, Val::SyntaxObject(_))) {
                    return Some(s.clone());
                }
            }
            None
        }
        ExprKind::List(elems) => {
            for e in elems {
                if let Some(v) = find_syntax_ellipsis_var(e, env) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

/// Expand an ellipsis element in a syntax template.
fn expand_syntax_ellipsis(sub: &Expr, env: &Env, result: &mut Vec<Expr>) {
    let Some(var_name) = find_syntax_ellipsis_var(sub, env) else { return };
    let Some(Val::List(items)) = env.get(&var_name) else { return };

    if matches!(&sub.kind, ExprKind::Symbol(sn) if sn == &var_name) {
        // Simple case: `elem ...` — just splice the syntax objects
        for item in &items {
            if let Val::SyntaxObject(e) = item {
                result.push((**e).clone());
            }
        }
    } else {
        // Complex case: sub-template with ellipsis var — expand for each element
        for item in &items {
            if let Val::SyntaxObject(e) = item {
                let child_env = env.push();
                child_env.define(var_name.clone(), Val::SyntaxObject(Box::new((**e).clone())));
                result.push(expand_syntax_template(sub, &child_env));
            }
        }
    }
}

/// Pattern-match a syntax-case clause and enter the body in the CEK machine.
pub(super) fn eval_syntax_case_match(
    stx_expr: Expr,
    literals: &[String],
    clauses: &[Expr],
    env: &Env,
    kont: &mut Vec<KFrame>,
) -> Result<CekState, EvalError> {
    for clause in clauses {
        let parts = match &clause.kind {
            ExprKind::List(p) if p.len() >= 2 => p,
            _ => continue,
        };
        let pattern = &parts[0];
        let body = &parts[parts.len() - 1]; // last element is body

        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => continue,
        };
        let input_elems = match &stx_expr.kind {
            ExprKind::List(e) => e,
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern_list(pat_elems, input_elems, literals, &mut bindings) {
            let new_env = env.push();
            for (name, binding) in &bindings {
                match binding {
                    MacroBinding::Single(e) => {
                        new_env.define(name.clone(), Val::SyntaxObject(Box::new(e.clone())));
                    }
                    MacroBinding::Many(es) => {
                        let syntax_list: Vec<Val> = es.iter()
                            .map(|e| Val::SyntaxObject(Box::new(e.clone())))
                            .collect();
                        new_env.define(name.clone(), Val::List(syntax_list));
                    }
                }
            }
            return enter_body_cek(std::slice::from_ref(body), &new_env, kont);
        }
    }
    Err(EvalError::Runtime("syntax-case: no matching clause".into()))
}

/// Apply hygiene to a syntax-case expanded expression.
/// Renames free variables from def_env, similar to syntax-rules hygiene.
pub(super) fn apply_syntax_hygiene(
    expr: Expr,
    def_env: &Env,
    use_env: &Env,
) -> (Expr, Env) {
    // Collect all symbols in the expression
    let mut all_syms = Vec::new();
    collect_all_symbols(&expr, &mut all_syms);

    let mut renames = HashMap::new();
    for sym in &all_syms {
        if SPECIAL_FORMS.contains(&sym.as_str()) {
            continue;
        }
        if renames.contains_key(sym) {
            continue;
        }
        if def_env.get(sym).is_some() && use_env.get(sym).is_none() {
            renames.insert(sym.clone(), gensym(sym));
        }
    }

    if renames.is_empty() {
        (expr, use_env.clone())
    } else {
        let renamed = rename_symbols(&expr, &renames);
        let new_env = use_env.push();
        for (orig, gs) in &renames {
            if let Some(val) = def_env.get(orig) {
                new_env.define(gs.clone(), val);
            }
        }
        (renamed, new_env)
    }
}

fn collect_all_symbols(expr: &Expr, out: &mut Vec<String>) {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if !out.contains(s) {
                out.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_all_symbols(e, out);
            }
        }
        _ => {}
    }
}

fn rename_symbols(expr: &Expr, renames: &HashMap<String, String>) -> Expr {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if let Some(new_name) = renames.get(s) {
                Expr::new(ExprKind::Symbol(new_name.clone()), expr.span)
            } else {
                expr.clone()
            }
        }
        ExprKind::List(elems) => {
            let new_elems: Vec<Expr> = elems.iter().map(|e| rename_symbols(e, renames)).collect();
            Expr::new(ExprKind::List(new_elems), expr.span)
        }
        _ => expr.clone(),
    }
}

/// Convert a Val back to an Expr (inverse of expr_to_val).
pub(super) fn val_to_expr(val: &Val) -> Result<Expr, EvalError> {
    let span = Span::new(0, 0);
    match val {
        Val::Int(n) => Ok(Expr::new(ExprKind::Int(*n), span)),
        Val::Float(x) => Ok(Expr::new(ExprKind::Float(*x), span)),
        Val::Rational(n, d) => Ok(Expr::new(ExprKind::Rational(*n, *d), span)),
        Val::Bool(b) => Ok(Expr::new(ExprKind::Bool(*b), span)),
        Val::Str(s) => Ok(Expr::new(ExprKind::Str(s.clone()), span)),
        Val::Char(c) => Ok(Expr::new(ExprKind::Char(*c), span)),
        Val::Symbol(s) => Ok(Expr::new(ExprKind::Symbol(s.clone()), span)),
        Val::List(items) => {
            let exprs: Vec<Expr> = items.iter().map(val_to_expr).collect::<Result<_, _>>()?;
            Ok(Expr::new(ExprKind::List(exprs), span))
        }
        _ => Err(EvalError::Type("datum->syntax: cannot convert value to syntax".into())),
    }
}

