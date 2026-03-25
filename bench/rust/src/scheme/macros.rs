use std::collections::HashMap;

use super::{
    eval, gensym, Env, Expr, ExprKind, Span, Val, EvalError,
};

#[derive(Clone, Debug)]
pub(crate) enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

pub(super) const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "begin",
    "let", "let*", "cond", "set!", "string-set!", "define-syntax", "syntax-rules",
    "define-record-type", "case-lambda", "letrec", "letrec*", "case", "do",
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

pub(super) fn eval_macro_call(
    elems: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    span: Span,
) -> Result<Val, EvalError> {
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
                return eval(&expanded, use_env);
            } else {
                let new_env = use_env.push();
                for (orig, gs) in &renames {
                    if let Some(val) = def_env.get(orig) {
                        new_env.define(gs.clone(), val);
                    }
                }
                return eval(&expanded, &new_env);
            }
        }
    }
    Err(EvalError::Runtime(format!("no matching syntax rule at {span}")))
}
