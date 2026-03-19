use super::expr::{Env, Expr};
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{prefix}__h{n}")
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "set!", "quote", "lambda", "if", "begin", "let", "cond",
    "and", "or", "call/cc", "call-with-current-continuation",
    "define-syntax", "syntax-rules",
];

fn is_special_form(s: &str) -> bool {
    SPECIAL_FORMS.contains(&s)
}

#[derive(Debug, Clone)]
pub enum MacroBinding {
    Single(Expr),
    Variadic(Vec<Expr>),
}

fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match (pattern, input) {
        (Expr::Symbol(s), _) if literals.contains(s) => {
            matches!(input, Expr::Symbol(s2) if s2 == s)
        }
        (Expr::Symbol(s), _) if s == "_" => true,
        (Expr::Symbol(s), _) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        (Expr::List(pats), Expr::List(inputs)) => {
            match_list_pattern(pats, inputs, literals, bindings)
        }
        (Expr::Integer(a), Expr::Integer(b)) => a == b,
        (Expr::Boolean(a), Expr::Boolean(b)) => a == b,
        (Expr::Str(a), Expr::Str(b)) => a == b,
        _ => false,
    }
}

fn all_match(
    pairs: &[(&Expr, &Expr)],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    pairs.iter().all(|(p, i)| match_pattern(p, i, literals, bindings))
}

fn match_ellipsis(
    pats: &[Expr],
    ep: usize,
    inputs: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let var_pat = &pats[ep - 1];
    let fixed_before = &pats[..ep - 1];
    let fixed_after = &pats[ep + 1..];
    let min_inputs = fixed_before.len() + fixed_after.len();
    if inputs.len() < min_inputs {
        return false;
    }
    let before_pairs: Vec<_> = fixed_before.iter().zip(inputs.iter()).collect();
    if !all_match(&before_pairs, literals, bindings) {
        return false;
    }
    let after_start = inputs.len() - fixed_after.len();
    let after_pairs: Vec<_> = fixed_after.iter().zip(inputs[after_start..].iter()).collect();
    if !all_match(&after_pairs, literals, bindings) {
        return false;
    }
    let var_inputs = &inputs[fixed_before.len()..after_start];
    match var_pat {
        Expr::Symbol(var_name) if !literals.contains(var_name) => {
            bindings.insert(var_name.clone(), MacroBinding::Variadic(var_inputs.to_vec()));
            true
        }
        _ => false,
    }
}

fn match_list_pattern(
    pats: &[Expr],
    inputs: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let ellipsis_pos = pats
        .iter()
        .position(|p| matches!(p, Expr::Symbol(s) if s == "..."));
    if let Some(ep) = ellipsis_pos {
        if ep == 0 {
            return false;
        }
        return match_ellipsis(pats, ep, inputs, literals, bindings);
    }
    if pats.len() != inputs.len() {
        return false;
    }
    let pairs: Vec<_> = pats.iter().zip(inputs.iter()).collect();
    all_match(&pairs, literals, bindings)
}

fn collect_pattern_vars(pattern: &Expr, literals: &[String], vars: &mut HashSet<String>) {
    match pattern {
        Expr::Symbol(s) if s == "..." || s == "_" || literals.contains(s) => {}
        Expr::Symbol(s) => {
            vars.insert(s.clone());
        }
        Expr::List(elems) => {
            for e in elems {
                collect_pattern_vars(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn collect_free_symbols(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    free: &mut HashSet<String>,
) {
    match template {
        Expr::Symbol(s) if s == "..." || pattern_vars.contains(s) => {}
        Expr::Symbol(s) => {
            free.insert(s.clone());
        }
        Expr::List(elems) => {
            for e in elems {
                collect_free_symbols(e, pattern_vars, free);
            }
        }
        _ => {}
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Result<Expr, String> {
    match template {
        Expr::Symbol(s) if s == "..." => Ok(template.clone()),
        Expr::Symbol(s) => expand_symbol(s, template, bindings, renames),
        Expr::List(elems) => expand_list_template(elems, bindings, renames),
        _ => Ok(template.clone()),
    }
}

fn expand_symbol(
    s: &str,
    original: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Result<Expr, String> {
    if let Some(MacroBinding::Single(val)) = bindings.get(s) {
        return Ok(val.clone());
    }
    if let Some(renamed) = renames.get(s) {
        return Ok(Expr::Symbol(renamed.clone()));
    }
    Ok(original.clone())
}

fn expand_list_template(
    elems: &[Expr],
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Result<Expr, String> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < elems.len() {
        let is_followed_by_ellipsis = i + 1 < elems.len()
            && matches!(&elems[i + 1], Expr::Symbol(s) if s == "...");
        if is_followed_by_ellipsis {
            result.extend(expand_variadic(&elems[i], bindings, renames)?);
            i += 2;
        } else {
            result.push(expand_template(&elems[i], bindings, renames)?);
            i += 1;
        }
    }
    Ok(Expr::List(result))
}

fn expand_variadic(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Result<Vec<Expr>, String> {
    let Some(name) = find_variadic_var(template, bindings) else {
        return Ok(vec![expand_template(template, bindings, renames)?]);
    };
    let Some(MacroBinding::Variadic(vals)) = bindings.get(&name) else {
        return Ok(vec![expand_template(template, bindings, renames)?]);
    };
    vals.iter()
        .map(|val| {
            let mut temp = bindings.clone();
            temp.insert(name.clone(), MacroBinding::Single(val.clone()));
            expand_template(template, &temp, renames)
        })
        .collect()
}

fn find_variadic_var(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
) -> Option<String> {
    match template {
        Expr::Symbol(s) if matches!(bindings.get(s), Some(MacroBinding::Variadic(_))) => {
            Some(s.clone())
        }
        Expr::List(elems) => elems.iter().find_map(|e| find_variadic_var(e, bindings)),
        _ => None,
    }
}

fn build_hygiene(
    free_syms: &HashSet<String>,
    def_env: &Env,
) -> (HashMap<String, String>, Vec<(String, Expr)>) {
    let mut renames = HashMap::new();
    let mut hygiene_bindings = Vec::new();
    for sym in free_syms {
        if is_special_form(sym) {
            continue;
        }
        let unique = gensym(sym);
        renames.insert(sym.clone(), unique.clone());
        if let Some(val) = def_env.get(sym) {
            hygiene_bindings.push((unique, val));
        }
    }
    (renames, hygiene_bindings)
}

/// Expand a macro invocation. Returns the expanded expression and the
/// environment it should be evaluated in (with hygiene bindings).
pub fn expand_macro(
    rules: &[(Expr, Expr)],
    literals: &[String],
    args: &[Expr],
    def_env: &Env,
    use_env: &Env,
) -> Result<(Expr, Env), String> {
    for (pattern, template) in rules {
        let mut bindings = HashMap::new();
        let pat_args = match pattern {
            Expr::List(elems) if !elems.is_empty() => &elems[1..],
            _ => continue,
        };
        if !match_list_pattern(pat_args, args, literals, &mut bindings) {
            continue;
        }
        let pattern_vars = gather_pattern_vars(pattern, literals);
        let mut free_syms = HashSet::new();
        collect_free_symbols(template, &pattern_vars, &mut free_syms);
        let (renames, hygiene_bindings) = build_hygiene(&free_syms, def_env);
        let expanded = expand_template(template, &bindings, &renames)?;
        let eval_env = use_env.child();
        for (name, val) in hygiene_bindings {
            eval_env.insert(name, val);
        }
        return Ok((expanded, eval_env));
    }
    Err("no matching pattern in syntax-rules".into())
}

fn gather_pattern_vars(pattern: &Expr, literals: &[String]) -> HashSet<String> {
    let mut vars = HashSet::new();
    if let Expr::List(elems) = pattern {
        for e in &elems[1..] {
            collect_pattern_vars(e, literals, &mut vars);
        }
    }
    vars
}
