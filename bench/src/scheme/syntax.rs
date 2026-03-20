use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::scheme::env::{self, Env};
use crate::scheme::error::SchemeError;
use crate::scheme::value::{self, MacroData, Value};

static GENSYM_CTR: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_CTR.fetch_add(1, Ordering::Relaxed);
    format!("{base}__{n}")
}

// ---------------------------------------------------------------------------
// Parse (syntax-rules (literals...) (pat tmpl) ...)
// ---------------------------------------------------------------------------

pub fn parse_syntax_rules(form: &Value, def_env: Env) -> Result<MacroData, SchemeError> {
    let items = value::to_vec(form)?;
    if items.len() < 2 {
        return Err(bad());
    }
    let Value::Symbol(ref kw) = items[0] else {
        return Err(bad());
    };
    if kw != "syntax-rules" {
        return Err(bad());
    }
    let literals = parse_literals(&items[1])?;
    let mut rules = Vec::new();
    for item in &items[2..] {
        let pair = value::to_vec(item)?;
        if pair.len() != 2 {
            return Err(bad());
        }
        rules.push((pair[0].clone(), pair[1].clone()));
    }
    Ok(MacroData {
        literals,
        rules,
        def_env,
    })
}

fn parse_literals(val: &Value) -> Result<Vec<String>, SchemeError> {
    let items = value::to_vec(val)?;
    items
        .into_iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s),
            _ => Err(bad()),
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Expand a macro invocation
// ---------------------------------------------------------------------------

/// Expand `(macro-name arg...)` using the macro's rules.
pub fn expand(mac: &MacroData, form: &[Value]) -> Result<Value, SchemeError> {
    for (pattern, template) in &mac.rules {
        if let Some(result) = try_expand_rule(pattern, template, form, mac)? {
            return Ok(result);
        }
    }
    Err(SchemeError::BadSyntax {
        form: "no matching macro rule".into(),
    })
}

fn try_expand_rule(
    pattern: &Value,
    template: &Value,
    form: &[Value],
    mac: &MacroData,
) -> Result<Option<Value>, SchemeError> {
    let pat_items = value::to_vec(pattern)?;
    let Some(bindings) = match_rule(&pat_items, form, &mac.literals) else {
        return Ok(None);
    };
    let (expanded, pre_binds) = instantiate(template, &bindings, &mac.def_env);
    if pre_binds.is_empty() {
        return Ok(Some(expanded));
    }
    Ok(Some(wrap_with_pre_bindings(expanded, pre_binds, &mac.def_env)))
}

fn wrap_with_pre_bindings(
    body: Value,
    pre_binds: Vec<(String, String)>,
    def_env: &Env,
) -> Value {
    let mut bindings = Vec::new();
    for (gs, orig) in pre_binds {
        if let Ok(val) = env::lookup(def_env, &orig) {
            let pair = value::from_vec(vec![Value::Symbol(gs), quote_value(val)]);
            bindings.push(pair);
        }
    }
    if bindings.is_empty() {
        return body;
    }
    let let_bindings = value::from_vec(bindings);
    value::from_vec(vec![
        Value::Symbol("let".into()),
        let_bindings,
        body,
    ])
}

fn quote_value(val: Value) -> Value {
    value::from_vec(vec![Value::Symbol("quote".into()), val])
}

// ---------------------------------------------------------------------------
// Pattern matching
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Binding {
    Single(Value),
    List(Vec<Value>),
}

fn match_rule(
    pat: &[Value],
    form: &[Value],
    literals: &[String],
) -> Option<HashMap<String, Binding>> {
    // pat[0] is the macro name — skip it, skip form[0] too
    let mut bindings = HashMap::new();
    match_elems(&pat[1..], &form[1..], literals, &mut bindings)?;
    Some(bindings)
}

fn match_elems(
    pats: &[Value],
    forms: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> Option<()> {
    let mut pi = 0;
    let mut fi = 0;
    while pi < pats.len() {
        if has_ellipsis(pats, pi) {
            return match_ellipsis(pats, pi, forms, fi, literals, bindings);
        }
        if fi >= forms.len() {
            return None;
        }
        match_single(&pats[pi], &forms[fi], literals, bindings)?;
        pi += 1;
        fi += 1;
    }
    if fi == forms.len() { Some(()) } else { None }
}

fn has_ellipsis(pats: &[Value], idx: usize) -> bool {
    idx + 1 < pats.len() && is_ellipsis(&pats[idx + 1])
}

fn match_ellipsis(
    pats: &[Value],
    pi: usize,
    forms: &[Value],
    fi: usize,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> Option<()> {
    let pat = &pats[pi];
    let after_ellipsis = pats.len() - pi - 2;
    let available = forms.len().checked_sub(fi)?;
    let count = available.checked_sub(after_ellipsis)?;
    let matched: Vec<Value> = forms[fi..fi + count].to_vec();
    if let Value::Symbol(name) = pat {
        bindings.insert(name.clone(), Binding::List(matched));
    }
    let new_fi = fi + count;
    match_elems(&pats[pi + 2..], &forms[new_fi..], literals, bindings)
}

fn match_single(
    pat: &Value,
    form: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> Option<()> {
    match pat {
        Value::Symbol(name) if name == "_" => Some(()),
        Value::Symbol(name) if literals.contains(name) => {
            match form {
                Value::Symbol(fname) if fname == name => Some(()),
                _ => None,
            }
        }
        Value::Symbol(name) => {
            bindings.insert(name.clone(), Binding::Single(form.clone()));
            Some(())
        }
        Value::Pair(_, _) => {
            let p = value::to_vec(pat).ok()?;
            let f = value::to_vec(form).ok()?;
            match_elems(&p, &f, literals, bindings)
        }
        _ => {
            // Literal match (numbers, booleans, etc.)
            if format!("{pat}") == format!("{form}") { Some(()) } else { None }
        }
    }
}

// ---------------------------------------------------------------------------
// Template instantiation
// ---------------------------------------------------------------------------

/// Returns (expanded_value, pre_bindings: [(gensym, original_name)])
fn instantiate(
    tmpl: &Value,
    bindings: &HashMap<String, Binding>,
    def_env: &Env,
) -> (Value, Vec<(String, String)>) {
    let mut renames = HashMap::new();
    let mut pre = Vec::new();
    let expanded = inst_rec(tmpl, bindings, def_env, &mut renames, &mut pre);
    (expanded, pre)
}

fn inst_rec(
    tmpl: &Value,
    bindings: &HashMap<String, Binding>,
    def_env: &Env,
    renames: &mut HashMap<String, String>,
    pre: &mut Vec<(String, String)>,
) -> Value {
    match tmpl {
        Value::Symbol(name) => inst_symbol(name, bindings, def_env, renames, pre),
        Value::Pair(_, _) => inst_list(tmpl, bindings, def_env, renames, pre),
        other => other.clone(),
    }
}

fn inst_symbol(
    name: &str,
    bindings: &HashMap<String, Binding>,
    _def_env: &Env,
    renames: &mut HashMap<String, String>,
    pre: &mut Vec<(String, String)>,
) -> Value {
    if let Some(Binding::Single(v)) = bindings.get(name) {
        return v.clone();
    }
    if is_keyword(name) {
        return Value::Symbol(name.to_string());
    }
    let gs = renames
        .entry(name.to_string())
        .or_insert_with(|| gensym(name))
        .clone();
    if !pre.iter().any(|(g, _)| g == &gs) {
        pre.push((gs.clone(), name.to_string()));
    }
    Value::Symbol(gs)
}

fn inst_list(
    tmpl: &Value,
    bindings: &HashMap<String, Binding>,
    def_env: &Env,
    renames: &mut HashMap<String, String>,
    pre: &mut Vec<(String, String)>,
) -> Value {
    let Ok(items) = value::to_vec(tmpl) else {
        return tmpl.clone();
    };
    let mut result = Vec::new();
    let mut i = 0;
    while i < items.len() {
        if has_ellipsis_in(&items, i) {
            expand_ellipsis(&items[i], bindings, &mut result);
            i += 2;
        } else {
            result.push(inst_rec(&items[i], bindings, def_env, renames, pre));
            i += 1;
        }
    }
    value::from_vec(result)
}

fn has_ellipsis_in(items: &[Value], idx: usize) -> bool {
    idx + 1 < items.len() && is_ellipsis(&items[idx + 1])
}

fn expand_ellipsis(
    tmpl_elem: &Value,
    bindings: &HashMap<String, Binding>,
    out: &mut Vec<Value>,
) {
    // Find which binding is a list (the ellipsis variable)
    let list_binding = find_list_binding(tmpl_elem, bindings);
    let Some((_, vals)) = list_binding else {
        return;
    };
    for v in vals {
        // For each repetition, substitute the ellipsis var with one element
        let expanded = subst_single(tmpl_elem, bindings, &v);
        out.push(expanded);
    }
}

fn find_list_binding(
    tmpl: &Value,
    bindings: &HashMap<String, Binding>,
) -> Option<(String, Vec<Value>)> {
    if let Value::Symbol(name) = tmpl {
        if let Some(Binding::List(vals)) = bindings.get(name.as_str()) {
            return Some((name.clone(), vals.clone()));
        }
    }
    None
}

fn subst_single(
    tmpl: &Value,
    bindings: &HashMap<String, Binding>,
    single_val: &Value,
) -> Value {
    match tmpl {
        Value::Symbol(name) => {
            if let Some(Binding::List(_)) = bindings.get(name.as_str()) {
                return single_val.clone();
            }
            if let Some(Binding::Single(v)) = bindings.get(name.as_str()) {
                return v.clone();
            }
            tmpl.clone()
        }
        Value::Pair(_, _) => {
            let Ok(items) = value::to_vec(tmpl) else {
                return tmpl.clone();
            };
            let expanded: Vec<Value> = items
                .iter()
                .map(|item| subst_single(item, bindings, single_val))
                .collect();
            value::from_vec(expanded)
        }
        other => other.clone(),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_ellipsis(val: &Value) -> bool {
    matches!(val, Value::Symbol(s) if s == "...")
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "if"
            | "quote"
            | "lambda"
            | "begin"
            | "cond"
            | "let"
            | "set!"
            | "and"
            | "or"
            | "define-syntax"
            | "syntax-rules"
            | "else"
    )
}

fn bad() -> SchemeError {
    SchemeError::BadSyntax {
        form: "syntax-rules".into(),
    }
}
