use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{base}_hyg_{id}")
}

const SPECIAL_FORMS: &[&str] = &[
    "define",
    "set!",
    "quote",
    "lambda",
    "if",
    "begin",
    "and",
    "or",
    "let",
    "cond",
    "call/cc",
    "call-with-current-continuation",
    "define-syntax",
];

#[derive(Debug, Clone)]
enum Binding {
    Single(Value),
    Sequence(Vec<Value>),
}

type Bindings = HashMap<String, Binding>;

/// Parse `(define-syntax name (syntax-rules (...) clauses...))`.
pub fn parse_define_syntax(
    args: &[Value],
    env: &Env,
) -> Result<(String, Value), EvalError> {
    let (name, sr) = validate_define_syntax_args(args)?;
    let (literals, rules) = parse_syntax_rules(sr)?;
    Ok((
        name.to_owned(),
        Value::Macro {
            literals,
            rules,
            def_env: env.clone(),
        },
    ))
}

fn validate_define_syntax_args(args: &[Value]) -> Result<(&str, &[Value]), EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Parse {
            msg: "define-syntax requires 2 arguments".into(),
        });
    }
    let Value::Symbol(name) = &args[0] else {
        return Err(EvalError::Parse {
            msg: "define-syntax: name must be a symbol".into(),
        });
    };
    let Value::List(sr) = &args[1] else {
        return Err(EvalError::Parse {
            msg: "define-syntax: expected syntax-rules".into(),
        });
    };
    Ok((name, sr))
}

type SyntaxRulesData = (Vec<String>, Vec<(Value, Value)>);

fn parse_syntax_rules(sr: &[Value]) -> Result<SyntaxRulesData, EvalError> {
    if sr.len() < 2 || !matches!(&sr[0], Value::Symbol(s) if s == "syntax-rules") {
        return Err(EvalError::Parse {
            msg: "expected syntax-rules".into(),
        });
    }
    let Value::List(lits) = &sr[1] else {
        return Err(EvalError::Parse {
            msg: "syntax-rules: literals must be a list".into(),
        });
    };
    let literals = extract_literal_names(lits)?;
    let rules = extract_rules(&sr[2..])?;
    Ok((literals, rules))
}

fn extract_literal_names(lits: &[Value]) -> Result<Vec<String>, EvalError> {
    lits.iter()
        .map(|v| match v {
            Value::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::Parse {
                msg: "literal must be a symbol".into(),
            }),
        })
        .collect()
}

fn extract_rules(clauses: &[Value]) -> Result<Vec<(Value, Value)>, EvalError> {
    clauses
        .iter()
        .map(|rule| {
            let Value::List(pair) = rule else {
                return Err(EvalError::Parse {
                    msg: "syntax-rules clause must be a list".into(),
                });
            };
            if pair.len() != 2 {
                return Err(EvalError::Parse {
                    msg: "clause needs pattern and template".into(),
                });
            }
            Ok((pair[0].clone(), pair[1].clone()))
        })
        .collect()
}

// ── expansion ────────────────────────────────────────────────────────

/// Expand a macro invocation. `input` includes the macro name at [0].
pub fn expand_macro(
    macro_val: &Value,
    input: &[Value],
    use_env: &Env,
) -> Result<Value, EvalError> {
    let Value::Macro {
        literals,
        rules,
        def_env,
    } = macro_val
    else {
        return Err(EvalError::Parse {
            msg: "not a macro".into(),
        });
    };
    let macro_name = extract_head_name(input);
    try_expand(rules, literals, input, &macro_name, def_env, use_env)
}

fn extract_head_name(input: &[Value]) -> String {
    match &input[0] {
        Value::Symbol(s) => s.clone(),
        _ => String::new(),
    }
}

fn try_expand(
    rules: &[(Value, Value)],
    literals: &[String],
    input: &[Value],
    macro_name: &str,
    def_env: &Env,
    use_env: &Env,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let Some(bindings) = match_pattern(pattern, input, literals) else {
            continue;
        };
        let pvars = collect_pattern_vars(pattern, literals);
        let renames = compute_renames(template, &pvars, macro_name, def_env);
        let mut renamed = template.clone();
        apply_renames(&mut renamed, &renames);
        install_hygiene_bindings(&renames, def_env, use_env);
        return substitute(&renamed, &bindings);
    }
    Err(EvalError::Parse {
        msg: format!("no matching syntax-rules clause for {macro_name}"),
    })
}

fn install_hygiene_bindings(
    renames: &HashMap<String, String>,
    def_env: &Env,
    use_env: &Env,
) {
    for (original, gen) in renames {
        if let Some(val) = def_env.get(original) {
            use_env.define(gen.clone(), val);
        }
    }
}

// ── pattern matching ─────────────────────────────────────────────────

fn match_pattern(
    pattern: &Value,
    input: &[Value],
    literals: &[String],
) -> Option<Bindings> {
    let Value::List(pat_elems) = pattern else {
        return None;
    };
    let mut bindings = Bindings::new();
    let ok = match_elems(&pat_elems[1..], &input[1..], literals, &mut bindings);
    ok.then_some(bindings)
}

fn match_elems(
    pat: &[Value],
    input: &[Value],
    literals: &[String],
    b: &mut Bindings,
) -> bool {
    if has_trailing_ellipsis(pat) {
        return match_ellipsis(pat, input, literals, b);
    }
    if pat.len() != input.len() {
        return false;
    }
    pat.iter()
        .zip(input.iter())
        .all(|(p, v)| match_one(p, v, literals, b))
}

fn has_trailing_ellipsis(pat: &[Value]) -> bool {
    pat.len() >= 2 && matches!(&pat[pat.len() - 1], Value::Symbol(s) if s == "...")
}

fn match_ellipsis(
    pat: &[Value],
    input: &[Value],
    literals: &[String],
    b: &mut Bindings,
) -> bool {
    let fixed = pat.len() - 2;
    if input.len() < fixed {
        return false;
    }
    for i in 0..fixed {
        if !match_one(&pat[i], &input[i], literals, b) {
            return false;
        }
    }
    let Value::Symbol(name) = &pat[fixed] else {
        return false;
    };
    if literals.contains(name) {
        return false;
    }
    b.insert(name.clone(), Binding::Sequence(input[fixed..].to_vec()));
    true
}

fn match_one(
    pat: &Value,
    input: &Value,
    literals: &[String],
    b: &mut Bindings,
) -> bool {
    match pat {
        Value::Symbol(s) if literals.contains(s) => {
            matches!(input, Value::Symbol(t) if t == s)
        }
        Value::Symbol(s) => {
            b.insert(s.clone(), Binding::Single(input.clone()));
            true
        }
        Value::List(pe) => match input {
            Value::List(ie) => match_elems(pe, ie, literals, b),
            _ => false,
        },
        _ => pat == input,
    }
}

// ── template substitution ────────────────────────────────────────────

fn substitute(template: &Value, bindings: &Bindings) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(name) => match bindings.get(name) {
            Some(Binding::Single(val)) => Ok(val.clone()),
            _ => Ok(template.clone()),
        },
        Value::List(elems) => substitute_list(elems, bindings),
        _ => Ok(template.clone()),
    }
}

fn substitute_list(
    elems: &[Value],
    bindings: &Bindings,
) -> Result<Value, EvalError> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < elems.len() {
        if is_followed_by_ellipsis(elems, i) {
            out.extend(expand_ellipsis(&elems[i], bindings)?);
            i += 2;
        } else {
            out.push(substitute(&elems[i], bindings)?);
            i += 1;
        }
    }
    Ok(Value::List(out))
}

fn is_followed_by_ellipsis(elems: &[Value], i: usize) -> bool {
    i + 1 < elems.len() && matches!(&elems[i + 1], Value::Symbol(s) if s == "...")
}

fn expand_ellipsis(
    elem: &Value,
    bindings: &Bindings,
) -> Result<Vec<Value>, EvalError> {
    let Some(var) = find_seq_var(elem, bindings) else {
        return Ok(Vec::new());
    };
    let Some(Binding::Sequence(vals)) = bindings.get(&var) else {
        return Ok(Vec::new());
    };
    vals.iter()
        .map(|v| {
            let mut b2 = bindings.clone();
            b2.insert(var.clone(), Binding::Single(v.clone()));
            substitute(elem, &b2)
        })
        .collect()
}

fn find_seq_var(template: &Value, bindings: &Bindings) -> Option<String> {
    match template {
        Value::Symbol(s) if matches!(bindings.get(s), Some(Binding::Sequence(_))) => {
            Some(s.clone())
        }
        Value::List(elems) => elems.iter().find_map(|e| find_seq_var(e, bindings)),
        _ => None,
    }
}

// ── hygiene ──────────────────────────────────────────────────────────

fn collect_pattern_vars(pattern: &Value, literals: &[String]) -> HashSet<String> {
    let mut vars = HashSet::new();
    gather_pvars(pattern, literals, &mut vars, true);
    vars
}

fn gather_pvars(
    pat: &Value,
    literals: &[String],
    out: &mut HashSet<String>,
    is_top: bool,
) {
    match pat {
        Value::Symbol(s) if s == "..." => {}
        Value::Symbol(s) if !is_top && !literals.contains(s) => {
            out.insert(s.clone());
        }
        Value::Symbol(_) => {}
        Value::List(elems) => {
            for (i, e) in elems.iter().enumerate() {
                gather_pvars(e, literals, out, i == 0 && is_top);
            }
        }
        _ => {}
    }
}

fn compute_renames(
    template: &Value,
    pvars: &HashSet<String>,
    macro_name: &str,
    def_env: &Env,
) -> HashMap<String, String> {
    let special: HashSet<&str> = SPECIAL_FORMS.iter().copied().collect();
    let mut renames = HashMap::new();
    find_free(template, pvars, macro_name, &special, def_env, &mut renames);
    renames
}

fn find_free(
    expr: &Value,
    pvars: &HashSet<String>,
    macro_name: &str,
    special: &HashSet<&str>,
    def_env: &Env,
    renames: &mut HashMap<String, String>,
) {
    match expr {
        Value::Symbol(s) if s == "..." => {}
        Value::Symbol(s) => maybe_rename(s, pvars, macro_name, special, def_env, renames),
        Value::List(elems) => {
            for e in elems {
                find_free(e, pvars, macro_name, special, def_env, renames);
            }
        }
        _ => {}
    }
}

fn maybe_rename(
    s: &str,
    pvars: &HashSet<String>,
    macro_name: &str,
    special: &HashSet<&str>,
    def_env: &Env,
    renames: &mut HashMap<String, String>,
) {
    if pvars.contains(s) || special.contains(s) || s == macro_name {
        return;
    }
    if renames.contains_key(s) {
        return;
    }
    let Some(val) = def_env.get(s) else { return };
    if matches!(val, Value::Macro { .. }) {
        return;
    }
    renames.insert(s.to_owned(), gensym(s));
}

fn apply_renames(expr: &mut Value, renames: &HashMap<String, String>) {
    match expr {
        Value::Symbol(s) => {
            if let Some(new) = renames.get(s.as_str()) {
                *s = new.clone();
            }
        }
        Value::List(elems) => elems.iter_mut().for_each(|e| apply_renames(e, renames)),
        _ => {}
    }
}
