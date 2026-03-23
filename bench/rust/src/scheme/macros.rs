use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::scheme::env::Env;
use crate::scheme::error::{ErrorKind, EvalError, Span};
use crate::scheme::parser::{Expr, ExprKind};
use crate::scheme::value::Value;

static GENSYM_CTR: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_CTR.fetch_add(1, Ordering::Relaxed);
    format!("{}#{}", base, n)
}

const KEYWORDS: &[&str] = &[
    "if", "define", "set!", "quote", "lambda", "begin", "and", "or",
    "let", "cond", "define-syntax", "syntax-rules",
];

enum Binding {
    One(Expr),
    Many(Vec<Expr>),
}

/// Expand a macro call. Returns the expanded expression and the environment
/// to evaluate it in (may have gensym'd bindings for definition-site vars).
pub fn expand_macro(
    literals: &[String],
    rules: &[(Vec<Expr>, Expr)],
    def_env: &Rc<Env>,
    args: &[Expr],
    call_env: &Rc<Env>,
    span: Span,
) -> Result<(Expr, Rc<Env>), EvalError> {
    for (pattern, template) in rules {
        if let Some(bindings) = try_match(pattern, args, literals) {
            return Ok(expand(template, &bindings, def_env, call_env, span));
        }
    }
    Err(ErrorKind::BadSyntax {
        form: "macro".into(),
        message: "no matching pattern".into(),
    }
    .into())
}

// ── pattern matching ────────────────────────────────────────

fn try_match(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, Binding>> {
    let mut bindings = HashMap::new();
    if match_seq(pattern, input, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn match_seq(
    pat: &[Expr],
    inp: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let ellipsis_pos = pat
        .iter()
        .position(|e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."));

    if let Some(epos) = ellipsis_pos {
        if epos == 0 {
            return false;
        }
        let before = &pat[..epos - 1];
        let repeated_pat = &pat[epos - 1];
        let after = &pat[epos + 1..];

        if inp.len() < before.len() + after.len() {
            return false;
        }

        for (p, i) in before.iter().zip(inp.iter()) {
            if !match_one(p, i, literals, bindings) {
                return false;
            }
        }

        let after_start = inp.len() - after.len();
        for (p, i) in after.iter().zip(inp[after_start..].iter()) {
            if !match_one(p, i, literals, bindings) {
                return false;
            }
        }

        let repeated_input = &inp[before.len()..after_start];
        let pat_vars = pattern_vars(repeated_pat, literals);
        for var in &pat_vars {
            bindings.insert(var.clone(), Binding::Many(Vec::new()));
        }
        for elem in repeated_input {
            let mut sub = HashMap::new();
            if !match_one(repeated_pat, elem, literals, &mut sub) {
                return false;
            }
            for var in &pat_vars {
                if let Some(Binding::Many(ref mut vec)) = bindings.get_mut(var) {
                    if let Some(Binding::One(expr)) = sub.remove(var) {
                        vec.push(expr);
                    }
                }
            }
        }
        true
    } else {
        if pat.len() != inp.len() {
            return false;
        }
        for (p, i) in pat.iter().zip(inp.iter()) {
            if !match_one(p, i, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn match_one(
    pat: &Expr,
    inp: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    match &pat.kind {
        ExprKind::Symbol(name) if name == "_" => true,
        ExprKind::Symbol(name) if literals.contains(name) => {
            matches!(&inp.kind, ExprKind::Symbol(s) if s == name)
        }
        ExprKind::Symbol(name) => {
            bindings.insert(name.clone(), Binding::One(inp.clone()));
            true
        }
        ExprKind::List(pelems) => match &inp.kind {
            ExprKind::List(ielems) => match_seq(pelems, ielems, literals, bindings),
            _ => false,
        },
        ExprKind::Boolean(b) => matches!(&inp.kind, ExprKind::Boolean(b2) if b == b2),
        ExprKind::Integer(n) => matches!(&inp.kind, ExprKind::Integer(n2) if n == n2),
        ExprKind::Str(s) => matches!(&inp.kind, ExprKind::Str(s2) if s == s2),
        ExprKind::Char(c) => matches!(&inp.kind, ExprKind::Char(c2) if c == c2),
    }
}

fn pattern_vars(pat: &Expr, literals: &[String]) -> Vec<String> {
    let mut out = Vec::new();
    collect_pvars(pat, literals, &mut out);
    out
}

fn collect_pvars(pat: &Expr, literals: &[String], out: &mut Vec<String>) {
    match &pat.kind {
        ExprKind::Symbol(s) if s == "_" || s == "..." => {}
        ExprKind::Symbol(s) if literals.contains(s) => {}
        ExprKind::Symbol(s) => out.push(s.clone()),
        ExprKind::List(elems) => {
            for e in elems {
                collect_pvars(e, literals, out);
            }
        }
        _ => {}
    }
}

// ── template expansion ──────────────────────────────────────

fn expand(
    template: &Expr,
    bindings: &HashMap<String, Binding>,
    def_env: &Rc<Env>,
    call_env: &Rc<Env>,
    span: Span,
) -> (Expr, Rc<Env>) {
    // Collect free variables: not pattern vars, not keywords, in def-env, not macros
    let mut free_vars: HashMap<String, String> = HashMap::new(); // original → gensym
    // Collect introduced names: not pattern vars, not keywords, not in def-env
    let mut introduced: HashMap<String, String> = HashMap::new(); // original → gensym
    classify_symbols(template, bindings, def_env, &mut free_vars, &mut introduced);

    // Create env with def-site bindings for free vars
    let eval_env = if free_vars.is_empty() {
        Rc::clone(call_env)
    } else {
        let mut names = Vec::new();
        let mut values = Vec::new();
        for (orig, gs) in &free_vars {
            if let Some(val) = def_env.get(orig) {
                names.push(gs.clone());
                values.push(val);
            }
        }
        Env::extend(call_env, names, values)
    };

    let mut renames: HashMap<String, String> = HashMap::new();
    renames.extend(free_vars);
    renames.extend(introduced);

    let expanded = subst(template, bindings, def_env, &renames, span);
    (expanded, eval_env)
}

fn classify_symbols(
    template: &Expr,
    bindings: &HashMap<String, Binding>,
    def_env: &Rc<Env>,
    free_vars: &mut HashMap<String, String>,
    introduced: &mut HashMap<String, String>,
) {
    match &template.kind {
        ExprKind::Symbol(name) => {
            if bindings.contains_key(name)
                || KEYWORDS.contains(&name.as_str())
                || name == "..."
            {
                return;
            }
            if let Some(val) = def_env.get(name) {
                if !matches!(val, Value::SyntaxRules { .. }) && !free_vars.contains_key(name) {
                    free_vars.insert(name.clone(), gensym(name));
                }
            } else if !introduced.contains_key(name) {
                introduced.insert(name.clone(), gensym(name));
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                classify_symbols(e, bindings, def_env, free_vars, introduced);
            }
        }
        _ => {}
    }
}

fn subst(
    template: &Expr,
    bindings: &HashMap<String, Binding>,
    def_env: &Rc<Env>,
    renames: &HashMap<String, String>,
    span: Span,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(name) if name == "..." => template.clone(),
        ExprKind::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    Binding::One(expr) => expr.clone(),
                    Binding::Many(_) => template.clone(),
                }
            } else if let Some(new_name) = renames.get(name) {
                Expr {
                    kind: ExprKind::Symbol(new_name.clone()),
                    span: template.span,
                }
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len()
                    && matches!(&elems[i + 1].kind, ExprKind::Symbol(s) if s == "...")
                {
                    result.extend(expand_ellipsis(&elems[i], bindings, def_env, renames, span));
                    i += 2;
                } else {
                    result.push(subst(&elems[i], bindings, def_env, renames, span));
                    i += 1;
                }
            }
            Expr {
                kind: ExprKind::List(result),
                span: template.span,
            }
        }
        _ => template.clone(),
    }
}

fn expand_ellipsis(
    template: &Expr,
    bindings: &HashMap<String, Binding>,
    def_env: &Rc<Env>,
    renames: &HashMap<String, String>,
    span: Span,
) -> Vec<Expr> {
    // Find how many repetitions by looking at repeated bindings used in template
    let tvars = template_vars(template);
    let count = tvars
        .iter()
        .filter_map(|v| match bindings.get(v) {
            Some(Binding::Many(vec)) => Some(vec.len()),
            _ => None,
        })
        .next();

    let count = match count {
        Some(n) => n,
        None => return Vec::new(),
    };

    let mut result = Vec::new();
    for idx in 0..count {
        let mut sub_bindings: HashMap<String, Binding> = HashMap::new();
        for (k, v) in bindings {
            match v {
                Binding::Many(vec) => {
                    if idx < vec.len() {
                        sub_bindings.insert(k.clone(), Binding::One(vec[idx].clone()));
                    }
                }
                Binding::One(e) => {
                    sub_bindings.insert(k.clone(), Binding::One(e.clone()));
                }
            }
        }
        result.push(subst(template, &sub_bindings, def_env, renames, span));
    }
    result
}

fn template_vars(template: &Expr) -> Vec<String> {
    let mut out = Vec::new();
    collect_tvars(template, &mut out);
    out
}

fn collect_tvars(template: &Expr, out: &mut Vec<String>) {
    match &template.kind {
        ExprKind::Symbol(s) => out.push(s.clone()),
        ExprKind::List(elems) => {
            for e in elems {
                collect_tvars(e, out);
            }
        }
        _ => {}
    }
}
