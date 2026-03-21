//! Hygienic macro expansion for `define-syntax` / `syntax-rules`.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::{Env, EvalError, Expr, Value};

/// Binding injections from definition-site env into use-site env.
type Injections = Vec<(String, Rc<RefCell<Value>>)>;

/// Result of matching a single pattern variable.
#[derive(Debug, Clone)]
enum MatchBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

/// Try to match input args against pattern args (both excluding the macro name
/// at position 0 in the pattern).
fn match_pattern(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
) -> Option<HashMap<String, MatchBinding>> {
    let mut bindings = HashMap::new();
    match_elements(&pattern[1..], input, literals, &mut bindings)?;
    Some(bindings)
}

fn match_ellipsis(
    pattern: &[Expr],
    pi: usize,
    input: &[Expr],
    ii: usize,
    bindings: &mut HashMap<String, MatchBinding>,
) -> Option<usize> {
    let Expr::Atom(var, _) = &pattern[pi] else {
        return None;
    };
    let remaining = pattern.len() - pi - 2;
    let available = input.len().checked_sub(ii)?;
    let consume = available.checked_sub(remaining)?;
    bindings.insert(
        var.clone(),
        MatchBinding::Many(input[ii..ii + consume].to_vec()),
    );
    Some(consume)
}

fn match_elements(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MatchBinding>,
) -> Option<()> {
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        let ellipsis_next = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1], Expr::Atom(s, _) if s == "...");

        if ellipsis_next {
            let consume = match_ellipsis(pattern, pi, input, ii, bindings)?;
            ii += consume;
            pi += 2;
        } else if ii >= input.len() {
            return None;
        } else {
            match_single(&pattern[pi], &input[ii], literals, bindings)?;
            pi += 1;
            ii += 1;
        }
    }

    (ii == input.len()).then_some(())
}

fn match_single(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MatchBinding>,
) -> Option<()> {
    match pattern {
        Expr::Atom(name, _) if literals.contains(name) => match input {
            Expr::Atom(n, _) if n == name => Some(()),
            _ => None,
        },
        Expr::Atom(name, _) => {
            bindings.insert(name.clone(), MatchBinding::Single(input.clone()));
            Some(())
        }
        Expr::List(pat_elems, _) => {
            let Expr::List(input_elems, _) = input else {
                return None;
            };
            match_elements(pat_elems, input_elems, literals, bindings)
        }
    }
}

// ── Template substitution ──────────────────────────────────────────

fn substitute_template(
    template: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match template {
        Expr::Atom(name, span) => {
            if let Some(gensym) = renames.get(name) {
                Expr::Atom(gensym.clone(), *span)
            } else if let Some(MatchBinding::Single(expr)) = bindings.get(name) {
                expr.clone()
            } else {
                template.clone()
            }
        }
        Expr::List(elems, span) => {
            Expr::List(substitute_list(elems, bindings, renames), *span)
        }
    }
}

fn substitute_ellipsis(
    elem: &Expr,
    bindings: &HashMap<String, MatchBinding>,
    renames: &HashMap<String, String>,
    out: &mut Vec<Expr>,
) {
    if let Expr::Atom(name, _) = elem {
        if let Some(MatchBinding::Many(exprs)) = bindings.get(name) {
            out.extend(exprs.iter().cloned());
            return;
        }
    }
    out.push(substitute_template(elem, bindings, renames));
}

fn substitute_list(
    elems: &[Expr],
    bindings: &HashMap<String, MatchBinding>,
    renames: &HashMap<String, String>,
) -> Vec<Expr> {
    let mut out = Vec::new();
    let mut i = 0;

    while i < elems.len() {
        let ellipsis_next = i + 1 < elems.len()
            && matches!(&elems[i + 1], Expr::Atom(s, _) if s == "...");

        if ellipsis_next {
            substitute_ellipsis(&elems[i], bindings, renames, &mut out);
            i += 2;
        } else {
            out.push(substitute_template(&elems[i], bindings, renames));
            i += 1;
        }
    }
    out
}

// ── Free-variable analysis for hygiene ─────────────────────────────

fn is_self_evaluating(name: &str) -> bool {
    name.parse::<i64>().is_ok()
        || name == "#t"
        || name == "#f"
        || name.starts_with("#\\")
        || (name.starts_with('"') && name.ends_with('"'))
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "syntax-rules"
            | "if"
            | "quote"
            | "and"
            | "or"
            | "lambda"
            | "let"
            | "begin"
            | "cond"
            | "display"
            | "write"
            | "newline"
            | "set!"
            | "string-set!"
            | "call/cc"
            | "call-with-current-continuation"
            | "map"
            | "..."
    )
}

fn collect_free_vars(expr: &Expr, pattern_vars: &HashSet<&String>, out: &mut HashSet<String>) {
    match expr {
        Expr::Atom(name, _) => {
            if !pattern_vars.contains(name)
                && !is_self_evaluating(name)
                && !is_special_form(name)
            {
                out.insert(name.clone());
            }
        }
        Expr::List(elems, _) => {
            elems
                .iter()
                .filter(|elem| !matches!(elem, Expr::Atom(s, _) if s == "..."))
                .for_each(|elem| collect_free_vars(elem, pattern_vars, out));
        }
    }
}

// ── Public expansion entry point ───────────────────────────────────

fn build_injections(
    free: &HashSet<String>,
    def_env: &Env,
    gensym_counter: &mut u64,
) -> (HashMap<String, String>, Injections) {
    let mut renames = HashMap::new();
    let mut injections = Vec::new();
    for var in free {
        let Some(binding_rc) = def_env.get(var) else {
            continue;
        };
        let gensym = format!("{var}__g{}", *gensym_counter);
        *gensym_counter += 1;
        renames.insert(var.clone(), gensym.clone());
        injections.push((gensym, Rc::clone(binding_rc)));
    }
    (renames, injections)
}

/// Expand a macro invocation. Returns the expanded expression plus any
/// definition-site bindings that must be injected into the use-site env
/// for hygiene (gensymed name → shared `Rc<RefCell<Value>>`).
pub fn expand(
    rules: &[(Vec<Expr>, Expr)],
    literals: &[String],
    input_args: &[Expr],
    def_env: &Env,
    gensym_counter: &mut u64,
) -> Result<(Expr, Injections), EvalError> {
    for (pattern, template) in rules {
        let Some(bindings) = match_pattern(pattern, input_args, literals) else {
            continue;
        };

        let pattern_vars: HashSet<&String> = bindings.keys().collect();

        let mut free = HashSet::new();
        collect_free_vars(template, &pattern_vars, &mut free);

        let (renames, injections) = build_injections(&free, def_env, gensym_counter);

        let expanded = substitute_template(template, &bindings, &renames);
        return Ok((expanded, injections));
    }

    Err(EvalError::Parse {
        message: "no matching syntax-rules pattern".to_string(),
    })
}
