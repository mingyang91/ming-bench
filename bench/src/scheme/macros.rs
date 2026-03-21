use super::Env;
use crate::scheme::builtins::is_builtin;
use crate::scheme::error::EvalError;
use crate::scheme::value::{Span, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("\x00{base}_{id}")
}

/// Evaluate `(define-syntax name (syntax-rules (literals...) rule...))`.
pub(crate) fn eval_define_syntax(
    args: &[Value],
    env: &mut Env,
    span: Span,
) -> Result<Value, EvalError> {
    let [Value::Symbol(name, _), transformer] = args else {
        return Err(EvalError::Parse {
            message: "define-syntax requires a name and a transformer".to_string(),
            span,
        });
    };
    let Value::List(items, _) = transformer else {
        return Err(EvalError::Parse {
            message: "define-syntax transformer must be syntax-rules".to_string(),
            span,
        });
    };
    let [Value::Symbol(sr, _), Value::List(lit_list, _), rules @ ..] = items.as_slice() else {
        return Err(EvalError::Parse {
            message: "syntax-rules requires literals list and rules".to_string(),
            span,
        });
    };
    if sr != "syntax-rules" {
        return Err(EvalError::Parse {
            message: format!("expected syntax-rules, got {sr}"),
            span,
        });
    }
    let literals: Vec<String> = lit_list
        .iter()
        .map(|v| match v {
            Value::Symbol(s, _) => Ok(s.clone()),
            other => Err(EvalError::TypeError {
                expected: "symbol".to_string(),
                got: format!("{other}"),
                span,
            }),
        })
        .collect::<Result<_, _>>()?;
    let parsed_rules: Vec<(Value, Value)> = rules
        .iter()
        .map(|rule| {
            let Value::List(items, _) = rule else {
                return Err(EvalError::Parse {
                    message: "syntax rule must be a list".to_string(),
                    span,
                });
            };
            let [pattern, template] = items.as_slice() else {
                return Err(EvalError::Parse {
                    message: "syntax rule must have pattern and template".to_string(),
                    span,
                });
            };
            Ok((pattern.clone(), template.clone()))
        })
        .collect::<Result<_, _>>()?;
    let macro_val = Value::Macro {
        literals,
        rules: parsed_rules,
        def_env: env.clone(),
    };
    let cell = Rc::new(RefCell::new(macro_val));
    env.insert(name.clone(), Rc::clone(&cell));
    // Self-reference: update def_env inside the macro to include itself
    {
        let mut val = cell.borrow_mut();
        if let Value::Macro { def_env, .. } = &mut *val {
            def_env.insert(name.clone(), Rc::clone(&cell));
        }
    }
    Ok(Value::Boolean(false))
}

/// Try to expand a macro. Returns the expanded form, with hygiene
/// bindings added to `env`.
pub(crate) fn expand_macro(
    proc: &Value,
    args: &[Value],
    env: &mut Env,
    span: Span,
) -> Result<Value, EvalError> {
    let Value::Macro {
        literals,
        rules,
        def_env,
    } = proc
    else {
        return Err(EvalError::Parse {
            message: "not a macro".to_string(),
            span,
        });
    };
    for (pattern, template) in rules {
        let Value::List(pat_items, _) = pattern else {
            continue;
        };
        let pat_args = &pat_items[1..]; // skip macro name in pattern
        if let Some(bindings) = match_pattern(pat_args, args, literals) {
            return expand_with_hygiene(template, &bindings, def_env, env, span);
        }
    }
    Err(EvalError::Parse {
        message: "no matching syntax rule".to_string(),
        span,
    })
}

// --- Pattern matching ---

#[derive(Debug, Clone)]
enum MatchBinding {
    Single(Value),
    Repeated(Vec<Value>),
}

fn match_pattern(
    pattern: &[Value],
    args: &[Value],
    literals: &[String],
) -> Option<HashMap<String, MatchBinding>> {
    let mut bindings = HashMap::new();
    if do_match(pattern, args, literals, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

fn do_match(
    pattern: &[Value],
    args: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, MatchBinding>,
) -> bool {
    if pattern.is_empty() {
        return args.is_empty();
    }
    // Check for trailing ellipsis: (fixed... var ...)
    if pattern.len() >= 2
        && matches!(&pattern[pattern.len() - 1], Value::Symbol(s, _) if s == "...")
    {
        return match_ellipsis(pattern, args, literals, bindings);
    }
    // No ellipsis — element-by-element
    if pattern.len() != args.len() {
        return false;
    }
    pattern
        .iter()
        .zip(args.iter())
        .all(|(p, a)| match_single(p, a, literals, bindings))
}

fn match_ellipsis(
    pattern: &[Value],
    args: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, MatchBinding>,
) -> bool {
    let fixed_pat = &pattern[..pattern.len() - 2];
    let variadic_pat = &pattern[pattern.len() - 2];
    if args.len() < fixed_pat.len() {
        return false;
    }
    let all_fixed_match = fixed_pat
        .iter()
        .zip(args.iter())
        .all(|(p, a)| match_single(p, a, literals, bindings));
    if !all_fixed_match {
        return false;
    }
    let rest = &args[fixed_pat.len()..];
    if let Value::Symbol(name, _) = variadic_pat {
        if !literals.contains(name) {
            bindings.insert(name.clone(), MatchBinding::Repeated(rest.to_vec()));
            return true;
        }
    }
    false
}

fn match_single(
    pattern: &Value,
    arg: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MatchBinding>,
) -> bool {
    match pattern {
        Value::Symbol(name, _) if name == "_" => true,
        Value::Symbol(name, _) if literals.contains(name) => {
            matches!(arg, Value::Symbol(s, _) if s == name)
        }
        Value::Symbol(name, _) => {
            bindings.insert(name.clone(), MatchBinding::Single(arg.clone()));
            true
        }
        _ => false,
    }
}

// --- Template expansion with hygiene ---

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "if"
            | "quote"
            | "and"
            | "or"
            | "lambda"
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
    ) || is_builtin(name)
}

/// Expand template and set up hygiene bindings in `use_env`.
fn expand_with_hygiene(
    template: &Value,
    bindings: &HashMap<String, MatchBinding>,
    def_env: &Env,
    use_env: &mut Env,
    span: Span,
) -> Result<Value, EvalError> {
    let pattern_vars: std::collections::HashSet<&String> = bindings.keys().collect();
    let mut rename_map: HashMap<String, String> = HashMap::new();
    collect_free_symbols(template, &pattern_vars, &mut rename_map);

    let expanded = expand_inner(template, bindings, &rename_map, span)?;

    // Bind gensymed names in use_env from definition-site env
    for (original, gensymed) in &rename_map {
        if let Some(cell) = def_env.get(original) {
            use_env.insert(gensymed.clone(), Rc::clone(cell));
        }
    }

    Ok(expanded)
}

/// Collect non-keyword, non-pattern-variable symbols that need renaming.
fn collect_free_symbols(
    template: &Value,
    pattern_vars: &std::collections::HashSet<&String>,
    rename_map: &mut HashMap<String, String>,
) {
    match template {
        Value::Symbol(name, _)
            if !pattern_vars.contains(name)
                && !is_keyword(name)
                && !rename_map.contains_key(name)
                && name != "..." =>
        {
            rename_map.insert(name.clone(), gensym(name));
        }
        Value::List(items, _) => {
            for item in items {
                collect_free_symbols(item, pattern_vars, rename_map);
            }
        }
        _ => {}
    }
}

/// Recursively substitute pattern variables and apply renames.
fn expand_inner(
    template: &Value,
    bindings: &HashMap<String, MatchBinding>,
    rename_map: &HashMap<String, String>,
    span: Span,
) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(name, s) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    MatchBinding::Single(val) => Ok(val.clone()),
                    MatchBinding::Repeated(vals) if vals.len() == 1 => Ok(vals[0].clone()),
                    _ => Ok(template.clone()),
                }
            } else if let Some(renamed) = rename_map.get(name) {
                Ok(Value::Symbol(renamed.clone(), *s))
            } else {
                Ok(template.clone())
            }
        }
        Value::List(items, s) => expand_list(items, *s, bindings, rename_map, span),
        _ => Ok(template.clone()),
    }
}

fn expand_list(
    items: &[Value],
    list_span: Span,
    bindings: &HashMap<String, MatchBinding>,
    rename_map: &HashMap<String, String>,
    span: Span,
) -> Result<Value, EvalError> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < items.len() {
        if i + 1 < items.len()
            && matches!(&items[i + 1], Value::Symbol(s, _) if s == "...")
        {
            expand_ellipsis(&items[i], bindings, rename_map, span, &mut result)?;
            i += 2;
        } else {
            result.push(expand_inner(&items[i], bindings, rename_map, span)?);
            i += 1;
        }
    }
    Ok(Value::List(result, list_span))
}

/// Expand `element ...` by repeating for each value of the repeated variable.
fn expand_ellipsis(
    element: &Value,
    bindings: &HashMap<String, MatchBinding>,
    rename_map: &HashMap<String, String>,
    span: Span,
    out: &mut Vec<Value>,
) -> Result<(), EvalError> {
    let repeated_vars = find_repeated_vars(element, bindings);
    let count = repeated_vars
        .first()
        .and_then(|name| match bindings.get(*name) {
            Some(MatchBinding::Repeated(vals)) => Some(vals.len()),
            _ => None,
        })
        .unwrap_or(0);

    for j in 0..count {
        let sub = sub_bindings_at(bindings, &repeated_vars, j);
        out.push(expand_inner(element, &sub, rename_map, span)?);
    }
    Ok(())
}

fn find_repeated_vars<'a>(
    template: &Value,
    bindings: &'a HashMap<String, MatchBinding>,
) -> Vec<&'a String> {
    let mut vars = Vec::new();
    collect_repeated(template, bindings, &mut vars);
    vars
}

fn collect_repeated<'a>(
    template: &Value,
    bindings: &'a HashMap<String, MatchBinding>,
    vars: &mut Vec<&'a String>,
) {
    match template {
        Value::Symbol(name, _) if is_new_repeated(name, bindings, vars) => {
            if let Some(key) = bindings.keys().find(|k| *k == name) {
                vars.push(key);
            }
        }
        Value::List(items, _) => {
            for item in items {
                collect_repeated(item, bindings, vars);
            }
        }
        _ => {}
    }
}

fn is_new_repeated(
    name: &String,
    bindings: &HashMap<String, MatchBinding>,
    vars: &[&String],
) -> bool {
    matches!(bindings.get(name), Some(MatchBinding::Repeated(_))) && !vars.contains(&name)
}

/// Create bindings for one iteration index, replacing Repeated with Single.
fn sub_bindings_at(
    bindings: &HashMap<String, MatchBinding>,
    repeated_vars: &[&String],
    index: usize,
) -> HashMap<String, MatchBinding> {
    let mut sub = bindings.clone();
    for var in repeated_vars {
        let Some(MatchBinding::Repeated(vals)) = bindings.get(*var) else {
            continue;
        };
        if let Some(val) = vals.get(index) {
            sub.insert((*var).clone(), MatchBinding::Single(val.clone()));
        }
    }
    sub
}
