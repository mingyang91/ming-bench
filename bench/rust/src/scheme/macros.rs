use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

const SPECIAL_FORMS: &[&str] = &[
    "define",
    "define-syntax",
    "quote",
    "lambda",
    "set!",
    "if",
    "begin",
    "cond",
    "and",
    "or",
    "let",
    "call/cc",
    "call-with-current-continuation",
    "string-set!",
];

pub enum Binding {
    Single(Value),
    Repeated(Vec<Value>),
}

pub type Bindings = HashMap<String, Binding>;

/// Expand a macro invocation. `input` is the full form including the macro name.
pub fn expand_macro(
    literals: &[String],
    rules: &[(Vec<Value>, Value)],
    def_env: &Rc<RefCell<Env>>,
    input: &[Value],
    counter: &mut u64,
) -> Result<Value, EvalError> {
    let args = &input[1..];
    for (pattern_elems, template) in rules {
        let pat = &pattern_elems[1..];
        if let Some(bindings) = try_match(pat, args, literals) {
            return instantiate(template, &bindings, def_env, counter);
        }
    }
    Err(EvalError::Parse {
        message: "no matching syntax-rules pattern".into(),
    })
}

fn try_match(pattern: &[Value], input: &[Value], literals: &[String]) -> Option<Bindings> {
    let mut bindings = HashMap::new();
    match_elements(pattern, input, literals, &mut bindings)?;
    Some(bindings)
}

fn match_elements(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
    bindings: &mut Bindings,
) -> Option<()> {
    let mut pi = 0;
    let mut ii = 0;

    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1], Value::Symbol(s) if s == "...");

        if has_ellipsis {
            let consumed = match_ellipsis_pattern(
                &pattern[pi], &input[ii..], pattern.len() - pi - 2, literals, bindings,
            )?;
            ii += consumed;
            pi += 2;
            continue;
        }

        if ii >= input.len() {
            return None;
        }
        match_single(&pattern[pi], &input[ii], literals, bindings)?;
        pi += 1;
        ii += 1;
    }

    if ii != input.len() {
        return None;
    }
    Some(())
}

fn match_ellipsis_pattern(
    pat_elem: &Value,
    input_rest: &[Value],
    remaining_pats: usize,
    literals: &[String],
    bindings: &mut Bindings,
) -> Option<usize> {
    let to_match = input_rest.len().saturating_sub(remaining_pats);
    let collected = collect_ellipsis_matches(pat_elem, &input_rest[..to_match], literals)?;
    if let Value::Symbol(name) = pat_elem {
        if !literals.contains(name) {
            bindings.insert(name.clone(), Binding::Repeated(collected));
        }
    }
    Some(to_match)
}

fn collect_ellipsis_matches(
    pat_elem: &Value,
    inputs: &[Value],
    literals: &[String],
) -> Option<Vec<Value>> {
    let mut collected = Vec::new();
    for item in inputs {
        match pat_elem {
            Value::Symbol(name) if !literals.contains(name) => {
                collected.push(item.clone());
            }
            _ => {
                let mut sub = HashMap::new();
                match_single(pat_elem, item, literals, &mut sub)?;
                collected.push(item.clone());
            }
        }
    }
    Some(collected)
}

pub fn match_single(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut Bindings,
) -> Option<()> {
    match pattern {
        Value::Symbol(name) if name == "_" => Some(()),
        Value::Symbol(name) if literals.contains(name) => match input {
            Value::Symbol(n) if n == name => Some(()),
            _ => None,
        },
        Value::Symbol(name) => {
            bindings.insert(name.clone(), Binding::Single(input.clone()));
            Some(())
        }
        Value::List(pel) => {
            let Value::List(iel) = input else { return None };
            match_elements(pel, iel, literals, bindings)
        }
        _ => {
            if pattern == input {
                Some(())
            } else {
                None
            }
        }
    }
}

pub fn instantiate(
    template: &Value,
    bindings: &Bindings,
    def_env: &Rc<RefCell<Env>>,
    counter: &mut u64,
) -> Result<Value, EvalError> {
    let pattern_vars: HashSet<&String> = bindings.keys().collect();
    let mut gensym_map: HashMap<String, String> = HashMap::new();
    collect_introduced(template, &pattern_vars, &mut gensym_map, counter);

    let expanded = substitute(template, bindings, &gensym_map)?;

    let hygiene_bindings: Vec<(String, Value)> = gensym_map
        .iter()
        .filter_map(|(orig, gs)| def_env.borrow().get(orig).map(|val| (gs.clone(), val)))
        .collect();

    if hygiene_bindings.is_empty() {
        Ok(expanded)
    } else {
        let binding_forms: Vec<Value> = hygiene_bindings
            .into_iter()
            .map(|(name, val)| Value::List(vec![Value::Symbol(name), val]))
            .collect();
        Ok(Value::List(vec![
            Value::Symbol("let".into()),
            Value::List(binding_forms),
            expanded,
        ]))
    }
}

pub fn collect_introduced(
    template: &Value,
    pattern_vars: &HashSet<&String>,
    gensym_map: &mut HashMap<String, String>,
    counter: &mut u64,
) {
    match template {
        Value::Symbol(name)
            if name != "..."
                && !pattern_vars.contains(name)
                && !SPECIAL_FORMS.contains(&name.as_str())
                && !gensym_map.contains_key(name) =>
        {
            let gensym = format!("{name}__h{counter}");
            *counter += 1;
            gensym_map.insert(name.clone(), gensym);
        }
        Value::List(elems) => {
            // Skip quoted forms — symbols inside (quote ...) are data, not identifiers
            if matches!(elems.first(), Some(Value::Symbol(s)) if s == "quote") {
                return;
            }
            for elem in elems {
                collect_introduced(elem, pattern_vars, gensym_map, counter);
            }
        }
        _ => {}
    }
}

pub fn substitute(
    template: &Value,
    bindings: &Bindings,
    gensym_map: &HashMap<String, String>,
) -> Result<Value, EvalError> {
    match template {
        Value::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    Binding::Single(val) => Ok(val.clone()),
                    Binding::Repeated(_) => Err(EvalError::Parse {
                        message: format!("ellipsis variable {name} used without ellipsis"),
                    }),
                }
            } else if let Some(gensym) = gensym_map.get(name) {
                Ok(Value::Symbol(gensym.clone()))
            } else {
                Ok(template.clone())
            }
        }
        Value::List(elems) => substitute_list(elems, bindings, gensym_map),
        _ => Ok(template.clone()),
    }
}

fn substitute_list(
    elems: &[Value],
    bindings: &Bindings,
    gensym_map: &HashMap<String, String>,
) -> Result<Value, EvalError> {
    // Don't substitute inside quoted forms
    if matches!(elems.first(), Some(Value::Symbol(s)) if s == "quote") {
        return Ok(Value::List(elems.to_vec()));
    }
    let mut result = Vec::new();
    let mut i = 0;

    while i < elems.len() {
        let has_ellipsis = i + 1 < elems.len()
            && matches!(&elems[i + 1], Value::Symbol(s) if s == "...");

        if has_ellipsis {
            expand_ellipsis_template(&elems[i], bindings, gensym_map, &mut result)?;
            i += 2;
        } else {
            result.push(substitute(&elems[i], bindings, gensym_map)?);
            i += 1;
        }
    }

    Ok(Value::List(result))
}

fn expand_ellipsis_template(
    elem: &Value,
    bindings: &Bindings,
    gensym_map: &HashMap<String, String>,
    result: &mut Vec<Value>,
) -> Result<(), EvalError> {
    let Some(var_name) = find_repeated_var(elem, bindings) else {
        return Ok(());
    };
    let Some(Binding::Repeated(vals)) = bindings.get(&var_name) else {
        return Ok(());
    };
    for val in vals {
        let mut temp = clone_bindings(bindings);
        temp.insert(var_name.clone(), Binding::Single(val.clone()));
        result.push(substitute(elem, &temp, gensym_map)?);
    }
    Ok(())
}

fn find_repeated_var(template: &Value, bindings: &Bindings) -> Option<String> {
    match template {
        Value::Symbol(name) => {
            if matches!(bindings.get(name), Some(Binding::Repeated(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        Value::List(elems) => elems.iter().find_map(|e| find_repeated_var(e, bindings)),
        _ => None,
    }
}

fn clone_bindings(bindings: &Bindings) -> Bindings {
    bindings
        .iter()
        .map(|(k, v)| {
            let cloned = match v {
                Binding::Single(val) => Binding::Single(val.clone()),
                Binding::Repeated(vals) => Binding::Repeated(vals.clone()),
            };
            (k.clone(), cloned)
        })
        .collect()
}

/// Match a syntax-case pattern against an input value.
/// Returns bindings if the match succeeds.
pub fn syntax_case_match(
    pattern: &Value,
    input: &Value,
    literals: &[String],
) -> Option<Bindings> {
    let mut bindings = HashMap::new();
    match_single(pattern, input, literals, &mut bindings)?;
    Some(bindings)
}
