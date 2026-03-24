use std::collections::{HashMap, HashSet};

use super::{gensym, Spanned, Value};

const SPECIAL_FORMS: &[&str] = &[
    "quote", "if", "define", "lambda", "let", "begin", "set!",
    "cond", "and", "or", "not", "string-set!", "define-syntax", "syntax-rules",
];

#[derive(Clone)]
enum PatternBinding {
    Single(Spanned),
    List(Vec<Spanned>),
}

type Bindings = HashMap<String, PatternBinding>;

fn match_pattern(
    pattern: &[Spanned],
    input: &[Spanned],
    literals: &[String],
    bindings: &mut Bindings,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;
    while pi < pattern.len() {
        let has_ellipsis = pi + 1 < pattern.len()
            && matches!(&pattern[pi + 1].val, Value::Symbol(s) if s == "...");
        if has_ellipsis {
            if let Value::Symbol(name) = &pattern[pi].val {
                if !literals.contains(name) {
                    let remaining_pat = pattern.len() - pi - 2;
                    let available = if input.len() >= ii { input.len() - ii } else { return false };
                    if available < remaining_pat { return false; }
                    let to_match = available - remaining_pat;
                    let matched: Vec<Spanned> = input[ii..ii + to_match].to_vec();
                    bindings.insert(name.clone(), PatternBinding::List(matched));
                    ii += to_match;
                    pi += 2;
                    continue;
                }
            }
            return false;
        }
        if ii >= input.len() { return false; }
        if !match_single(&pattern[pi], &input[ii], literals, bindings) {
            return false;
        }
        pi += 1;
        ii += 1;
    }
    ii == input.len()
}

fn match_single(
    pattern: &Spanned,
    input: &Spanned,
    literals: &[String],
    bindings: &mut Bindings,
) -> bool {
    match &pattern.val {
        Value::Symbol(name) => {
            if literals.contains(name) {
                matches!(&input.val, Value::Symbol(s) if s == name)
            } else if name == "_" {
                true
            } else {
                bindings.insert(name.clone(), PatternBinding::Single(input.clone()));
                true
            }
        }
        Value::List(pat_elems) => {
            if let Value::List(inp_elems) = &input.val {
                match_pattern(pat_elems, inp_elems, literals, bindings)
            } else {
                false
            }
        }
        Value::Integer(n) => matches!(&input.val, Value::Integer(m) if n == m),
        Value::Boolean(b) => matches!(&input.val, Value::Boolean(c) if b == c),
        _ => false,
    }
}

fn find_ellipsis_vars(template: &Spanned, bindings: &Bindings) -> Vec<String> {
    match &template.val {
        Value::Symbol(name) => {
            if matches!(bindings.get(name), Some(PatternBinding::List(_))) {
                vec![name.clone()]
            } else {
                vec![]
            }
        }
        Value::List(elems) => {
            let mut vars = Vec::new();
            for elem in elems {
                vars.extend(find_ellipsis_vars(elem, bindings));
            }
            vars
        }
        _ => vec![],
    }
}

fn expand_ellipsis_template(
    elem: &Spanned,
    bindings: &Bindings,
    renames: &HashMap<String, String>,
    result: &mut Vec<Spanned>,
) {
    let evars = find_ellipsis_vars(elem, bindings);
    let Some(var_name) = evars.first() else {
        return;
    };
    let Some(PatternBinding::List(vals)) = bindings.get(var_name) else {
        return;
    };
    for val in vals {
        let mut single_bindings = bindings.clone();
        single_bindings.insert(var_name.clone(), PatternBinding::Single(val.clone()));
        result.push(instantiate_template(elem, &single_bindings, renames));
    }
}

fn instantiate_template(
    template: &Spanned,
    bindings: &Bindings,
    renames: &HashMap<String, String>,
) -> Spanned {
    match &template.val {
        Value::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    PatternBinding::Single(val) => val.clone(),
                    PatternBinding::List(_) => template.clone(),
                }
            } else if let Some(new_name) = renames.get(name) {
                Spanned::new(Value::Symbol(new_name.clone()), template.span)
            } else {
                template.clone()
            }
        }
        Value::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                let has_ellipsis = i + 1 < elems.len()
                    && matches!(&elems[i + 1].val, Value::Symbol(s) if s == "...");
                if has_ellipsis {
                    expand_ellipsis_template(&elems[i], bindings, renames, &mut result);
                    i += 2;
                } else {
                    result.push(instantiate_template(&elems[i], bindings, renames));
                    i += 1;
                }
            }
            Spanned::new(Value::List(result), template.span)
        }
        _ => template.clone(),
    }
}

fn collect_free_vars(template: &Spanned, pattern_vars: &HashSet<String>) -> HashSet<String> {
    match &template.val {
        Value::Symbol(name) if !pattern_vars.contains(name) && name != "..." => {
            let mut set = HashSet::new();
            set.insert(name.clone());
            set
        }
        Value::List(elems) => {
            let mut set = HashSet::new();
            for elem in elems {
                set.extend(collect_free_vars(elem, pattern_vars));
            }
            set
        }
        _ => HashSet::new(),
    }
}

/// Try to match a macro invocation against rules, returning the expanded form and renames.
/// Returns `None` if no rule matched.
pub(super) fn try_expand(
    items: &[Spanned],
    literals: &[String],
    rules: &[(Spanned, Spanned)],
) -> Option<(Spanned, HashMap<String, String>)> {
    for (pattern, template) in rules {
        let Value::List(pat_elems) = &pattern.val else { continue };
        let mut bindings = Bindings::new();
        if match_pattern(&pat_elems[1..], &items[1..], literals, &mut bindings) {
            let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
            let free_vars = collect_free_vars(template, &pattern_vars);
            let mut renames = HashMap::new();
            for var in &free_vars {
                if !SPECIAL_FORMS.contains(&var.as_str()) {
                    renames.insert(var.clone(), gensym(var));
                }
            }
            let expanded = instantiate_template(template, &bindings, &renames);
            return Some((expanded, renames));
        }
    }
    None
}
