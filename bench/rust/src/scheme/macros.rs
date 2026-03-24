use std::collections::HashMap;
use std::rc::Rc;
use std::cell::Cell;

use crate::scheme::value::{Value, ValueKind};
use crate::scheme::env::Env;
use crate::scheme::EvalError;

thread_local! {
    static GENSYM_COUNTER: Cell<u64> = Cell::new(0);
}

fn gensym(base: &str) -> String {
    GENSYM_COUNTER.with(|c| {
        let n = c.get();
        c.set(n + 1);
        format!("{}__hyg_{}", base, n)
    })
}

#[derive(Clone)]
enum MatchResult {
    Single(Value),
    Many(Vec<Value>),
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "let", "begin",
    "cond", "set!", "string-set!", "define-syntax", "syntax-rules",
    "let*", "letrec", "when", "unless", "case", "do", "else", "=>",
    "quasiquote", "unquote", "unquote-splicing",
];

pub fn expand_syntax_rules(
    rules: &[(Value, Value)],
    literals: &[String],
    input: &[Value],
    def_env: &Rc<Env>,
    use_env: &Rc<Env>,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ValueKind::List(e) => e,
            _ => continue,
        };
        if let Some(bindings) = match_pattern(&pat_elems[1..], &input[1..], literals) {
            return instantiate_template(template, &bindings, def_env, use_env);
        }
    }
    Err(EvalError::Syntax("no matching syntax-rules pattern".into()))
}

fn match_pattern(
    pattern: &[Value],
    input: &[Value],
    literals: &[String],
) -> Option<HashMap<String, MatchResult>> {
    let mut bindings = HashMap::new();

    // Find ellipsis position in pattern (top level only)
    let ellipsis_pos = pattern.iter().position(|p| matches!(&p.kind, ValueKind::Symbol(s) if s == "..."));

    if let Some(ep) = ellipsis_pos {
        if ep == 0 { return None; }
        let before = &pattern[..ep - 1];
        let ellipsis_var = &pattern[ep - 1];
        let after = &pattern[ep + 1..];

        if input.len() < before.len() + after.len() { return None; }

        // Match fixed elements before ellipsis
        for (p, i) in before.iter().zip(input.iter()) {
            match_single(p, i, literals, &mut bindings)?;
        }

        // Match fixed elements after ellipsis (from end)
        let after_start = input.len() - after.len();
        for (p, i) in after.iter().zip(input[after_start..].iter()) {
            match_single(p, i, literals, &mut bindings)?;
        }

        // Ellipsis part
        let ellipsis_input = &input[before.len()..after_start];
        match &ellipsis_var.kind {
            ValueKind::Symbol(name) if !literals.contains(name) && name != "_" => {
                bindings.insert(name.clone(), MatchResult::Many(ellipsis_input.to_vec()));
            }
            _ => {
                for item in ellipsis_input {
                    match_single(ellipsis_var, item, literals, &mut HashMap::new())?;
                }
            }
        }
        Some(bindings)
    } else {
        if pattern.len() != input.len() { return None; }
        for (p, i) in pattern.iter().zip(input.iter()) {
            match_single(p, i, literals, &mut bindings)?;
        }
        Some(bindings)
    }
}

fn match_single(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MatchResult>,
) -> Option<()> {
    match &pattern.kind {
        ValueKind::Symbol(name) if name == "_" => Some(()),
        ValueKind::Symbol(name) if literals.contains(name) => {
            match &input.kind {
                ValueKind::Symbol(s) if s == name => Some(()),
                _ => None,
            }
        }
        ValueKind::Symbol(name) => {
            bindings.insert(name.clone(), MatchResult::Single(input.clone()));
            Some(())
        }
        ValueKind::List(pat_elems) => {
            match &input.kind {
                ValueKind::List(inp_elems) => {
                    let sub = match_pattern(pat_elems, inp_elems, literals)?;
                    bindings.extend(sub);
                    Some(())
                }
                _ => None,
            }
        }
        _ => {
            if pattern == input { Some(()) } else { None }
        }
    }
}

fn instantiate_template(
    template: &Value,
    bindings: &HashMap<String, MatchResult>,
    def_env: &Rc<Env>,
    use_env: &Rc<Env>,
) -> Result<Value, EvalError> {
    let mut renames: HashMap<String, String> = HashMap::new();
    collect_free_symbols(template, bindings, &mut renames);

    // Inject definition-site bindings for renamed symbols
    for (original, gensym_name) in &renames {
        if let Some(val) = def_env.get(original) {
            use_env.set(gensym_name.clone(), val);
        }
    }

    Ok(instantiate(template, bindings, &renames))
}

fn collect_free_symbols(
    template: &Value,
    bindings: &HashMap<String, MatchResult>,
    renames: &mut HashMap<String, String>,
) {
    match &template.kind {
        ValueKind::Symbol(name) => {
            if !bindings.contains_key(name)
                && name != "..."
                && !SPECIAL_FORMS.contains(&name.as_str())
                && !renames.contains_key(name)
            {
                renames.insert(name.clone(), gensym(name));
            }
        }
        ValueKind::List(elems) => {
            for elem in elems {
                collect_free_symbols(elem, bindings, renames);
            }
        }
        _ => {}
    }
}

fn instantiate(
    template: &Value,
    bindings: &HashMap<String, MatchResult>,
    renames: &HashMap<String, String>,
) -> Value {
    match &template.kind {
        ValueKind::Symbol(name) => {
            if let Some(result) = bindings.get(name) {
                match result {
                    MatchResult::Single(v) => v.clone(),
                    MatchResult::Many(_) => template.clone(),
                }
            } else if let Some(gensym_name) = renames.get(name) {
                Value::unpos(ValueKind::Symbol(gensym_name.clone()))
            } else {
                template.clone()
            }
        }
        ValueKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && matches!(&elems[i + 1].kind, ValueKind::Symbol(s) if s == "...") {
                    // Element followed by "..." - expand
                    expand_ellipsis(&elems[i], bindings, renames, &mut result);
                    i += 2;
                } else {
                    result.push(instantiate(&elems[i], bindings, renames));
                    i += 1;
                }
            }
            Value::unpos(ValueKind::List(result))
        }
        _ => template.clone(),
    }
}

fn expand_ellipsis(
    elem: &Value,
    bindings: &HashMap<String, MatchResult>,
    renames: &HashMap<String, String>,
    result: &mut Vec<Value>,
) {
    match &elem.kind {
        ValueKind::Symbol(name) => {
            if let Some(MatchResult::Many(values)) = bindings.get(name) {
                result.extend(values.iter().cloned());
            }
        }
        ValueKind::List(_) => {
            // Find a Many-bound variable in this sub-template to determine count
            if let Some(var_name) = find_many_var(elem, bindings) {
                if let Some(MatchResult::Many(values)) = bindings.get(&var_name) {
                    let count = values.len();
                    for idx in 0..count {
                        let indexed = index_bindings(bindings, idx);
                        result.push(instantiate(elem, &indexed, renames));
                    }
                }
            }
        }
        _ => {}
    }
}

fn find_many_var(template: &Value, bindings: &HashMap<String, MatchResult>) -> Option<String> {
    match &template.kind {
        ValueKind::Symbol(name) => {
            if matches!(bindings.get(name), Some(MatchResult::Many(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        ValueKind::List(elems) => {
            for elem in elems {
                if let Some(v) = find_many_var(elem, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn index_bindings(bindings: &HashMap<String, MatchResult>, idx: usize) -> HashMap<String, MatchResult> {
    let mut result = HashMap::new();
    for (key, val) in bindings {
        match val {
            MatchResult::Single(v) => {
                result.insert(key.clone(), MatchResult::Single(v.clone()));
            }
            MatchResult::Many(values) => {
                if idx < values.len() {
                    result.insert(key.clone(), MatchResult::Single(values[idx].clone()));
                }
            }
        }
    }
    result
}
