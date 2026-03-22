use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::value::Value;

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone)]
enum MacroBinding {
    Single(Value),
    Spliced(Vec<Value>),
}

/// Expand a macro call. `input` is the items of the call form (e.g., `[my-and, 1, 2, 3]`).
pub fn expand_macro(
    input: &[Value],
    literals: &[String],
    rules: &[(Value, Value)],
    def_env: &Rc<RefCell<Env>>,
) -> Result<Value, EvalError> {
    for (pattern, template) in rules {
        let Value::List(pat_items) = pattern else {
            continue;
        };
        let mut bindings = HashMap::new();
        // Skip the first element (macro name) in both pattern and input
        if match_pattern_list(&pat_items[1..], &input[1..], literals, &mut bindings) {
            let mut gensym_map = HashMap::new();
            return Ok(expand_template(template, &bindings, &mut gensym_map, def_env));
        }
    }
    Err(EvalError::parse("no matching pattern for macro application"))
}

fn match_pattern(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match pattern {
        Value::Symbol(name) if name == "_" => true,
        Value::Symbol(name) if name == "..." => false,
        Value::Symbol(name) if literals.contains(name) => {
            matches!(input, Value::Symbol(s) if s == name)
        }
        Value::Symbol(name) => {
            bindings.insert(name.clone(), MacroBinding::Single(input.clone()));
            true
        }
        Value::List(pat_items) => {
            let Value::List(inp_items) = input else {
                return false;
            };
            match_pattern_list(pat_items, inp_items, literals, bindings)
        }
        Value::Boolean(a) => matches!(input, Value::Boolean(b) if a == b),
        Value::Integer(a) => matches!(input, Value::Integer(b) if a == b),
        Value::Str(a) => matches!(input, Value::Str(b) if a == b),
        _ => false,
    }
}

fn match_pattern_list(
    pat: &[Value],
    inp: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let ellipsis_pos = pat.iter().position(|v| matches!(v, Value::Symbol(s) if s == "..."));

    match ellipsis_pos {
        None => {
            if pat.len() != inp.len() {
                return false;
            }
            for (p, i) in pat.iter().zip(inp.iter()) {
                if !match_pattern(p, i, literals, bindings) {
                    return false;
                }
            }
            true
        }
        Some(0) => false,
        Some(epos) => {
            let before = &pat[..epos - 1];
            let ellipsis_pat = &pat[epos - 1];
            let after = &pat[epos + 1..];

            let min_required = before.len() + after.len();
            if inp.len() < min_required {
                return false;
            }

            // Match fixed prefix
            for (p, i) in before.iter().zip(inp.iter()) {
                if !match_pattern(p, i, literals, bindings) {
                    return false;
                }
            }

            // Match fixed suffix
            let after_start = inp.len() - after.len();
            for (p, i) in after.iter().zip(inp[after_start..].iter()) {
                if !match_pattern(p, i, literals, bindings) {
                    return false;
                }
            }

            // Match ellipsis elements
            let middle = &inp[before.len()..after_start];
            match ellipsis_pat {
                Value::Symbol(var_name) if !literals.contains(var_name) => {
                    bindings.insert(var_name.clone(), MacroBinding::Spliced(middle.to_vec()));
                    true
                }
                _ => {
                    // Complex subpattern — match each element individually
                    for elem in middle {
                        if !match_pattern(ellipsis_pat, elem, literals, bindings) {
                            return false;
                        }
                    }
                    true
                }
            }
        }
    }
}

fn is_special_form_or_builtin(name: &str) -> bool {
    matches!(name,
        "if" | "let" | "begin" | "set!" | "define" | "lambda" | "quote"
        | "cond" | "and" | "or" | "define-syntax" | "syntax-rules" | "string-set!"
        | "call/cc" | "call-with-current-continuation"
        | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
        | "not" | "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "string?" | "number?" | "boolean?" | "pair?" | "symbol?"
        | "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string"
        | "symbol->string" | "string->symbol"
        | "string-ref" | "char?" | "string-copy"
        | "apply" | "else"
    )
}

fn expand_template(
    template: &Value,
    bindings: &HashMap<String, MacroBinding>,
    gensym_map: &mut HashMap<String, String>,
    def_env: &Rc<RefCell<Env>>,
) -> Value {
    match template {
        Value::Symbol(name) => {
            if let Some(binding) = bindings.get(name) {
                match binding {
                    MacroBinding::Single(val) => val.clone(),
                    MacroBinding::Spliced(_) => Value::Symbol(name.clone()),
                }
            } else if is_special_form_or_builtin(name) {
                Value::Symbol(name.clone())
            } else if let Ok(val) = def_env.borrow().get(name) {
                // Definition-site binding: if it's a macro, keep as symbol
                // for recursive expansion; otherwise substitute value for hygiene
                if matches!(val, Value::SyntaxRules { .. }) {
                    Value::Symbol(name.clone())
                } else {
                    val
                }
            } else {
                // Macro-introduced identifier — rename for hygiene
                let gensym = gensym_map.entry(name.clone()).or_insert_with(|| {
                    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
                    format!("#{name}#{id}")
                });
                Value::Symbol(gensym.clone())
            }
        }
        Value::List(items) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len()
                    && matches!(&items[i + 1], Value::Symbol(s) if s == "...")
                {
                    expand_ellipsis(&items[i], bindings, gensym_map, def_env, &mut result);
                    i += 2;
                } else {
                    result.push(expand_template(&items[i], bindings, gensym_map, def_env));
                    i += 1;
                }
            }
            Value::List(result)
        }
        _ => template.clone(),
    }
}

fn expand_ellipsis(
    template: &Value,
    bindings: &HashMap<String, MacroBinding>,
    gensym_map: &mut HashMap<String, String>,
    def_env: &Rc<RefCell<Env>>,
    result: &mut Vec<Value>,
) {
    if let Some(var_name) = find_spliced_var(template, bindings) {
        if let Some(MacroBinding::Spliced(vals)) = bindings.get(&var_name) {
            for val in vals {
                let mut local_bindings = bindings.clone();
                local_bindings.insert(var_name.clone(), MacroBinding::Single(val.clone()));
                result.push(expand_template(template, &local_bindings, gensym_map, def_env));
            }
        }
    }
}

fn find_spliced_var(template: &Value, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match template {
        Value::Symbol(name) => {
            if matches!(bindings.get(name), Some(MacroBinding::Spliced(_))) {
                Some(name.clone())
            } else {
                None
            }
        }
        Value::List(items) => {
            for item in items {
                if let Some(var) = find_spliced_var(item, bindings) {
                    return Some(var);
                }
            }
            None
        }
        _ => None,
    }
}
