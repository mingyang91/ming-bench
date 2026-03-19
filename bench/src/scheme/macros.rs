use super::types::{Env, Value};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{base}__hyg{id}")
}

enum Binding {
    Single(Value),
    Spliced(Vec<Value>),
}

pub struct Expansion {
    pub form: Value,
    pub def_bindings: HashMap<String, Value>,
}

pub fn try_expand(head: &str, elems: &[Value], env: &Env) -> Option<Result<Expansion, String>> {
    let mac = env.get(head)?;
    let Value::Macro {
        literals,
        rules,
        def_env,
        ..
    } = &mac
    else {
        return None;
    };
    Some(expand(literals, rules, &elems[1..], def_env))
}

fn expand(
    literals: &[String],
    rules: &[(Value, Value)],
    input: &[Value],
    def_env: &Env,
) -> Result<Expansion, String> {
    for (pattern, template) in rules {
        let Value::List(elems) = pattern else {
            continue;
        };
        if elems.is_empty() {
            continue;
        }
        let mut bindings = HashMap::new();
        if match_pattern(&elems[1..], input, literals, &mut bindings) {
            return instantiate(template, &bindings, def_env);
        }
    }
    Err("no matching syntax-rules pattern".into())
}

fn match_pattern(
    patterns: &[Value],
    inputs: &[Value],
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let mut state = MatchState {
        pi: 0,
        ii: 0,
        patterns,
        inputs,
        literals,
    };
    while state.pi < patterns.len() {
        if !state.step(bindings) {
            return false;
        }
    }
    state.ii == inputs.len()
}

struct MatchState<'a> {
    pi: usize,
    ii: usize,
    patterns: &'a [Value],
    inputs: &'a [Value],
    literals: &'a [String],
}

impl MatchState<'_> {
    fn step(&mut self, bindings: &mut HashMap<String, Binding>) -> bool {
        if self.pi + 1 < self.patterns.len() && is_ellipsis(&self.patterns[self.pi + 1]) {
            let ok = match_ellipsis(
                &self.patterns[self.pi],
                self.patterns.len() - self.pi - 2,
                self.inputs,
                &mut self.ii,
                bindings,
            );
            self.pi += 2;
            return ok;
        }
        if !match_single(&self.patterns[self.pi], self.inputs.get(self.ii), self.literals, bindings) {
            return false;
        }
        self.pi += 1;
        self.ii += 1;
        true
    }
}

fn match_single(
    pat: &Value,
    input: Option<&Value>,
    literals: &[String],
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let Some(input) = input else { return false };
    match pat {
        Value::Symbol(s) if !literals.contains(s) => {
            bindings.insert(s.clone(), Binding::Single(input.clone()));
            true
        }
        _ => val_eq(pat, input),
    }
}

fn match_ellipsis(
    pat: &Value,
    remaining_pats: usize,
    inputs: &[Value],
    ii: &mut usize,
    bindings: &mut HashMap<String, Binding>,
) -> bool {
    let Value::Symbol(name) = pat else {
        return false;
    };
    let available = inputs.len().saturating_sub(*ii + remaining_pats);
    bindings.insert(
        name.clone(),
        Binding::Spliced(inputs[*ii..*ii + available].to_vec()),
    );
    *ii += available;
    true
}

fn is_ellipsis(v: &Value) -> bool {
    matches!(v, Value::Symbol(s) if s == "...")
}

fn val_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Boolean(a), Value::Boolean(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        _ => false,
    }
}

const KEYWORDS: &[&str] = &[
    "define",
    "if",
    "quote",
    "and",
    "or",
    "lambda",
    "begin",
    "let",
    "cond",
    "set!",
    "define-syntax",
    "syntax-rules",
    "cons",
    "car",
    "cdr",
    "null?",
    "list",
    "length",
    "boolean?",
    "number?",
    "pair?",
    "string?",
    "symbol?",
    "not",
    "apply",
    "call/cc",
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "=",
    "<=",
    ">=",
    "else",
];

fn instantiate(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    def_env: &Env,
) -> Result<Expansion, String> {
    let mut free_vars = Vec::new();
    collect_free(template, bindings, &mut free_vars);

    let mut renames: HashMap<String, String> = HashMap::new();
    for var in &free_vars {
        renames.entry(var.clone()).or_insert_with(|| gensym(var));
    }

    let form = subst(template, bindings, &renames);

    let mut def_bindings = HashMap::new();
    for (orig, renamed) in &renames {
        if let Some(val) = def_env.get(orig) {
            def_bindings.insert(renamed.clone(), val);
        }
    }

    Ok(Expansion {
        form,
        def_bindings,
    })
}

fn collect_free(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    free: &mut Vec<String>,
) {
    match template {
        Value::Symbol(s) if s == "..." => {}
        Value::Symbol(s) => {
            let dominated = bindings.contains_key(s)
                || KEYWORDS.contains(&s.as_str())
                || free.contains(s);
            if !dominated {
                free.push(s.clone());
            }
        }
        Value::List(elems) => {
            for e in elems {
                collect_free(e, bindings, free);
            }
        }
        _ => {}
    }
}

fn subst(
    template: &Value,
    bindings: &HashMap<String, Binding>,
    renames: &HashMap<String, String>,
) -> Value {
    match template {
        Value::Symbol(s) => subst_symbol(s, template, bindings, renames),
        Value::List(elems) => subst_list(elems, bindings, renames),
        _ => template.clone(),
    }
}

fn subst_symbol(
    s: &str,
    original: &Value,
    bindings: &HashMap<String, Binding>,
    renames: &HashMap<String, String>,
) -> Value {
    if let Some(b) = bindings.get(s) {
        return match b {
            Binding::Single(v) => v.clone(),
            Binding::Spliced(vs) => Value::List(vs.clone()),
        };
    }
    if let Some(r) = renames.get(s) {
        return Value::Symbol(r.clone());
    }
    original.clone()
}

fn subst_list(
    elems: &[Value],
    bindings: &HashMap<String, Binding>,
    renames: &HashMap<String, String>,
) -> Value {
    let mut result = Vec::new();
    let mut i = 0;
    while i < elems.len() {
        if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
            splice_ellipsis(&elems[i], bindings, &mut result);
            i += 2;
        } else {
            result.push(subst(&elems[i], bindings, renames));
            i += 1;
        }
    }
    Value::List(result)
}

fn splice_ellipsis(elem: &Value, bindings: &HashMap<String, Binding>, out: &mut Vec<Value>) {
    let Value::Symbol(s) = elem else { return };
    let Some(Binding::Spliced(vals)) = bindings.get(s) else {
        return;
    };
    out.extend(vals.iter().cloned());
}
