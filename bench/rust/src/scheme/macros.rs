use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

use super::{
    env_get, env_set, Env, EvalError, Expr, PatternBinding, Pos, Value, BUILTINS, SPECIAL_FORMS,
};
use super::forms::eval_lambda;

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn gensym(base: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{base}__hyg_{n}")
}

pub(super) fn eval_define_syntax(
    args: &[Expr],
    call_pos: Pos,
    env: &Env,
) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!(
            "{call_pos}: define-syntax requires 2 arguments"
        )));
    }
    let name = match &args[0] {
        Expr::Symbol(s, _) => s.clone(),
        _ => {
            return Err(EvalError::Parse(format!(
                "{call_pos}: define-syntax: expected symbol"
            )))
        }
    };
    match &args[1] {
        Expr::List(elems, _) if !elems.is_empty() => {
            if let Expr::Symbol(s, _) = &elems[0] {
                if s == "syntax-rules" {
                    let (literals, rules) = parse_syntax_rules(&elems[1..], call_pos)?;
                    env_set(
                        env,
                        name,
                        Value::Macro {
                            literals,
                            rules,
                            def_env: env.clone(),
                        },
                    );
                    return Ok(Value::Void);
                } else if s == "lambda" {
                    // syntax-case transformer: (lambda (stx) body ...)
                    let transformer = eval_lambda(&elems[1..], call_pos, env)?;
                    env_set(
                        env,
                        name,
                        Value::MacroTransformer(Box::new(transformer)),
                    );
                    return Ok(Value::Void);
                }
            }
        }
        _ => {}
    }
    Err(EvalError::Parse(format!(
        "{call_pos}: define-syntax: expected syntax-rules or lambda"
    )))
}

type SyntaxRulesResult = Result<(Vec<String>, Vec<(Expr, Expr)>), EvalError>;

fn parse_syntax_rules(args: &[Expr], call_pos: Pos) -> SyntaxRulesResult {
    if args.is_empty() {
        return Err(EvalError::Parse(format!(
            "{call_pos}: syntax-rules: expected literals list"
        )));
    }
    let literals = match &args[0] {
        Expr::List(elems, _) => {
            let mut lits = Vec::new();
            for e in elems {
                if let Expr::Symbol(s, _) = e {
                    lits.push(s.clone());
                } else {
                    return Err(EvalError::Parse(format!(
                        "{call_pos}: syntax-rules: literals must be symbols"
                    )));
                }
            }
            lits
        }
        _ => {
            return Err(EvalError::Parse(format!(
                "{call_pos}: syntax-rules: expected literals list"
            )))
        }
    };
    let mut rules = Vec::new();
    for rule_expr in &args[1..] {
        match rule_expr {
            Expr::List(parts, _) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "{call_pos}: syntax-rules: each rule must be (pattern template)"
                )))
            }
        }
    }
    Ok((literals, rules))
}

pub(super) fn match_pattern(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    match pattern {
        Expr::Symbol(name, _) => {
            if name == "_" {
                return true;
            }
            if literals.contains(name) {
                if let Expr::Symbol(input_name, _) = input {
                    return input_name == name;
                }
                return false;
            }
            bindings.insert(name.clone(), PatternBinding::Single(input.clone()));
            true
        }
        Expr::List(pelems, _) => {
            if let Expr::List(ielems, _) = input {
                match_pattern_list(pelems, ielems, literals, bindings)
            } else {
                false
            }
        }
        Expr::Integer(n, _) => matches!(input, Expr::Integer(m, _) if *m == *n),
        Expr::Boolean(b, _) => matches!(input, Expr::Boolean(c, _) if *c == *b),
        Expr::Str(s, _) => matches!(input, Expr::Str(t, _) if t == s),
        _ => false,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(s, _) if s == "...")
}

fn collect_repeated_bindings(
    pat_vars: &[String],
    sub_bindings: &mut HashMap<String, PatternBinding>,
    repeated: &mut HashMap<String, Vec<Expr>>,
) {
    for var in pat_vars {
        if let Some(PatternBinding::Single(expr)) = sub_bindings.remove(var) {
            repeated
                .get_mut(var)
                .expect("pattern var was pre-inserted")
                .push(expr);
        }
    }
}

fn match_pattern_list(
    patterns: &[Expr],
    inputs: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, PatternBinding>,
) -> bool {
    let mut pi = 0;
    let mut ii = 0;

    while pi < patterns.len() {
        if pi + 1 < patterns.len() && is_ellipsis(&patterns[pi + 1]) {
            let pat = &patterns[pi];
            let pat_vars = collect_pattern_var_names_vec(pat, literals);
            let mut repeated: HashMap<String, Vec<Expr>> = HashMap::new();
            for var in &pat_vars {
                repeated.insert(var.clone(), Vec::new());
            }
            let remaining_patterns = patterns.len() - pi - 2;
            let available = if inputs.len() >= ii + remaining_patterns {
                inputs.len() - ii - remaining_patterns
            } else {
                return false;
            };
            for j in 0..available {
                let mut sub_bindings = HashMap::new();
                if !match_pattern(pat, &inputs[ii + j], literals, &mut sub_bindings) {
                    return false;
                }
                collect_repeated_bindings(&pat_vars, &mut sub_bindings, &mut repeated);
            }
            for (var, exprs) in repeated {
                bindings.insert(var, PatternBinding::Repeated(exprs));
            }
            ii += available;
            pi += 2;
            continue;
        }
        if ii >= inputs.len() {
            return false;
        }
        if !match_pattern(&patterns[pi], &inputs[ii], literals, bindings) {
            return false;
        }
        pi += 1;
        ii += 1;
    }

    ii == inputs.len()
}

fn collect_pattern_var_names_vec(pattern: &Expr, literals: &[String]) -> Vec<String> {
    let mut vars = Vec::new();
    collect_pattern_var_names_inner(pattern, literals, &mut vars);
    vars
}

fn collect_pattern_var_names_inner(pattern: &Expr, literals: &[String], vars: &mut Vec<String>) {
    match pattern {
        Expr::Symbol(name, _) => {
            if name != "..." && name != "_" && !literals.contains(name) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                collect_pattern_var_names_inner(e, literals, vars);
            }
        }
        _ => {}
    }
}

fn collect_template_free_symbols(
    template: &Expr,
    pattern_vars: &HashSet<String>,
) -> HashSet<String> {
    let special: HashSet<&str> = SPECIAL_FORMS.iter().copied().collect();
    let builtins: HashSet<&str> = BUILTINS.iter().copied().collect();
    let mut result = HashSet::new();
    collect_free_inner(template, pattern_vars, &special, &builtins, &mut result);
    result
}

fn collect_free_inner(
    template: &Expr,
    pattern_vars: &HashSet<String>,
    special: &HashSet<&str>,
    builtins: &HashSet<&str>,
    result: &mut HashSet<String>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if !pattern_vars.contains(name)
                && !special.contains(name.as_str())
                && !builtins.contains(name.as_str())
                && name != "..."
                && name != "_"
                && name != "else"
            {
                result.insert(name.clone());
            }
        }
        Expr::List(elems, _) => {
            // Skip inside quote forms — quoted symbols are data, not free references
            if let Some(Expr::Symbol(s, _)) = elems.first() {
                if s == "quote" {
                    return;
                }
            }
            for e in elems {
                collect_free_inner(e, pattern_vars, special, builtins, result);
            }
        }
        _ => {}
    }
}

fn expand_ellipsis_template(
    elem: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    hygiene_map: &HashMap<String, String>,
    result: &mut Vec<Expr>,
) {
    let rep_vars = find_repeated_vars(elem, bindings);
    let Some(first) = rep_vars.first() else {
        return;
    };
    let Some(PatternBinding::Repeated(items)) = bindings.get(first) else {
        return;
    };
    let count = items.len();
    for j in 0..count {
        let indexed = index_bindings(bindings, &rep_vars, j);
        result.push(expand_template(elem, &indexed, hygiene_map));
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
    hygiene_map: &HashMap<String, String>,
) -> Expr {
    match template {
        Expr::Symbol(name, pos) => {
            if let Some(PatternBinding::Single(expr)) = bindings.get(name) {
                return expr.clone();
            }
            if let Some(renamed) = hygiene_map.get(name) {
                return Expr::Symbol(renamed.clone(), *pos);
            }
            template.clone()
        }
        Expr::List(elems, pos) => {
            // Don't expand inside quote forms
            if let Some(Expr::Symbol(s, _)) = elems.first() {
                if s == "quote" {
                    return template.clone();
                }
            }
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    expand_ellipsis_template(&elems[i], bindings, hygiene_map, &mut result);
                    i += 2;
                    continue;
                }
                result.push(expand_template(&elems[i], bindings, hygiene_map));
                i += 1;
            }
            Expr::List(result, *pos)
        }
        _ => template.clone(),
    }
}

fn find_repeated_vars(
    template: &Expr,
    bindings: &HashMap<String, PatternBinding>,
) -> Vec<String> {
    let mut vars = Vec::new();
    match template {
        Expr::Symbol(name, _) => {
            if matches!(bindings.get(name), Some(PatternBinding::Repeated(_))) {
                vars.push(name.clone());
            }
        }
        Expr::List(elems, _) => {
            for e in elems {
                vars.extend(find_repeated_vars(e, bindings));
            }
        }
        _ => {}
    }
    vars
}

fn index_bindings(
    bindings: &HashMap<String, PatternBinding>,
    rep_vars: &[String],
    index: usize,
) -> HashMap<String, PatternBinding> {
    let mut new_bindings = bindings.clone();
    for var in rep_vars {
        if let Some(PatternBinding::Repeated(items)) = bindings.get(var) {
            new_bindings.insert(var.clone(), PatternBinding::Single(items[index].clone()));
        }
    }
    new_bindings
}

/// Expand a `syntax` (aka `#'`) template form using pattern variable bindings from the environment.
/// Returns the expanded Expr and hygiene bindings.
pub(super) fn expand_syntax_form(
    template: &Expr,
    env: &Env,
) -> (Expr, Vec<(String, Value)>) {
    // Collect pattern bindings from environment (SyntaxObject / SyntaxList entries)
    let mut bindings = HashMap::new();
    collect_syntax_env_bindings(template, env, &mut bindings);

    let pattern_vars: HashSet<String> = bindings.keys().cloned().collect();
    let free_syms = collect_template_free_symbols(template, &pattern_vars);

    let mut hygiene_map = HashMap::new();
    let mut hygiene_bindings = Vec::new();
    for sym in &free_syms {
        let gs = gensym(sym);
        hygiene_map.insert(sym.clone(), gs.clone());
        if let Some(val) = env_get(env, sym) {
            hygiene_bindings.push((gs, val));
        }
    }

    let expanded = expand_template(template, &bindings, &hygiene_map);
    (expanded, hygiene_bindings)
}

fn collect_syntax_env_bindings(
    template: &Expr,
    env: &Env,
    bindings: &mut HashMap<String, PatternBinding>,
) {
    match template {
        Expr::Symbol(name, _) => {
            if name != "..." && !bindings.contains_key(name) {
                if let Some(val) = env_get(env, name) {
                    match val {
                        Value::SyntaxObject(expr, _) => {
                            bindings.insert(name.clone(), PatternBinding::Single(*expr));
                        }
                        Value::SyntaxList(exprs) => {
                            bindings.insert(name.clone(), PatternBinding::Repeated(exprs));
                        }
                        _ => {}
                    }
                }
            }
        }
        Expr::List(elems, _) => {
            // Skip inside quote forms
            if let Some(Expr::Symbol(s, _)) = elems.first() {
                if s == "quote" {
                    return;
                }
            }
            for e in elems {
                collect_syntax_env_bindings(e, env, bindings);
            }
        }
        _ => {}
    }
}

/// Convert a Value back to an Expr (inverse of expr_to_value).
pub(super) fn value_to_expr(val: &Value) -> Expr {
    let p = super::parser::Pos::default();
    match val {
        Value::Integer(n) => Expr::Integer(*n, p),
        Value::Float(f) => Expr::Float(*f, p),
        Value::Rational(n, d) => Expr::Rational(*n, *d, p),
        Value::Boolean(b) => Expr::Boolean(*b, p),
        Value::Str(s) => Expr::Str(s.clone(), p),
        Value::Symbol(s) => Expr::Symbol(s.clone(), p),
        Value::Char(c) => Expr::Char(*c, p),
        Value::List(elems) => Expr::List(elems.iter().map(value_to_expr).collect(), p),
        Value::SyntaxObject(expr, _) => (**expr).clone(),
        _ => Expr::Symbol(format!("{val}"), p),
    }
}

pub(super) fn expand_macro(
    literals: &[String],
    rules: &[(Expr, Expr)],
    call_elems: &[Expr],
    call_pos: Pos,
    def_env: &Env,
) -> Result<(Expr, Vec<(String, Value)>), EvalError> {
    let call_args = &call_elems[1..];

    for (pattern, template) in rules {
        let pat_args = match pattern {
            Expr::List(elems, _) => &elems[1..],
            _ => continue,
        };

        let mut bindings = HashMap::new();
        if match_pattern_list(pat_args, call_args, literals, &mut bindings) {
            let mut pattern_vars = HashSet::new();
            for pe in pat_args {
                for v in collect_pattern_var_names_vec(pe, literals) {
                    pattern_vars.insert(v);
                }
            }

            let free_syms = collect_template_free_symbols(template, &pattern_vars);
            let mut hygiene_map = HashMap::new();
            let mut hygiene_bindings = Vec::new();
            for sym in &free_syms {
                let gs = gensym(sym);
                hygiene_map.insert(sym.clone(), gs.clone());
                if let Some(val) = env_get(def_env, sym) {
                    hygiene_bindings.push((gs, val));
                }
            }

            let expanded = expand_template(template, &bindings, &hygiene_map);
            return Ok((expanded, hygiene_bindings));
        }
    }

    Err(EvalError::Parse(format!(
        "{call_pos}: no matching syntax-rules pattern"
    )))
}
