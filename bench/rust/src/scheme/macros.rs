use std::collections::HashMap;

use crate::scheme::env::Env;
use crate::scheme::error::{EvalError, Span};
use crate::scheme::value::{SyntaxRules, Value};

#[derive(Debug, Clone)]
enum Binding {
    Single(Value),
    Ellipsis(Vec<Value>),
}

type PatternBindings = HashMap<String, Binding>;

/// Expand a macro application. `input` is the full list elements including macro name.
pub(crate) fn expand_macro(
    sr: &SyntaxRules,
    input: &[Value],
) -> Result<Value, EvalError> {
    let span = input.first().map_or(Span::default(), |v| v.span());

    for (pattern, template) in &sr.rules {
        let Value::List(pattern_elems, _) = pattern else { continue };
        if pattern_elems.is_empty() || input.is_empty() {
            continue;
        }
        let mut bindings = PatternBindings::new();
        if match_list(&pattern_elems[1..], &input[1..], &sr.literals, &mut bindings) {
            let pattern_vars = collect_pattern_vars(&pattern_elems[1..], &sr.literals);
            let introduced = collect_introduced_bindings(template, &pattern_vars);

            let mut gensym_map = HashMap::new();
            for name in &introduced {
                let gensym = sr.def_env.gensym(name);
                gensym_map.insert(name.clone(), gensym);
            }

            return Ok(expand_template(
                template,
                &bindings,
                &sr.def_env,
                &gensym_map,
                &pattern_vars,
            ));
        }
    }

    Err(EvalError::Parse {
        message: "no matching syntax-rules pattern".to_string(),
        span,
    })
}

fn collect_pattern_vars(patterns: &[Value], literals: &[String]) -> Vec<String> {
    let mut vars = Vec::new();
    for p in patterns {
        collect_pattern_vars_inner(p, literals, &mut vars);
    }
    vars
}

fn collect_pattern_vars_inner(pattern: &Value, literals: &[String], vars: &mut Vec<String>) {
    match pattern {
        Value::Symbol(name, _) if name != "_" && name != "..." && !literals.contains(name) => {
            vars.push(name.clone());
        }
        Value::List(elems, _) => {
            for elem in elems {
                collect_pattern_vars_inner(elem, literals, vars);
            }
        }
        Value::Symbol(_, _)
        | Value::Integer(_, _)
        | Value::Boolean(_, _)
        | Value::String(_, _, _)
        | Value::Char(_, _)
        | Value::Vector(_, _)
        | Value::Closure { .. }
        | Value::Continuation(_)
        | Value::Macro(_)
        | Value::Void => {}
    }
}

fn match_list(
    patterns: &[Value],
    inputs: &[Value],
    literals: &[String],
    bindings: &mut PatternBindings,
) -> bool {
    let ellipsis_pos = patterns
        .iter()
        .position(|p| matches!(p, Value::Symbol(s, _) if s == "..."));

    if let Some(epos) = ellipsis_pos {
        if epos == 0 {
            return false;
        }
        let prefix_len = epos - 1;
        let suffix = &patterns[epos + 1..];
        let suffix_len = suffix.len();

        if inputs.len() < prefix_len + suffix_len {
            return false;
        }

        // Match fixed prefix (before the ellipsis variable)
        for i in 0..prefix_len {
            if !match_pattern(&patterns[i], &inputs[i], literals, bindings) {
                return false;
            }
        }

        // Match fixed suffix
        let suffix_start = inputs.len() - suffix_len;
        for (i, sp) in suffix.iter().enumerate() {
            if !match_pattern(sp, &inputs[suffix_start + i], literals, bindings) {
                return false;
            }
        }

        // Bind ellipsis variable
        let ellipsis_pattern = &patterns[epos - 1];
        let Value::Symbol(var_name, _) = ellipsis_pattern else {
            return false;
        };
        let ellipsis_values: Vec<Value> = inputs[prefix_len..suffix_start].to_vec();
        bindings.insert(var_name.clone(), Binding::Ellipsis(ellipsis_values));

        true
    } else {
        if patterns.len() != inputs.len() {
            return false;
        }
        for (p, inp) in patterns.iter().zip(inputs.iter()) {
            if !match_pattern(p, inp, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn match_pattern(
    pattern: &Value,
    input: &Value,
    literals: &[String],
    bindings: &mut PatternBindings,
) -> bool {
    match pattern {
        Value::Symbol(name, _) if name == "_" => true,
        Value::Symbol(name, _) if name == "..." => true,
        Value::Symbol(name, _) if literals.contains(name) => {
            matches!(input, Value::Symbol(iname, _) if iname == name)
        }
        Value::Symbol(name, _) => {
            bindings.insert(name.clone(), Binding::Single(input.clone()));
            true
        }
        Value::List(pelems, _) => {
            let Value::List(ielems, _) = input else {
                return false;
            };
            match_list(pelems, ielems, literals, bindings)
        }
        Value::Integer(a, _) => matches!(input, Value::Integer(b, _) if a == b),
        Value::Boolean(a, _) => matches!(input, Value::Boolean(b, _) if a == b),
        Value::String(_, _, _)
        | Value::Char(_, _)
        | Value::Vector(_, _)
        | Value::Closure { .. }
        | Value::Continuation(_)
        | Value::Macro(_)
        | Value::Void => false,
    }
}

fn expand_template(
    template: &Value,
    bindings: &PatternBindings,
    def_env: &Env,
    gensym_map: &HashMap<String, String>,
    pattern_vars: &[String],
) -> Value {
    match template {
        Value::Symbol(name, span) => {
            if name == "..." {
                return template.clone();
            }
            // Pattern variable — substitute from bindings
            if pattern_vars.contains(name) {
                if let Some(Binding::Single(val)) = bindings.get(name) {
                    return val.clone();
                }
                return template.clone();
            }
            // Introduced binding — rename via gensym
            if let Some(renamed) = gensym_map.get(name) {
                return Value::Symbol(renamed.clone(), *span);
            }
            // Special form or builtin — keep as-is
            if is_special_form(name) || is_builtin(name) {
                return template.clone();
            }
            // Free variable — check definition-site env for hygiene
            if let Some(val) = def_env.get(name) {
                // Leave macro references as symbols (resolved at eval time)
                if matches!(val, Value::Macro(_)) {
                    return template.clone();
                }
                return val;
            }
            template.clone()
        }
        Value::List(elems, span) => {
            let expanded =
                expand_list_template(elems, bindings, def_env, gensym_map, pattern_vars);
            Value::List(expanded, *span)
        }
        Value::Integer(_, _)
        | Value::Boolean(_, _)
        | Value::String(_, _, _)
        | Value::Char(_, _)
        | Value::Vector(_, _)
        | Value::Closure { .. }
        | Value::Continuation(_)
        | Value::Macro(_)
        | Value::Void => template.clone(),
    }
}

fn expand_list_template(
    elems: &[Value],
    bindings: &PatternBindings,
    def_env: &Env,
    gensym_map: &HashMap<String, String>,
    pattern_vars: &[String],
) -> Vec<Value> {
    let mut result = Vec::new();
    let mut i = 0;

    while i < elems.len() {
        if i + 1 < elems.len()
            && matches!(&elems[i + 1], Value::Symbol(s, _) if s == "...")
        {
            // Ellipsis expansion
            let sub_template = &elems[i];
            let vars = template_symbols(sub_template);
            let ellipsis_var = vars
                .iter()
                .find(|v| matches!(bindings.get(*v), Some(Binding::Ellipsis(_))));

            if let Some(var_name) = ellipsis_var {
                if let Some(Binding::Ellipsis(values)) = bindings.get(var_name) {
                    for val in values {
                        let mut local_bindings = bindings.clone();
                        local_bindings
                            .insert(var_name.clone(), Binding::Single(val.clone()));
                        result.push(expand_template(
                            sub_template,
                            &local_bindings,
                            def_env,
                            gensym_map,
                            pattern_vars,
                        ));
                    }
                }
            }
            i += 2;
        } else {
            result.push(expand_template(
                &elems[i],
                bindings,
                def_env,
                gensym_map,
                pattern_vars,
            ));
            i += 1;
        }
    }

    result
}

fn template_symbols(template: &Value) -> Vec<String> {
    match template {
        Value::Symbol(name, _) if name != "..." => vec![name.clone()],
        Value::List(elems, _) => elems.iter().flat_map(template_symbols).collect(),
        Value::Symbol(_, _)
        | Value::Integer(_, _)
        | Value::Boolean(_, _)
        | Value::String(_, _, _)
        | Value::Char(_, _)
        | Value::Vector(_, _)
        | Value::Closure { .. }
        | Value::Continuation(_)
        | Value::Macro(_)
        | Value::Void => vec![],
    }
}

fn collect_introduced_bindings(template: &Value, pattern_vars: &[String]) -> Vec<String> {
    let Value::List(elems, _) = template else {
        return vec![];
    };
    if elems.is_empty() {
        return vec![];
    }

    let mut introduced = Vec::new();

    let is_let_form = matches!(&elems[0], Value::Symbol(head, _) if matches!(head.as_str(), "let" | "let*" | "letrec"));
    if is_let_form && elems.len() >= 2 {
        if let Value::List(bindings_list, _) = &elems[1] {
            collect_let_bindings(bindings_list, pattern_vars, &mut introduced);
        }
    }

    // Recurse into sub-expressions
    for elem in elems {
        if matches!(elem, Value::List(_, _)) {
            introduced.extend(collect_introduced_bindings(elem, pattern_vars));
        }
    }

    introduced
}

fn collect_let_bindings(bindings_list: &[Value], pattern_vars: &[String], introduced: &mut Vec<String>) {
    for binding in bindings_list {
        let Value::List(pair, _) = binding else { continue };
        let Some(Value::Symbol(name, _)) = pair.first() else { continue };
        if !pattern_vars.contains(name)
            && !is_special_form(name)
            && !is_builtin(name)
        {
            introduced.push(name.clone());
        }
    }
}

fn is_special_form(name: &str) -> bool {
    matches!(
        name,
        "if" | "define"
            | "set!"
            | "quote"
            | "lambda"
            | "let"
            | "begin"
            | "cond"
            | "and"
            | "or"
            | "not"
            | "define-syntax"
            | "syntax-rules"
            | "let*"
            | "letrec"
            | "letrec*"
            | "case"
            | "do"
            | "else"
    )
}

fn is_builtin(name: &str) -> bool {
    matches!(
        name,
        "+" | "-"
            | "*"
            | "/"
            | "<"
            | ">"
            | "="
            | "<="
            | ">="
            | "cons"
            | "car"
            | "cdr"
            | "null?"
            | "list"
            | "length"
            | "append"
            | "pair?"
            | "string?"
            | "number?"
            | "boolean?"
            | "symbol?"
            | "zero?"
            | "positive?"
            | "negative?"
            | "even?"
            | "odd?"
            | "abs"
            | "min"
            | "max"
            | "modulo"
            | "remainder"
            | "quotient"
            | "display"
            | "write"
            | "newline"
            | "string-append"
            | "string-length"
            | "substring"
            | "string->number"
            | "number->string"
            | "symbol->string"
            | "string->symbol"
            | "string-ref"
            | "string-set!"
            | "string-copy"
            | "char?"
            | "apply"
            | "call/cc"
            | "call-with-current-continuation"
            | "eqv?"
            | "equal?"
            | "eq?"
            | "map"
            | "list-ref"
            | "list-tail"
            | "list?"
            | "assoc"
            | "expt"
            | "vector"
            | "make-vector"
            | "vector-ref"
            | "vector-set!"
            | "vector-length"
            | "vector?"
            | "vector->list"
            | "list->vector"
            | "string->list"
            | "list->string"
            | "char-alphabetic?"
            | "char-numeric?"
            | "char-upcase"
            | "char-downcase"
            | "char=?"
            | "char<?"
            | "string=?"
            | "string<?"
            | "string-ci=?"
            | "string-upcase"
            | "string-downcase"
    )
}
