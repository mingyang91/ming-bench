use super::{internal_builtin_name, syntax_error, EvalError, Expr, SourcePos, BUILTIN_BINDINGS};
use std::collections::{HashMap, HashSet};

const INTERNAL_PREFIX: &str = "#%";

#[derive(Clone)]
struct BindingInfo {
    internal: String,
    scope_id: usize,
}

#[derive(Clone)]
struct MacroBinding {
    internal: String,
    scope_id: usize,
}

#[derive(Clone)]
struct ExpandEnv {
    vars: HashMap<String, BindingInfo>,
    macros: HashMap<String, MacroBinding>,
    scope_id: usize,
}

impl ExpandEnv {
    fn root() -> Self {
        let mut vars = HashMap::new();
        for (name, _) in BUILTIN_BINDINGS {
            vars.insert(
                (*name).to_string(),
                BindingInfo {
                    internal: internal_builtin_name(name),
                    scope_id: 0,
                },
            );
        }

        Self {
            vars,
            macros: HashMap::new(),
            scope_id: 0,
        }
    }

    fn child(&self, scope_id: usize) -> Self {
        let mut child = self.clone();
        child.scope_id = scope_id;
        child
    }
}

#[derive(Clone)]
struct SyntaxRuleMacro {
    surface_name: String,
    literals: HashSet<String>,
    rules: Vec<SyntaxRule>,
    def_vars: HashMap<String, String>,
    def_macros: HashMap<String, String>,
}

#[derive(Clone)]
struct SyntaxRule {
    pattern: Expr,
    template: Expr,
}

#[derive(Clone, Default)]
struct MatchBindings {
    single: HashMap<String, Expr>,
    repeated: HashMap<String, Vec<Expr>>,
}

impl MatchBindings {
    fn bind_single(&mut self, name: &str, value: &Expr) -> bool {
        match self.single.get(name) {
            Some(existing) => expr_eq(existing, value),
            None => {
                self.single.insert(name.to_string(), value.clone());
                true
            }
        }
    }

    fn bind_repeated(&mut self, name: &str, value: &Expr) {
        self.repeated
            .entry(name.to_string())
            .or_default()
            .push(value.clone());
    }
}

#[derive(Clone, Default)]
struct IntroEnv {
    introduced: HashMap<String, String>,
    pattern_overrides: HashMap<String, String>,
}

struct Expander {
    next_scope_id: usize,
    next_internal_id: usize,
    macros: HashMap<String, SyntaxRuleMacro>,
}

pub(super) fn expand_program(exprs: Vec<Expr>) -> Result<Vec<Expr>, EvalError> {
    let mut expander = Expander::new();
    let mut env = ExpandEnv::root();
    expander.expand_sequence(&exprs, &mut env)
}

impl Expander {
    fn new() -> Self {
        Self {
            next_scope_id: 1,
            next_internal_id: 0,
            macros: HashMap::new(),
        }
    }

    fn expand_sequence(
        &mut self,
        exprs: &[Expr],
        env: &mut ExpandEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::new();

        for expr in exprs {
            if let Some(expr) = self.expand_sequence_expr(expr, env)? {
                expanded.push(expr);
            }
        }

        Ok(expanded)
    }

    fn expand_sequence_expr(
        &mut self,
        expr: &Expr,
        env: &mut ExpandEnv,
    ) -> Result<Option<Expr>, EvalError> {
        let Expr::List(items, pos) = expr else {
            return Ok(Some(self.expand_expr(expr, env)?));
        };

        let Some(head) = items.first() else {
            return Ok(Some(expr.clone()));
        };

        match symbol_name(head) {
            Some("define-syntax") => {
                self.expand_define_syntax(&items[1..], *pos, env)?;
                Ok(None)
            }
            Some("define") => Ok(Some(self.expand_define(&items[1..], *pos, env)?)),
            Some("begin") => {
                let body = self.expand_sequence(&items[1..], env)?;
                Ok(Some(list_with_head("begin", *pos, body)))
            }
            _ => Ok(Some(self.expand_expr(expr, env)?)),
        }
    }

    fn expand_expr(&mut self, expr: &Expr, env: &ExpandEnv) -> Result<Expr, EvalError> {
        match expr {
            Expr::Int(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
                Ok(expr.clone())
            }
            Expr::Symbol(name, pos) => {
                Ok(Expr::Symbol(self.resolve_source_symbol(name, env), *pos))
            }
            Expr::List(items, pos) => {
                let Some(head) = items.first() else {
                    return Ok(expr.clone());
                };

                if let Some(macro_id) = self.lookup_macro_call(head, env) {
                    return self.expand_macro_call(items, *pos, &macro_id, env);
                }

                match symbol_name(head) {
                    Some("quote") => Ok(expr.clone()),
                    Some("if") => self.expand_if(items, *pos, env),
                    Some("lambda") => self.expand_lambda(items, *pos, env),
                    Some("define") => {
                        let mut local = env.clone();
                        self.expand_define(&items[1..], *pos, &mut local)
                    }
                    Some("define-syntax") => {
                        let mut local = env.clone();
                        self.expand_define_syntax(&items[1..], *pos, &mut local)?;
                        Ok(list_with_head("begin", *pos, Vec::new()))
                    }
                    Some("set!") => self.expand_set(items, *pos, env),
                    Some("and") | Some("or") => self.expand_n_ary_special(items, *pos, env),
                    Some("let") => self.expand_let(items, *pos, env),
                    Some("letrec") => self.expand_letrec(items, *pos, env, false),
                    Some("letrec*") => self.expand_letrec(items, *pos, env, true),
                    Some("begin") => {
                        let mut local = env.clone();
                        let body = self.expand_sequence(&items[1..], &mut local)?;
                        Ok(list_with_head("begin", *pos, body))
                    }
                    Some("cond") => self.expand_cond(items, *pos, env),
                    Some("case") => self.expand_case(items, *pos, env),
                    Some("guard") => self.expand_guard(items, *pos, env),
                    _ => self.expand_application(items, *pos, env),
                }
            }
        }
    }

    fn expand_define(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        env: &mut ExpandEnv,
    ) -> Result<Expr, EvalError> {
        match parts {
            [Expr::Symbol(name, target_pos), value_expr] => {
                let internal = self.bind_var(env, name);
                let value = self.expand_expr(value_expr, env)?;
                Ok(Expr::List(
                    vec![
                        Expr::Symbol("define".into(), pos),
                        Expr::Symbol(internal, *target_pos),
                        value,
                    ],
                    pos,
                ))
            }
            [Expr::List(signature, signature_pos), body @ ..] => {
                if body.is_empty() {
                    return Err(syntax_error(pos, "define requires a function body"));
                }

                let Some((name_expr, formals)) = signature.split_first() else {
                    return Err(syntax_error(
                        *signature_pos,
                        "define requires a function name",
                    ));
                };

                let Expr::Symbol(name, name_pos) = name_expr else {
                    return Err(syntax_error(
                        name_expr.pos(),
                        "define function name must be a symbol",
                    ));
                };

                let internal = self.bind_var(env, name);
                let mut body_env = self.child_env(env);
                let expanded_formals = self.expand_source_formals(formals, &mut body_env)?;
                let expanded_body = self.expand_sequence(body, &mut body_env)?;

                let mut signature_items = Vec::with_capacity(expanded_formals.len() + 1);
                signature_items.push(Expr::Symbol(internal, *name_pos));
                signature_items.extend(expanded_formals);

                let mut result = Vec::with_capacity(expanded_body.len() + 2);
                result.push(Expr::Symbol("define".into(), pos));
                result.push(Expr::List(signature_items, *signature_pos));
                result.extend(expanded_body);
                Ok(Expr::List(result, pos))
            }
            _ => Err(syntax_error(expr_pos(parts, pos), "malformed define")),
        }
    }

    fn expand_define_syntax(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        env: &mut ExpandEnv,
    ) -> Result<(), EvalError> {
        let [name_expr, transformer] = parts else {
            return Err(syntax_error(pos, "malformed define-syntax"));
        };

        let Expr::Symbol(name, _) = name_expr else {
            return Err(syntax_error(
                name_expr.pos(),
                "define-syntax name must be a symbol",
            ));
        };

        let internal_name = self.bind_macro(env, name);
        let mut def_macros = env
            .macros
            .iter()
            .map(|(name, binding)| (name.clone(), binding.internal.clone()))
            .collect::<HashMap<_, _>>();
        def_macros.insert(name.clone(), internal_name.clone());
        let def_vars = env
            .vars
            .iter()
            .map(|(name, binding)| (name.clone(), binding.internal.clone()))
            .collect::<HashMap<_, _>>();

        let macro_def =
            self.parse_syntax_rules(name, &internal_name, transformer, &def_vars, &def_macros)?;
        self.macros.insert(internal_name, macro_def);
        Ok(())
    }

    fn expand_if(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let mut result = vec![Expr::Symbol("if".into(), pos)];
        match items {
            [_, test, consequent] => {
                result.push(self.expand_expr(test, env)?);
                result.push(self.expand_expr(consequent, env)?);
            }
            [_, test, consequent, alternate] => {
                result.push(self.expand_expr(test, env)?);
                result.push(self.expand_expr(consequent, env)?);
                result.push(self.expand_expr(alternate, env)?);
            }
            _ => return Err(syntax_error(pos, "if requires 2 or 3 arguments")),
        }

        Ok(Expr::List(result, pos))
    }

    fn expand_lambda(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "lambda requires a parameter list"));
        };
        let Some((formals_expr, body)) = rest.split_first() else {
            return Err(syntax_error(pos, "lambda requires a parameter list"));
        };
        if body.is_empty() {
            return Err(syntax_error(pos, "lambda requires a body"));
        }

        let mut body_env = self.child_env(env);
        let expanded_formals = self.expand_source_formals_expr(formals_expr, &mut body_env)?;
        let expanded_body = self.expand_sequence(body, &mut body_env)?;

        let mut result = Vec::with_capacity(expanded_body.len() + 2);
        result.push(Expr::Symbol("lambda".into(), pos));
        result.push(expanded_formals);
        result.extend(expanded_body);
        Ok(Expr::List(result, pos))
    }

    fn expand_set(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let [_, target, value_expr] = items else {
            return Err(syntax_error(pos, "set! requires exactly 2 arguments"));
        };

        let target = match target {
            Expr::Symbol(name, target_pos) => {
                Expr::Symbol(self.resolve_source_symbol(name, env), *target_pos)
            }
            _ => target.clone(),
        };

        Ok(Expr::List(
            vec![
                Expr::Symbol("set!".into(), pos),
                target,
                self.expand_expr(value_expr, env)?,
            ],
            pos,
        ))
    }

    fn expand_n_ary_special(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let mut result = Vec::with_capacity(items.len());
        result.push(items[0].clone());
        for expr in &items[1..] {
            result.push(self.expand_expr(expr, env)?);
        }
        Ok(Expr::List(result, pos))
    }

    fn expand_let(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, parts)) = items.split_first() else {
            return Err(syntax_error(pos, "let requires bindings"));
        };

        match parts {
            [Expr::Symbol(name, name_pos), bindings_expr, body @ ..] => {
                if body.is_empty() {
                    return Err(syntax_error(pos, "let requires a body"));
                }

                let bindings = binding_list(bindings_expr)?;
                let mut body_env = self.child_env(env);
                let let_name = self.bind_var(&mut body_env, name);
                let expanded_bindings = self.expand_let_bindings(bindings, env, &mut body_env)?;
                let expanded_body = self.expand_sequence(body, &mut body_env)?;

                let mut result = Vec::with_capacity(expanded_body.len() + 3);
                result.push(Expr::Symbol("let".into(), pos));
                result.push(Expr::Symbol(let_name, *name_pos));
                result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
                result.extend(expanded_body);
                Ok(Expr::List(result, pos))
            }
            [bindings_expr, body @ ..] => {
                if body.is_empty() {
                    return Err(syntax_error(pos, "let requires a body"));
                }

                let bindings = binding_list(bindings_expr)?;
                let mut body_env = self.child_env(env);
                let expanded_bindings = self.expand_let_bindings(bindings, env, &mut body_env)?;
                let expanded_body = self.expand_sequence(body, &mut body_env)?;

                let mut result = Vec::with_capacity(expanded_body.len() + 2);
                result.push(Expr::Symbol("let".into(), pos));
                result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
                result.extend(expanded_body);
                Ok(Expr::List(result, pos))
            }
            _ => Err(syntax_error(pos, "let requires bindings")),
        }
    }

    fn expand_letrec(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
        sequential: bool,
    ) -> Result<Expr, EvalError> {
        let Some((_, parts)) = items.split_first() else {
            return Err(syntax_error(pos, "letrec requires bindings"));
        };
        let [bindings_expr, body @ ..] = parts else {
            return Err(syntax_error(pos, "letrec requires bindings"));
        };
        if body.is_empty() {
            let form_name = if sequential { "letrec*" } else { "letrec" };
            return Err(syntax_error(pos, format!("{form_name} requires a body")));
        }

        let bindings = binding_list(bindings_expr)?;
        let mut body_env = self.child_env(env);
        let expanded_bindings = if sequential {
            self.expand_recursive_star_bindings(bindings, &mut body_env)?
        } else {
            self.expand_recursive_bindings(bindings, &mut body_env)?
        };
        let expanded_body = self.expand_sequence(body, &mut body_env)?;

        let mut result = Vec::with_capacity(expanded_body.len() + 2);
        result.push(Expr::Symbol(
            if sequential {
                "letrec*".into()
            } else {
                "letrec".into()
            },
            pos,
        ));
        result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
        result.extend(expanded_body);
        Ok(Expr::List(result, pos))
    }

    fn expand_cond(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let mut clauses = Vec::with_capacity(items.len());
        clauses.push(Expr::Symbol("cond".into(), pos));

        for clause in &items[1..] {
            let Expr::List(parts, clause_pos) = clause else {
                return Err(syntax_error(clause.pos(), "cond clause must be a list"));
            };
            let Some((test, body)) = parts.split_first() else {
                return Err(syntax_error(clause.pos(), "cond clause cannot be empty"));
            };

            let mut clause_items = Vec::with_capacity(parts.len());
            if matches!(symbol_name(test), Some("else")) {
                clause_items.push(test.clone());
            } else {
                clause_items.push(self.expand_expr(test, env)?);
            }

            let mut clause_env = env.clone();
            let expanded_body = self.expand_sequence(body, &mut clause_env)?;
            clause_items.extend(expanded_body);
            clauses.push(Expr::List(clause_items, *clause_pos));
        }

        Ok(Expr::List(clauses, pos))
    }

    fn expand_case(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "case requires a key"));
        };
        let Some((key, clauses)) = rest.split_first() else {
            return Err(syntax_error(pos, "case requires a key"));
        };

        let mut result = Vec::with_capacity(items.len());
        result.push(Expr::Symbol("case".into(), pos));
        result.push(self.expand_expr(key, env)?);

        for clause in clauses {
            let Expr::List(parts, clause_pos) = clause else {
                return Err(syntax_error(clause.pos(), "case clause must be a list"));
            };
            let Some((datum_expr, body)) = parts.split_first() else {
                return Err(syntax_error(clause.pos(), "case clause cannot be empty"));
            };

            let mut clause_items = Vec::with_capacity(parts.len());
            if matches!(symbol_name(datum_expr), Some("else")) {
                clause_items.push(datum_expr.clone());
            } else {
                let Expr::List(datums, datum_pos) = datum_expr else {
                    return Err(syntax_error(
                        datum_expr.pos(),
                        "case clause datums must be a list",
                    ));
                };
                clause_items.push(Expr::List(datums.clone(), *datum_pos));
            }

            let mut clause_env = env.clone();
            let expanded_body = self.expand_sequence(body, &mut clause_env)?;
            clause_items.extend(expanded_body);
            result.push(Expr::List(clause_items, *clause_pos));
        }

        Ok(Expr::List(result, pos))
    }

    fn expand_guard(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "guard requires a variable and body"));
        };
        let Some((spec, body)) = rest.split_first() else {
            return Err(syntax_error(pos, "guard requires a variable and body"));
        };
        if body.is_empty() {
            return Err(syntax_error(pos, "guard requires a body"));
        }

        let Expr::List(spec_items, spec_pos) = spec else {
            return Err(syntax_error(
                spec.pos(),
                "guard requires a variable and clauses",
            ));
        };
        let Some((var_expr, clauses)) = spec_items.split_first() else {
            return Err(syntax_error(
                spec.pos(),
                "guard requires an exception variable",
            ));
        };
        let Expr::Symbol(name, name_pos) = var_expr else {
            return Err(syntax_error(
                var_expr.pos(),
                "guard exception variable must be a symbol",
            ));
        };

        let mut clause_env = self.child_env(env);
        let internal = self.bind_var(&mut clause_env, name);
        let mut expanded_spec = Vec::with_capacity(spec_items.len());
        expanded_spec.push(Expr::Symbol(internal, *name_pos));

        for clause in clauses {
            let Expr::List(parts, clause_pos) = clause else {
                return Err(syntax_error(clause.pos(), "guard clause must be a list"));
            };
            let Some((test, clause_body)) = parts.split_first() else {
                return Err(syntax_error(clause.pos(), "guard clause cannot be empty"));
            };

            let mut clause_items = Vec::with_capacity(parts.len());
            if matches!(symbol_name(test), Some("else")) {
                clause_items.push(test.clone());
            } else {
                clause_items.push(self.expand_expr(test, &clause_env)?);
            }

            let mut clause_body_env = clause_env.clone();
            let expanded_body = self.expand_sequence(clause_body, &mut clause_body_env)?;
            clause_items.extend(expanded_body);
            expanded_spec.push(Expr::List(clause_items, *clause_pos));
        }

        let mut body_env = env.clone();
        let expanded_body = self.expand_sequence(body, &mut body_env)?;

        let mut result = Vec::with_capacity(expanded_body.len() + 2);
        result.push(Expr::Symbol("guard".into(), pos));
        result.push(Expr::List(expanded_spec, *spec_pos));
        result.extend(expanded_body);
        Ok(Expr::List(result, pos))
    }

    fn expand_application(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let mut result = Vec::with_capacity(items.len());
        for item in items {
            result.push(self.expand_expr(item, env)?);
        }
        Ok(Expr::List(result, pos))
    }

    fn expand_let_bindings(
        &mut self,
        bindings: &[Expr],
        value_env: &ExpandEnv,
        body_env: &mut ExpandEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(bindings.len());

        for binding in bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };
            let Expr::Symbol(name, name_pos) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "let binding name must be a symbol",
                ));
            };

            let value = self.expand_expr(value_expr, value_env)?;
            let internal = self.bind_var(body_env, name);
            expanded.push(Expr::List(
                vec![Expr::Symbol(internal, *name_pos), value],
                *binding_pos,
            ));
        }

        Ok(expanded)
    }

    fn expand_recursive_bindings(
        &mut self,
        bindings: &[Expr],
        body_env: &mut ExpandEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut parsed = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };
            let Expr::Symbol(name, name_pos) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "let binding name must be a symbol",
                ));
            };

            let internal = self.bind_var(body_env, name);
            parsed.push((internal, *name_pos, value_expr.clone(), *binding_pos));
        }

        let value_env = body_env.clone();
        parsed
            .into_iter()
            .map(|(internal, name_pos, value_expr, binding_pos)| {
                Ok(Expr::List(
                    vec![
                        Expr::Symbol(internal, name_pos),
                        self.expand_expr(&value_expr, &value_env)?,
                    ],
                    binding_pos,
                ))
            })
            .collect()
    }

    fn expand_recursive_star_bindings(
        &mut self,
        bindings: &[Expr],
        body_env: &mut ExpandEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };
            let Expr::Symbol(name, name_pos) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "let binding name must be a symbol",
                ));
            };

            let internal = self.bind_var(body_env, name);
            expanded.push(Expr::List(
                vec![
                    Expr::Symbol(internal, *name_pos),
                    self.expand_expr(value_expr, body_env)?,
                ],
                *binding_pos,
            ));
        }

        Ok(expanded)
    }

    fn expand_source_formals_expr(
        &mut self,
        expr: &Expr,
        env: &mut ExpandEnv,
    ) -> Result<Expr, EvalError> {
        match expr {
            Expr::Symbol(name, pos) => Ok(Expr::Symbol(self.bind_var(env, name), *pos)),
            Expr::List(items, pos) => Ok(Expr::List(self.expand_source_formals(items, env)?, *pos)),
            _ => Err(syntax_error(
                expr.pos(),
                "lambda parameters must be a list or symbol",
            )),
        }
    }

    fn expand_source_formals(
        &mut self,
        items: &[Expr],
        env: &mut ExpandEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        for item in items {
            match item {
                Expr::Symbol(name, pos) if name == "." => {
                    expanded.push(item.clone());
                }
                Expr::Symbol(name, pos) => {
                    expanded.push(Expr::Symbol(self.bind_var(env, name), *pos));
                }
                _ => return Err(syntax_error(item.pos(), "parameter names must be symbols")),
            }
        }
        Ok(expanded)
    }

    fn parse_syntax_rules(
        &self,
        surface_name: &str,
        _internal_name: &str,
        transformer: &Expr,
        def_vars: &HashMap<String, String>,
        def_macros: &HashMap<String, String>,
    ) -> Result<SyntaxRuleMacro, EvalError> {
        let Expr::List(items, pos) = transformer else {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be syntax-rules",
            ));
        };

        let Some(Expr::Symbol(head, _)) = items.first() else {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be syntax-rules",
            ));
        };
        if head != "syntax-rules" {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be syntax-rules",
            ));
        }

        let Some((literal_expr, rules)) = items[1..].split_first() else {
            return Err(syntax_error(*pos, "syntax-rules requires a literal list"));
        };

        let Expr::List(literal_items, _) = literal_expr else {
            return Err(syntax_error(
                literal_expr.pos(),
                "syntax-rules literals must be a list",
            ));
        };

        let mut literals = HashSet::new();
        for literal in literal_items {
            let Expr::Symbol(name, _) = literal else {
                return Err(syntax_error(
                    literal.pos(),
                    "syntax-rules literals must be symbols",
                ));
            };
            literals.insert(resolve_definition_identifier(name, def_vars, def_macros));
        }

        if rules.is_empty() {
            return Err(syntax_error(
                *pos,
                "syntax-rules requires at least one rule",
            ));
        }

        let mut parsed_rules = Vec::with_capacity(rules.len());
        for rule in rules {
            let Expr::List(parts, _) = rule else {
                return Err(syntax_error(rule.pos(), "syntax-rules rule must be a list"));
            };
            let [pattern, template] = parts.as_slice() else {
                return Err(syntax_error(
                    rule.pos(),
                    "syntax-rules rule must contain a pattern and template",
                ));
            };
            parsed_rules.push(SyntaxRule {
                pattern: pattern.clone(),
                template: template.clone(),
            });
        }

        Ok(SyntaxRuleMacro {
            surface_name: surface_name.to_string(),
            literals,
            rules: parsed_rules,
            def_vars: def_vars.clone(),
            def_macros: def_macros.clone(),
        })
    }

    fn expand_macro_call(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_id: &str,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let macro_def = self
            .macros
            .get(macro_id)
            .cloned()
            .ok_or_else(|| syntax_error(pos, "unknown syntax transformer"))?;

        for rule in &macro_def.rules {
            if let Some(bindings) = self.match_rule(rule, items, &macro_def, env)? {
                let expanded = self.expand_template(
                    &rule.template,
                    &macro_def,
                    &bindings,
                    None,
                    &IntroEnv::default(),
                )?;
                return self.expand_expr(&expanded, env);
            }
        }

        Err(syntax_error(
            pos,
            format!(
                "no matching syntax-rules clause for {}",
                macro_def.surface_name
            ),
        ))
    }

    fn match_rule(
        &self,
        rule: &SyntaxRule,
        input_items: &[Expr],
        macro_def: &SyntaxRuleMacro,
        env: &ExpandEnv,
    ) -> Result<Option<MatchBindings>, EvalError> {
        let Expr::List(pattern_items, _) = &rule.pattern else {
            return Err(syntax_error(
                rule.pattern.pos(),
                "syntax-rules pattern must be a list",
            ));
        };

        let Some(head) = pattern_items.first() else {
            return Ok(None);
        };

        let mut literals = macro_def.literals.clone();
        if let Some(name) = symbol_name(head) {
            if name != "_" {
                literals.insert(resolve_definition_identifier(
                    name,
                    &macro_def.def_vars,
                    &macro_def.def_macros,
                ));
            }
        } else {
            return Err(syntax_error(
                head.pos(),
                "syntax-rules pattern head must be a symbol",
            ));
        }

        let mut bindings = MatchBindings::default();
        if self.match_list_pattern(
            pattern_items,
            input_items,
            &literals,
            macro_def,
            env,
            &mut bindings,
        )? {
            Ok(Some(bindings))
        } else {
            Ok(None)
        }
    }

    fn match_list_pattern(
        &self,
        patterns: &[Expr],
        inputs: &[Expr],
        literals: &HashSet<String>,
        macro_def: &SyntaxRuleMacro,
        env: &ExpandEnv,
        bindings: &mut MatchBindings,
    ) -> Result<bool, EvalError> {
        self.match_list_pattern_from(patterns, 0, inputs, 0, literals, macro_def, env, bindings)
    }

    fn match_list_pattern_from(
        &self,
        patterns: &[Expr],
        pattern_index: usize,
        inputs: &[Expr],
        input_index: usize,
        literals: &HashSet<String>,
        macro_def: &SyntaxRuleMacro,
        env: &ExpandEnv,
        bindings: &mut MatchBindings,
    ) -> Result<bool, EvalError> {
        if pattern_index == patterns.len() {
            return Ok(input_index == inputs.len());
        }

        if is_ellipsis(&patterns[pattern_index]) {
            return Err(syntax_error(
                patterns[pattern_index].pos(),
                "misplaced ellipsis in pattern",
            ));
        }

        let repeated =
            pattern_index + 1 < patterns.len() && is_ellipsis(&patterns[pattern_index + 1]);
        if repeated {
            let min_rest = min_required_pattern_items(&patterns[pattern_index + 2..]);
            if inputs.len() < input_index + min_rest {
                return Ok(false);
            }

            let max_count = inputs.len() - input_index - min_rest;
            for count in 0..=max_count {
                let mut local = bindings.clone();
                let mut matched = true;

                for offset in 0..count {
                    if !self.match_pattern(
                        &patterns[pattern_index],
                        &inputs[input_index + offset],
                        true,
                        literals,
                        macro_def,
                        env,
                        &mut local,
                    )? {
                        matched = false;
                        break;
                    }
                }

                if matched
                    && self.match_list_pattern_from(
                        patterns,
                        pattern_index + 2,
                        inputs,
                        input_index + count,
                        literals,
                        macro_def,
                        env,
                        &mut local,
                    )?
                {
                    *bindings = local;
                    return Ok(true);
                }
            }

            Ok(false)
        } else {
            let Some(input) = inputs.get(input_index) else {
                return Ok(false);
            };
            let mut local = bindings.clone();
            if !self.match_pattern(
                &patterns[pattern_index],
                input,
                false,
                literals,
                macro_def,
                env,
                &mut local,
            )? {
                return Ok(false);
            }
            if self.match_list_pattern_from(
                patterns,
                pattern_index + 1,
                inputs,
                input_index + 1,
                literals,
                macro_def,
                env,
                &mut local,
            )? {
                *bindings = local;
                Ok(true)
            } else {
                Ok(false)
            }
        }
    }

    fn match_pattern(
        &self,
        pattern: &Expr,
        input: &Expr,
        repeated: bool,
        literals: &HashSet<String>,
        macro_def: &SyntaxRuleMacro,
        env: &ExpandEnv,
        bindings: &mut MatchBindings,
    ) -> Result<bool, EvalError> {
        match pattern {
            Expr::Int(value, _) => Ok(matches!(input, Expr::Int(found, _) if found == value)),
            Expr::Bool(value, _) => Ok(matches!(input, Expr::Bool(found, _) if found == value)),
            Expr::String(value, _) => Ok(matches!(input, Expr::String(found, _) if found == value)),
            Expr::Char(value, _) => Ok(matches!(input, Expr::Char(found, _) if found == value)),
            Expr::Symbol(name, _) => {
                if name == "_" {
                    return Ok(true);
                }

                let resolved =
                    resolve_definition_identifier(name, &macro_def.def_vars, &macro_def.def_macros);
                if literals.contains(&resolved) {
                    return Ok(match input {
                        Expr::Symbol(found, _) => {
                            resolve_call_identifier(found, env, &self.macros) == resolved
                        }
                        _ => false,
                    });
                }

                if repeated {
                    bindings.bind_repeated(name, input);
                    Ok(true)
                } else {
                    Ok(bindings.bind_single(name, input))
                }
            }
            Expr::List(pattern_items, _) => {
                let Expr::List(input_items, _) = input else {
                    return Ok(false);
                };
                self.match_list_pattern(
                    pattern_items,
                    input_items,
                    literals,
                    macro_def,
                    env,
                    bindings,
                )
            }
        }
    }

    fn expand_template(
        &mut self,
        template: &Expr,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        match template {
            Expr::Int(_, _) | Expr::Bool(_, _) | Expr::String(_, _) | Expr::Char(_, _) => {
                Ok(template.clone())
            }
            Expr::Symbol(name, pos) => {
                self.expand_template_symbol(name, *pos, macro_def, bindings, repeat_index, intro)
            }
            Expr::List(items, pos) => {
                let Some(head) = items.first() else {
                    return Ok(template.clone());
                };

                match symbol_name(head) {
                    Some("lambda") => self.expand_template_lambda(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("let") => self.expand_template_let(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("letrec") => self.expand_template_letrec(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                        false,
                    ),
                    Some("letrec*") => self.expand_template_letrec(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                        true,
                    ),
                    Some("case") => self.expand_template_case(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("define") => self.expand_template_define(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    _ => {
                        let mut expanded = Vec::new();
                        let mut index = 0;
                        while index < items.len() {
                            if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                                let count = repeat_count(&items[index], bindings)?;
                                for item_index in 0..count {
                                    expanded.push(self.expand_template(
                                        &items[index],
                                        macro_def,
                                        bindings,
                                        Some(item_index),
                                        intro,
                                    )?);
                                }
                                index += 2;
                            } else {
                                expanded.push(self.expand_template(
                                    &items[index],
                                    macro_def,
                                    bindings,
                                    repeat_index,
                                    intro,
                                )?);
                                index += 1;
                            }
                        }
                        Ok(Expr::List(expanded, *pos))
                    }
                }
            }
        }
    }

    fn expand_template_symbol(
        &self,
        name: &str,
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        if let Some(internal) = intro.pattern_overrides.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        if let Some(value) = lookup_binding_expr(bindings, name, repeat_index)? {
            return Ok(value);
        }

        if let Some(internal) = intro.introduced.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        if let Some(internal) = macro_def.def_vars.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        if let Some(internal) = macro_def.def_macros.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        Ok(Expr::Symbol(name.to_string(), pos))
    }

    fn expand_template_lambda(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "lambda requires a parameter list"));
        };
        let Some((formals_expr, body)) = rest.split_first() else {
            return Err(syntax_error(pos, "lambda requires a parameter list"));
        };

        let mut body_intro = intro.clone();
        let formals = self.expand_template_formals_expr(
            formals_expr,
            macro_def,
            bindings,
            repeat_index,
            &mut body_intro,
        )?;
        let expanded_body = self.expand_template_sequence(
            body,
            macro_def,
            bindings,
            repeat_index,
            &mut body_intro,
        )?;

        let mut expanded = Vec::with_capacity(expanded_body.len() + 2);
        expanded.push(Expr::Symbol("lambda".into(), pos));
        expanded.push(formals);
        expanded.extend(expanded_body);

        Ok(Expr::List(expanded, pos))
    }

    fn expand_template_let(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "let requires bindings"));
        };

        match rest {
            [Expr::Symbol(name, name_pos), bindings_expr, body @ ..] => {
                let raw_bindings = binding_list(bindings_expr)?;
                let mut body_intro = intro.clone();
                let let_name = self.bind_template_identifier(
                    &Expr::Symbol(name.clone(), *name_pos),
                    macro_def,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?;
                let expanded_bindings = self.expand_template_bindings(
                    raw_bindings,
                    macro_def,
                    bindings,
                    repeat_index,
                    intro,
                    &mut body_intro,
                )?;

                let mut result = Vec::with_capacity(items.len());
                result.push(Expr::Symbol("let".into(), pos));
                result.push(let_name);
                result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
                result.extend(self.expand_template_sequence(
                    body,
                    macro_def,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?);
                Ok(Expr::List(result, pos))
            }
            [bindings_expr, body @ ..] => {
                let raw_bindings = binding_list(bindings_expr)?;
                let mut body_intro = intro.clone();
                let expanded_bindings = self.expand_template_bindings(
                    raw_bindings,
                    macro_def,
                    bindings,
                    repeat_index,
                    intro,
                    &mut body_intro,
                )?;

                let mut result = Vec::with_capacity(items.len());
                result.push(Expr::Symbol("let".into(), pos));
                result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
                result.extend(self.expand_template_sequence(
                    body,
                    macro_def,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?);
                Ok(Expr::List(result, pos))
            }
            _ => Err(syntax_error(pos, "let requires bindings")),
        }
    }

    fn expand_template_letrec(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
        sequential: bool,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "letrec requires bindings"));
        };

        let [bindings_expr, body @ ..] = rest else {
            return Err(syntax_error(pos, "letrec requires bindings"));
        };

        let raw_bindings = binding_list(bindings_expr)?;
        let mut body_intro = intro.clone();
        let expanded_bindings = if sequential {
            self.expand_template_recursive_star_bindings(
                raw_bindings,
                macro_def,
                bindings,
                repeat_index,
                &mut body_intro,
            )?
        } else {
            self.expand_template_recursive_bindings(
                raw_bindings,
                macro_def,
                bindings,
                repeat_index,
                &mut body_intro,
            )?
        };

        let mut result = Vec::with_capacity(items.len());
        result.push(Expr::Symbol(
            if sequential {
                "letrec*".into()
            } else {
                "letrec".into()
            },
            pos,
        ));
        result.push(Expr::List(expanded_bindings, bindings_expr.pos()));
        result.extend(self.expand_template_sequence(
            body,
            macro_def,
            bindings,
            repeat_index,
            &mut body_intro,
        )?);
        Ok(Expr::List(result, pos))
    }

    fn expand_template_case(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        let Some((_, rest)) = items.split_first() else {
            return Err(syntax_error(pos, "case requires a key"));
        };
        let Some((key, clauses)) = rest.split_first() else {
            return Err(syntax_error(pos, "case requires a key"));
        };

        let mut result = Vec::with_capacity(items.len());
        result.push(Expr::Symbol("case".into(), pos));
        result.push(self.expand_template(key, macro_def, bindings, repeat_index, intro)?);

        for clause in clauses {
            let Expr::List(parts, clause_pos) = clause else {
                return Err(syntax_error(clause.pos(), "case clause must be a list"));
            };
            let Some((datum_expr, body)) = parts.split_first() else {
                return Err(syntax_error(clause.pos(), "case clause cannot be empty"));
            };

            let mut clause_items = Vec::with_capacity(parts.len());
            if matches!(symbol_name(datum_expr), Some("else")) {
                clause_items.push(datum_expr.clone());
            } else {
                let Expr::List(datums, datum_pos) = datum_expr else {
                    return Err(syntax_error(
                        datum_expr.pos(),
                        "case clause datums must be a list",
                    ));
                };
                clause_items.push(Expr::List(datums.clone(), *datum_pos));
            }

            let mut clause_intro = intro.clone();
            clause_items.extend(self.expand_template_sequence(
                body,
                macro_def,
                bindings,
                repeat_index,
                &mut clause_intro,
            )?);
            result.push(Expr::List(clause_items, *clause_pos));
        }

        Ok(Expr::List(result, pos))
    }

    fn expand_template_define(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        let mut local = intro.clone();
        self.expand_template_define_in_sequence(
            items,
            pos,
            macro_def,
            bindings,
            repeat_index,
            &mut local,
        )
    }

    fn expand_template_define_in_sequence(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Expr, EvalError> {
        match &items[1..] {
            [Expr::Symbol(name, name_pos), value_expr] => {
                let target = self.bind_template_identifier(
                    &Expr::Symbol(name.clone(), *name_pos),
                    macro_def,
                    bindings,
                    repeat_index,
                    intro,
                )?;
                let value =
                    self.expand_template(value_expr, macro_def, bindings, repeat_index, intro)?;
                Ok(Expr::List(
                    vec![Expr::Symbol("define".into(), pos), target, value],
                    pos,
                ))
            }
            [Expr::List(signature, signature_pos), body @ ..] => {
                let Some((name_expr, formals)) = signature.split_first() else {
                    return Err(syntax_error(
                        *signature_pos,
                        "define requires a function name",
                    ));
                };

                let target = self.bind_template_identifier(
                    name_expr,
                    macro_def,
                    bindings,
                    repeat_index,
                    intro,
                )?;
                let mut body_intro = intro.clone();
                let expanded_formals = self.expand_template_formals(
                    formals,
                    macro_def,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?;
                let expanded_body = self.expand_template_sequence(
                    body,
                    macro_def,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?;

                let mut signature_items = Vec::with_capacity(formals.len() + 1);
                signature_items.push(target);
                signature_items.extend(expanded_formals);

                let mut result = Vec::with_capacity(expanded_body.len() + 2);
                result.push(Expr::Symbol("define".into(), pos));
                result.push(Expr::List(signature_items, *signature_pos));
                result.extend(expanded_body);
                Ok(Expr::List(result, pos))
            }
            _ => Err(syntax_error(pos, "malformed define")),
        }
    }

    fn expand_template_sequence(
        &mut self,
        exprs: &[Expr],
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(exprs.len());
        for expr in exprs {
            if let Expr::List(items, pos) = expr {
                if matches!(items.first().and_then(symbol_name), Some("define")) {
                    expanded.push(self.expand_template_define_in_sequence(
                        items,
                        *pos,
                        macro_def,
                        bindings,
                        repeat_index,
                        intro,
                    )?);
                    continue;
                }
            }

            expanded.push(self.expand_template(expr, macro_def, bindings, repeat_index, intro)?);
        }
        Ok(expanded)
    }

    fn expand_template_bindings(
        &mut self,
        raw_bindings: &[Expr],
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        value_intro: &IntroEnv,
        body_intro: &mut IntroEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(raw_bindings.len());
        for binding in raw_bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };
            let value =
                self.expand_template(value_expr, macro_def, bindings, repeat_index, value_intro)?;
            let name = self.bind_template_identifier(
                name_expr,
                macro_def,
                bindings,
                repeat_index,
                body_intro,
            )?;
            expanded.push(Expr::List(vec![name, value], *binding_pos));
        }
        Ok(expanded)
    }

    fn expand_template_recursive_bindings(
        &mut self,
        raw_bindings: &[Expr],
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut parsed = Vec::with_capacity(raw_bindings.len());
        for binding in raw_bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };

            let name =
                self.bind_template_identifier(name_expr, macro_def, bindings, repeat_index, intro)?;
            parsed.push((name, value_expr.clone(), *binding_pos));
        }

        parsed
            .into_iter()
            .map(|(name, value_expr, binding_pos)| {
                Ok(Expr::List(
                    vec![
                        name,
                        self.expand_template(
                            &value_expr,
                            macro_def,
                            bindings,
                            repeat_index,
                            intro,
                        )?,
                    ],
                    binding_pos,
                ))
            })
            .collect()
    }

    fn expand_template_recursive_star_bindings(
        &mut self,
        raw_bindings: &[Expr],
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(raw_bindings.len());
        for binding in raw_bindings {
            let Expr::List(parts, binding_pos) = binding else {
                return Err(syntax_error(binding.pos(), "let binding must be a list"));
            };
            let [name_expr, value_expr] = parts.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "let binding must contain a name and value",
                ));
            };

            let name =
                self.bind_template_identifier(name_expr, macro_def, bindings, repeat_index, intro)?;
            let value =
                self.expand_template(value_expr, macro_def, bindings, repeat_index, intro)?;
            expanded.push(Expr::List(vec![name, value], *binding_pos));
        }
        Ok(expanded)
    }

    fn expand_template_formals_expr(
        &mut self,
        expr: &Expr,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Expr, EvalError> {
        match expr {
            Expr::Symbol(_, _) => {
                self.bind_template_identifier(expr, macro_def, bindings, repeat_index, intro)
            }
            Expr::List(items, pos) => Ok(Expr::List(
                self.expand_template_formals(items, macro_def, bindings, repeat_index, intro)?,
                *pos,
            )),
            _ => Err(syntax_error(
                expr.pos(),
                "lambda parameters must be a list or symbol",
            )),
        }
    }

    fn expand_template_formals(
        &mut self,
        items: &[Expr],
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Vec<Expr>, EvalError> {
        let mut expanded = Vec::with_capacity(items.len());
        for item in items {
            if matches!(symbol_name(item), Some(".")) {
                expanded.push(item.clone());
            } else {
                expanded.push(self.bind_template_identifier(
                    item,
                    macro_def,
                    bindings,
                    repeat_index,
                    intro,
                )?);
            }
        }
        Ok(expanded)
    }

    fn bind_template_identifier(
        &mut self,
        expr: &Expr,
        macro_def: &SyntaxRuleMacro,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Expr, EvalError> {
        let Expr::Symbol(name, pos) = expr else {
            return Err(syntax_error(
                expr.pos(),
                "binding identifier must be a symbol",
            ));
        };

        if let Some(internal) = intro.pattern_overrides.get(name) {
            return Ok(Expr::Symbol(internal.clone(), *pos));
        }
        if let Some(internal) = intro.introduced.get(name) {
            return Ok(Expr::Symbol(internal.clone(), *pos));
        }

        if binding_exists(bindings, name) {
            let bound_expr = lookup_binding_expr(bindings, name, repeat_index)?
                .ok_or_else(|| syntax_error(*pos, "pattern variable used outside of ellipsis"))?;
            let Expr::Symbol(bound_name, _) = bound_expr else {
                return Err(syntax_error(*pos, "binding identifier must be a symbol"));
            };
            let internal = self.fresh_internal("var", &bound_name);
            intro
                .pattern_overrides
                .insert(name.clone(), internal.clone());
            return Ok(Expr::Symbol(internal, *pos));
        }

        if macro_def.def_vars.contains_key(name) || macro_def.def_macros.contains_key(name) {
            return Ok(self.expand_template_symbol(
                name,
                *pos,
                macro_def,
                bindings,
                repeat_index,
                intro,
            )?);
        }

        let internal = self.fresh_internal("var", name);
        intro.introduced.insert(name.clone(), internal.clone());
        Ok(Expr::Symbol(internal, *pos))
    }

    fn bind_var(&mut self, env: &mut ExpandEnv, name: &str) -> String {
        if is_internal_name(name) {
            return name.to_string();
        }

        if let Some(binding) = env.vars.get(name) {
            if binding.scope_id == env.scope_id {
                return binding.internal.clone();
            }
        }

        let internal = self.fresh_internal("var", name);
        env.vars.insert(
            name.to_string(),
            BindingInfo {
                internal: internal.clone(),
                scope_id: env.scope_id,
            },
        );
        internal
    }

    fn bind_macro(&mut self, env: &mut ExpandEnv, name: &str) -> String {
        if let Some(binding) = env.macros.get(name) {
            if binding.scope_id == env.scope_id {
                return binding.internal.clone();
            }
        }

        let internal = self.fresh_internal("macro", name);
        env.macros.insert(
            name.to_string(),
            MacroBinding {
                internal: internal.clone(),
                scope_id: env.scope_id,
            },
        );
        internal
    }

    fn child_env(&mut self, env: &ExpandEnv) -> ExpandEnv {
        let scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        env.child(scope_id)
    }

    fn resolve_source_symbol(&self, name: &str, env: &ExpandEnv) -> String {
        if is_internal_name(name) {
            return name.to_string();
        }
        env.vars
            .get(name)
            .map(|binding| binding.internal.clone())
            .unwrap_or_else(|| name.to_string())
    }

    fn lookup_macro_call(&self, head: &Expr, env: &ExpandEnv) -> Option<String> {
        let name = symbol_name(head)?;
        if is_internal_name(name) {
            return self.macros.contains_key(name).then(|| name.to_string());
        }
        if is_special_form_name(name) || env.vars.contains_key(name) {
            return None;
        }
        env.macros.get(name).map(|binding| binding.internal.clone())
    }

    fn fresh_internal(&mut self, kind: &str, base: &str) -> String {
        self.next_internal_id += 1;
        format!("{INTERNAL_PREFIX}{kind}:{base}:{}", self.next_internal_id)
    }
}

fn binding_exists(bindings: &MatchBindings, name: &str) -> bool {
    bindings.single.contains_key(name) || bindings.repeated.contains_key(name)
}

fn lookup_binding_expr(
    bindings: &MatchBindings,
    name: &str,
    repeat_index: Option<usize>,
) -> Result<Option<Expr>, EvalError> {
    if let Some(value) = bindings.single.get(name) {
        return Ok(Some(value.clone()));
    }

    if let Some(values) = bindings.repeated.get(name) {
        let Some(index) = repeat_index else {
            return Err(syntax_error(
                values
                    .first()
                    .map(Expr::pos)
                    .unwrap_or(SourcePos { line: 1, col: 1 }),
                "pattern variable used outside of ellipsis",
            ));
        };
        return Ok(values.get(index).cloned());
    }

    Ok(None)
}

fn repeat_count(template: &Expr, bindings: &MatchBindings) -> Result<usize, EvalError> {
    if let Expr::Symbol(name, pos) = template {
        if let Some(values) = bindings.repeated.get(name) {
            return Ok(values.len());
        }
        if bindings.single.contains_key(name) {
            return Err(syntax_error(
                *pos,
                "non-repeated pattern variable used with ellipsis",
            ));
        }
    }

    if let Expr::List(items, _) = template {
        for item in items {
            if is_ellipsis(item) {
                continue;
            }
            if let Ok(count) = repeat_count(item, bindings) {
                if count > 0 {
                    return Ok(count);
                }
            }
        }
    }

    Ok(0)
}

fn binding_list(expr: &Expr) -> Result<&[Expr], EvalError> {
    let Expr::List(bindings, _) = expr else {
        return Err(syntax_error(expr.pos(), "let bindings must be a list"));
    };
    Ok(bindings)
}

fn resolve_definition_identifier(
    name: &str,
    def_vars: &HashMap<String, String>,
    def_macros: &HashMap<String, String>,
) -> String {
    if is_internal_name(name) {
        return name.to_string();
    }
    def_vars
        .get(name)
        .cloned()
        .or_else(|| def_macros.get(name).cloned())
        .unwrap_or_else(|| name.to_string())
}

fn resolve_call_identifier(
    name: &str,
    env: &ExpandEnv,
    macros: &HashMap<String, SyntaxRuleMacro>,
) -> String {
    if is_internal_name(name) {
        return name.to_string();
    }
    env.vars
        .get(name)
        .map(|binding| binding.internal.clone())
        .or_else(|| env.macros.get(name).map(|binding| binding.internal.clone()))
        .or_else(|| macros.contains_key(name).then(|| name.to_string()))
        .unwrap_or_else(|| name.to_string())
}

fn min_required_pattern_items(patterns: &[Expr]) -> usize {
    let mut count = 0;
    let mut index = 0;
    while index < patterns.len() {
        if index + 1 < patterns.len() && is_ellipsis(&patterns[index + 1]) {
            index += 2;
        } else {
            count += 1;
            index += 1;
        }
    }
    count
}

fn list_with_head(head: &str, pos: SourcePos, body: Vec<Expr>) -> Expr {
    let mut items = Vec::with_capacity(body.len() + 1);
    items.push(Expr::Symbol(head.to_string(), pos));
    items.extend(body);
    Expr::List(items, pos)
}

fn symbol_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Symbol(name, _) => Some(name),
        _ => None,
    }
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(symbol_name(expr), Some("..."))
}

fn is_internal_name(name: &str) -> bool {
    name.starts_with(INTERNAL_PREFIX)
}

fn is_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "define"
            | "define-syntax"
            | "set!"
            | "if"
            | "quote"
            | "lambda"
            | "and"
            | "or"
            | "let"
            | "letrec"
            | "letrec*"
            | "begin"
            | "cond"
            | "case"
            | "guard"
    )
}

fn expr_pos(exprs: &[Expr], default: SourcePos) -> SourcePos {
    exprs.first().map(Expr::pos).unwrap_or(default)
}

fn expr_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Int(a, _), Expr::Int(b, _)) => a == b,
        (Expr::Bool(a, _), Expr::Bool(b, _)) => a == b,
        (Expr::String(a, _), Expr::String(b, _)) => a == b,
        (Expr::Char(a, _), Expr::Char(b, _)) => a == b,
        (Expr::Symbol(a, _), Expr::Symbol(b, _)) => a == b,
        (Expr::List(a, _), Expr::List(b, _)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b.iter())
                    .all(|(left, right)| expr_eq(left, right))
        }
        _ => false,
    }
}
