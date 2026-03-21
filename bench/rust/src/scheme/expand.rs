use super::{
    default_env, eval_expr, internal_builtin_name, invalid_argument, not_callable, quote_expr,
    syntax_error, type_error, unbound_variable, wrong_arity, EvalError, Expr, SourcePos, Value,
    BUILTIN_BINDINGS,
};
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

#[derive(Clone)]
enum MacroDef {
    SyntaxRules(SyntaxRuleMacro),
    SyntaxCase(SyntaxCaseMacro),
}

#[derive(Clone)]
struct SyntaxCaseMacro {
    surface_name: String,
    input_name: String,
    literals: HashSet<String>,
    clauses: Vec<SyntaxCaseClause>,
    def_vars: HashMap<String, String>,
    def_macros: HashMap<String, String>,
}

#[derive(Clone)]
struct SyntaxCaseClause {
    pattern: Expr,
    fender: Option<Expr>,
    output: Expr,
}

struct MacroContext<'a> {
    surface_name: &'a str,
    literals: &'a HashSet<String>,
    def_vars: &'a HashMap<String, String>,
    def_macros: &'a HashMap<String, String>,
}

#[derive(Clone)]
struct RecordFieldSpec {
    name: String,
    accessor_name: String,
    mutator_name: Option<String>,
    pos: SourcePos,
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

#[derive(Clone)]
enum TransformerValue {
    Datum(Value),
    Syntax(Expr),
}

impl TransformerValue {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Datum(value) => value.type_name(),
            Self::Syntax(_) => "syntax",
        }
    }
}

#[derive(Clone, Default)]
struct TransformerEnv {
    values: HashMap<String, TransformerValue>,
}

struct Expander {
    next_scope_id: usize,
    next_internal_id: usize,
    macros: HashMap<String, MacroDef>,
}

impl SyntaxRuleMacro {
    fn context(&self) -> MacroContext<'_> {
        MacroContext {
            surface_name: &self.surface_name,
            literals: &self.literals,
            def_vars: &self.def_vars,
            def_macros: &self.def_macros,
        }
    }
}

impl SyntaxCaseMacro {
    fn context(&self) -> MacroContext<'_> {
        MacroContext {
            surface_name: &self.surface_name,
            literals: &self.literals,
            def_vars: &self.def_vars,
            def_macros: &self.def_macros,
        }
    }
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
            Some("define") => {
                let expanded = self.expand_define(&items[1..], *pos, env)?;
                self.record_sequence_define_alias(&items[1..], env);
                Ok(Some(expanded))
            }
            Some("define-record-type") => {
                Ok(Some(self.expand_define_record_type(&items[1..], *pos, env)?))
            }
            Some("begin") => {
                let body = self.expand_sequence(&items[1..], env)?;
                Ok(Some(list_with_head("begin", *pos, body)))
            }
            _ => {
                if let Some(macro_id) = self.lookup_macro_call(head, env) {
                    let expanded = self.expand_macro_output(items, *pos, &macro_id, env)?;
                    return self.expand_sequence_expr(&expanded, env);
                }

                let expanded = self.expand_expr(expr, env)?;
                if let Expr::List(items, pos) = &expanded {
                    match items.first().and_then(symbol_name) {
                        Some("define-syntax") => {
                            self.expand_define_syntax(&items[1..], *pos, env)?;
                            return Ok(None);
                        }
                        Some("define") => {
                            let expanded = self.expand_define(&items[1..], *pos, env)?;
                            self.record_sequence_define_alias(&items[1..], env);
                            return Ok(Some(expanded));
                        }
                        Some("define-record-type") => {
                            return Ok(Some(
                                self.expand_define_record_type(&items[1..], *pos, env)?,
                            ));
                        }
                        Some("begin") => {
                            let body = self.expand_sequence(&items[1..], env)?;
                            return Ok(Some(list_with_head("begin", *pos, body)));
                        }
                        _ => {}
                    }
                }
                Ok(Some(expanded))
            }
        }
    }

    fn expand_expr(&mut self, expr: &Expr, env: &ExpandEnv) -> Result<Expr, EvalError> {
        match expr {
            Expr::Int(_, _)
            | Expr::Rational(_, _)
            | Expr::Float(_, _)
            | Expr::Bool(_, _)
            | Expr::String(_, _)
            | Expr::Char(_, _) => {
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
                    Some("define-record-type") => {
                        let mut local = env.clone();
                        self.expand_define_record_type(&items[1..], *pos, &mut local)
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

        let macro_def = self.parse_transformer(name, &internal_name, transformer, &def_vars, &def_macros)?;
        self.macros.insert(internal_name, macro_def);
        Ok(())
    }

    fn expand_define_record_type(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        env: &mut ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let Some((type_name_expr, rest)) = parts.split_first() else {
            return Err(syntax_error(
                pos,
                "define-record-type requires a type, constructor, and predicate",
            ));
        };
        let Some((constructor_expr, rest)) = rest.split_first() else {
            return Err(syntax_error(
                pos,
                "define-record-type requires a constructor and predicate",
            ));
        };
        let Some((predicate_expr, field_exprs)) = rest.split_first() else {
            return Err(syntax_error(
                pos,
                "define-record-type requires a predicate",
            ));
        };

        let Expr::Symbol(type_name, type_pos) = type_name_expr else {
            return Err(syntax_error(
                type_name_expr.pos(),
                "record type name must be a symbol",
            ));
        };
        let Expr::List(constructor_items, constructor_pos) = constructor_expr else {
            return Err(syntax_error(
                constructor_expr.pos(),
                "record constructor spec must be a list",
            ));
        };
        let Some((constructor_name_expr, constructor_field_exprs)) = constructor_items.split_first()
        else {
            return Err(syntax_error(
                constructor_expr.pos(),
                "record constructor spec cannot be empty",
            ));
        };
        let Expr::Symbol(constructor_name, _) = constructor_name_expr else {
            return Err(syntax_error(
                constructor_name_expr.pos(),
                "record constructor name must be a symbol",
            ));
        };
        let Expr::Symbol(predicate_name, _) = predicate_expr else {
            return Err(syntax_error(
                predicate_expr.pos(),
                "record predicate name must be a symbol",
            ));
        };

        let mut constructor_fields = Vec::with_capacity(constructor_field_exprs.len());
        let mut constructor_params = HashMap::new();
        for field_expr in constructor_field_exprs {
            let Expr::Symbol(field_name, field_pos) = field_expr else {
                return Err(syntax_error(
                    field_expr.pos(),
                    "record constructor fields must be symbols",
                ));
            };
            let internal = self.fresh_internal("record-arg", field_name);
            if constructor_params
                .insert(field_name.clone(), internal.clone())
                .is_some()
            {
                return Err(syntax_error(
                    *field_pos,
                    format!("duplicate constructor field {field_name}"),
                ));
            }
            constructor_fields.push(internal);
        }

        let mut fields = Vec::with_capacity(field_exprs.len());
        let mut seen_fields = HashSet::new();
        for field_expr in field_exprs {
            let Expr::List(field_items, field_pos) = field_expr else {
                return Err(syntax_error(
                    field_expr.pos(),
                    "record field spec must be a list",
                ));
            };

            let parsed = match field_items.as_slice() {
                [Expr::Symbol(field_name, _), Expr::Symbol(accessor_name, _)] => RecordFieldSpec {
                    name: field_name.clone(),
                    accessor_name: accessor_name.clone(),
                    mutator_name: None,
                    pos: *field_pos,
                },
                [
                    Expr::Symbol(field_name, _),
                    Expr::Symbol(accessor_name, _),
                    Expr::Symbol(mutator_name, _),
                ] => RecordFieldSpec {
                    name: field_name.clone(),
                    accessor_name: accessor_name.clone(),
                    mutator_name: Some(mutator_name.clone()),
                    pos: *field_pos,
                },
                _ => {
                    return Err(syntax_error(
                        field_expr.pos(),
                        "record field spec must be (field accessor) or (field accessor mutator)",
                    ))
                }
            };

            if !seen_fields.insert(parsed.name.clone()) {
                return Err(syntax_error(
                    parsed.pos,
                    format!("duplicate record field {}", parsed.name),
                ));
            }
            if !constructor_params.contains_key(&parsed.name) {
                return Err(syntax_error(
                    parsed.pos,
                    format!("constructor missing field {}", parsed.name),
                ));
            }
            fields.push(parsed);
        }

        let constructor_internal = self.bind_var(env, constructor_name);
        let predicate_internal = self.bind_var(env, predicate_name);
        let mut field_bindings = Vec::with_capacity(fields.len());
        for field in &fields {
            let accessor_internal = self.bind_var(env, &field.accessor_name);
            let mutator_internal = field
                .mutator_name
                .as_ref()
                .map(|name| self.bind_var(env, name));
            field_bindings.push((accessor_internal, mutator_internal));
        }

        let hidden_tag = self.fresh_internal("record-tag", type_name);

        let tag_value = list_expr(
            vec![
                builtin_symbol_expr("vector", pos),
                quote_symbol_expr(type_name, *type_pos),
            ],
            pos,
        );

        let mut constructor_body_items = Vec::with_capacity(fields.len() + 2);
        constructor_body_items.push(builtin_symbol_expr("vector", pos));
        constructor_body_items.push(symbol_expr(hidden_tag.clone(), pos));
        for field in &fields {
            constructor_body_items.push(symbol_expr(
                constructor_params
                    .get(&field.name)
                    .expect("field existence checked above")
                    .clone(),
                pos,
            ));
        }
        let constructor_body = list_expr(constructor_body_items, pos);

        let record_value_param = self.fresh_internal("record-value", type_name);
        let record_value_expr = symbol_expr(record_value_param.clone(), pos);
        let vector_ref_zero = list_expr(
            vec![
                builtin_symbol_expr("vector-ref", pos),
                record_value_expr.clone(),
                Expr::Int(0, pos),
            ],
            pos,
        );
        let vector_length = list_expr(
            vec![
                builtin_symbol_expr("vector-length", pos),
                record_value_expr.clone(),
            ],
            pos,
        );
        let predicate_body = list_expr(
            vec![
                symbol_expr("and", pos),
                list_expr(
                    vec![
                        builtin_symbol_expr("vector?", pos),
                        record_value_expr.clone(),
                    ],
                    pos,
                ),
                list_expr(
                    vec![
                        builtin_symbol_expr("=", pos),
                        vector_length,
                        Expr::Int(fields.len() as i64 + 1, pos),
                    ],
                    pos,
                ),
                list_expr(
                    vec![
                        builtin_symbol_expr("eq?", pos),
                        vector_ref_zero,
                        symbol_expr(hidden_tag.clone(), pos),
                    ],
                    pos,
                ),
            ],
            pos,
        );

        let mut definitions = Vec::with_capacity(3 + fields.len() * 2);
        definitions.push(define_value_expr(hidden_tag, tag_value, pos));
        definitions.push(define_function_expr(
            constructor_internal,
            constructor_fields,
            constructor_body,
            *constructor_pos,
        ));
        definitions.push(define_function_expr(
            predicate_internal,
            vec![record_value_param],
            predicate_body,
            pos,
        ));

        for (index, (field, (accessor_internal, mutator_internal))) in
            fields.iter().zip(field_bindings.into_iter()).enumerate()
        {
            let accessor_record = self.fresh_internal("record-access", &field.name);
            let accessor_body = list_expr(
                vec![
                    builtin_symbol_expr("vector-ref", field.pos),
                    symbol_expr(accessor_record.clone(), field.pos),
                    Expr::Int(index as i64 + 1, field.pos),
                ],
                field.pos,
            );
            definitions.push(define_function_expr(
                accessor_internal,
                vec![accessor_record],
                accessor_body,
                field.pos,
            ));

            if let Some(mutator_internal) = mutator_internal {
                let mutator_record = self.fresh_internal("record-set", &field.name);
                let mutator_value = self.fresh_internal("record-value", &field.name);
                let mutator_body = list_expr(
                    vec![
                        builtin_symbol_expr("vector-set!", field.pos),
                        symbol_expr(mutator_record.clone(), field.pos),
                        Expr::Int(index as i64 + 1, field.pos),
                        symbol_expr(mutator_value.clone(), field.pos),
                    ],
                    field.pos,
                );
                definitions.push(define_function_expr(
                    mutator_internal,
                    vec![mutator_record, mutator_value],
                    mutator_body,
                    field.pos,
                ));
            }
        }

        Ok(list_with_head("begin", pos, definitions))
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

    fn parse_transformer(
        &self,
        surface_name: &str,
        internal_name: &str,
        transformer: &Expr,
        def_vars: &HashMap<String, String>,
        def_macros: &HashMap<String, String>,
    ) -> Result<MacroDef, EvalError> {
        let Expr::List(items, _) = transformer else {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be syntax-rules or lambda",
            ));
        };

        match items.first().and_then(symbol_name) {
            Some("syntax-rules") => Ok(MacroDef::SyntaxRules(self.parse_syntax_rules(
                surface_name,
                internal_name,
                transformer,
                def_vars,
                def_macros,
            )?)),
            Some("lambda") => Ok(MacroDef::SyntaxCase(self.parse_syntax_case(
                surface_name,
                transformer,
                def_vars,
                def_macros,
            )?)),
            _ => Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be syntax-rules or lambda",
            )),
        }
    }

    fn parse_syntax_case(
        &self,
        surface_name: &str,
        transformer: &Expr,
        def_vars: &HashMap<String, String>,
        def_macros: &HashMap<String, String>,
    ) -> Result<SyntaxCaseMacro, EvalError> {
        let Expr::List(items, _) = transformer else {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be lambda",
            ));
        };

        let [Expr::Symbol(head, _), formals_expr, body_expr] = items.as_slice() else {
            return Err(syntax_error(
                transformer.pos(),
                "syntax-case transformer must be a single-argument lambda",
            ));
        };
        if head != "lambda" {
            return Err(syntax_error(
                transformer.pos(),
                "define-syntax transformer must be lambda",
            ));
        }

        let Expr::List(formals, _) = formals_expr else {
            return Err(syntax_error(
                formals_expr.pos(),
                "syntax-case transformer parameters must be a list",
            ));
        };
        let [Expr::Symbol(input_name, _)] = formals.as_slice() else {
            return Err(syntax_error(
                formals_expr.pos(),
                "syntax-case transformer must accept exactly one parameter",
            ));
        };

        let Expr::List(body_items, body_pos) = body_expr else {
            return Err(syntax_error(
                body_expr.pos(),
                "syntax-case transformer body must be syntax-case",
            ));
        };
        let Some(Expr::Symbol(body_head, _)) = body_items.first() else {
            return Err(syntax_error(
                body_expr.pos(),
                "syntax-case transformer body must be syntax-case",
            ));
        };
        if body_head != "syntax-case" {
            return Err(syntax_error(
                body_expr.pos(),
                "syntax-case transformer body must be syntax-case",
            ));
        }

        let [stx_expr, literal_expr, clauses @ ..] = &body_items[1..] else {
            return Err(syntax_error(
                *body_pos,
                "syntax-case requires an input, literals, and clauses",
            ));
        };
        if !matches!(stx_expr, Expr::Symbol(name, _) if name == input_name) {
            return Err(syntax_error(
                stx_expr.pos(),
                "syntax-case input must be the transformer parameter",
            ));
        }

        let Expr::List(literal_items, _) = literal_expr else {
            return Err(syntax_error(
                literal_expr.pos(),
                "syntax-case literals must be a list",
            ));
        };

        let mut literals = HashSet::new();
        for literal in literal_items {
            let Expr::Symbol(name, _) = literal else {
                return Err(syntax_error(
                    literal.pos(),
                    "syntax-case literals must be symbols",
                ));
            };
            literals.insert(resolve_definition_identifier(name, def_vars, def_macros));
        }

        if clauses.is_empty() {
            return Err(syntax_error(
                *body_pos,
                "syntax-case requires at least one clause",
            ));
        }

        let mut parsed_clauses = Vec::with_capacity(clauses.len());
        for clause in clauses {
            let Expr::List(parts, _) = clause else {
                return Err(syntax_error(clause.pos(), "syntax-case clause must be a list"));
            };

            let parsed = match parts.as_slice() {
                [pattern, output] => SyntaxCaseClause {
                    pattern: pattern.clone(),
                    fender: None,
                    output: output.clone(),
                },
                [pattern, fender, output] => SyntaxCaseClause {
                    pattern: pattern.clone(),
                    fender: Some(fender.clone()),
                    output: output.clone(),
                },
                _ => {
                    return Err(syntax_error(
                        clause.pos(),
                        "syntax-case clause must contain a pattern, optional fender, and output",
                    ))
                }
            };

            parsed_clauses.push(parsed);
        }

        Ok(SyntaxCaseMacro {
            surface_name: surface_name.to_string(),
            input_name: input_name.clone(),
            literals,
            clauses: parsed_clauses,
            def_vars: def_vars.clone(),
            def_macros: def_macros.clone(),
        })
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
        let expanded = self.expand_macro_output(items, pos, macro_id, env)?;
        self.expand_expr(&expanded, env)
    }

    fn expand_macro_output(
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

        match macro_def {
            MacroDef::SyntaxRules(macro_def) => {
                let macro_ctx = macro_def.context();
                for rule in &macro_def.rules {
                    if let Some(bindings) =
                        self.match_rule(&rule.pattern, items, &macro_ctx, env)?
                    {
                        let expanded = self.expand_template(
                            &rule.template,
                            &macro_ctx,
                            &bindings,
                            None,
                            &IntroEnv::default(),
                        )?;
                        return Ok(expanded);
                    }
                }

                Err(syntax_error(
                    pos,
                    format!(
                        "no matching syntax-rules clause for {}",
                        macro_ctx.surface_name
                    ),
                ))
            }
            MacroDef::SyntaxCase(macro_def) => {
                self.expand_syntax_case_output(items, pos, &macro_def, env)
            }
        }
    }

    fn expand_syntax_case_output(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_def: &SyntaxCaseMacro,
        env: &ExpandEnv,
    ) -> Result<Expr, EvalError> {
        let macro_ctx = macro_def.context();
        let mut transformer_env = TransformerEnv::default();
        transformer_env.values.insert(
            macro_def.input_name.clone(),
            TransformerValue::Syntax(Expr::List(items.to_vec(), pos)),
        );

        for clause in &macro_def.clauses {
            let Some(bindings) = self.match_rule(&clause.pattern, items, &macro_ctx, env)? else {
                continue;
            };

            if let Some(fender) = &clause.fender {
                let value =
                    self.eval_transformer_expr(fender, &macro_ctx, &bindings, &transformer_env)?;
                if !transformer_truthy(&value) {
                    continue;
                }
            }

            let value =
                self.eval_transformer_expr(&clause.output, &macro_ctx, &bindings, &transformer_env)?;
            let TransformerValue::Syntax(expanded) = value else {
                return Err(type_error(
                    clause.output.pos(),
                    "syntax",
                    value.type_name(),
                ));
            };
            return Ok(expanded);
        }

        Err(syntax_error(
            pos,
            format!(
                "no matching syntax-case clause for {}",
                macro_def.surface_name
            ),
        ))
    }

    fn eval_transformer_expr(
        &mut self,
        expr: &Expr,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        match expr {
            Expr::Int(value, _) => Ok(TransformerValue::Datum(Value::Int(*value))),
            Expr::Rational(value, _) => Ok(TransformerValue::Datum(Value::Rational(*value))),
            Expr::Float(value, _) => Ok(TransformerValue::Datum(Value::Float(*value))),
            Expr::Bool(value, _) => Ok(TransformerValue::Datum(Value::Bool(*value))),
            Expr::String(value, _) => Ok(TransformerValue::Datum(Value::String(
                super::SchemeString::new(value.clone()),
            ))),
            Expr::Char(value, _) => Ok(TransformerValue::Datum(Value::Char(*value))),
            Expr::Symbol(name, pos) => self
                .lookup_transformer_value(name, *pos, bindings, env)?
                .ok_or_else(|| unbound_variable(*pos, name)),
            Expr::List(items, pos) => {
                let Some(head) = items.first() else {
                    return Err(syntax_error(*pos, "cannot evaluate empty list"));
                };

                match symbol_name(head) {
                    Some("quote") => self.eval_transformer_quote(&items[1..], *pos),
                    Some("syntax") => {
                        self.eval_transformer_syntax(&items[1..], *pos, macro_ctx, bindings, env)
                    }
                    Some("with-syntax") => self.eval_transformer_with_syntax(
                        &items[1..],
                        *pos,
                        macro_ctx,
                        bindings,
                        env,
                    ),
                    Some("begin") => {
                        self.eval_transformer_sequence(&items[1..], macro_ctx, bindings, env)
                    }
                    Some("if") => {
                        self.eval_transformer_if(&items[1..], *pos, macro_ctx, bindings, env)
                    }
                    Some("and") => self.eval_transformer_and(&items[1..], macro_ctx, bindings, env),
                    Some("or") => self.eval_transformer_or(&items[1..], macro_ctx, bindings, env),
                    Some("let") => self.eval_transformer_let(
                        &items[1..],
                        *pos,
                        macro_ctx,
                        bindings,
                        env,
                        false,
                    ),
                    Some("let*") => self.eval_transformer_let(
                        &items[1..],
                        *pos,
                        macro_ctx,
                        bindings,
                        env,
                        true,
                    ),
                    _ => self.eval_transformer_application(items, *pos, macro_ctx, bindings, env),
                }
            }
        }
    }

    fn eval_transformer_sequence(
        &mut self,
        exprs: &[Expr],
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        let mut value = TransformerValue::Datum(Value::Void);
        for expr in exprs {
            value = self.eval_transformer_expr(expr, macro_ctx, bindings, env)?;
        }
        Ok(value)
    }

    fn eval_transformer_quote(
        &self,
        parts: &[Expr],
        pos: SourcePos,
    ) -> Result<TransformerValue, EvalError> {
        let [datum] = parts else {
            return Err(wrong_arity(pos, "quote", "exactly 1", parts.len()));
        };
        Ok(TransformerValue::Datum(quote_expr(datum)))
    }

    fn eval_transformer_syntax(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        let [template] = parts else {
            return Err(wrong_arity(pos, "syntax", "exactly 1", parts.len()));
        };

        let mut template_bindings = bindings.clone();
        for (name, value) in &env.values {
            if let TransformerValue::Syntax(expr) = value {
                template_bindings.single.insert(name.clone(), expr.clone());
            }
        }

        Ok(TransformerValue::Syntax(self.expand_template(
            template,
            macro_ctx,
            &template_bindings,
            None,
            &IntroEnv::default(),
        )?))
    }

    fn eval_transformer_with_syntax(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        let [binding_expr, body @ ..] = parts else {
            return Err(syntax_error(pos, "with-syntax requires bindings and a body"));
        };

        let Expr::List(raw_bindings, _) = binding_expr else {
            return Err(syntax_error(
                binding_expr.pos(),
                "with-syntax bindings must be a list",
            ));
        };

        let mut evaluated = Vec::with_capacity(raw_bindings.len());
        for binding in raw_bindings {
            let Expr::List(items, _) = binding else {
                return Err(syntax_error(
                    binding.pos(),
                    "with-syntax binding must be a list",
                ));
            };
            let [name_expr, value_expr] = items.as_slice() else {
                return Err(syntax_error(
                    binding.pos(),
                    "with-syntax binding must contain a name and value",
                ));
            };
            let Expr::Symbol(name, _) = name_expr else {
                return Err(syntax_error(
                    name_expr.pos(),
                    "with-syntax binding name must be a symbol",
                ));
            };

            let value = self.eval_transformer_expr(value_expr, macro_ctx, bindings, env)?;
            let TransformerValue::Syntax(syntax) = value else {
                return Err(type_error(
                    value_expr.pos(),
                    "syntax",
                    value.type_name(),
                ));
            };
            evaluated.push((name.clone(), syntax));
        }

        let mut next_env = env.clone();
        let mut next_bindings = bindings.clone();
        for (name, syntax) in evaluated {
            next_env
                .values
                .insert(name.clone(), TransformerValue::Syntax(syntax.clone()));
            next_bindings.single.insert(name, syntax);
        }

        self.eval_transformer_sequence(body, macro_ctx, &next_bindings, &next_env)
    }

    fn eval_transformer_if(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        match parts {
            [test, consequent] => {
                let test_value = self.eval_transformer_expr(test, macro_ctx, bindings, env)?;
                if transformer_truthy(&test_value) {
                    self.eval_transformer_expr(consequent, macro_ctx, bindings, env)
                } else {
                    Ok(TransformerValue::Datum(Value::Void))
                }
            }
            [test, consequent, alternate] => {
                let test_value = self.eval_transformer_expr(test, macro_ctx, bindings, env)?;
                if transformer_truthy(&test_value) {
                    self.eval_transformer_expr(consequent, macro_ctx, bindings, env)
                } else {
                    self.eval_transformer_expr(alternate, macro_ctx, bindings, env)
                }
            }
            _ => Err(syntax_error(pos, "if requires 2 or 3 arguments")),
        }
    }

    fn eval_transformer_and(
        &mut self,
        exprs: &[Expr],
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        let mut value = TransformerValue::Datum(Value::Bool(true));
        for expr in exprs {
            value = self.eval_transformer_expr(expr, macro_ctx, bindings, env)?;
            if !transformer_truthy(&value) {
                return Ok(value);
            }
        }
        Ok(value)
    }

    fn eval_transformer_or(
        &mut self,
        exprs: &[Expr],
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        for expr in exprs {
            let value = self.eval_transformer_expr(expr, macro_ctx, bindings, env)?;
            if transformer_truthy(&value) {
                return Ok(value);
            }
        }
        Ok(TransformerValue::Datum(Value::Bool(false)))
    }

    fn eval_transformer_let(
        &mut self,
        parts: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
        sequential: bool,
    ) -> Result<TransformerValue, EvalError> {
        let [binding_expr, body @ ..] = parts else {
            return Err(syntax_error(pos, "let requires bindings and a body"));
        };
        let Expr::List(raw_bindings, _) = binding_expr else {
            return Err(syntax_error(binding_expr.pos(), "let requires bindings"));
        };

        let mut next_env = env.clone();
        let mut next_bindings = bindings.clone();

        if sequential {
            for binding in raw_bindings {
                let Expr::List(items, _) = binding else {
                    return Err(syntax_error(binding.pos(), "let binding must be a list"));
                };
                let [name_expr, value_expr] = items.as_slice() else {
                    return Err(syntax_error(
                        binding.pos(),
                        "let binding must contain a name and value",
                    ));
                };
                let Expr::Symbol(name, _) = name_expr else {
                    return Err(syntax_error(
                        name_expr.pos(),
                        "let binding name must be a symbol",
                    ));
                };

                let value =
                    self.eval_transformer_expr(value_expr, macro_ctx, &next_bindings, &next_env)?;
                if let TransformerValue::Syntax(syntax) = &value {
                    next_bindings.single.insert(name.clone(), syntax.clone());
                }
                next_env.values.insert(name.clone(), value);
            }
        } else {
            let mut evaluated = Vec::with_capacity(raw_bindings.len());
            for binding in raw_bindings {
                let Expr::List(items, _) = binding else {
                    return Err(syntax_error(binding.pos(), "let binding must be a list"));
                };
                let [name_expr, value_expr] = items.as_slice() else {
                    return Err(syntax_error(
                        binding.pos(),
                        "let binding must contain a name and value",
                    ));
                };
                let Expr::Symbol(name, _) = name_expr else {
                    return Err(syntax_error(
                        name_expr.pos(),
                        "let binding name must be a symbol",
                    ));
                };

                let value = self.eval_transformer_expr(value_expr, macro_ctx, bindings, env)?;
                evaluated.push((name.clone(), value));
            }

            for (name, value) in evaluated {
                if let TransformerValue::Syntax(syntax) = &value {
                    next_bindings.single.insert(name.clone(), syntax.clone());
                }
                next_env.values.insert(name, value);
            }
        }

        self.eval_transformer_sequence(body, macro_ctx, &next_bindings, &next_env)
    }

    fn eval_transformer_application(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<TransformerValue, EvalError> {
        let Some(head) = items.first() else {
            return Err(syntax_error(pos, "cannot evaluate empty list"));
        };
        let Some(name) = symbol_name(head) else {
            return Err(syntax_error(pos, "transformer application requires a symbol operator"));
        };

        if let Some(value) = self.lookup_transformer_value(name, pos, bindings, env)? {
            return Err(not_callable(pos, value.type_name()));
        }

        let args = items[1..]
            .iter()
            .map(|expr| self.eval_transformer_expr(expr, macro_ctx, bindings, env))
            .collect::<Result<Vec<_>, _>>()?;

        match name {
            "syntax->datum" => {
                let [value] = args.as_slice() else {
                    return Err(wrong_arity(pos, "syntax->datum", "exactly 1", args.len()));
                };
                let syntax = expect_transformer_syntax(value, pos, "syntax->datum")?;
                Ok(TransformerValue::Datum(quote_expr(syntax)))
            }
            "datum->syntax" => {
                let [context, datum] = args.as_slice() else {
                    return Err(wrong_arity(pos, "datum->syntax", "exactly 2", args.len()));
                };
                let context = expect_transformer_syntax(context, pos, "datum->syntax")?;
                let datum = expect_transformer_datum(datum, pos, "datum->syntax")?;
                Ok(TransformerValue::Syntax(datum_to_expr(datum, context.pos())?))
            }
            _ => {
                let datum_args = args
                    .iter()
                    .map(|value| expect_transformer_datum(value, pos, name).cloned())
                    .collect::<Result<Vec<_>, _>>()?;

                let mut runtime_items = Vec::with_capacity(datum_args.len() + 1);
                runtime_items.push(symbol_expr(name, pos));
                for arg in &datum_args {
                    runtime_items.push(value_to_runtime_expr(arg, pos)?);
                }

                Ok(TransformerValue::Datum(eval_expr(
                    &Expr::List(runtime_items, pos),
                    &default_env(),
                )?))
            }
        }
    }

    fn lookup_transformer_value(
        &self,
        name: &str,
        pos: SourcePos,
        bindings: &MatchBindings,
        env: &TransformerEnv,
    ) -> Result<Option<TransformerValue>, EvalError> {
        if let Some(value) = env.values.get(name) {
            return Ok(Some(value.clone()));
        }
        if let Some(value) = bindings.single.get(name) {
            return Ok(Some(TransformerValue::Syntax(value.clone())));
        }
        if bindings.repeated.contains_key(name) {
            return Err(syntax_error(pos, "pattern variable used outside of ellipsis"));
        }
        Ok(None)
    }

    fn match_rule(
        &self,
        pattern: &Expr,
        input_items: &[Expr],
        macro_ctx: &MacroContext<'_>,
        env: &ExpandEnv,
    ) -> Result<Option<MatchBindings>, EvalError> {
        let Expr::List(pattern_items, _) = pattern else {
            return Err(syntax_error(
                pattern.pos(),
                "syntax-rules pattern must be a list",
            ));
        };

        let Some(head) = pattern_items.first() else {
            return Ok(None);
        };

        let mut literals = macro_ctx.literals.clone();
        if let Some(name) = symbol_name(head) {
            if name != "_" {
                literals.insert(resolve_definition_identifier(
                    name,
                    macro_ctx.def_vars,
                    macro_ctx.def_macros,
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
                macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
        env: &ExpandEnv,
        bindings: &mut MatchBindings,
    ) -> Result<bool, EvalError> {
        self.match_list_pattern_from(patterns, 0, inputs, 0, literals, macro_ctx, env, bindings)
    }

    fn match_list_pattern_from(
        &self,
        patterns: &[Expr],
        pattern_index: usize,
        inputs: &[Expr],
        input_index: usize,
        literals: &HashSet<String>,
        macro_ctx: &MacroContext<'_>,
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
                        macro_ctx,
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
                        macro_ctx,
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
                macro_ctx,
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
                macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
        env: &ExpandEnv,
        bindings: &mut MatchBindings,
    ) -> Result<bool, EvalError> {
        match pattern {
            Expr::Int(value, _) => Ok(matches!(input, Expr::Int(found, _) if found == value)),
            Expr::Rational(value, _) => {
                Ok(matches!(input, Expr::Rational(found, _) if found == value))
            }
            Expr::Float(value, _) => Ok(matches!(input, Expr::Float(found, _) if found == value)),
            Expr::Bool(value, _) => Ok(matches!(input, Expr::Bool(found, _) if found == value)),
            Expr::String(value, _) => Ok(matches!(input, Expr::String(found, _) if found == value)),
            Expr::Char(value, _) => Ok(matches!(input, Expr::Char(found, _) if found == value)),
            Expr::Symbol(name, _) => {
                if name == "_" {
                    return Ok(true);
                }

                let resolved =
                    resolve_definition_identifier(name, macro_ctx.def_vars, macro_ctx.def_macros);
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
                    macro_ctx,
                    env,
                    bindings,
                )
            }
        }
    }

    fn expand_template(
        &mut self,
        template: &Expr,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        match template {
            Expr::Int(_, _)
            | Expr::Rational(_, _)
            | Expr::Float(_, _)
            | Expr::Bool(_, _)
            | Expr::String(_, _)
            | Expr::Char(_, _) => {
                Ok(template.clone())
            }
            Expr::Symbol(name, pos) => {
                self.expand_template_symbol(name, *pos, macro_ctx, bindings, repeat_index, intro)
            }
            Expr::List(items, pos) => {
                let Some(head) = items.first() else {
                    return Ok(template.clone());
                };

                match symbol_name(head) {
                    Some("lambda") => self.expand_template_lambda(
                        items,
                        *pos,
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("let") => self.expand_template_let(
                        items,
                        *pos,
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("letrec") => self.expand_template_letrec(
                        items,
                        *pos,
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                        false,
                    ),
                    Some("letrec*") => self.expand_template_letrec(
                        items,
                        *pos,
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                        true,
                    ),
                    Some("case") => self.expand_template_case(
                        items,
                        *pos,
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                    ),
                    Some("define") => self.expand_template_define(
                        items,
                        *pos,
                        macro_ctx,
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
                                        macro_ctx,
                                        bindings,
                                        Some(item_index),
                                        intro,
                                    )?);
                                }
                                index += 2;
                            } else {
                                expanded.push(self.expand_template(
                                    &items[index],
                                    macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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

        if let Some(internal) = macro_ctx.def_vars.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        if let Some(internal) = macro_ctx.def_macros.get(name) {
            return Ok(Expr::Symbol(internal.clone(), pos));
        }

        Ok(Expr::Symbol(name.to_string(), pos))
    }

    fn expand_template_lambda(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
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
            macro_ctx,
            bindings,
            repeat_index,
            &mut body_intro,
        )?;
        let expanded_body = self.expand_template_sequence(
            body,
            macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
                    macro_ctx,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?;
                let expanded_bindings = self.expand_template_bindings(
                    raw_bindings,
                    macro_ctx,
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
                    macro_ctx,
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
                    macro_ctx,
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
                    macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
                macro_ctx,
                bindings,
                repeat_index,
                &mut body_intro,
            )?
        } else {
            self.expand_template_recursive_bindings(
                raw_bindings,
                macro_ctx,
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
            macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
        result.push(self.expand_template(key, macro_ctx, bindings, repeat_index, intro)?);

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
                macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &IntroEnv,
    ) -> Result<Expr, EvalError> {
        let mut local = intro.clone();
        self.expand_template_define_in_sequence(
            items,
            pos,
            macro_ctx,
            bindings,
            repeat_index,
            &mut local,
        )
    }

    fn expand_template_define_in_sequence(
        &mut self,
        items: &[Expr],
        pos: SourcePos,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Expr, EvalError> {
        match &items[1..] {
            [Expr::Symbol(name, name_pos), value_expr] => {
                let target = self.bind_template_identifier(
                    &Expr::Symbol(name.clone(), *name_pos),
                    macro_ctx,
                    bindings,
                    repeat_index,
                    intro,
                )?;
                let value =
                    self.expand_template(value_expr, macro_ctx, bindings, repeat_index, intro)?;
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
                    macro_ctx,
                    bindings,
                    repeat_index,
                    intro,
                )?;
                let mut body_intro = intro.clone();
                let expanded_formals = self.expand_template_formals(
                    formals,
                    macro_ctx,
                    bindings,
                    repeat_index,
                    &mut body_intro,
                )?;
                let expanded_body = self.expand_template_sequence(
                    body,
                    macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
                        macro_ctx,
                        bindings,
                        repeat_index,
                        intro,
                    )?);
                    continue;
                }
            }

            expanded.push(self.expand_template(expr, macro_ctx, bindings, repeat_index, intro)?);
        }
        Ok(expanded)
    }

    fn expand_template_bindings(
        &mut self,
        raw_bindings: &[Expr],
        macro_ctx: &MacroContext<'_>,
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
                self.expand_template(value_expr, macro_ctx, bindings, repeat_index, value_intro)?;
            let name = self.bind_template_identifier(
                name_expr,
                macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
                self.bind_template_identifier(name_expr, macro_ctx, bindings, repeat_index, intro)?;
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
                            macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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
                self.bind_template_identifier(name_expr, macro_ctx, bindings, repeat_index, intro)?;
            let value =
                self.expand_template(value_expr, macro_ctx, bindings, repeat_index, intro)?;
            expanded.push(Expr::List(vec![name, value], *binding_pos));
        }
        Ok(expanded)
    }

    fn expand_template_formals_expr(
        &mut self,
        expr: &Expr,
        macro_ctx: &MacroContext<'_>,
        bindings: &MatchBindings,
        repeat_index: Option<usize>,
        intro: &mut IntroEnv,
    ) -> Result<Expr, EvalError> {
        match expr {
            Expr::Symbol(_, _) => {
                self.bind_template_identifier(expr, macro_ctx, bindings, repeat_index, intro)
            }
            Expr::List(items, pos) => Ok(Expr::List(
                self.expand_template_formals(items, macro_ctx, bindings, repeat_index, intro)?,
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
        macro_ctx: &MacroContext<'_>,
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
                    macro_ctx,
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
        macro_ctx: &MacroContext<'_>,
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

        if macro_ctx.def_vars.contains_key(name) || macro_ctx.def_macros.contains_key(name) {
            return Ok(self.expand_template_symbol(
                name,
                *pos,
                macro_ctx,
                bindings,
                repeat_index,
                intro,
            )?);
        }

        let internal = self.fresh_internal("var", name);
        intro.introduced.insert(name.clone(), internal.clone());
        Ok(Expr::Symbol(internal, *pos))
    }

    fn record_sequence_define_alias(&self, parts: &[Expr], env: &mut ExpandEnv) {
        let name = match parts {
            [Expr::Symbol(name, _), _] => name.as_str(),
            [Expr::List(signature, _), ..] => match signature.first() {
                Some(Expr::Symbol(name, _)) => name.as_str(),
                _ => return,
            },
            _ => return,
        };

        let Some(surface) = internal_surface_name(name) else {
            return;
        };

        env.vars.insert(
            surface.to_string(),
            BindingInfo {
                internal: name.to_string(),
                scope_id: env.scope_id,
            },
        );
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

fn transformer_truthy(value: &TransformerValue) -> bool {
    !matches!(value, TransformerValue::Datum(Value::Bool(false)))
}

fn expect_transformer_syntax<'a>(
    value: &'a TransformerValue,
    pos: SourcePos,
    _name: &str,
) -> Result<&'a Expr, EvalError> {
    match value {
        TransformerValue::Syntax(expr) => Ok(expr),
        other => Err(type_error(pos, "syntax", other.type_name())),
    }
}

fn expect_transformer_datum<'a>(
    value: &'a TransformerValue,
    pos: SourcePos,
    _name: &str,
) -> Result<&'a Value, EvalError> {
    match value {
        TransformerValue::Datum(value) => Ok(value),
        other => Err(type_error(pos, "datum", other.type_name())),
    }
}

fn value_to_runtime_expr(value: &Value, pos: SourcePos) -> Result<Expr, EvalError> {
    match value {
        Value::Int(value) => Ok(Expr::Int(*value, pos)),
        Value::Rational(value) => Ok(Expr::Rational(*value, pos)),
        Value::Float(value) => Ok(Expr::Float(*value, pos)),
        Value::Bool(value) => Ok(Expr::Bool(*value, pos)),
        Value::String(value) => Ok(Expr::String(value.to_plain_string(), pos)),
        Value::Char(value) => Ok(Expr::Char(*value, pos)),
        _ => Ok(list_expr(
            vec![symbol_expr("quote", pos), datum_to_expr(value, pos)?],
            pos,
        )),
    }
}

fn datum_to_expr(value: &Value, pos: SourcePos) -> Result<Expr, EvalError> {
    match value {
        Value::Int(value) => Ok(Expr::Int(*value, pos)),
        Value::Rational(value) => Ok(Expr::Rational(*value, pos)),
        Value::Float(value) => Ok(Expr::Float(*value, pos)),
        Value::Bool(value) => Ok(Expr::Bool(*value, pos)),
        Value::String(value) => Ok(Expr::String(value.to_plain_string(), pos)),
        Value::Symbol(value) => Ok(Expr::Symbol(value.clone(), pos)),
        Value::Char(value) => Ok(Expr::Char(*value, pos)),
        Value::List(items) => Ok(Expr::List(
            items.iter().map(|item| datum_to_expr(item, pos)).collect::<Result<Vec<_>, _>>()?,
            pos,
        )),
        Value::Pair(_) => datum_pair_to_expr(value, pos),
        Value::Vector(_) => Err(invalid_argument(
            pos,
            "datum->syntax does not support vectors",
        )),
        other => Err(type_error(pos, "datum", other.type_name())),
    }
}

fn datum_pair_to_expr(value: &Value, pos: SourcePos) -> Result<Expr, EvalError> {
    let mut items = Vec::new();
    let mut current = value.clone();
    let mut seen_pairs = HashSet::new();

    loop {
        match current {
            Value::List(rest) => {
                for item in &rest {
                    items.push(datum_to_expr(item, pos)?);
                }
                return Ok(Expr::List(items, pos));
            }
            Value::Pair(pair) => {
                if !seen_pairs.insert(pair.addr()) {
                    return Err(invalid_argument(
                        pos,
                        "datum->syntax does not support cyclic pairs",
                    ));
                }
                items.push(datum_to_expr(&pair.car(), pos)?);
                current = pair.cdr();
            }
            other => {
                items.push(symbol_expr(".", pos));
                items.push(datum_to_expr(&other, pos)?);
                return Ok(Expr::List(items, pos));
            }
        }
    }
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
    macros: &HashMap<String, MacroDef>,
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

fn list_expr(items: Vec<Expr>, pos: SourcePos) -> Expr {
    Expr::List(items, pos)
}

fn symbol_expr(name: impl Into<String>, pos: SourcePos) -> Expr {
    Expr::Symbol(name.into(), pos)
}

fn builtin_symbol_expr(name: &str, pos: SourcePos) -> Expr {
    symbol_expr(internal_builtin_name(name), pos)
}

fn quote_symbol_expr(name: &str, pos: SourcePos) -> Expr {
    list_expr(vec![symbol_expr("quote", pos), symbol_expr(name, pos)], pos)
}

fn define_value_expr(name: impl Into<String>, value: Expr, pos: SourcePos) -> Expr {
    list_expr(
        vec![symbol_expr("define", pos), symbol_expr(name, pos), value],
        pos,
    )
}

fn define_function_expr(
    name: impl Into<String>,
    params: Vec<String>,
    body: Expr,
    pos: SourcePos,
) -> Expr {
    let mut signature = Vec::with_capacity(params.len() + 1);
    signature.push(symbol_expr(name, pos));
    signature.extend(params.into_iter().map(|param| symbol_expr(param, pos)));
    list_expr(
        vec![symbol_expr("define", pos), list_expr(signature, pos), body],
        pos,
    )
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

fn internal_surface_name(name: &str) -> Option<&str> {
    let rest = name.strip_prefix(INTERNAL_PREFIX)?;
    let (_, rest) = rest.split_once(':')?;
    Some(rest.rsplit_once(':').map_or(rest, |(base, _)| base))
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
            | "define-record-type"
    )
}

fn expr_pos(exprs: &[Expr], default: SourcePos) -> SourcePos {
    exprs.first().map(Expr::pos).unwrap_or(default)
}

fn expr_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Int(a, _), Expr::Int(b, _)) => a == b,
        (Expr::Rational(a, _), Expr::Rational(b, _)) => a == b,
        (Expr::Float(a, _), Expr::Float(b, _)) => a == b,
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
