use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

use super::EvalError;

type EvalResult<T> = Result<T, EvalError>;

#[derive(Clone, Copy, Debug, Default)]
struct Span {
    line: usize,
    col: usize,
}

#[derive(Clone, Debug)]
enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(Symbol),
    List(Vec<Expr>),
}

#[derive(Clone, Debug)]
struct Symbol {
    name: String,
    span: Span,
}

#[derive(Clone)]
enum Value {
    Int(i64),
    Bool(bool),
    Str(String),
    Symbol(String),
    Nil,
    Pair(Rc<PairValue>),
    Builtin(Builtin),
    Closure(Rc<Closure>),
    Macro(Rc<MacroProc>),
    Syntax(Syntax),
    SyntaxList(Vec<Syntax>),
    Void,
}

#[derive(Clone)]
struct PairValue {
    car: Value,
    cdr: Value,
}

#[derive(Clone, Copy)]
enum Builtin {
    Add,
    Div,
    Zero,
    Cons,
    Car,
    Cdr,
    Null,
    List,
    Apply,
    StringAppend,
    NumberToString,
    StringToSymbol,
    SyntaxToDatum,
    DatumToSyntax,
    Display,
    Write,
    Newline,
}

#[derive(Clone)]
struct Closure {
    params: ParamSpec,
    body: Vec<Expr>,
    env: Env,
}

#[derive(Clone)]
struct MacroProc {
    transformer: Rc<Closure>,
}

#[derive(Clone)]
struct Syntax {
    datum: Expr,
}

#[derive(Clone)]
struct ParamSpec {
    fixed: Vec<String>,
    rest: Option<String>,
}

#[derive(Clone)]
struct Env(Rc<EnvData>);

struct EnvData {
    parent: Option<Env>,
    bindings: RefCell<HashMap<String, Rc<RefCell<Value>>>>,
}

#[derive(Clone, Debug)]
enum TokenKind {
    LParen,
    RParen,
    Quote,
    SyntaxQuote,
    Atom(String),
    Str(String),
}

#[derive(Clone, Debug)]
struct Token {
    kind: TokenKind,
    span: Span,
}

struct Parser<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    col: usize,
    buffered: Vec<Token>,
}

pub(crate) fn eval_str_impl(input: &str) -> EvalResult<String> {
    let mut interpreter = Interpreter::default();
    let value = interpreter.eval_program(input)?;
    Ok(render_value(&value))
}

pub(crate) fn eval_str_with_output_impl(input: &str) -> EvalResult<(String, String)> {
    let mut interpreter = Interpreter::default();
    let value = interpreter.eval_program(input)?;
    Ok((render_value(&value), interpreter.output))
}

#[derive(Default)]
struct Interpreter {
    output: String,
    gensym: usize,
}

impl Interpreter {
    fn eval_program(&mut self, input: &str) -> EvalResult<Value> {
        let mut parser = Parser::new(input);
        let forms = parser.parse_program()?;
        if forms.is_empty() {
            return Err(EvalError::Message("empty program".into()));
        }

        let env = self.global_env();
        let mut last = Value::Void;
        for form in &forms {
            last = self.eval(&env, form)?;
        }
        Ok(last)
    }

    fn global_env(&self) -> Env {
        let env = Env::new();
        env.define("+", Value::Builtin(Builtin::Add));
        env.define("/", Value::Builtin(Builtin::Div));
        env.define("zero?", Value::Builtin(Builtin::Zero));
        env.define("cons", Value::Builtin(Builtin::Cons));
        env.define("car", Value::Builtin(Builtin::Car));
        env.define("cdr", Value::Builtin(Builtin::Cdr));
        env.define("null?", Value::Builtin(Builtin::Null));
        env.define("list", Value::Builtin(Builtin::List));
        env.define("apply", Value::Builtin(Builtin::Apply));
        env.define("string-append", Value::Builtin(Builtin::StringAppend));
        env.define("number->string", Value::Builtin(Builtin::NumberToString));
        env.define("string->symbol", Value::Builtin(Builtin::StringToSymbol));
        env.define("syntax->datum", Value::Builtin(Builtin::SyntaxToDatum));
        env.define("datum->syntax", Value::Builtin(Builtin::DatumToSyntax));
        env.define("display", Value::Builtin(Builtin::Display));
        env.define("write", Value::Builtin(Builtin::Write));
        env.define("newline", Value::Builtin(Builtin::Newline));
        env
    }

    fn eval(&mut self, env: &Env, expr: &Expr) -> EvalResult<Value> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Str(value) => Ok(Value::Str(value.clone())),
            Expr::Symbol(symbol) => env.get(&symbol.name).ok_or_else(|| {
                self.error_at(symbol.span, format!("unbound symbol: {}", symbol.name))
            }),
            Expr::List(items) => self.eval_list(env, items),
        }
    }

    fn eval_list(&mut self, env: &Env, items: &[Expr]) -> EvalResult<Value> {
        if items.is_empty() {
            return Err(EvalError::Message("cannot evaluate empty list".into()));
        }

        if let Expr::Symbol(symbol) = &items[0] {
            match symbol.name.as_str() {
                "define" => return self.eval_define(env, &items[1..]),
                "define-syntax" => return self.eval_define_syntax(env, &items[1..]),
                "lambda" => return self.eval_lambda(env, &items[1..]),
                "if" => return self.eval_if(env, &items[1..]),
                "begin" => return self.eval_sequence(env, &items[1..]),
                "set!" => return self.eval_set(env, &items[1..]),
                "quote" => return self.eval_quote(&items[1..]),
                "syntax" => return self.eval_syntax(env, &items[1..]),
                "syntax-case" => return self.eval_syntax_case(env, &items[1..]),
                "with-syntax" => return self.eval_with_syntax(env, &items[1..]),
                "let" => return self.eval_let(env, &items[1..]),
                _ => {}
            }

            if let Some(Value::Macro(mac)) = env.get(&symbol.name) {
                let expanded = self.expand_macro(&mac, &Expr::List(items.to_vec()))?;
                return self.eval(env, &expanded);
            }
        }

        let operator = self.eval(env, &items[0])?;
        let mut args = Vec::with_capacity(items.len().saturating_sub(1));
        for item in &items[1..] {
            args.push(self.eval(env, item)?);
        }
        self.apply(operator, args)
    }

    fn eval_define(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() < 2 {
            return Err(EvalError::Message(
                "define expects a target and value".into(),
            ));
        }

        match &forms[0] {
            Expr::Symbol(symbol) => {
                if forms.len() != 2 {
                    return Err(self.error_at(symbol.span, "define expects exactly 2 arguments"));
                }
                let value = self.eval(env, &forms[1])?;
                env.define(&symbol.name, value);
                Ok(Value::Void)
            }
            Expr::List(target) => {
                if target.is_empty() {
                    return Err(EvalError::Message(
                        "define function target cannot be empty".into(),
                    ));
                }
                let name = expect_symbol(&target[0])?;
                let params = parse_param_list(&target[1..])?;
                let closure = Closure {
                    params,
                    body: forms[1..].to_vec(),
                    env: env.clone(),
                };
                env.define(&name.name, Value::Closure(Rc::new(closure)));
                Ok(Value::Void)
            }
            _ => Err(EvalError::Message(
                "define target must be a symbol or function signature".into(),
            )),
        }
    }

    fn eval_define_syntax(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() != 2 {
            return Err(EvalError::Message(
                "define-syntax expects exactly 2 arguments".into(),
            ));
        }

        let name = expect_symbol(&forms[0])?;
        let transformer = self.eval(env, &forms[1])?;
        let closure = match transformer {
            Value::Closure(closure) => closure,
            _ => {
                return Err(
                    self.error_at(name.span, "define-syntax requires a transformer procedure")
                )
            }
        };
        env.define(
            &name.name,
            Value::Macro(Rc::new(MacroProc {
                transformer: closure,
            })),
        );
        Ok(Value::Void)
    }

    fn eval_lambda(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() < 2 {
            return Err(EvalError::Message(
                "lambda expects parameters and a body".into(),
            ));
        }

        let params = parse_lambda_params(&forms[0])?;
        Ok(Value::Closure(Rc::new(Closure {
            params,
            body: forms[1..].to_vec(),
            env: env.clone(),
        })))
    }

    fn eval_if(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() != 2 && forms.len() != 3 {
            return Err(EvalError::Message("if expects 2 or 3 arguments".into()));
        }

        let condition = self.eval(env, &forms[0])?;
        if is_truthy(&condition) {
            self.eval(env, &forms[1])
        } else if forms.len() == 3 {
            self.eval(env, &forms[2])
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_sequence(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        let mut last = Value::Void;
        for form in forms {
            last = self.eval(env, form)?;
        }
        Ok(last)
    }

    fn eval_set(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() != 2 {
            return Err(EvalError::Message(
                "set! expects exactly 2 arguments".into(),
            ));
        }

        let target = expect_symbol(&forms[0])?;
        let value = self.eval(env, &forms[1])?;
        env.set(&target.name, value)
            .map_err(|_| self.error_at(target.span, format!("unbound symbol: {}", target.name)))?;
        Ok(Value::Void)
    }

    fn eval_quote(&mut self, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() != 1 {
            return Err(EvalError::Message(
                "quote expects exactly 1 argument".into(),
            ));
        }
        Ok(expr_to_datum(&forms[0]))
    }

    fn eval_syntax(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() != 1 {
            return Err(EvalError::Message(
                "syntax expects exactly 1 argument".into(),
            ));
        }
        Ok(Value::Syntax(Syntax {
            datum: self.expand_syntax_template(env, &forms[0], None, &HashMap::new())?,
        }))
    }

    fn eval_with_syntax(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() < 2 {
            return Err(EvalError::Message(
                "with-syntax expects bindings and a body".into(),
            ));
        }

        let bindings = match &forms[0] {
            Expr::List(items) => items,
            _ => {
                return Err(EvalError::Message(
                    "with-syntax bindings must be a list".into(),
                ))
            }
        };

        let child = env.child();
        for binding in bindings {
            let pair = match binding {
                Expr::List(items) if items.len() == 2 => items,
                _ => {
                    return Err(EvalError::Message(
                        "with-syntax bindings must have the form (name value)".into(),
                    ))
                }
            };

            let name = expect_symbol(&pair[0])?;
            let value = self.eval(env, &pair[1])?;
            match value {
                Value::Syntax(_) | Value::SyntaxList(_) => child.define(&name.name, value),
                _ => {
                    return Err(
                        self.error_at(name.span, "with-syntax values must be syntax objects")
                    )
                }
            }
        }

        self.eval_sequence(&child, &forms[1..])
    }

    fn eval_syntax_case(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() < 3 {
            return Err(EvalError::Message(
                "syntax-case expects a value, literals, and at least one clause".into(),
            ));
        }

        let target = match self.eval(env, &forms[0])? {
            Value::Syntax(syntax) => syntax,
            _ => {
                return Err(EvalError::Message(
                    "syntax-case expects a syntax object".into(),
                ))
            }
        };

        let literals = match &forms[1] {
            Expr::List(items) => {
                let mut set = HashSet::new();
                for item in items {
                    let symbol = expect_symbol(item)?;
                    set.insert(symbol.name.clone());
                }
                set
            }
            _ => {
                return Err(EvalError::Message(
                    "syntax-case literals must be a list".into(),
                ))
            }
        };

        for clause in &forms[2..] {
            let parts = match clause {
                Expr::List(items) if items.len() == 2 || items.len() == 3 => items,
                _ => {
                    return Err(EvalError::Message(
                        "syntax-case clauses must have the form (pattern template) or (pattern guard template)".into(),
                    ))
                }
            };

            let mut bindings = HashMap::new();
            if !match_pattern(&parts[0], &target.datum, &literals, &mut bindings)? {
                continue;
            }

            let clause_env = env.child();
            for (name, binding) in bindings {
                match binding {
                    PatternBinding::Single(syntax) => {
                        clause_env.define(&name, Value::Syntax(syntax));
                    }
                    PatternBinding::Repeated(values) => {
                        clause_env.define(&name, Value::SyntaxList(values));
                    }
                }
            }

            if parts.len() == 3 {
                let guard = self.eval(&clause_env, &parts[1])?;
                if !is_truthy(&guard) {
                    continue;
                }
                return self.eval(&clause_env, &parts[2]);
            }

            return self.eval(&clause_env, &parts[1]);
        }

        Err(EvalError::Message(
            "syntax-case found no matching clause".into(),
        ))
    }

    fn eval_let(&mut self, env: &Env, forms: &[Expr]) -> EvalResult<Value> {
        if forms.len() < 2 {
            return Err(EvalError::Message("let expects bindings and a body".into()));
        }

        let bindings = match &forms[0] {
            Expr::List(items) => items,
            _ => return Err(EvalError::Message("let bindings must be a list".into())),
        };

        let mut evaluated = Vec::with_capacity(bindings.len());
        for binding in bindings {
            let pair = match binding {
                Expr::List(items) if items.len() == 2 => items,
                _ => {
                    return Err(EvalError::Message(
                        "let bindings must have the form (name value)".into(),
                    ))
                }
            };

            let name = expect_symbol(&pair[0])?;
            evaluated.push((name.name.clone(), self.eval(env, &pair[1])?));
        }

        let child = env.child();
        for (name, value) in evaluated {
            child.define(&name, value);
        }
        self.eval_sequence(&child, &forms[1..])
    }

    fn expand_macro(&mut self, mac: &MacroProc, call: &Expr) -> EvalResult<Expr> {
        let result = self.apply(
            Value::Closure(mac.transformer.clone()),
            vec![Value::Syntax(Syntax {
                datum: call.clone(),
            })],
        )?;
        match result {
            Value::Syntax(syntax) => Ok(syntax.datum),
            _ => Err(EvalError::Message(
                "macro transformer must return a syntax object".into(),
            )),
        }
    }

    fn apply(&mut self, proc: Value, args: Vec<Value>) -> EvalResult<Value> {
        match proc {
            Value::Builtin(builtin) => self.apply_builtin(builtin, args),
            Value::Closure(closure) => self.apply_closure(&closure, args),
            _ => Err(EvalError::Message(
                "first list element is not a procedure".into(),
            )),
        }
    }

    fn apply_closure(&mut self, closure: &Closure, args: Vec<Value>) -> EvalResult<Value> {
        if args.len() < closure.params.fixed.len() {
            return Err(EvalError::Message(format!(
                "expected at least {} arguments, got {}",
                closure.params.fixed.len(),
                args.len()
            )));
        }

        if closure.params.rest.is_none() && args.len() != closure.params.fixed.len() {
            return Err(EvalError::Message(format!(
                "expected {} arguments, got {}",
                closure.params.fixed.len(),
                args.len()
            )));
        }

        let call_env = closure.env.child();
        for (name, value) in closure.params.fixed.iter().zip(args.iter()) {
            call_env.define(name, value.clone());
        }

        if let Some(rest_name) = &closure.params.rest {
            let rest = list_from_slice(&args[closure.params.fixed.len()..]);
            call_env.define(rest_name, rest);
        }

        self.eval_sequence(&call_env, &closure.body)
    }

    fn apply_builtin(&mut self, builtin: Builtin, args: Vec<Value>) -> EvalResult<Value> {
        match builtin {
            Builtin::Add => {
                let mut total = 0_i64;
                for arg in args {
                    total += expect_int_value(arg, "+")?;
                }
                Ok(Value::Int(total))
            }
            Builtin::Div => {
                if args.is_empty() {
                    return Err(EvalError::Message("/ expects at least 1 argument".into()));
                }
                let mut iter = args.into_iter();
                let first = expect_int_value(iter.next().unwrap(), "/")?;
                if iter.len() == 0 {
                    if first == 0 {
                        return Err(EvalError::Message("division by zero".into()));
                    }
                    return Ok(Value::Int(1 / first));
                }

                let mut result = first;
                for arg in iter {
                    let divisor = expect_int_value(arg, "/")?;
                    if divisor == 0 {
                        return Err(EvalError::Message("division by zero".into()));
                    }
                    result /= divisor;
                }
                Ok(Value::Int(result))
            }
            Builtin::Zero => {
                expect_exact_arity(&args, 1, "zero?")?;
                Ok(Value::Bool(
                    expect_int_value(args[0].clone(), "zero?")? == 0,
                ))
            }
            Builtin::Cons => {
                expect_exact_arity(&args, 2, "cons")?;
                Ok(Value::Pair(Rc::new(PairValue {
                    car: args[0].clone(),
                    cdr: args[1].clone(),
                })))
            }
            Builtin::Car => {
                expect_exact_arity(&args, 1, "car")?;
                car_of(&args[0])
            }
            Builtin::Cdr => {
                expect_exact_arity(&args, 1, "cdr")?;
                cdr_of(&args[0])
            }
            Builtin::Null => {
                expect_exact_arity(&args, 1, "null?")?;
                Ok(Value::Bool(matches!(args[0], Value::Nil)))
            }
            Builtin::List => Ok(list_from_slice(&args)),
            Builtin::Apply => {
                if args.len() < 2 {
                    return Err(EvalError::Message(
                        "apply expects at least 2 arguments".into(),
                    ));
                }
                let proc = args[0].clone();
                let mut call_args = args[1..args.len() - 1].to_vec();
                let tail = list_to_vec(&args[args.len() - 1])
                    .ok_or_else(|| EvalError::Message("apply expects a proper list".into()))?;
                call_args.extend(tail);
                self.apply(proc, call_args)
            }
            Builtin::StringAppend => {
                let mut result = String::new();
                for arg in args {
                    result.push_str(&expect_string_value(arg, "string-append")?);
                }
                Ok(Value::Str(result))
            }
            Builtin::NumberToString => {
                expect_exact_arity(&args, 1, "number->string")?;
                Ok(Value::Str(
                    expect_int_value(args[0].clone(), "number->string")?.to_string(),
                ))
            }
            Builtin::StringToSymbol => {
                expect_exact_arity(&args, 1, "string->symbol")?;
                Ok(Value::Symbol(expect_string_value(
                    args[0].clone(),
                    "string->symbol",
                )?))
            }
            Builtin::SyntaxToDatum => {
                expect_exact_arity(&args, 1, "syntax->datum")?;
                match &args[0] {
                    Value::Syntax(syntax) => Ok(expr_to_datum(&syntax.datum)),
                    _ => Err(EvalError::Message(
                        "syntax->datum expects a syntax object".into(),
                    )),
                }
            }
            Builtin::DatumToSyntax => {
                expect_exact_arity(&args, 2, "datum->syntax")?;
                match &args[0] {
                    Value::Syntax(_) => Ok(Value::Syntax(Syntax {
                        datum: value_to_expr(args[1].clone())?,
                    })),
                    _ => Err(EvalError::Message(
                        "datum->syntax expects a syntax object as its first argument".into(),
                    )),
                }
            }
            Builtin::Display => {
                expect_exact_arity(&args, 1, "display")?;
                self.output.push_str(&display_value(&args[0]));
                Ok(Value::Void)
            }
            Builtin::Write => {
                expect_exact_arity(&args, 1, "write")?;
                self.output.push_str(&render_value(&args[0]));
                Ok(Value::Void)
            }
            Builtin::Newline => {
                expect_exact_arity(&args, 0, "newline")?;
                self.output.push('\n');
                Ok(Value::Void)
            }
        }
    }

    fn expand_syntax_template(
        &mut self,
        env: &Env,
        expr: &Expr,
        repeat_index: Option<usize>,
        renamed: &HashMap<String, String>,
    ) -> EvalResult<Expr> {
        match expr {
            Expr::Symbol(symbol) => self.expand_template_symbol(env, symbol, repeat_index, renamed),
            Expr::List(items) => {
                if let Some(expanded) =
                    self.expand_template_let(env, items, repeat_index, renamed)?
                {
                    return Ok(expanded);
                }

                let mut expanded = Vec::with_capacity(items.len());
                let mut index = 0;
                while index < items.len() {
                    if index + 1 < items.len() && is_ellipsis(&items[index + 1]) {
                        let repeat_count = self.template_repeat_count(env, &items[index])?;
                        for repeat in 0..repeat_count {
                            expanded.push(self.expand_syntax_template(
                                env,
                                &items[index],
                                Some(repeat),
                                renamed,
                            )?);
                        }
                        index += 2;
                        continue;
                    }

                    expanded.push(self.expand_syntax_template(
                        env,
                        &items[index],
                        repeat_index,
                        renamed,
                    )?);
                    index += 1;
                }
                Ok(Expr::List(expanded))
            }
            _ => Ok(expr.clone()),
        }
    }

    fn expand_template_symbol(
        &self,
        env: &Env,
        symbol: &Symbol,
        repeat_index: Option<usize>,
        renamed: &HashMap<String, String>,
    ) -> EvalResult<Expr> {
        if let Some(value) = env.get(&symbol.name) {
            match value {
                Value::Syntax(syntax) => return Ok(syntax.datum),
                Value::SyntaxList(values) => {
                    if let Some(index) = repeat_index {
                        return values
                            .get(index)
                            .cloned()
                            .map(|syntax| syntax.datum)
                            .ok_or_else(|| {
                                EvalError::Message(format!(
                                    "ellipsis index out of range for {}",
                                    symbol.name
                                ))
                            });
                    }
                    return Err(EvalError::Message(format!(
                        "pattern variable {} used outside ellipsis",
                        symbol.name
                    )));
                }
                _ => {}
            }
        }

        if let Some(fresh) = renamed.get(&symbol.name) {
            return Ok(Expr::Symbol(Symbol {
                name: fresh.clone(),
                span: symbol.span,
            }));
        }

        Ok(Expr::Symbol(symbol.clone()))
    }

    fn expand_template_let(
        &mut self,
        env: &Env,
        items: &[Expr],
        repeat_index: Option<usize>,
        renamed: &HashMap<String, String>,
    ) -> EvalResult<Option<Expr>> {
        if items.len() < 3 || !is_plain_symbol(&items[0], "let") {
            return Ok(None);
        }

        let raw_bindings = match &items[1] {
            Expr::List(bindings) => bindings,
            _ => return Ok(None),
        };

        let mut local_renamed = renamed.clone();
        let mut expanded_bindings = Vec::with_capacity(raw_bindings.len());
        for binding in raw_bindings {
            let pair = match binding {
                Expr::List(items) if items.len() == 2 => items,
                _ => {
                    return Err(EvalError::Message(
                        "macro-generated let bindings must have the form (name value)".into(),
                    ))
                }
            };

            let name_expr = match &pair[0] {
                Expr::Symbol(symbol) => {
                    if matches!(
                        env.get(&symbol.name),
                        Some(Value::Syntax(_) | Value::SyntaxList(_))
                    ) {
                        self.expand_syntax_template(env, &pair[0], repeat_index, &local_renamed)?
                    } else {
                        let fresh = self.fresh_name(&symbol.name);
                        local_renamed.insert(symbol.name.clone(), fresh.clone());
                        Expr::Symbol(Symbol {
                            name: fresh,
                            span: symbol.span,
                        })
                    }
                }
                _ => self.expand_syntax_template(env, &pair[0], repeat_index, &local_renamed)?,
            };

            let value_expr =
                self.expand_syntax_template(env, &pair[1], repeat_index, &local_renamed)?;
            expanded_bindings.push(Expr::List(vec![name_expr, value_expr]));
        }

        let mut expanded_items = vec![
            self.expand_syntax_template(env, &items[0], repeat_index, renamed)?,
            Expr::List(expanded_bindings),
        ];
        for body in &items[2..] {
            expanded_items.push(self.expand_syntax_template(
                env,
                body,
                repeat_index,
                &local_renamed,
            )?);
        }
        Ok(Some(Expr::List(expanded_items)))
    }

    fn template_repeat_count(&self, env: &Env, expr: &Expr) -> EvalResult<usize> {
        let mut names = HashSet::new();
        collect_repeated_template_names(env, expr, &mut names);
        if names.is_empty() {
            return Err(EvalError::Message(
                "template ellipsis must reference a repeated pattern variable".into(),
            ));
        }

        let mut count = None;
        for name in names {
            let len = match env.get(&name) {
                Some(Value::SyntaxList(values)) => values.len(),
                _ => 0,
            };
            match count {
                None => count = Some(len),
                Some(existing) if existing == len => {}
                Some(_) => {
                    return Err(EvalError::Message(
                        "template ellipsis has mismatched repetition counts".into(),
                    ))
                }
            }
        }
        Ok(count.unwrap_or(0))
    }

    fn fresh_name(&mut self, base: &str) -> String {
        self.gensym += 1;
        format!("__ming_syntax_{}_{}", base, self.gensym)
    }

    fn error_at(&self, span: Span, message: impl Into<String>) -> EvalError {
        if span.line > 0 && span.col > 0 {
            EvalError::Message(format!("{}:{}: {}", span.line, span.col, message.into()))
        } else {
            EvalError::Message(message.into())
        }
    }
}

#[derive(Clone)]
enum PatternBinding {
    Single(Syntax),
    Repeated(Vec<Syntax>),
}

fn match_pattern(
    pattern: &Expr,
    candidate: &Expr,
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> EvalResult<bool> {
    match pattern {
        Expr::Symbol(symbol) => match symbol.name.as_str() {
            "_" => Ok(true),
            "..." => Ok(false),
            name if literals.contains(name) => Ok(matches!(
                candidate,
                Expr::Symbol(candidate_symbol) if candidate_symbol.name == name
            )),
            name => bind_single(bindings, name, candidate.clone()),
        },
        Expr::List(pattern_items) => {
            let candidate_items = match candidate {
                Expr::List(items) => items,
                _ => return Ok(false),
            };
            match_list_pattern(pattern_items, candidate_items, literals, bindings)
        }
        Expr::Int(value) => Ok(matches!(candidate, Expr::Int(other) if other == value)),
        Expr::Bool(value) => Ok(matches!(candidate, Expr::Bool(other) if other == value)),
        Expr::Str(value) => Ok(matches!(candidate, Expr::Str(other) if other == value)),
    }
}

fn match_list_pattern(
    pattern_items: &[Expr],
    candidate_items: &[Expr],
    literals: &HashSet<String>,
    bindings: &mut HashMap<String, PatternBinding>,
) -> EvalResult<bool> {
    let mut pattern_index = 0;
    let mut candidate_index = 0;

    while pattern_index < pattern_items.len() {
        if pattern_index + 1 < pattern_items.len() && is_ellipsis(&pattern_items[pattern_index + 1])
        {
            if pattern_index + 2 != pattern_items.len() {
                return Err(EvalError::Message(
                    "only trailing ellipsis patterns are supported".into(),
                ));
            }

            let mut repeated_names = HashSet::new();
            collect_pattern_names(&pattern_items[pattern_index], literals, &mut repeated_names);
            ensure_repeated_bindings(bindings, &repeated_names)?;

            while candidate_index < candidate_items.len() {
                let mut local = HashMap::new();
                if !match_pattern(
                    &pattern_items[pattern_index],
                    &candidate_items[candidate_index],
                    literals,
                    &mut local,
                )? {
                    return Ok(false);
                }
                append_repeated(bindings, &local, &repeated_names)?;
                candidate_index += 1;
            }

            pattern_index += 2;
            break;
        }

        if candidate_index >= candidate_items.len() {
            return Ok(false);
        }

        if !match_pattern(
            &pattern_items[pattern_index],
            &candidate_items[candidate_index],
            literals,
            bindings,
        )? {
            return Ok(false);
        }

        pattern_index += 1;
        candidate_index += 1;
    }

    Ok(pattern_index == pattern_items.len() && candidate_index == candidate_items.len())
}

fn bind_single(
    bindings: &mut HashMap<String, PatternBinding>,
    name: &str,
    candidate: Expr,
) -> EvalResult<bool> {
    match bindings.get(name) {
        Some(PatternBinding::Single(existing)) => Ok(expr_eq(&existing.datum, &candidate)),
        Some(PatternBinding::Repeated(_)) => Ok(false),
        None => {
            bindings.insert(
                name.to_string(),
                PatternBinding::Single(Syntax { datum: candidate }),
            );
            Ok(true)
        }
    }
}

fn collect_pattern_names(pattern: &Expr, literals: &HashSet<String>, out: &mut HashSet<String>) {
    match pattern {
        Expr::Symbol(symbol) => {
            if symbol.name != "_" && symbol.name != "..." && !literals.contains(&symbol.name) {
                out.insert(symbol.name.clone());
            }
        }
        Expr::List(items) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_pattern_names(item, literals, out);
                }
            }
        }
        _ => {}
    }
}

fn ensure_repeated_bindings(
    bindings: &mut HashMap<String, PatternBinding>,
    names: &HashSet<String>,
) -> EvalResult<()> {
    for name in names {
        match bindings.get(name) {
            Some(PatternBinding::Single(_)) => {
                return Err(EvalError::Message(format!(
                    "pattern variable {} used as both single and repeated",
                    name
                )))
            }
            Some(PatternBinding::Repeated(_)) => {}
            None => {
                bindings.insert(name.clone(), PatternBinding::Repeated(Vec::new()));
            }
        }
    }
    Ok(())
}

fn append_repeated(
    bindings: &mut HashMap<String, PatternBinding>,
    local: &HashMap<String, PatternBinding>,
    names: &HashSet<String>,
) -> EvalResult<()> {
    for name in names {
        let value = match local.get(name) {
            Some(PatternBinding::Single(syntax)) => syntax.clone(),
            Some(PatternBinding::Repeated(_)) => {
                return Err(EvalError::Message(
                    "nested repeated patterns are not supported".into(),
                ))
            }
            None => {
                return Err(EvalError::Message(format!(
                    "missing repeated pattern variable {}",
                    name
                )))
            }
        };

        match bindings.get_mut(name) {
            Some(PatternBinding::Repeated(values)) => values.push(value),
            _ => {
                return Err(EvalError::Message(format!(
                    "pattern variable {} is not repeated",
                    name
                )))
            }
        }
    }
    Ok(())
}

fn collect_repeated_template_names(env: &Env, expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Symbol(symbol) => {
            if matches!(env.get(&symbol.name), Some(Value::SyntaxList(_))) {
                out.insert(symbol.name.clone());
            }
        }
        Expr::List(items) => {
            for item in items {
                if !is_ellipsis(item) {
                    collect_repeated_template_names(env, item, out);
                }
            }
        }
        _ => {}
    }
}

fn parse_lambda_params(expr: &Expr) -> EvalResult<ParamSpec> {
    match expr {
        Expr::Symbol(symbol) => Ok(ParamSpec {
            fixed: Vec::new(),
            rest: Some(symbol.name.clone()),
        }),
        Expr::List(items) => parse_param_list(items),
        _ => Err(EvalError::Message(
            "lambda parameters must be a symbol or list".into(),
        )),
    }
}

fn parse_param_list(items: &[Expr]) -> EvalResult<ParamSpec> {
    let mut fixed = Vec::new();
    let mut index = 0;
    while index < items.len() {
        let symbol = expect_symbol(&items[index])?;
        if symbol.name == "." {
            if index + 2 != items.len() {
                return Err(EvalError::Message("invalid dotted parameter list".into()));
            }
            let rest = expect_symbol(&items[index + 1])?;
            return Ok(ParamSpec {
                fixed,
                rest: Some(rest.name.clone()),
            });
        }
        fixed.push(symbol.name.clone());
        index += 1;
    }
    Ok(ParamSpec { fixed, rest: None })
}

fn expr_to_datum(expr: &Expr) -> Value {
    match expr {
        Expr::Int(value) => Value::Int(*value),
        Expr::Bool(value) => Value::Bool(*value),
        Expr::Str(value) => Value::Str(value.clone()),
        Expr::Symbol(symbol) => Value::Symbol(symbol.name.clone()),
        Expr::List(items) => {
            let values: Vec<Value> = items.iter().map(expr_to_datum).collect();
            list_from_slice(&values)
        }
    }
}

fn value_to_expr(value: Value) -> EvalResult<Expr> {
    match value {
        Value::Int(value) => Ok(Expr::Int(value)),
        Value::Bool(value) => Ok(Expr::Bool(value)),
        Value::Str(value) => Ok(Expr::Str(value)),
        Value::Symbol(value) => Ok(Expr::Symbol(Symbol {
            name: value,
            span: Span::default(),
        })),
        Value::Nil => Ok(Expr::List(Vec::new())),
        Value::Pair(pair) => {
            let items = pair_to_vec(&Value::Pair(pair)).ok_or_else(|| {
                EvalError::Message("cannot convert improper list to syntax".into())
            })?;
            let mut exprs = Vec::with_capacity(items.len());
            for item in items {
                exprs.push(value_to_expr(item)?);
            }
            Ok(Expr::List(exprs))
        }
        Value::Syntax(syntax) => Ok(syntax.datum),
        _ => Err(EvalError::Message(
            "datum->syntax expects a datum-compatible value".into(),
        )),
    }
}

fn list_from_slice(items: &[Value]) -> Value {
    let mut result = Value::Nil;
    for item in items.iter().rev() {
        result = Value::Pair(Rc::new(PairValue {
            car: item.clone(),
            cdr: result,
        }));
    }
    result
}

fn pair_to_vec(value: &Value) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    let mut current = value.clone();
    loop {
        match current {
            Value::Nil => return Some(items),
            Value::Pair(pair) => {
                items.push(pair.car.clone());
                current = pair.cdr.clone();
            }
            _ => return None,
        }
    }
}

fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    pair_to_vec(value)
}

fn car_of(value: &Value) -> EvalResult<Value> {
    match value {
        Value::Pair(pair) => Ok(pair.car.clone()),
        _ => Err(EvalError::Message("car expects a non-empty list".into())),
    }
}

fn cdr_of(value: &Value) -> EvalResult<Value> {
    match value {
        Value::Pair(pair) => Ok(pair.cdr.clone()),
        _ => Err(EvalError::Message("cdr expects a non-empty list".into())),
    }
}

fn expect_symbol(expr: &Expr) -> EvalResult<&Symbol> {
    match expr {
        Expr::Symbol(symbol) => Ok(symbol),
        _ => Err(EvalError::Message("expected symbol".into())),
    }
}

fn expect_exact_arity(args: &[Value], expected: usize, name: &str) -> EvalResult<()> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(EvalError::Message(format!(
            "{} expects exactly {} arguments",
            name, expected
        )))
    }
}

fn expect_int_value(value: Value, name: &str) -> EvalResult<i64> {
    match value {
        Value::Int(value) => Ok(value),
        _ => Err(EvalError::Message(format!(
            "{} expects integer arguments",
            name
        ))),
    }
}

fn expect_string_value(value: Value, name: &str) -> EvalResult<String> {
    match value {
        Value::Str(value) => Ok(value),
        _ => Err(EvalError::Message(format!(
            "{} expects string arguments",
            name
        ))),
    }
}

fn is_truthy(value: &Value) -> bool {
    !matches!(value, Value::Bool(false))
}

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(expr, Expr::Symbol(symbol) if symbol.name == "...")
}

fn is_plain_symbol(expr: &Expr, expected: &str) -> bool {
    matches!(expr, Expr::Symbol(symbol) if symbol.name == expected)
}

fn expr_eq(left: &Expr, right: &Expr) -> bool {
    match (left, right) {
        (Expr::Int(a), Expr::Int(b)) => a == b,
        (Expr::Bool(a), Expr::Bool(b)) => a == b,
        (Expr::Str(a), Expr::Str(b)) => a == b,
        (Expr::Symbol(a), Expr::Symbol(b)) => a.name == b.name,
        (Expr::List(a), Expr::List(b)) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| expr_eq(a, b))
        }
        _ => false,
    }
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Int(value) => value.to_string(),
        Value::Bool(true) => "#t".into(),
        Value::Bool(false) => "#f".into(),
        Value::Str(value) => format!("{:?}", value),
        Value::Symbol(value) => value.clone(),
        Value::Nil => "()".into(),
        Value::Pair(_) => render_pair(value),
        Value::Builtin(_) => "#<procedure>".into(),
        Value::Closure(_) => "#<procedure>".into(),
        Value::Macro(_) => "#<macro>".into(),
        Value::Syntax(_) => "#<syntax>".into(),
        Value::SyntaxList(_) => "#<syntax-list>".into(),
        Value::Void => String::new(),
    }
}

fn display_value(value: &Value) -> String {
    match value {
        Value::Str(value) => value.clone(),
        _ => render_value(value),
    }
}

fn render_pair(value: &Value) -> String {
    let mut parts = Vec::new();
    let mut current = value.clone();

    loop {
        match current {
            Value::Pair(pair) => {
                parts.push(render_value(&pair.car));
                current = pair.cdr.clone();
            }
            Value::Nil => return format!("({})", parts.join(" ")),
            other => {
                return format!("({} . {})", parts.join(" "), render_value(&other));
            }
        }
    }
}

impl Env {
    fn new() -> Self {
        Self(Rc::new(EnvData {
            parent: None,
            bindings: RefCell::new(HashMap::new()),
        }))
    }

    fn child(&self) -> Self {
        Self(Rc::new(EnvData {
            parent: Some(self.clone()),
            bindings: RefCell::new(HashMap::new()),
        }))
    }

    fn define(&self, name: &str, value: Value) {
        self.0
            .bindings
            .borrow_mut()
            .insert(name.to_string(), Rc::new(RefCell::new(value)));
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.lookup_cell(name).map(|cell| cell.borrow().clone())
    }

    fn set(&self, name: &str, value: Value) -> Result<(), ()> {
        if let Some(cell) = self.lookup_cell(name) {
            *cell.borrow_mut() = value;
            Ok(())
        } else {
            Err(())
        }
    }

    fn lookup_cell(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(value) = self.0.bindings.borrow().get(name) {
            return Some(value.clone());
        }
        self.0.parent.as_ref()?.lookup_cell(name)
    }
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            col: 1,
            buffered: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> EvalResult<Vec<Expr>> {
        let mut exprs = Vec::new();
        while self.peek_token()?.is_some() {
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr(&mut self) -> EvalResult<Expr> {
        let token = self
            .next_token()?
            .ok_or_else(|| EvalError::Message("unexpected end of input".into()))?;
        match token.kind {
            TokenKind::LParen => {
                let mut items = Vec::new();
                loop {
                    match self.peek_token()? {
                        Some(Token {
                            kind: TokenKind::RParen,
                            ..
                        }) => {
                            self.next_token()?;
                            break;
                        }
                        Some(_) => items.push(self.parse_expr()?),
                        None => {
                            return Err(EvalError::Message(format!(
                                "{}:{}: unterminated list",
                                token.span.line, token.span.col
                            )))
                        }
                    }
                }
                Ok(Expr::List(items))
            }
            TokenKind::RParen => Err(EvalError::Message(format!(
                "{}:{}: unexpected ')'",
                token.span.line, token.span.col
            ))),
            TokenKind::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![make_symbol("quote", token.span), quoted]))
            }
            TokenKind::SyntaxQuote => {
                let quoted = self.parse_expr()?;
                Ok(Expr::List(vec![make_symbol("syntax", token.span), quoted]))
            }
            TokenKind::Atom(text) => Ok(parse_atom(&text, token.span)),
            TokenKind::Str(text) => Ok(Expr::Str(text)),
        }
    }

    fn peek_token(&mut self) -> EvalResult<Option<Token>> {
        if self.buffered.is_empty() {
            if let Some(token) = self.read_token()? {
                self.buffered.push(token);
            }
        }
        Ok(self.buffered.first().cloned())
    }

    fn next_token(&mut self) -> EvalResult<Option<Token>> {
        if !self.buffered.is_empty() {
            return Ok(Some(self.buffered.remove(0)));
        }
        self.read_token()
    }

    fn read_token(&mut self) -> EvalResult<Option<Token>> {
        self.skip_ws_and_comments();
        if self.offset >= self.input.len() {
            return Ok(None);
        }

        let span = Span {
            line: self.line,
            col: self.col,
        };
        let remaining = &self.input[self.offset..];
        let mut chars = remaining.chars();
        let ch = chars.next().unwrap();

        match ch {
            '(' => {
                self.bump_char(ch);
                Ok(Some(Token {
                    kind: TokenKind::LParen,
                    span,
                }))
            }
            ')' => {
                self.bump_char(ch);
                Ok(Some(Token {
                    kind: TokenKind::RParen,
                    span,
                }))
            }
            '\'' => {
                self.bump_char(ch);
                Ok(Some(Token {
                    kind: TokenKind::Quote,
                    span,
                }))
            }
            '#' if remaining.starts_with("#'") => {
                self.bump_char('#');
                self.bump_char('\'');
                Ok(Some(Token {
                    kind: TokenKind::SyntaxQuote,
                    span,
                }))
            }
            '"' => {
                self.bump_char('"');
                let mut value = String::new();
                while self.offset < self.input.len() {
                    let current = self.input[self.offset..].chars().next().unwrap();
                    self.bump_char(current);
                    match current {
                        '"' => {
                            return Ok(Some(Token {
                                kind: TokenKind::Str(value),
                                span,
                            }))
                        }
                        '\\' => {
                            let escaped = self
                                .input
                                .get(self.offset..)
                                .and_then(|rest| rest.chars().next())
                                .ok_or_else(|| {
                                    EvalError::Message(format!(
                                        "{}:{}: unterminated string escape",
                                        span.line, span.col
                                    ))
                                })?;
                            self.bump_char(escaped);
                            match escaped {
                                'n' => value.push('\n'),
                                't' => value.push('\t'),
                                '"' => value.push('"'),
                                '\\' => value.push('\\'),
                                other => value.push(other),
                            }
                        }
                        other => value.push(other),
                    }
                }
                Err(EvalError::Message(format!(
                    "{}:{}: unterminated string",
                    span.line, span.col
                )))
            }
            _ => {
                let start = self.offset;
                while self.offset < self.input.len() {
                    let current = self.input[self.offset..].chars().next().unwrap();
                    if current.is_whitespace() || matches!(current, '(' | ')' | '\'' | ';') {
                        break;
                    }
                    if current == '#' && self.input[self.offset..].starts_with("#'") {
                        break;
                    }
                    self.bump_char(current);
                }
                let text = self.input[start..self.offset].to_string();
                Ok(Some(Token {
                    kind: TokenKind::Atom(text),
                    span,
                }))
            }
        }
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            let Some(ch) = self.input[self.offset..].chars().next() else {
                return;
            };

            if ch.is_whitespace() {
                self.bump_char(ch);
                continue;
            }
            if ch == ';' {
                while let Some(current) = self.input[self.offset..].chars().next() {
                    self.bump_char(current);
                    if current == '\n' {
                        break;
                    }
                }
                continue;
            }
            return;
        }
    }

    fn bump_char(&mut self, ch: char) {
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}

fn parse_atom(text: &str, span: Span) -> Expr {
    match text {
        "#t" => Expr::Bool(true),
        "#f" => Expr::Bool(false),
        _ => match text.parse::<i64>() {
            Ok(value) => Expr::Int(value),
            Err(_) => make_symbol(text, span),
        },
    }
}

fn make_symbol(name: impl Into<String>, span: Span) -> Expr {
    Expr::Symbol(Symbol {
        name: name.into(),
        span,
    })
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&render_value(self))
    }
}
