use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

mod builtins;
pub mod error;
mod parser;

use builtins::apply_builtin;
pub use error::EvalError;
use error::SourcePos;
use parser::Parser;

#[derive(Clone, Debug, PartialEq)]
enum Expr {
    Integer(i64, SourcePos),
    Boolean(bool, SourcePos),
    String(String, SourcePos),
    Char(char, SourcePos),
    Symbol(String, SourcePos),
    List(Vec<Expr>, SourcePos),
}

impl Expr {
    fn pos(&self) -> SourcePos {
        match self {
            Self::Integer(_, pos)
            | Self::Boolean(_, pos)
            | Self::String(_, pos)
            | Self::Char(_, pos)
            | Self::Symbol(_, pos)
            | Self::List(_, pos) => *pos,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    Abs,
    Modulo,
    Remainder,
    Quotient,
    Min,
    Max,
    Expt,
    ZeroPred,
    PositivePred,
    NegativePred,
    OddPred,
    EvenPred,
    Less,
    Greater,
    Equal,
    LessEqual,
    EqPred,
    EqualPred,
    Not,
    Display,
    Write,
    Newline,
    Cons,
    Car,
    Cdr,
    Append,
    List,
    Length,
    ListRef,
    ListTail,
    ListPred,
    Assoc,
    Map,
    StringAppend,
    StringLength,
    Substring,
    StringToNumber,
    NumberToString,
    SymbolToString,
    StringToSymbol,
    StringRef,
    StringSet,
    StringCopy,
    NullPred,
    NumberPred,
    StringPred,
    BooleanPred,
    PairPred,
    SymbolPred,
    CharPred,
    CharAlphabeticPred,
    CharNumericPred,
    CharUpcase,
    CharDowncase,
    CharEqual,
    CharLess,
    StringEqual,
    StringLess,
    StringCiEqual,
    StringUpcase,
    StringDowncase,
    Apply,
}

impl Builtin {
    fn name(self) -> &'static str {
        match self {
            Builtin::Add => "+",
            Builtin::Sub => "-",
            Builtin::Mul => "*",
            Builtin::Div => "/",
            Builtin::Abs => "abs",
            Builtin::Modulo => "modulo",
            Builtin::Remainder => "remainder",
            Builtin::Quotient => "quotient",
            Builtin::Min => "min",
            Builtin::Max => "max",
            Builtin::Expt => "expt",
            Builtin::ZeroPred => "zero?",
            Builtin::PositivePred => "positive?",
            Builtin::NegativePred => "negative?",
            Builtin::OddPred => "odd?",
            Builtin::EvenPred => "even?",
            Builtin::Less => "<",
            Builtin::Greater => ">",
            Builtin::Equal => "=",
            Builtin::LessEqual => "<=",
            Builtin::EqPred => "eq?",
            Builtin::EqualPred => "equal?",
            Builtin::Not => "not",
            Builtin::Display => "display",
            Builtin::Write => "write",
            Builtin::Newline => "newline",
            Builtin::Cons => "cons",
            Builtin::Car => "car",
            Builtin::Cdr => "cdr",
            Builtin::Append => "append",
            Builtin::List => "list",
            Builtin::Length => "length",
            Builtin::ListRef => "list-ref",
            Builtin::ListTail => "list-tail",
            Builtin::ListPred => "list?",
            Builtin::Assoc => "assoc",
            Builtin::Map => "map",
            Builtin::StringAppend => "string-append",
            Builtin::StringLength => "string-length",
            Builtin::Substring => "substring",
            Builtin::StringToNumber => "string->number",
            Builtin::NumberToString => "number->string",
            Builtin::SymbolToString => "symbol->string",
            Builtin::StringToSymbol => "string->symbol",
            Builtin::StringRef => "string-ref",
            Builtin::StringSet => "string-set!",
            Builtin::StringCopy => "string-copy",
            Builtin::NullPred => "null?",
            Builtin::NumberPred => "number?",
            Builtin::StringPred => "string?",
            Builtin::BooleanPred => "boolean?",
            Builtin::PairPred => "pair?",
            Builtin::SymbolPred => "symbol?",
            Builtin::CharPred => "char?",
            Builtin::CharAlphabeticPred => "char-alphabetic?",
            Builtin::CharNumericPred => "char-numeric?",
            Builtin::CharUpcase => "char-upcase",
            Builtin::CharDowncase => "char-downcase",
            Builtin::CharEqual => "char=?",
            Builtin::CharLess => "char<?",
            Builtin::StringEqual => "string=?",
            Builtin::StringLess => "string<?",
            Builtin::StringCiEqual => "string-ci=?",
            Builtin::StringUpcase => "string-upcase",
            Builtin::StringDowncase => "string-downcase",
            Builtin::Apply => "apply",
        }
    }
}

#[derive(Clone)]
struct SchemeString {
    chars: Rc<RefCell<Vec<char>>>,
    mutable: bool,
}

impl SchemeString {
    fn from_owned(value: String, mutable: bool) -> Self {
        Self {
            chars: Rc::new(RefCell::new(value.chars().collect())),
            mutable,
        }
    }

    fn literal(value: &str) -> Self {
        Self::from_owned(value.into(), false)
    }

    fn fresh(value: String) -> Self {
        Self::from_owned(value, true)
    }

    fn mutable_copy(&self) -> Self {
        Self {
            chars: Rc::new(RefCell::new(self.chars.borrow().clone())),
            mutable: true,
        }
    }

    fn to_plain_string(&self) -> String {
        self.chars.borrow().iter().collect()
    }

    fn chars(&self) -> Vec<char> {
        self.chars.borrow().clone()
    }

    fn len(&self) -> usize {
        self.chars.borrow().len()
    }

    fn get(&self, index: usize) -> Option<char> {
        self.chars.borrow().get(index).copied()
    }

    fn set(&self, index: usize, value: char) -> bool {
        let mut chars = self.chars.borrow_mut();
        let Some(slot) = chars.get_mut(index) else {
            return false;
        };
        *slot = value;
        true
    }

    fn is_mutable(&self) -> bool {
        self.mutable
    }
}

#[derive(Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(SchemeString),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Pair(Box<Value>, Box<Value>),
    Builtin(Builtin),
    Procedure(Rc<Procedure>),
    Void,
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::String(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::Char(_) => "char",
            Value::List(_) => "list",
            Value::Pair(_, _) => "pair",
            Value::Builtin(_) | Value::Procedure(_) => "procedure",
            Value::Void => "void",
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn render(&self) -> String {
        render_value(self, RenderMode::Write)
    }

    fn render_display(&self) -> String {
        render_value(self, RenderMode::Display)
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

type EnvRef = Rc<Env>;

struct Env {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<String, Value>>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Self {
            parent,
            bindings: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.borrow().get(name).cloned() {
            return Some(value);
        }

        self.parent.as_ref().and_then(|parent| parent.lookup(name))
    }

    fn set(&self, name: &str, value: Value) -> bool {
        {
            let mut bindings = self.bindings.borrow_mut();
            if let Some(slot) = bindings.get_mut(name) {
                *slot = value;
                return true;
            }
        }

        self.parent
            .as_ref()
            .is_some_and(|parent| parent.set(name, value))
    }
}

struct Procedure {
    name: Option<String>,
    params: Params,
    body: Vec<Expr>,
    env: EnvRef,
}

#[derive(Clone)]
struct Params {
    required: Vec<String>,
    rest: Option<String>,
}

impl Params {
    fn fixed(required: Vec<String>) -> Self {
        Self {
            required,
            rest: None,
        }
    }

    fn expected_args(&self) -> String {
        match self.rest {
            Some(_) => format!("at least {}", self.required.len()),
            None => self.required.len().to_string(),
        }
    }

    fn matches_arity(&self, got: usize) -> bool {
        match self.rest {
            Some(_) => got >= self.required.len(),
            None => got == self.required.len(),
        }
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
///
/// # Examples
/// ```
/// use ming::scheme::eval_str;
/// assert_eq!(eval_str("(+ 1 2)"), Ok("3".into()));
/// ```
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let (value, _) = eval_program(input)?;
    Ok(value.render())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let (value, output) = eval_program(input)?;
    Ok((value.render(), output))
}

fn eval_program(input: &str) -> Result<(Value, String), EvalError> {
    let mut parser = Parser::new(input);
    let exprs = parser.parse_all()?;
    let env = initial_env();
    let mut output = String::new();
    let value = eval_sequence(&exprs, &env, &mut output)?;
    Ok((value, output))
}

fn initial_env() -> EnvRef {
    let env = Env::new(None);

    for builtin in [
        Builtin::Add,
        Builtin::Sub,
        Builtin::Mul,
        Builtin::Div,
        Builtin::Abs,
        Builtin::Modulo,
        Builtin::Remainder,
        Builtin::Quotient,
        Builtin::Min,
        Builtin::Max,
        Builtin::Expt,
        Builtin::ZeroPred,
        Builtin::PositivePred,
        Builtin::NegativePred,
        Builtin::OddPred,
        Builtin::EvenPred,
        Builtin::Less,
        Builtin::Greater,
        Builtin::Equal,
        Builtin::LessEqual,
        Builtin::EqPred,
        Builtin::EqualPred,
        Builtin::Not,
        Builtin::Display,
        Builtin::Write,
        Builtin::Newline,
        Builtin::Cons,
        Builtin::Car,
        Builtin::Cdr,
        Builtin::Append,
        Builtin::List,
        Builtin::Length,
        Builtin::ListRef,
        Builtin::ListTail,
        Builtin::ListPred,
        Builtin::Assoc,
        Builtin::Map,
        Builtin::StringAppend,
        Builtin::StringLength,
        Builtin::Substring,
        Builtin::StringToNumber,
        Builtin::NumberToString,
        Builtin::SymbolToString,
        Builtin::StringToSymbol,
        Builtin::StringRef,
        Builtin::StringSet,
        Builtin::StringCopy,
        Builtin::NullPred,
        Builtin::NumberPred,
        Builtin::StringPred,
        Builtin::BooleanPred,
        Builtin::PairPred,
        Builtin::SymbolPred,
        Builtin::CharPred,
        Builtin::CharAlphabeticPred,
        Builtin::CharNumericPred,
        Builtin::CharUpcase,
        Builtin::CharDowncase,
        Builtin::CharEqual,
        Builtin::CharLess,
        Builtin::StringEqual,
        Builtin::StringLess,
        Builtin::StringCiEqual,
        Builtin::StringUpcase,
        Builtin::StringDowncase,
        Builtin::Apply,
    ] {
        env.define(builtin.name().into(), Value::Builtin(builtin));
    }

    env
}

fn eval_sequence(exprs: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut result = Value::Void;

    for expr in exprs {
        result = eval(expr, env, output)?;
    }

    Ok(result)
}

fn eval(expr: &Expr, env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let pos = expr.pos();

    match expr {
        Expr::Integer(value, _) => Ok(Value::Integer(*value)),
        Expr::Boolean(value, _) => Ok(Value::Boolean(*value)),
        Expr::String(value, _) => Ok(Value::String(SchemeString::literal(value))),
        Expr::Char(value, _) => Ok(Value::Char(*value)),
        Expr::Symbol(name, _) => env
            .lookup(name)
            .ok_or_else(|| EvalError::UnboundVariable { name: name.clone() })
            .map_err(|error| error.with_position(pos)),
        Expr::List(items, _) => {
            eval_list(items, env, output).map_err(|error| error.with_position(pos))
        }
    }
}

fn eval_list(items: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let Some((head, tail)) = items.split_first() else {
        return Err(EvalError::Syntax {
            message: "cannot evaluate empty list".into(),
        });
    };

    if let Expr::Symbol(name, _) = head {
        match name.as_str() {
            "define" => return eval_define(tail, env, output),
            "set!" => return eval_set(tail, env, output),
            "if" => return eval_if(tail, env, output),
            "quote" => return eval_quote(tail),
            "lambda" => return build_lambda(tail, env, None),
            "and" => return eval_and(tail, env, output),
            "or" => return eval_or(tail, env, output),
            "begin" => return eval_begin(tail, env, output),
            "cond" => return eval_cond(tail, env, output),
            "let" => return eval_let(tail, env, output),
            _ => {}
        }
    }

    let callable = eval(head, env, output)?;
    let args = eval_args(tail, env, output)?;
    apply(callable, &args, output)
}

fn eval_define(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = if let Some(parts) = lambda_parts(value_expr) {
                build_lambda(parts, env, Some(name.clone()))?
            } else {
                eval(value_expr, env, output)?
            };
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        [Expr::List(signature, _), body @ ..] => {
            let Some((Expr::Symbol(name, _), params)) = signature.split_first() else {
                return Err(EvalError::Syntax {
                    message: "define: expected function name".into(),
                });
            };
            if body.is_empty() {
                return Err(EvalError::Syntax {
                    message: "define: expected function body".into(),
                });
            }

            let value = new_procedure(Some(name.clone()), parse_param_list(params)?, body, env);
            env.define(name.clone(), value);
            Ok(Value::Void)
        }
        _ => Err(EvalError::Syntax {
            message: "define: invalid syntax".into(),
        }),
    }
}

fn eval_set(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), value_expr] => {
            let value = eval(value_expr, env, output)?;
            if env.set(name, value) {
                Ok(Value::Void)
            } else {
                Err(EvalError::UnboundVariable { name: name.clone() })
            }
        }
        [_, _] => Err(EvalError::Syntax {
            message: "set!: expected variable name".into(),
        }),
        _ => Err(wrong_arg_count("set!", "2", args.len())),
    }
}

fn eval_if(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [condition, then_branch, else_branch] => {
            if eval(condition, env, output)?.is_truthy() {
                eval(then_branch, env, output)
            } else {
                eval(else_branch, env, output)
            }
        }
        _ => Err(wrong_arg_count("if", "3", args.len())),
    }
}

fn eval_quote(args: &[Expr]) -> Result<Value, EvalError> {
    match args {
        [expr] => Ok(quote_expr(expr)),
        _ => Err(wrong_arg_count("quote", "1", args.len())),
    }
}

fn eval_args(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::with_capacity(args.len());
    for expr in args {
        values.push(eval(expr, env, output)?);
    }
    Ok(values)
}

fn eval_and(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    let mut last = Value::Boolean(true);

    for arg in args {
        let value = eval(arg, env, output)?;
        if !value.is_truthy() {
            return Ok(value);
        }
        last = value;
    }

    Ok(last)
}

fn eval_or(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for arg in args {
        let value = eval(arg, env, output)?;
        if value.is_truthy() {
            return Ok(value);
        }
    }

    Ok(Value::Boolean(false))
}

fn eval_begin(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    eval_sequence(args, env, output)
}

fn eval_cond(clauses: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    for (index, clause) in clauses.iter().enumerate() {
        let Expr::List(items, _) = clause else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };
        let Some((test, body)) = items.split_first() else {
            return Err(EvalError::Syntax {
                message: "cond: expected clause".into(),
            });
        };

        if matches!(test, Expr::Symbol(name, _) if name == "else") {
            if index + 1 != clauses.len() {
                return Err(EvalError::Syntax {
                    message: "cond: else must be last".into(),
                });
            }
            return eval_sequence(body, env, output);
        }

        let value = eval(test, env, output)?;
        if value.is_truthy() {
            return if body.is_empty() {
                Ok(value)
            } else {
                eval_sequence(body, env, output)
            };
        }
    }

    Ok(Value::Void)
}

fn eval_let(args: &[Expr], env: &EnvRef, output: &mut String) -> Result<Value, EvalError> {
    match args {
        [Expr::Symbol(name, _), bindings, body @ ..] => {
            eval_named_let(name, bindings, body, env, output)
        }
        [bindings, body @ ..] => eval_plain_let(bindings, body, env, output),
        _ => Err(EvalError::Syntax {
            message: "let: invalid syntax".into(),
        }),
    }
}

fn eval_plain_let(
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut values = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        values.push(eval(value_expr, env, output)?);
    }

    let let_env = Env::new(Some(env.clone()));
    for ((name, _), value) in bindings.into_iter().zip(values) {
        let_env.define(name, value);
    }

    eval_sequence(body, &let_env, output)
}

fn eval_named_let(
    name: &str,
    bindings_expr: &Expr,
    body: &[Expr],
    env: &EnvRef,
    output: &mut String,
) -> Result<Value, EvalError> {
    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "let: expected body".into(),
        });
    }

    let bindings = parse_let_bindings(bindings_expr)?;
    let mut args = Vec::with_capacity(bindings.len());
    for (_, value_expr) in &bindings {
        args.push(eval(value_expr, env, output)?);
    }
    let params = bindings.iter().map(|(param, _)| param.clone()).collect();

    let let_env = Env::new(Some(env.clone()));
    let procedure = new_procedure(Some(name.into()), Params::fixed(params), body, &let_env);
    let_env.define(name.into(), procedure.clone());
    apply(procedure, &args, output)
}

fn parse_let_bindings(bindings_expr: &Expr) -> Result<Vec<(String, Expr)>, EvalError> {
    let Expr::List(bindings, _) = bindings_expr else {
        return Err(EvalError::Syntax {
            message: "let: expected bindings".into(),
        });
    };

    let mut parsed = Vec::with_capacity(bindings.len());
    for binding in bindings {
        match binding {
            Expr::List(parts, _) => match parts.as_slice() {
                [Expr::Symbol(name, _), value_expr] => {
                    parsed.push((name.clone(), value_expr.clone()));
                }
                _ => {
                    return Err(EvalError::Syntax {
                        message: "let: expected binding pair".into(),
                    });
                }
            },
            _ => {
                return Err(EvalError::Syntax {
                    message: "let: expected binding pair".into(),
                });
            }
        }
    }

    Ok(parsed)
}

fn build_lambda(parts: &[Expr], env: &EnvRef, name: Option<String>) -> Result<Value, EvalError> {
    let [params_expr, body @ ..] = parts else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameters and body".into(),
        });
    };

    if body.is_empty() {
        return Err(EvalError::Syntax {
            message: "lambda: expected body".into(),
        });
    }

    Ok(new_procedure(
        name,
        parse_params_expr(params_expr)?,
        body,
        env,
    ))
}

fn new_procedure(name: Option<String>, params: Params, body: &[Expr], env: &EnvRef) -> Value {
    Value::Procedure(Rc::new(Procedure {
        name,
        params,
        body: body.to_vec(),
        env: env.clone(),
    }))
}

fn parse_params_expr(params: &Expr) -> Result<Params, EvalError> {
    let Expr::List(items, _) = params else {
        return Err(EvalError::Syntax {
            message: "lambda: expected parameter list".into(),
        });
    };

    parse_param_list(items)
}

fn parse_param_list(params: &[Expr]) -> Result<Params, EvalError> {
    let mut required = Vec::with_capacity(params.len());
    let mut index = 0;

    while index < params.len() {
        match &params[index] {
            Expr::Symbol(name, _) if name == "." => {
                if index + 2 != params.len() {
                    return Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    });
                }

                return match &params[index + 1] {
                    Expr::Symbol(name, _) if name != "." => Ok(Params {
                        required,
                        rest: Some(name.clone()),
                    }),
                    _ => Err(EvalError::Syntax {
                        message: "lambda: expected parameter name".into(),
                    }),
                };
            }
            Expr::Symbol(name, _) => required.push(name.clone()),
            _ => {
                return Err(EvalError::Syntax {
                    message: "lambda: expected parameter name".into(),
                });
            }
        }

        index += 1;
    }

    Ok(Params {
        required,
        rest: None,
    })
}

fn lambda_parts(expr: &Expr) -> Option<&[Expr]> {
    let Expr::List(items, _) = expr else {
        return None;
    };
    let (Expr::Symbol(name, _), tail) = items.split_first()? else {
        return None;
    };

    if name == "lambda" {
        Some(tail)
    } else {
        None
    }
}

fn quote_expr(expr: &Expr) -> Value {
    match expr {
        Expr::Integer(value, _) => Value::Integer(*value),
        Expr::Boolean(value, _) => Value::Boolean(*value),
        Expr::String(value, _) => Value::String(SchemeString::literal(value)),
        Expr::Char(value, _) => Value::Char(*value),
        Expr::Symbol(value, _) => Value::Symbol(value.clone()),
        Expr::List(items, _) => Value::List(items.iter().map(quote_expr).collect()),
    }
}

fn apply(callable: Value, args: &[Value], output: &mut String) -> Result<Value, EvalError> {
    match callable {
        Value::Builtin(builtin) => apply_builtin(builtin, args, output),
        Value::Procedure(procedure) => apply_procedure(&procedure, args, output),
        value => Err(EvalError::NotAProcedure {
            got: value.type_name().into(),
        }),
    }
}

fn apply_procedure(
    procedure: &Procedure,
    args: &[Value],
    output: &mut String,
) -> Result<Value, EvalError> {
    if !procedure.params.matches_arity(args.len()) {
        let name = procedure.name.as_deref().unwrap_or("lambda");
        let expected = procedure.params.expected_args();
        return Err(wrong_arg_count(name, &expected, args.len()));
    }

    let call_env = Env::new(Some(procedure.env.clone()));
    for (param, arg) in procedure.params.required.iter().zip(args.iter()) {
        call_env.define(param.clone(), arg.clone());
    }
    if let Some(rest) = &procedure.params.rest {
        call_env.define(
            rest.clone(),
            Value::List(args[procedure.params.required.len()..].to_vec()),
        );
    }

    eval_sequence(&procedure.body, &call_env, output)
}

fn wrong_arg_count(name: &str, expected: &str, got: usize) -> EvalError {
    EvalError::WrongArgCount {
        name: name.into(),
        expected: expected.into(),
        got,
    }
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match value {
        Value::Integer(value) => value.to_string(),
        Value::Boolean(true) => "#t".into(),
        Value::Boolean(false) => "#f".into(),
        Value::String(value) => match mode {
            RenderMode::Write => format!("\"{}\"", escape_string(&value.to_plain_string())),
            RenderMode::Display => value.to_plain_string(),
        },
        Value::Symbol(value) => value.clone(),
        Value::Char(ch) => render_char(*ch, mode),
        Value::List(items) => render_list(items, mode),
        Value::Pair(head, tail) => render_pair(head, tail, mode),
        Value::Builtin(_) | Value::Procedure(_) => "#<procedure>".into(),
        Value::Void => String::new(),
    }
}

fn render_list(items: &[Value], mode: RenderMode) -> String {
    let parts: Vec<String> = items
        .iter()
        .map(|value| render_value(value, mode))
        .collect();
    format!("({})", parts.join(" "))
}

fn render_pair(head: &Value, tail: &Value, mode: RenderMode) -> String {
    let mut rendered = String::new();
    rendered.push('(');
    rendered.push_str(&render_value(head, mode));
    render_pair_tail(tail, mode, &mut rendered);
    rendered.push(')');
    rendered
}

fn render_pair_tail(tail: &Value, mode: RenderMode, rendered: &mut String) {
    match tail {
        Value::List(items) => {
            for item in items {
                rendered.push(' ');
                rendered.push_str(&render_value(item, mode));
            }
        }
        Value::Pair(head, next) => {
            rendered.push(' ');
            rendered.push_str(&render_value(head, mode));
            render_pair_tail(next, mode, rendered);
        }
        other => {
            rendered.push_str(" . ");
            rendered.push_str(&render_value(other, mode));
        }
    }
}

fn render_char(ch: char, mode: RenderMode) -> String {
    match mode {
        RenderMode::Display => ch.to_string(),
        RenderMode::Write => match ch {
            ' ' => "#\\space".into(),
            '\n' => "#\\newline".into(),
            other => format!("#\\{other}"),
        },
    }
}

fn escape_string(value: &str) -> String {
    let mut escaped = String::new();

    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }

    escaped
}

#[cfg(test)]
mod tests;
