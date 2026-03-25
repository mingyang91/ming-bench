pub mod error;
mod builtins;

pub use error::EvalError;
use builtins::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym(prefix: &str) -> String {
    let n = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("#{}#{}", prefix, n)
}

pub(crate) type BuiltinFn = fn(&[Val], &Env) -> Result<Val, EvalError>;

#[derive(Clone)]
pub(crate) enum Val {
    Int(i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Val>),
    Pair(Box<Val>, Box<Val>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Vec<Expr>,
        env: Env,
    },
    Builtin(BuiltinFn),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Expr, Expr)>,
        def_env: Env,
    },
    Void,
}

impl Val {
    fn is_truthy(&self) -> bool {
        !matches!(self, Val::Bool(false))
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{n}"),
            Val::Bool(true) => write!(f, "#t"),
            Val::Bool(false) => write!(f, "#f"),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Char(c) => write!(f, "#\\{c}"),
            Val::Symbol(s) => write!(f, "{s}"),
            Val::List(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Val::Pair(a, b) => {
                write!(f, "({a}")?;
                let mut cur = b.as_ref();
                loop {
                    match cur {
                        Val::Pair(ca, cb) => {
                            write!(f, " {ca}")?;
                            cur = cb.as_ref();
                        }
                        Val::List(v) if v.is_empty() => break,
                        other => { write!(f, " . {other}")?; break; }
                    }
                }
                write!(f, ")")
            }
            Val::Lambda { .. } | Val::Builtin(..) | Val::Macro { .. } => write!(f, "#<procedure>"),
            Val::Void => write!(f, "#<void>"),
        }
    }
}

// --- Environment ---

type Frame = Rc<RefCell<HashMap<String, Val>>>;

#[derive(Clone)]
pub(crate) struct Env {
    frames: Vec<Frame>,
    pub(crate) output: Rc<RefCell<String>>,
}

impl Env {
    fn new() -> Self {
        let frame = Rc::new(RefCell::new(HashMap::new()));
        let builtins: &[(&str, BuiltinFn)] = &[
            ("+", builtin_add as BuiltinFn),
            ("-", builtin_sub),
            ("*", builtin_mul),
            ("/", builtin_div),
            ("<", builtin_lt),
            (">", builtin_gt),
            ("=", builtin_eq),
            ("<=", builtin_le),
            (">=", builtin_ge),
            ("not", builtin_not),
            ("cons", builtin_cons as BuiltinFn),
            ("car", builtin_car),
            ("cdr", builtin_cdr),
            ("list", builtin_list),
            ("null?", builtin_null),
            ("length", builtin_length),
            ("append", builtin_append),
            ("boolean?", builtin_is_boolean),
            ("number?", builtin_is_number),
            ("string?", builtin_is_string),
            ("symbol?", builtin_is_symbol),
            ("pair?", builtin_is_pair),
            ("char?", builtin_is_char),
            ("display", builtin_display),
            ("write", builtin_write),
            ("newline", builtin_newline),
            ("string-append", builtin_string_append),
            ("string-length", builtin_string_length),
            ("substring", builtin_substring),
            ("string->number", builtin_string_to_number),
            ("number->string", builtin_number_to_string),
            ("symbol->string", builtin_symbol_to_string),
            ("string->symbol", builtin_string_to_symbol),
            ("string-ref", builtin_string_ref),
            ("string-copy", builtin_string_copy),
            ("apply", builtin_apply),
            ("equal?", builtin_equal),
            ("eq?", builtin_eq_pred),
            ("abs", builtin_abs),
            ("modulo", builtin_modulo),
            ("remainder", builtin_remainder),
            ("quotient", builtin_quotient),
            ("min", builtin_min),
            ("max", builtin_max),
            ("expt", builtin_expt),
            ("zero?", builtin_zero),
            ("positive?", builtin_positive),
            ("negative?", builtin_negative),
            ("odd?", builtin_odd),
            ("even?", builtin_even),
            ("list-ref", builtin_list_ref),
            ("list-tail", builtin_list_tail),
            ("list?", builtin_is_list),
            ("assoc", builtin_assoc),
            ("map", builtin_map),
            ("char-alphabetic?", builtin_char_alphabetic),
            ("char-numeric?", builtin_char_numeric),
            ("char-upcase", builtin_char_upcase),
            ("char-downcase", builtin_char_downcase),
            ("char=?", builtin_char_eq),
            ("char<?", builtin_char_lt),
            ("string=?", builtin_string_eq),
            ("string<?", builtin_string_lt),
            ("string-ci=?", builtin_string_ci_eq),
            ("string-upcase", builtin_string_upcase),
            ("string-downcase", builtin_string_downcase),
        ];
        for &(name, f) in builtins {
            frame.borrow_mut().insert(name.to_string(), Val::Builtin(f));
        }
        Env { frames: vec![frame], output: Rc::new(RefCell::new(String::new())) }
    }

    fn get(&self, name: &str) -> Option<Val> {
        for frame in self.frames.iter().rev() {
            if let Some(v) = frame.borrow().get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn define(&self, name: String, val: Val) {
        self.frames.last().expect("env has no frames").borrow_mut().insert(name, val);
    }

    fn set(&self, name: &str, val: Val) -> Result<(), EvalError> {
        for frame in self.frames.iter().rev() {
            let mut f = frame.borrow_mut();
            if f.contains_key(name) {
                f.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(EvalError::UnboundVariable(name.to_string()))
    }

    fn push(&self) -> Env {
        let mut frames = self.frames.clone();
        frames.push(Rc::new(RefCell::new(HashMap::new())));
        Env { frames, output: Rc::clone(&self.output) }
    }
}

// --- Source Positions ---

#[derive(Debug, Clone, Copy)]
pub(crate) struct Span {
    line: usize,
    col: usize,
}

impl Span {
    fn new(line: usize, col: usize) -> Self {
        Span { line, col }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// --- Parser ---

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    kind: ExprKind,
    span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum ExprKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Char(char),
    Symbol(String),
    List(Vec<Expr>),
}

impl Expr {
    fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

struct Token {
    text: String,
    span: Span,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    let mut line = 1usize;
    let mut col = 1usize;
    while let Some(&c) = chars.peek() {
        match c {
            '\n' => { chars.next(); line += 1; col = 1; }
            ' ' | '\t' | '\r' => { chars.next(); col += 1; }
            ';' => {
                while let Some(&c2) = chars.peek() {
                    chars.next();
                    col += 1;
                    if c2 == '\n' { line += 1; col = 1; break; }
                }
            }
            '(' => { tokens.push(Token { text: "(".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            ')' => { tokens.push(Token { text: ")".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '\'' => { tokens.push(Token { text: "'".into(), span: Span::new(line, col) }); chars.next(); col += 1; }
            '"' => {
                let start_span = Span::new(line, col);
                chars.next();
                col += 1;
                let mut s = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            col += 1;
                            match chars.next() {
                                Some('n') => { s.push('\n'); col += 1; }
                                Some('t') => { s.push('\t'); col += 1; }
                                Some('"') => { s.push('"'); col += 1; }
                                Some('\\') => { s.push('\\'); col += 1; }
                                Some(other) => { s.push('\\'); s.push(other); col += 1; }
                                None => break,
                            }
                        }
                        Some('"') => { col += 1; break; }
                        Some('\n') => { s.push('\n'); line += 1; col = 1; }
                        Some(c2) => { s.push(c2); col += 1; }
                        None => break,
                    }
                }
                tokens.push(Token { text: format!("\"{}\"", s), span: start_span });
            }
            _ => {
                let start_span = Span::new(line, col);
                let mut tok = String::new();
                while let Some(&c2) = chars.peek() {
                    if c2 == '(' || c2 == ')' || c2 == ' ' || c2 == '\t' || c2 == '\n' || c2 == '\r' || c2 == ';' || c2 == '\'' {
                        break;
                    }
                    tok.push(c2);
                    chars.next();
                    col += 1;
                }
                tokens.push(Token { text: tok, span: start_span });
            }
        }
    }
    tokens
}

fn parse(tokens: &[Token]) -> Result<(Expr, usize), EvalError> {
    if tokens.is_empty() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let tok = &tokens[0];
    let span = tok.span;
    if tok.text == "'" {
        let (inner, consumed) = parse(&tokens[1..])?;
        Ok((Expr::new(ExprKind::List(vec![
            Expr::new(ExprKind::Symbol("quote".into()), span),
            inner,
        ]), span), 1 + consumed))
    } else if tok.text == "(" {
        let mut elems = Vec::new();
        let mut i = 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (expr, consumed) = parse(&tokens[i..])?;
            elems.push(expr);
            i += consumed;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse(format!("missing closing paren at {span}")));
        }
        Ok((Expr::new(ExprKind::List(elems), span), i + 1))
    } else if tok.text == ")" {
        Err(EvalError::Parse(format!("unexpected ) at {span}")))
    } else if tok.text.starts_with('"') {
        let s = tok.text[1..tok.text.len()-1].to_string();
        Ok((Expr::new(ExprKind::Str(s), span), 1))
    } else if tok.text == "#t" {
        Ok((Expr::new(ExprKind::Bool(true), span), 1))
    } else if tok.text == "#f" {
        Ok((Expr::new(ExprKind::Bool(false), span), 1))
    } else if tok.text.starts_with("#\\") {
        let rest = &tok.text[2..];
        let ch = match rest {
            "space" => ' ',
            "newline" => '\n',
            "tab" => '\t',
            s if s.len() == 1 => s.chars().next().expect("single-char string is non-empty"),
            _ => return Err(EvalError::Parse(format!("unknown character literal: {} at {span}", tok.text))),
        };
        Ok((Expr::new(ExprKind::Char(ch), span), 1))
    } else if let Ok(n) = tok.text.parse::<i64>() {
        Ok((Expr::new(ExprKind::Int(n), span), 1))
    } else {
        Ok((Expr::new(ExprKind::Symbol(tok.text.clone()), span), 1))
    }
}

fn parse_all(input: &str) -> Result<Vec<Expr>, EvalError> {
    let tokens = tokenize(input);
    let mut exprs = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (expr, consumed) = parse(&tokens[i..])?;
        exprs.push(expr);
        i += consumed;
    }
    Ok(exprs)
}

// --- Evaluator ---

fn span_err(span: Span, err: EvalError) -> EvalError {
    // If the error message already contains position info, return as-is
    let msg = err.to_string();
    if msg.contains(':') && msg.bytes().any(|b| b.is_ascii_digit()) {
        // Check more carefully: look for digit:digit pattern
        let bytes = msg.as_bytes();
        let has_pos = bytes.windows(3).any(|w| {
            w[0].is_ascii_digit() && w[1] == b':' && w[2].is_ascii_digit()
        });
        if has_pos {
            return err;
        }
    }
    match err {
        EvalError::Parse(m) => EvalError::Parse(format!("{m} at {span}")),
        EvalError::Type(m) => EvalError::Type(format!("{m} at {span}")),
        EvalError::UnboundVariable(m) => EvalError::UnboundVariable(format!("{m} at {span}")),
        EvalError::Arity(m) => EvalError::Arity(format!("{m} at {span}")),
        EvalError::Runtime(m) => EvalError::Runtime(format!("{m} at {span}")),
    }
}

fn eval(expr: &Expr, env: &Env) -> Result<Val, EvalError> {
    let span = expr.span;
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(name) => {
            env.get(name).ok_or_else(|| EvalError::UnboundVariable(format!("{name} at {span}")))
        }
        ExprKind::List(elems) => {
            if elems.is_empty() {
                return Ok(Val::List(vec![]));
            }
            // Check for special forms
            if let ExprKind::Symbol(op) = &elems[0].kind {
                match op.as_str() {
                    "define" => return eval_define(&elems[1..], env, span),
                    "if" => return eval_if(&elems[1..], env, span),
                    "quote" => return eval_quote(&elems[1..], span),
                    "lambda" => return eval_lambda(&elems[1..], env, span),
                    "and" => return eval_and(&elems[1..], env),
                    "or" => return eval_or(&elems[1..], env),
                    "begin" => return eval_begin(&elems[1..], env),
                    "let" => return eval_let(&elems[1..], env, span),
                    "cond" => return eval_cond(&elems[1..], env),
                    "set!" => return eval_set_bang(&elems[1..], env, span),
                    "string-set!" => return eval_string_set(&elems[1..], env, span),
                    "define-syntax" => return eval_define_syntax(&elems[1..], env, span),
                    _ => {
                        if let Some(Val::Macro { literals, rules, def_env }) = env.get(op) {
                            return eval_macro_call(elems, &literals, &rules, &def_env, env, span);
                        }
                    }
                }
            }
            // Evaluate function position
            let func = eval(&elems[0], env)?;
            let args: Vec<Val> = elems[1..].iter().map(|e| eval(e, env)).collect::<Result<_, _>>()?;
            apply_val(&func, &args, env).map_err(|e| span_err(span, e))
        }
    }
}

pub(crate) fn apply_val(func: &Val, args: &[Val], caller_env: &Env) -> Result<Val, EvalError> {
    match func {
        Val::Lambda { params, rest_param, body, env } => {
            if let Some(rest) = rest_param {
                if args.len() < params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected at least {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                new_env.define(rest.clone(), Val::List(args[params.len()..].to_vec()));
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            } else {
                if args.len() != params.len() {
                    return Err(EvalError::Arity(format!(
                        "expected {} arguments, got {}", params.len(), args.len()
                    )));
                }
                let new_env = env.push();
                for (p, a) in params.iter().zip(args.iter()) {
                    new_env.define(p.clone(), a.clone());
                }
                let mut result = Val::Void;
                for expr in body {
                    result = eval(expr, &new_env)?;
                }
                Ok(result)
            }
        }
        Val::Builtin(f) => f(args, caller_env),
        _ => Err(EvalError::Type("not a procedure".into())),
    }
}

fn parse_params(exprs: &[Expr], span: Span) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < exprs.len() {
        match &exprs[i].kind {
            ExprKind::Symbol(s) if s == "." => {
                if i + 1 >= exprs.len() {
                    return Err(EvalError::Parse(format!("missing rest parameter after . at {span}")));
                }
                rest_param = Some(match &exprs[i + 1].kind {
                    ExprKind::Symbol(s) => s.clone(),
                    _ => return Err(EvalError::Parse(format!("expected rest parameter name at {span}"))),
                });
                break;
            }
            ExprKind::Symbol(s) => params.push(s.clone()),
            _ => return Err(EvalError::Parse(format!("expected parameter name at {span}"))),
        }
        i += 1;
    }
    Ok((params, rest_param))
}

// --- Macros (syntax-rules) ---

#[derive(Clone, Debug)]
enum MacroBinding {
    Single(Expr),
    Many(Vec<Expr>),
}

fn match_pattern_list(
    pattern: &[Expr],
    input: &[Expr],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let ellipsis_pos = pattern.iter().position(
        |e| matches!(&e.kind, ExprKind::Symbol(s) if s == "..."),
    );
    if let Some(epos) = ellipsis_pos {
        let fixed_before = &pattern[..epos - 1];
        let ellipsis_pat = &pattern[epos - 1];
        let fixed_after = &pattern[epos + 1..];
        let min_len = fixed_before.len() + fixed_after.len();
        if input.len() < min_len {
            return false;
        }
        for (p, e) in fixed_before.iter().zip(input.iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        let after_start = input.len() - fixed_after.len();
        for (p, e) in fixed_after.iter().zip(input[after_start..].iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        let ellipsis_input = &input[fixed_before.len()..after_start];
        match &ellipsis_pat.kind {
            ExprKind::Symbol(s) if !literals.contains(s) && s != "_" => {
                bindings.insert(s.clone(), MacroBinding::Many(ellipsis_input.to_vec()));
                true
            }
            _ => ellipsis_input.is_empty(),
        }
    } else {
        if pattern.len() != input.len() {
            return false;
        }
        for (p, e) in pattern.iter().zip(input.iter()) {
            if !match_pattern_single(p, e, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn match_pattern_single(
    pattern: &Expr,
    input: &Expr,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pattern.kind {
        ExprKind::Symbol(s) if s == "_" => true,
        ExprKind::Symbol(s) if literals.contains(s) => {
            matches!(&input.kind, ExprKind::Symbol(s2) if s2 == s)
        }
        ExprKind::Symbol(s) => {
            bindings.insert(s.clone(), MacroBinding::Single(input.clone()));
            true
        }
        _ => false,
    }
}

fn find_ellipsis_var(expr: &Expr, bindings: &HashMap<String, MacroBinding>) -> Option<String> {
    match &expr.kind {
        ExprKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Many(_))) {
                return Some(s.clone());
            }
            None
        }
        ExprKind::List(elems) => {
            for e in elems {
                if let Some(v) = find_ellipsis_var(e, bindings) {
                    return Some(v);
                }
            }
            None
        }
        _ => None,
    }
}

fn collect_template_free_vars(
    expr: &Expr,
    pattern_vars: &[String],
    out: &mut Vec<String>,
) {
    match &expr.kind {
        ExprKind::Symbol(s) if s != "..." => {
            if !pattern_vars.contains(s) && !out.contains(s) {
                out.push(s.clone());
            }
        }
        ExprKind::List(elems) => {
            for e in elems {
                collect_template_free_vars(e, pattern_vars, out);
            }
        }
        _ => {}
    }
}

const SPECIAL_FORMS: &[&str] = &[
    "define", "if", "quote", "lambda", "and", "or", "begin",
    "let", "cond", "set!", "string-set!", "define-syntax", "syntax-rules",
];

fn is_ellipsis(expr: &Expr) -> bool {
    matches!(&expr.kind, ExprKind::Symbol(s) if s == "...")
}

fn expand_ellipsis_element(
    sub: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
    result: &mut Vec<Expr>,
) {
    let Some(var_name) = find_ellipsis_var(sub, bindings) else { return };
    let Some(MacroBinding::Many(exprs)) = bindings.get(&var_name) else { return };
    if matches!(&sub.kind, ExprKind::Symbol(sn) if sn == &var_name) {
        result.extend(exprs.iter().cloned());
    } else {
        for expr in exprs {
            let mut sub_bindings = bindings.clone();
            sub_bindings.insert(var_name.clone(), MacroBinding::Single(expr.clone()));
            result.push(expand_template(sub, &sub_bindings, renames));
        }
    }
}

fn expand_template(
    template: &Expr,
    bindings: &HashMap<String, MacroBinding>,
    renames: &HashMap<String, String>,
) -> Expr {
    match &template.kind {
        ExprKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(e) => e.clone(),
                    MacroBinding::Many(_) => template.clone(),
                }
            } else if let Some(new_name) = renames.get(s) {
                Expr::new(ExprKind::Symbol(new_name.clone()), template.span)
            } else {
                template.clone()
            }
        }
        ExprKind::List(elems) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < elems.len() {
                if i + 1 < elems.len() && is_ellipsis(&elems[i + 1]) {
                    expand_ellipsis_element(&elems[i], bindings, renames, &mut result);
                    i += 2;
                    continue;
                }
                result.push(expand_template(&elems[i], bindings, renames));
                i += 1;
            }
            Expr::new(ExprKind::List(result), template.span)
        }
        _ => template.clone(),
    }
}

fn eval_macro_call(
    elems: &[Expr],
    literals: &[String],
    rules: &[(Expr, Expr)],
    def_env: &Env,
    use_env: &Env,
    span: Span,
) -> Result<Val, EvalError> {
    for (pattern, template) in rules {
        let pat_elems = match &pattern.kind {
            ExprKind::List(e) => e,
            _ => continue,
        };
        let mut bindings = HashMap::new();
        if match_pattern_list(&pat_elems[1..], &elems[1..], literals, &mut bindings) {
            let pattern_vars: Vec<String> = bindings.keys().cloned().collect();
            let mut free_vars = Vec::new();
            collect_template_free_vars(template, &pattern_vars, &mut free_vars);

            let mut renames = HashMap::new();
            for sym in &free_vars {
                if SPECIAL_FORMS.contains(&sym.as_str()) {
                    continue;
                }
                if def_env.get(sym).is_some() {
                    renames.insert(sym.clone(), gensym(sym));
                }
            }

            let expanded = expand_template(template, &bindings, &renames);

            if renames.is_empty() {
                return eval(&expanded, use_env);
            } else {
                let new_env = use_env.push();
                for (orig, gs) in &renames {
                    if let Some(val) = def_env.get(orig) {
                        new_env.define(gs.clone(), val);
                    }
                }
                return eval(&expanded, &new_env);
            }
        }
    }
    Err(EvalError::Runtime(format!("no matching syntax rule at {span}")))
}

fn eval_define_syntax(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("define-syntax: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s.clone(),
        _ => return Err(EvalError::Parse(format!("define-syntax: expected symbol at {span}"))),
    };
    let transformer = match &args[1].kind {
        ExprKind::List(elems) => elems,
        _ => return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {span}"))),
    };
    if transformer.is_empty()
        || !matches!(&transformer[0].kind, ExprKind::Symbol(s) if s == "syntax-rules")
    {
        return Err(EvalError::Parse(format!("define-syntax: expected syntax-rules at {span}")));
    }
    if transformer.len() < 2 {
        return Err(EvalError::Parse(format!("syntax-rules: missing literals at {span}")));
    }
    let literals = match &transformer[1].kind {
        ExprKind::List(lits) => lits
            .iter()
            .map(|e| match &e.kind {
                ExprKind::Symbol(s) => Ok(s.clone()),
                _ => Err(EvalError::Parse(format!(
                    "syntax-rules: expected literal symbol at {span}"
                ))),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => {
            return Err(EvalError::Parse(format!(
                "syntax-rules: expected literals list at {span}"
            )))
        }
    };
    let mut rules = Vec::new();
    for rule_expr in &transformer[2..] {
        match &rule_expr.kind {
            ExprKind::List(parts) if parts.len() == 2 => {
                rules.push((parts[0].clone(), parts[1].clone()));
            }
            _ => {
                return Err(EvalError::Parse(format!(
                    "syntax-rules: invalid rule at {span}"
                )))
            }
        }
    }
    env.define(
        name,
        Val::Macro {
            literals,
            rules,
            def_env: env.clone(),
        },
    );
    Ok(Val::Void)
}

fn eval_define(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("define: missing arguments at {span}")));
    }
    match &args[0].kind {
        ExprKind::Symbol(name) => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("define: expected 2 arguments at {span}")));
            }
            let val = eval(&args[1], env)?;
            env.define(name.clone(), val);
            Ok(Val::Void)
        }
        ExprKind::List(sig) => {
            // (define (f params...) body...)
            if sig.is_empty() {
                return Err(EvalError::Parse(format!("define: empty signature at {span}")));
            }
            let name = match &sig[0].kind {
                ExprKind::Symbol(s) => s.clone(),
                _ => return Err(EvalError::Parse(format!("define: expected symbol at {span}"))),
            };
            let (params, rest_param) = parse_params(&sig[1..], span)?;
            let body = args[1..].to_vec();
            let lambda = Val::Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
            };
            env.define(name, lambda);
            Ok(Val::Void)
        }
        _ => Err(EvalError::Parse(format!("define: expected symbol or list at {span}"))),
    }
}

fn eval_set_bang(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::Arity(format!("set!: expected 2 arguments at {span}")));
    }
    let name = match &args[0].kind {
        ExprKind::Symbol(s) => s,
        _ => return Err(EvalError::Parse(format!("set!: expected symbol at {span}"))),
    };
    let val = eval(&args[1], env)?;
    env.set(name, val).map_err(|e| span_err(span, e))?;
    Ok(Val::Void)
}

fn eval_if(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(EvalError::Arity(format!("if: expected 2 or 3 arguments at {span}")));
    }
    let cond = eval(&args[0], env)?;
    if cond.is_truthy() {
        eval(&args[1], env)
    } else if args.len() == 3 {
        eval(&args[2], env)
    } else {
        Ok(Val::Void)
    }
}

fn eval_quote(args: &[Expr], span: Span) -> Result<Val, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity(format!("quote: expected 1 argument at {span}")));
    }
    expr_to_val(&args[0])
}

fn expr_to_val(expr: &Expr) -> Result<Val, EvalError> {
    match &expr.kind {
        ExprKind::Int(n) => Ok(Val::Int(*n)),
        ExprKind::Bool(b) => Ok(Val::Bool(*b)),
        ExprKind::Str(s) => Ok(Val::Str(s.clone())),
        ExprKind::Char(c) => Ok(Val::Char(*c)),
        ExprKind::Symbol(s) => Ok(Val::Symbol(s.clone())),
        ExprKind::List(elems) => {
            let vals: Vec<Val> = elems.iter().map(expr_to_val).collect::<Result<_, _>>()?;
            Ok(Val::List(vals))
        }
    }
}

fn eval_lambda(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("lambda: missing parameters at {span}")));
    }
    let (params, rest_param) = match &args[0].kind {
        ExprKind::List(param_exprs) => parse_params(param_exprs, span)?,
        ExprKind::Symbol(s) => {
            // (lambda rest body...) — single rest param
            (vec![], Some(s.clone()))
        }
        _ => return Err(EvalError::Parse(format!("lambda: expected parameter list at {span}"))),
    };
    let body = args[1..].to_vec();
    Ok(Val::Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
    })
}

fn eval_and(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(true));
    }
    let mut result = Val::Bool(true);
    for expr in exprs {
        result = eval(expr, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(exprs: &[Expr], env: &Env) -> Result<Val, EvalError> {
    if exprs.is_empty() {
        return Ok(Val::Bool(false));
    }
    for expr in exprs {
        let result = eval(expr, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Val::Bool(false))
}

fn eval_begin(args: &[Expr], env: &Env) -> Result<Val, EvalError> {
    let mut result = Val::Void;
    for expr in args {
        result = eval(expr, env)?;
    }
    Ok(result)
}

fn eval_let(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.is_empty() {
        return Err(EvalError::Parse(format!("let: missing arguments at {span}")));
    }
    // Named let: (let name ((var init) ...) body ...)
    if let ExprKind::Symbol(name) = &args[0].kind {
        if args.len() < 2 {
            return Err(EvalError::Parse(format!("let: missing bindings at {span}")));
        }
        let bindings = match &args[1].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let mut params = Vec::new();
        let mut inits = Vec::new();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        params.push(s.clone());
                        inits.push(eval(&pair[1], env)?);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let body = args[2..].to_vec();
        let new_env = env.push();
        let lambda = Val::Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: new_env.clone(),
        };
        new_env.define(name.clone(), lambda.clone());
        apply_val(&lambda, &inits, &new_env)
    } else {
        // Regular let: (let ((var init) ...) body ...)
        let bindings = match &args[0].kind {
            ExprKind::List(b) => b,
            _ => return Err(EvalError::Parse(format!("let: expected bindings list at {span}"))),
        };
        let new_env = env.push();
        for b in bindings {
            match &b.kind {
                ExprKind::List(pair) if pair.len() == 2 => {
                    if let ExprKind::Symbol(s) = &pair[0].kind {
                        let val = eval(&pair[1], env)?;
                        new_env.define(s.clone(), val);
                    } else {
                        return Err(EvalError::Parse(format!("let: expected variable name at {span}")));
                    }
                }
                _ => return Err(EvalError::Parse(format!("let: invalid binding at {span}"))),
            }
        }
        let mut result = Val::Void;
        for expr in &args[1..] {
            result = eval(expr, &new_env)?;
        }
        Ok(result)
    }
}

fn eval_cond(clauses: &[Expr], env: &Env) -> Result<Val, EvalError> {
    for clause in clauses {
        match &clause.kind {
            ExprKind::List(parts) if !parts.is_empty() => {
                // Check for else clause
                if let ExprKind::Symbol(s) = &parts[0].kind {
                    if s == "else" {
                        let mut result = Val::Void;
                        for expr in &parts[1..] {
                            result = eval(expr, env)?;
                        }
                        return Ok(result);
                    }
                }
                let test = eval(&parts[0], env)?;
                if test.is_truthy() {
                    let mut result = test;
                    for expr in &parts[1..] {
                        result = eval(expr, env)?;
                    }
                    return Ok(result);
                }
            }
            _ => return Err(EvalError::Parse(format!("cond: invalid clause at {}", clause.span))),
        }
    }
    Ok(Val::Void)
}

fn eval_string_set(args: &[Expr], env: &Env, span: Span) -> Result<Val, EvalError> {
    if args.len() != 3 {
        return Err(EvalError::Arity(format!("string-set!: expected 3 arguments at {span}")));
    }
    let target = eval(&args[0], env)?;
    let idx = match eval(&args[1], env)? {
        Val::Int(n) => n as usize,
        _ => return Err(EvalError::Type("string-set!: expected integer index".into())),
    };
    let ch = match eval(&args[2], env)? {
        Val::Char(c) => c,
        _ => return Err(EvalError::Type("string-set!: expected char".into())),
    };
    let mut s = match target {
        Val::Str(s) => s,
        _ => return Err(EvalError::Type("string-set!: expected string".into())),
    };
    let mut chars: Vec<char> = s.chars().collect();
    if idx >= chars.len() {
        return Err(EvalError::Runtime("string-set!: index out of range".into()));
    }
    chars[idx] = ch;
    s = chars.into_iter().collect();
    // If the first argument is a variable, update it in the environment
    if let ExprKind::Symbol(name) = &args[0].kind {
        env.set(name, Val::Str(s))?;
    }
    Ok(Val::Void)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    Ok(last.to_string())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    let env = Env::new();
    let mut last = Val::Void;
    for expr in &exprs {
        last = eval(expr, &env)?;
    }
    let output = env.output.borrow().clone();
    Ok((last.to_string(), output))
}

#[cfg(test)]
mod tests;
