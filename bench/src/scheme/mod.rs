pub mod error;

pub use error::EvalError;

use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Value>),
    Lambda {
        params: Vec<String>,
        rest_param: Option<String>,
        body: Box<Ast>,
        env: Env,
    },
    Builtin(String),
    Continuation { id: usize, top_expr_idx: usize, call_line: usize, call_col: usize },
    Vector(Rc<RefCell<Vec<Value>>>),
    Macro {
        literals: Vec<String>,
        rules: Vec<(Ast, Ast)>,
        def_env: Env,
    },
}

type EnvCell = Rc<RefCell<Value>>;
type Env = HashMap<String, EnvCell>;

thread_local! {
    static CONT_JUMP: RefCell<Option<(usize, Value, usize, usize, usize)>> = RefCell::new(None);
    static CONT_RESUME: RefCell<Option<(Value, usize, usize)>> = RefCell::new(None);
    static CURRENT_TOP_IDX: Cell<usize> = Cell::new(0);
    static NEXT_CONT_ID: Cell<usize> = Cell::new(1);
    static GENSYM_COUNTER: Cell<usize> = Cell::new(0);
}

fn env_get(env: &Env, name: &str) -> Option<Value> {
    env.get(name).map(|cell| cell.borrow().clone())
}

fn env_set(env: &mut Env, name: String, val: Value) {
    env.insert(name, Rc::new(RefCell::new(val)));
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::Char(c) => format!("#\\{}", c),
            Value::Lambda { .. } => "#<procedure>".to_string(),
            Value::Builtin(_) => "#<procedure>".to_string(),
            Value::Vector(v) => {
                let items = v.borrow();
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("#({})", parts.join(" "))
            }
            Value::Continuation { .. } => "#<continuation>".to_string(),
            Value::Macro { .. } => "#<macro>".to_string(),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_scheme_string()).collect();
                format!("({})", parts.join(" "))
            }
        }
    }
}

/// Token with source position.
struct Token {
    text: String,
    line: usize,
    col: usize,
}

/// AST node with source position.
#[derive(Debug, Clone, PartialEq)]
struct Ast {
    kind: AstKind,
    line: usize,
    col: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum AstKind {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    Char(char),
    List(Vec<Ast>),
}

/// Tokenize input into a list of tokens with positions.
fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line: usize = 1;
    let mut col: usize = 1;
    while i < chars.len() {
        match chars[i] {
            '\n' => {
                i += 1;
                line += 1;
                col = 1;
            }
            ' ' | '\t' | '\r' => {
                i += 1;
                col += 1;
            }
            '"' => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                s.push('"');
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        if chars[i + 1] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 2;
                        }
                        i += 2;
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                    col += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(Token {
                    text: chars[i].to_string(),
                    line,
                    col,
                });
                i += 1;
                col += 1;
            }
            _ => {
                let start_col = col;
                let start_line = line;
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                    col += 1;
                }
                tokens.push(Token { text: s, line: start_line, col: start_col });
            }
        }
    }
    tokens
}

/// Parse a single expression from tokens, returning (ast, next_index).
fn parse(tokens: &[Token], pos: usize) -> Result<(Ast, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[pos];
    let line = token.line;
    let col = token.col;
    if token.text == "(" {
        let mut items = Vec::new();
        let mut i = pos + 1;
        while i < tokens.len() && tokens[i].text != ")" {
            let (val, next) = parse(tokens, i)?;
            items.push(val);
            i = next;
        }
        if i >= tokens.len() {
            return Err(EvalError::Parse("missing closing paren".into())
                .with_position(line, col));
        }
        Ok((Ast { kind: AstKind::List(items), line, col }, i + 1))
    } else if token.text == "'" {
        let (inner, next) = parse(tokens, pos + 1)?;
        Ok((
            Ast {
                kind: AstKind::List(vec![
                    Ast { kind: AstKind::Symbol("quote".into()), line, col },
                    inner,
                ]),
                line,
                col,
            },
            next,
        ))
    } else if token.text == ")" {
        Err(EvalError::Parse("unexpected )".into()).with_position(line, col))
    } else if token.text.starts_with("#\\") {
        let ch = if token.text.len() == 3 {
            token.text.chars().nth(2).unwrap()
        } else {
            match &token.text[2..] {
                "space" => ' ',
                "newline" => '\n',
                "tab" => '\t',
                _ => return Err(EvalError::Parse(format!("unknown character literal: {}", token.text))
                    .with_position(line, col)),
            }
        };
        Ok((Ast { kind: AstKind::Char(ch), line, col }, pos + 1))
    } else if token.text == "#t" {
        Ok((Ast { kind: AstKind::Boolean(true), line, col }, pos + 1))
    } else if token.text == "#f" {
        Ok((Ast { kind: AstKind::Boolean(false), line, col }, pos + 1))
    } else if token.text.starts_with('"') {
        let inner = &token.text[1..token.text.len() - 1];
        Ok((Ast { kind: AstKind::Str(inner.to_string()), line, col }, pos + 1))
    } else if let Ok(n) = token.text.parse::<i64>() {
        Ok((Ast { kind: AstKind::Integer(n), line, col }, pos + 1))
    } else {
        Ok((Ast { kind: AstKind::Symbol(token.text.clone()), line, col }, pos + 1))
    }
}

/// Convert an AST back to a Value (for quote).
fn ast_to_value(ast: &Ast) -> Value {
    match &ast.kind {
        AstKind::Integer(n) => Value::Integer(*n),
        AstKind::Boolean(b) => Value::Boolean(*b),
        AstKind::Str(s) => Value::Str(s.clone()),
        AstKind::Symbol(s) => Value::Symbol(s.clone()),
        AstKind::Char(c) => Value::Char(*c),
        AstKind::List(items) => Value::List(items.iter().map(ast_to_value).collect()),
    }
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Integer(x), Value::Integer(y)) => x == y,
        (Value::Boolean(x), Value::Boolean(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::List(xs), Value::List(ys)) => {
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
        (Value::Vector(xs), Value::Vector(ys)) => {
            let xs = xs.borrow();
            let ys = ys.borrow();
            xs.len() == ys.len() && xs.iter().zip(ys.iter()).all(|(a, b)| values_equal(a, b))
        }
        _ => false,
    }
}

fn is_builtin(name: &str) -> bool {
    matches!(name, "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" |
        "cons" | "car" | "cdr" | "null?" | "list" | "length" |
        "string?" | "number?" | "boolean?" | "pair?" | "symbol?" |
        "display" | "write" | "newline" |
        "string-append" | "string-length" | "substring" |
        "string->number" | "number->string" | "symbol->string" | "string->symbol" |
        "string-copy" | "string-ref" | "char?" | "map" |
        "string->list" | "list->string" | "char->integer" | "integer->char" |
        "apply" | "call/cc" | "call-with-current-continuation" |
        "equal?" | "eq?" | "eqv?" |
        "vector" | "make-vector" | "vector-ref" | "vector-set!" | "vector-length" |
        "vector?" | "vector->list" | "list->vector" |
        "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt" |
        "zero?" | "positive?" | "negative?" | "odd?" | "even?")
}

fn parse_params(param_asts: &[Ast]) -> Result<(Vec<String>, Option<String>), EvalError> {
    let mut params = Vec::new();
    let mut rest_param = None;
    let mut i = 0;
    while i < param_asts.len() {
        if let AstKind::Symbol(s) = &param_asts[i].kind {
            if s == "." {
                if i + 1 < param_asts.len() {
                    if let AstKind::Symbol(rest) = &param_asts[i + 1].kind {
                        rest_param = Some(rest.clone());
                        break;
                    }
                }
                return Err(EvalError::Parse("bad dot notation in parameters".into()));
            }
            params.push(s.clone());
        } else {
            return Err(EvalError::TypeError("parameter must be symbol".into())
                .with_position(param_asts[i].line, param_asts[i].col));
        }
        i += 1;
    }
    Ok((params, rest_param))
}

fn gensym(base: &str) -> String {
    GENSYM_COUNTER.with(|c| {
        let n = c.get();
        c.set(n + 1);
        format!("{}#~{}", base, n)
    })
}

#[derive(Clone, Debug)]
enum MacroBinding {
    Single(Ast),
    Ellipsis(Vec<Ast>),
}

fn collect_pattern_vars(pat: &Ast, literals: &[String]) -> Vec<String> {
    match &pat.kind {
        AstKind::Symbol(s) if s != "_" && s != "..." && !literals.contains(s) => {
            vec![s.clone()]
        }
        AstKind::List(items) => {
            items.iter().flat_map(|i| collect_pattern_vars(i, literals)).collect()
        }
        _ => vec![],
    }
}

fn match_syntax_element(
    pat: &Ast,
    inp: &Ast,
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    match &pat.kind {
        AstKind::Symbol(s) if s == "_" => true,
        AstKind::Symbol(s) if literals.contains(s) => {
            matches!(&inp.kind, AstKind::Symbol(is) if is == s)
        }
        AstKind::Symbol(s) if s != "..." => {
            bindings.insert(s.clone(), MacroBinding::Single(inp.clone()));
            true
        }
        AstKind::List(pats) => {
            if let AstKind::List(inps) = &inp.kind {
                match_syntax_elements(pats, inps, literals, bindings)
            } else {
                false
            }
        }
        AstKind::Integer(a) => matches!(&inp.kind, AstKind::Integer(b) if a == b),
        AstKind::Boolean(a) => matches!(&inp.kind, AstKind::Boolean(b) if a == b),
        _ => false,
    }
}

fn match_syntax_elements(
    pats: &[Ast],
    inps: &[Ast],
    literals: &[String],
    bindings: &mut HashMap<String, MacroBinding>,
) -> bool {
    let ellipsis_pos = pats.iter().position(|p| matches!(&p.kind, AstKind::Symbol(s) if s == "..."));

    if let Some(ep) = ellipsis_pos {
        if ep == 0 { return false; }
        let before = ep - 1;
        let after = pats.len() - ep - 1;
        if inps.len() < before + after { return false; }

        for i in 0..before {
            if !match_syntax_element(&pats[i], &inps[i], literals, bindings) {
                return false;
            }
        }

        let repeat_count = inps.len() - before - after;
        let repeat_pat = &pats[ep - 1];
        let var_names = collect_pattern_vars(repeat_pat, literals);
        let mut ellipsis_lists: HashMap<String, Vec<Ast>> = HashMap::new();
        for name in &var_names {
            ellipsis_lists.insert(name.clone(), Vec::new());
        }
        for i in 0..repeat_count {
            let mut sub_bindings = HashMap::new();
            if !match_syntax_element(repeat_pat, &inps[before + i], literals, &mut sub_bindings) {
                return false;
            }
            for (k, v) in sub_bindings {
                if let MacroBinding::Single(ast) = v {
                    ellipsis_lists.entry(k).or_default().push(ast);
                }
            }
        }
        for (k, v) in ellipsis_lists {
            bindings.insert(k, MacroBinding::Ellipsis(v));
        }

        for i in 0..after {
            if !match_syntax_element(&pats[ep + 1 + i], &inps[inps.len() - after + i], literals, bindings) {
                return false;
            }
        }
        true
    } else {
        if pats.len() != inps.len() { return false; }
        for (p, i) in pats.iter().zip(inps.iter()) {
            if !match_syntax_element(p, i, literals, bindings) {
                return false;
            }
        }
        true
    }
}

fn collect_template_ellipsis_vars(
    template: &Ast,
    bindings: &HashMap<String, MacroBinding>,
) -> Vec<String> {
    match &template.kind {
        AstKind::Symbol(s) => {
            if matches!(bindings.get(s), Some(MacroBinding::Ellipsis(_))) {
                vec![s.clone()]
            } else {
                vec![]
            }
        }
        AstKind::List(items) => {
            items.iter().flat_map(|i| collect_template_ellipsis_vars(i, bindings)).collect()
        }
        _ => vec![],
    }
}

fn instantiate_template(
    template: &Ast,
    bindings: &HashMap<String, MacroBinding>,
    hygiene_map: &HashMap<String, String>,
) -> Ast {
    match &template.kind {
        AstKind::Symbol(s) => {
            if let Some(binding) = bindings.get(s) {
                match binding {
                    MacroBinding::Single(ast) => ast.clone(),
                    MacroBinding::Ellipsis(_) => template.clone(),
                }
            } else if let Some(renamed) = hygiene_map.get(s) {
                Ast { kind: AstKind::Symbol(renamed.clone()), line: template.line, col: template.col }
            } else {
                template.clone()
            }
        }
        AstKind::List(items) => {
            let mut result = Vec::new();
            let mut i = 0;
            while i < items.len() {
                if i + 1 < items.len() && matches!(&items[i + 1].kind, AstKind::Symbol(s) if s == "...") {
                    let sub = &items[i];
                    let vars = collect_template_ellipsis_vars(sub, bindings);
                    if let Some(first_var) = vars.first() {
                        if let Some(MacroBinding::Ellipsis(elts)) = bindings.get(first_var) {
                            let count = elts.len();
                            for j in 0..count {
                                let mut iter_bindings = bindings.clone();
                                for var in &vars {
                                    if let Some(MacroBinding::Ellipsis(var_elts)) = bindings.get(var) {
                                        if j < var_elts.len() {
                                            iter_bindings.insert(var.clone(), MacroBinding::Single(var_elts[j].clone()));
                                        }
                                    }
                                }
                                result.push(instantiate_template(sub, &iter_bindings, hygiene_map));
                            }
                        }
                    }
                    i += 2;
                } else {
                    result.push(instantiate_template(&items[i], bindings, hygiene_map));
                    i += 1;
                }
            }
            Ast { kind: AstKind::List(result), line: template.line, col: template.col }
        }
        _ => template.clone(),
    }
}

fn collect_free_template_vars(template: &Ast, pattern_vars: &[String]) -> Vec<String> {
    match &template.kind {
        AstKind::Symbol(s) if s != "..." && !pattern_vars.contains(s) => {
            vec![s.clone()]
        }
        AstKind::List(items) => {
            items.iter().flat_map(|i| collect_free_template_vars(i, pattern_vars)).collect()
        }
        _ => vec![],
    }
}

/// Evaluate a parsed Scheme expression with tail call optimization.
fn eval(ast: &Ast, env: &mut Env, out: &mut String) -> Result<Value, EvalError> {
    let mut cur_ast = ast.clone();
    let mut tco_env: Option<Env> = None;

    'tco: loop {
        let e = match &mut tco_env {
            Some(le) => le,
            None => env,
        };
        let line = cur_ast.line;
        let col = cur_ast.col;
        match cur_ast.kind.clone() {
            AstKind::Integer(n) => return Ok(Value::Integer(n)),
            AstKind::Boolean(b) => return Ok(Value::Boolean(b)),
            AstKind::Str(s) => return Ok(Value::Str(s)),
            AstKind::Char(c) => return Ok(Value::Char(c)),
            AstKind::Symbol(s) => {
                if let Some(v) = env_get(e, &s) {
                    return Ok(v);
                }
                if is_builtin(&s) {
                    return Ok(Value::Builtin(s));
                }
                return Err(EvalError::UndefinedVariable(s).with_position(line, col));
            }
            AstKind::List(items) => {
                if items.is_empty() {
                    return Err(EvalError::Parse("empty application".into())
                        .with_position(line, col));
                }
                // Check for special forms by symbol name
                if let AstKind::Symbol(ref op) = items[0].kind {
                    match op.as_str() {
                        "define" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            match &items[1].kind {
                                AstKind::Symbol(name) => {
                                    if items.len() != 3 {
                                        return Err(EvalError::Arity.with_position(line, col));
                                    }
                                    let val = eval(&items[2], e, out)?;
                                    env_set(e, name.clone(), val);
                                    return Ok(Value::Symbol("ok".into()));
                                }
                                AstKind::List(sig) => {
                                    if sig.is_empty() {
                                        return Err(EvalError::Parse(
                                            "define: empty signature".into(),
                                        ).with_position(line, col));
                                    }
                                    let name = match &sig[0].kind {
                                        AstKind::Symbol(s) => s.clone(),
                                        _ => {
                                            return Err(EvalError::TypeError(
                                                "define expects symbol".into(),
                                            ).with_position(line, col))
                                        }
                                    };
                                    let (params, rest_param) = parse_params(&sig[1..])?;
                                    let body = if items.len() == 3 {
                                        items[2].clone()
                                    } else {
                                        let mut begin_items = vec![Ast {
                                            kind: AstKind::Symbol("begin".into()),
                                            line,
                                            col,
                                        }];
                                        begin_items.extend(items[2..].iter().cloned());
                                        Ast {
                                            kind: AstKind::List(begin_items),
                                            line,
                                            col,
                                        }
                                    };
                                    let lambda = Value::Lambda {
                                        params: params.clone(),
                                        rest_param: rest_param.clone(),
                                        body: Box::new(body.clone()),
                                        env: e.clone(),
                                    };
                                    env_set(e, name.clone(), lambda);
                                    let lambda = Value::Lambda {
                                        params,
                                        rest_param,
                                        body: Box::new(body),
                                        env: e.clone(),
                                    };
                                    *e.get(&name).unwrap().borrow_mut() = lambda;
                                    return Ok(Value::Symbol("ok".into()));
                                }
                                _ => {
                                    return Err(EvalError::TypeError(
                                        "define expects symbol or list".into(),
                                    ).with_position(line, col))
                                }
                            }
                        }
                        "set!" => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let name = match &items[1].kind {
                                AstKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::TypeError(
                                    "set!: expected symbol".into(),
                                ).with_position(line, col)),
                            };
                            let val = eval(&items[2], e, out)?;
                            match e.get(&name) {
                                Some(cell) => {
                                    *cell.borrow_mut() = val;
                                    return Ok(Value::Symbol("ok".into()));
                                }
                                None => {
                                    return Err(EvalError::UndefinedVariable(name)
                                        .with_position(line, col));
                                }
                            }
                        }
                        "lambda" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let param_list = match &items[1].kind {
                                AstKind::List(ps) => ps,
                                _ => {
                                    return Err(EvalError::TypeError(
                                        "lambda params must be a list".into(),
                                    ).with_position(line, col))
                                }
                            };
                            let (params, rest_param) = parse_params(param_list)?;
                            let body = if items.len() == 3 {
                                items[2].clone()
                            } else {
                                let mut begin_items = vec![Ast {
                                    kind: AstKind::Symbol("begin".into()),
                                    line,
                                    col,
                                }];
                                begin_items.extend(items[2..].iter().cloned());
                                Ast {
                                    kind: AstKind::List(begin_items),
                                    line,
                                    col,
                                }
                            };
                            return Ok(Value::Lambda {
                                params,
                                rest_param,
                                body: Box::new(body),
                                env: e.clone(),
                            });
                        }
                        "if" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let cond = eval(&items[1], e, out)?;
                            if cond != Value::Boolean(false) {
                                cur_ast = items[2].clone();
                                continue;
                            } else if items.len() > 3 {
                                cur_ast = items[3].clone();
                                continue;
                            } else {
                                return Ok(Value::Symbol("ok".into()));
                            }
                        }
                        "quote" => {
                            if items.len() != 2 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            return Ok(ast_to_value(&items[1]));
                        }
                        "and" => {
                            if items.len() <= 1 {
                                return Ok(Value::Boolean(true));
                            }
                            for a in &items[1..items.len() - 1] {
                                let result = eval(a, e, out)?;
                                if result == Value::Boolean(false) {
                                    return Ok(result);
                                }
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
                        }
                        "or" => {
                            if items.len() <= 1 {
                                return Ok(Value::Boolean(false));
                            }
                            for a in &items[1..items.len() - 1] {
                                let result = eval(a, e, out)?;
                                if result != Value::Boolean(false) {
                                    return Ok(result);
                                }
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
                        }
                        "begin" => {
                            if items.len() <= 1 {
                                return Ok(Value::Symbol("ok".into()));
                            }
                            for expr in &items[1..items.len() - 1] {
                                eval(expr, e, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            continue;
                        }
                        "let" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            // Named let: (let name ((var init) ...) body ...)
                            if let AstKind::Symbol(loop_name) = &items[1].kind {
                                if items.len() < 4 {
                                    return Err(EvalError::Arity.with_position(line, col));
                                }
                                let bindings = match &items[2].kind {
                                    AstKind::List(bs) => bs,
                                    _ => return Err(EvalError::TypeError("let: bindings must be a list".into())
                                        .with_position(line, col)),
                                };
                                let mut params = Vec::new();
                                let mut init_vals = Vec::new();
                                for b in bindings {
                                    match &b.kind {
                                        AstKind::List(pair) if pair.len() == 2 => {
                                            let pname = match &pair[0].kind {
                                                AstKind::Symbol(s) => s.clone(),
                                                _ => return Err(EvalError::TypeError("let: binding name must be symbol".into())
                                                    .with_position(pair[0].line, pair[0].col)),
                                            };
                                            let val = eval(&pair[1], e, out)?;
                                            params.push(pname);
                                            init_vals.push(val);
                                        }
                                        _ => return Err(EvalError::TypeError("let: bad binding".into())
                                            .with_position(b.line, b.col)),
                                    }
                                }
                                let body = if items.len() == 4 {
                                    items[3].clone()
                                } else {
                                    let mut begin_items = vec![Ast {
                                        kind: AstKind::Symbol("begin".into()),
                                        line, col,
                                    }];
                                    begin_items.extend(items[3..].iter().cloned());
                                    Ast { kind: AstKind::List(begin_items), line, col }
                                };
                                let mut local_env = e.clone();
                                let lambda = Value::Lambda {
                                    params: params.clone(),
                                    rest_param: None,
                                    body: Box::new(body.clone()),
                                    env: local_env.clone(),
                                };
                                env_set(&mut local_env, loop_name.clone(), lambda);
                                // Update closure env to include self-reference
                                let lambda = Value::Lambda {
                                    params: params.clone(),
                                    rest_param: None,
                                    body: Box::new(body),
                                    env: local_env.clone(),
                                };
                                *local_env.get(loop_name).unwrap().borrow_mut() = lambda;
                                for (p, v) in params.iter().zip(init_vals) {
                                    env_set(&mut local_env, p.clone(), v);
                                }
                                for expr in &items[3..items.len() - 1] {
                                    eval(expr, &mut local_env, out)?;
                                }
                                cur_ast = items.last().unwrap().clone();
                                tco_env = Some(local_env);
                                continue;
                            }
                            let bindings = match &items[1].kind {
                                AstKind::List(bs) => bs,
                                _ => return Err(EvalError::TypeError("let: bindings must be a list".into())
                                    .with_position(line, col)),
                            };
                            let mut local_env = e.clone();
                            for b in bindings {
                                match &b.kind {
                                    AstKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            AstKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::TypeError("let: binding name must be symbol".into())
                                                .with_position(pair[0].line, pair[0].col)),
                                        };
                                        let val = eval(&pair[1], e, out)?;
                                        env_set(&mut local_env, name, val);
                                    }
                                    _ => return Err(EvalError::TypeError("let: bad binding".into())
                                        .with_position(b.line, b.col)),
                                }
                            }
                            for expr in &items[2..items.len() - 1] {
                                eval(expr, &mut local_env, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            tco_env = Some(local_env);
                            continue;
                        }
                        "letrec" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let bindings = match &items[1].kind {
                                AstKind::List(bs) => bs,
                                _ => return Err(EvalError::TypeError("letrec: bindings must be a list".into())
                                    .with_position(line, col)),
                            };
                            let mut local_env = e.clone();
                            // First pass: bind all names to Void placeholders
                            let mut names = Vec::new();
                            let mut inits = Vec::new();
                            for b in bindings {
                                match &b.kind {
                                    AstKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            AstKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::TypeError("letrec: binding name must be symbol".into())
                                                .with_position(pair[0].line, pair[0].col)),
                                        };
                                        env_set(&mut local_env, name.clone(), Value::Boolean(false));
                                        names.push(name);
                                        inits.push(&pair[1]);
                                    }
                                    _ => return Err(EvalError::TypeError("letrec: bad binding".into())
                                        .with_position(b.line, b.col)),
                                }
                            }
                            // Second pass: evaluate inits in local_env (all names visible)
                            for (name, init_expr) in names.iter().zip(inits) {
                                let val = eval(init_expr, &mut local_env, out)?;
                                *local_env.get(name).unwrap().borrow_mut() = val;
                            }
                            for expr in &items[2..items.len() - 1] {
                                eval(expr, &mut local_env, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            tco_env = Some(local_env);
                            continue;
                        }
                        "letrec*" => {
                            if items.len() < 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let bindings = match &items[1].kind {
                                AstKind::List(bs) => bs,
                                _ => return Err(EvalError::TypeError("letrec*: bindings must be a list".into())
                                    .with_position(line, col)),
                            };
                            let mut local_env = e.clone();
                            for b in bindings {
                                match &b.kind {
                                    AstKind::List(pair) if pair.len() == 2 => {
                                        let name = match &pair[0].kind {
                                            AstKind::Symbol(s) => s.clone(),
                                            _ => return Err(EvalError::TypeError("letrec*: binding name must be symbol".into())
                                                .with_position(pair[0].line, pair[0].col)),
                                        };
                                        let val = eval(&pair[1], &mut local_env, out)?;
                                        env_set(&mut local_env, name, val);
                                    }
                                    _ => return Err(EvalError::TypeError("letrec*: bad binding".into())
                                        .with_position(b.line, b.col)),
                                }
                            }
                            for expr in &items[2..items.len() - 1] {
                                eval(expr, &mut local_env, out)?;
                            }
                            cur_ast = items.last().unwrap().clone();
                            tco_env = Some(local_env);
                            continue;
                        }
                        "string-set!" => {
                            return Err(EvalError::TypeError(
                                "string-set!: strings are immutable".into(),
                            ).with_position(line, col));
                        }
                        "cond" => {
                            for clause in &items[1..] {
                                match &clause.kind {
                                    AstKind::List(parts) if parts.len() >= 2 => {
                                        if let AstKind::Symbol(s) = &parts[0].kind {
                                            if s == "else" {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, e, out)?;
                                                }
                                                cur_ast = parts.last().unwrap().clone();
                                                continue 'tco;
                                            }
                                        }
                                        let test = eval(&parts[0], e, out)?;
                                        if test != Value::Boolean(false) {
                                            for expr in &parts[1..parts.len() - 1] {
                                                eval(expr, e, out)?;
                                            }
                                            cur_ast = parts.last().unwrap().clone();
                                            continue 'tco;
                                        }
                                    }
                                    _ => return Err(EvalError::TypeError("cond: bad clause".into())
                                        .with_position(clause.line, clause.col)),
                                }
                            }
                            return Ok(Value::Symbol("ok".into()));
                        }
                        "case" => {
                            if items.len() < 2 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let key = eval(&items[1], e, out)?;
                            for clause in &items[2..] {
                                match &clause.kind {
                                    AstKind::List(parts) if parts.len() >= 2 => {
                                        if let AstKind::Symbol(s) = &parts[0].kind {
                                            if s == "else" {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, e, out)?;
                                                }
                                                cur_ast = parts.last().unwrap().clone();
                                                continue 'tco;
                                            }
                                        }
                                        // parts[0] should be a list of datums
                                        if let AstKind::List(datums) = &parts[0].kind {
                                            let matched = datums.iter().any(|d| {
                                                let dv = ast_to_value(d);
                                                match (&key, &dv) {
                                                    (Value::Integer(x), Value::Integer(y)) => x == y,
                                                    (Value::Boolean(x), Value::Boolean(y)) => x == y,
                                                    (Value::Symbol(x), Value::Symbol(y)) => x == y,
                                                    (Value::Char(x), Value::Char(y)) => x == y,
                                                    _ => false,
                                                }
                                            });
                                            if matched {
                                                for expr in &parts[1..parts.len() - 1] {
                                                    eval(expr, e, out)?;
                                                }
                                                cur_ast = parts.last().unwrap().clone();
                                                continue 'tco;
                                            }
                                        }
                                    }
                                    _ => return Err(EvalError::TypeError("case: bad clause".into())
                                        .with_position(clause.line, clause.col)),
                                }
                            }
                            return Ok(Value::Symbol("ok".into()));
                        }
                        "define-syntax" => {
                            if items.len() != 3 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let name = match &items[1].kind {
                                AstKind::Symbol(s) => s.clone(),
                                _ => return Err(EvalError::TypeError(
                                    "define-syntax: expected symbol".into(),
                                ).with_position(line, col)),
                            };
                            let sr = match &items[2].kind {
                                AstKind::List(parts) => parts,
                                _ => return Err(EvalError::TypeError(
                                    "define-syntax: expected syntax-rules".into(),
                                ).with_position(line, col)),
                            };
                            if sr.is_empty() || !matches!(&sr[0].kind, AstKind::Symbol(s) if s == "syntax-rules") {
                                return Err(EvalError::TypeError(
                                    "define-syntax: expected syntax-rules".into(),
                                ).with_position(line, col));
                            }
                            if sr.len() < 2 {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            let macro_literals = match &sr[1].kind {
                                AstKind::List(lits) => {
                                    lits.iter().map(|l| match &l.kind {
                                        AstKind::Symbol(s) => Ok(s.clone()),
                                        _ => Err(EvalError::TypeError(
                                            "define-syntax: literal must be symbol".into(),
                                        ).with_position(l.line, l.col)),
                                    }).collect::<Result<Vec<_>, _>>()?
                                }
                                _ => return Err(EvalError::TypeError(
                                    "define-syntax: expected literal list".into(),
                                ).with_position(sr[1].line, sr[1].col)),
                            };
                            let mut macro_rules = Vec::new();
                            for clause in &sr[2..] {
                                match &clause.kind {
                                    AstKind::List(pair) if pair.len() == 2 => {
                                        macro_rules.push((pair[0].clone(), pair[1].clone()));
                                    }
                                    _ => return Err(EvalError::TypeError(
                                        "define-syntax: bad rule".into(),
                                    ).with_position(clause.line, clause.col)),
                                }
                            }
                            let macro_val = Value::Macro {
                                literals: macro_literals,
                                rules: macro_rules,
                                def_env: e.clone(),
                            };
                            env_set(e, name, macro_val);
                            return Ok(Value::Symbol("ok".into()));
                        }
                        _ => {}
                    }
                }
                // Check for macro application
                if let AstKind::Symbol(ref s) = items[0].kind {
                    if let Some(Value::Macro { literals, rules, def_env }) = env_get(e, s) {
                        let macro_name = s.clone();
                        for (pattern, template) in &rules {
                            let mut bindings = HashMap::new();
                            let pat_elts = match &pattern.kind {
                                AstKind::List(p) => &p[1..],
                                _ => continue,
                            };
                            if match_syntax_elements(pat_elts, &items[1..], &literals, &mut bindings) {
                                let all_pattern_vars: Vec<String> = pat_elts.iter()
                                    .flat_map(|p| collect_pattern_vars(p, &literals))
                                    .collect();
                                let free_vars = collect_free_template_vars(&template, &all_pattern_vars);
                                let mut hygiene_map = HashMap::new();
                                for fv in &free_vars {
                                    if def_env.contains_key(fv) {
                                        let gs = gensym(fv);
                                        hygiene_map.insert(fv.clone(), gs.clone());
                                        let val = def_env.get(fv).unwrap().borrow().clone();
                                        env_set(e, gs, val);
                                    }
                                }
                                let expanded = instantiate_template(&template, &bindings, &hygiene_map);
                                cur_ast = expanded;
                                continue 'tco;
                            }
                        }
                        return Err(EvalError::TypeError(
                            format!("no matching pattern for macro {}", macro_name),
                        ).with_position(line, col));
                    }
                }
                // General application: evaluate operator and arguments
                let mut args = Vec::new();
                for a in &items[1..] {
                    args.push(eval(a, e, out)?);
                }
                // Try builtin first if operator is a symbol not in env
                // (skip "apply" — it needs special handling below)
                if let AstKind::Symbol(s) = &items[0].kind {
                    if !e.contains_key(s.as_str()) && s != "apply" && s != "call/cc" && s != "call-with-current-continuation" {
                        return apply_builtin(s, &args, out)
                            .map_err(|err| err.with_position(line, col));
                    }
                }
                let func = eval(&items[0], e, out)?;
                match func {
                    Value::Lambda {
                        params,
                        rest_param,
                        body,
                        env: closed_env,
                    } => {
                        let mut local_env = closed_env.clone();
                        if let Some(ref rest) = rest_param {
                            if args.len() < params.len() {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            for (p, a) in params.iter().zip(&args) {
                                env_set(&mut local_env, p.clone(), a.clone());
                            }
                            env_set(&mut local_env, rest.clone(), Value::List(args[params.len()..].to_vec()));
                        } else {
                            if args.len() != params.len() {
                                return Err(EvalError::Arity.with_position(line, col));
                            }
                            for (p, a) in params.iter().zip(args) {
                                env_set(&mut local_env, p.clone(), a);
                            }
                        }
                        cur_ast = *body;
                        tco_env = Some(local_env);
                        continue;
                    }
                    Value::Builtin(ref bname) if bname == "apply" => {
                        if args.len() < 2 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let apply_func = args[0].clone();
                        let last = args.last().unwrap();
                        let tail = match last {
                            Value::List(items) => items.clone(),
                            _ => return Err(EvalError::TypeError("apply: last argument must be a list".into()).with_position(line, col)),
                        };
                        let mut full_args: Vec<Value> = args[1..args.len()-1].to_vec();
                        full_args.extend(tail);
                        match apply_func {
                            Value::Lambda { params, rest_param, body, env: closed_env } => {
                                let mut local_env = closed_env.clone();
                                if let Some(ref rest) = rest_param {
                                    if full_args.len() < params.len() {
                                        return Err(EvalError::Arity.with_position(line, col));
                                    }
                                    for (p, a) in params.iter().zip(&full_args) {
                                        env_set(&mut local_env, p.clone(), a.clone());
                                    }
                                    env_set(&mut local_env, rest.clone(), Value::List(full_args[params.len()..].to_vec()));
                                } else {
                                    if full_args.len() != params.len() {
                                        return Err(EvalError::Arity.with_position(line, col));
                                    }
                                    for (p, a) in params.iter().zip(full_args) {
                                        env_set(&mut local_env, p.clone(), a);
                                    }
                                }
                                cur_ast = *body;
                                tco_env = Some(local_env);
                                continue;
                            }
                            Value::Builtin(ref bname2) => {
                                return apply_builtin(bname2, &full_args, out)
                                    .map_err(|err| err.with_position(line, col));
                            }
                            _ => return Err(EvalError::NotAProcedure.with_position(line, col)),
                        }
                    }
                    Value::Builtin(ref bname) if bname == "call/cc" || bname == "call-with-current-continuation" => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let proc = args.into_iter().next().unwrap();
                        // Check if we're resuming a continuation at this position
                        let resume = CONT_RESUME.with(|r| {
                            let cur = r.borrow();
                            if let Some((_, rl, rc)) = cur.as_ref() {
                                if *rl == line && *rc == col {
                                    drop(cur);
                                    r.borrow_mut().take().map(|(v, _, _)| v)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        });
                        if let Some(val) = resume {
                            return Ok(val);
                        }
                        // Create new continuation
                        let id = NEXT_CONT_ID.with(|c| { let v = c.get(); c.set(v + 1); v });
                        let top_idx = CURRENT_TOP_IDX.with(|c| c.get());
                        let k = Value::Continuation { id, top_expr_idx: top_idx, call_line: line, call_col: col };
                        // Call proc with k
                        match proc {
                            Value::Lambda { params, rest_param, body, env: closed_env } => {
                                let mut local_env = closed_env;
                                if let Some(ref rest) = rest_param {
                                    if params.is_empty() {
                                        env_set(&mut local_env, rest.clone(), Value::List(vec![k]));
                                    } else {
                                        for (i, p) in params.iter().enumerate() {
                                            if i == 0 {
                                                env_set(&mut local_env, p.clone(), k.clone());
                                            }
                                        }
                                        env_set(&mut local_env, rest.clone(), Value::List(vec![]));
                                    }
                                } else {
                                    if params.len() != 1 {
                                        return Err(EvalError::Arity.with_position(line, col));
                                    }
                                    env_set(&mut local_env, params[0].clone(), k);
                                }
                                match eval(&body, &mut local_env, out) {
                                    Ok(v) => return Ok(v),
                                    Err(EvalError::ContinuationJump) => {
                                        let jump = CONT_JUMP.with(|c| c.borrow_mut().take());
                                        if let Some((jid, jval, jtop, jl, jc)) = jump {
                                            if jid == id {
                                                return Ok(jval);
                                            }
                                            CONT_JUMP.with(|c| *c.borrow_mut() = Some((jid, jval, jtop, jl, jc)));
                                        }
                                        return Err(EvalError::ContinuationJump);
                                    }
                                    Err(e) => return Err(e),
                                }
                            }
                            _ => return Err(EvalError::NotAProcedure.with_position(line, col)),
                        }
                    }
                    Value::Continuation { id, top_expr_idx, call_line, call_col } => {
                        if args.len() != 1 {
                            return Err(EvalError::Arity.with_position(line, col));
                        }
                        let val = args.into_iter().next().unwrap();
                        CONT_JUMP.with(|c| *c.borrow_mut() = Some((id, val, top_expr_idx, call_line, call_col)));
                        return Err(EvalError::ContinuationJump);
                    }
                    Value::Builtin(ref bname) => {
                        return apply_builtin(bname, &args, out)
                            .map_err(|err| err.with_position(line, col));
                    }
                    _ => return Err(EvalError::NotAProcedure.with_position(items[0].line, items[0].col)),
                }
            }
        }
    }
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError("expected integer".into())),
    }
}

fn apply_builtin(op: &str, args: &[Value], out: &mut String) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(a)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-expect_integer(&args[0])?));
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                result -= expect_integer(a)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= expect_integer(a)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::Arity);
            }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                let d = expect_integer(a)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? < expect_integer(&args[1])?))
        }
        ">" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? > expect_integer(&args[1])?))
        }
        "=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? == expect_integer(&args[1])?))
        }
        "<=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? <= expect_integer(&args[1])?))
        }
        ">=" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? >= expect_integer(&args[1])?))
        }
        "not" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(args[0] == Value::Boolean(false)))
        }
        "cons" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            match &args[1] {
                Value::List(tail) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(tail.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => Err(EvalError::TypeError("cons: second argument must be a list".into())),
            }
        }
        "car" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(items[0].clone()),
                Value::List(_) => Err(EvalError::TypeError("car: empty list".into())),
                _ => Err(EvalError::TypeError("car: not a pair".into())),
            }
        }
        "cdr" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) if !items.is_empty() => Ok(Value::List(items[1..].to_vec())),
                Value::List(_) => Err(EvalError::TypeError("cdr: empty list".into())),
                _ => Err(EvalError::TypeError("cdr: not a pair".into())),
            }
        }
        "null?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if items.is_empty())))
        }
        "list" => {
            Ok(Value::List(args.to_vec()))
        }
        "length" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) => Ok(Value::Integer(items.len() as i64)),
                _ => Err(EvalError::TypeError("length: not a list".into())),
            }
        }
        "string?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "number?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        "boolean?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::List(items) if !items.is_empty())))
        }
        "symbol?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        "equal?" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "eq?" | "eqv?" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let result = match (&args[0], &args[1]) {
                (Value::Integer(x), Value::Integer(y)) => x == y,
                (Value::Boolean(x), Value::Boolean(y)) => x == y,
                (Value::Symbol(x), Value::Symbol(y)) => x == y,
                (Value::Char(x), Value::Char(y)) => x == y,
                (Value::List(x), Value::List(y)) => x.is_empty() && y.is_empty(),
                _ => false,
            };
            Ok(Value::Boolean(result))
        }
        "display" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => out.push_str(s),
                other => out.push_str(&other.to_scheme_string()),
            }
            Ok(Value::Symbol("ok".into()))
        }
        "write" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            out.push_str(&args[0].to_scheme_string());
            Ok(Value::Symbol("ok".into()))
        }
        "newline" => {
            if !args.is_empty() { return Err(EvalError::Arity); }
            out.push('\n');
            Ok(Value::Symbol("ok".into()))
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::TypeError("string-append: expected string".into())),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::TypeError("string-length: expected string".into())),
            }
        }
        "substring" => {
            if args.len() != 3 { return Err(EvalError::Arity); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::TypeError("substring: expected string".into())),
            };
            let start = expect_integer(&args[1])? as usize;
            let end = expect_integer(&args[2])? as usize;
            if start > end || end > s.len() {
                return Err(EvalError::TypeError("substring: index out of range".into()));
            }
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::TypeError("string->number: expected string".into())),
            }
        }
        "number->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            let n = expect_integer(&args[0])?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::TypeError("symbol->string: expected symbol".into())),
            }
        }
        "string->symbol" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::TypeError("string->symbol: expected string".into())),
            }
        }
        "string-copy" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::TypeError("string-copy: expected string".into())),
            }
        }
        "string-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::TypeError("string-ref: expected string".into())),
            };
            let idx = expect_integer(&args[1])? as usize;
            if idx >= s.len() {
                return Err(EvalError::TypeError("string-ref: index out of range".into()));
            }
            Ok(Value::Char(s.as_bytes()[idx] as char))
        }
        "char?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "map" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let func = &args[0];
            let items = match &args[1] {
                Value::List(items) => items,
                _ => return Err(EvalError::TypeError("map: expected list".into())),
            };
            let mut results = Vec::new();
            for item in items {
                match func {
                    Value::Lambda { params, body, env: closed_env, .. } => {
                        if params.len() != 1 {
                            return Err(EvalError::Arity);
                        }
                        let mut local_env = closed_env.clone();
                        env_set(&mut local_env, params[0].clone(), item.clone());
                        results.push(eval(&body, &mut local_env, out)?);
                    }
                    _ => return Err(EvalError::NotAProcedure),
                }
            }
            Ok(Value::List(results))
        }
        "string->list" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Str(s) => Ok(Value::List(s.chars().map(Value::Char).collect())),
                _ => Err(EvalError::TypeError("string->list: expected string".into())),
            }
        }
        "list->string" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) => {
                    let mut s = String::new();
                    for item in items {
                        match item {
                            Value::Char(c) => s.push(*c),
                            _ => return Err(EvalError::TypeError("list->string: expected list of characters".into())),
                        }
                    }
                    Ok(Value::Str(s))
                }
                _ => Err(EvalError::TypeError("list->string: expected list".into())),
            }
        }
        "char->integer" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Char(c) => Ok(Value::Integer(*c as i64)),
                _ => Err(EvalError::TypeError("char->integer: expected character".into())),
            }
        }
        "integer->char" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            let n = expect_integer(&args[0])?;
            Ok(Value::Char(char::from_u32(n as u32).unwrap_or('\0')))
        }
        "vector" => {
            Ok(Value::Vector(Rc::new(RefCell::new(args.to_vec()))))
        }
        "make-vector" => {
            if args.is_empty() || args.len() > 2 { return Err(EvalError::Arity); }
            let len = expect_integer(&args[0])? as usize;
            let fill = if args.len() == 2 { args[1].clone() } else { Value::Integer(0) };
            Ok(Value::Vector(Rc::new(RefCell::new(vec![fill; len]))))
        }
        "vector-ref" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = expect_integer(&args[1])? as usize;
                    let items = v.borrow();
                    if idx >= items.len() {
                        return Err(EvalError::TypeError("vector-ref: index out of range".into()));
                    }
                    Ok(items[idx].clone())
                }
                _ => Err(EvalError::TypeError("vector-ref: expected vector".into())),
            }
        }
        "vector-set!" => {
            if args.len() != 3 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Vector(v) => {
                    let idx = expect_integer(&args[1])? as usize;
                    let mut items = v.borrow_mut();
                    if idx >= items.len() {
                        return Err(EvalError::TypeError("vector-set!: index out of range".into()));
                    }
                    items[idx] = args[2].clone();
                    Ok(Value::Symbol("ok".into()))
                }
                _ => Err(EvalError::TypeError("vector-set!: expected vector".into())),
            }
        }
        "vector-length" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Vector(v) => Ok(Value::Integer(v.borrow().len() as i64)),
                _ => Err(EvalError::TypeError("vector-length: expected vector".into())),
            }
        }
        "vector?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(matches!(&args[0], Value::Vector(_))))
        }
        "vector->list" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::Vector(v) => Ok(Value::List(v.borrow().clone())),
                _ => Err(EvalError::TypeError("vector->list: expected vector".into())),
            }
        }
        "list->vector" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            match &args[0] {
                Value::List(items) => Ok(Value::Vector(Rc::new(RefCell::new(items.clone())))),
                _ => Err(EvalError::TypeError("list->vector: expected list".into())),
            }
        }
        "abs" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Integer(expect_integer(&args[0])?.abs()))
        }
        "modulo" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let a = expect_integer(&args[0])?;
            let b = expect_integer(&args[1])?;
            if b == 0 { return Err(EvalError::DivisionByZero); }
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() { return Err(EvalError::Arity); }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                let v = expect_integer(a)?;
                if v < result { result = v; }
            }
            Ok(Value::Integer(result))
        }
        "max" => {
            if args.is_empty() { return Err(EvalError::Arity); }
            let mut result = expect_integer(&args[0])?;
            for a in &args[1..] {
                let v = expect_integer(a)?;
                if v > result { result = v; }
            }
            Ok(Value::Integer(result))
        }
        "expt" => {
            if args.len() != 2 { return Err(EvalError::Arity); }
            let base = expect_integer(&args[0])?;
            let exp = expect_integer(&args[1])?;
            if exp < 0 { return Err(EvalError::TypeError("expt: negative exponent".into())); }
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? == 0))
        }
        "positive?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? > 0))
        }
        "negative?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? < 0))
        }
        "odd?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 { return Err(EvalError::Arity); }
            Ok(Value::Boolean(expect_integer(&args[0])? % 2 == 0))
        }
        _ => Err(EvalError::UndefinedVariable(op.to_string())),
    }
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    let mut asts = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (ast, next) = parse(&tokens, pos)?;
        asts.push(ast);
        pos = next;
    }
    if asts.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    NEXT_CONT_ID.with(|c| c.set(1));
    CONT_RESUME.with(|r| *r.borrow_mut() = None);
    CONT_JUMP.with(|c| *c.borrow_mut() = None);
    GENSYM_COUNTER.with(|c| c.set(0));
    let mut env = Env::new();
    let mut out = String::new();
    let mut last_result = None;
    let mut idx = 0;
    while idx < asts.len() {
        CURRENT_TOP_IDX.with(|c| c.set(idx));
        match eval(&asts[idx], &mut env, &mut out) {
            Ok(v) => {
                last_result = Some(v);
                idx += 1;
            }
            Err(EvalError::ContinuationJump) => {
                let jump = CONT_JUMP.with(|c| c.borrow_mut().take());
                if let Some((_id, val, top_idx, cl, cc)) = jump {
                    CONT_RESUME.with(|r| *r.borrow_mut() = Some((val, cl, cc)));
                    idx = top_idx;
                } else {
                    return Err(EvalError::ContinuationJump);
                }
            }
            Err(e) => return Err(e),
        }
    }
    match last_result {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(input: &str) -> Result<(String, String), EvalError> {
    let tokens = tokenize(input);
    let mut asts = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (ast, next) = parse(&tokens, pos)?;
        asts.push(ast);
        pos = next;
    }
    if asts.is_empty() {
        return Err(EvalError::Parse("empty input".into()));
    }
    NEXT_CONT_ID.with(|c| c.set(1));
    CONT_RESUME.with(|r| *r.borrow_mut() = None);
    CONT_JUMP.with(|c| *c.borrow_mut() = None);
    GENSYM_COUNTER.with(|c| c.set(0));
    let mut env = Env::new();
    let mut out = String::new();
    let mut last_result = None;
    let mut idx = 0;
    while idx < asts.len() {
        CURRENT_TOP_IDX.with(|c| c.set(idx));
        match eval(&asts[idx], &mut env, &mut out) {
            Ok(v) => {
                last_result = Some(v);
                idx += 1;
            }
            Err(EvalError::ContinuationJump) => {
                let jump = CONT_JUMP.with(|c| c.borrow_mut().take());
                if let Some((_id, val, top_idx, cl, cc)) = jump {
                    CONT_RESUME.with(|r| *r.borrow_mut() = Some((val, cl, cc)));
                    idx = top_idx;
                } else {
                    return Err(EvalError::ContinuationJump);
                }
            }
            Err(e) => return Err(e),
        }
    }
    match last_result {
        Some(v) => Ok((v.to_scheme_string(), out)),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

#[cfg(test)]
mod tests;
