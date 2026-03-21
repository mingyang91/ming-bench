pub mod error;

pub use error::EvalError;

// ---------------------------------------------------------------------------
// Value representation
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Void,
}

impl Value {
    fn display_scheme(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(elems) => {
                let inner: Vec<String> = elems.iter().map(|v| v.display_scheme()).collect();
                format!("({})", inner.join(" "))
            }
            Value::Void => "".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Str(String),
}

fn tokenize(input: &str) -> Result<Vec<Token>, EvalError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            ';' => {
                // line comment
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => { tokens.push(Token::LParen); i += 1; }
            ')' => { tokens.push(Token::RParen); i += 1; }
            '"' => {
                i += 1;
                let mut s = String::new();
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            c => { s.push('\\'); s.push(c); }
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse("unterminated string".into()));
                }
                i += 1; // closing quote
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => { tokens.push(Token::Boolean(true)); i += 2; }
                        'f' => { tokens.push(Token::Boolean(false)); i += 2; }
                        _ => return Err(EvalError::Parse(format!("unexpected #{}", chars[i+1]))),
                    }
                } else {
                    return Err(EvalError::Parse("unexpected #".into()));
                }
            }
            _ => {
                // symbol or number
                let start = i;
                while i < chars.len() && !matches!(chars[i], ' '|'\t'|'\n'|'\r'|'('|')'|'"'|';') {
                    i += 1;
                }
                let word: String = chars[start..i].iter().collect();
                if let Ok(n) = word.parse::<i64>() {
                    tokens.push(Token::Integer(n));
                } else {
                    tokens.push(Token::Symbol(word));
                }
            }
        }
    }
    Ok(tokens)
}

// ---------------------------------------------------------------------------
// Parser — tokens → Value (s-expression)
// ---------------------------------------------------------------------------

fn parse(tokens: &[Token], pos: &mut usize) -> Result<Value, EvalError> {
    if *pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    match &tokens[*pos] {
        Token::Integer(n) => { let v = Value::Integer(*n); *pos += 1; Ok(v) }
        Token::Boolean(b) => { let v = Value::Boolean(*b); *pos += 1; Ok(v) }
        Token::Str(s) => { let v = Value::Str(s.clone()); *pos += 1; Ok(v) }
        Token::Symbol(s) => { let v = Value::Symbol(s.clone()); *pos += 1; Ok(v) }
        Token::LParen => {
            *pos += 1;
            let mut elems = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                elems.push(parse(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(EvalError::Parse("missing closing paren".into()));
            }
            *pos += 1; // RParen
            Ok(Value::List(elems))
        }
        Token::RParen => Err(EvalError::Parse("unexpected )".into())),
    }
}

fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse(&tokens, &mut pos)?);
    }
    Ok(exprs)
}

// ---------------------------------------------------------------------------
// Evaluator
// ---------------------------------------------------------------------------

fn eval(expr: &Value) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => Err(EvalError::UnboundVariable(name.clone())),
        Value::List(elems) => {
            if elems.is_empty() {
                return Err(EvalError::Parse("empty application".into()));
            }
            // Check for special forms and builtins by symbol name
            if let Value::Symbol(op) = &elems[0] {
                match op.as_str() {
                    "and" => return eval_and(&elems[1..]),
                    "or" => return eval_or(&elems[1..]),
                    "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">=" | "not" => {
                        return eval_builtin(op, &elems[1..]);
                    }
                    _ => {}
                }
            }
            // General function application
            let func = eval(&elems[0])?;
            Err(EvalError::NotAProcedure(func.display_scheme()))
        }
        Value::Void => Ok(Value::Void),
    }
}

fn eval_builtin(op: &str, args: &[Value]) -> Result<Value, EvalError> {
    match op {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += expect_integer(&eval(a)?)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0 });
            }
            let first = expect_integer(&eval(&args[0])?)?;
            if args.len() == 1 {
                return Ok(Value::Integer(-first));
            }
            let mut result = first;
            for a in &args[1..] {
                result -= expect_integer(&eval(a)?)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut prod: i64 = 1;
            for a in args {
                prod *= expect_integer(&eval(a)?)?;
            }
            Ok(Value::Integer(prod))
        }
        "/" => {
            if args.is_empty() {
                return Err(EvalError::WrongArgCount { expected: "at least 1".into(), got: 0 });
            }
            let first = expect_integer(&eval(&args[0])?)?;
            if args.len() == 1 {
                if first == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                return Ok(Value::Integer(1 / first));
            }
            let mut result = first;
            for a in &args[1..] {
                let d = expect_integer(&eval(a)?)?;
                if d == 0 {
                    return Err(EvalError::DivisionByZero);
                }
                result /= d;
            }
            Ok(Value::Integer(result))
        }
        "<" => compare_op(args, |a, b| a < b),
        ">" => compare_op(args, |a, b| a > b),
        "=" => compare_op(args, |a, b| a == b),
        "<=" => compare_op(args, |a, b| a <= b),
        ">=" => compare_op(args, |a, b| a >= b),
        "not" => {
            if args.len() != 1 {
                return Err(EvalError::WrongArgCount { expected: "1".into(), got: args.len() });
            }
            let val = eval(&args[0])?;
            Ok(Value::Boolean(!val.is_truthy()))
        }
        _ => Err(EvalError::UnboundVariable(op.to_string())),
    }
}

fn eval_and(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(true));
    }
    let mut result = Value::Boolean(true);
    for a in args {
        result = eval(a)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_or(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        return Ok(Value::Boolean(false));
    }
    let mut result = Value::Boolean(false);
    for a in args {
        result = eval(a)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn compare_op(args: &[Value], cmp: fn(i64, i64) -> bool) -> Result<Value, EvalError> {
    if args.len() < 2 {
        return Err(EvalError::WrongArgCount { expected: "at least 2".into(), got: args.len() });
    }
    let mut prev = expect_integer(&eval(&args[0])?)?;
    for a in &args[1..] {
        let cur = expect_integer(&eval(a)?)?;
        if !cmp(prev, cur) {
            return Ok(Value::Boolean(false));
        }
        prev = cur;
    }
    Ok(Value::Boolean(true))
}

fn expect_integer(v: &Value) -> Result<i64, EvalError> {
    match v {
        Value::Integer(n) => Ok(*n),
        _ => Err(EvalError::TypeError(format!("expected integer, got {}", v.display_scheme()))),
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    let mut last = Value::Void;
    for expr in &exprs {
        last = eval(expr)?;
    }
    Ok(last.display_scheme())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
