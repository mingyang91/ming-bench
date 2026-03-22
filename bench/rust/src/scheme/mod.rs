pub mod error;

pub use error::EvalError;

use std::collections::HashMap;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Boolean(false))
    }

    fn as_integer(&self, context: &str) -> Result<i64, EvalError> {
        match self {
            Value::Integer(n) => Ok(*n),
            other => Err(EvalError::TypeError {
                message: format!("{context}: expected number, got {}", other.type_name()),
            }),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "number",
            Value::Boolean(_) => "boolean",
            Value::Str(_) => "string",
            Value::Symbol(_) => "symbol",
            Value::List(_) => "list",
        }
    }

    fn display(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
            Value::Symbol(s) => s.clone(),
            Value::List(items) => {
                let inner: Vec<String> = items.iter().map(|v| v.display()).collect();
                format!("({})", inner.join(" "))
            }
        }
    }
}

// --- Tokenizer ---

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
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
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
                            c => {
                                s.push('\\');
                                s.push(c);
                            }
                        }
                    } else {
                        s.push(chars[i]);
                    }
                    i += 1;
                }
                if i >= chars.len() {
                    return Err(EvalError::Parse {
                        message: "unterminated string".to_string(),
                    });
                }
                i += 1; // closing quote
                tokens.push(Token::Str(s));
            }
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(Token::Boolean(true));
                            i += 2;
                        }
                        'f' => {
                            tokens.push(Token::Boolean(false));
                            i += 2;
                        }
                        _ => {
                            return Err(EvalError::Parse {
                                message: format!("unexpected character after #: {}", chars[i + 1]),
                            });
                        }
                    }
                } else {
                    return Err(EvalError::Parse {
                        message: "unexpected end after #".to_string(),
                    });
                }
            }
            _ => {
                // Symbol or number
                let start = i;
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
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

// --- Parser ---

fn parse(tokens: &[Token], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse {
            message: "unexpected end of input".to_string(),
        });
    }

    match &tokens[pos] {
        Token::Integer(n) => Ok((Value::Integer(*n), pos + 1)),
        Token::Boolean(b) => Ok((Value::Boolean(*b), pos + 1)),
        Token::Str(s) => Ok((Value::Str(s.clone()), pos + 1)),
        Token::Symbol(s) => Ok((Value::Symbol(s.clone()), pos + 1)),
        Token::LParen => {
            let mut items = Vec::new();
            let mut i = pos + 1;
            loop {
                if i >= tokens.len() {
                    return Err(EvalError::Parse {
                        message: "unclosed parenthesis".to_string(),
                    });
                }
                if tokens[i] == Token::RParen {
                    return Ok((Value::List(items), i + 1));
                }
                let (val, next) = parse(tokens, i)?;
                items.push(val);
                i = next;
            }
        }
        Token::RParen => Err(EvalError::Parse {
            message: "unexpected )".to_string(),
        }),
    }
}

fn parse_all(input: &str) -> Result<Vec<Value>, EvalError> {
    let tokens = tokenize(input)?;
    let mut exprs = Vec::new();
    let mut pos = 0;
    while pos < tokens.len() {
        let (expr, next) = parse(&tokens, pos)?;
        exprs.push(expr);
        pos = next;
    }
    Ok(exprs)
}

// --- Evaluator ---

type Env = HashMap<String, Value>;

fn default_env() -> Env {
    Env::new()
}

fn eval(expr: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) => Ok(expr.clone()),
        Value::Symbol(name) => env.get(name).cloned().ok_or_else(|| EvalError::UnboundVariable {
            name: name.clone(),
        }),
        Value::List(items) => {
            if items.is_empty() {
                return Err(EvalError::Parse {
                    message: "empty application".to_string(),
                });
            }

            // Check for special forms
            if let Value::Symbol(op) = &items[0] {
                match op.as_str() {
                    "+" => return builtin_add(&items[1..], env),
                    "-" => return builtin_sub(&items[1..], env),
                    "*" => return builtin_mul(&items[1..], env),
                    "/" => return builtin_div(&items[1..], env),
                    "<" => return builtin_cmp(&items[1..], env, "<"),
                    ">" => return builtin_cmp(&items[1..], env, ">"),
                    "=" => return builtin_cmp(&items[1..], env, "="),
                    "<=" => return builtin_cmp(&items[1..], env, "<="),
                    "not" => return builtin_not(&items[1..], env),
                    "and" => return builtin_and(&items[1..], env),
                    "or" => return builtin_or(&items[1..], env),
                    _ => {}
                }
            }

            Err(EvalError::UnboundVariable {
                name: items[0].display(),
            })
        }
    }
}

fn eval_args(args: &[Value], env: &mut Env) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|a| eval(a, env)).collect()
}

fn builtin_add(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    let mut sum: i64 = 0;
    for v in &vals {
        sum += v.as_integer("+")?;
    }
    Ok(Value::Integer(sum))
}

fn builtin_sub(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.is_empty() {
        return Err(EvalError::Arity {
            procedure: "-".to_string(),
            expected: "at least 1".to_string(),
            got: 0,
        });
    }
    if vals.len() == 1 {
        return Ok(Value::Integer(-vals[0].as_integer("-")?));
    }
    let mut result = vals[0].as_integer("-")?;
    for v in &vals[1..] {
        result -= v.as_integer("-")?;
    }
    Ok(Value::Integer(result))
}

fn builtin_mul(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    let mut product: i64 = 1;
    for v in &vals {
        product *= v.as_integer("*")?;
    }
    Ok(Value::Integer(product))
}

fn builtin_div(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: "/".to_string(),
            expected: "2".to_string(),
            got: vals.len(),
        });
    }
    let a = vals[0].as_integer("/")?;
    let b = vals[1].as_integer("/")?;
    if b == 0 {
        return Err(EvalError::DivisionByZero);
    }
    Ok(Value::Integer(a / b))
}

fn builtin_cmp(args: &[Value], env: &mut Env, op: &str) -> Result<Value, EvalError> {
    let vals = eval_args(args, env)?;
    if vals.len() != 2 {
        return Err(EvalError::Arity {
            procedure: op.to_string(),
            expected: "2".to_string(),
            got: vals.len(),
        });
    }
    let a = vals[0].as_integer(op)?;
    let b = vals[1].as_integer(op)?;
    let result = match op {
        "<" => a < b,
        ">" => a > b,
        "=" => a == b,
        "<=" => a <= b,
        ">=" => a >= b,
        other => unreachable!("unknown comparison operator: {other}"),
    };
    Ok(Value::Boolean(result))
}

fn builtin_not(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::Arity {
            procedure: "not".to_string(),
            expected: "1".to_string(),
            got: args.len(),
        });
    }
    let val = eval(&args[0], env)?;
    Ok(Value::Boolean(!val.is_truthy()))
}

fn builtin_and(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(true);
    for arg in args {
        result = eval(arg, env)?;
        if !result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

fn builtin_or(args: &[Value], env: &mut Env) -> Result<Value, EvalError> {
    let mut result = Value::Boolean(false);
    for arg in args {
        result = eval(arg, env)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(result)
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let exprs = parse_all(input)?;
    if exprs.is_empty() {
        return Err(EvalError::Parse {
            message: "no expressions".to_string(),
        });
    }
    let mut env = default_env();
    let mut result = Value::Boolean(false);
    for expr in &exprs {
        result = eval(expr, &mut env)?;
    }
    Ok(result.display())
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
