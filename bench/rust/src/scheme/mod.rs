mod builtins;
pub mod error;

pub use error::EvalError;

use builtins::{
    builtin_add, builtin_and, builtin_cmp, builtin_div, builtin_mul, builtin_not, builtin_or,
    builtin_sub,
};
use std::collections::HashMap;

type Env = HashMap<String, Value>;

/// A Scheme value.
#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Symbol(String),
    List(Vec<Value>),
    Lambda {
        name: Option<String>,
        params: Vec<String>,
        body: Vec<Value>,
        closure_env: Env,
    },
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
            Value::Lambda { .. } => "procedure",
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
            Value::Lambda { .. } => "#<procedure>".to_string(),
        }
    }
}

// --- Tokenizer ---

#[derive(Debug, Clone, PartialEq)]
enum Token {
    LParen,
    RParen,
    Quote,
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
            '\'' => {
                tokens.push(Token::Quote);
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
        Token::Quote => {
            let (val, next) = parse(tokens, pos + 1)?;
            Ok((Value::List(vec![Value::Symbol("quote".to_string()), val]), next))
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

fn default_env() -> Env {
    Env::new()
}

fn eval(expr: &Value, env: &mut Env) -> Result<Value, EvalError> {
    match expr {
        Value::Integer(_) | Value::Boolean(_) | Value::Str(_) | Value::Lambda { .. } => Ok(expr.clone()),
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
                    "quote" => {
                        if items.len() != 2 {
                            return Err(EvalError::Arity {
                                procedure: "quote".to_string(),
                                expected: "1".to_string(),
                                got: items.len() - 1,
                            });
                        }
                        return Ok(items[1].clone());
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(EvalError::Parse {
                                message: "if requires 2 or 3 arguments".to_string(),
                            });
                        }
                        let cond = eval(&items[1], env)?;
                        if cond.is_truthy() {
                            return eval(&items[2], env);
                        } else if items.len() == 4 {
                            return eval(&items[3], env);
                        }
                        return Ok(Value::Boolean(false));
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(EvalError::Parse {
                                message: "define requires at least 2 arguments".to_string(),
                            });
                        }
                        match &items[1] {
                            Value::Symbol(name) => {
                                let mut val = eval(&items[2], env)?;
                                // Tag lambdas with their name for self-recursion
                                if let Value::Lambda { name: ref mut n, .. } = val {
                                    *n = Some(name.clone());
                                }
                                env.insert(name.clone(), val);
                                return Ok(Value::Boolean(false));
                            }
                            Value::List(sig) => {
                                // (define (f params...) body...)
                                if sig.is_empty() {
                                    return Err(EvalError::Parse {
                                        message: "define: empty signature".to_string(),
                                    });
                                }
                                let name = match &sig[0] {
                                    Value::Symbol(n) => n.clone(),
                                    other => return Err(EvalError::TypeError {
                                        message: format!("define: expected symbol for name, got {}", other.type_name()),
                                    }),
                                };
                                let params: Vec<String> = sig[1..].iter().map(|p| {
                                    match p {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        other => Err(EvalError::TypeError {
                                            message: format!("define: expected symbol for parameter, got {}", other.type_name()),
                                        }),
                                    }
                                }).collect::<Result<_, _>>()?;
                                let body: Vec<Value> = items[2..].to_vec();
                                let closure = Value::Lambda {
                                    name: Some(name.clone()),
                                    params,
                                    body,
                                    closure_env: env.clone(),
                                };
                                env.insert(name, closure);
                                return Ok(Value::Boolean(false));
                            }
                            other => return Err(EvalError::TypeError {
                                message: format!("define: expected symbol or list, got {}", other.type_name()),
                            }),
                        }
                    }
                    "lambda" => {
                        if items.len() < 3 {
                            return Err(EvalError::Parse {
                                message: "lambda requires params and body".to_string(),
                            });
                        }
                        let params = match &items[1] {
                            Value::List(param_list) => {
                                param_list.iter().map(|p| {
                                    match p {
                                        Value::Symbol(s) => Ok(s.clone()),
                                        other => Err(EvalError::TypeError {
                                            message: format!("lambda: expected symbol for parameter, got {}", other.type_name()),
                                        }),
                                    }
                                }).collect::<Result<Vec<_>, _>>()?
                            }
                            other => return Err(EvalError::TypeError {
                                message: format!("lambda: expected parameter list, got {}", other.type_name()),
                            }),
                        };
                        let body: Vec<Value> = items[2..].to_vec();
                        return Ok(Value::Lambda {
                            name: None,
                            params,
                            body,
                            closure_env: env.clone(),
                        });
                    }
                    "+" => return builtin_add(&items[1..], env),
                    "-" => return builtin_sub(&items[1..], env),
                    "*" => return builtin_mul(&items[1..], env),
                    "/" => return builtin_div(&items[1..], env),
                    "<" => return builtin_cmp(&items[1..], env, "<"),
                    ">" => return builtin_cmp(&items[1..], env, ">"),
                    "=" => return builtin_cmp(&items[1..], env, "="),
                    "<=" => return builtin_cmp(&items[1..], env, "<="),
                    ">=" => return builtin_cmp(&items[1..], env, ">="),
                    "not" => return builtin_not(&items[1..], env),
                    "and" => return builtin_and(&items[1..], env),
                    "or" => return builtin_or(&items[1..], env),
                    _ => {}
                }
            }

            // Function application
            let func = eval(&items[0], env)?;
            let args = eval_args(&items[1..], env)?;
            apply_function(&func, &args)
        }
    }
}

fn apply_function(func: &Value, args: &[Value]) -> Result<Value, EvalError> {
    match func {
        Value::Lambda { name, params, body, closure_env } => {
            if params.len() != args.len() {
                return Err(EvalError::Arity {
                    procedure: name.as_deref().unwrap_or("lambda").to_string(),
                    expected: params.len().to_string(),
                    got: args.len(),
                });
            }
            let mut local_env = closure_env.clone();
            // Inject self-reference for recursion
            if let Some(fn_name) = name {
                local_env.insert(fn_name.clone(), func.clone());
            }
            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.insert(param.clone(), arg.clone());
            }
            let mut result = Value::Boolean(false);
            for expr in body {
                result = eval(expr, &mut local_env)?;
            }
            Ok(result)
        }
        other => Err(EvalError::TypeError {
            message: format!("not a procedure: {}", other.display()),
        }),
    }
}

fn eval_args(args: &[Value], env: &mut Env) -> Result<Vec<Value>, EvalError> {
    args.iter().map(|a| eval(a, env)).collect()
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
