pub mod error;

pub use error::EvalError;

/// A Scheme value.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    Str(String),
}

impl Value {
    fn to_scheme_string(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => "#t".to_string(),
            Value::Boolean(false) => "#f".to_string(),
            Value::Str(s) => format!("\"{}\"", s),
        }
    }
}

/// Tokenize input into a list of token strings.
fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '"' => {
                let mut s = String::new();
                s.push('"');
                i += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        s.push(chars[i]);
                        s.push(chars[i + 1]);
                        i += 2;
                    } else {
                        s.push(chars[i]);
                        i += 1;
                    }
                }
                if i < chars.len() {
                    s.push('"');
                    i += 1;
                }
                tokens.push(s);
            }
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '(' | ')' | '\'' => {
                tokens.push(chars[i].to_string());
                i += 1;
            }
            _ => {
                let mut s = String::new();
                while i < chars.len()
                    && !matches!(chars[i], ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';')
                {
                    s.push(chars[i]);
                    i += 1;
                }
                tokens.push(s);
            }
        }
    }
    tokens
}

/// Parse a single expression from tokens, returning (value, next_index).
fn parse(tokens: &[String], pos: usize) -> Result<(Value, usize), EvalError> {
    if pos >= tokens.len() {
        return Err(EvalError::Parse("unexpected end of input".into()));
    }
    let token = &tokens[pos];
    if token == "#t" {
        Ok((Value::Boolean(true), pos + 1))
    } else if token == "#f" {
        Ok((Value::Boolean(false), pos + 1))
    } else if token.starts_with('"') {
        // String literal — strip surrounding quotes
        let inner = &token[1..token.len() - 1];
        Ok((Value::Str(inner.to_string()), pos + 1))
    } else if let Ok(n) = token.parse::<i64>() {
        Ok((Value::Integer(n), pos + 1))
    } else {
        Err(EvalError::Parse(format!("unexpected token: {}", token)))
    }
}

/// Evaluate a single parsed value (for L1, values are self-evaluating).
fn eval(value: &Value) -> Result<Value, EvalError> {
    Ok(value.clone())
}

/// Evaluate one or more Scheme expressions and return the string
/// representation of the last result.
pub fn eval_str(input: &str) -> Result<String, EvalError> {
    let tokens = tokenize(input);
    let mut pos = 0;
    let mut last_result = None;
    while pos < tokens.len() {
        let (val, next) = parse(&tokens, pos)?;
        let result = eval(&val)?;
        last_result = Some(result);
        pos = next;
    }
    match last_result {
        Some(v) => Ok(v.to_scheme_string()),
        None => Err(EvalError::Parse("empty input".into())),
    }
}

/// Evaluate Scheme expressions, returning both the result value and
/// any output produced by `display`, `write`, or `newline`.
pub fn eval_str_with_output(_input: &str) -> Result<(String, String), EvalError> {
    todo!()
}

#[cfg(test)]
mod tests;
