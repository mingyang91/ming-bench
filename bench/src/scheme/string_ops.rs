use std::rc::Rc;

use crate::scheme::builtins::expect_integer;
use crate::scheme::env::Env;
use crate::scheme::error::EvalError;
use crate::scheme::eval;
use crate::scheme::value::Value;

fn expect_string(val: &Value) -> Result<&str, EvalError> {
    match val {
        Value::String(s) => Ok(s.as_str()),
        other => Err(EvalError::TypeError {
            expected: "string".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(string-append s ...)` — concatenate strings.
pub fn eval_string_append(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let vals: Vec<Value> = args.iter().map(|a| eval(a, env)).collect::<Result<_, _>>()?;
    let result: String = vals
        .iter()
        .map(expect_string)
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(Value::String(result))
}

/// Evaluate `(string-length s)` — length of a string.
pub fn eval_string_length(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    Ok(Value::Integer(s.len() as i64))
}

/// Evaluate `(substring s start end)` — extract a substring.
pub fn eval_substring(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [s_expr, start_expr, end_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "3".into(),
            got: args.len(),
        });
    };
    let s_val = eval(s_expr, env)?;
    let s = expect_string(&s_val)?;
    let start = expect_integer(&eval(start_expr, env)?)? as usize;
    let end = expect_integer(&eval(end_expr, env)?)? as usize;
    if start > end || end > s.len() {
        return Err(EvalError::TypeError {
            expected: format!("valid substring indices (0..{})", s.len()),
            got: format!("{start}..{end}"),
        });
    }
    Ok(Value::String(s[start..end].to_string()))
}

/// Evaluate `(string->number s)` — parse a string as a number.
pub fn eval_string_to_number(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    match s.parse::<i64>() {
        Ok(n) => Ok(Value::Integer(n)),
        Err(_) => Ok(Value::Boolean(false)),
    }
}

/// Evaluate `(number->string n)` — convert a number to a string.
pub fn eval_number_to_string(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let n = expect_integer(&val)?;
    Ok(Value::String(n.to_string()))
}

/// Evaluate `(symbol->string sym)` — convert a symbol to a string.
pub fn eval_symbol_to_string(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    match val {
        Value::Symbol(s) => Ok(Value::String(s)),
        other => Err(EvalError::TypeError {
            expected: "symbol".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(string->symbol s)` — convert a string to a symbol.
pub fn eval_string_to_symbol(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    Ok(Value::Symbol(s.to_string()))
}

/// Evaluate `(string-copy s)` — return a copy of the string.
pub fn eval_string_copy(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    Ok(Value::String(s.to_string()))
}

/// Evaluate `(string-set! ...)` — R7RS: strings are immutable, always errors.
pub fn eval_string_set(args: &[Value], _env: &Rc<Env>) -> Result<Value, EvalError> {
    let [_, _, _] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "3".into(),
            got: args.len(),
        });
    };
    Err(EvalError::ImmutableString)
}

/// Evaluate `(string->list s)` — convert string to list of characters.
pub fn eval_string_to_list(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    let chars = s.chars().map(Value::Char).collect();
    Ok(Value::List(chars))
}

/// Evaluate `(list->string lst)` — convert list of characters to string.
pub fn eval_list_to_string(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::List(elems) = val else {
        return Err(EvalError::TypeError {
            expected: "list".into(),
            got: format!("{val}"),
        });
    };
    let s: String = elems
        .iter()
        .map(|v| match v {
            Value::Char(c) => Ok(*c),
            other => Err(EvalError::TypeError {
                expected: "char".into(),
                got: format!("{other}"),
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(Value::String(s))
}

/// Evaluate `(char->integer c)` — character to integer code point.
pub fn eval_char_to_integer(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let Value::Char(c) = val else {
        return Err(EvalError::TypeError {
            expected: "char".into(),
            got: format!("{val}"),
        });
    };
    Ok(Value::Integer(c as i64))
}

/// Evaluate `(integer->char n)` — integer code point to character.
pub fn eval_integer_to_char(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let n = expect_integer(&val)?;
    let c = char::from_u32(n as u32).ok_or_else(|| EvalError::TypeError {
        expected: "valid Unicode code point".into(),
        got: format!("{n}"),
    })?;
    Ok(Value::Char(c))
}

/// Evaluate `(string-ref s k)` — character at index k.
pub fn eval_string_ref(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [s_expr, k_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let s_val = eval(s_expr, env)?;
    let s = expect_string(&s_val)?;
    let k = expect_integer(&eval(k_expr, env)?)? as usize;
    s.chars().nth(k).map(Value::Char).ok_or_else(|| EvalError::TypeError {
        expected: format!("index < {}", s.len()),
        got: format!("{k}"),
    })
}

fn expect_char(val: &Value) -> Result<char, EvalError> {
    match val {
        Value::Char(c) => Ok(*c),
        other => Err(EvalError::TypeError {
            expected: "char".into(),
            got: format!("{other}"),
        }),
    }
}

/// Evaluate `(char=? c1 c2)` — character equality.
pub fn eval_char_eq(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a = expect_char(&eval(a_expr, env)?)?;
    let b = expect_char(&eval(b_expr, env)?)?;
    Ok(Value::Boolean(a == b))
}

/// Evaluate `(char<? c1 c2)` — character ordering.
pub fn eval_char_lt(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a = expect_char(&eval(a_expr, env)?)?;
    let b = expect_char(&eval(b_expr, env)?)?;
    Ok(Value::Boolean(a < b))
}

/// Evaluate `(char-alphabetic? c)`.
pub fn eval_char_alphabetic(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [c_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let c = expect_char(&eval(c_expr, env)?)?;
    Ok(Value::Boolean(c.is_ascii_alphabetic()))
}

/// Evaluate `(char-numeric? c)`.
pub fn eval_char_numeric(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [c_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let c = expect_char(&eval(c_expr, env)?)?;
    Ok(Value::Boolean(c.is_ascii_digit()))
}

/// Evaluate `(char-upcase c)`.
pub fn eval_char_upcase(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [c_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let c = expect_char(&eval(c_expr, env)?)?;
    Ok(Value::Char(c.to_ascii_uppercase()))
}

/// Evaluate `(char-downcase c)`.
pub fn eval_char_downcase(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [c_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let c = expect_char(&eval(c_expr, env)?)?;
    Ok(Value::Char(c.to_ascii_lowercase()))
}

/// Evaluate `(string=? s1 s2)` — string equality.
pub fn eval_string_eq(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a_val = eval(a_expr, env)?;
    let b_val = eval(b_expr, env)?;
    let a = expect_string(&a_val)?;
    let b = expect_string(&b_val)?;
    Ok(Value::Boolean(a == b))
}

/// Evaluate `(string<? s1 s2)` — string lexicographic ordering.
pub fn eval_string_lt(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a_val = eval(a_expr, env)?;
    let b_val = eval(b_expr, env)?;
    let a = expect_string(&a_val)?;
    let b = expect_string(&b_val)?;
    Ok(Value::Boolean(a < b))
}

/// Evaluate `(string-upcase s)` — convert string to uppercase.
pub fn eval_string_upcase(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    Ok(Value::String(s.to_uppercase()))
}

/// Evaluate `(string-downcase s)` — convert string to lowercase.
pub fn eval_string_downcase(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [arg] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "1".into(),
            got: args.len(),
        });
    };
    let val = eval(arg, env)?;
    let s = expect_string(&val)?;
    Ok(Value::String(s.to_lowercase()))
}

/// Evaluate `(string-ci=? s1 s2)` — case-insensitive string equality.
pub fn eval_string_ci_eq(args: &[Value], env: &Rc<Env>) -> Result<Value, EvalError> {
    let [a_expr, b_expr] = args else {
        return Err(EvalError::WrongArgCount {
            expected: "2".into(),
            got: args.len(),
        });
    };
    let a_val = eval(a_expr, env)?;
    let b_val = eval(b_expr, env)?;
    let a = expect_string(&a_val)?;
    let b = expect_string(&b_val)?;
    Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase()))
}
