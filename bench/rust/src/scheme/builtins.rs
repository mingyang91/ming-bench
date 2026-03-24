use super::{
    as_integer, apply_func, is_proper_list, values_eq, values_equal,
    DisplayValue, EvalError, Pos, Value,
};

fn apply_numeric_builtin(name: &str, args: &[Value], call_pos: Pos) -> Result<Value, EvalError> {
    match name {
        "+" => {
            let mut sum: i64 = 0;
            for a in args {
                sum += as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(sum))
        }
        "-" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: - requires at least 1 argument")));
            }
            if args.len() == 1 {
                return Ok(Value::Integer(-as_integer(&args[0], call_pos)?));
            }
            let mut result = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                result -= as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(result))
        }
        "*" => {
            let mut product: i64 = 1;
            for a in args {
                product *= as_integer(a, call_pos)?;
            }
            Ok(Value::Integer(product))
        }
        "/" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: / requires at least 2 arguments")));
            }
            let mut result = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                let divisor = as_integer(a, call_pos)?;
                if divisor == 0 {
                    return Err(EvalError::DivisionByZero(format!("{call_pos}: division by zero")));
                }
                result /= divisor;
            }
            Ok(Value::Integer(result))
        }
        "<" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: < requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? < as_integer(&args[1], call_pos)?))
        }
        ">" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: > requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? > as_integer(&args[1], call_pos)?))
        }
        "=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: = requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? == as_integer(&args[1], call_pos)?))
        }
        "<=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: <= requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? <= as_integer(&args[1], call_pos)?))
        }
        ">=" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: >= requires 2 arguments")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? >= as_integer(&args[1], call_pos)?))
        }
        "abs" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: abs requires 1 argument")));
            }
            Ok(Value::Integer(as_integer(&args[0], call_pos)?.abs()))
        }
        "modulo" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: modulo requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(((a % b) + b) % b))
        }
        "remainder" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: remainder requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(a % b))
        }
        "quotient" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: quotient requires 2 arguments")));
            }
            let a = as_integer(&args[0], call_pos)?;
            let b = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(a / b))
        }
        "min" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: min requires at least 1 argument")));
            }
            let mut m = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                let n = as_integer(a, call_pos)?;
                if n < m { m = n; }
            }
            Ok(Value::Integer(m))
        }
        "max" => {
            if args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: max requires at least 1 argument")));
            }
            let mut m = as_integer(&args[0], call_pos)?;
            for a in &args[1..] {
                let n = as_integer(a, call_pos)?;
                if n > m { m = n; }
            }
            Ok(Value::Integer(m))
        }
        "expt" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: expt requires 2 arguments")));
            }
            let base = as_integer(&args[0], call_pos)?;
            let exp = as_integer(&args[1], call_pos)?;
            Ok(Value::Integer(base.pow(exp as u32)))
        }
        "zero?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: zero? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? == 0))
        }
        "positive?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: positive? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? > 0))
        }
        "negative?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: negative? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? < 0))
        }
        "odd?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: odd? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? % 2 != 0))
        }
        "even?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: even? requires 1 argument")));
            }
            Ok(Value::Boolean(as_integer(&args[0], call_pos)? % 2 == 0))
        }
        "number?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Integer(_))))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn apply_list_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: cons requires 2 arguments")));
            }
            match &args[1] {
                Value::List(elems) => {
                    let mut new_list = vec![args[0].clone()];
                    new_list.extend(elems.iter().cloned());
                    Ok(Value::List(new_list))
                }
                _ => {
                    Ok(Value::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
                }
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: car requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => Ok(elems[0].clone()),
                Value::Pair(a, _) => Ok(a.as_ref().clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: car: not a pair"))),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: cdr requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) if !elems.is_empty() => {
                    Ok(Value::List(elems[1..].to_vec()))
                }
                Value::Pair(_, d) => Ok(d.as_ref().clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: cdr: not a pair"))),
            }
        }
        "null?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: null? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if e.is_empty())))
        }
        "list" => Ok(Value::List(args.to_vec())),
        "length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: length requires 1 argument")));
            }
            match &args[0] {
                Value::List(elems) => Ok(Value::Integer(elems.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: length: not a list"))),
            }
        }
        "append" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::List(elems) => result.extend(elems.iter().cloned()),
                    _ => return Err(EvalError::Type(format!("{call_pos}: append: not a list"))),
                }
            }
            Ok(Value::List(result))
        }
        "list-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: list-ref requires 2 arguments")));
            }
            let idx = as_integer(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(elems) => Ok(elems[idx].clone()),
                _ => Err(EvalError::Type(format!("{call_pos}: list-ref: not a list"))),
            }
        }
        "list-tail" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: list-tail requires 2 arguments")));
            }
            let idx = as_integer(&args[1], call_pos)? as usize;
            match &args[0] {
                Value::List(elems) => Ok(Value::List(elems[idx..].to_vec())),
                _ => Err(EvalError::Type(format!("{call_pos}: list-tail: not a list"))),
            }
        }
        "list?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: list? requires 1 argument")));
            }
            Ok(Value::Boolean(is_proper_list(&args[0])))
        }
        "assoc" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: assoc requires 2 arguments")));
            }
            let key = &args[0];
            match &args[1] {
                Value::List(alist) => {
                    for entry in alist {
                        match entry {
                            Value::List(pair) if !pair.is_empty() => {
                                if values_equal(key, &pair[0]) {
                                    return Ok(entry.clone());
                                }
                            }
                            _ => {}
                        }
                    }
                    Ok(Value::Boolean(false))
                }
                _ => Err(EvalError::Type(format!("{call_pos}: assoc: expected list"))),
            }
        }
        "apply" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: apply requires at least 2 arguments")));
            }
            let func = &args[0];
            let last = &args[args.len() - 1];
            let tail = match last {
                Value::List(elems) => elems.clone(),
                _ => return Err(EvalError::Type(format!("{call_pos}: apply: last argument must be a list"))),
            };
            let mut combined = args[1..args.len() - 1].to_vec();
            combined.extend(tail);
            apply_func(func, &combined, call_pos, output)
        }
        "eq?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: eq? requires 2 arguments")));
            }
            Ok(Value::Boolean(values_eq(&args[0], &args[1])))
        }
        "equal?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: equal? requires 2 arguments")));
            }
            Ok(Value::Boolean(values_equal(&args[0], &args[1])))
        }
        "map" => {
            if args.len() < 2 {
                return Err(EvalError::Arity(format!("{call_pos}: map requires at least 2 arguments")));
            }
            let func = &args[0];
            let lists: Vec<&Vec<Value>> = args[1..].iter().map(|a| match a {
                Value::List(elems) => Ok(elems),
                _ => Err(EvalError::Type(format!("{call_pos}: map: expected list"))),
            }).collect::<Result<_, _>>()?;
            let len = lists[0].len();
            let mut result = Vec::new();
            for i in 0..len {
                let map_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(apply_func(func, &map_args, call_pos, output)?);
            }
            Ok(Value::List(result))
        }
        "boolean?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: boolean? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Boolean(_))))
        }
        "pair?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: pair? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::List(e) if !e.is_empty()) || matches!(&args[0], Value::Pair(_, _))))
        }
        "symbol?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Symbol(_))))
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

fn apply_string_io_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "display" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: display requires 1 argument")));
            }
            let s = format!("{}", DisplayValue(&args[0]));
            output.push_str(&s);
            Ok(Value::Void)
        }
        "write" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: write requires 1 argument")));
            }
            let s = format!("{}", &args[0]);
            output.push_str(&s);
            Ok(Value::Void)
        }
        "newline" => {
            if !args.is_empty() {
                return Err(EvalError::Arity(format!("{call_pos}: newline takes 0 arguments")));
            }
            output.push('\n');
            Ok(Value::Void)
        }
        "string-append" => {
            let mut result = String::new();
            for a in args {
                match a {
                    Value::Str(s) => result.push_str(s),
                    _ => return Err(EvalError::Type(format!("{call_pos}: string-append: expected string"))),
                }
            }
            Ok(Value::Str(result))
        }
        "string-length" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-length requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
                _ => Err(EvalError::Type(format!("{call_pos}: string-length: expected string"))),
            }
        }
        "substring" => {
            if args.len() != 3 {
                return Err(EvalError::Arity(format!("{call_pos}: substring requires 3 arguments")));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: substring: expected string"))),
            };
            let start = as_integer(&args[1], call_pos)? as usize;
            let end = as_integer(&args[2], call_pos)? as usize;
            Ok(Value::Str(s[start..end].to_string()))
        }
        "string->number" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->number requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => match s.parse::<i64>() {
                    Ok(n) => Ok(Value::Integer(n)),
                    Err(_) => Ok(Value::Boolean(false)),
                },
                _ => Err(EvalError::Type(format!("{call_pos}: string->number: expected string"))),
            }
        }
        "number->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: number->string requires 1 argument")));
            }
            let n = as_integer(&args[0], call_pos)?;
            Ok(Value::Str(n.to_string()))
        }
        "symbol->string" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: symbol->string requires 1 argument")));
            }
            match &args[0] {
                Value::Symbol(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: symbol->string: expected symbol"))),
            }
        }
        "string->symbol" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string->symbol requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Symbol(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string->symbol: expected string"))),
            }
        }
        "string-ref" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string-ref requires 2 arguments")));
            }
            let s = match &args[0] {
                Value::Str(s) => s,
                _ => return Err(EvalError::Type(format!("{call_pos}: string-ref: expected string"))),
            };
            let idx = as_integer(&args[1], call_pos)? as usize;
            Ok(Value::Char(s.chars().nth(idx).ok_or_else(|| {
                EvalError::Type(format!("{call_pos}: string-ref: index out of range"))
            })?))
        }
        "string-copy" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-copy requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.clone())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-copy: expected string"))),
            }
        }
        "string?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Str(_))))
        }
        "char?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char? requires 1 argument")));
            }
            Ok(Value::Boolean(matches!(&args[0], Value::Char(_))))
        }
        "char=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: char=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("{call_pos}: char=?: expected characters"))),
            }
        }
        "char<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: char<? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("{call_pos}: char<?: expected characters"))),
            }
        }
        "char-alphabetic?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-alphabetic? requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_alphabetic())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-alphabetic?: expected character"))),
            }
        }
        "char-numeric?" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-numeric? requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Boolean(c.is_ascii_digit())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-numeric?: expected character"))),
            }
        }
        "char-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-upcase requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-upcase: expected character"))),
            }
        }
        "char-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: char-downcase requires 1 argument")));
            }
            match &args[0] {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: char-downcase: expected character"))),
            }
        }
        "string=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a == b)),
                _ => Err(EvalError::Type(format!("{call_pos}: string=?: expected strings"))),
            }
        }
        "string<?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string<? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a < b)),
                _ => Err(EvalError::Type(format!("{call_pos}: string<?: expected strings"))),
            }
        }
        "string-ci=?" => {
            if args.len() != 2 {
                return Err(EvalError::Arity(format!("{call_pos}: string-ci=? requires 2 arguments")));
            }
            match (&args[0], &args[1]) {
                (Value::Str(a), Value::Str(b)) => Ok(Value::Boolean(a.to_lowercase() == b.to_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-ci=?: expected strings"))),
            }
        }
        "string-upcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-upcase requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_uppercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-upcase: expected string"))),
            }
        }
        "string-downcase" => {
            if args.len() != 1 {
                return Err(EvalError::Arity(format!("{call_pos}: string-downcase requires 1 argument")));
            }
            match &args[0] {
                Value::Str(s) => Ok(Value::Str(s.to_lowercase())),
                _ => Err(EvalError::Type(format!("{call_pos}: string-downcase: expected string"))),
            }
        }
        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}

pub(super) fn apply_builtin(name: &str, args: &[Value], call_pos: Pos, output: &mut String) -> Result<Value, EvalError> {
    match name {
        "+" | "-" | "*" | "/" | "<" | ">" | "=" | "<=" | ">="
        | "abs" | "modulo" | "remainder" | "quotient" | "min" | "max" | "expt"
        | "zero?" | "positive?" | "negative?" | "odd?" | "even?"
        | "number?" => apply_numeric_builtin(name, args, call_pos),

        "cons" | "car" | "cdr" | "null?" | "list" | "length" | "append"
        | "list-ref" | "list-tail" | "list?" | "assoc"
        | "apply" | "eq?" | "equal?" | "map"
        | "boolean?" | "pair?" | "symbol?" => apply_list_builtin(name, args, call_pos, output),

        "display" | "write" | "newline"
        | "string-append" | "string-length" | "substring"
        | "string->number" | "number->string" | "symbol->string" | "string->symbol"
        | "string-ref" | "string-copy" | "string?" | "char?"
        | "char=?" | "char<?" | "char-alphabetic?" | "char-numeric?"
        | "char-upcase" | "char-downcase"
        | "string=?" | "string<?" | "string-ci=?"
        | "string-upcase" | "string-downcase" => apply_string_io_builtin(name, args, call_pos, output),

        _ => Err(EvalError::UnboundVariable(format!("{call_pos}: {name}"))),
    }
}
