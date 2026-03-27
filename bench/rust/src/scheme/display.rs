use super::Value;

pub(crate) fn display_value(v: &Value) -> String {
    match v {
        Value::Str(s, _) => s.borrow().clone(),
        Value::Char(c) => c.to_string(),
        Value::List(items) => {
            let mut s = String::from("(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(item));
            }
            s.push(')');
            s
        }
        Value::Pair(cell) => {
            let mut s = String::from("(");
            let (car, cdr) = {
                let b = cell.borrow();
                (b.0.clone(), b.1.clone())
            };
            s.push_str(&display_value(&car));
            let mut current = cdr;
            let mut depth = 0usize;
            loop {
                match current {
                    Value::List(ref items) if items.is_empty() => break,
                    Value::List(ref items) => {
                        for item in items {
                            s.push(' ');
                            s.push_str(&display_value(item));
                        }
                        break;
                    }
                    Value::Pair(ref next) => {
                        depth += 1;
                        if depth > 100_000 {
                            s.push_str(" ...");
                            break;
                        }
                        let (car, cdr2) = {
                            let b = next.borrow();
                            (b.0.clone(), b.1.clone())
                        };
                        s.push(' ');
                        s.push_str(&display_value(&car));
                        current = cdr2;
                    }
                    _ => {
                        s.push_str(" . ");
                        s.push_str(&display_value(&current));
                        break;
                    }
                }
            }
            s.push(')');
            s
        }
        Value::Builtin(name) => format!("#<procedure:{name}>"),
        Value::Record { type_tag, .. } => format!("#<record:{type_tag}>"),
        Value::Vector(v) => {
            let items = v.borrow();
            let mut s = String::from("#(");
            for (i, item) in items.iter().enumerate() {
                if i > 0 { s.push(' '); }
                s.push_str(&display_value(item));
            }
            s.push(')');
            s
        }
        other => other.to_string(),
    }
}
