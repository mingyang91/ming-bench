use super::record::render_record;
use super::Value;

use std::collections::HashSet;
use std::rc::Rc;

pub(super) fn render_value(value: &Value) -> String {
    RenderContext::new(RenderMode::Write).render(value)
}

pub(super) fn render_display_value(value: &Value) -> String {
    RenderContext::new(RenderMode::Display).render(value)
}

struct RenderContext {
    mode: RenderMode,
    active_pairs: HashSet<usize>,
    active_vectors: HashSet<usize>,
}

impl RenderContext {
    fn new(mode: RenderMode) -> Self {
        Self {
            mode,
            active_pairs: HashSet::new(),
            active_vectors: HashSet::new(),
        }
    }

    fn render(&mut self, value: &Value) -> String {
        match value {
            Value::Number(number) => number.render(),
            Value::Boolean(true) => "#t".into(),
            Value::Boolean(false) => "#f".into(),
            Value::String(value) => match self.mode {
                RenderMode::Write => render_string(value),
                RenderMode::Display => value.clone(),
            },
            Value::MutableString(value) => {
                let value = value.borrow().iter().collect::<String>();
                match self.mode {
                    RenderMode::Write => render_string(&value),
                    RenderMode::Display => value,
                }
            }
            Value::Symbol(value) => value.clone(),
            Value::Char(value) => match self.mode {
                RenderMode::Write => render_char(*value),
                RenderMode::Display => value.to_string(),
            },
            Value::Syntax(_) => "#<syntax>".into(),
            Value::Pair(_) | Value::List(_) => self.render_list_like(value),
            Value::Vector(items) => self.render_vector(items),
            Value::Procedure(_)
            | Value::NativeProcedure(_)
            | Value::Builtin(_)
            | Value::Continuation(_)
            | Value::ContinuationHandle(_)
            | Value::ExpiredContinuation => "#<procedure>".into(),
            Value::Values(_) => "#<values>".into(),
            Value::Record(record) => render_record(record),
            Value::Uninitialized => "#<uninitialized>".into(),
            Value::Void => "#<void>".into(),
        }
    }

    fn render_list_like(&mut self, value: &Value) -> String {
        let mut current = value.clone();
        let mut rendered_items = Vec::new();
        let mut inserted_pairs = Vec::new();

        let tail = loop {
            match current {
                Value::List(items) => {
                    for item in items {
                        rendered_items.push(self.render(&item));
                    }
                    break None;
                }
                Value::Pair(pair) => {
                    let ptr = Rc::as_ptr(&pair) as usize;
                    if !self.active_pairs.insert(ptr) {
                        break Some("#<circular>".into());
                    }
                    inserted_pairs.push(ptr);

                    let pair = pair.borrow();
                    rendered_items.push(self.render(&pair.car));
                    current = pair.cdr.clone();
                }
                other => break Some(self.render(&other)),
            }
        };

        for ptr in inserted_pairs {
            self.active_pairs.remove(&ptr);
        }

        match tail {
            Some(tail) if rendered_items.is_empty() => format!("({tail})"),
            Some(tail) => format!("({} . {tail})", rendered_items.join(" ")),
            None if rendered_items.is_empty() => "()".into(),
            None => format!("({})", rendered_items.join(" ")),
        }
    }

    fn render_vector(&mut self, items: &Rc<std::cell::RefCell<Vec<Value>>>) -> String {
        let ptr = Rc::as_ptr(items) as usize;
        if !self.active_vectors.insert(ptr) {
            return "#<circular>".into();
        }

        let rendered_items = items
            .borrow()
            .iter()
            .map(|item| self.render(item))
            .collect::<Vec<_>>()
            .join(" ");

        self.active_vectors.remove(&ptr);
        format!("#({rendered_items})")
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');
    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            other => rendered.push(other),
        }
    }
    rendered.push('"');
    rendered
}

fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}
