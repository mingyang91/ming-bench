use super::Value;

pub(super) fn render_string(value: &str) -> String {
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

pub(super) fn render_list(items: &[Value]) -> String {
    if items.is_empty() {
        return "()".into();
    }

    let rendered_items = items
        .iter()
        .map(Value::render)
        .collect::<Vec<_>>()
        .join(" ");
    format!("({rendered_items})")
}

pub(super) fn render_display_list(items: &[Value]) -> String {
    if items.is_empty() {
        return "()".into();
    }

    let rendered_items = items
        .iter()
        .map(Value::render_display)
        .collect::<Vec<_>>()
        .join(" ");
    format!("({rendered_items})")
}

pub(super) fn render_improper_list(items: &[Value], tail: &Value) -> String {
    render_dotted_list(items, tail, RenderMode::Write)
}

pub(super) fn render_vector(items: &[Value]) -> String {
    let rendered_items = items
        .iter()
        .map(Value::render)
        .collect::<Vec<_>>()
        .join(" ");
    format!("#({rendered_items})")
}

pub(super) fn render_display_improper_list(items: &[Value], tail: &Value) -> String {
    render_dotted_list(items, tail, RenderMode::Display)
}

pub(super) fn render_char(value: char) -> String {
    match value {
        ' ' => "#\\space".into(),
        '\n' => "#\\newline".into(),
        other => format!("#\\{other}"),
    }
}

#[derive(Clone, Copy)]
enum RenderMode {
    Write,
    Display,
}

fn render_dotted_list(items: &[Value], tail: &Value, mode: RenderMode) -> String {
    let mut rendered = String::from("(");

    for (index, item) in items.iter().enumerate() {
        if index > 0 {
            rendered.push(' ');
        }
        rendered.push_str(&render_value(item, mode));
    }

    if !items.is_empty() {
        rendered.push_str(" . ");
    }
    rendered.push_str(&render_value(tail, mode));
    rendered.push(')');
    rendered
}

fn render_value(value: &Value, mode: RenderMode) -> String {
    match mode {
        RenderMode::Write => value.render(),
        RenderMode::Display => value.render_display(),
    }
}
