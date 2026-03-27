use std::cell::RefCell;
use std::rc::Rc;

use super::EvalError;

#[derive(Clone)]
pub(super) struct SchemeString(Rc<SchemeStringData>);

struct SchemeStringData {
    chars: RefCell<Vec<char>>,
    mutable: bool,
}

impl SchemeString {
    pub(super) fn immutable(value: impl AsRef<str>) -> Self {
        Self::from_chars(value.as_ref().chars().collect(), false)
    }

    fn from_chars(chars: Vec<char>, mutable: bool) -> Self {
        Self(Rc::new(SchemeStringData {
            chars: RefCell::new(chars),
            mutable,
        }))
    }

    pub(super) fn to_plain_string(&self) -> String {
        self.0.chars.borrow().iter().collect()
    }

    pub(super) fn len(&self) -> usize {
        self.0.chars.borrow().len()
    }

    pub(super) fn char_at(&self, index: usize) -> Option<char> {
        self.0.chars.borrow().get(index).copied()
    }

    pub(super) fn substring(&self, start: usize, end: usize) -> String {
        self.0.chars.borrow()[start..end].iter().collect()
    }

    pub(super) fn mutable_copy(&self) -> Self {
        Self::from_chars(self.0.chars.borrow().clone(), true)
    }

    pub(super) fn set_char(&self, index: usize, value: char) -> Result<(), EvalError> {
        if !self.0.mutable {
            return Err(EvalError::ImmutableString);
        }

        let mut chars = self.0.chars.borrow_mut();
        let len = chars.len();
        let Some(slot) = chars.get_mut(index) else {
            return Err(EvalError::IndexOutOfBounds {
                index: index as i64,
                len,
            });
        };
        *slot = value;
        Ok(())
    }
}

pub(super) fn parse_char_literal(token: &str) -> Option<char> {
    match token {
        "space" => Some(' '),
        "newline" => Some('\n'),
        "tab" => Some('\t'),
        "return" => Some('\r'),
        _ => {
            let mut chars = token.chars();
            let ch = chars.next()?;
            if chars.next().is_none() {
                Some(ch)
            } else {
                None
            }
        }
    }
}

pub(super) fn escape_string(input: &str) -> String {
    let mut escaped = String::new();
    for ch in input.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            other => escaped.push(other),
        }
    }
    escaped
}

pub(super) fn render_char(ch: char) -> String {
    match ch {
        ' ' => "#\\space".to_string(),
        '\n' => "#\\newline".to_string(),
        other => format!("#\\{other}"),
    }
}
