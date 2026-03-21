use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct SchemeString(Rc<RefCell<StringState>>);

struct StringState {
    characters: Vec<char>,
    mutability: StringMutability,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StringMutability {
    Immutable,
    Mutable,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringMutationError {
    Immutable,
    IndexOutOfBounds { length: usize },
}

impl SchemeString {
    pub fn immutable(value: impl Into<String>) -> Self {
        Self::new(value.into(), StringMutability::Immutable)
    }

    pub fn mutable(value: impl Into<String>) -> Self {
        Self::new(value.into(), StringMutability::Mutable)
    }

    fn new(value: String, mutability: StringMutability) -> Self {
        Self(Rc::new(RefCell::new(StringState {
            characters: value.chars().collect(),
            mutability,
        })))
    }

    pub fn as_string(&self) -> String {
        self.0.borrow().characters.iter().collect()
    }

    pub fn len(&self) -> usize {
        self.0.borrow().characters.len()
    }

    pub fn char_at(&self, index: usize) -> Option<char> {
        self.0.borrow().characters.get(index).copied()
    }

    pub fn substring(&self, start: usize, end: usize) -> Self {
        let characters: String = self.0.borrow().characters[start..end].iter().collect();
        Self::immutable(characters)
    }

    pub fn mutable_copy(&self) -> Self {
        Self::mutable(self.as_string())
    }

    pub fn set_char(&self, index: usize, value: char) -> Result<(), StringMutationError> {
        let mut state = self.0.borrow_mut();

        if state.mutability == StringMutability::Immutable {
            return Err(StringMutationError::Immutable);
        }

        let length = state.characters.len();
        let Some(character) = state.characters.get_mut(index) else {
            return Err(StringMutationError::IndexOutOfBounds { length });
        };

        *character = value;
        Ok(())
    }
}
