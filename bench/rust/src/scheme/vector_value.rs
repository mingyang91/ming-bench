use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Clone)]
pub struct SchemeVector(Rc<RefCell<Vec<Value>>>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VectorMutationError {
    IndexOutOfBounds { length: usize },
}

impl SchemeVector {
    pub fn new(elements: Vec<Value>) -> Self {
        Self(Rc::new(RefCell::new(elements)))
    }

    pub fn make(length: usize, fill: Value) -> Self {
        Self::new(vec![fill; length])
    }

    pub fn is_same_object(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn len(&self) -> usize {
        self.0.borrow().len()
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.0.borrow().get(index).cloned()
    }

    pub fn items(&self) -> Vec<Value> {
        self.0.borrow().clone()
    }

    pub fn set(&self, index: usize, value: Value) -> Result<(), VectorMutationError> {
        let mut elements = self.0.borrow_mut();
        let length = elements.len();
        let Some(slot) = elements.get_mut(index) else {
            return Err(VectorMutationError::IndexOutOfBounds { length });
        };

        *slot = value;
        Ok(())
    }
}
