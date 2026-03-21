use std::cell::RefCell;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Clone)]
pub struct SchemePair(Rc<RefCell<PairState>>);

struct PairState {
    car: Value,
    cdr: Value,
}

impl SchemePair {
    pub fn new(car: Value, cdr: Value) -> Self {
        Self(Rc::new(RefCell::new(PairState { car, cdr })))
    }

    pub fn is_same_object(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    pub fn id(&self) -> usize {
        Rc::as_ptr(&self.0) as usize
    }

    pub fn car(&self) -> Value {
        self.0.borrow().car.clone()
    }

    pub fn cdr(&self) -> Value {
        self.0.borrow().cdr.clone()
    }

    pub fn parts(&self) -> (Value, Value) {
        let state = self.0.borrow();
        (state.car.clone(), state.cdr.clone())
    }

    pub fn set_car(&self, value: Value) {
        self.0.borrow_mut().car = value;
    }

    pub fn set_cdr(&self, value: Value) {
        self.0.borrow_mut().cdr = value;
    }
}
