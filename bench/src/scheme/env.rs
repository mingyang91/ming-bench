use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::ast::BindingKey;
use crate::scheme::runtime::Value;

pub type CellRef = Rc<RefCell<Value>>;
pub type EnvRef = Rc<Environment>;

#[derive(Debug)]
pub struct Environment {
    parent: Option<EnvRef>,
    bindings: RefCell<HashMap<BindingKey, CellRef>>,
}

pub fn root_env() -> EnvRef {
    Rc::new(Environment {
        parent: None,
        bindings: RefCell::new(HashMap::new()),
    })
}

pub fn child_env(parent: EnvRef) -> EnvRef {
    Rc::new(Environment {
        parent: Some(parent),
        bindings: RefCell::new(HashMap::new()),
    })
}

pub fn define_placeholder(env: &EnvRef, key: BindingKey) -> CellRef {
    local_cell(env, &key).unwrap_or_else(|| insert_value(env, key, Value::Void))
}

pub fn define_value(env: &EnvRef, key: BindingKey, value: Value) -> CellRef {
    let cell = Rc::new(RefCell::new(value));
    env.bindings.borrow_mut().insert(key, cell.clone());
    cell
}

pub fn lookup_cell(env: &EnvRef, key: &BindingKey) -> Option<CellRef> {
    let mut current = Some(env.clone());
    while let Some(frame) = current {
        if let Some(cell) = frame.bindings.borrow().get(key) {
            return Some(cell.clone());
        }
        current = frame.parent.clone();
    }
    None
}

pub fn snapshot_plain_bindings(env: &EnvRef) -> HashMap<String, CellRef> {
    let mut snapshot = HashMap::new();
    let mut current = Some(env.clone());
    while let Some(frame) = current {
        collect_plain_bindings(&frame, &mut snapshot);
        current = frame.parent.clone();
    }
    snapshot
}

fn local_cell(env: &EnvRef, key: &BindingKey) -> Option<CellRef> {
    env.bindings.borrow().get(key).cloned()
}

fn insert_value(env: &EnvRef, key: BindingKey, value: Value) -> CellRef {
    let cell = Rc::new(RefCell::new(value));
    env.bindings.borrow_mut().insert(key, cell.clone());
    cell
}

fn collect_plain_bindings(env: &EnvRef, snapshot: &mut HashMap<String, CellRef>) {
    let bindings: Vec<(String, CellRef)> = env
        .bindings
        .borrow()
        .iter()
        .filter_map(|(key, cell)| match key {
            BindingKey::Plain(name) => Some((name.clone(), cell.clone())),
            BindingKey::Gensym(_) => None,
        })
        .collect();
    for (name, cell) in bindings {
        snapshot.entry(name).or_insert(cell);
    }
}
