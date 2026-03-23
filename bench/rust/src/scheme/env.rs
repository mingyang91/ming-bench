use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::scheme::value::Value;

#[derive(Debug, Clone)]
pub(crate) struct ReplayLevel {
    pub(crate) exprs: Vec<Value>,
    pub(crate) env: Env,
}

#[derive(Debug, Clone)]
struct BodyFrame {
    exprs: Vec<Value>,
    current_index: usize,
    env: Env,
}

#[derive(Debug)]
struct ContStore {
    next_id: u64,
    pending_return: Option<Value>,
    body_stack: Vec<BodyFrame>,
    registry: HashMap<u64, Vec<ReplayLevel>>,
}

impl ContStore {
    fn new() -> Self {
        ContStore {
            next_id: 1,
            pending_return: None,
            body_stack: Vec::new(),
            registry: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>,
    output: Rc<RefCell<String>>,
    cont_store: Rc<RefCell<ContStore>>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

#[derive(Debug, Clone)]
struct EnvInner {
    bindings: HashMap<String, Value>,
    parent: Option<Env>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: None,
            })),
            output: Rc::new(RefCell::new(String::new())),
            cont_store: Rc::new(RefCell::new(ContStore::new())),
        }
    }

    pub fn with_parent(parent: &Env) -> Self {
        Env {
            inner: Rc::new(RefCell::new(EnvInner {
                bindings: HashMap::new(),
                parent: Some(parent.clone()),
            })),
            output: parent.output.clone(),
            cont_store: parent.cont_store.clone(),
        }
    }

    pub fn write_output(&self, s: &str) {
        self.output.borrow_mut().push_str(s);
    }

    pub fn take_output(&self) -> String {
        self.output.borrow_mut().split_off(0)
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let inner = self.inner.borrow();
        if let Some(val) = inner.bindings.get(name) {
            Some(val.clone())
        } else {
            inner.parent.as_ref().and_then(|p| p.get(name))
        }
    }

    pub fn define(&self, name: String, value: Value) {
        self.inner.borrow_mut().bindings.insert(name, value);
    }

    /// Mutate an existing binding. Walks the environment chain.
    /// Returns `true` if the binding was found and updated, `false` if unbound.
    pub fn set(&self, name: &str, value: Value) -> bool {
        let mut inner = self.inner.borrow_mut();
        if inner.bindings.contains_key(name) {
            inner.bindings.insert(name.to_string(), value);
            return true;
        }
        match &inner.parent {
            Some(parent) => parent.set(name, value),
            None => false,
        }
    }

    /// Check if two Envs point to the same inner bindings.
    pub(crate) fn same(&self, other: &Env) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }

    /// Push a body frame onto the continuation body stack.
    pub(crate) fn push_body_frame(&self, exprs: Vec<Value>) {
        self.cont_store.borrow_mut().body_stack.push(BodyFrame {
            exprs,
            current_index: 0,
            env: self.clone(),
        });
    }

    /// Pop the top body frame from the continuation body stack.
    pub(crate) fn pop_body_frame(&self) {
        self.cont_store.borrow_mut().body_stack.pop();
    }

    /// Update the current index in the top body frame.
    pub(crate) fn set_body_index(&self, index: usize) {
        if let Some(frame) = self.cont_store.borrow_mut().body_stack.last_mut() {
            frame.current_index = index;
        }
    }

    /// Take the pending return value (used during replay to short-circuit call/cc).
    pub(crate) fn take_pending_return(&self) -> Option<Value> {
        self.cont_store.borrow_mut().pending_return.take()
    }

    /// Set the pending return value for replay.
    pub(crate) fn set_pending_return(&self, value: Value) {
        self.cont_store.borrow_mut().pending_return = Some(value);
    }

    /// Capture the current body stack as a continuation.
    /// Returns the new continuation ID.
    pub(crate) fn capture_continuation(&self) -> u64 {
        let mut store = self.cont_store.borrow_mut();
        let id = store.next_id;
        store.next_id += 1;
        let levels: Vec<ReplayLevel> = store
            .body_stack
            .iter()
            .map(|frame| ReplayLevel {
                exprs: frame.exprs[frame.current_index..].to_vec(),
                env: frame.env.clone(),
            })
            .collect();
        store.registry.insert(id, levels);
        id
    }

    /// Find replay data for a continuation that matches the given env.
    pub(crate) fn get_replay_for_env(&self, cont_id: u64, target_env: &Env) -> Option<ReplayLevel> {
        let store = self.cont_store.borrow();
        store.registry.get(&cont_id).and_then(|levels| {
            levels
                .iter()
                .find(|l| l.env.same(target_env))
                .cloned()
        })
    }
}
