use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::object::Object;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    outer: Option<Rc<RefCell<Environment>>>,
    pub values: HashMap<String, Object>,
}

impl Environment {
    pub fn get(&self, key: &str) -> Option<Object> {
        self.values
            .get(key)
            .cloned()
            .or_else(|| self.outer.as_ref().and_then(|o| o.borrow().get(key)))
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.values.insert(key.into(), value);
    }

    pub fn set_all(&mut self, values: HashMap<String, Object>) {
        for (k, v) in values {
            self.set(&k, v);
        }
    }

    pub fn new_sub(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            outer: Some(outer),
            values: Default::default(),
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment {
            outer: None,
            values: Default::default(),
        }
    }
}
