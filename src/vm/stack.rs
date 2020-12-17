use std::fmt;

use crate::object;

#[derive(Debug)]
pub struct Stack {
    elems: Vec<object::Object>,
    sp: usize, // points to next free
}

impl Stack {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            elems: vec![object::NULL; capacity],
            sp: 0,
        }
    }

    pub fn pop(&mut self) -> object::Object {
        let object = self.elems[self.sp - 1].clone();
        self.sp -= 1;
        object
    }

    pub fn peek(&mut self) -> &object::Object {
        &self.elems[self.sp - 1]
    }

    pub fn push(&mut self, obj: object::Object) {
        self.elems[self.sp] = obj;
        self.sp += 1;
    }

    pub fn last_popped_elem(&self) -> &object::Object {
        &self.elems[self.sp]
    }
}

impl<'a> fmt::Display for Stack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for i in 0..self.sp {
            writeln!(f, "{:>0width$}: {}", i, self.elems[i].inspect(), width = 2)?;
        }
        Ok(())
    }
}
