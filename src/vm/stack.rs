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

    pub fn peek_offset(&mut self, offset: usize) -> &object::Object {
        &self.elems[self.sp - 1 - offset]
    }

    pub fn peek_at(&mut self, idx: usize) -> &object::Object {
        &self.elems[idx]
    }

    pub fn push(&mut self, obj: object::Object) {
        self.elems[self.sp] = obj;
        self.sp += 1;
    }

    pub fn push_at(&mut self, idx: usize, obj: object::Object) {
        self.elems[idx] = obj;
    }

    pub fn sp(&self) -> usize {
        self.sp
    }

    pub fn inc_sp(&mut self, n: u8) {
        self.sp += n as usize;
    }

    pub fn set_sp(&mut self, sp: usize) {
        self.sp = sp;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut stack = Stack::with_capacity(10);
        stack.push(int(1));
        assert_eq!(int(1), stack.pop());

        stack.push(int(1));
        stack.push(int(2));
        stack.push(int(3));
        assert_eq!(int(3), stack.pop());
        assert_eq!(int(2), stack.pop());
        assert_eq!(int(1), stack.pop());
    }

    #[test]
    fn test_last_popped() {
        let mut stack = Stack::with_capacity(10);
        stack.push(int(1));
        assert_eq!(int(1), stack.pop());
        assert_eq!(int(1), *stack.last_popped_elem());

        stack.push(int(1));
        stack.push(int(2));
        assert_eq!(int(2), stack.pop());
        assert_eq!(int(2), *stack.last_popped_elem());

        assert_eq!(int(1), stack.pop());
        assert_eq!(int(1), *stack.last_popped_elem());
    }

    #[test]
    fn test_peek_offset() {
        let mut stack = Stack::with_capacity(10);
        stack.push(int(1));
        stack.push(int(2));
        stack.push(int(3));
        assert_eq!(int(3), *stack.peek_offset(0));
        assert_eq!(int(2), *stack.peek_offset(1));
        assert_eq!(int(1), *stack.peek_offset(2));

        stack.pop();

        assert_eq!(int(2), *stack.peek_offset(0));
        assert_eq!(int(1), *stack.peek_offset(1));
    }

    #[test]
    fn test_push_at_doesnt_modify_sp() {
        let mut stack = Stack::with_capacity(10);
        stack.push(int(1));

        // create two hole and fill them
        stack.inc_sp(2);
        stack.push_at(1, int(2));
        stack.push_at(2, int(3));

        stack.push(int(4));

        assert_eq!(*stack.peek_at(0), int(1));
        assert_eq!(*stack.peek_at(1), int(2));
        assert_eq!(*stack.peek_at(2), int(3));
        assert_eq!(*stack.peek_at(3), int(4));

        assert_eq!(stack.pop(), int(4));
        assert_eq!(stack.pop(), int(3));
        assert_eq!(stack.pop(), int(2));
        assert_eq!(stack.pop(), int(1));
    }

    fn int(x: i64) -> object::Object {
        object::Object::Integer(x)
    }
}
