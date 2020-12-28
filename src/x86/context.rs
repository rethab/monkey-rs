use super::instructions::Label;
use crate::ast::Identifier;

use std::collections::HashMap;
use std::mem;

pub struct Context(Inner);

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Label(Label),
    Stack(i32),
}

struct Inner {
    parent: Option<Box<Inner>>,
    values: HashMap<String, Ref>,
}

impl Inner {
    pub fn resolve(&self, ident: &Identifier) -> Ref {
        self.values.get(&ident.value).cloned().unwrap_or_else(|| {
            if let Some(p) = self.parent.as_ref() {
                (*p).resolve(ident)
            } else {
                panic!("Identifier {} not found", ident.value)
            }
        })
    }
}

impl Context {
    pub fn define(&mut self, ident: Identifier, lbl: Label) {
        self.0.values.insert(ident.value, Ref::Label(lbl));
    }

    pub fn define_stack(&mut self, ident: Identifier, offset: i32) {
        if self.0.parent.is_none() {
            panic!("Cannot define stack in root");
        }
        self.0.values.insert(ident.value, Ref::Stack(offset));
    }

    pub fn resolve(&self, ident: &Identifier) -> Ref {
        self.0.resolve(ident)
    }

    pub fn enter_function(&mut self) {
        let parent = mem::take(&mut self.0);
        self.0.parent = Some(Box::new(parent));
    }

    pub fn leave_function(&mut self) {
        self.0 = *self.0.parent.take().expect("No parent");
    }
}

impl Default for Context {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl Default for Inner {
    fn default() -> Self {
        Inner {
            parent: None,
            values: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_in_self() {
        let mut ctx = Context::default();
        ctx.define(ident("a"), label("l1"));
        ctx.define(ident("b"), label("l2"));
        assert_eq!(ctx.resolve(&ident("a")), ref_label("l1"));
        assert_eq!(ctx.resolve(&ident("b")), ref_label("l2"));
    }

    #[test]
    fn resolve_in_parent() {
        let mut ctx = Context::default();
        ctx.define(ident("a"), label("l1"));
        ctx.enter_function();
        assert_eq!(ctx.resolve(&ident("a")), ref_label("l1"));
    }

    #[test]
    fn resolve_stack_in_parent() {
        let mut ctx = Context::default();
        ctx.enter_function();
        ctx.define_stack(ident("b"), 33);
        ctx.enter_function();
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(33));
    }

    #[test]
    fn resolve_with_precedence() {
        let mut ctx = Context::default();
        ctx.define(ident("a"), label("l1"));
        ctx.enter_function();
        ctx.define(ident("a"), label("l2"));
        assert_eq!(ctx.resolve(&ident("a")), ref_label("l2"));
    }

    #[test]
    fn resolve_stack_with_precedence() {
        let mut ctx = Context::default();
        ctx.enter_function();
        ctx.define_stack(ident("b"), 33);
        ctx.enter_function();
        ctx.define_stack(ident("b"), 34);
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(34));
    }

    #[test]
    fn resolve_with_precedence_in_parent() {
        let mut ctx = Context::default();
        ctx.enter_function();
        ctx.define(ident("a"), label("l1"));
        ctx.define_stack(ident("b"), 33);
        ctx.enter_function();
        ctx.define(ident("a"), label("l2"));
        ctx.define_stack(ident("b"), 34);
        ctx.enter_function();
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(34));
    }

    #[test]
    #[should_panic]
    fn not_resolve_label_from_child() {
        let mut ctx = Context::default();
        ctx.enter_function();
        ctx.define(ident("a"), label("l1"));
        ctx.leave_function();
        ctx.resolve(&ident("a"));
    }

    #[test]
    #[should_panic]
    fn not_resolve_stack_from_child() {
        let mut ctx = Context::default();
        ctx.enter_function();
        ctx.define_stack(ident("a"), 33);
        ctx.leave_function();
        ctx.resolve(&ident("a"));
    }

    #[test]
    #[should_panic]
    fn not_define_stack_in_root() {
        let mut ctx = Context::default();
        ctx.define_stack(ident("a"), 33);
    }

    fn label(name: &str) -> Label {
        Label(name.to_owned())
    }

    fn ref_label(name: &str) -> Ref {
        Ref::Label(Label(name.to_owned()))
    }

    fn ref_stack(x: i32) -> Ref {
        Ref::Stack(x)
    }

    fn ident(name: &str) -> Identifier {
        use crate::token;
        Identifier {
            value: name.to_owned(),
            token: token::Token {
                tpe: String::new(),
                literal: String::new(),
            },
        }
    }
}
