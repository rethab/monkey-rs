use super::instructions::Label;
use crate::ast::Identifier;

use std::collections::HashMap;
use std::mem;

pub struct Context(Inner);

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Global(Label),
    Local(u8),
    Stack(i32),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scope {
    Global,
    Local,
}

struct Inner {
    parent: Option<Box<Inner>>,
    values: HashMap<String, Ref>,
    n_params: usize,
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
    pub fn define(&mut self, ident: Identifier) -> Ref {
        let r = if self.scope() == Scope::Global {
            Ref::Global(Label(ident.value.clone()))
        } else {
            Ref::Local(self.0.values.len() as u8)
        };

        self.0.values.insert(ident.value, r.clone());
        r
    }

    pub fn define_global(&mut self, ident: Identifier, label: Label) {
        self.0.values.insert(ident.value, Ref::Global(label));
    }

    pub fn define_stack(&mut self, ident: Identifier, offset: i32) {
        if self.scope() != Scope::Local {
            panic!("Can only define local things on stack");
        }
        self.0.values.insert(ident.value, Ref::Stack(offset));
    }

    pub fn local_definitions(&self) -> usize {
        self.0
            .values
            .iter()
            .filter(|(_, r)| matches!(r, Ref::Local(_)))
            .count()
    }

    pub fn n_params(&self) -> usize {
        self.0.n_params
    }

    pub fn scope(&self) -> Scope {
        if self.0.parent.is_none() {
            Scope::Global
        } else {
            Scope::Local
        }
    }

    pub fn resolve(&self, ident: &Identifier) -> Ref {
        self.0.resolve(ident)
    }

    pub fn enter_function(&mut self, n_params: usize) {
        let parent = mem::replace(
            &mut self.0,
            Inner {
                parent: None,
                values: HashMap::new(),
                n_params, // after arguments, where locals start
            },
        );
        self.0.parent = Some(Box::new(parent));
    }

    pub fn leave_function(&mut self) {
        self.0 = *self.0.parent.take().expect("No parent");
    }
}

impl Default for Context {
    fn default() -> Self {
        Self(Inner {
            parent: None,
            values: HashMap::new(),
            n_params: 0,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn define_returns_value() {
        let mut ctx = Context::default();
        assert_eq!(ctx.define(ident("a")), ref_global("a"));
        assert_eq!(ctx.define(ident("b")), ref_global("b"));

        ctx.enter_function(0);
        assert_eq!(ctx.define(ident("a")), ref_local(0));
        assert_eq!(ctx.define(ident("b")), ref_local(1));
    }

    #[test]
    fn local_indices_increase_per_level() {
        let mut ctx = Context::default();
        ctx.enter_function(0);

        ctx.define(ident("a"));
        ctx.define(ident("b"));
        ctx.define(ident("c"));
        assert_eq!(ctx.resolve(&ident("c")), ref_local(2));

        ctx.enter_function(0);
        ctx.define(ident("d"));
        ctx.define(ident("e"));
        assert_eq!(ctx.resolve(&ident("e")), ref_local(1));
    }

    #[test]
    fn resolve_in_self() {
        let mut ctx = Context::default();
        ctx.define(ident("a"));
        ctx.define(ident("b"));
        assert_eq!(ctx.resolve(&ident("a")), ref_global("a"));
        assert_eq!(ctx.resolve(&ident("b")), ref_global("b"));
    }

    #[test]
    fn define_global() {
        let mut ctx = Context::default();

        ctx.define_global(ident("a"), Label("l1".to_owned()));
        assert_eq!(ctx.resolve(&ident("a")), ref_global("l1"));

        ctx.define_global(ident("b"), Label("l2".to_owned()));
        assert_eq!(ctx.resolve(&ident("b")), ref_global("l2"));
    }

    #[test]
    fn resolve_in_parent() {
        let mut ctx = Context::default();
        ctx.define(ident("a"));
        ctx.enter_function(0);
        assert_eq!(ctx.resolve(&ident("a")), ref_global("a"));
    }

    #[test]
    fn resolve_stack_in_parent() {
        let mut ctx = Context::default();
        ctx.enter_function(0);
        ctx.define_stack(ident("b"), 33);
        ctx.enter_function(0);
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(33));
    }

    #[test]
    fn resolve_with_precedence() {
        let mut ctx = Context::default();
        ctx.define(ident("a"));
        ctx.enter_function(0);
        ctx.define(ident("a"));
        assert_eq!(ctx.resolve(&ident("a")), ref_local(0));
    }

    #[test]
    fn resolve_stack_with_precedence() {
        let mut ctx = Context::default();
        ctx.enter_function(0);
        ctx.define_stack(ident("b"), 33);
        ctx.enter_function(0);
        ctx.define_stack(ident("b"), 34);
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(34));
    }

    #[test]
    fn resolve_with_precedence_in_parent() {
        let mut ctx = Context::default();

        ctx.enter_function(0);
        ctx.define(ident("a"));
        ctx.define_stack(ident("b"), 33);

        ctx.enter_function(0);
        ctx.define(ident("a"));
        ctx.define_stack(ident("b"), 34);

        ctx.enter_function(0);
        assert_eq!(ctx.resolve(&ident("b")), ref_stack(34));
    }

    #[test]
    fn n_params() {
        let mut ctx = Context::default();

        ctx.enter_function(1);
        assert_eq!(ctx.n_params(), 1);

        ctx.enter_function(3);
        assert_eq!(ctx.n_params(), 3);

        ctx.enter_function(7);
        assert_eq!(ctx.n_params(), 7);

        ctx.leave_function();
        assert_eq!(ctx.n_params(), 3);

        ctx.leave_function();
        assert_eq!(ctx.n_params(), 1);
    }

    #[test]
    fn count_local_definitions() {
        let mut ctx = Context::default();
        ctx.define(ident("a"));

        ctx.enter_function(0);
        ctx.define(ident("b"));
        ctx.define_stack(ident("c"), 33);
        assert_eq!(ctx.local_definitions(), 1);

        ctx.enter_function(0);
        ctx.define(ident("d"));
        ctx.define(ident("e"));
        ctx.define_stack(ident("c"), 33);
        assert_eq!(ctx.local_definitions(), 2);

        ctx.leave_function();
        assert_eq!(ctx.local_definitions(), 1);
    }

    #[test]
    fn scope() {
        let mut ctx = Context::default();
        assert_eq!(ctx.scope(), Scope::Global);

        ctx.enter_function(0);
        assert_eq!(ctx.scope(), Scope::Local);

        ctx.enter_function(0);
        assert_eq!(ctx.scope(), Scope::Local);

        ctx.leave_function();
        assert_eq!(ctx.scope(), Scope::Local);

        ctx.leave_function();
        assert_eq!(ctx.scope(), Scope::Global);
    }

    #[test]
    #[should_panic]
    fn not_resolve_label_from_child() {
        let mut ctx = Context::default();
        ctx.enter_function(0);
        ctx.define(ident("a"));
        ctx.leave_function();
        ctx.resolve(&ident("a"));
    }

    #[test]
    #[should_panic]
    fn not_resolve_stack_from_child() {
        let mut ctx = Context::default();
        ctx.enter_function(0);
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

    fn ref_global(name: &str) -> Ref {
        Ref::Global(Label(name.to_owned()))
    }

    fn ref_local(idx: u8) -> Ref {
        Ref::Local(idx)
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
