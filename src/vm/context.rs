use crate::ast;
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Box<Context>>,
    builtins: HashMap<String, ScopedValue>,
    scope: Scope,
    symbols: HashMap<String, ScopedValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScopedValue {
    Global(u16),
    Local(u8),
    Builtin(u8),
}

#[derive(Clone, Debug, PartialEq)]
enum Scope {
    Global(u16),
    Local(u8),
}

impl Scope {
    fn inc(&mut self) {
        match self {
            Scope::Global(n) => *n += 1,
            Scope::Local(n) => *n += 1,
        }
    }

    fn is_global(&self) -> bool {
        match self {
            Scope::Global(_) => true,
            Scope::Local(_) => false,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            parent: None,
            scope: Scope::Global(0),
            builtins: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
}

impl Context {
    pub fn define(&mut self, ident: ast::Identifier) -> ScopedValue {
        let value = match self.scope {
            Scope::Local(idx) => ScopedValue::Local(idx),
            Scope::Global(idx) => ScopedValue::Global(idx),
        };
        self.symbols.insert(ident.value, value.clone());
        self.scope.inc();

        value
    }

    pub fn define_builtin(&mut self, idx: u8, name: String) -> ScopedValue {
        if !self.scope.is_global() {
            panic!("Can only define builtins in global scope");
        }

        let value = ScopedValue::Builtin(idx);
        self.builtins.insert(name, value.clone());
        value
    }

    pub fn resolve(&self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.resolve_builtin(ident)
            .or_else(|| self.resolve_symbol(ident))
    }

    fn resolve_symbol(&self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.symbols
            .get(&ident.value)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve(ident)))
    }

    pub fn resolve_builtin(&self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.builtins
            .get(&ident.value)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve_builtin(ident)))
    }

    pub fn num_definitions(&self) -> u8 {
        self.symbols.len().try_into().unwrap()
    }

    pub fn local(self) -> Self {
        Self {
            parent: Some(Box::new(self)),
            builtins: HashMap::new(),
            scope: Scope::Local(0),
            symbols: HashMap::new(),
        }
    }

    pub fn unlocal(mut self) -> Self {
        if let Some(p) = self.parent.take() {
            *p
        } else {
            // TODO should this panic?
            self
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_local_global() {
        let mut ctx = Context::default();
        ctx.define(ident("a"));
        ctx.define(ident("b"));
        assert_eq!(ctx.resolve(&ident("a")), global(0));
        assert_eq!(ctx.resolve(&ident("b")), global(1));

        // local
        ctx = ctx.local();
        ctx.define(ident("a"));
        ctx.define(ident("c"));
        assert_eq!(ctx.resolve(&ident("a")), local(0));
        assert_eq!(ctx.resolve(&ident("b")), global(1));
        assert_eq!(ctx.resolve(&ident("c")), local(1));

        // unlocal
        ctx = ctx.unlocal();
        assert_eq!(ctx.resolve(&ident("a")), global(0));
        assert_eq!(ctx.resolve(&ident("b")), global(1));

        // define global
        ctx.define(ident("c"));
        assert_eq!(ctx.resolve(&ident("c")), global(2));
    }

    #[test]
    fn test_nested_local() {
        let mut g = Context::default();
        g.define(ident("a"));
        assert_eq!(g.resolve(&ident("a")), global(0));
        assert_eq!(g.num_definitions(), 1);

        // local
        let mut l = g.local();
        l.define(ident("b"));
        l.define(ident("c"));
        assert_eq!(l.resolve(&ident("a")), global(0));
        assert_eq!(l.resolve(&ident("b")), local(0));
        assert_eq!(l.resolve(&ident("c")), local(1));
        assert_eq!(l.num_definitions(), 2);

        // nested local
        let mut nl = l.local();
        nl.define(ident("e"));
        nl.define(ident("f"));
        assert_eq!(nl.resolve(&ident("a")), global(0));
        assert_eq!(nl.resolve(&ident("e")), local(0));
        assert_eq!(nl.resolve(&ident("f")), local(1));
        assert_eq!(nl.num_definitions(), 2);

        // unlocal
        let g = nl.unlocal().unlocal();
        assert_eq!(g.resolve(&ident("a")), global(0));
        assert_eq!(g.num_definitions(), 1);
    }

    #[test]
    fn test_builtins() {
        let mut g = Context::default();
        g.define_builtin(8, "foo".to_owned());
        g.define_builtin(4, "bar".to_owned());
        g.define_builtin(1, "baz".to_owned());

        assert_eq!(g.resolve(&ident("foo")), builtin(8));
        assert_eq!(g.resolve(&ident("bar")), builtin(4));
        assert_eq!(g.resolve(&ident("baz")), builtin(1));
    }

    #[test]
    fn test_builtins_precedence() {
        let mut g = Context::default();
        g.define_builtin(99, "foo".to_owned());
        g.define_builtin(2, "bar".to_owned());

        g.define(ident("foo"));
        assert_eq!(g.resolve(&ident("foo")), builtin(99));
        assert_eq!(g.resolve(&ident("bar")), builtin(2));

        let mut l = g.local();
        l.define(ident("foo"));
        l.define(ident("bar"));
        assert_eq!(l.resolve(&ident("foo")), builtin(99));
        assert_eq!(l.resolve(&ident("bar")), builtin(2));

        let mut g = l.unlocal();
        g.define(ident("foo"));
        g.define(ident("bar"));
        assert_eq!(g.resolve(&ident("foo")), builtin(99));
        assert_eq!(g.resolve(&ident("bar")), builtin(2));
    }

    #[test]
    #[should_panic]
    fn test_buitins_cannot_define_on_local_ctx() {
        Context::default().local().define_builtin(4, "a".to_owned());
    }

    fn global(x: u16) -> Option<ScopedValue> {
        Some(ScopedValue::Global(x))
    }

    fn local(x: u8) -> Option<ScopedValue> {
        Some(ScopedValue::Local(x))
    }

    fn builtin(x: u8) -> Option<ScopedValue> {
        Some(ScopedValue::Builtin(x))
    }

    fn ident(x: &'static str) -> ast::Identifier {
        use crate::token::Token;
        ast::Identifier {
            token: Token {
                tpe: String::new(),
                literal: String::new(),
            },
            value: x.to_owned(),
        }
    }
}
