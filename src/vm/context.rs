use crate::ast;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Box<Context>>,
    scope: Scope,
    symbols: HashMap<String, ScopedValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScopedValue {
    Global(u16),
    Local(u8),
}

#[derive(Clone, Debug)]
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
}

impl Default for Context {
    fn default() -> Self {
        Self {
            parent: None,
            scope: Scope::Global(0),
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

    pub fn resolve(&self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.symbols
            .get(&ident.value)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve(ident)))
    }

    pub fn local(self) -> Self {
        Self {
            parent: Some(Box::new(self)),
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

        // local
        let mut l = g.local();
        l.define(ident("b"));
        l.define(ident("c"));
        assert_eq!(l.resolve(&ident("a")), global(0));
        assert_eq!(l.resolve(&ident("b")), local(0));
        assert_eq!(l.resolve(&ident("c")), local(1));

        // nested local
        let mut nl = l.local();
        nl.define(ident("e"));
        nl.define(ident("f"));
        assert_eq!(nl.resolve(&ident("a")), global(0));
        assert_eq!(nl.resolve(&ident("e")), local(0));
        assert_eq!(nl.resolve(&ident("f")), local(1));

        // unlocal
        let g = nl.unlocal().unlocal();
        assert_eq!(g.resolve(&ident("a")), global(0));
    }

    fn global(x: u16) -> Option<ScopedValue> {
        Some(ScopedValue::Global(x))
    }

    fn local(x: u8) -> Option<ScopedValue> {
        Some(ScopedValue::Local(x))
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
