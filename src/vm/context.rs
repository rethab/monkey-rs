use crate::ast;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Context {
    parent: Option<Box<Context>>,
    builtins: HashMap<String, ScopedValue>,
    scope: Scope,
    symbols: HashMap<String, ScopedValue>,
    free_symbols: FreeVariables,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScopedValue {
    Global(u16),
    Local(u8),
    Free(u8),
    Builtin(u8),
    CurrentFunction,
}

impl ScopedValue {
    fn is_global(&self) -> bool {
        matches!(self, ScopedValue::Global(_))
    }

    fn is_builtin(&self) -> bool {
        matches!(self, ScopedValue::Builtin(_))
    }
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
        matches!(self, Scope::Global(_))
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            parent: None,
            scope: Scope::Global(0),
            builtins: HashMap::new(),
            symbols: HashMap::new(),
            free_symbols: FreeVariables::default(),
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

    pub fn resolve(&mut self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.resolve_builtin(ident)
            .or_else(|| self.resolve_symbol(ident))
    }

    fn resolve_symbol(&mut self, ident: &ast::Identifier) -> Option<ScopedValue> {
        if let Some(symbol) = self.symbols.get(&ident.value).cloned() {
            Some(symbol)
        } else {
            let maybe_resolved = self.parent.as_mut().and_then(|p| p.resolve_symbol(ident));
            if let Some(v) = maybe_resolved {
                Some(if v.is_builtin() || v.is_global() {
                    v
                } else {
                    self.define_free(ident, v)
                })
            } else {
                None
            }
        }
    }

    fn define_free(&mut self, ident: &ast::Identifier, original: ScopedValue) -> ScopedValue {
        let idx = self.free_symbols.resolve(ident, original);
        ScopedValue::Free(idx)
    }

    pub fn resolve_builtin(&self, ident: &ast::Identifier) -> Option<ScopedValue> {
        self.builtins
            .get(&ident.value)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.resolve_builtin(ident)))
    }

    pub fn num_definitions(&self) -> u8 {
        let mut sum = 0;
        for symbol in self.symbols.values() {
            if matches!(symbol, ScopedValue::Local(_) | ScopedValue::Global(_)) {
                sum += 1;
            }
        }
        sum
    }

    pub fn free_symbols(&self) -> Vec<ScopedValue> {
        self.free_symbols.originals()
    }

    pub fn local(self, current_function: Option<String>) -> Self {
        let mut symbols = HashMap::new();
        if let Some(fname) = current_function {
            symbols.insert(fname, ScopedValue::CurrentFunction);
        }

        Self {
            parent: Some(Box::new(self)),
            builtins: HashMap::new(),
            scope: Scope::Local(0),
            symbols,
            free_symbols: FreeVariables::default(),
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

#[derive(Clone, Debug)]
struct FreeVariables {
    resolved: Vec<(String, ScopedValue, u8)>,
}

impl FreeVariables {
    fn resolve(&mut self, ident: &ast::Identifier, original: ScopedValue) -> u8 {
        if let Some(idx) = self.get(ident) {
            idx
        } else {
            let idx = self.resolved.len() as u8;
            self.insert(ident.clone(), original, idx);
            idx
        }
    }

    fn get(&self, ident: &ast::Identifier) -> Option<u8> {
        self.resolved
            .iter()
            .find(|(name, _, _)| name == &ident.value)
            .map(|(_, _, idx)| *idx)
    }

    fn insert(&mut self, ident: ast::Identifier, original: ScopedValue, idx: u8) {
        self.resolved.push((ident.value, original, idx))
    }

    fn originals(&self) -> Vec<ScopedValue> {
        self.resolved.iter().map(|(_, v, _)| v.clone()).collect()
    }
}

impl Default for FreeVariables {
    fn default() -> Self {
        FreeVariables { resolved: vec![] }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_undefined() {
        let mut ctx = Context::default();
        assert_eq!(ctx.resolve(&ident("a")), None);
        ctx.define(ident("a"));
        assert_eq!(ctx.resolve(&ident("b")), None);
    }

    #[test]
    fn test_local_global() {
        let mut g = Context::default();
        g.define(ident("a"));
        g.define(ident("b"));
        assert_eq!(g.resolve(&ident("a")), global(0));
        assert_eq!(g.resolve(&ident("b")), global(1));

        // local
        let mut l = g.local(None);
        l.define(ident("a"));
        l.define(ident("c"));
        assert_eq!(l.resolve(&ident("a")), local(0));
        assert_eq!(l.resolve(&ident("b")), global(1));
        assert_eq!(l.resolve(&ident("c")), local(1));

        // unlocal
        g = l.unlocal();
        assert_eq!(g.resolve(&ident("a")), global(0));
        assert_eq!(g.resolve(&ident("b")), global(1));

        // define global
        g.define(ident("c"));
        assert_eq!(g.resolve(&ident("c")), global(2));
    }

    #[test]
    fn test_nested_local() {
        let mut g = Context::default();
        g.define(ident("a"));
        assert_eq!(g.resolve(&ident("a")), global(0));
        assert_eq!(g.num_definitions(), 1);

        // local
        let mut l = g.local(None);
        l.define(ident("b"));
        l.define(ident("c"));
        assert_eq!(l.resolve(&ident("a")), global(0));
        assert_eq!(l.resolve(&ident("b")), local(0));
        assert_eq!(l.resolve(&ident("c")), local(1));
        assert_eq!(l.num_definitions(), 2);

        // nested local
        let mut nl = l.local(None);
        nl.define(ident("e"));
        nl.define(ident("f"));
        assert_eq!(nl.resolve(&ident("a")), global(0));
        assert_eq!(nl.resolve(&ident("e")), local(0));
        assert_eq!(nl.resolve(&ident("f")), local(1));
        assert_eq!(nl.num_definitions(), 2);

        // unlocal
        let mut g = nl.unlocal().unlocal();
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
    fn test_free_variables() {
        let mut g = Context::default();
        g.define(ident("g"));
        assert_eq!(g.resolve(&ident("g")), global(0));
        assert_eq!(g.num_definitions(), 1);

        // local
        let mut l = g.local(None);
        l.define(ident("l"));

        // nested local
        let mut nl = l.local(None);
        nl.define(ident("nl"));
        assert_eq!(nl.resolve(&ident("g")), global(0));
        assert_eq!(nl.resolve(&ident("l")), free(0));
        assert_eq!(nl.resolve(&ident("nl")), local(0));

        // double nested local
        let mut nnl = nl.local(None);
        nnl.define(ident("nnl1"));
        nnl.define(ident("nnl2"));
        assert_eq!(nnl.resolve(&ident("g")), global(0));
        assert_eq!(nnl.resolve(&ident("l")), free(0));
        assert_eq!(nnl.resolve(&ident("l")), free(0));
        assert_eq!(nnl.resolve(&ident("nnl1")), local(0));
        assert_eq!(nnl.resolve(&ident("nnl2")), local(1));

        // triple nested local
        let mut nnnl = nnl.local(None);
        nnnl.define(ident("nnnl"));
        assert_eq!(nnnl.resolve(&ident("g")), global(0));
        assert_eq!(nnnl.resolve(&ident("nnnl")), local(0));
        assert_eq!(nnnl.resolve(&ident("l")), free(0));
        assert_eq!(nnnl.resolve(&ident("nl")), free(1));
        assert_eq!(nnnl.resolve(&ident("nl")), free(1));
        assert_eq!(nnnl.resolve(&ident("nnl1")), free(2));

        let mut nnl = nnnl.unlocal();
        assert_eq!(nnl.resolve(&ident("nl")), free(1));
    }

    #[test]
    fn test_resolve_free_variables_in_parent() {
        let mut g = Context::default();
        g.define(ident("g"));

        let mut l = g.local(None);
        l.define(ident("l"));
        l.resolve(&ident("g"));
        assert_eq!(l.free_symbols().len(), 0);

        let mut nl = l.local(None);
        nl.define(ident("nl"));
        nl.resolve(&ident("g"));
        assert_eq!(nl.free_symbols().len(), 0);

        let mut nnl = nl.local(None);
        nnl.define(ident("nnl"));
        nnl.resolve(&ident("nnl"));
        nnl.resolve(&ident("nl"));
        nnl.resolve(&ident("l"));
        assert_eq!(nnl.free_symbols().len(), 2);

        let nl = nnl.unlocal();
        assert_eq!(nl.free_symbols().len(), 1);

        let l = nl.unlocal();
        assert_eq!(l.free_symbols().len(), 0);

        let g = l.unlocal();
        assert_eq!(g.free_symbols().len(), 0);
    }

    #[test]
    fn test_resolve_free_variables_sorted() {
        let g = Context::default();

        let mut l = g.local(None);
        l.define(ident("a"));
        l.define(ident("b"));
        l.define(ident("c"));
        l.define(ident("d"));
        l.define(ident("e"));

        let mut nl = l.local(None);
        nl.resolve(&ident("b"));
        nl.resolve(&ident("a"));
        nl.resolve(&ident("c"));
        nl.resolve(&ident("e"));
        nl.resolve(&ident("d"));
        assert_eq!(
            nl.free_symbols(),
            vec![
                local(1).unwrap(),
                local(0).unwrap(),
                local(2).unwrap(),
                local(4).unwrap(),
                local(3).unwrap(),
            ]
        );
    }

    #[test]
    fn test_resolve_current_function() {
        let g = Context::default();

        let mut l = g.local(Some("myFunc".into()));
        assert_eq!(
            l.resolve(&ident("myFunc")),
            Some(ScopedValue::CurrentFunction)
        );

        let mut nl = l.local(None);
        assert_eq!(nl.resolve(&ident("myFunc")), free(0));

        let mut l = nl.unlocal();
        assert_eq!(
            l.resolve(&ident("myFunc")),
            Some(ScopedValue::CurrentFunction)
        );
    }

    #[test]
    fn test_shadow_current_function() {
        let g = Context::default();

        let mut l = g.local(Some("myFunc".into()));
        l.define(ident("myFunc"));
        assert!(matches!(
            l.resolve(&ident("myFunc")),
            Some(ScopedValue::Local(_))
        ));
    }

    #[test]
    fn test_shadow_current_function_nested() {
        let g = Context::default();

        let l = g.local(Some("myFunc".into()));
        let mut nl = l.local(Some("fib".into()));

        assert_eq!(
            nl.resolve(&ident("fib")),
            Some(ScopedValue::CurrentFunction)
        );
        nl.define(ident("fib"));
        assert!(matches!(
            nl.resolve(&ident("fib")),
            Some(ScopedValue::Local(_))
        ));

        assert!(matches!(
            nl.resolve(&ident("myFunc")),
            Some(ScopedValue::Free(_))
        ));
        nl.define(ident("myFunc"));
        assert!(matches!(
            nl.resolve(&ident("myFunc")),
            Some(ScopedValue::Local(_))
        ));
    }

    #[test]
    fn test_builtins_precedence() {
        let mut g = Context::default();
        g.define_builtin(99, "foo".to_owned());
        g.define_builtin(2, "bar".to_owned());

        g.define(ident("foo"));
        assert_eq!(g.resolve(&ident("foo")), builtin(99));
        assert_eq!(g.resolve(&ident("bar")), builtin(2));

        let mut l = g.local(None);
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
        Context::default()
            .local(None)
            .define_builtin(4, "a".to_owned());
    }

    fn global(x: u16) -> Option<ScopedValue> {
        Some(ScopedValue::Global(x))
    }

    fn local(x: u8) -> Option<ScopedValue> {
        Some(ScopedValue::Local(x))
    }

    fn free(x: u8) -> Option<ScopedValue> {
        Some(ScopedValue::Free(x))
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
