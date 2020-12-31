use crate::ast::Identifier;

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin<'a> {
    pub name: &'a str,
    arguments: usize,
}

const STRLEN: Builtin = Builtin {
    name: "strlen",
    arguments: 1,
};

pub fn get_builtin(ident: &Identifier) -> Option<Builtin<'static>> {
    match ident.value.as_ref() {
        "strlen" => Some(STRLEN),
        _ => None,
    }
}
