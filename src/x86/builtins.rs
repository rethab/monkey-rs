use crate::ast;
use crate::lexer;
use crate::parser;

pub fn is_builtin(ident: &ast::Identifier) -> bool {
    matches!(
        ident.value.as_ref(),
        "strlen" | "strcat" | "strcpy" | "malloc"
    )
}

pub fn builtin_strconcat() -> ast::Function {
    let function = "
        fn(a, b) {
            let alen = strlen(a);
            let blen = strlen(a);
            let addr = malloc(alen + blen + 1);
            strcat(strcpy(addr, a), b);
            addr
        }
    ";
    let ast::Program(mut stmts) = *parser::Parser::new(lexer::Lexer::new(function))
        .parse_program()
        .expect("Failed to parse");
    if stmts.len() > 1 {
        panic!("Should only generate one function literal");
    }
    match stmts.remove(0) {
        ast::Statement::Expression { value, .. } => ast::Function::from_expression(value),
        other => panic!("not an expression: {:?}", other),
    }
}
