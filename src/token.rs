pub type TokenType = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub tpe: TokenType,
    pub literal: String,
}

pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str = "EOF";

pub const IDENT: &str = "IDENT";
pub const INT: &str = "INT";
pub const STRING: &str = "STRING";

pub const LET: &str = "LET";
pub const FUNCTION: &str = "FUNCTION";
pub const TRUE: &str = "TRUE";
pub const FALSE: &str = "FALSE";
pub const IF: &str = "IF";
pub const ELSE: &str = "ELSE";
pub const RETURN: &str = "RETURN";

pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";
pub const MINUS: &str = "-";
pub const BANG: &str = "!";
pub const ASTERISK: &str = "*";
pub const SLASH: &str = "/";

pub const EQ: &str = "==";
pub const NEQ: &str = "!=";
pub const LT: &str = "<";
pub const GT: &str = ">";

pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";
pub const COLON: &str = ":";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";
