use crate::token;

use std::fmt;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let {
        token: token::Token,
        name: Identifier,
        expression: Box<Expression>,
    },
    Return {
        token: token::Token,
        value: Expression,
    },
    Expression {
        token: token::Token,
        value: Expression,
    },
    Block {
        token: token::Token,
        statements: Vec<Statement>,
    },
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        use Statement::*;
        match self {
            Let { token, .. } => token.literal.clone(),
            Return { token, .. } => token.literal.clone(),
            Expression { token, .. } => token.literal.clone(),
            Block { token, .. } => token.literal.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    IntLiteral {
        token: token::Token,
        value: u32,
    },
    BooleanLiteral {
        token: token::Token,
        value: bool,
    },
    StringLiteral {
        token: token::Token,
        value: String,
    },
    ArrayLiteral {
        token: token::Token,
        values: Vec<Expression>,
    },
    MapLiteral {
        token: token::Token,
        values: Vec<(Expression, Expression)>,
    },
    Index {
        token: token::Token,
        container: Box<Expression>,
        index: Box<Expression>,
    },
    FunctionLiteral {
        token: token::Token,
        name: Option<String>,
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
    Identifier(Identifier),
    Prefix {
        token: token::Token,
        op: String,
        rhs: Box<Expression>,
    },
    Infix {
        token: token::Token,
        op: String,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    If {
        token: token::Token,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Call {
        token: token::Token,
        function: Function,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    Identifier(Identifier),
    Literal {
        token: token::Token,
        name: Option<String>,
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
}

impl Into<Expression> for Function {
    fn into(self) -> Expression {
        use Function::*;
        match self {
            Identifier(ident) => Expression::Identifier(ident),
            Literal {
                token,
                name,
                parameters,
                body,
            } => Expression::FunctionLiteral {
                token,
                name,
                parameters,
                body,
            },
        }
    }
}

impl Function {
    pub fn from_expression(exp: Expression) -> Self {
        match exp {
            Expression::Identifier(identifier) => Function::Identifier(identifier),
            Expression::FunctionLiteral {
                token,
                name,
                parameters,
                body,
            } => Function::Literal {
                token,
                name,
                parameters,
                body,
            },
            other => panic!("Expression {:?} cannot be used as a function", other),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        use Expression::*;
        match self {
            IntLiteral { token, .. } => token.literal.clone(),
            BooleanLiteral { token, .. } => token.literal.clone(),
            StringLiteral { token, .. } => token.literal.clone(),
            ArrayLiteral { token, .. } => token.literal.clone(),
            MapLiteral { token, .. } => token.literal.clone(),
            Index { token, .. } => token.literal.clone(),
            FunctionLiteral { token, .. } => token.literal.clone(),
            Identifier(ident) => ident.token.literal.clone(),
            Prefix { token, .. } => token.literal.clone(),
            Infix { token, .. } => token.literal.clone(),
            If { token, .. } => token.literal.clone(),
            Call { token, .. } => token.literal.clone(),
        }
    }
}

#[derive(Clone)]
pub struct Program(pub Vec<Statement>);

pub fn token_literal(p: &Program) -> String {
    if !p.0.is_empty() {
        p.0[0].token_literal()
    } else {
        String::new()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.0.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let {
                name, expression, ..
            } => write!(f, "let {} = {}", name, expression),
            Statement::Return { value, .. } => write!(f, "return {};", value),
            Statement::Expression { value, .. } => write!(f, "{}", value),
            Statement::Block { statements, .. } => {
                for stmt in statements {
                    write!(f, "{}", stmt)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntLiteral { value, .. } => write!(f, "{}", value),
            Expression::BooleanLiteral { value, .. } => write!(f, "{}", value),
            Expression::StringLiteral { value, .. } => write!(f, "{}", value),
            Expression::ArrayLiteral { values, .. } => {
                write!(f, "[")?;
                let mut first = true;
                for value in values {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Expression::MapLiteral { values, .. } => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in values {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            Expression::Index {
                container, index, ..
            } => write!(f, "({}[{}])", container, index),
            Expression::FunctionLiteral {
                name,
                parameters,
                body,
                ..
            } => {
                write!(f, "<{}>", name.as_ref().unwrap_or(&"anonymous".to_owned()))?;
                write!(f, "fn(")?;
                let mut first = true;
                for parameter in parameters {
                    if !first {
                        write!(f, ", ")?;
                    } else {
                        first = false
                    }
                    write!(f, "{}", parameter)?;
                }
                write!(f, ") {}", body)
            }
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::Prefix { op, rhs, .. } => write!(f, "({}{})", op, rhs),
            Expression::Infix { op, lhs, rhs, .. } => write!(f, "({} {} {})", lhs, op, rhs),
            Expression::If {
                condition,
                consequence,
                alternative: Some(alternative),
                ..
            } => write!(f, "if {} {} else {}", condition, consequence, alternative),
            Expression::If {
                condition,
                consequence,
                alternative: None,
                ..
            } => write!(f, "if {} {}", condition, consequence),
            Expression::Call {
                function,
                arguments,
                ..
            } => {
                write!(f, "{}", Into::<Expression>::into(function.clone()))?;
                write!(f, "(")?;
                let mut first = true;
                for argument in arguments {
                    if first {
                        first = false
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", argument)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
