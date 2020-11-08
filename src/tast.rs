use crate::ast;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeVariable {
    Known(Type),
    Unknown(TypeId),
}

impl TypeVariable {
    pub fn is_known(&self) -> bool {
        match self {
            TypeVariable::Known(..) => true,
            TypeVariable::Unknown(..) => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Boolean,
    String_,
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId {
    x: i32,
}

impl TypeId {
    pub fn next(self) -> (TypeVariable, Self) {
        let next = Self { x: self.x + 1 };
        (TypeVariable::Unknown(self), next)
    }
}

impl Default for TypeId {
    fn default() -> Self {
        Self { x: 0 }
    }
}

pub trait Typed {
    fn tpe(&self) -> TypeVariable;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let {
        name: Identifier,
        expression: Box<Expression>,
    },
    Return {
        value: Expression,
    },
    Expression {
        value: Expression,
    },
    Block {
        statements: Vec<Statement>,
    },
}

#[derive(Debug)]
pub struct Program(pub Vec<Statement>);

impl Typed for Statement {
    fn tpe(&self) -> TypeVariable {
        use Statement::*;
        use TypeVariable::*;
        match self {
            Let { .. } => Known(Type::Unit),
            Return { value, .. } => value.tpe(),
            Expression { value, .. } => value.tpe(),
            Block { statements, .. } => {
                if statements.is_empty() {
                    Known(Type::Unit)
                } else {
                    statements[statements.len() - 1].tpe()
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub tpe: TypeVariable,
    pub value: String,
}

impl Typed for Identifier {
    fn tpe(&self) -> TypeVariable {
        self.tpe.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    IntLiteral {
        value: u32,
    },
    BooleanLiteral {
        value: bool,
    },
    StringLiteral {
        value: String,
    },
    ArrayLiteral {
        tpe: TypeVariable,
        values: Vec<Expression>,
    },
    MapLiteral {
        tpe: TypeVariable,
        values: Vec<(Expression, Expression)>,
    },
    Index {
        tpe: TypeVariable,
        container: Box<Expression>,
        index: Box<Expression>,
    },
    FunctionLiteral {
        return_type: TypeVariable,
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
    Identifier(Identifier),
    Prefix {
        tpe: TypeVariable,
        op: String,
        rhs: Box<Expression>,
    },
    Infix {
        tpe: TypeVariable,
        op: String,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    If {
        tpe: TypeVariable,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Call {
        tpe: TypeVariable,
        function: Function,
        arguments: Vec<Expression>,
    },
}

impl Typed for Expression {
    fn tpe(&self) -> TypeVariable {
        use Expression::*;
        use TypeVariable::*;
        match self {
            IntLiteral { .. } => Known(Type::Int),
            BooleanLiteral { .. } => Known(Type::Boolean),
            StringLiteral { .. } => Known(Type::String_),
            ArrayLiteral { tpe, .. } => tpe.clone(),
            MapLiteral { tpe, .. } => tpe.clone(),
            Index { tpe, .. } => tpe.clone(),
            FunctionLiteral { return_type, .. } => return_type.clone(),
            Identifier(ident) => ident.tpe.clone(),
            Prefix { tpe, .. } => tpe.clone(),
            Infix { tpe, .. } => tpe.clone(),
            If { tpe, .. } => tpe.clone(),
            Call { tpe, .. } => tpe.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    Identifier(Identifier),
    Literal {
        return_type: TypeVariable,
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
}

impl Typed for Function {
    fn tpe(&self) -> TypeVariable {
        use Function::*;
        match self {
            Identifier(ident) => ident.tpe.clone(),
            Literal { return_type, .. } => return_type.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Equation {
    IsEqual(TypeVariable, TypeVariable),
}

pub type Equations = Vec<Equation>;

pub type Solutions = HashMap<TypeId, Type>;

type Parameters = Vec<Identifier>;
type ReturnType = TypeVariable;

#[derive(Clone, Debug)]
pub struct Context {
    identifiers: HashMap<String, TypeVariable>,
    functions: HashMap<String, (Parameters, ReturnType)>,
}

impl Context {
    pub fn with_identifier(mut self, identifier: Identifier) -> Self {
        self.identifiers.insert(identifier.value, identifier.tpe);
        self
    }

    pub fn with_function(mut self, identifier: Identifier, parameters: Parameters) -> Self {
        self.functions
            .insert(identifier.value, (parameters, identifier.tpe));
        self
    }

    pub fn with_all(mut self, identifiers: Vec<Identifier>) -> Self {
        for identifier in identifiers {
            self = self.with_identifier(identifier)
        }
        self
    }

    pub fn lookup_identifier(&self, identifier: &ast::Identifier) -> Option<TypeVariable> {
        self.identifiers.get(&identifier.value).cloned()
    }

    pub fn lookup_function(&self, identifier: &Identifier) -> Option<(Parameters, ReturnType)> {
        self.functions.get(&identifier.value).cloned()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            identifiers: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}
