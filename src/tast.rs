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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeId {
    x: i32,
}

impl TypeId {
    pub fn new() -> Self {
        Self { x: 0 }
    }

    pub fn next(self) -> (Self, Self) {
        let next = Self { x: self.x + 1 };
        (self, next)
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
        tpe: TypeVariable,
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
            FunctionLiteral { tpe, .. } => tpe.clone(),
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
        parameters: Vec<Identifier>,
        body: Box<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Equation {
    IsEqual(TypeVariable, TypeVariable),
}

pub type Equations = Vec<Equation>;
