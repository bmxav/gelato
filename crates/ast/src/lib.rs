use std::fmt;
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
    Unit,
}

impl Type {
    pub fn is_basic_type(&self) -> bool {
        match self {
            Self::Int | Self::String => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Type::Int => "Int",
            Type::String => "String",
            Type::Bool => "Bool",
            Type::Unit => "Unit",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

#[derive(Debug)]
pub struct Expr<T> {
    pub kind: ExprKind<T>,
    pub ty: T
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl Deref for Identifier {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub enum StmtKind<T> {
    Let(Identifier, Expr<T>),
    Var(Identifier, Expr<T>),
    VarDecl(Identifier, Type),
    Assign(Expr<T>, Expr<T>),
    OpAssign(BinOp, Expr<T>, Expr<T>),
    Expr(Expr<T>),
}

#[derive(Debug)]
pub struct Stmt<T> {
    pub kind: StmtKind<T>,
    pub ty: T,
}

#[derive(Debug)]
pub struct Block<T> {
    pub stmts: Vec<Stmt<T>>,
    pub ty: T,
}

#[derive(Debug)]
pub enum Literal {
    Identifier(Identifier),
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug)]
pub enum ExprKind<T> {
    Literal(Literal),
    Binary(BinOp, Box<Expr<T>>, Box<Expr<T>>),
    If(Box<Expr<T>>, Block<T>, Option<Block<T>>),
    Block(Block<T>),
}

#[derive(Debug)]
pub struct Module<T> {
    pub body: Block<T>,
}

pub type UntypedBlock = Block<()>;

impl UntypedBlock {
    pub fn new(stmts: Vec<UntypedStmt>) -> Self {
        Self { stmts, ty: () }
    }
}

pub type UntypedExpr = Expr<()>;

impl UntypedExpr {
    pub fn new(kind: UntypedExprKind) -> Self {
        Self { kind, ty: () }
    }
}

pub type UntypedExprKind = ExprKind<()>;

pub type UntypedStmt = Stmt<()>;

impl UntypedStmt {
    pub fn new(kind: UntypedStmtKind) -> Self {
        Self { kind, ty: () }
    }
}

pub type UntypedStmtKind = StmtKind<()>;

pub type UntypedModule = Module<()>;

impl UntypedModule {
    pub fn new(body: UntypedBlock) -> Self {
        Self { body }
    }
}

pub type TypedBlock = Block<Type>;
pub type TypedExpr = Expr<Type>;
pub type TypedExprKind = ExprKind<Type>;
pub type TypedStmt = Stmt<Type>;
pub type TypedStmtKind = StmtKind<Type>;
pub type TypedModule = Module<Type>;
