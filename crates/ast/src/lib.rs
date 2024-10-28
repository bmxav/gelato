use std::ops::Deref;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    And,
    Or,
    Assign,
    AddAssign,
    SubAssign,
    MemberAccess,
}

#[derive(Debug)]
pub struct TExprNode<T> {
    pub expr: TExpr<T>,
    pub t: T,
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
pub enum TExpr<T> {
    Identifier(Identifier),
    String(String),
    Int(i64),
    Bool(bool),
    BinaryExpr {
        op: BinaryOp,
        left: Box<TExprNode<T>>,
        right: Box<TExprNode<T>>,
    },
    If {
        cond: Box<TExprNode<T>>,
        then: Box<TExprNode<T>>,
        els: Option<Box<TExprNode<T>>>,
    },
    Block(Vec<TAstNode<T>>),
}

#[derive(Debug)]
pub enum TStmtNode<T> {
    Let {
        identifier: Identifier,
        expr: TExprNode<T>,
    },
    Var {
        identifier: Identifier,
        expr: TExprNode<T>,
   }
}

#[derive(Debug)]
pub enum TAstNode<T> {
    Stmt(TStmtNode<T>),
    Expr(TExprNode<T>),
}

pub type Expr = TExpr<()>;
pub type ExprNode = TExprNode<()>;

impl ExprNode {
    pub fn new(expr: Expr) -> ExprNode {
        ExprNode { expr, t: () }
    }
}

pub type StmtNode = TStmtNode<()>;
pub type AstNode = TAstNode<()>;
