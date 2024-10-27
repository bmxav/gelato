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
    PlusAssign,
    MinusAssign,
    MemberAccess,
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Identifier(String),
    String(String),
    Int(i64),
    BinaryExpr {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    Block(Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Stmt {
    Import {
        path: String,
        alias: Option<String>,
    },
    LetDecl {
        identifier: String,
        expr: Expr,
    },
    VarDecl {
        identifier: String,
        expr: Expr,
    }
}

#[derive(Debug)]
pub enum Node {
    Stmt(Stmt),
    Expr(Expr),
}
