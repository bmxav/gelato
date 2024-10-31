use std::fmt;

pub trait Phase: fmt::Debug {
    type Literal: fmt::Debug;
    type Variable: fmt::Debug;
    type Binary: fmt::Debug;
    type If: fmt::Debug;
    type Block: fmt::Debug;
}

#[derive(Debug)]
pub struct Empty {}

impl Phase for Empty {
    type Literal = ();
    type Variable = ();
    type Binary = ();
    type If = ();
    type Block = ();
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
    Unit
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

#[derive(Clone, Debug)]
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
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum Stmt<P: Phase = Empty> {
    Let(P::Variable, Ident, Expr<P>),
    Var(P::Variable, Ident, Expr<P>),
    VarDecl(P::Variable, Ident, Type),
    Assign(Expr<P>, Expr<P>),
    OpAssign(BinOp, Expr<P>, Expr<P>),
    Expr(Expr<P>),
}

#[derive(Debug)]
pub struct Block<P: Phase = Empty> {
    stmts: Vec<Stmt<P>>,
}

impl<P: Phase + fmt::Debug> Block<P> {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
        }
    }

    pub fn with_body(stmts: Vec<Stmt<P>>) -> Self {
        Self {
            stmts,
        }
    }

    pub fn add_child(&mut self, stmt: Stmt<P>) {
        self.stmts.push(stmt);
    }

    pub fn remove_last_child(&mut self) -> Option<Stmt<P>> {
        self.stmts.pop()
    }

    pub fn last_child(&self) -> Option<&Stmt<P>> {
        self.stmts.last()
    }

    pub fn iter(&self) -> impl Iterator<Item=&Stmt<P>> {
        self.stmts.iter()
    }
}

impl<P: Phase> IntoIterator for Block<P> {
    type Item = Stmt<P>;
    type IntoIter = <Vec<Stmt<P>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.stmts.into_iter()
    }
}

pub struct BlockIterator<'a, P: Phase = Empty> {
    block: &'a Block<P>,
    index: usize,
}

impl<'a, P: Phase> Iterator for BlockIterator<'a, P> {
    type Item = &'a Stmt<P>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.block.stmts.len() {
            let item = &self.block.stmts[self.index];
            self.index += 1;
            Some(item)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum Expr<P: Phase = Empty> {
    Literal(P::Literal, Literal),
    Variable(P::Variable, Ident),
    Binary(P::Binary, BinOp, Box<Expr<P>>, Box<Expr<P>>),
    If(P::If, Box<Expr<P>>, Block<P>, Option<Block<P>>),
    Block(P::Block, Block<P>),
}

#[derive(Debug)]
pub struct Module<P: Phase = Empty> {
    pub name: String,
    pub body: Block<P>,
}

impl<P: Phase> Module<P> {
    pub fn new(name: String, body: Block<P>) -> Self {
        Self {
            name,
            body,
        }
    }
}
