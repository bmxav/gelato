mod writer;

pub use writer::Writer;

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
    }
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
}

#[derive(Debug)]
pub struct Block {
    children: Vec<Node>,
}

impl Block {
    pub fn new() -> Self {
        Self {
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, node: Node) {
        self.children.push(node)
    }

    pub fn iter(&self) -> BlockIterator {
        BlockIterator {
            block: self,
            index: 0,
        }
    }
}

pub struct BlockIterator<'a> {
    block: &'a Block,
    index: usize,
}

impl<'a> Iterator for BlockIterator<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.block.children.len() {
            let node = &self.block.children[self.index];
            self.index += 1;
            Some(node)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Block(Block),
    Stmt(Stmt),
    Expr(Expr),
}
