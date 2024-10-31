use ast::{BinOp, Ident, Literal, Type};

use super::{TypedBlock, TypedExpr, TypedModule, TypedStmt};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConversionError {
    #[error("expected expression, but found statement")]
    ExpectedExpression,
}

#[derive(Clone, Debug)]
pub enum GoBinOp {
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

#[derive(Clone, Debug)]
pub enum GoType {
    Any,
    Bool,
    Int,
    String,
}

#[derive(Clone, Debug)]
pub enum GoLiteral {
    Bool(bool),
    Int(i64),
    String(String),
}

#[derive(Clone, Debug)]
pub enum GoExpr {
    Variable(Ident),
    Literal(GoLiteral),
    Binary(GoBinOp, Box<GoExpr>, Box<GoExpr>),
}

#[derive(Clone, Debug)]
pub struct GoBlock {
    stmts: Vec<GoStmt>,
}

impl GoBlock {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
        }
    }

    pub fn with_body(stmts: Vec<GoStmt>) -> Self {
        Self {
            stmts,
        }
    }

    pub fn add_child(&mut self, stmt: GoStmt) {
        self.stmts.push(stmt);
    }

    pub fn remove_last_child(&mut self) -> Option<GoStmt> {
        self.stmts.pop()
    }

    pub fn last_child(&self) -> Option<&GoStmt> {
        self.stmts.last()
    }

    pub fn iter(&self) -> impl Iterator<Item=&GoStmt> {
        self.stmts.iter()
    }
}

impl IntoIterator for GoBlock {
    type Item = GoStmt;
    type IntoIter = <Vec<GoStmt> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.stmts.into_iter()
    }
}

pub struct GoBlockIterator<'a> {
    block: &'a GoBlock,
    index: usize,
}

impl<'a> Iterator for GoBlockIterator<'a> {
    type Item = &'a GoStmt;

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

#[derive(Clone, Debug)]
pub struct GoIf {
    pub cond: GoExpr,
    pub then: GoBlock,
    pub els: Option<GoBlock>,
}

#[derive(Clone, Debug)]
pub enum GoStmt {
    Var(Ident, GoType, GoExpr),
    VarDecl(Ident, GoType),
    Assign(GoExpr, GoExpr),
    OpAssign(GoBinOp, GoExpr, GoExpr),
    If(GoIf),
    Block(GoBlock),
    Expr(GoExpr),
}

#[derive(Clone, Debug)]
pub struct GoPackage {
    pub name: String,
    pub body: GoBlock,
}

pub struct Translator {

}

impl Translator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn translate(&self, module: TypedModule) ->  Result<GoPackage, ConversionError> {
        let block = self.translate_block(module.body)?;

        let package = GoPackage {
            name: module.name,
            body: block,
        };

        Ok(package)
    }

    fn translate_block(&self, block: TypedBlock) -> Result<GoBlock, ConversionError> {
        let mut go_block = GoBlock::new();

        for stmt in block {
            self.translate_stmt(stmt, &mut go_block)?;
        }

        Ok(go_block)
    }

    // `if` blocks in Go are not expressions, so we need to convert them to a statement.
    fn translate_if_expr(&self, cond: TypedExpr, then: TypedBlock, els: Option<TypedBlock>) -> Result<GoIf, ConversionError> {
        let cond = self.expect_expr(cond)?;
        let then = self.translate_block(then)?;
        let els = els.map(|block| self.translate_block(block)).transpose()?;
        Ok(GoIf { cond, then, els })
    }

    fn translate_stmt(&self, stmt: TypedStmt, scope: &mut GoBlock) -> Result<(), ConversionError> {
        // `let` bindings that are the result of a block expression will be treated the same
        // as `var` bindings, since we will have to mutate them to perform the assignment.

        match stmt {
            TypedStmt::Let(typ, ident, TypedExpr::If(_, cond, then, els))
                | TypedStmt::Var(typ, ident, TypedExpr::If(_, cond, then, els)) => {
                // Create a variable to assign the result of the expression to.
                // This will reference the `var` declaration we are adding to the scope.
                let left = GoExpr::Variable(ident.clone());
                let mut if_stmt = self.translate_if_expr(*cond, then, els)?;
                self.assign_if(left, &mut if_stmt)?;

                // Add the variable declaration to the scope.
                scope.add_child(GoStmt::VarDecl(ident.clone(), self.translate_type(typ.kind)));
                scope.add_child(GoStmt::If(if_stmt));
            }
            TypedStmt::Let(typ, ident, TypedExpr::Block(_, block))
                | TypedStmt::Var(typ, ident, TypedExpr::Block(_, block)) => {
                // Create a variable to assign the result of the expression to.
                // This will reference the `var` declaration we are adding to the scope.
                let left = GoExpr::Variable(ident.clone());

                let mut block = self.translate_block(block)?;
                self.assign_block(left, &mut block);

                // Add the variable declaration to the scope.
                scope.add_child(GoStmt::VarDecl(ident.clone(), self.translate_type(typ.kind)));
                scope.add_child(GoStmt::Block(block));
            }
            TypedStmt::Let(typ, ident, expr) | TypedStmt::Var(typ, ident, expr) => {
                let expr = self.expect_expr(expr)?;
                scope.add_child(GoStmt::Var(ident.clone(), self.translate_type(typ.kind), expr));
            }
            TypedStmt::Assign(left, TypedExpr::If(_, cond, then, els)) => {
                // Add the rewritten `if` statement to the current scope.
                let left = self.expect_expr(left)?;
                let mut if_stmt = self.translate_if_expr(*cond, then, els)?;
                self.assign_if(left, &mut if_stmt)?;
                scope.add_child(GoStmt::If(if_stmt));
            }
            TypedStmt::Assign(left, TypedExpr::Block(_, block)) => {
                let left = self.expect_expr(left)?;
                let mut block = self.translate_block(block)?;
                self.assign_block(left, &mut block);
                scope.add_child(GoStmt::Block(block));
            }
            TypedStmt::Assign(left, right) => {
                let left = self.expect_expr(left)?;
                let right = self.expect_expr(right)?;
                scope.add_child(GoStmt::Assign(left, right));
            }
            TypedStmt::OpAssign(op, left, TypedExpr::If(_, cond, then, els)) => {
                let op = self.translate_binop(op);
                let left = self.expect_expr(left)?;
                let mut if_stmt = self.translate_if_expr(*cond, then, els)?;
                self.op_assign_if(op, left, &mut if_stmt)?;
                scope.add_child(GoStmt::If(if_stmt));
            }
            TypedStmt::OpAssign(op, left, TypedExpr::Block(_, block)) => {
                let op = self.translate_binop(op);
                let left = self.expect_expr(left)?;
                let mut block = self.translate_block(block)?;
                self.op_assign_block(op, left, &mut block);
                scope.add_child(GoStmt::Block(block));
            }
            TypedStmt::OpAssign(op, left, right) => {
                let op = self.translate_binop(op);
                let left = self.expect_expr(left)?;
                let right = self.expect_expr(right)?;
                scope.add_child(GoStmt::OpAssign(op, left, right))
            }
            TypedStmt::Expr(expr) => {
                let expr = self.translate_expr(expr)?;
                scope.add_child(expr);
            }
        }

        Ok(())
    }

    fn assign_if(&self, left: GoExpr, if_stmt: &mut GoIf) -> Result<(), ConversionError> {
        self.assign_block(left.clone(), &mut if_stmt.then);
        if let Some(mut block) = if_stmt.els.as_mut() {
            self.assign_block(left, &mut block);
        }
        Ok(())
    }

    fn op_assign_if(&self, op: GoBinOp, left: GoExpr, stmt: &mut GoIf) -> Result<(), ConversionError> {
        self.op_assign_block(op.clone(), left.clone(), &mut stmt.then);
        if let Some(block) = stmt.els.as_mut() {
            self.op_assign_block(op.clone(), left.clone(), block);
        }
        Ok(())
    }

    fn assign_block(&self, left: GoExpr, block: &mut GoBlock) {
        match block.remove_last_child() {
            Some(GoStmt::Expr(expr)) => block.add_child(GoStmt::Assign(left, expr)),
            Some(child) => block.add_child(child),
            None => {}
        }
    }

    fn op_assign_block(&self, op: GoBinOp, left: GoExpr, block: &mut GoBlock) {
        match block.remove_last_child() {
            Some(GoStmt::Expr(expr)) => block.add_child(GoStmt::OpAssign(op, left, expr)),
            Some(child) => block.add_child(child),
            _ => {}
        }
    }

    fn translate_type(&self, typ: Type) -> GoType {
        match typ {
            Type::Int => GoType::Int,
            Type::String => GoType::String,
            Type::Bool => GoType::Bool,
            Type::Unit => GoType::Any,
        }
    }

    fn translate_binop(&self, op: BinOp) -> GoBinOp {
        match op {
            BinOp::Add => GoBinOp::Add,
            BinOp::Sub => GoBinOp::Sub,
            BinOp::Mul => GoBinOp::Mul,
            BinOp::Div => GoBinOp::Div,
            BinOp::Rem => GoBinOp::Rem,
            BinOp::Eq => GoBinOp::Eq,
            BinOp::Ne => GoBinOp::Ne,
            BinOp::Lt => GoBinOp::Lt,
            BinOp::Lte => GoBinOp::Lte,
            BinOp::Gt => GoBinOp::Gt,
            BinOp::Gte => GoBinOp::Gte,
            BinOp::And => GoBinOp::And,
            BinOp::Or => GoBinOp::Or,
        }
    }

    fn expect_expr(&self, expr: TypedExpr) -> Result<GoExpr, ConversionError> {
        match self.translate_expr(expr)? {
            GoStmt::Expr(expr) => Ok(expr),
            _ => Err(ConversionError::ExpectedExpression),
        }
    }

    fn translate_expr(&self, expr: TypedExpr) -> Result<GoStmt, ConversionError> {
        let go_stmt = match expr {
            ast::Expr::Literal(_, Literal::Bool(b)) => GoStmt::Expr(GoExpr::Literal(GoLiteral::Bool(b))),
            ast::Expr::Literal(_, Literal::Int(i)) => GoStmt::Expr(GoExpr::Literal(GoLiteral::Int(i))),
            ast::Expr::Literal(_, Literal::String(s)) => GoStmt::Expr(GoExpr::Literal(GoLiteral::String(s))),
            ast::Expr::Variable(_, ident) => GoStmt::Expr(GoExpr::Variable(ident)),
            ast::Expr::Binary(_, op, left, right) => {
                let op = self.translate_binop(op);
                let left = self.expect_expr(*left)?;
                let right = self.expect_expr(*right)?;
                GoStmt::Expr(GoExpr::Binary(op, Box::new(left), Box::new(right)))
            }
            ast::Expr::If(_, cond, then, els) => {
                let if_stmt = self.translate_if_expr(*cond, then, els)?;
                GoStmt::If(if_stmt)
            }
            ast::Expr::Block(_, block) => {
                let block = self.translate_block(block)?;
                GoStmt::Block(block)
            }
        };
        Ok(go_stmt)
    }
}
