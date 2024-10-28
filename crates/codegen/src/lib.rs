pub mod toolchain;

use ast::{BinaryOp, Identifier};
use syntax::{Type, TypedAstNode, TypedExpr, TypedExprNode, TypedStmtNode};

pub struct CodeGen<'a, W: std::io::Write> {
    output: &'a mut W,
}

impl<'a, W> CodeGen<'a, W> where W: std::io::Write {
    pub fn new(output: &'a mut W) -> Self {
        Self { output }
    }

    pub fn gen(&mut self, node: &TypedAstNode) -> Result<(), std::io::Error> {
        match node {
            TypedAstNode::Stmt(stmt) => self.gen_stmt(stmt),
            TypedAstNode::Expr(expr) => self.gen_expr(expr),
        }
    }

    fn gen_stmt(&mut self, stmt: &TypedStmtNode) -> Result<(), std::io::Error> {
        match stmt {
            TypedStmtNode::Let { identifier, expr } => self.gen_let_stmt(identifier, expr),
            TypedStmtNode::Var { identifier, expr } => self.gen_var_stmt(identifier, expr),
        }
    }

    fn gen_let_stmt(&mut self, identifier: &Identifier, expr: &TypedExprNode) -> Result<(), std::io::Error> {
        write!(self.output, "const ")?;
        self.gen_identifier(identifier)?;
        write!(self.output, " ")?;
        self.gen_type(&expr.t)?;
        write!(self.output, " = ")?;
        self.gen_expr(expr)?;
        write!(self.output, ";")
    }

    fn gen_var_stmt(&mut self, identifier: &Identifier, expr: &TypedExprNode) -> Result<(), std::io::Error> {
        self.gen_identifier(identifier)?;
        write!(self.output, " := ")?;
        self.gen_expr(expr)?;
        write!(self.output, ";")
    }

    fn gen_identifier(&mut self, identifier: &Identifier) -> Result<(), std::io::Error> {
        // The names of identifiers we get have been resolved and are not valid Go identifiers.
        // Currently the convention used is "{name}.{id}", so we will just take the name portion
        // to make it easier to debug generated code.
        let name = identifier.split_once(".")
            .map(|(name, _)| name.to_string())
            .unwrap_or(identifier.to_string());

        write!(self.output, "{name}")
    }

    fn gen_type(&mut self, ty: &Type) -> Result<(), std::io::Error> {
        let name = match ty {
            Type::Int => "int",
            Type::String => "string",
            Type::Bool => "bool",
            Type::Unit => "",
        };
        write!(self.output, "{name}")
    }

    fn gen_expr(&mut self, expr: &TypedExprNode) -> Result<(), std::io::Error> {
        match &expr.expr {
            TypedExpr::Int(n) => write!(self.output, "{}", n.to_string()).map(|_| ()),
            TypedExpr::String(s) => write!(self.output, "\"{}\"", s.to_string()).map(|_| ()),
            TypedExpr::Bool(b) => write!(self.output, "{}", b.to_string()).map(|_| ()),
            TypedExpr::Identifier(ident) => self.gen_identifier(ident),
            TypedExpr::BinaryExpr { op, left, right } => self.gen_binary_expr(op, left, right),
            TypedExpr::If { cond, then, els } => self.gen_if_expr(cond, then, els.as_ref().map(|e| e.as_ref())),
            TypedExpr::Block(nodes) => self.gen_block_expr(nodes),
        }
    }

    fn gen_if_expr(&mut self, cond: &TypedExprNode, then: &TypedExprNode, els: Option<&TypedExprNode>) -> Result<(), std::io::Error> {
        write!(self.output, "if ")?;
        self.gen_expr(cond)?;
        write!(self.output, " {{")?;
        self.gen_expr(then)?;

        if let Some(els) = els {
            write!(self.output, "}} else {{")?;
            self.gen_expr(els)?;
        }

        write!(self.output, "}}")
    }

    fn gen_binary_expr(&mut self, op: &BinaryOp, left: &TypedExprNode, right: &TypedExprNode) -> Result<(), std::io::Error> {
        let op_text = match op {
            BinaryOp::Add => "+",
            BinaryOp::And => "&&",
            BinaryOp::Assign => "=",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::GreaterThan => ">",
            BinaryOp::GreaterThanEq => ">=",
            BinaryOp::LessThan => "<",
            BinaryOp::LessThanEq => "<=",
            BinaryOp::MemberAccess => ".",
            BinaryOp::SubAssign => "-=",
            BinaryOp::Mul => "*",
            BinaryOp::NotEq => "!=",
            BinaryOp::Or => "||",
            BinaryOp::AddAssign => "+=",
            BinaryOp::Sub => "-",
        };

        self.gen_expr(left)?;
        write!(self.output, " {op_text} ")?;
        self.gen_expr(right)?;

        match op {
            BinaryOp::Assign | BinaryOp::AddAssign | BinaryOp::SubAssign => {
                write!(self.output, ";")?;
            }
            _ => {},
        }

        Ok(())
    }

    fn gen_block_expr(&mut self, nodes: &[TypedAstNode]) -> Result<(), std::io::Error> {
        for node in nodes {
            match node {
                TypedAstNode::Stmt(stmt) => self.gen_stmt(stmt)?,
                TypedAstNode::Expr(expr) => self.gen_expr(expr)?,
            }
        }
        Ok(())
    }

}
