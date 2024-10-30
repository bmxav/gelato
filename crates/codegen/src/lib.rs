pub mod toolchain;

use ast::{BinOp, Identifier, Literal, TypedBlock, TypedExpr, TypedExprKind, TypedModule, TypedStmt, TypedStmtKind, Type};

pub struct CodeGen<'a, W: std::io::Write> {
    output: &'a mut W,
}

impl<'a, W> CodeGen<'a, W> where W: std::io::Write {
    pub fn new(output: &'a mut W) -> Self {
        Self { output }
    }

    pub fn gen(&mut self, module: &TypedModule) -> Result<(), std::io::Error> {
        for stmt in &module.body.stmts {
            match &stmt.kind {
                TypedStmtKind::Let(ident, expr) => self.gen_let_stmt(ident, expr)?,
                TypedStmtKind::Var(ident, expr) => self.gen_var_stmt(ident, expr)?,
                TypedStmtKind::VarDecl(ident, ty) => self.gen_var_decl(ident, ty)?,
                TypedStmtKind::Assign(left, right) => self.gen_assignment(left, right)?,
                TypedStmtKind::OpAssign(op, left, right) => self.gen_op_assignment(op, left, right)?,
                TypedStmtKind::Expr(expr) => self.gen_expr(expr)?,
            }
        }
        Ok(())
    }

    fn gen_let_stmt(&mut self, identifier: &Identifier, expr: &TypedExpr) -> Result<(), std::io::Error> {
        write!(self.output, "const ")?;
        self.gen_identifier(identifier)?;
        write!(self.output, " ")?;
        self.gen_type(&expr.ty)?;
        write!(self.output, " = ")?;
        self.gen_expr(expr)?;
        write!(self.output, ";")
    }

    fn gen_var_stmt(&mut self, identifier: &Identifier, expr: &TypedExpr) -> Result<(), std::io::Error> {
        self.gen_identifier(identifier)?;
        write!(self.output, " := ")?;
        self.gen_expr(expr)?;
        write!(self.output, ";")
    }

    fn gen_var_decl(&mut self, ident: &Identifier, ty: &Type) -> Result<(), std::io::Error> {
        write!(self.output, "var ")?;
        self.gen_identifier(ident)?;
        write!(self.output, " ")?;
        self.gen_type(ty)?;
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

    fn gen_assignment(&mut self, left: &TypedExpr, right: &TypedExpr) -> Result<(), std::io::Error> {
        self.gen_expr(left)?;
        write!(self.output, " = ")?;
        self.gen_expr(right)?;
        write!(self.output, ";")
    }

    fn gen_op_assignment(&mut self, op: &BinOp, left: &TypedExpr, right: &TypedExpr) -> Result<(), std::io::Error> {
        self.gen_expr(left)?;

        let op_text = match op {
            BinOp::Add => "+=",
            BinOp::Sub => "-=",
            _ => panic!("invalid operator assignment {op:?}"),
        };
        write!(self.output, " {op_text} ")?;

        self.gen_expr(right)?;
        write!(self.output, ";")
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

    fn gen_expr(&mut self, expr: &TypedExpr) -> Result<(), std::io::Error> {
        match &expr.kind {
            TypedExprKind::Literal(Literal::Int(n)) => write!(self.output, "{}", n.to_string()).map(|_| ()),
            TypedExprKind::Literal(Literal::String(s)) => write!(self.output, "\"{}\"", s.to_string()).map(|_| ()),
            TypedExprKind::Literal(Literal::Bool(b)) => write!(self.output, "{}", b.to_string()).map(|_| ()),
            TypedExprKind::Literal(Literal::Identifier(ident)) => self.gen_identifier(ident),
            TypedExprKind::Binary(op, left, right) => self.gen_binary_expr(op, left, right),
            TypedExprKind::If(cond, then, els) => self.gen_if_expr(cond, then, els.as_ref()),
            TypedExprKind::Block(block) => self.gen_block(block),
        }
    }

    fn gen_if_expr(&mut self, cond: &TypedExpr, then: &TypedBlock, els: Option<&TypedBlock>) -> Result<(), std::io::Error> {
        write!(self.output, "if ")?;
        self.gen_expr(cond)?;
        write!(self.output, " {{")?;
        self.gen_block(then)?;

        if let Some(els) = els {
            write!(self.output, "}} else {{")?;
            self.gen_block(els)?;
        }

        write!(self.output, "}}")
    }

    fn gen_binary_expr(&mut self, op: &BinOp, left: &TypedExpr, right: &TypedExpr) -> Result<(), std::io::Error> {
        let op_text = match op {
            BinOp::Add => "+",
            BinOp::And => "&&",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Gt => ">",
            BinOp::Gte => ">=",
            BinOp::Lt => "<",
            BinOp::Lte => "<=",
            BinOp::Mul => "*",
            BinOp::Ne => "!=",
            BinOp::Or => "||",
            BinOp::Rem => "%",
            BinOp::Sub => "-",
        };

        self.gen_expr(left)?;
        write!(self.output, " {op_text} ")?;
        self.gen_expr(right)?;

        Ok(())
    }

    fn gen_block(&mut self, block: &TypedBlock) -> Result<(), std::io::Error> {
        for stmt in &block.stmts {
            match &stmt.kind {
                TypedStmtKind::Let(ident, expr) => self.gen_let_stmt(ident, expr)?,
                TypedStmtKind::Var(ident, expr) => self.gen_var_stmt(ident, expr)?,
                TypedStmtKind::VarDecl(ident, ty) => self.gen_var_decl(ident, ty)?,
                TypedStmtKind::Assign(left, right) => self.gen_assignment(left, right)?,
                TypedStmtKind::OpAssign(op, left, right) => self.gen_op_assignment(op, left, right)?,
                TypedStmtKind::Expr(expr) => self.gen_expr(expr)?,
            }
        }

        Ok(())
    }

}
