pub mod toolchain;

use ast::Ident;
use syntax::{GoBinOp, GoBlock, GoExpr, GoIf, GoLiteral, GoPackage, GoStmt, GoType};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodeGenError {
    #[error("io error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("invalid assignment operator: {0}")]
    InvalidAssignmentOperator(String),
}

pub struct CodeGen<'a, W: std::io::Write> {
    output: &'a mut W,
}

impl<'a, W> CodeGen<'a, W> where W: std::io::Write {
    pub fn new(output: &'a mut W) -> Self {
        Self { output }
    }

    pub fn gen(&mut self, package: &GoPackage) -> Result<(), CodeGenError> {
        self.gen_block(&package.body)
    }

    fn gen_block(&mut self, block: &GoBlock) -> Result<(), CodeGenError> {
        write!(self.output, "{{ ")?;
        for stmt in block.iter() {
            match stmt {
                GoStmt::Var(ident, _, expr) => self.gen_var_init(ident, expr)?,
                GoStmt::VarDecl(ident, typ) => self.gen_var_decl(ident, typ)?,
                GoStmt::Assign(left, right) => self.gen_assign(left, right)?,
                GoStmt::OpAssign(op, left, right) => self.gen_op_assign(op, left, right)?,
                GoStmt::If(if_stmt) => self.gen_if(if_stmt)?,
                GoStmt::Block(block) => self.gen_block(block)?,
                GoStmt::Expr(expr) => self.gen_expr(expr)?,
            }
        }
        write!(self.output, "}} ")?;
        Ok(())
    }

    fn gen_var_init(&mut self, ident: &Ident, expr: &GoExpr) -> Result<(), CodeGenError> {
        write!(self.output, "{ident} := ")?;
        self.gen_expr(expr)?;
        write!(self.output, "; ")?;
        Ok(())
    }

    fn gen_var_decl(&mut self, ident: &Ident, typ: &GoType) -> Result<(), CodeGenError> {
        write!(self.output, "var {ident} ")?;
        self.gen_type(typ)?;
        write!(self.output, "; ")?;
        Ok(())
    }

    fn gen_assign(&mut self, left: &GoExpr, right: &GoExpr) -> Result<(), CodeGenError> {
        self.gen_expr(left)?;
        write!(self.output, " = ")?;
        self.gen_expr(right)?;
        write!(self.output, "; ")?;
        Ok(())
    }

    fn gen_op_assign(&mut self, op: &GoBinOp, left: &GoExpr, right: &GoExpr) -> Result<(), CodeGenError> {
        self.gen_expr(left)?;

        let op_text = match op {
            GoBinOp::Add => "+=",
            GoBinOp::Sub => "-=",
            GoBinOp::Mul => "*=",
            GoBinOp::Div => "/=",
            GoBinOp::Rem => "%=",
            _ => {
                let text = self.binop_text(op);
                return Err(CodeGenError::InvalidAssignmentOperator(text.to_string()));
            }
        };
        write!(self.output, " {op_text} ")?;

        self.gen_expr(right)?;
        write!(self.output, "; ")?;

        Ok(())
    }

    fn gen_if(&mut self, stmt: &GoIf) -> Result<(), CodeGenError> {
        write!(self.output, "if ")?;

        self.gen_expr(&stmt.cond)?;
        write!(self.output, " ")?;

        self.gen_block(&stmt.then)?;

        if let Some(block) = stmt.els.as_ref() {
            write!(self.output, "else ")?;
            self.gen_block(block)?;
        }

        Ok(())
    }

    fn gen_expr(&mut self, expr: &GoExpr) -> Result<(), CodeGenError> {
        match expr {
            GoExpr::Variable(ident) => write!(self.output, "{ident}")?,
            GoExpr::Literal(literal) => {
                let text = match literal {
                    GoLiteral::Bool(b) => b.to_string(),
                    GoLiteral::Int(i) => i.to_string(),
                    GoLiteral::String(s) => format!("\"{s}\""),
                };
                write!(self.output, "{text}")?;
            }
            GoExpr::Binary(op, left, right) => {
                write!(self.output, "(")?;
                self.gen_expr(&*left)?;
                let op = self.binop_text(op);
                write!(self.output, " {op} ")?;
                self.gen_expr(&*right)?;
                write!(self.output, ")")?;
            }
        }
        Ok(())
    }

    fn gen_type(&mut self, typ: &GoType) -> Result<(), CodeGenError> {
        let text = match typ {
            GoType::Any => "any",
            GoType::Bool => "bool",
            GoType::Int => "int",
            GoType::String => "string",
        };
        write!(self.output, "{text}")?;
        Ok(())
    }

    fn binop_text(&self, op: &GoBinOp) -> &'static str {
        match op {
            GoBinOp::Add => "+",
            GoBinOp::Sub => "-",
            GoBinOp::Mul => "*",
            GoBinOp::Div => "/",
            GoBinOp::Rem => "%",
            GoBinOp::Eq => "==",
            GoBinOp::Ne => "!=",
            GoBinOp::Lt => "<",
            GoBinOp::Lte => "<=",
            GoBinOp::Gt => ">",
            GoBinOp::Gte => ">=",
            GoBinOp::And => "&&",
            GoBinOp::Or => "||",
        }

    }

}
