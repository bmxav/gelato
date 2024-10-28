use ast::{AstNode, BinaryOp, Expr, ExprNode, Identifier, StmtNode, TAstNode, TExpr, TExprNode, TStmtNode};

use thiserror::Error;

use std::collections::HashMap;

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

pub type TypedExpr = TExpr<Type>;

pub type TypedExprNode = TExprNode<Type>;

pub type TypedStmtNode = TStmtNode<Type>;

pub type TypedAstNode = TAstNode<Type>;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("type mismatch '{0:?}' and '{1:?}'")]
    TypeMismatch(Type, Type),
    #[error("expected type '{found:?}' but found '{expected:?}'")]
    UnexpectedType { expected: Type, found: Type },
    #[error("unknown type for identifier '{0}'")]
    UnknownType(String),
}

pub struct TypeChecker {
    identifier_types: HashMap<Identifier, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            identifier_types: HashMap::new(),
        }
    }

    pub fn type_check(&mut self, node: AstNode) -> Result<TypedAstNode, TypeError> {
        let typed = match node {
            AstNode::Stmt(stmt) => TypedAstNode::Stmt(self.type_check_stmt(stmt)?),
            AstNode::Expr(expr) => TypedAstNode::Expr(self.type_check_expr(expr)?),
        };
        Ok(typed)
    }

    fn type_check_stmt(&mut self, stmt: StmtNode) -> Result<TypedStmtNode, TypeError> {
        let typed = match stmt {
            StmtNode::Let { identifier, expr } => {
                let expr = self.type_check_expr(expr)?;
                self.identifier_types.insert(identifier.clone(), expr.t.clone());
                TypedStmtNode::Let { identifier: identifier.clone(), expr }
            }
            StmtNode::Var { identifier, expr } => {
                let expr = self.type_check_expr(expr)?;
                self.identifier_types.insert(identifier.clone(), expr.t.clone());
                TypedStmtNode::Var { identifier: identifier.clone(), expr }
            }
        };
        Ok(typed)
    }

    fn type_check_expr(&mut self, expr: ExprNode) -> Result<TypedExprNode, TypeError> {
        let typed = match expr.expr {
            Expr::Int(n) => TypedExprNode { expr: TypedExpr::Int(n), t: Type::Int },
            Expr::String(s) => TypedExprNode { expr: TypedExpr::String(s), t: Type::String },
            Expr::Bool(b) => TypedExprNode { expr: TypedExpr::Bool(b), t: Type::Bool },
            Expr::Identifier(ident) => {
                let t = self.identifier_types.get(&ident)
                    .ok_or(TypeError::UnknownType(ident.clone().to_string()))?.clone();
                TypedExprNode { expr: TypedExpr::Identifier(ident), t: t }
            }
            Expr::BinaryExpr { op, left, right } => {
                let left = Box::new(self.type_check_expr(*left)?);
                let right = Box::new(self.type_check_expr(*right)?);
                if left.t != right.t {
                    return Err(TypeError::TypeMismatch(left.t, right.t));
                }

                let t = match op {
                    // Assignment operator will always be unit.
                    BinaryOp::Assign | BinaryOp::AddAssign | BinaryOp::SubAssign  => Type::Unit,
                    BinaryOp::And
                        | BinaryOp::Eq
                        | BinaryOp::GreaterThan
                        | BinaryOp::GreaterThanEq
                        | BinaryOp::LessThan
                        | BinaryOp::LessThanEq
                        | BinaryOp::Or
                        | BinaryOp::NotEq => Type::Bool,
                    _ => left.t.clone(),
                };

                TypedExprNode { expr: TypedExpr::BinaryExpr { op, left, right }, t }
            }
            Expr::If { cond, then, els } => {
                let cond = Box::new(self.type_check_expr(*cond)?);
                if cond.t != Type::Bool {
                    return Err(TypeError::UnexpectedType { expected: Type::Bool, found: cond.t });
                }

                let then = Box::new(self.type_check_expr(*then)?);
                let els = match els.map(|e| self.type_check_expr(*e)).transpose()? {
                    Some(els) if then.t == els.t => Some(Box::new(els)),
                    Some(els) => {
                        return Err(TypeError::TypeMismatch(then.t, els.t));
                    }
                    None => None,
                };
                let t = then.t.clone();
                TypedExprNode { expr: TypedExpr::If { cond, then, els }, t }
            }
            Expr::Block(items) => {
                let mut block = Vec::with_capacity(items.len());
                let mut t = Type::Unit;
                for item in items {
                    let typed_item = match item {
                        AstNode::Stmt(stmt) => {
                            t = Type::Unit;
                            TypedAstNode::Stmt(self.type_check_stmt(stmt)?)
                        }
                        AstNode::Expr(expr) => {
                            let typed_expr = self.type_check_expr(expr)?;
                            t = typed_expr.t.clone();
                            TypedAstNode::Expr(typed_expr)
                        }
                    };
                    block.push(typed_item);
                }
                TypedExprNode { expr: TypedExpr::Block(block), t: t }
            }
        };

        Ok(typed)
    }
}
