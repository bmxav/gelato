use ast::{BinaryOp, BlockItem, Expr, Node, Stmt};

use thiserror::Error;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unknown,
    Int,
    String,
    Unit,
}

#[derive(Debug)]
pub struct TypedExpr {
    pub expr: Expr,
    pub ty: Type
}

#[derive(Debug)]
pub enum TypedNode {
    Expr(TypedExpr),
    Stmt(Stmt),
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("type mismatch '{0:?}' and '{1:?}'")]
    TypeMismatch(Type, Type),
    #[error("unknown type for identifier '{0}'")]
    UnknownType(String),
}

pub struct TypeChecker {
    identifier_types: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            identifier_types: HashMap::new(),
        }
    }

    pub fn type_check(&mut self, node: Node) -> Result<TypedNode, TypeError> {
        let typed = match node {
            Node::Stmt(stmt) => {
                // Statements always return unit type, but we must check any expresions within.
                let _ = self.type_check_stmt(&stmt)?;
                TypedNode::Stmt(stmt)
            }
            Node::Expr(expr) => {
                let ty = self.type_check_expr(&expr)?;
                TypedNode::Expr(TypedExpr{ expr, ty })
            }
        };

        Ok(typed)
    }

    fn type_check_stmt(&mut self, stmt: &Stmt) -> Result<Type, TypeError> {
        match stmt {
            Stmt::Import { .. } => {},
            Stmt::LetDecl { identifier, expr } => {
                let ty = self.type_check_expr(&expr)?;
                self.identifier_types.insert(identifier.clone(), ty);
            }
            Stmt::VarDecl { identifier, expr } => {
                let ty = self.type_check_expr(&expr)?;
                self.identifier_types.insert(identifier.clone(), ty);
            }
        }
        Ok(Type::Unit)
    }

    fn type_check_expr(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        let typed_expr = match expr {
            Expr::Int(_) => Type::Int,
            Expr::String(_) => Type::String,
            Expr::Identifier(name) => {
                self.identifier_types.get(name)
                    .ok_or(TypeError::UnknownType(name.clone()))?.clone()
            }
            Expr::BinaryExpr { op, left, right, .. } => {
                let left_ty = self.type_check_expr(&*left)?;
                let right_ty = self.type_check_expr(&*right)?;
                if left_ty != right_ty {
                    return Err(TypeError::TypeMismatch(left_ty, right_ty));
                }

                match op {
                    // Assignment operator will always return unit type.
                    BinaryOp::Assign => Type::Unit,
                    _ => left_ty,
                }
            }
            Expr::If { cond, then, els } => {
                //TODO(bmxav): Once booleans are implemented, need to type check this condition.
                let _cond_ty = self.type_check_expr(&*cond)?;
                let then_ty = self.type_check_expr(&*then)?;
                if let Some(els_ty) = els.as_ref().map(|e| self.type_check_expr(e)).transpose()? {
                    if then_ty != els_ty {
                        return Err(TypeError::TypeMismatch(then_ty, els_ty));
                    }
                }
                then_ty
            }
            Expr::Block(items) => {
                let mut ty = Type::Unit;
                for item in items {
                    ty = match item {
                        BlockItem::Expr(expr) => self.type_check_expr(expr)?,
                        BlockItem::Stmt(stmt) => self.type_check_stmt(stmt)?,
                    };
                }
                ty
            }
        };

        Ok(typed_expr)
    }
}
