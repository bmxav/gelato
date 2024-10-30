use ast::{
    BinOp, Identifier, Literal, TypedBlock, TypedExpr, TypedExprKind, TypedModule, TypedStmt,
    TypedStmtKind, Type, UntypedBlock, UntypedExpr, UntypedExprKind, UntypedModule, UntypedStmt,
    UntypedStmtKind
};

use thiserror::Error;

use std::collections::HashMap;

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

    pub fn type_check(&mut self, module: UntypedModule) -> Result<TypedModule, TypeError> {
        let body = self.type_check_block(module.body)?;
        Ok(TypedModule { body })
    }

    fn type_check_block(&mut self, block: UntypedBlock) -> Result<TypedBlock, TypeError>  {
        let stmts = block.stmts.into_iter()
            .map(|stmt| self.type_check_stmt(stmt))
            .collect::<Result<Vec<TypedStmt>, TypeError>>()?;

        let ty = stmts.last().map(|s| s.ty.clone()).unwrap_or(Type::Unit);

        Ok(TypedBlock { stmts, ty })
    }

    fn type_check_stmt(&mut self, stmt: UntypedStmt) -> Result<TypedStmt, TypeError> {
        let typed_stmt = match stmt.kind {
            UntypedStmtKind::Let(ident, expr) => {
                let expr = self.type_check_expr(expr)?;
                self.identifier_types.insert(ident.clone(), expr.ty.clone());
                TypedStmt {
                    kind: TypedStmtKind::Let(ident, expr),
                    ty: Type::Unit,
                }
            }
            UntypedStmtKind::Var(ident, expr) => {
                let expr = self.type_check_expr(expr)?;
                self.identifier_types.insert(ident.clone(), expr.ty.clone());
                TypedStmt {
                    kind: TypedStmtKind::Var(ident, expr),
                    ty: Type::Unit,
                }
            }
            UntypedStmtKind::VarDecl(ident, ty) => {
                self.identifier_types.insert(ident.clone(), ty.clone());
                TypedStmt {
                    kind: TypedStmtKind::VarDecl(ident, ty),
                    ty: Type::Unit,
                }
            }
            UntypedStmtKind::Assign(left, right) => {
                let left = self.type_check_expr(left)?;
                let right = self.type_check_expr(right)?;

                //TODO: Check that left-hand side allows assignment.
                if left.ty != right.ty {
                    return Err(TypeError::TypeMismatch(left.ty, right.ty));
                }

                TypedStmt {
                    kind: TypedStmtKind::Assign(left, right),
                    ty: Type::Unit,
                }
            }
            UntypedStmtKind::OpAssign(op, left, right) => {
                let left = self.type_check_expr(left)?;
                let right = self.type_check_expr(right)?;

                //TODO: Check that left-hand side allows assignment and that both sides
                // are valid operands.
                if left.ty != right.ty {
                    return Err(TypeError::TypeMismatch(left.ty, right.ty));
                }

                TypedStmt {
                    kind: TypedStmtKind::OpAssign(op, left, right),
                    ty: Type::Unit,
                }
            }
            UntypedStmtKind::Expr(expr) => {
                let expr = self.type_check_expr(expr)?;
                let ty = expr.ty.clone();
                TypedStmt {
                    kind: TypedStmtKind::Expr(expr),
                    ty,
                }
            }
        };

        Ok(typed_stmt)
    }

    fn type_check_expr(&mut self, expr: UntypedExpr) -> Result<TypedExpr, TypeError> {
        let typed_expr = match expr.kind {
            UntypedExprKind::Literal(Literal::Identifier(ident)) => {
                let ty = self.identifier_types.get(&ident)
                    .ok_or(TypeError::UnknownType(ident.to_string()))?
                    .clone();
                TypedExpr {
                    kind: TypedExprKind::Literal(Literal::Identifier(ident)),
                    ty,
                }
            },
            UntypedExprKind::Literal(s @ Literal::String(_)) => {
                TypedExpr {
                    kind: TypedExprKind::Literal(s),
                    ty: Type::String,
                }
            },
            UntypedExprKind::Literal(i @ Literal::Int(_)) => {
                TypedExpr {
                    kind: TypedExprKind::Literal(i),
                    ty: Type::Int,
                }
            },
            UntypedExprKind::Literal(b @ Literal::Bool(_)) => {
                TypedExpr {
                    kind: TypedExprKind::Literal(b),
                    ty: Type::Bool,
                }
            },
            UntypedExprKind::If(cond, then, els) => {
                let cond = self.type_check_expr(*cond)?;
                if cond.ty != Type::Bool {
                    return Err(TypeError::UnexpectedType { expected: Type::Bool, found: cond.ty })
                }

                let then = self.type_check_block(then)?;
                let els = match els.map(|e| self.type_check_block(e)).transpose()? {
                    Some(els) if els.ty == then.ty => Some(els),
                    Some(els) => return Err(TypeError::TypeMismatch(then.ty, els.ty)),
                    None => None,
                };

                let ty = then.ty.clone();
                TypedExpr {
                    kind: TypedExprKind::If(Box::new(cond), then, els),
                    ty,
                }
            },
            UntypedExprKind::Block(block) => {
                let block = self.type_check_block(block)?;
                let ty = block.ty.clone();
                TypedExpr {
                    kind: TypedExprKind::Block(block),
                    ty,
                }
            },
            UntypedExprKind::Binary(op, left, right) => {
                let left = self.type_check_expr(*left)?;
                let right = self.type_check_expr(*right)?;

                if left.ty != right.ty {
                    return Err(TypeError::TypeMismatch(left.ty, right.ty));
                }

                //TODO: Ensure that the binop is supported by both operands.
                let ty = match op {
                    BinOp::And
                        | BinOp::Eq
                        | BinOp::Gt
                        | BinOp::Gte
                        | BinOp::Lt
                        | BinOp::Lte
                        | BinOp::Or
                        | BinOp::Ne => Type::Bool,
                    _ => left.ty.clone(),
                };

                TypedExpr {
                    kind: TypedExprKind::Binary(op, Box::new(left), Box::new(right)),
                    ty,
                }
            },
        };

        Ok(typed_expr)
    }
}
