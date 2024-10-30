use thiserror::Error;

use ast::{BinOp, Identifier, Literal, Type, TypedBlock, TypedExpr, TypedExprKind, TypedModule, TypedStmt, TypedStmtKind};

#[derive(Error, Debug)]
pub enum ExpansionError {
    #[error("expansion of an expression of Unit type is not currently supported")]
    UnitExpansion,
}

pub struct Expander {
}

impl Expander {
    pub fn new() -> Self {
        Self {}
    }

    pub fn expand(&self, module: TypedModule) -> Result<TypedModule, ExpansionError> {
        let body = self.expand_block(module.body)?;
        Ok(TypedModule { body })
    }

    fn expand_block(&self, block: TypedBlock) -> Result<TypedBlock, ExpansionError> {
        let mut expanded_block = TypedBlock { stmts: Vec::new(), ty: block.ty.clone() };

        for stmt in block.stmts {
            self.expand_stmt(stmt, &mut expanded_block)?;
        }

        Ok(expanded_block)
    }

    fn assign_block(&self, ident: Identifier, block: &mut TypedBlock) -> Result<(), ExpansionError> {
        let ident = TypedExpr { kind: TypedExprKind::Literal(Literal::Identifier(ident)), ty: Type::Unit };
        // Rewrite the last statement in the block as an assignment to the given identifier.
        // Right now only expressions that do not evaluate to to the Unit type are supported.
        match block.stmts.pop() {
            Some(TypedStmt { kind: TypedStmtKind::Expr(expr), ty }) if ty != Type::Unit => {
                let stmt = TypedStmt {
                    kind: TypedStmtKind::Assign(ident, expr),
                    ty: Type::Unit,
                };
                block.stmts.push(stmt);

                // End of block is now an assignment, so it has the Unit type.
                block.ty = Type::Unit;

                Ok(())
            }
            _ => return Err(ExpansionError::UnitExpansion)
        }
    }

    fn expand_stmt(&self, stmt: TypedStmt, current_block: &mut TypedBlock) -> Result<(), ExpansionError> {
        match stmt.kind {
            TypedStmtKind::Let(ident, expr) => {
                match expr {
                    TypedExpr { kind: TypedExprKind::If(cond, mut then, mut els), ty } => {
                        // Add the var declaration for the type being assigned.
                        let decl = TypedStmt {
                            kind: TypedStmtKind::VarDecl(ident.clone(), ty.clone()),
                            ty: Type::Unit,
                        };
                        current_block.stmts.push(decl);

                        // Rewrite the `then` block.
                        self.assign_block(ident.clone(), &mut then)?;

                        // Rewrite the `else` block if it exists.
                        els.as_mut()
                            .map(|e| self.assign_block(ident.clone(), e))
                            .transpose()?;


                        // Push the `if` statement onto the current block.
                        let expanded_if = TypedExpr {
                            kind: TypedExprKind::If(cond, then, els),
                            ty: Type::Unit,
                        };

                        current_block.stmts.push(TypedStmt {
                            kind: TypedStmtKind::Expr(expanded_if),
                            ty: Type::Unit,
                        });
                    }
                    TypedExpr { kind: TypedExprKind::Block(mut block), ty } => {
                        let decl = TypedStmt {
                            kind: TypedStmtKind::VarDecl(ident.clone(), ty.clone()),
                            ty: Type::Unit,
                        };
                        current_block.stmts.push(decl);

                        self.assign_block(ident.clone(), &mut block)?;

                        let expanded_block = TypedExpr {
                            kind: TypedExprKind::Block(block),
                            ty: Type::Unit,
                        };

                        current_block.stmts.push(TypedStmt {
                            kind: TypedStmtKind::Expr(expanded_block),
                            ty: Type::Unit,
                        });
                    }
                    _ => {
                        let decl = TypedStmt {
                            kind: TypedStmtKind::Let(ident, expr),
                            ty: Type::Unit,
                        };
                        current_block.stmts.push(decl);
                    }
                }
            }
            _ => current_block.stmts.push(stmt),
        }
        Ok(())
    }
}
