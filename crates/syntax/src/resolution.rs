use ast::{AstNode, BinaryOp, Expr, ExprNode, StmtNode};
use thiserror::Error;

use std::collections::HashMap;

#[derive(Error, Debug)]
pub enum ResolutionError {
    #[error("identifier '{0}' has not been defined")]
    UndefinedIdentifier(String),
    //TODO(bmxav): Without location information, this is going to be annoying.
    #[error("invalid assignment")]
    InvalidAssignment,
    #[error("identifier has already been defined in this scope")]
    DuplicateDefinition(String),
    #[error("assignment to immutable binding '{0}'")]
    ImmutableAssignment(String),
}

pub struct Binding {
    id: String,
    mutable: bool,
}

impl Binding {
    pub fn new(id: String, mutable: bool) -> Self {
        Self { id, mutable }
    }
}

pub struct Resolver {
    scopes: Vec<HashMap<String, Binding>>,
    counter: usize,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            counter: 0,
        }
    }

    pub fn resolve(&mut self, node: AstNode) -> Result<AstNode, ResolutionError> {

        let resolved_node = match node {
            AstNode::Stmt(stmt) => AstNode::Stmt(self.resolve_stmt(stmt)?),
            AstNode::Expr(expr) => AstNode::Expr(self.resolve_expr(expr)?),
        };

        Ok(resolved_node)
    }

    fn resolve_stmt(&mut self, stmt: StmtNode) -> Result<StmtNode, ResolutionError> {
        let resolved = match stmt {
            StmtNode::Let { identifier, expr } => {
                let resolved = self.resolve_expr(expr)?;
                let id = self.new_binding(&identifier, false)?;
                StmtNode::Let {
                    identifier: id,
                    expr: resolved,
                }
            }
            StmtNode::Var { identifier, expr } => {
                let resolved = self.resolve_expr(expr)?;
                let id = self.new_binding(&identifier, true)?;
                StmtNode::Var {
                    identifier: id,
                    expr: resolved,
                }
            }
        };
        Ok(resolved)
    }

    fn resolve_block(&mut self, block: Vec<AstNode>) -> Result<Expr, ResolutionError> {
        self.scopes.push(HashMap::new());

        let mut items = Vec::with_capacity(block.len());

        for item in block {
            let resolved = match item {
                AstNode::Stmt(stmt) => AstNode::Stmt(self.resolve_stmt(stmt)?),
                AstNode::Expr(expr) => AstNode::Expr(self.resolve_expr(expr)?),
            };
            items.push(resolved);
        }

        self.scopes.pop();

        Ok(Expr::Block(items))
    }

    fn resolve_expr(&mut self, expr: ExprNode) -> Result<ExprNode, ResolutionError> {
        let resolved = match expr.expr {
            Expr::Identifier(ident) => Expr::Identifier(self.find_binding(&ident)?.id.clone()),
            Expr::BinaryExpr { op: BinaryOp::Assign, left, right } => {
                let left_resolved = match left.expr {
                    Expr::Identifier(ident) if !self.find_binding(&ident)?.mutable => {
                        return Err(ResolutionError::ImmutableAssignment(ident.clone()))
                    }
                    Expr::Identifier(_) => {
                        self.resolve_expr(*left)?
                    }
                    _ => return Err(ResolutionError::InvalidAssignment)
                };

                let right_resolved = self.resolve_expr(*right)?;
                Expr::BinaryExpr {
                    op: BinaryOp::Assign,
                    left: Box::new(left_resolved),
                    right: Box::new(right_resolved)
                }

            }
            Expr::BinaryExpr { op, left, right } => {
                Expr::BinaryExpr {
                    op: op,
                    left: Box::new(self.resolve_expr(*left)?),
                    right: Box::new(self.resolve_expr(*right)?),
                }
            }
            Expr::If { cond, then, els } => {
                Expr::If {
                    cond: Box::new(self.resolve_expr(*cond)?),
                    then: Box::new(self.resolve_expr(*then)?),
                    els: els.map(|e| self.resolve_expr(*e)).transpose()?.map(Box::new),
                }
            }
            Expr::Block(items) => {
                self.resolve_block(items)?
            }
            e => e,
        };
        Ok(ExprNode::new(resolved))
    }

    fn new_binding(&mut self, identifier: &str, mutable: bool) -> Result<String, ResolutionError> {
        self.counter += 1;
        let id = format!("{}.{}", identifier, self.counter);

        match self.scopes.last_mut() {
            Some(scope) if scope.contains_key(identifier) => {
                Err(ResolutionError::DuplicateDefinition(identifier.to_string()))
            }
            Some(scope) => {
                scope.insert(identifier.to_string(), Binding::new(id.clone(), mutable));
                Ok(id)
            }
            None => unreachable!("Scope should always be set"),
        }
    }

    fn find_binding(&self, identifier: &str) -> Result<&Binding, ResolutionError> {
        for scope in self.scopes.iter().rev() {
            match scope.get(identifier) {
                Some(binding) => return Ok(binding),
                None => continue,
            }
        }
        Err(ResolutionError::UndefinedIdentifier(identifier.to_string()))
    }
}
