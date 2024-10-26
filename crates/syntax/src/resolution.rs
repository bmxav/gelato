use ast::{BinaryOp, Expr, Node, Stmt};
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
}

pub struct Resolver {
    scopes: Vec<HashMap<String, String>>,
    counter: usize,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            counter: 0,
        }
    }

    pub fn resolve(&mut self, node: Node) -> Result<Node, ResolutionError> {

        let resolved_node = match node {
            Node::Stmt(Stmt::Import { .. }) => node,
            Node::Stmt(Stmt::LetDecl { identifier, expr }) => {
                let id = self.new_id(&identifier)?;
                Node::Stmt(Stmt::LetDecl {
                    identifier: id,
                    expr: self.resolve_expr(expr)?,
                })
            }
            Node::Block(block) => Node::Block(self.resolve_block(block)?),
            Node::Expr(expr) => Node::Expr(self.resolve_expr(expr)?),
        };

        Ok(resolved_node)
    }

    fn resolve_block(&mut self, block: Vec<Node>) -> Result<Vec<Node>, ResolutionError> {
        self.scopes.push(HashMap::new());

        let nodes: Result<Vec<Node>, ResolutionError> = block
            .into_iter()
            .map(|n| self.resolve(n))
            .collect();

        self.scopes.pop();

        Ok(nodes?)
    }

    fn resolve_expr(&mut self, expr: Expr) -> Result<Expr, ResolutionError> {
        let resolved = match expr {
            Expr::Identifier(name) => Expr::Identifier(self.resolved_id(&name)?),
            Expr::BinaryExpr { op: BinaryOp::Assign, left, right } => {
                let left = match *left {
                    Expr::Identifier(_) => self.resolve_expr(*left)?,
                    _ => return Err(ResolutionError::InvalidAssignment)
                };

                let right = self.resolve_expr(*right)?;
                Expr::BinaryExpr { op: BinaryOp::Assign, left: Box::new(left), right: Box::new(right) }

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
                    then: self.resolve_block(then)?,
                    els: els.map(|e| self.resolve_block(e)).transpose()?,
                }
            }
            _ => expr,
        };
        Ok(resolved)
    }

    fn new_id(&mut self, identifier: &str) -> Result<String, ResolutionError> {
        self.counter += 1;
        let id = format!("{}.{}", identifier, self.counter);

        match self.scopes.last_mut() {
            Some(scope) if scope.contains_key(identifier) => {
                Err(ResolutionError::DuplicateDefinition(identifier.to_string()))
            }
            Some(scope) => {
                scope.insert(identifier.to_string(), id.clone());
                Ok(id)
            }
            None => unreachable!("Scope should always be set"),
        }
    }

    fn resolved_id(&self, identifier: &str) -> Result<String, ResolutionError> {
        for scope in self.scopes.iter().rev() {
            match scope.get(identifier).map(|s| s.clone()) {
                Some(id) => return Ok(id),
                None => continue,
            }
        }

        Err(ResolutionError::UndefinedIdentifier(identifier.to_string()))
    }
}
