use ast::{
    UntypedBlock, UntypedExpr, UntypedExprKind, Identifier, Literal, UntypedModule,
    UntypedStmt, UntypedStmtKind,
};
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

type Scope = HashMap<Identifier, Identifier>;

pub struct Resolver {
    scopes: Vec<Scope>,
    counter: usize,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            counter: 0,
        }
    }

    pub fn resolve(&mut self, mut module: UntypedModule) -> Result<UntypedModule, ResolutionError> {
        module.body = self.resolve_block(module.body)?;
        Ok(module)
    }

    fn resolve_stmt(&mut self, stmt: UntypedStmt) -> Result<UntypedStmt, ResolutionError> {
        let kind = match stmt.kind {
            UntypedStmtKind::Let(ident, expr) => {
                let expr = self.resolve_expr(expr)?;
                let ident = self.new_binding(&ident)?;
                UntypedStmtKind::Let(ident, expr)
            }
            UntypedStmtKind::Var(ident, expr) => {
                let expr = self.resolve_expr(expr)?;
                let ident = self.new_binding(&ident)?;
                UntypedStmtKind::Var(ident, expr)
            }
            UntypedStmtKind::VarDecl(ident, ty) => {
                let ident = self.new_binding(&ident)?;
                UntypedStmtKind::VarDecl(ident, ty)
            }
            UntypedStmtKind::Assign(left, right) => {
                let left = self.resolve_expr(left)?;
                let right = self.resolve_expr(right)?;
                UntypedStmtKind::Assign(left, right)
            }
            UntypedStmtKind::OpAssign(op, left, right) => {
                let left = self.resolve_expr(left)?;
                let right = self.resolve_expr(right)?;
                UntypedStmtKind::OpAssign(op, left, right)
            }
            UntypedStmtKind::Expr(expr) => {
                let expr = self.resolve_expr(expr)?;
                UntypedStmtKind::Expr(expr)
            }
        };

        Ok(UntypedStmt::new(kind))
    }

    fn resolve_expr(&mut self, expr: UntypedExpr) -> Result<UntypedExpr, ResolutionError> {
        let kind = match expr.kind {
            UntypedExprKind::Literal(Literal::Identifier(ident)) => {
                let ident = self.find_binding(&ident)?.clone();
                UntypedExprKind::Literal(Literal::Identifier(ident))
            }
            UntypedExprKind::Binary(op, left, right) => {
                let left = self.resolve_expr(*left)?;
                let right = self.resolve_expr(*right)?;
                UntypedExprKind::Binary(op, Box::new(left), Box::new(right))
            }
            UntypedExprKind::If(cond, then, els) => {
                let cond = self.resolve_expr(*cond)?;
                let then = self.resolve_block(then)?;
                let els = els.map(|e| self.resolve_block(e)).transpose()?;
                UntypedExprKind::If(Box::new(cond), then, els)
            }
            UntypedExprKind::Block(block) => {
                let block = self.resolve_block(block)?;
                UntypedExprKind::Block(block)
            }
            _ => expr.kind,
        };

        Ok(UntypedExpr::new(kind))
    }

    fn resolve_block(&mut self, mut block: UntypedBlock) -> Result<UntypedBlock, ResolutionError> {
        self.scopes.push(Scope::new());

        let stmts = block.stmts
            .into_iter()
            .map(|stmt| self.resolve_stmt(stmt))
            .collect::<Result<Vec<UntypedStmt>, ResolutionError>>()?;

        self.scopes.pop();

        block.stmts = stmts;

        Ok(block)
    }

    fn new_binding(&mut self, ident: &Identifier) -> Result<Identifier, ResolutionError> {
        self.counter += 1;
        let resolved_ident = Identifier::new(format!("{}.{}", ident.to_string(), self.counter));

        match self.scopes.last_mut() {
            Some(scope) if scope.contains_key(ident) => {
                Err(ResolutionError::DuplicateDefinition(ident.to_string()))
            }
            Some(scope) => {
                scope.insert(ident.clone(), resolved_ident.clone());
                Ok(resolved_ident)
            }
            None => unreachable!("Scope should always be set"),
        }
    }

    fn find_binding(&self, ident: &Identifier) -> Result<&Identifier, ResolutionError> {
        for scope in self.scopes.iter().rev() {
            match scope.get(ident) {
                Some(binding) => return Ok(binding),
                None => continue,
            }
        }
        Err(ResolutionError::UndefinedIdentifier(ident.to_string()))
    }
}
