use ast::{Block, BlockIterator, Expr, Ident, Module, Phase, Stmt};

use thiserror::Error;

use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Resolution {
    pub name: String,
}

impl Resolution {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug)]
pub struct Resolved;

impl Phase for Resolved {
    type Literal = ();
    type Variable = Resolution;
    type Binary = ();
    type If = ();
    type Block = ();
}

pub type ResolvedModule = Module<Resolved>;
pub type ResolvedStmt = Stmt<Resolved>;
pub type ResolvedExpr = Expr<Resolved>;
pub type ResolvedBlock = Block<Resolved>;
pub type ResolvedBlockIterator<'a> = BlockIterator<'a, Resolved>;

#[derive(Error, Debug)]
pub enum ResolutionError {
    #[error("identifier '{0}' has not been defined")]
    Undefined(String),
    #[error("invalid assignment")]
    InvalidAssignment,
    #[error("identifier '{0}' has already been defined within this scope")]
    DuplicateDefinition(String),
    #[error("assignment to immutable binding '{0}'")]
    ImmutableAssignment(String),
}

type Scope = HashMap<Ident, Resolution>;

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

    pub fn resolve(&mut self, module: Module) -> Result<ResolvedModule, ResolutionError> {
        let module = ResolvedModule::new(module.name, self.resolve_block(module.body)?);
        Ok(module)
    }

    fn resolve_stmt(&mut self, stmt: Stmt) -> Result<ResolvedStmt, ResolutionError> {
        let stmt = match stmt {
            Stmt::Let(_, ident, expr) => {
                let expr = self.resolve_expr(expr)?;
                let res = self.new_binding(&ident)?;
                ResolvedStmt::Let(res, ident, expr)
            }
            Stmt::Var(_, ident, expr) => {
                let expr = self.resolve_expr(expr)?;
                let res = self.new_binding(&ident)?;
                ResolvedStmt::Var(res, ident, expr)
            }
            Stmt::VarDecl(_, ident, ty) => {
                let res = self.new_binding(&ident)?;
                ResolvedStmt::VarDecl(res, ident, ty)
            }
            Stmt::Assign(left, right) => {
                let left = self.resolve_expr(left)?;
                let right = self.resolve_expr(right)?;
                ResolvedStmt::Assign(left, right)
            }
            Stmt::OpAssign(op, left, right) => {
                let left = self.resolve_expr(left)?;
                let right = self.resolve_expr(right)?;
                ResolvedStmt::OpAssign(op, left, right)
            }
            Stmt::Expr(expr) => {
                let expr = self.resolve_expr(expr)?;
                ResolvedStmt::Expr(expr)
            }
        };
        Ok(stmt)
    }

    fn resolve_expr(&mut self, expr: Expr) -> Result<ResolvedExpr, ResolutionError> {
        let expr = match expr {
            Expr::Variable(_, ident) => {
                let res = self.find_binding(&ident)?.clone();
                ResolvedExpr::Variable(res, ident)
            }
            Expr::Literal(_, literal) => ResolvedExpr::Literal((), literal),
            Expr::Binary(_, op, left, right) => {
                let left = self.resolve_expr(*left)?;
                let right = self.resolve_expr(*right)?;
                ResolvedExpr::Binary((), op, Box::new(left), Box::new(right))
            }
            Expr::If(_, cond, then, els) => {
                let cond = self.resolve_expr(*cond)?;
                let then = self.resolve_block(then)?;
                let els = els.map(|e| self.resolve_block(e)).transpose()?;
                ResolvedExpr::If((), Box::new(cond), then, els)
            }
            Expr::Block(_, block) => ResolvedExpr::Block((), self.resolve_block(block)?),
        };

        Ok(expr)
    }

    fn resolve_block(&mut self, block: Block) -> Result<ResolvedBlock, ResolutionError> {
        self.scopes.push(Scope::new());

        let stmts = block.into_iter()
            .map(|stmt| self.resolve_stmt(stmt))
            .collect::<Result<Vec<ResolvedStmt>, ResolutionError>>()?;

        self.scopes.pop();

        Ok(ResolvedBlock::with_body(stmts))
    }

    fn new_binding(&mut self, ident: &Ident) -> Result<Resolution, ResolutionError> {
        self.counter += 1;
        let name = format!("{}.{}", ident.to_string(), self.counter);
        let resolution = Resolution::new(name);

        match self.scopes.last_mut() {
            Some(scope) if scope.contains_key(ident) => {
                Err(ResolutionError::DuplicateDefinition(ident.to_string()))
            }
            Some(scope) => {
                scope.insert(ident.clone(), resolution.clone());
                Ok(resolution)
            }
            None => unreachable!("Scope should always be set"),
        }
    }

    fn find_binding(&self, ident: &Ident) -> Result<&Resolution, ResolutionError> {
        for scope in self.scopes.iter().rev() {
            match scope.get(ident) {
                Some(res) => return Ok(res),
                None => continue,
            }
        }
        Err(ResolutionError::Undefined(ident.to_string()))
    }
}
