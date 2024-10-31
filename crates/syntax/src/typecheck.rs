use ast::{BinOp, Block, BlockIterator, Expr, Ident, Literal, Module, Phase, Stmt, Type};
use crate::resolve::{Resolution, ResolvedBlock, ResolvedExpr, ResolvedModule, ResolvedStmt};

use thiserror::Error;

use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeInfo {
    pub kind: Type,
}

impl TypeInfo {
    pub fn new(kind: Type) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
pub struct VariableTypeInfo {
    pub kind: Type,
    pub resolution: Resolution,
}

impl VariableTypeInfo {
    pub fn new(kind: Type, resolution: Resolution) -> Self {
        Self { kind, resolution }
    }

    pub fn extract<T: TypeExtractor>(typ: &T, resolution: Resolution) -> Self {
        Self {
            kind: typ.extract_type(),
            resolution,
        }
    }
}

#[derive(Debug)]
pub struct Typed;

impl Phase for Typed {
    type Literal = TypeInfo;
    type Variable = VariableTypeInfo;
    type Binary = TypeInfo;
    type If = TypeInfo;
    type Block = TypeInfo;
}

pub type TypedModule = Module<Typed>;

pub type TypedStmt = Stmt<Typed>;

pub type TypedExpr = Expr<Typed>;

pub trait TypeExtractor {
    fn extract_type(&self) -> Type;
}

impl TypeExtractor for TypedExpr {
    fn extract_type(&self) -> Type {
        match self {
            Expr::Variable(typ, _) => typ.kind.clone(),
            Expr::Literal(typ, _) | Expr::Binary(typ, _, _, _) | Expr::If(typ, _, _, _) | Expr::Block(typ, _) => typ.kind.clone()
        }
    }
}

pub type TypedBlock = Block<Typed>;

impl TypeExtractor for TypedBlock {
    fn extract_type(&self) -> Type {
        let typ = self.last_child().map(|stmt| match stmt {
            Stmt::Expr(expr) => expr.extract_type(),
            _ => Type::Unit,
        });
        typ.unwrap_or(Type::Unit)
    }
}


pub type TypedBlockIterator<'a> = BlockIterator<'a, Typed>;

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
    identifier_types: HashMap<Ident, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            identifier_types: HashMap::new(),
        }
    }

    pub fn type_check(&mut self, module: ResolvedModule) -> Result<TypedModule, TypeError> {
        let body = self.type_check_block(module.body)?;
        Ok(TypedModule::new(module.name, body))
    }

    fn type_check_block(&mut self, block: ResolvedBlock) -> Result<TypedBlock, TypeError>  {
        let stmts = block.into_iter()
            .map(|stmt| self.type_check_stmt(stmt))
            .collect::<Result<Vec<TypedStmt>, TypeError>>()?;
        Ok(TypedBlock::with_body(stmts))
    }

    fn type_check_stmt(&mut self, stmt: ResolvedStmt) -> Result<TypedStmt, TypeError> {
        let typed_stmt = match stmt {
            ResolvedStmt::Let(res, ident, expr) => {
                let expr = self.type_check_expr(expr)?;
                let type_info = VariableTypeInfo::extract(&expr, res);
                self.identifier_types.insert(ident.clone(), type_info.kind.clone());
                TypedStmt::Let(type_info, ident, expr)
            }
            ResolvedStmt::Var(res, ident, expr) => {
                let expr = self.type_check_expr(expr)?;
                let type_info = VariableTypeInfo::extract(&expr, res);
                self.identifier_types.insert(ident.clone(), type_info.kind.clone());
                TypedStmt::Var(type_info, ident, expr)
            }
            ResolvedStmt::Assign(left, right) => {
                let left = self.type_check_expr(left)?;
                let right = self.type_check_expr(right)?;

                let left_typ = left.extract_type();
                let right_typ = right.extract_type();

                //TODO: Check that left-hand side allows assignment.
                if left_typ != right_typ {
                    return Err(TypeError::TypeMismatch(left_typ, right_typ));
                }

                TypedStmt::Assign(left, right)
            }
            ResolvedStmt::OpAssign(op, left, right) => {
                let left = self.type_check_expr(left)?;
                let right = self.type_check_expr(right)?;

                let left_typ = left.extract_type();
                let right_typ = right.extract_type();

                //TODO: Check that left-hand side allows assignment and that both sides
                // are valid operands.
                if left_typ != right_typ {
                    return Err(TypeError::TypeMismatch(left_typ, right_typ));
                }

                TypedStmt::OpAssign(op, left, right)
            }
            ResolvedStmt::Expr(expr) => {
                let expr = self.type_check_expr(expr)?;
                TypedStmt::Expr(expr)
            }
        };

        Ok(typed_stmt)
    }

    fn type_check_expr(&mut self, expr: ResolvedExpr) -> Result<TypedExpr, TypeError> {
        let typed_expr = match expr {
            ResolvedExpr::Literal(_, s @ Literal::String(_)) => {
                TypedExpr::Literal(TypeInfo::new(Type::String), s)
            }
            ResolvedExpr::Literal(_, i @ Literal::Int(_)) => {
                TypedExpr::Literal(TypeInfo::new(Type::Int), i)
            }
            ResolvedExpr::Literal(_, b @ Literal::Bool(_)) => {
                TypedExpr::Literal(TypeInfo::new(Type::Bool), b)
            },
            ResolvedExpr::Variable(res, ident) => {
                let typ = self.identifier_types.get(&ident)
                    .ok_or(TypeError::UnknownType(ident.to_string()))?
                    .clone();
                TypedExpr::Variable(VariableTypeInfo::new(typ, res), ident)
            }
            ResolvedExpr::If(_, cond, then, els) => {
                let cond = self.type_check_expr(*cond)?;
                let cond_typ = cond.extract_type();
                if cond_typ != Type::Bool {
                    return Err(TypeError::UnexpectedType { expected: Type::Bool, found: cond_typ })
                }

                let then = self.type_check_block(then)?;
                let then_typ = then.extract_type();
                let els = els.map(|b| self.type_check_block(b)).transpose()?;
                let els_typ = els.as_ref().map(|block| block.extract_type());

                match els_typ {
                    Some(typ) if typ != then_typ => return Err(TypeError::TypeMismatch(then_typ, typ)),
                    _ => {}
                }

                let type_info = TypeInfo::new(then_typ);
                TypedExpr::If(type_info, Box::new(cond), then, els)
            },
            ResolvedExpr::Block(_, block) => {
                let block = self.type_check_block(block)?;
                TypedExpr::Block(TypeInfo::new(block.extract_type()), block)
            },
            ResolvedExpr::Binary(_, op, left, right) => {
                let left = self.type_check_expr(*left)?;
                let right = self.type_check_expr(*right)?;

                let left_typ = left.extract_type();
                let right_typ = right.extract_type();

                if left_typ != right_typ {
                    return Err(TypeError::TypeMismatch(left_typ, right_typ));
                }

                //TODO: Ensure that the binop is supported by both operands.
                let typ = match op {
                    BinOp::And
                        | BinOp::Eq
                        | BinOp::Gt
                        | BinOp::Gte
                        | BinOp::Lt
                        | BinOp::Lte
                        | BinOp::Or
                        | BinOp::Ne => Type::Bool,
                    _ => left_typ.clone(),
                };

                let type_info = TypeInfo::new(typ);
                TypedExpr::Binary(type_info, op, Box::new(left), Box::new(right))
            },
        };

        Ok(typed_expr)
    }
}
