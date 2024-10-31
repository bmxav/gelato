mod errors;
mod lexer;
mod token;

pub use crate::errors::{ParseError, ParseErrorKind};
pub use crate::lexer::Lexer;
pub use crate::token::{Token, TokenKind};

use ast::{BinOp, Ident, Literal, Block, Expr, Module, Stmt};

use std::path::{Path, PathBuf};

pub struct SourceFile {
    path: Option<PathBuf>,
    content: String,
}

impl SourceFile where {
    pub fn new(content: String) -> Self {
        Self {
            path: None,
            content: content,
        }
    }

    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let content = std::fs::read_to_string(&path)?;

        let source_file = Self {
            path: Some(path.as_ref().into()),
            content: content,
        };

        Ok(source_file)
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_ref().map(|p| p.as_path())
    }

    pub fn content(&self) -> &str {
        &self.content
    }
}

pub struct Parser<'a> {
    source_file: &'a SourceFile,
    lexer: Lexer<'a>,
    next_token: Token<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        let mut lexer = Lexer::new(source_file.content());
        let next_token = lexer.next_token();

        Self {
            source_file,
            lexer,
            next_token,
        }
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        let name =  self.source_file.path().and_then(|path| path.file_name()).unwrap_or_default().to_string_lossy();
        let module = Module::new(
            name.to_string(),
            self.parse_block()?,
        );
        Ok(module)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();

        loop {
            let stmt = match self.next_token.kind {
                TokenKind::Semicolon => {
                    // Ignore rogue semicolons.
                    let _ = self.consume();
                    continue
                }
                TokenKind::Else | TokenKind::End | TokenKind::Eof => break,
                _ => self.parse_stmt()?
            };

            stmts.push(stmt);
        }

        Ok(Block::with_body(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let stmt = match self.next_token.kind {
            TokenKind::Let => self.parse_let()?,
            TokenKind::Var => self.parse_var()?,
            _ => {
                // We must be looking for an expression.
                let expr = self.parse_expr(1)?;

                // Check if this is the left-hand side of an assignment.
                self.parse_assignment(expr)?
            }
        };

        self.expect(TokenKind::Semicolon)?;

        Ok(stmt)
    }

    // Checks if the given expression is actually the left-hand side of an assignment operation.
    // If so, this will return the appropriate assignment statement node, otherwise it will return
    // a statement representing the original expression that was passed in.
    fn parse_assignment(&mut self, expr: Expr) -> Result<Stmt, ParseError> {
        let kind = match self.matches(TokenKind::Eq) {
            Some(_) => {
                let right = self.parse_expr(1)?;
                Stmt::Assign(expr, right)
            },
            None => match self.matches_assign_op() {
                Some(op) => {
                    let right = self.parse_expr(1)?;
                    Stmt::OpAssign(op, expr, right)
                }
                None => Stmt::Expr(expr)
            }
        };
        Ok(kind)
    }

    fn matches_assign_op(&mut self) -> Option<BinOp> {
        let token = self.matches_if(|kind| match kind {
            TokenKind::PlusEq | TokenKind::MinusEq => true,
            _ => false,
        })?;

        let op = match token.kind {
            TokenKind::PlusEq => BinOp::Add,
            TokenKind::MinusEq => BinOp::Sub,
            _ => return None,
        };

        Some(op)
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let _ = self.expect(TokenKind::Let)?;
        let ident = self.parse_ident()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr(1)?;
        Ok(Stmt::Let((), ident, expr))
    }

    fn parse_var(&mut self) -> Result<Stmt, ParseError> {
        let _ = self.expect(TokenKind::Var)?;
        let ident = self.parse_ident()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr(1)?;
        Ok(Stmt::Var((), ident, expr))
    }


    fn parse_expr(&mut self, min_precedence: u8) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        loop {
            let op = self.bin_op(self.next_token.kind);
            match op {
                Some(op) if self.precedence(&op) >= min_precedence => {
                    let _ = self.consume();
                    let right = self.parse_expr(self.precedence(&op) + 1)?;
                    left = Expr::Binary((), op, Box::new(left), Box::new(right));
                }
                _ => break
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let factor = match self.next_token.kind {
            TokenKind::Int => {
                let literal = self.parse_int_literal()?;
                Expr::Literal((), Literal::Int(literal))
            }
            TokenKind::String { .. } => {
                let literal = self.parse_string_literal()?;
                Expr::Literal((), Literal::String(literal))
            }
            TokenKind::True | TokenKind::False => {
                let literal = self.parse_bool_literal()?;
                Expr::Literal((), Literal::Bool(literal))
            },
            TokenKind::Identifier => {
                let ident = self.parse_ident()?;
                Expr::Variable((), ident)
            }
            TokenKind::If => self.parse_if_expr()?,
            TokenKind::LeftParen => self.parse_grouping()?,
            _ => return Err(self.unexpected_token_error(&self.next_token))
        };

        Ok(factor)
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        self.expect(TokenKind::Identifier).map(|token| Ident::new(token.text.to_string()))
    }

    fn parse_string_literal(&mut self) -> Result<String, ParseError> {
        let token = self.expect_if(|kind| match kind {
            TokenKind::String { .. }  => true,
            _ => false,
        })?;

        match token.kind {
            TokenKind::String { terminated: true } => {
                let s = token.text[1..token.text.len() - 1].to_string();
                self.verify_escape_sequences(&token, &s)?;
                Ok(s)
            }
            _ => Err(self.emit_token_error(&token, ParseErrorKind::UnterminatedString(token.text.to_string())))
        }
    }

    fn verify_escape_sequences(&self, token: &Token<'a>, s: &str) -> Result<(), ParseError> {
        let mut escaped = false;

        for (i, c) in s.char_indices() {
            match c {
                '\\' => {
                    escaped = !escaped;
                    continue
                }
                'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\'' | '"' if escaped => {}
                _ if escaped => {
                    return Err(self.emit_token_error(token, ParseErrorKind::InvalidEscapeSequence(i, c)));
                }
                _ => {}
            }

            escaped = false;
        }

        Ok(())
    }

    fn parse_int_literal(&mut self) -> Result<i64, ParseError> {
        let token = self.expect(TokenKind::Int)?;
        let value = token.text.parse::<i64>()
            .map_err(|_| self.emit_token_error(&token, ParseErrorKind::InvalidIntLiteral(token.text.to_string())))?;
        Ok(value)
    }

    fn parse_bool_literal(&mut self) -> Result<bool, ParseError> {
        let token = self.expect_if(|kind| match kind {
            TokenKind::True | TokenKind::False => true,
            _ => false
        })?;

        let value = if token.kind == TokenKind::True {
            true
        } else {
            false
        };

        Ok(value)
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        let _ = self.expect(TokenKind::If)?;
        let cond = Box::new(self.parse_expr(1)?);

        let _ = self.expect(TokenKind::Then)?;
        let then = self.parse_block()?;

        let els = self.matches(TokenKind::Else)
            .map(|_| self.parse_block())
            .transpose()?;

        let _ = self.expect(TokenKind::End)?;

        Ok(Expr::If((), cond, then, els))
    }

    fn parse_grouping(&mut self) -> Result<Expr, ParseError> {
        let _ = self.expect(TokenKind::LeftParen)?;
        let expr = self.parse_expr(1)?;
        let _ = self.expect(TokenKind::RightParen)?;
        Ok(expr)
    }

    fn consume(&mut self) -> Token<'a> {
        let token = self.next_token;
        self.next_token = self.lexer.next_token();
        token
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>, ParseError> {
        // We are going to consume to the next token either way. If we aren't expecting this
        // token here we should work on finding the next synchronization point.
        let token = self.consume();

        if token.kind == kind {
            Ok(token)
        } else {
            Err(self.unexpected_token_error(&token))
        }
    }

    fn expect_if<F>(&mut self, f: F) -> Result<Token<'a>, ParseError>
    where F: FnOnce(TokenKind) -> bool {
        let token = self.consume();

        if f(token.kind) {
            Ok(token)
        } else {
            Err(self.unexpected_token_error(&token))
        }
    }

    fn matches(&mut self, kind: TokenKind) -> Option<Token<'a>> {
        if self.next_token.kind == kind {
            Some(self.consume())
        } else {
            None
        }
    }

    fn matches_if<F>(&mut self, f: F) -> Option<Token<'a>>
    where F: FnOnce(TokenKind) -> bool {
        if f(self.next_token.kind) {
            Some(self.consume())
        } else {
            None
        }
    }

    fn unexpected_token_error(&self, token: &Token<'a>) -> ParseError {
        self.emit_token_error(token, ParseErrorKind::UnexpectedToken(token.text.to_string()))
    }

    fn emit_token_error(&self, token: &Token<'a>, kind: ParseErrorKind) -> ParseError {
        //TODO(bmxav): Record this error.
        ParseError {
            line: token.line,
            col: token.col,
            kind: kind,
        }
    }

    fn bin_op(&self, kind: TokenKind) -> Option<BinOp> {
        let op = match kind {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Sub,
            TokenKind::Star => BinOp::Mul,
            TokenKind::Slash => BinOp::Div,
            TokenKind::EqEq => BinOp::Eq,
            TokenKind::BangEq => BinOp::Ne,
            TokenKind::LessThan => BinOp::Lt,
            TokenKind::LessThanEq => BinOp::Lte,
            TokenKind::GreaterThan => BinOp::Gt,
            TokenKind::GreaterThanEq => BinOp::Gte,
            TokenKind::And => BinOp::And,
            TokenKind::Or => BinOp::Or,
            _ => return None,
        };
        Some(op)
    }

    fn precedence(&self, op: &BinOp) -> u8 {
        match op {
            BinOp::Mul | BinOp::Div | BinOp::Rem => 50,
            BinOp::Add | BinOp::Sub => 40,
            BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte => 35,
            BinOp::Eq | BinOp::Ne => 30,
            BinOp::And => 10,
            BinOp::Or => 5,
        }
    }
}
