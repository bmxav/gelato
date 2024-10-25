mod errors;

pub use crate::errors::*;

use ast;
use lex::{Lexer, Token, TokenKind};

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

    pub fn parse(&mut self) -> Result<ast::Node, ParseError> {
        //TODO(bmxav): Add the concept of files and modules. For now we will just use a
        // `Block` as the root node. When this changes we will no longer allow expressions
        // to appear in the top-level block, only statements.
        //
        // When modules are in place, it may be worth looking into implementing
        // implicit entry points for a single module. This would allow for expressions in
        // the top-level block and have a "main()" function generated for it.
        let mut block = ast::Block::new();

        loop {
            //TODO(bmxav): Don't return errors immediately. They should be recorded and parsing
            // should continue from the next synchronization point. Any errors can then be
            // reported at the very end.
            let node = match self.next_token.kind {
                TokenKind::Import => ast::Node::Stmt(self.parse_import_stmt()?),
                TokenKind::Let => ast::Node::Stmt(self.parse_let_decl()?),
                TokenKind::Eof => break,
                _ => {
                    ast::Node::Expr(self.parse_expr(1)?)
                }
            };
            block.add_child(node);
        }

        Ok(ast::Node::Block(block))
    }

    fn parse_import_stmt(&mut self) -> Result<ast::Stmt, ParseError> {
        let _ = self.expect(TokenKind::Import)?;

        let path = self.parse_string_literal()?;

        // Check if there is an import alias present.
        let alias = if self.matches(TokenKind::As).is_some() {
            let token = self.expect(TokenKind::Identifier)?;
            Some(token.text.to_string())
        } else {
            None
        };

        Ok(ast::Stmt::Import { path, alias })
    }

    fn parse_let_decl(&mut self) -> Result<ast::Stmt, ParseError> {
        let _ = self.expect(TokenKind::Let)?;
        let identifier = self.parse_identifier()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr(1)?;

        Ok(ast::Stmt::LetDecl { identifier, expr })
    }

    fn parse_expr(&mut self, min_precedence: u8) -> Result<ast::Expr, ParseError> {
        let mut left = self.parse_factor()?;

        loop {
            let op = self.binary_op(self.next_token.kind);
            match op {
                Some(op) if self.precedence(&op) >= min_precedence => {
                    // Eat the binary operator.
                    let _ = self.consume();

                    let right = self.parse_expr(self.precedence(&op) + 1)?;
                    left = ast::Expr::BinaryExpr {
                        op: op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                _ => break
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<ast::Expr, ParseError> {
        let factor = match self.next_token.kind {
            TokenKind::Int => ast::Expr::Int(self.parse_int_constant()?),
            TokenKind::String { .. } => ast::Expr::String(self.parse_string_literal()?),
            TokenKind::Identifier => ast::Expr::Identifier(self.parse_identifier()?),
            TokenKind::If => {
                let _ = self.expect(TokenKind::If)?;

                let cond = Box::new(self.parse_expr(1)?);

                let then = self.expect(TokenKind::Then)
                    .and_then(|_| self.parse_expr(1))
                    .map(Box::new)?;

                let els = self.matches(TokenKind::Else)
                    .map(|_| self.parse_expr(1))
                    .transpose()?
                    .map(Box::new);

                let _ = self.expect(TokenKind::End)?;

                ast::Expr::If { cond, then, els }
            }
            TokenKind::LeftParen => {
                let _ = self.expect(TokenKind::LeftParen)?;
                let expr = self.parse_expr(1)?;
                let _ = self.expect(TokenKind::RightParen)?;
                expr
            }
            _ => return Err(self.unexpected_token_error(&self.next_token))
        };

        Ok(factor)
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        self.expect(TokenKind::Identifier).map(|token| token.text.to_string())
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

    fn parse_int_constant(&mut self) -> Result<i64, ParseError> {
        let token = self.expect(TokenKind::Int)?;
        let value = token.text.parse::<i64>()
            .map_err(|_| self.emit_token_error(&token, ParseErrorKind::InvalidIntLiteral(token.text.to_string())))?;
        Ok(value)
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

    fn binary_op(&self, kind: TokenKind) -> Option<ast::BinaryOp> {
        let op = match kind {
            TokenKind::Plus => ast::BinaryOp::Add,
            TokenKind::Minus => ast::BinaryOp::Sub,
            TokenKind::Star => ast::BinaryOp::Mul,
            TokenKind::Slash => ast::BinaryOp::Div,
            TokenKind::Eq => ast::BinaryOp::Assign,
            TokenKind::PlusEq => ast::BinaryOp::PlusAssign,
            TokenKind::MinusEq => ast::BinaryOp::MinusAssign,
            TokenKind::EqEq => ast::BinaryOp::Eq,
            TokenKind::BangEq => ast::BinaryOp::NotEq,
            TokenKind::LessThan => ast::BinaryOp::LessThan,
            TokenKind::LessThanEq => ast::BinaryOp::LessThanEq,
            TokenKind::GreaterThan => ast::BinaryOp::GreaterThan,
            TokenKind::GreaterThanEq => ast::BinaryOp::GreaterThanEq,
            TokenKind::Dot => ast::BinaryOp::MemberAccess,
            TokenKind::And => ast::BinaryOp::And,
            TokenKind::Or => ast::BinaryOp::Or,
            _ => return None,
        };
        Some(op)
    }

    fn precedence(&self, op: &ast::BinaryOp) -> u8 {
        match op {
            ast::BinaryOp::MemberAccess => 60,
            ast::BinaryOp::Mul | ast::BinaryOp::Div => 50,
            ast::BinaryOp::Add | ast::BinaryOp::Sub => 40,
            ast::BinaryOp::LessThan
                | ast::BinaryOp::LessThanEq
                | ast::BinaryOp::GreaterThan
                | ast::BinaryOp::GreaterThanEq => 35,
            ast::BinaryOp::Eq | ast::BinaryOp::NotEq => 30,
            ast::BinaryOp::And => 10,
            ast::BinaryOp::Or => 5,
            ast::BinaryOp::Assign | ast::BinaryOp::PlusAssign | ast::BinaryOp::MinusAssign => 1,
        }
    }
}
