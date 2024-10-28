mod errors;
mod lexer;
mod token;

pub use crate::errors::{ParseError, ParseErrorKind};
pub use crate::lexer::Lexer;
pub use crate::token::{Token, TokenKind};

use ast::{AstNode, BinaryOp, Expr, ExprNode, Identifier, StmtNode};

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

    pub fn parse(&mut self) -> Result<AstNode, ParseError> {
        //TODO(bmxav): Add the concept of files and modules. For now we will just use a
        // `Block` as the root node. When this changes we will no longer allow expressions
        // to appear in the top-level block, only statements.
        //
        // When modules are in place, it may be worth looking into implementing
        // implicit entry points for a single module. This would allow for expressions in
        // the top-level block and have a "main()" function generated for it.
        self.parse_block().map(|block| AstNode::Expr(block))
    }

    fn parse_block(&mut self) -> Result<ExprNode, ParseError> {
        let mut block = Vec::new();

        loop {
            //TODO(bmxav): Don't return errors immediately. They should be recorded and parsing
            // should continue from the next synchronization point. Any errors can then be
            // reported at the very end.
            let node = match self.next_token.kind {
                TokenKind::Let => AstNode::Stmt(self.parse_let_decl()?),
                TokenKind::Var => AstNode::Stmt(self.parse_var_decl()?),
                TokenKind::Else | TokenKind::End | TokenKind::Eof => break,
                _ => AstNode::Expr(self.parse_expr(1)?),
            };
            block.push(node);
        }

        Ok(ExprNode::new(Expr::Block(block)))
    }

    fn parse_let_decl(&mut self) -> Result<StmtNode, ParseError> {
        let _ = self.expect(TokenKind::Let)?;
        let identifier = self.parse_identifier()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr(1)?;

        Ok(StmtNode::Let { identifier, expr })
    }

    fn parse_var_decl(&mut self) -> Result<StmtNode, ParseError> {
        let _ = self.expect(TokenKind::Var)?;
        let identifier = self.parse_identifier()?;
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expr(1)?;

        Ok(StmtNode::Var{ identifier, expr })
    }

    fn parse_expr(&mut self, min_precedence: u8) -> Result<ExprNode, ParseError> {
        let mut left = self.parse_factor()?;

        loop {
            let op = self.binary_op(self.next_token.kind);
            match op {
                Some(op) if self.precedence(&op) >= min_precedence => {
                    // Eat the binary operator.
                    let _ = self.consume();

                    let right = self.parse_expr(self.precedence(&op) + 1)?;
                    left = ExprNode::new(Expr::BinaryExpr {
                        op: op,
                        left: Box::new(left),
                        right: Box::new(right),
                    });
                }
                _ => break
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<ExprNode, ParseError> {
        let factor = match self.next_token.kind {
            TokenKind::Int => ExprNode::new(Expr::Int(self.parse_int_constant()?)),
            TokenKind::String { .. } => ExprNode::new(Expr::String(self.parse_string_literal()?)),
            TokenKind::Identifier => ExprNode::new(Expr::Identifier(self.parse_identifier()?)),
            TokenKind::True => ExprNode::new(Expr::Bool(self.parse_true_literal()?)),
            TokenKind::False => ExprNode::new(Expr::Bool(self.parse_false_literal()?)),
            TokenKind::If => {
                let _ = self.expect(TokenKind::If)?;

                let cond = Box::new(self.parse_expr(1)?);

                let _ = self.expect(TokenKind::Then);
                let then = self.parse_block()?;

                let els = self.matches(TokenKind::Else)
                    .map(|_| self.parse_block())
                    .transpose()?;

                let _ = self.expect(TokenKind::End)?;

                ExprNode::new(Expr::If { cond, then: Box::new(then), els: els.map(Box::new) })
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

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        self.expect(TokenKind::Identifier).map(|token| Identifier::new(token.text.to_string()))
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

    fn parse_true_literal(&mut self) -> Result<bool, ParseError> {
        let _ = self.expect(TokenKind::True)?;
        Ok(true)
    }

    fn parse_false_literal(&mut self) -> Result<bool, ParseError> {
        let _ = self.expect(TokenKind::False)?;
        Ok(false)
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

    fn binary_op(&self, kind: TokenKind) -> Option<BinaryOp> {
        let op = match kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Star => BinaryOp::Mul,
            TokenKind::Slash => BinaryOp::Div,
            TokenKind::Eq => BinaryOp::Assign,
            TokenKind::PlusEq => BinaryOp::AddAssign,
            TokenKind::MinusEq => BinaryOp::SubAssign,
            TokenKind::EqEq => BinaryOp::Eq,
            TokenKind::BangEq => BinaryOp::NotEq,
            TokenKind::LessThan => BinaryOp::LessThan,
            TokenKind::LessThanEq => BinaryOp::LessThanEq,
            TokenKind::GreaterThan => BinaryOp::GreaterThan,
            TokenKind::GreaterThanEq => BinaryOp::GreaterThanEq,
            TokenKind::Dot => BinaryOp::MemberAccess,
            TokenKind::And => BinaryOp::And,
            TokenKind::Or => BinaryOp::Or,
            _ => return None,
        };
        Some(op)
    }

    fn precedence(&self, op: &BinaryOp) -> u8 {
        match op {
            BinaryOp::MemberAccess => 60,
            BinaryOp::Mul | BinaryOp::Div => 50,
            BinaryOp::Add | BinaryOp::Sub => 40,
            BinaryOp::LessThan
                | BinaryOp::LessThanEq
                | BinaryOp::GreaterThan
                | BinaryOp::GreaterThanEq => 35,
            BinaryOp::Eq | BinaryOp::NotEq => 30,
            BinaryOp::And => 10,
            BinaryOp::Or => 5,
            BinaryOp::Assign | BinaryOp::AddAssign | BinaryOp::SubAssign => 1,
        }
    }
}
