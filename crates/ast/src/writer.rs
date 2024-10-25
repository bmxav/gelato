use super::*;

pub struct Writer<W: std::io::Write> {
    w: W,
    indent: usize,
    level: usize,
}

impl<W> Writer<W> where W: std::io::Write {
    pub fn new(w: W, indent: usize) -> Self {
        Self {
            w,
            indent,
            level: 0,
        }
    }

    pub fn write(&mut self, node: &Node) -> Result<(), std::io::Error> {
        let result = match node {
            Node::Block(block) => self.write_block(block),
            Node::Expr(expr) => self.write_expr(expr),
            Node::Stmt(stmt) => self.write_stmt(stmt),
        };
        result
    }

    fn write_block(&mut self, block: &Block) -> Result<(), std::io::Error> {
        self.level += 1;
        for node in block.iter() {
            self.write(node)?;
        }
        self.level -= 1;
        Ok(())
    }

    fn write_expr(&mut self, expr: &Expr) -> Result<(), std::io::Error> {
        self.level += 1;

        let result = match expr {
            Expr::Identifier(_) | Expr::Int(_) | Expr::String(_) => {
                self.write_text(&format!("{expr:?}"))
            }
            Expr::BinaryExpr { op, left, right } => {
                self.write_text("BinaryOp")?;
                self.write_text(&format!("{op:?}"))?;
                self.write_expr(left)?;
                self.write_expr(right)
            }
            Expr::If { cond, then, els } => {
                self.write_text("If")?;
                self.write_expr(cond)?;
                let result = self.write_expr(then);
                match els {
                    Some(branch) => self.write_expr(branch),
                    None => result,
                }
            }
        };

        self.level -= 1;

        result
    }

    fn write_stmt(&mut self, stmt: &Stmt) -> Result<(), std::io::Error> {
        match stmt {
            Stmt::LetDecl { identifier, expr } => {
                self.write_text("LetDecl")?;
                self.write_text(&format!("{identifier}"))?;
                self.write_expr(expr)
            }
            Stmt::Import { path, alias } => {
                self.write_text("Import")?;
                let result = self.write_text(path);
                match alias {
                    Some(name) => self.write_text(name),
                    None => result,
                }
            }
        }
    }

    fn write_text(&mut self, message: &str) -> Result<(), std::io::Error> {
        let indent = self.indent * self.level;
        writeln!(&mut self.w, "{:indent$}{message}", "")
    }
}
