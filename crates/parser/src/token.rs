#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    Unknown,

    // Types
    Identifier,
    Int,
    String { terminated: bool },

    // Assignment
    Eq,
    PlusEq,
    MinusEq,

    // Operators
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dot,

    // Comparisons
    EqEq,
    BangEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    And,
    Or,

    // Groupings
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Punctuation
    Comma,
    Colon,
    Semicolon,

    // Keywords
    As,
    Def,
    Else,
    End,
    If,
    Import,
    Let,
    Then,
    Var,

    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
    pub line: usize,
    pub col: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a str, line: usize, col: usize) -> Self {
        Self {
            kind,
            text,
            line,
            col,
        }
    }
}
