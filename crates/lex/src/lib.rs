use std::iter::Peekable;
use std::str::CharIndices;

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

#[derive(Clone, Copy, Debug)]
struct Cursor {
    pos: usize,
    line: usize,
    col: usize,
}

impl Cursor {
    fn new(pos: usize, line: usize, col: usize) -> Self {
        Self {
            pos,
            line,
            col,
        }
    }

    fn start() -> Self {
        Self::new(0, 1, 1)
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    cursor: Cursor,
    token: Cursor,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input,
            chars: input.char_indices().peekable(),
            cursor: Cursor::start(),
            token: Cursor::start(),
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        while let Some(c) = self.peek() {
            self.token = self.cursor;

            self.consume();

            let kind = match c {
                _ if c.is_whitespace() => continue,
                '#' => {
                    //TODO: Add comments that can be captured for doc and other purposes.
                    self.consume_while(|c| c != '\n');
                    continue;
                },
                '0'..='9' => self.read_number(),
                '=' => {
                    if self.matches('=') {
                        TokenKind::EqEq
                    } else {
                        TokenKind::Eq
                    }
                }
                '!' => {
                    if self.matches('=') {
                        TokenKind::BangEq
                    } else {
                        TokenKind::Bang
                    }
                }
                '+' => {
                    if self.matches('=') {
                        TokenKind::PlusEq
                    } else {
                        TokenKind::Plus
                    }
                }
                '-' => {
                    if self.matches('=') {
                        TokenKind::MinusEq
                    } else {
                        TokenKind::Minus
                    }
                }
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                '%' => TokenKind::Percent,
                '<' => {
                    if self.matches('=') {
                        TokenKind::LessThanEq
                    } else {
                        TokenKind::LessThan
                    }
                }
                '>' => {
                    if self.matches('=') {
                        TokenKind::GreaterThanEq
                    } else {
                        TokenKind::GreaterThan
                    }
                }
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '{' => TokenKind::LeftBrace,
                '}' => TokenKind::RightBrace,
                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,
                '.' => TokenKind::Dot,
                ',' => TokenKind::Comma,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,
                '"' => self.read_string(),
                c if c.is_alphabetic() || c == '_' => self.read_identifier(),
                _ => TokenKind::Unknown,
            };

            return self.create_token(kind);
        }

        self.token = self.cursor;
        self.create_token(TokenKind::Eof)
    }

    fn create_token(&self, kind: TokenKind) -> Token<'a> {
        Token::new(kind, self.current_lexeme(), self.token.line, self.token.col)
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn consume(&mut self) {
        if let Some((i, c)) = self.chars.next() {
            if c == '\n' {
                self.cursor.line += 1;
                self.cursor.col = 1;
            } else {
                self.cursor.col += 1;
            }
            self.cursor.pos += i - self.cursor.pos + 1;
        }
    }

    fn consume_while<F>(&mut self, func: F) where F: Fn(char) -> bool {
        while let Some((_, c)) = self.chars.peek() {
            if func(*c) {
                self.consume()
            } else {
                break
            }
        }
    }

    fn matches(&mut self, c: char) -> bool {
        match self.peek() {
            Some(p) if p == c => {
                self.consume();
                true
            }
            _ => false,
        }
    }

    fn current_lexeme(&self) -> &'a str {
        &self.input[self.token.pos..self.cursor.pos]
    }

    fn read_number(&mut self) -> TokenKind {
        self.consume_while(|c| c.is_ascii_digit() || c == '_');
        TokenKind::Int
    }

    fn read_string(&mut self) -> TokenKind {
        let mut escaped = false;
        let mut terminated = false;

        while let Some(c) = self.peek() {
            match c {
                '\\' => escaped = !escaped,
                '"' => {
                    terminated = true;
                    self.consume();
                    break;
                }
                '\n' => break,
                _ => {}
            }

            self.consume();
        }

        TokenKind::String { terminated }
    }

    fn read_identifier(&mut self) -> TokenKind {
        self.consume_while(|c| c.is_alphanumeric() || c == '_');
        match self.check_keyword() {
            Some(kind) => kind,
            None => TokenKind::Identifier,
        }
    }

    fn check_keyword(&self) -> Option<TokenKind> {
        let kind = match self.current_lexeme() {
            "and" => TokenKind::And,
            "as" => TokenKind::As,
            "def" => TokenKind::Def,
            "else" => TokenKind::Else,
            "end" => TokenKind::End,
            "if" => TokenKind::If,
            "import" => TokenKind::Import,
            "let" => TokenKind::Let,
            "then" => TokenKind::Then,
            "or" => TokenKind::Or,
            _ => return None,
        };
        Some(kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_identifiers() {
        let inputs = [
            "myIdentifier",
            "_myIdentifier",
            "my_identifier",
            "myIdentifier123",
            "myIdentifier_123",
            "myIdèntifièr",
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input);

            assert_eq!(lexer.next_token(), Token::new(TokenKind::Identifier, input, 1, 1));
            assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.chars().count() + 1));
        }
    }

    #[test]
    fn unknown_token() {
        let input = "`";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::Unknown, input, 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.len() + 1));
    }

    #[test]
    fn string_literal() {
        let input = "\"this is a string\"";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::String { terminated: true }, input, 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, 19));
    }

    #[test]
    fn unterminated_string_literal() {
        let input = "\"this is unterminated";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::String { terminated: false }, input, 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.len() + 1));
    }

    #[test]
    fn string_with_escaped_newline() {
        let input = "\"string\\nwith\\nnewlines\"";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::String { terminated: true }, input, 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.len() + 1));
    }

    #[test]
    fn int_literal() {
        let input = "123";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::Int, "123", 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.len() + 1));
    }

    #[test]
    fn int_literal_with_underscores() {
        let input = "123_456";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::Int, "123_456", 1, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.len() + 1));
    }

    #[test]
    fn ignore_comments() {
        let input = "# comment about a number.\n123\n# comment after the number";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::new(TokenKind::Int, "123", 2, 1));
        assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 3, 27));
    }


    #[test]
    fn simple_tokens() {
        let inputs = [
            ("+", TokenKind::Plus),
            ("-", TokenKind::Minus),
            ("*", TokenKind::Star),
            ("/", TokenKind::Slash),
            ("%", TokenKind::Percent),
            ("<", TokenKind::LessThan),
            ("<=", TokenKind::LessThanEq),
            (">", TokenKind::GreaterThan),
            (">=", TokenKind::GreaterThanEq),
            ("=", TokenKind::Eq),
            ("==", TokenKind::EqEq),
            ("!=", TokenKind::BangEq),
            ("!", TokenKind::Bang),
            (".", TokenKind::Dot),
            (":", TokenKind::Colon),
            (";", TokenKind::Semicolon),
            (",", TokenKind::Comma),
            ("(", TokenKind::LeftParen),
            (")", TokenKind::RightParen),
            ("{", TokenKind::LeftBrace),
            ("}", TokenKind::RightBrace),
            ("[", TokenKind::LeftBracket),
            ("]", TokenKind::RightBracket),
        ];

        for input in inputs {
            let mut lexer = Lexer::new(input.0);
            assert_eq!(lexer.next_token(), Token::new(input.1, input.0, 1, 1));
            assert_eq!(lexer.next_token(), Token::new(TokenKind::Eof, "", 1, input.0.len() + 1));
        }
    }
}
