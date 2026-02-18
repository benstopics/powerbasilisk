use crate::error::{PbError, PbResult};
use crate::token::{Located, Token};

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    file: Option<String>,
}

impl Lexer {
    pub fn new(source: &str, file: Option<&str>) -> Self {
        Lexer {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            file: file.map(|s| s.to_string()),
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied();
        if let Some(c) = ch {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn file_ref(&self) -> Option<&str> {
        self.file.as_deref()
    }

    fn make_located(&self, token: Token, start_line: usize, start_col: usize) -> Located {
        Located {
            token,
            line: start_line,
            col: start_col,
            file: self.file.clone(),
        }
    }

    /// Tokenize all logical lines. Handles line continuations (`_` at end of line).
    pub fn tokenize(&mut self) -> PbResult<Vec<Located>> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();

            let Some(ch) = self.peek() else {
                tokens.push(self.make_located(Token::Eof, self.line, self.col));
                break;
            };

            let start_line = self.line;
            let start_col = self.col;

            match ch {
                '\n' => {
                    self.advance();
                    // Only emit EOL if last token isn't already an EOL or we have tokens
                    if let Some(last) = tokens.last() {
                        if last.token != Token::Eol && last.token != Token::Eof {
                            tokens.push(self.make_located(Token::Eol, start_line, start_col));
                        }
                    }
                }

                // Comment
                '\'' => {
                    self.skip_to_eol();
                }

                // String literal
                '"' => {
                    let tok = self.read_string_literal(start_line, start_col)?;
                    tokens.push(tok);
                }

                // Hash: could be #INCLUDE, #COMPILE, etc., or file number prefix
                '#' => {
                    self.advance();
                    // Check if followed by an alpha character (preprocessor directive)
                    if let Some(next) = self.peek() {
                        if next.is_ascii_alphabetic() {
                            let saved_pos = self.pos;
                            let saved_line = self.line;
                            let saved_col = self.col;
                            let word = self.read_word();
                            let tok = match word.to_uppercase().as_str() {
                                "INCLUDE" => Some(Token::HashInclude),
                                "COMPILE" => Some(Token::HashCompile),
                                "DIM" => Some(Token::HashDim),
                                "IF" => Some(Token::HashIf),
                                "ELSEIF" => Some(Token::HashElseIf),
                                "ELSE" => Some(Token::HashElse),
                                "ENDIF" => Some(Token::HashEndIf),
                                "REGISTER" => Some(Token::HashRegister),
                                "DEBUG" => Some(Token::HashDebug),
                                _ => None,
                            };
                            if let Some(tok) = tok {
                                tokens.push(self.make_located(tok, start_line, start_col));
                            } else {
                                // Not a known directive — rewind and emit just Hash
                                self.pos = saved_pos;
                                self.line = saved_line;
                                self.col = saved_col;
                                tokens.push(self.make_located(Token::Hash, start_line, start_col));
                            }
                        } else {
                            tokens.push(self.make_located(Token::Hash, start_line, start_col));
                        }
                    } else {
                        tokens.push(self.make_located(Token::Hash, start_line, start_col));
                    }
                }

                // Percent constant
                '%' => {
                    self.advance();
                    let name = self.read_word();
                    if name.is_empty() {
                        return Err(PbError::lexer(
                            "Expected constant name after %",
                            self.file_ref(),
                            start_line,
                            start_col,
                        ));
                    }
                    tokens.push(self.make_located(
                        Token::PercentConstant(format!("%{}", name.to_uppercase())),
                        start_line,
                        start_col,
                    ));
                }

                // Ampersand: could be &H hex, &B binary, &O octal literal, or string concat
                '&' => {
                    self.advance();
                    if let Some(next) = self.peek() {
                        if next == 'H' || next == 'h' {
                            // Hex literal: &H1F
                            self.advance(); // consume 'H'
                            let tok = self.read_hex_literal(start_line, start_col)?;
                            tokens.push(tok);
                        } else if next == 'B' || next == 'b' {
                            // Binary literal: &B010101
                            self.advance(); // consume 'B'
                            let tok = self.read_binary_literal(start_line, start_col)?;
                            tokens.push(tok);
                        } else if next == 'O' || next == 'o' {
                            // Octal literal: &O77
                            self.advance(); // consume 'O'
                            let tok = self.read_octal_literal(start_line, start_col)?;
                            tokens.push(tok);
                        } else {
                            tokens
                                .push(self.make_located(Token::Ampersand, start_line, start_col));
                        }
                    } else {
                        tokens.push(self.make_located(Token::Ampersand, start_line, start_col));
                    }
                }

                // Operators and delimiters
                '+' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Plus, start_line, start_col));
                }
                '-' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Minus, start_line, start_col));
                }
                '*' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Star, start_line, start_col));
                }
                '/' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Slash, start_line, start_col));
                }
                '\\' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Backslash, start_line, start_col));
                }
                '^' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Caret, start_line, start_col));
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('<') {
                        self.advance();
                        tokens.push(self.make_located(Token::Lte, start_line, start_col));
                    } else if self.peek() == Some('>') {
                        self.advance();
                        tokens.push(self.make_located(Token::Gte, start_line, start_col));
                    } else {
                        tokens.push(self.make_located(Token::Eq, start_line, start_col));
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        tokens.push(self.make_located(Token::Neq, start_line, start_col));
                    } else if self.peek() == Some('=') {
                        self.advance();
                        tokens.push(self.make_located(Token::Lte, start_line, start_col));
                    } else {
                        tokens.push(self.make_located(Token::Lt, start_line, start_col));
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        tokens.push(self.make_located(Token::Gte, start_line, start_col));
                    } else {
                        tokens.push(self.make_located(Token::Gt, start_line, start_col));
                    }
                }
                '(' => {
                    self.advance();
                    tokens.push(self.make_located(Token::LParen, start_line, start_col));
                }
                ')' => {
                    self.advance();
                    tokens.push(self.make_located(Token::RParen, start_line, start_col));
                }
                ',' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Comma, start_line, start_col));
                }
                ';' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Semicolon, start_line, start_col));
                }
                ':' => {
                    self.advance();
                    tokens.push(self.make_located(Token::Colon, start_line, start_col));
                }
                '.' => {
                    self.advance();
                    if self.peek() == Some('.') {
                        self.advance();
                        tokens.push(self.make_located(Token::DotDot, start_line, start_col));
                    } else if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                        // .3 → FloatLiteral(0.3) — leading dot float
                        let mut num_str = String::from("0.");
                        while let Some(ch) = self.peek() {
                            if ch.is_ascii_digit() {
                                num_str.push(ch);
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        self.consume_numeric_suffix();
                        let val: f64 = num_str.parse().unwrap_or(0.0);
                        tokens.push(self.make_located(Token::FloatLiteral(val), start_line, start_col));
                    } else {
                        tokens.push(self.make_located(Token::Dot, start_line, start_col));
                    }
                }

                // Numeric literal
                c if c.is_ascii_digit() => {
                    let tok = self.read_number(start_line, start_col)?;
                    tokens.push(tok);
                }

                // Identifier or keyword (starts with alpha or underscore)
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let tok = self.read_identifier_or_keyword(start_line, start_col)?;
                    // Handle line continuation: _\n at end of line
                    if tok.token == Token::Underscore {
                        // This was a lone underscore; check if it's line continuation
                        self.skip_whitespace();
                        if self.peek() == Some('\n') {
                            self.advance(); // consume the newline, continuation
                            continue;
                        }
                        // Otherwise just treat as underscore token
                        tokens.push(tok);
                    } else {
                        tokens.push(tok);
                    }
                }

                // Dollar sign alone (shouldn't normally appear alone, but handle)
                '$' => {
                    self.advance();
                    // Check if it's a string equate like $CRLF
                    let word = self.read_word();
                    if !word.is_empty() {
                        tokens.push(self.make_located(
                            Token::Identifier(format!("${}", word.to_uppercase())),
                            start_line,
                            start_col,
                        ));
                    } else {
                        // bare $ -- probably error, skip
                        continue;
                    }
                }

                // Skip unknown characters
                _ => {
                    self.advance();
                }
            }
        }

        Ok(tokens)
    }

    fn skip_to_eol(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn read_word(&mut self) -> String {
        let mut word = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        word
    }

    fn read_string_literal(
        &mut self,
        start_line: usize,
        start_col: usize,
    ) -> PbResult<Located> {
        self.advance(); // consume opening "
        let mut s = String::new();
        loop {
            match self.peek() {
                Some('"') => {
                    self.advance();
                    // Check for escaped quote ""
                    if self.peek() == Some('"') {
                        s.push('"');
                        self.advance();
                    } else {
                        break;
                    }
                }
                Some('\n') | None => {
                    // Unterminated string — allow it, PB is lenient
                    break;
                }
                Some(ch) => {
                    s.push(ch);
                    self.advance();
                }
            }
        }
        Ok(self.make_located(Token::StringLiteral(s), start_line, start_col))
    }

    fn read_hex_literal(&mut self, start_line: usize, start_col: usize) -> PbResult<Located> {
        let mut hex = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_hexdigit() {
                hex.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        // Consume optional type suffixes: &, &&, %%
        self.consume_numeric_suffix();

        if hex.is_empty() {
            return Err(PbError::lexer(
                "Expected hex digits after &H",
                self.file_ref(),
                start_line,
                start_col,
            ));
        }
        let value = i64::from_str_radix(&hex, 16).unwrap_or(0);
        Ok(self.make_located(Token::IntegerLiteral(value), start_line, start_col))
    }

    fn read_binary_literal(&mut self, start_line: usize, start_col: usize) -> PbResult<Located> {
        let mut bits = String::new();
        while let Some(ch) = self.peek() {
            if ch == '0' || ch == '1' {
                bits.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        self.consume_numeric_suffix();
        if bits.is_empty() {
            // Treat &B with no digits as 0
            return Ok(self.make_located(Token::IntegerLiteral(0), start_line, start_col));
        }
        let value = i64::from_str_radix(&bits, 2).unwrap_or(0);
        Ok(self.make_located(Token::IntegerLiteral(value), start_line, start_col))
    }

    fn read_octal_literal(&mut self, start_line: usize, start_col: usize) -> PbResult<Located> {
        let mut oct = String::new();
        while let Some(ch) = self.peek() {
            if ch >= '0' && ch <= '7' {
                oct.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        self.consume_numeric_suffix();
        if oct.is_empty() {
            return Ok(self.make_located(Token::IntegerLiteral(0), start_line, start_col));
        }
        let value = i64::from_str_radix(&oct, 8).unwrap_or(0);
        Ok(self.make_located(Token::IntegerLiteral(value), start_line, start_col))
    }

    fn read_number(&mut self, start_line: usize, start_col: usize) -> PbResult<Located> {
        let mut num_str = String::new();
        let mut is_float = false;

        // Read integer part
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point
        if self.peek() == Some('.') {
            // Make sure it's not `..` (range)
            if self.peek_ahead(1) != Some('.') {
                is_float = true;
                num_str.push('.');
                self.advance();
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() {
                        num_str.push(ch);
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        // Consume type suffix
        self.consume_numeric_suffix();

        if is_float {
            let val: f64 = num_str.parse().unwrap_or(0.0);
            Ok(self.make_located(Token::FloatLiteral(val), start_line, start_col))
        } else {
            let val: i64 = num_str.parse().unwrap_or(0);
            Ok(self.make_located(Token::IntegerLiteral(val), start_line, start_col))
        }
    }

    /// Consume numeric literal suffixes like ##, &&, %%, &, !, #, @, @@
    fn consume_numeric_suffix(&mut self) {
        match self.peek() {
            Some('#') => {
                self.advance();
                if self.peek() == Some('#') {
                    self.advance(); // ##
                }
            }
            Some('&') => {
                self.advance();
                if self.peek() == Some('&') {
                    self.advance(); // &&
                }
            }
            Some('%') => {
                self.advance();
                if self.peek() == Some('%') {
                    self.advance(); // %%
                }
            }
            Some('!') => {
                self.advance();
            }
            Some('@') => {
                self.advance();
                if self.peek() == Some('@') {
                    self.advance(); // @@
                }
            }
            Some('?') => {
                // Some Win32API equates use ??? suffix
                while self.peek() == Some('?') {
                    self.advance();
                }
            }
            _ => {}
        }
    }

    fn read_identifier_or_keyword(
        &mut self,
        start_line: usize,
        start_col: usize,
    ) -> PbResult<Located> {
        let mut word = String::new();

        // Read the base word
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for line continuation: bare underscore at end of line
        if word == "_" {
            return Ok(self.make_located(Token::Underscore, start_line, start_col));
        }

        // Check for type suffix on identifier
        let suffix = match self.peek() {
            Some('#') => {
                self.advance();
                if self.peek() == Some('#') {
                    self.advance();
                    "##"
                } else {
                    "#"
                }
            }
            Some('&') => {
                self.advance();
                if self.peek() == Some('&') {
                    self.advance();
                    "&&"
                } else {
                    "&"
                }
            }
            Some('%') => {
                self.advance();
                if self.peek() == Some('%') {
                    self.advance();
                    "%%"
                } else {
                    "%"
                }
            }
            Some('!') => {
                self.advance();
                "!"
            }
            Some('@') => {
                self.advance();
                if self.peek() == Some('@') {
                    self.advance();
                    "@@"
                } else {
                    "@"
                }
            }
            Some('$') => {
                self.advance();
                "$"
            }
            _ => "",
        };

        let full_name = if suffix.is_empty() {
            word.clone()
        } else {
            format!("{}{}", word, suffix)
        };

        // Check if it's a keyword (only if no suffix)
        if suffix.is_empty() {
            if let Some(kw) = Token::from_keyword(&word) {
                return Ok(self.make_located(kw, start_line, start_col));
            }
        }

        // Check special REM comment
        if suffix.is_empty() && word.to_uppercase() == "REM" {
            self.skip_to_eol();
            return Ok(self.make_located(Token::Eol, start_line, start_col));
        }

        Ok(self.make_located(
            Token::Identifier(full_name.to_uppercase()),
            start_line,
            start_col,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("GLOBAL X AS LONG\n", None);
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0].token, Token::Global));
        assert!(matches!(tokens[1].token, Token::Identifier(ref s) if s == "X"));
        assert!(matches!(tokens[2].token, Token::As));
        assert!(matches!(tokens[3].token, Token::Long));
    }

    #[test]
    fn test_type_suffix() {
        let mut lexer = Lexer::new("Cash#(i) = 0\n", None);
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0].token, Token::Identifier(ref s) if s == "CASH#"));
    }

    #[test]
    fn test_string_literal() {
        let mut lexer = Lexer::new("PRINT \"hello world\"\n", None);
        let tokens = lexer.tokenize().unwrap();
        assert!(matches!(tokens[0].token, Token::Print));
        assert!(
            matches!(tokens[1].token, Token::StringLiteral(ref s) if s == "hello world")
        );
    }

    #[test]
    fn test_hex_literal() {
        let mut lexer = Lexer::new("%X = &H00000004&\n", None);
        let tokens = lexer.tokenize().unwrap();
        assert!(
            matches!(tokens[0].token, Token::PercentConstant(ref s) if s == "%X")
        );
        assert!(matches!(tokens[1].token, Token::Eq));
        assert!(matches!(tokens[2].token, Token::IntegerLiteral(4)));
    }

    #[test]
    fn test_line_continuation() {
        let mut lexer = Lexer::new("A = 1 + _\n  2\n", None);
        let tokens = lexer.tokenize().unwrap();
        // Should produce: Identifier(A), Eq, IntegerLiteral(1), Plus, IntegerLiteral(2), Eol
        let non_eol: Vec<_> = tokens
            .iter()
            .filter(|t| !matches!(t.token, Token::Eol | Token::Eof))
            .collect();
        assert_eq!(non_eol.len(), 5); // A = 1 + 2
    }
}
