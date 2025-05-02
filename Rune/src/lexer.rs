use std::str::Chars;
use std::iter::Peekable;
use std::fmt;

// --- Token Definitions (as provided by user) ---
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    FloatLiteral(f64),
    CharLiteral(char),
    Keyword(String),
    Symbol(char),
    Operator(Operator),
    Assignment(String),
    List(String), // Assuming this is the 'list' keyword token
    StringType(String),
    IntType(String),
    FloatType(String),
    BoolType(String),
    CharType(String),
    ErrorType(String), // Consider using a different type for lexer/parser errors
    VoidType(String),
    Semicolon, // Explicit Semicolon or inserted one
    RangeArrow, // ->
    Eof,
    InputToken, // >>
    OutputToken, // <<
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Arithmetic(ArithmeticOperator),
    Comparison(ComparisonOperator),
    Logical(LogicalOperator),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOperator { Add, Sub, Mul, Div }
#[derive(Debug, PartialEq, Clone)]
pub enum ComparisonOperator { Equal, NotEqual, LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual }
#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOperator { And, Or, Not }

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}
impl fmt::Display for LogicalOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}
impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}
impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}


// --- Lexer Structure with ASI State ---
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    peeked_token: Option<Token>, // Stores token peeked by public peek_token()
    // ASI State:
    last_token_emitted: Option<Token>, // The last *non-whitespace, non-comment, non-semicolon* token returned
    paren_level: usize, // Nesting level for ()
    brace_level: usize, // Nesting level for {}
    bracket_level: usize, // Nesting level for []
    last_returned_was_semicolon: bool, // Flag to track if the *immediately* last returned token was a semicolon
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            peeked_token: None,
            last_token_emitted: None,
            paren_level: 0,
            brace_level: 0,
            bracket_level: 0,
            last_returned_was_semicolon: false,
        }
    }

    fn next_char(&mut self) -> Option<char> { self.input.next() }
    fn peek_char(&mut self) -> Option<&char> { self.input.peek() }

    fn consume_while<F>(&mut self, condition: F) -> String where F: Fn(char) -> bool {
        let mut result = String::new();
        while let Some(&ch) = self.peek_char() {
            if condition(ch) { result.push(self.next_char().unwrap()); } else { break; }
        }
        result
    }

    fn expect_char(&mut self, expected: char) {
        match self.next_char() {
            Some(ch) if ch == expected => {},
            found => panic!("Expected '{}', found {:?}", expected, found),
        }
    }

     fn skip_all_whitespace(&mut self) {
         while let Some(&ch) = self.peek_char() {
             if ch.is_whitespace() { self.next_char(); } else { break; }
         }
     }

    fn is_newline(ch: char) -> bool { ch == '\n' || ch == '\r' }

     fn skip_non_newline_whitespace(&mut self) {
         while let Some(&ch) = self.peek_char() {
             if ch == ' ' || ch == '\t' || ch == '\r' { self.next_char(); } else { break; }
         }
     }

    // Helper contains the core tokenization logic.
    // Returns Option<Token> if a token was found, or None if a comment was skipped. Panics on unexpected chars.
    // It does NOT handle newlines or ASI logic.
    // It does NOT update grouping levels.
    fn next_real_token_body(&mut self) -> Option<Token> {
        match self.peek_char()? {
            '0'..='9' => { let val_str = self.consume_while(|c| c.is_digit(10)); if let Some('.') = self.peek_char() { self.next_char(); let decimal_part = self.consume_while(|c| c.is_digit(10)); let full_str = format!("{}.{}", val_str, decimal_part); let f = full_str.parse::<f64>().unwrap_or_else(|e| panic!("Failed to parse float literal '{}': {}", full_str, e)); Some(Token::FloatLiteral(f)) } else { let val = val_str.parse::<i64>().unwrap_or_else(|e| panic!("Failed to parse integer literal '{}': {}", val_str, e)); Some(Token::IntLiteral(val)) } },
            'a'..='z' | 'A'..='Z' | '_' => { let ident = self.consume_while(|c| c.is_alphanumeric() || c == '_'); Some(match ident.as_str() { "true" => Token::BoolLiteral(true), "false" => Token::BoolLiteral(false), "if" | "elif" | "else" | "loop" | "func" | "return" | "do" | "break" | "continue" => Token::Keyword(ident), "string" => Token::StringType(ident), "int" => Token::IntType(ident), "float" => Token::FloatType(ident), "bool" => Token::BoolType(ident), "char" => Token::CharType(ident), "void" => Token::VoidType(ident), "error" => Token::ErrorType(ident), "list" => Token::List(ident), _ => Token::Ident(ident), }) },
            '#' => { self.next_char(); self.consume_while(|c| !Self::is_newline(c)); return None; },
            '=' => { self.next_char(); if let Some('=') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Comparison(ComparisonOperator::Equal))) } else { Some(Token::Assignment("=".into())) } },
            '\'' => { self.next_char(); let char_lit = self.next_char().expect("Expected character in literal"); self.expect_char('\''); Some(Token::CharLiteral(char_lit)) },
            '"' => { self.next_char(); let string_lit = self.consume_while(|c| c != '"'); self.expect_char('"'); Some(Token::StringLiteral(string_lit)) },
            '+' => { self.next_char(); Some(Token::Operator(Operator::Arithmetic(ArithmeticOperator::Add))) },
            '-' => { self.next_char(); if let Some('>') = self.peek_char() { self.next_char(); Some(Token::RangeArrow) } else { Some(Token::Operator(Operator::Arithmetic(ArithmeticOperator::Sub))) } },
            '*' => { self.next_char(); Some(Token::Operator(Operator::Arithmetic(ArithmeticOperator::Mul))) },
            '/' => { self.next_char(); Some(Token::Operator(Operator::Arithmetic(ArithmeticOperator::Div))) },
            '>' => { self.next_char(); if let Some('=') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThanOrEqual))) } else if let Some('>') = self.peek_char() { self.next_char(); Some(Token::InputToken) } else { Some(Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan))) } },
            '<' => { self.next_char(); if let Some('=') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Comparison(ComparisonOperator::LessThanOrEqual))) } else if let Some('<') = self.peek_char() { self.next_char(); Some(Token::OutputToken) } else { Some(Token::Operator(Operator::Comparison(ComparisonOperator::LessThan))) } },
            '!' => { self.next_char(); if let Some('=') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Comparison(ComparisonOperator::NotEqual))) } else { Some(Token::Operator(Operator::Logical(LogicalOperator::Not))) } },
            ';' => { self.next_char(); Some(Token::Semicolon) },
            '|' => { self.next_char(); if let Some('|') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Logical(LogicalOperator::Or))) } else { Some(Token::Symbol('|')) } },
            '&' => { self.next_char(); if let Some('&') = self.peek_char() { self.next_char(); Some(Token::Operator(Operator::Logical(LogicalOperator::And))) } else { Some(Token::Symbol('&')) } },
            '{' => { self.next_char(); Some(Token::Symbol('{')) }, '}' => { self.next_char(); Some(Token::Symbol('}')) }, '(' => { self.next_char(); Some(Token::Symbol('(')) }, ')' => { self.next_char(); Some(Token::Symbol(')')) }, '[' => { self.next_char(); Some(Token::Symbol('[')) }, ']' => { self.next_char(); Some(Token::Symbol(']')) }, ',' => { self.next_char(); Some(Token::Symbol(',')) }, ':' => { self.next_char(); Some(Token::Symbol(':')) }, '`' => { self.next_char(); Some(Token::Symbol('`')) },
            _ => { let unexpected = self.next_char().expect("Should get a character if peek_char was Some"); panic!("Unexpected character: {}", unexpected); }
        }
    }

    fn update_grouping_levels(&mut self, token: &Token) {
        match token {
            Token::Symbol('{') => self.brace_level += 1, Token::Symbol('}') => { if self.brace_level > 0 { self.brace_level -= 1; } },
            Token::Symbol('(') => self.paren_level += 1, Token::Symbol(')') => { if self.paren_level > 0 { self.paren_level -= 1; } },
            Token::Symbol('[') => self.bracket_level += 1, Token::Symbol(']') => { if self.bracket_level > 0 { self.bracket_level -= 1; } },
            _ => {}
        }
    }

    fn token_can_end_statement(&self, token: &Token) -> bool {
        match token {
            Token::Ident(_) | Token::IntLiteral(_) | Token::StringLiteral(_) | Token::BoolLiteral(_) | Token::FloatLiteral(_) | Token::CharLiteral(_) |
            Token::Symbol('}') | Token::Symbol(')') | Token::Symbol(']') |
            Token::Semicolon | Token::OutputToken | Token::InputToken => true,
            Token::Keyword(k) => matches!(k.as_str(), "break" | "continue" | "return" | "do"),
            _ => false,
        }
    }

      fn token_can_start_statement(&self, token: &Token) -> bool {
          match token {
              Token::Ident(_) | Token::IntLiteral(_) | Token::StringLiteral(_) | Token::BoolLiteral(_) | Token::FloatLiteral(_) | Token::CharLiteral(_) |
              Token::Keyword(_) |
              Token::Symbol('{') | Token::Symbol('(') | Token::Symbol('[') |
              Token::Semicolon |
              Token::List(_) | Token::StringType(_) | Token::IntType(_) | Token::FloatType(_) | Token::BoolType(_) | Token::CharType(_) | Token::ErrorType(_) | Token::VoidType(_) |
              Token::Operator(Operator::Comparison(ComparisonOperator::LessThan)) |
              Token::Operator(Operator::Arithmetic(ArithmeticOperator::Sub)) | Token::Operator(Operator::Logical(LogicalOperator::Not)) |
              Token::OutputToken => true,
              // Token::Eof handled explicitly in get_next_token_logic newline logic
              _ => false,
          }
      }

    // Helper to peek the next token *after* skipping *all* whitespace, without using self.peeked_token.
    // Used internally by get_next_token_logic for ASI lookahead. Saves/Restores input state.
    fn peek_next_token_internal(&mut self) -> Token {
        // Save current input state
        let saved_input = self.input.clone();

        self.skip_all_whitespace(); // Skip all whitespace *before* attempting to peek the token

        // Try to get the next real token using the core logic
        // This runs on the actual input stream temporarily.
        // Handle None (comment) by calling it again until a token or EOF is found.
        let mut token = None;
        while token.is_none() && self.peek_char().is_some() {
             token = self.next_real_token_body();
             if token.is_none() {
                 // Skipped a comment, skip any whitespace after it before trying again
                 self.skip_all_whitespace();
             }
        }

        let final_token = token.unwrap_or(Token::Eof);

        // Restore input state immediately after peeking
        self.input = saved_input;

        final_token // Return the temporary token, do NOT store in self.peeked_token
    }


    // Public peek_token function. Skips all whitespace and gets the next token.
    // Used by the parser for lookahead. Saves/Restores input state.
    // It populates self.peeked_token.
    pub fn peek_token(&mut self) -> Token {
        if let Some(token) = &self.peeked_token {
            return token.clone();
        }

        // Save current input state
        let saved_input = self.input.clone();

        self.skip_all_whitespace(); // Skip all whitespace *before* attempting to peek the token

        // Try to get the next real token using the core logic
        // This runs on the actual input stream temporarily.
        // Handle None (comment) by calling it again until a token or EOF is found.
        let mut token = None;
        while token.is_none() && self.peek_char().is_some() {
             token = self.next_real_token_body();
             if token.is_none() {
                 // Skipped a comment, skip any whitespace after it before trying again
                 self.skip_all_whitespace();
             }
        }

        let final_token = token.unwrap_or(Token::Eof);

        // Restore input state immediately after peeking
        self.input = saved_input;

        self.peeked_token = Some(final_token.clone()); // Store the peeked token
        final_token // Return the peeked token
    }

    // Internal helper containing the main tokenization and ASI logic
    fn get_next_token_logic(&mut self) -> Token {
         // 1. Return peeked token first if available
        if let Some(token) = self.peeked_token.take() {
            self.update_grouping_levels(&token); // Update levels for the token
            // Only update last_token_emitted if it's a significant token (not Semicolon)
            if !matches!(token, Token::Semicolon) {
                 self.last_token_emitted = Some(token.clone());
            }
            // Do NOT update last_returned_was_semicolon here; public next_token does that.
            return token;
        }

        // Loop to find the next token or insert a semicolon
        loop {
            // Skip non-newline whitespace
            self.skip_non_newline_whitespace();

            match self.peek_char() {
                Some(ch) if Self::is_newline(*ch) => {
                    // Found a newline. Potential ASI.
                    self.next_char(); // Consume the newline character

                    // Suppress ASI inside paren or bracket, but NOT brace.
                    if self.paren_level > 0 || self.bracket_level > 0 { // FIX: Only suppress inside () or []
                        continue; // Newline is just whitespace inside () or []
                    }

                    // We are at top level or inside a brace block {}. Potential ASI.
                    if let Some(last_token_copy) = self.last_token_emitted.clone() {
                         // Use the internal peek helper to get the next token for ASI check
                        let next_token_after_ws = self.peek_next_token_internal();

                         // ASI Insertion Condition (Go-like logic):
                         // 1. The last token was NOT a semicolon (prevents double semicolon like `;\n`).
                         // 2. The last token can end a statement.
                         // 3. The last token was NOT an opener/operator/comma (that indicate expression continuation).
                         // 4. The next token is NOT an explicit semicolon or closing brace `}`.
                         // 5. The next token can start a statement OR next token is EOF.

                         let last_is_terminator = self.token_can_end_statement(&last_token_copy);

                         // Check if the last token is one that implies the statement/expression continues
                         let last_is_opener_operator = matches!(&last_token_copy,
                             Token::Symbol('(') | Token::Symbol('{') | Token::Symbol('[') | Token::Symbol(',') |
                             Token::Assignment(_) | Token::Operator(_) // Add other operators like -> if needed
                         );

                         // Check if the next token is one that suppresses the preceding semicolon
                         let next_is_suppressor = matches!(&next_token_after_ws,
                             Token::Semicolon | Token::Symbol('}') // Standard Go suppressors after newline
                             // Add other closing delimiters like ')' or ']' here if they should suppress ASI
                         );

                         // Final ASI condition:
                         if !self.last_returned_was_semicolon // Previous returned was not semicolon
                            && last_is_terminator // Last token can end a statement
                            && !last_is_opener_operator // Last token doesn't force continuation
                            && !next_is_suppressor // Next token doesn't suppress
                            && (self.token_can_start_statement(&next_token_after_ws) || matches!(&next_token_after_ws, Token::Eof)) // Next can start OR is EOF
                         {
                             // ASI triggered. Return Semicolon.
                             return Token::Semicolon;
                         }
                    }
                    // If ASI condition failed or no last token, this newline is ignored.
                    continue;
                },
                Some(';') => {
                    // Found an explicit semicolon.
                    self.next_char(); // Consume ';'
                    // Return the explicit semicolon.
                    // The last_returned_was_semicolon flag will be updated by the public next_token.
                    return Token::Semicolon;
                },
                Some(_) => {
                    // Found the start of a non-ignorable token (not newline, not semicolon).
                    // Tokenize it using next_real_token_body.
                    match self.next_real_token_body() {
                         Some(real_token) => {
                              // Found a real token. Update state and return it.
                              self.update_grouping_levels(&real_token); // Update levels based on the token found
                              // Only update last_token_emitted if it's a significant token (not Semicolon)
                              if !matches!(real_token, Token::Semicolon) {
                                self.last_token_emitted = Some(real_token.clone()); // Store it as last emitted significant token
                              }
                              // Do NOT update last_returned_was_semicolon here.
                              return real_token;
                         },
                         None => {
                            // next_real_token_body returned None, meaning it skipped a comment.
                            continue; // Go back to start of loop to find next token
                         }
                    }
                },
                None => {
                    // Reached EOF after skipping all trailing whitespace/comments.
                    // Do NOT insert a semicolon automatically at EOF.
                    // The parser should handle EOF as the program termination.
                    self.last_token_emitted = Some(Token::Eof); // Record Eof as last token
                    // Do NOT update last_returned_was_semicolon here.
                    return Token::Eof;
                }
            }
        } // End of outer loop
    }

    // --- Main next_token function (public) ---
    pub fn next_token(&mut self) -> Token {
        // Get the next token using the internal logic
        let next_token = self.get_next_token_logic();

        // Update the flag based on the token that is ABOUT TO BE RETURNED
        // This flag is checked by the *next* call to get_next_token_logic (specifically in the newline handling)
        self.last_returned_was_semicolon = matches!(&next_token, Token::Semicolon);

        // Return the token
        next_token
    }
}
