use crate::error::{Diagnostic, Phase, Span};
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

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
    List(String),
    StringType(String),
    IntType(String),
    FloatType(String),
    BoolType(String),
    CharType(String),
    ErrorType(String),
    VoidType(String),
    Semicolon,
    RangeArrow,
    Eof,
    Error(String),
    InputToken,
    OutputToken,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Arithmetic(ArithmeticOperator),
    Comparison(ComparisonOperator),
    Logical(LogicalOperator),
    Type(TypeOperator),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOperator {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
}

#[derive(Debug, PartialEq, Clone)]
pub enum ComparisonOperator {
    Equal,           // "=="
    NotEqual,        // "!="
    LessThan,        // <
    GreaterThan,     // ">"
    LessThanOrEqual, // <=
    GreaterThanOrEqual, // ">="
                     // Question, // "?"
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOperator {
    And, // "&&"
    Or,  // "||"
    Not, // !
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeOperator {
    Question, // "?"
}

impl fmt::Display for ArithmeticOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArithmeticOperator::Add => write!(f, "+"),
            ArithmeticOperator::Sub => write!(f, "-"),
            ArithmeticOperator::Mul => write!(f, "*"),
            ArithmeticOperator::Div => write!(f, "/"),
        }
    }
}

impl fmt::Display for LogicalOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LogicalOperator::And => write!(f, "&&"),
            LogicalOperator::Or => write!(f, "||"),
            LogicalOperator::Not => write!(f, "!"),
        }
    }
}

// For ComparisonOperator
impl fmt::Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ComparisonOperator::Equal => write!(f, "=="),
            ComparisonOperator::NotEqual => write!(f, "!="),
            ComparisonOperator::LessThan => write!(f, "<"),
            ComparisonOperator::GreaterThan => write!(f, ">"),
            ComparisonOperator::LessThanOrEqual => write!(f, "<="),
            ComparisonOperator::GreaterThanOrEqual => write!(f, ">="),
            // ComparisonOperator::Question => write!(f, "?"),
        }
    }
}

impl fmt::Display for TypeOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeOperator::Question => write!(f, "?"),
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Arithmetic(op) => write!(f, "{}", op),
            Operator::Logical(op) => write!(f, "{}", op),
            Operator::Comparison(op) => write!(f, "{}", op),
            Operator::Type(op) => write!(f, "{}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    peeked_token: Option<Token>,
    peeked_span: Option<Span>,
    token_span: Span,
    token_start: usize,
    byte_offset: usize,
    in_list: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            peeked_token: None,
            peeked_span: None,
            token_span: Span::default(),
            token_start: 0,
            byte_offset: 0,
            in_list: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.input.next()?;
        self.byte_offset += ch.len_utf8();
        Some(ch)
    }

    pub fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn consume_while<F>(&mut self, condition: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(&ch) = self.peek_char() {
            if condition(ch) {
                result.push(self.next_char().unwrap());
            } else {
                break;
            }
        }
        result
    }

    pub fn peek_token(&mut self) -> Token {
        if let Some(token) = &self.peeked_token {
            token.clone()
        } else {
            let token = self.scan_token();
            self.token_span = Span::new(self.token_start, self.byte_offset);
            self.peeked_token = Some(token.clone());
            self.peeked_span = Some(self.token_span.clone());
            token
        }
    }

    pub fn token_span(&self) -> Span {
        self.token_span.clone()
    }

    /// Current byte offset, useful for positioning diagnostics at end of input.
    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    pub fn next_token_checked(&mut self) -> Result<Token, Diagnostic> {
        let token = self.next_token();
        if let Token::Error(message) = &token {
            return Err(Diagnostic::new(
                Phase::Lexing,
                message.clone(),
                self.token_span(),
            ));
        }
        Ok(token)
    }

    pub fn next_token(&mut self) -> Token {
        // Return peeked token if we have one
        if let Some(token) = self.peeked_token.take() {
            if let Some(span) = self.peeked_span.take() {
                self.token_span = span;
            }
            return token;
        }
        let token = self.scan_token();
        self.token_span = Span::new(self.token_start, self.byte_offset);
        token
    }

    fn scan_token(&mut self) -> Token {
        // let mut x = 0;

        while let Some(&ch) = self.peek_char() {
            // println!("inList: {}", self.in_list);
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    self.next_char();
                    continue;
                }
                _ => {
                    self.token_start = self.byte_offset;
                }
            }
            match ch {
                ' ' | '\t' | '\n' | '\r' => {
                    self.next_char();
                    continue;
                }
                '0'..='9' => {
                    let digits = self.consume_while(|c| c.is_ascii_digit());
                    let val = match digits.parse::<i64>() {
                        Ok(value) => value,
                        Err(_) => return Token::Error("integer literal is out of range".into()),
                    };
                    if let Some('.') = self.peek_char() {
                        self.next_char();
                        let val2 = self.consume_while(|c| c.is_ascii_digit());
                        if val2.is_empty() {
                            return Token::Error("expected digits after decimal point".into());
                        }
                        let fstr = format!("{}.{}", val, val2);
                        return match fstr.parse::<f64>() {
                            Ok(value) if value.is_finite() => Token::FloatLiteral(value),
                            _ => Token::Error("invalid floating point literal".into()),
                        };
                    }
                    return Token::IntLiteral(val);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let ident = self.consume_while(|c| c.is_alphanumeric() || c == '_');

                    return match ident.as_str() {
                        "true" => Token::BoolLiteral(true),
                        "false" => Token::BoolLiteral(false),
                        "if" | "elif" | "else" | "loop" | "func" | "return" | "do" | "guard" => {
                            Token::Keyword(ident)
                        }
                        "string" => Token::StringType(ident),
                        "int" => Token::IntType(ident),
                        "float" => Token::FloatType(ident),
                        "bool" => Token::BoolType(ident),
                        "char" => Token::CharType(ident),
                        "void" => Token::VoidType(ident),
                        "error" => Token::ErrorType(ident),
                        "list" => {
                            self.in_list += 1;
                            Token::List(ident)
                        }
                        _ => Token::Ident(ident),
                    };
                }
                '#' => {
                    self.next_char();
                    self.consume_while(|c| c != '\n');
                }
                '=' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(ComparisonOperator::Equal));
                    }
                    return Token::Assignment("=".into());
                }
                '?' => {
                    self.next_char();
                    // return Token::Operator(Operator::Comparison(ComparisonOperator::Question))
                    return Token::Operator(Operator::Type(TypeOperator::Question));
                }
                '\'' => {
                    self.next_char();
                    let char_lit = self.consume_while(|c| c != '\'' && c != '\n');
                    if self.next_char() != Some('\'') {
                        return Token::Error("unterminated character literal".into());
                    }

                    // Check if the length is not exactly 1
                    if char_lit.chars().count() != 1 {
                        return Token::Error("character literal must contain one character".into());
                    }

                    // Convert the string to a char and return the token
                    return match char_lit.chars().next() {
                        Some(value) => Token::CharLiteral(value),
                        None => Token::Error("character literal must contain one character".into()),
                    };
                }
                '"' => {
                    self.next_char();
                    let string_lit = self.consume_while(|c| c != '"' && c != '\n');
                    if self.next_char() != Some('"') {
                        return Token::Error("unterminated string literal".into());
                    }
                    return Token::StringLiteral(string_lit);
                }
                '+' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Add));
                }
                '-' => {
                    self.next_char();
                    if let Some('>') = self.peek_char() {
                        self.next_char();
                        return Token::RangeArrow;
                    }
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Sub));
                }
                '*' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Mul));
                }
                '/' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Div));
                }
                '>' => {
                    self.next_char();
                    if self.in_list > 0 {
                        self.in_list -= 1;
                        return Token::Operator(Operator::Comparison(
                            ComparisonOperator::GreaterThan,
                        ));
                    }

                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(
                            ComparisonOperator::GreaterThanOrEqual,
                        ));
                    } else if let Some('>') = self.peek_char() {
                        self.next_char();
                        return Token::InputToken;
                    }
                    return Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan));
                }
                '<' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(
                            ComparisonOperator::LessThanOrEqual,
                        ));
                    } else if let Some('<') = self.peek_char() {
                        self.next_char();
                        return Token::OutputToken;
                    }
                    return Token::Operator(Operator::Comparison(ComparisonOperator::LessThan));
                }
                '!' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(ComparisonOperator::NotEqual));
                    }
                    return Token::Operator(Operator::Logical(LogicalOperator::Not));
                }
                '|' => {
                    self.next_char();
                    if let Some('|') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Logical(LogicalOperator::Or));
                    } else {
                        return Token::Error("expected a second '|' for logical OR".into());
                    }
                }
                '&' => {
                    self.next_char();
                    if let Some('&') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Logical(LogicalOperator::And));
                    } else {
                        return Token::Error("expected a second '&' for logical AND".into());
                    }
                }
                ';' => {
                    self.next_char();
                    return Token::Semicolon;
                }
                '{' | '}' | '(' | ')' | '[' | ']' | ',' | ':' | '`' => {
                    return Token::Symbol(self.next_char().unwrap());
                }
                _ => {
                    self.next_char();
                    return Token::Error(format!("unexpected character {:?}", ch));
                }
            }
        }
        Token::Eof
    }
}
