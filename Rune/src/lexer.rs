use std::str::Chars;
use std::iter::Peekable;

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
    RangeArrow,
    Equality,
    Eof,
    Error(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Arithmetic(ArithmeticOperator)
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    peeked_token: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { 
            input: input.chars().peekable(),
            peeked_token: None,
        }
    }
    
    fn next_char(&mut self) -> Option<char> {
        self.input.next()
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
            let token = self.next_token();
            self.peeked_token = Some(token.clone());
            token
        }
    }
    
    pub fn next_token(&mut self) -> Token {
        // Return peeked token if we have one
        if let Some(token) = self.peeked_token.take() {
            return token;
        }

        while let Some(&ch) = self.peek_char() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => { self.next_char(); continue; }
                '0'..='9' => {
                    let val = self.consume_while(|c| c.is_digit(10)).parse().unwrap();
                    if let Some('.') = self.peek_char() {
                        self.next_char();
                        let val2 :String = self.consume_while(|c| c.is_digit(10)).parse().unwrap();
                        let fstr = format!("{}.{}", val, val2);
                        let f = fstr.parse::<f64>();
                        match f {
                            Ok(float_value) => return Token::FloatLiteral(float_value),
                            Err(e) => {
                                panic!("Failed to parse float: {}", e);
                            }
                        }
                    }
                    return Token::IntLiteral(val)
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let ident = self.consume_while(|c| c.is_alphanumeric() || c == '_');
                    return match ident.as_str() {
                        "true" => Token::BoolLiteral(true),
                        "false" => Token::BoolLiteral(false),
                        "if" | "elif" | "else" | "loop" | "func" | "" | "return" => Token::Keyword(ident),
                        "string" => Token::StringType(ident),
                        "int" => Token::IntType(ident),
                        "float" => Token::FloatType(ident),
                        "bool" => Token::BoolType(ident),
                        "char" => Token::CharType(ident),
                        "list" => return Token::List(ident),
                        _ => Token::Ident(ident),
                    };
                },
                '#' => {
                    self.next_char();
                    self.consume_while(|c| c != '\n');
                },
                '=' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Equality;
                    }
                    return Token::Assignment("=".into())
                },
                // '\'' => {
                //     self.next_char();
                //     let char_lit = self.consume_while(|c| c != '\'');
                //     self.next_char();
                //     return Token::CharLiteral(char_lit);
                // },
                '"' => {
                    self.next_char();
                    let string_lit = self.consume_while(|c| c != '"');
                    self.next_char();
                    return Token::StringLiteral(string_lit);
                },
                '+' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Add))
                },
                '-' => {
                    self.next_char();
                    if let Some('>') = self.peek_char() {
                        self.next_char();
                        return Token::RangeArrow;
                    }
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Subtract))
                },
                '*' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Multiply))
                },
                '/' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Divide))
                },
                '{' | '}' | '(' | ')' | '[' | ']' | ',' | ':' | '<' | '>' => {
                    return Token::Symbol(self.next_char().unwrap())
                },
                _ => { self.next_char(); continue; }
            }
        }
        Token::Eof
    }
}