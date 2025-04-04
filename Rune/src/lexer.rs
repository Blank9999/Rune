use std::str::Chars;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String),
    IntLiteral(i64),
    StringLiteral(String),
    BoolLiteral(bool),
    Keyword(String),
    Symbol(char),
    Operator(String),
    List(String),
    StringType(String),
    IntType(String),
    FloatType(String),
    BoolType(String),
    CharType(String),
    Comment(String),
    Eof,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input: input.chars().peekable() }
    }
    
    fn next_char(&mut self) -> Option<char> {
        self.input.next()
    }
    
    fn peek_char(&mut self) -> Option<&char> {
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
    
    pub fn next_token(&mut self) -> Token {
        while let Some(&ch) = self.peek_char() {
            match ch {
                ' ' | '\t' | '\n' | '\r' => { self.next_char(); continue; }
                '0'..='9' => return Token::IntLiteral(self.consume_while(|c| c.is_digit(10)).parse().unwrap()),
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
                    let comment = self.consume_while(|c| c != '\n');
                    return Token::Comment(comment);
                },
                '"' => {
                    self.next_char(); // consume opening quote
                    let string_lit = self.consume_while(|c| c != '"');
                    self.next_char(); // consume closing quote
                    return Token::StringLiteral(string_lit);
                }
                '+' | '-' | '*' | '/' => return Token::Operator(self.next_char().unwrap().to_string()),
                '{' | '}' | '(' | ')' | '[' | ']' | ',' | ':' | '<' | '>' => return Token::Symbol(self.next_char().unwrap()),
                _ => { self.next_char(); continue; }
            }
        }
        Token::Eof
    }
}
