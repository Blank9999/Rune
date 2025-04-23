use std::str::Chars;
use std::iter::Peekable;
use std::fmt;

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
    // Error(String),
    InputToken,
    OutputToken,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Arithmetic(ArithmeticOperator),
    Comparison(ComparisonOperator),
    Logical(LogicalOperator),
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
    Equal,     // "=="
    NotEqual,  // "!="
    LessThan,  // <
    GreaterThan, // ">"
    LessThanOrEqual, // <= 
    GreaterThanOrEqual, // ">="
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOperator {
    And,       // "&&"
    Or,        // "||"
    Not,      // !
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
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operator::Arithmetic(op) => write!(f, "{}", op),
            Operator::Logical(op) => write!(f, "{}", op),
            Operator::Comparison(op) => write!(f, "{}", op),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    peeked_token: Option<Token>,
    in_list: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { 
            input: input.chars().peekable(),
            peeked_token: None,
            in_list: 0,
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
        // let mut x = 0;

        while let Some(&ch) = self.peek_char() {
            // println!("inList: {}", self.in_list);
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
                        "if" | "elif" | "else" | "loop" | "func" | "" | "return" | "do" => Token::Keyword(ident),
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
                        },
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
                        return Token::Operator(Operator::Comparison(ComparisonOperator::Equal))
                    }
                    return Token::Assignment("=".into())
                },
                '\'' => {
                    self.next_char();
                    let char_lit = self.consume_while(|c| c != '\'');
                    self.next_char();
                    
                    // Check if the length is not exactly 1
                    if char_lit.len() != 1 {
                        panic!("Failed to parse char: {}", char_lit);
                    }
                    
                    // Convert the string to a char and return the token
                    return Token::CharLiteral(char_lit.chars().next().unwrap());
                },
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
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Sub))
                },
                '*' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Mul))
                },
                '/' => {
                    self.next_char();
                    return Token::Operator(Operator::Arithmetic(ArithmeticOperator::Div))
                },
                '>' => {
                    self.next_char();
                    if self.in_list > 0 { 
                        self.in_list -= 1;
                        return Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan));
                    }

                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThanOrEqual));
                    }             
                    else if let Some('>') = self.peek_char() {
                        self.next_char();
                        return Token::InputToken; 
                    }
                    return Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan));
                },
                '<' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(ComparisonOperator::LessThanOrEqual));
                    }
                    
                     else if let Some('<') = self.peek_char() {
                        self.next_char();
                        return Token::OutputToken;
                    } 
                    return Token::Operator(Operator::Comparison(ComparisonOperator::LessThan));
                },
                '!' => {
                    self.next_char();
                    if let Some('=') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Comparison(ComparisonOperator::NotEqual));
                    }
                    return Token::Operator(Operator::Logical(LogicalOperator::Not))
                },
                '|' => {
                    self.next_char();
                    if let Some('|') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Logical(LogicalOperator::Or));
                    } else {
                        return Token::Symbol(self.next_char().unwrap())
                    }
                },
                '&' => {
                    self.next_char();
                    if let Some('&') = self.peek_char() {
                        self.next_char();
                        return Token::Operator(Operator::Logical(LogicalOperator::And));
                    } else {
                        return Token::Symbol(self.next_char().unwrap())
                    }
                },
                '{' | '}' | '(' | ')' | '[' | ']' | ',' | ':' | '`' => {
                    return Token::Symbol(self.next_char().unwrap())
                },
                _ => { self.next_char(); continue; }
            }
        }
        Token::Eof
    }
}