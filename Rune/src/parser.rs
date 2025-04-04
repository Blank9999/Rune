use crate::lexer::{Lexer, Token};
use crate::ast::{Type, Literal, Expression, Declaration,Program}; 

// mod ast;  

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a>  Parser<'a>  {
    pub fn new(mut lexer:  Lexer<'a>) -> Self {
        let current = lexer.next_token();
        Self { lexer, current }
    }

    fn advance(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn expect(&mut self, expected: &Token) {
        if &self.current == expected {
            self.advance();
        } else {
            panic!("Expected {:?}, found {:?}", expected, self.current);
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut declarations = Vec::new();

        while self.current != Token::Eof {
            let decl = self.parse_declaration();
            declarations.push(decl);
            // match self.current {
            //     Token::StringType(_) | Token::IntType(_) | Token::FloatType(_) | Token::BoolType(_) => {
            //         // Parse the declaration
            //         println!("It reached the variable declarations part");
            //         let decl = self.parse_declaration();
            //         declarations.push(decl);
            //     }
            //     _ => {
            //         // Handle other tokens here]
            //         println!("It did not reach the variable declarations part");
            //     }
            // }
        }

        Program { declarations }
    }

    pub fn parse_declaration(&mut self) -> Declaration {
        let var_type = self.parse_type();
        let identifier = if let Token::Ident(name) = &self.current {
            let id = name.clone();
            self.advance();
            id
        } else {
            panic!("Expected identifier, found {:?}", self.current);
        };
        // println!("{:#?}", identifier);
        // self.advance();
        // println!("{:#?}", self.lexer);

        self.expect(&Token::Assignemt("=".into()));
        let value = self.parse_expression();
        Declaration {
            var_type,
            identifier,
            value,
        }
    }

    fn parse_type(&mut self) -> Type {
        match &self.current {
            Token::IntType(_) => {
                self.advance();
                Type::Int
            }
            Token::StringType(_) => {
                self.advance();
                Type::String
            }
            Token::BoolType(_) => {
                self.advance();
                Type::Bool
            }
            Token::CharType(_) => {
                self.advance();
                Type::Char
            }
            Token::FloatType(_) => {
                self.advance();
                Type::Float
            }
            _ => panic!("Expected type, found {:?}", self.current),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        match &self.current {
            Token::IntLiteral(n) => {
                let lit = Literal::Int(*n);
                self.advance();
                Expression::Literal(lit)
            }
            Token::StringLiteral(s) => {
                let lit = Literal::String(s.clone());
                self.advance();
                Expression::Literal(lit)
            }
            Token::BoolLiteral(b) => {
                let lit = Literal::Bool(*b);
                self.advance();
                Expression::Literal(lit)
            }
            Token::Ident(name) => {
                let id = name.clone();
                self.advance();
                Expression::Identifier(id)
            }
            _ => panic!("Unexpected token in expression: {:?}", self.current)
        }
    }
}