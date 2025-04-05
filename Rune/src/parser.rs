use crate::lexer::{Lexer, Token};
use crate::ast::{
    Type, Literal, Expression, Declaration, Program, Statement, IfExpr, LoopExpr,
    Function, Parameter,
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
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
        let mut statements = Vec::new();
        while self.current != Token::Eof {
            let stmt = self.parse_statement();
            statements.push(stmt);
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.current {
            Token::Keyword(k) if k == "if" => self.parse_if(),
            Token::Keyword(k) if k == "loop" => self.parse_loop(),
            Token::Keyword(k) if k == "return" => {
                self.advance();
                let expr = self.parse_expression();
                Statement::Return(expr)
            }
            Token::Keyword(k) if k == "guard" => {
                self.advance();
                let cond = self.parse_expression();
                Statement::Guard(cond)
            }
            Token::Keyword(k) if k == "func" => {
                self.advance();
                Statement::Function(self.parse_function())
            }

            Token::IntType(_) 
            | Token::StringType(_) 
            | Token::BoolType(_) 
            | Token::FloatType(_) 
            | Token::CharType(_) => {
                Statement::Declaration(self.parse_declaration())
            }
        
            // optionally, if you use Token::Keyword("int"), support that too
            Token::Keyword(k) if ["int", "string", "bool", "char", "float"].contains(&k.as_str()) => {
                Statement::Declaration(self.parse_declaration())
            }
            _ => {
                let expr = self.parse_expression();
                Statement::Expression(expr)
            }
        }
    }    

    fn parse_declaration(&mut self) -> Declaration {
        let var_type = self.parse_type();
        let identifier = if let Token::Ident(name) = &self.current {
            let id = name.clone();
            self.advance();
            id
        } else {
            panic!("Expected identifier, found {:?}", self.current);
        };

        self.expect(&Token::Assignment("=".into()));
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
            _ => panic!("Unexpected token in expression: {:?}", self.current),
        }
    }

    fn parse_if(&mut self) -> Statement {
        self.advance(); // consume 'if'

        let mut conditions = Vec::new();
        while let Some(Token::IntLiteral(_) | Token::Ident(_) | Token::BoolLiteral(_)) = Some(&self.current) {
            conditions.push(self.parse_expression());

            if let Token::Symbol(',') = self.current {
                self.advance();
            } else {
                break;
            }
        }

        let then_block = self.parse_block();

        let mut elif_blocks = Vec::new();
        while let Token::Keyword(k) = &self.current {
            if k == "elif" {
                self.advance();
                let mut elif_conds = Vec::new();
                while let Some(Token::IntLiteral(_) | Token::Ident(_) | Token::BoolLiteral(_)) = Some(&self.current) {
                    elif_conds.push(self.parse_expression());
                    if let Token::Symbol(',') = self.current {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let elif_block = self.parse_block();
                elif_blocks.push((elif_conds, elif_block));
            } else {
                break;
            }
        }

        let else_block = if let Token::Keyword(k) = &self.current {
            if k == "else" {
                self.advance();
                Some(self.parse_block())
            } else {
                None
            }
        } else {
            None
        };

        Statement::If(IfExpr {
            conditions,
            then_block,
            elif_blocks,
            else_block,
        })
    }

    fn parse_loop(&mut self) -> Statement {
        self.advance(); // consume 'loop'

        match &self.current {
            // Range loop: `loop var -> iterable { ... }`
            Token::Ident(var) => {
                let var_name = var.clone();
                self.advance(); // consume identifier

                // Expect the `->` symbol for range-based loop
                if let Token::RangeArrow = self.current {
                    self.advance(); 

                    // Handles loop num -> nums {} type of range
                    if let Token::Ident(name) = &self.current {
                        let iteratable = self.parse_expression();
                        let body = self.parse_block();
                        
                        Statement::Loop(LoopExpr::ForEach {
                            var: var_name,
                            iterable: iteratable,
                            body,
                        })
                    } else {
                        // Handles loop i -> 0 : 3 : 1 type of range
                        let start_expr = self.parse_expression();
                        let mut end_expr = None;
                        let mut step_expr = None;

                        // Optional end expression and step expression
                        
                        if let Token::Symbol(':') = self.current {
                            self.advance();
                            end_expr = Some(self.parse_expression());

                            if let Token::Symbol(':') = self.current {
                                self.advance();
                                step_expr = Some(self.parse_expression());
                            }
                        }

                        // Now, expect the block of code for the body of the loop
                        let body = self.parse_block();
                        Statement::Loop(LoopExpr::Range {
                            var: var_name,
                            start: start_expr,
                            end: end_expr.expect("Expected end expression"),
                            step: step_expr,
                            body,
                        })
                    }
                } else {
                    panic!("Expected '->' after loop variable in range-based loop");
                }
            }
            // Condition-based loop `loop condition { ... }`
            Token::BoolLiteral(_) | Token::Ident(_) => {
                let condition = self.parse_expression();
                let body = self.parse_block();
                Statement::Loop(LoopExpr::Condition { condition, body })
            }
            // Infinite loop
            _ => {
                let body = self.parse_block();
                Statement::Loop(LoopExpr::Infinite { body })
            }
        }
    }

    fn parse_function(&mut self) -> Function {
        let return_type = self.parse_type();
        let name = if let Token::Ident(name) = &self.current {
            let id = name.clone();
            self.advance();
            id
        } else {
            panic!("Expected function name");
        };

        self.expect(&Token::Symbol('('));
        let mut params = Vec::new();

        while self.current != Token::Symbol(')') {
            let param_type = self.parse_type();
            let param_name = if let Token::Ident(name) = &self.current {
                let name = name.clone();
                self.advance();
                name
            } else {
                panic!("Expected parameter name");
            };

            let default = if let Token::Assignment(_) = &self.current {
                self.advance();
                Some(self.parse_expression())
            } else {
                None
            };

            params.push(Parameter {
                param_type,
                param_name,
                default,
            });

            if let Token::Symbol(',') = self.current {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::Symbol(')'));
        let body = self.parse_block();

        Function {
            return_type,
            name,
            parameters: params,
            body,
        }
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        let mut block = Vec::new();
        self.expect(&Token::Symbol('{'));

        while self.current != Token::Symbol('}') {
            block.push(self.parse_statement());
        }

        self.expect(&Token::Symbol('}'));
        block
    }
}
