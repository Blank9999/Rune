use crate::lexer::{Lexer, Token,Operator,ComparisonOperator,LogicalOperator};
use crate::ast::{
    Type, Literal, Expression, Declaration, Program, Statement, IfExpr, LoopExpr,
    Function, Parameter,Condition
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

    fn peek(&mut self) -> Token {
        self.lexer.peek_token()
    }

    fn expect(&mut self, expected: &Token) {
        if &self.current == expected {
            self.advance();
        } else if &self.current != &Token::Symbol('{') {
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
            Token::Keyword(k) if k == "do" => {
                self.advance();
                let expr = self.parse_expression();
                Statement::Do(expr)
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

            Token::OutputToken => {
                self.parse_output()
            }

            Token::IntType(_) 
            | Token::StringType(_) 
            | Token::BoolType(_) 
            | Token::FloatType(_) 
            | Token::CharType(_) => {
                if let Token::Symbol('(') = self.peek() {
                    Statement::Function(self.parse_function())
                } else if  let Token::Ident(_) = self.peek() {
                    self.parse_declaration()
                } else {
                    panic!("It was neither the syntax for a vaiable declaration or a lambda function");
                }
            }
        
            // optionally, if you use Token::Keyword("int"), support that too
            Token::Keyword(k) if ["int", "string", "bool", "char", "float"].contains(&k.as_str()) => {
                self.parse_declaration()
            }

            Token::List(_) | Token::Operator(Operator::Comparison(ComparisonOperator::LessThan)) => {
                self.parse_declaration()
            }

            _ => {
                let expr = self.parse_expression();
                Statement::Expression(expr)
            }
        }
    }  

    fn parse_output(&mut self) -> Statement {
        self.advance();
        let expression = self.parse_expression();
        Statement::Output(expression)
    }

    fn parse_declaration(&mut self) -> Statement {
        let var_type = self.parse_type();

        let identifier = if let Token::Ident(name) = &self.current {
            let id = name.clone();
            self.advance();
            id
        } else {
            panic!("Expected identifier, found {:?}", self.current);
        };
    
        if let Token::Assignment(_) = self.current {
            self.advance();
            // Check if the next token is the ">>" operator for input
            if let Token::InputToken = &self.current {
                self.advance();
    
                // Check if there is an optional string literal before the ">>"
                let prompt = if let Token::StringLiteral(prompt) = &self.current {
                    let prompt_str = prompt.clone();
                    self.advance();
                    Some(prompt_str)
                } else {
                    None
                };
                // Return the input statement
                return Statement::Input {
                    var_type,
                    identifier,
                    prompt,
                };
            }
    
            // Otherwise, parse a regular expression
            if let Type::Bool = &var_type  {
                if !matches!(&self.current, Token::BoolLiteral(_)) {
                    let mut conditions = Vec::new();
                    let value = self.parse_condition();
                    conditions.push(value);
            
                    return Statement::Declaration(Declaration {
                        var_type,
                        identifier,
                        value: Expression::ConditionList(conditions),
                    });
                }
            }

            let value = self.parse_expression();

            return Statement::Declaration(Declaration {
                var_type,
                identifier,
                value,
            });
        } else {
            panic!("Expected '=', found {:?}", self.current);
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

            // Union type: <int, string, float>
            Token::Operator(Operator::Comparison(ComparisonOperator::LessThan)) => {
                self.advance(); // consume '<'
                let mut types = vec![self.parse_type()];
                while let Token::Symbol(',') = self.current {
                    self.advance();
                    types.push(self.parse_type());
                }
                self.expect(&Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan)));
                Type::Union(types)
            }

            // List types
            Token::List(_) => {
                self.advance(); // consume 'List'
                self.expect(&Token::Operator(Operator::Comparison(ComparisonOperator::LessThan)));

                let combinator: Option<String> = match &self.current {
                    Token::Operator(Operator::Logical(LogicalOperator::And)) => {
                        self.advance();
                        Some("&&".to_string())
                    }
                    Token::Operator(Operator::Logical(LogicalOperator::Or)) => {
                        self.advance();
                        Some("||".to_string())
                    }
                    _ => None,
                };

                if let Some(op) = combinator {
                    self.expect(&Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan)));

                    if let Token::Symbol('(') = self.current {
                        self.advance();
                        let size = if let Token::IntLiteral(n) = self.current {
                            let val = n as usize;
                            self.advance();
                            val
                        } else {
                            panic!("Expected list size in List<T>(n)");
                        };
                        self.expect(&Token::Symbol(')'));
                        return Type::ConditionFixedList(op,size);
                    } else {
                        return Type::ConditionList(op);
                    }

                } else {
                    let mut inner_types = vec![self.parse_type()];
                    while let Token::Symbol(',') = self.current {
                        self.advance();
                        inner_types.push(self.parse_type());
                    }
    
                    self.expect(&Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan)));
    
                    // Check if it's a fixed-length list like List<int>(3)
                    if let Token::Symbol('(') = self.current {
                        self.advance();
                        let size = if let Token::IntLiteral(n) = self.current {
                            let val = n as usize;
                            self.advance();
                            val
                        } else {
                            panic!("Expected list size in List<T>(n)");
                        };
                        self.expect(&Token::Symbol(')'));
                        Type::FixedList(inner_types, size)
                    } else {
                        Type::List(inner_types)
                    }
                }

               
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
            Token::FloatLiteral(f) => {
                let lit = Literal::Float(*f);
                self.advance();
                Expression::Literal(lit)
            }
            Token::CharLiteral(c) => {
                let lit = Literal::Char(*c);
                self.advance();
                Expression::Literal(lit)
            }
            Token::Ident(name) => {
                let id = name.clone();
                self.advance();
                Expression::Identifier(id)
            }

            Token::Symbol('`') => {
                self.advance(); // Skip the opening backtick
                let mut static_parts = Vec::new();
                let mut expressions = Vec::new();
    
                loop {
                    match &self.current {
                        Token::Symbol('`') => {
                            self.advance(); // Skip the closing backtick
                            break;
                        },
                        Token::Symbol('{') => {
                            self.advance(); // Skip the opening curly brace
                            let expr = self.parse_expression();
                            expressions.push(expr);
    
                            // Expect the closing curly brace
                            if let Token::Symbol('}') = &self.current {
                                self.advance(); // Skip the closing curly brace
                            } else {
                                panic!("Expected '}}', found {:?}", self.current);
                            }
                        },
                        Token::Ident(s) => {
                            static_parts.push(s.clone());
                            self.advance();
                        },
                        _ => {
                            panic!("Unexpected token inside interpolated string: {:?}", self.current);
                        }
                    }
                }
    
                Expression::InterpolatedString(static_parts, expressions)
            }

            // Parse list literals (both curly and square brackets)
            Token::Symbol('{') | Token::Symbol('[') => {
                let opening_brace = self.current.clone();
                // Consume the opening brace
                self.advance();

                // Parse the list elements until the closing brace or square bracket
                let mut elements = self.parse_list_expression(|parser| parser.parse_expression());

                // Create and return the list literal expression
                let list_expr = Expression::Literal(Literal::List(elements));
                list_expr
            }
            _ => panic!("Unexpected token in expression: {:?}", self.current),
        }
    }

    fn parse_list_expression<F>(&mut self, mut parse_fn: F) -> Vec<Expression>
where
    F: FnMut(&mut Parser) -> Expression,
 {
        let mut elements = Vec::new();
        loop {
            if let Token::Symbol('}') | Token::Symbol(']') = &self.current {
                // If it's a closing brace or bracket, stop parsing
                self.advance();
                break;
            }

            // Parse the expression for the list element
            let element = parse_fn(self);
            elements.push(element);

            // If there's a comma, skip it and continue parsing the next element
            if let Token::Symbol(',') = &self.current {
                self.advance();
            } else {
                // Otherwise, if we hit a closing brace/bracket, we're done
                if let Token::Symbol('}') | Token::Symbol(']') = &self.current {
                    self.advance();
                    break;
                } else {
                    panic!("Expected a ',' or closing brace/bracket, found: {:?}", self.current);
                }
            }
        }
        return elements;

    }

    fn parse_comparison_expression(&mut self) -> Expression {
        let mut left = self.parse_expression();
        // let mut left = self.parse_condition();
        while let Token::Operator(Operator::Comparison(_)) = &self.current {
            let op_token = self.current.clone();
            self.advance(); // consume the operator

            let right = self.parse_expression(); // again, use your existing parser
            // let right = self.parse_expression();
            let op_str = self.token_to_op_string(&op_token);

            left = Expression::BinaryOp(Box::new(left), op_str, Box::new(right));
        }
        left
    }

    fn token_to_op_string(&self, token: &Token) -> String {
        match token {
            Token::Operator(Operator::Comparison(ComparisonOperator::Equal)) => "==".to_string(),
            Token::Operator(Operator::Comparison(ComparisonOperator::NotEqual)) => "!=".to_string(),
            Token::Operator(Operator::Comparison(ComparisonOperator::LessThan)) => "<".to_string(),
            Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThan)) => ">".to_string(),
            Token::Operator(Operator::Comparison(ComparisonOperator::LessThanOrEqual)) => "<=".to_string(),
            Token::Operator(Operator::Comparison(ComparisonOperator::GreaterThanOrEqual)) => ">=".to_string(),
            _ => panic!("Expected comparison operator, got {:?}", token),
        }
    }


    fn parse_if(&mut self) -> Statement {
        self.advance(); 
        let condition_expr = self.parse_condition();
        let then_block = self.parse_block();
        let mut elif_blocks = Vec::new();
        while let Token::Keyword(k) = &self.current {
            if k == "elif" {
                self.advance();
                let mut elif_conds = self.parse_condition();
                let elif_block = self.parse_block();         
                elif_blocks.push((elif_conds, elif_block));
            } else {
                break;
            }
        }
        println!("It reached here and fifnshed parsing the elif statement");
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
            condition: condition_expr,
            then_block,
            elif_blocks,
            else_block,
        })
    }

    pub fn parse_condition(&mut self) -> Condition {
        // self.expect(&Token::Symbol('('));
        // let cond = self.parse_or_condition();
        // self.expect(&Token::Symbol(')'));
        // cond

        if let Token::Symbol('(') = &self.current {
            // Handle the case where the condition is inside parentheses
            self.advance(); // Consume '('
            let cond = self.parse_or_condition();
            self.expect(&Token::Symbol(')')); // Ensure we get the closing ')'
            cond
        } else {
            // Handle the case where there's no parentheses (e.g., x == 3)
            self.parse_or_condition()
        }

    }

    fn parse_or_condition(&mut self) -> Condition {
        let mut left = self.parse_and_condition();

        while let Token::Operator(Operator::Logical(LogicalOperator::Or)) = self.current {
            self.advance();
            let right = self.parse_and_condition();
            left = Condition::Or(Box::new(left), Box::new(right));
        }

        left
    }

    fn parse_and_condition(&mut self) -> Condition {
        let mut left = self.parse_not_condition();

        while let Token::Operator(Operator::Logical(LogicalOperator::And)) = self.current {
            self.advance();
            let right = self.parse_not_condition();
            left = Condition::And(Box::new(left), Box::new(right));
        }

        left
    }

    fn parse_not_condition(&mut self) -> Condition {
        if let Token::Operator(Operator::Logical(LogicalOperator::Not)) = self.current {
            self.advance();
            let inner = self.parse_not_condition();
            Condition::Not(Box::new(inner))
        } else if let Token::Symbol('(') = self.current {
            self.advance();
            let cond = self.parse_or_condition();
            self.expect(&Token::Symbol(')'));
            Condition::Grouped(Box::new(cond))
        } else if let Token::List(_) = self.current {
            self.parse_condition_list()
        } 
        else {
            Condition::Single(self.parse_comparison_expression())
        }
    }

    fn parse_condition_list(&mut self) -> Condition {
        let comb = self.parse_type();

        // println!("It reached the condition list function");

        match &comb {
            Type::ConditionList(comb) |
            Type::ConditionFixedList(comb, _) => {
                match self.current {
                    Token::Symbol('{') | Token::Symbol('[') => {
                        self.advance();
                        let exprs = self.parse_list_expression(|parser| parser.parse_comparison_expression());
                        let mut conditions: Vec<Condition> = exprs.into_iter().map(Condition::Single).collect(); 
                        let mut result = conditions.remove(0);

                        for cond in conditions {
                            result = match comb.as_str() {
                                "&&" => Condition::And(Box::new(result), Box::new(cond)),
                                "||" => Condition::Or(Box::new(result), Box::new(cond)),
                                _ => panic!("Unsupported combinator: {}", comb),
                            }
                        }

                        // self.expect(&Token::Symbol('}'));
                        // self.advance();

                        return result;
                    },
                    _ => {
                        panic!("Expected || or && but found: {:?}",self.current)
                    }
                }
            }
            _ => {
                panic!(
                    "Expected 'some type of list but found: {:?}",
                    self.current
                );
            }
        }
    }


    fn parse_loop(&mut self) -> Statement {
        let peek = self.peek();
        match peek {
            // Range loop: `loop var -> iterable { ... }`
            Token::Ident(var) => {
                self.advance();
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
                    // panic!("Expected '->' after loop variable in range-based loop");
                    self.advance();
                    let mut condition = Vec::new();
                    condition.push(self.parse_condition());
                    let body = self.parse_block();
                    Statement::Loop(LoopExpr::Condition { condition, body })
                }
            }
            // Condition-based loop `loop condition { ... }`
            Token::BoolLiteral(_) | Token::Ident(_) | Token::Symbol('(') | Token::List(_) => {
                self.advance();
                let mut condition = Vec::new();
                condition.push(self.parse_condition());
                let body = self.parse_block();
                Statement::Loop(LoopExpr::Condition { condition, body })
            }
            // Infinite loop
            _ => {
                self.advance();
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
            Some(id)
        } else {
            None  // Anonymous function
        };
        // let name = if let Token::Ident(name) = &self.current {
        //     let id = name.clone();
        //     self.advance();
        //     id
        // } else {
        //     panic!("Expected function name");
        // };

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
