mod lexer;  // Import the lexer module
mod parser;
mod ast;  

use crate::lexer::{Lexer,Token}; // Bring Lexer and Token into scope
use crate::parser::Parser;


fn main() {

    let source_code = r#"
        func addition(int x, int y) int {
            int z = x + y
            return z
        }
    "#;

    let source_code = r#"
        4 + 3 * 5 / 10 + 5 - 6
    "#;

    let mut lexer: Lexer<'_> = Lexer::new(source_code);
    // let mut parser = Parser::new(lexer);
    // let decl = parser.parse_program();
    // println!("{:#?}", decl);

    loop {
        let token: Token = lexer.next_token();
        println!("{:?}", token);
        if token == Token::Eof {
            break;
        }
    }
}