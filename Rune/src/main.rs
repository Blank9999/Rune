mod lexer;  // Import the lexer module
mod parser;
mod ast;  

use crate::lexer::{Lexer,Token}; // Bring Lexer and Token into scope
use crate::parser::Parser;


fn main() {

    let source_code = r#"
        int x = 42
        string name = "Nahan"
        int y = 1142
        string province = "Ontario"
        bool value = True
        int a = 842
        bool status = True
        int b = 452
        string country = "Canada"
        bool checked = True
        int c = 421
        string city = "Waterloo"
        bool result = False

    "#;

    // let source_code = "int x = 4";

    let mut lexer: Lexer<'_> = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let decl = parser.parse_program();
    println!("{:#?}", decl);



    // loop {
    //     let token: Token = lexer.next_token();
    //     println!("{:?}", token);
    //     if token == Token::Eof {
    //         break;
    //     }
    // }
}