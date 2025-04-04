mod lexer;  // Import the lexer module
mod parser;
mod ast;  

use crate::lexer::{Lexer,Token}; // Bring Lexer and Token into scope
use crate::parser::{Parser};


fn main() {

    // let source_code = r#"
    //     int x = 42
    //     string name = "Ayaan"
    //     # nefioenfioewnfioewnfewio
    //     bool flag = true
    //     if x == 1 { return } # yooo what dsdbubseuide
    //     list<int, string>
    // "#;

    let source_code = "int x = 4";

    let mut lexer: Lexer<'_> = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let decl = parser.parse_declaration();
    println!("{:#?}", decl);



    // loop {
    //     let token: Token = lexer.next_token();
    //     println!("{:?}", token);
    //     if token == Token::Eof {
    //         break;
    //     }
    // }
}