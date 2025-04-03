mod lexer;  // Import the lexer module

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into sco

fn main() {
    let source_code = r#"
        Int x = 42
        String name = "Ayaan"
        Bool flag = true
        if x == 1 { return }
    "#;

    let mut lexer = Lexer::new(source_code);
    loop {
        let token = lexer.next_token();
        println!("{:?}", token);
        if token == Token::Eof {
            break;
        }
    }
}