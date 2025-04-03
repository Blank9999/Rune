mod lexer;  // Import the lexer module
mod literal;

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into scope


fn main() {
    let source_code: &str = r#"
        Int x = 42
        String name = "Ayaan"
        Bool flag = true
        if x == 1 { return }
    "#;

    let mut lexer: Lexer<'_> = Lexer::new(source_code);
    loop {
        let token: Token = lexer.next_token();
        println!("{:?}", token);
        if token == Token::Eof {
            break;
        }
    }
}