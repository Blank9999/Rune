mod lexer;  // Import the lexer module

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into scope


fn main() {

    let source_code = r#"
        int x = 42
        string name = "Ayaan"
        # nefioenfioewnfioewnfewio
        bool flag = true
        if x == 1 { return } # yooo what dsdbubseuide
        list<int, string>
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