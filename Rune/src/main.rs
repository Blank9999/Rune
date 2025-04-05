mod lexer;  // Import the lexer module
mod parser;
mod ast;  

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into scope
use crate::parser::Parser;


fn main() {
    let test_cases = vec![
        r#"
        int x = 5
        bool name = false
        "#,
        r#"
        if hello {
            int y = 3
        }
        "#,
        r#"
        int a = 10
        if condition1,5 {
        } elif another {
        } elif {
        } else {
        }
        "#,
        r#"
        bool flag = true;
        "#,
        r#"
        loop { 
        }
        "#,
        r#"
        func int ayaan(int x, bool y = false) {
            int z = 4
            return x
        }
        "#,
        // r#"
        // int x = 4 + 3
        // "#,
    ];


    let test_cases = vec![
        // r#"
        // int x = 5
        // bool name = false
        // "#,
        // r#"
        // if hello {
        //     int y = 3
        // }
        // "#,
        // r#"
        // int a = 10
        // if condition1,5 {
        // } elif another {
        // } elif {
        // } else {
        // }
        // "#,
        // r#"
        // bool flag = true;
        // "#,
        r#"
        loop num -> nums { 
            int x = 5
            string y = x
            loop x -> y {
                int a = 5
                string b = c
            }
        }
        "#,
        r#"
        loop i -> 0 : 3 : 1 { 
            int x = 5
            string y = x
            loop x -> y {
                int a = 5
                string b = c
            }
        }
        "#,
        // r#"
        // func int ayaan(int x, bool y = false) {
        //     int z = 4
        //     return x
        // }
        // "#,
        // r#"
        // int x = 4 + 3
        // "#,
    ];

    for (i, source_code) in test_cases.iter().enumerate() {
        println!("--- Test Case {} ---", i + 1);
        let mut lexer = Lexer::new(source_code);
        // loop {
        //     let token = lexer.next_token();
        //     println!("{:?}", token);
        //     if token == Token::Eof {
        //         break;
        //     }
        // }

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        println!("{:#?}", program);
        println!();
    }
}