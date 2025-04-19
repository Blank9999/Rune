mod lexer;  // Import the lexer module
mod parser;
mod ast;  

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into scope
use crate::parser::Parser;


fn main() {
    let test_cases = vec![
        // r#"
        // x == y
        // x > y
        // x < y
        // x >= y
        // x <= y
        // x != y
        // name && address
        // name || address
        // !name
        // "#,
        // r#"
        // int x = 5 
        // int y = 10
        // x == y
        // "#,
        // r#"
        // if list<||>{x == 1, x == 2 , x == 3} || x == 3 || x == 5 && x != 10 {
        // } elif {
        // }  elif  {}
        //   else {
        // }
        // "#,
        r#"
        if list<||>{x == 1, x == 2 , x == 3, x == 7, x == 7, x == 7} || x == 0 {
        
        }
        "#,
        // r#"
        // bool flag = true;
        // "#,
        //        r#"
        // int (string x, int y) {x + y}
        // "#,
        r#"
        loop list<||>{x == 1, x == 2 , x == 3, x == 7, x == 7, x == 7} || x == 3 && x == 5 && x == 7 && x == 8 { 

        }
      "#,
        // r#"
        // loop { 
        //     int x = 5
        //     string y = x
        // }
        // "#,
        // r#"
        // loop num -> nums { 
        //     int x = 5
        //     string y = x
        //     loop x -> y {
        //         int a = 5
        //         string b = c
        //     }
        // }
        // "#,
        // r#"
        // loop i -> 0 : 3 : 1 { 
        //     int x = 5
        //     string y = x
        //     loop x -> y {
        //         int a = 5
        //         string b = c
        //     }
        // }
        // "#,
        // r#"
        // func int ayaan(int x, bool y = false) {
        //     int z = 4
        //     return x
        // }
        // "#,
        // r#"
        // int (string x, int y) {
        //     int z = 4
        //      return x
        // }
        // "#,
        // r#"
        // int x = 4
        // "#,

        // r#"
        // func int ayaan(int x, bool y = false) {
        //     int z = 4
        //     return x
        // }
        // "#,
        // r#"
        // int (string x, int y) {
        //     int z = 4
        //      return x
        // }
        // "#,
        // r#"
        // var y = x != 1 
        // "#,
    //     r#"
    //      int x = 3 
    //    "#,
    //    r#"
    //      int x = >> "Pick a number: " 
    //      << 3 << x
    //     "#,
    //     r#"
    //     do `hello{pet}bye{pet2}`
    //     string y = `hello{pet}bye{pet2}`
    //     do y
    //     "#,
    //     r#"
    //     char x = 'z'
    //     "#,
        // r#"
        // list<int,list<string>> f = {1, 2 ,3}    
        // r#"
        // list<int>(3) f = {1, 2 ,3}    
        // "#,
        // r#"
        // if list<&&>{x == 1, x == 2 , x == 3} || x == 3 {
        // }  
        // "#,
        // r#"
        // if x == list<int>{1, 2, 3}{
        // }  
        // "#,
        // r#"
        // list<int, list<int, list<int>>> x = [3, [4, 5]]
        // "#,
        // r#"
        // list<int>(5) f = {x == 1, x == 2 , x == 3}    
        // "#,
       // r#"
        //  <int, float> x = 1      
        // "#,
        r#"
         bool a = list<||>{x == 1, x == 2 , x == 3, x == 7, x == 7, x == 7} || x == 0
       "#,
       r#"
       bool a = false
     "#,
     r#"
     list<||> a = list<||>{x == 1, x == 2 , x == 3, x == 7, x == 7, x == 7}
    "#,
//     r#"
//     bool a = list<||>{x == 1, x == 2 , x == 3, x == 7, x == 7, x == 7}
//  "#,
    //     r#"
    //     list<int,list<int>> a = {1, 2 ,{3,4,5}}
    //  "#,
    // //    r#"
    //      int x = >>
    //  "#,
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