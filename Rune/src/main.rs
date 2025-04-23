mod lexer;  // Import the lexer module
mod parser;
mod ast;  
mod semantic_analyzer;

use crate::lexer::{Lexer, Token}; // Bring Lexer and Token into scope
use crate::parser::Parser;
use crate::semantic_analyzer::SemanticAnalyzer;
use std::io::{self, Write};
use std::error::Error;

fn read_numbers_from_line() -> Result<Vec<i32>, Box<dyn Error>> {
    // 1. Prompt the user for input
    print!("Enter a list of numbers separated by spaces: ");
    io::stdout().flush()?; // Use ? to propagate potential I/O errors

    // 2. Read the line of input from the user
    let mut input_string = String::new();
    io::stdin().read_line(&mut input_string)?; // Use ? to propagate potential I/O errors

    // 3. Process the input string
    let numbers_list: Vec<i32> = input_string
        .trim()
        .split_whitespace()
        .map(|s| s.parse::<i32>())
        .collect::<Result<Vec<i32>, _>>()?; // Use ? to propagate the first parsing error

    // 4. If we reached here, parsing was successful
    Ok(numbers_list) // Return the Vec<i32> wrapped in Ok()
}

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
        // r#"
        // int x = getX(y, z, 3)
        // "#,
        // r#"
        // int x = getX(4 + 3 / 2, 5.5 * 2 + 6 / 3)
        // "#,
        // r#"
        // if x + 2 == 4 || (x + 3) / 2 == 10 {
        // }
        // "#,
        r#"
        int x = 5;
        x = 10;
        "#,
        r#"
        list<int> x = {10, 20}
        list<float> x = list<float>{10.0}
        if x == list<||>{x==4, x==5} {
        }
        "#,
        r#"
        int x = 5;
        x = list<string>{"hl"};
        "#,
    ];

    let test_cases = [
        r#"
        list<int> x = {1,2,3};
        "#,

        // REDECLARATION CHECK
        r#"
        list<int, float> x = {1,2,3};
        list<int, float> x = {1,2,3};
        "#,

        // UNION CHECKS
        r#"
        <int, float> x = 1
        "#,
        // r#" 
        // <int, float> x = 1
        // <int, float> x = 1
        // "#,

        // BASIC IF CEHCK
        r#"
        int x = 5;
        if x == 4 {
            int x = 6;
        }
        "#,

        // FUNCTION CHECKS WITH NAME COLLISION & VOID
        r#"
        func int get() {
        }

        int x = 4;
        if x == get() {
        }
        "#,
        r#"
        func void x() {
        }
        int x = 5;
        "#,

        // LOOP VARIABLES CHECKS
        r#"
        int x = 4
        loop i -> 0 : x : 1 {
        }
        "#,


        // ALL LOOP (except foreach) CHECKS
        r#"
        loop {
        }
        "#,

        r#"
        loop false{
        }
        "#,

        r#"
        loop i -> 1 : 3 {
        }
        "#,

        r#"
        loop i -> 1 : : 3 {
        }
        "#,

        // LIST CONDITIONAL CHECKS
        r#"
        bool x = list<||>{3 == 4} || true
        if x {
            int x = 5;
        }
        "#,
        r#"
        list<||> y = list<||>{3 == 4}; 
        if y && true {

        }

        "#,

        // NESTED FUNCTION & LIST CONDITIONAL CHECK
        r#"
        int x = 4;
        list<||> y = list<||>{3 == 4}; 
        if y {
            func int z() {
            }
            int x = z();
        }

        "#,
        r#"
        int x = 4;
        func int x() {
        }

        "#,

        // I/O CHECKS
        r#"
        int x = >> "hello"
        string x = >> "hello"
        "#,
        r#"
        int x = >> "hello"
        << x
        list<int> x = {1, 2}
        << x
        "#,
        r#"
        << x
        "#,

        // INTERPOLATION STRING CHECKS
        r#"
        string p = "Dog"
        string x = `create{pet}`
        "#,
        r#"
        string pet = "Dog"
        string x = `create{pet}`
        "#,

        // INTERPOLATION FUNCTION LOOP CHECKS
        r#"
        list<string> pets = {"Dog", "Cat"}
        loop pet -> pets {
            do `create{pet}`
        }
        "#,
        r#"
        list<string> pets = {"Dog", "Cat"}
        loop pet -> pets {
            do `create{pets}`
        }
        "#,
        r#"
        list<string, int> pets = {"Dog", "Cat", 4}
        
        "#,

        // ODD CASE, NEED TO RESOLVE HOW RUNTIME CHECKS WILL BE DONE
        r#"
        string x = "hello()"
        do "x = 4"
        do x
        "#,

        // COMMENT CHECK
        r#"
        int x = 5 # im a comment
        "#,

    ];

    let numbers_list = read_numbers_from_line().expect("Failed to read or parse numbers");
    println!("{:?}", &numbers_list);

    for (i, source_code) in test_cases.iter().enumerate() {
        let index = (i + 1) as i32;
        if numbers_list.contains(&index) || numbers_list.len() == (0 as usize){
            println!("--- Test Case {} ---", i + 1);
            println!("{}", source_code);
            let mut lexer = Lexer::new(source_code);
            // loop { // PRINT LEXER TOKENIZATION
            //     let token = lexer.next_token();
            //     println!("{:?}", token);
            //     if token == Token::Eof {
            //         break;
            //     }
            // }

            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            //println!("{:#?}", program); // PRINT AST PARSED

            let mut semantic_analyzer = SemanticAnalyzer::new();
            
            // Perform semantic analysis and check for errors
            match semantic_analyzer.analyze(&program) {
                Ok(_) => println!("Semantic analysis successful!"),
                Err(e) => eprintln!("Semantic analysis failed: {}", e),
            }
            println!();
        }
    }
}