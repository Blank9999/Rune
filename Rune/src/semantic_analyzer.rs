use std::collections::HashMap;

use crate::ast::{
    Program, Statement, Declaration, Assignment, Expression, Literal, Type, Function, Parameter,
    IfExpr, LoopExpr, Condition,
};

// A simple representation of a symbol in the symbol table
#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    var_type: Type,
    // Add other relevant information like mutability, etc.
}

// A simple symbol table structure
#[derive(Debug, Default)]
struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    // Create a new symbol table with the global scope
    fn new() -> Self {
        let mut table = SymbolTable::default();
        table.enter_scope(); // Enter the global scope
        table
    }

    // Enter a new scope
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Exit the current scope
    fn exit_scope(&mut self) {
        if self.scopes.pop().is_none() {
            // This should not happen in a well-structured program
            panic!("Attempted to exit global scope");
        }
    }

    // Define a symbol in the current scope.
    // If a symbol with the same name already exists in the current scope, it is replaced.
    fn define_symbol(&mut self, name: String, var_type: Type) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().expect("No active scope");

        // Insert or update the symbol in the current scope.
        // The HashMap's insert method automatically handles replacement if the key exists.
        current_scope.insert(name.clone(), Symbol { name, var_type });

        Ok(())
    }

    // Resolve a symbol, searching from the current scope outwards
    fn resolve_symbol(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol.clone());
            }
        }
        None
    }

    // TODO: Add a method to resolve function symbols
    // fn resolve_function(&self, name: &str) -> Option<FunctionSignature> { ... }
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    // TODO: Add state to track the current function's expected return type
    // current_return_type: Option<Type>,
    // Add other state needed for analysis, e.g., error reporting
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            // current_return_type: None,
        }
    }

    // Main entry point for semantic analysis
    pub fn analyze(&mut self, program: &Program) -> Result<(), String> {
        // Enter the global scope before checking the program
        // self.symbol_table.enter_scope(); // Already done in SymbolTable::new()

        self.check_program(program)?;

        // Exit the global scope
        // self.symbol_table.exit_scope(); // Should be the last scope left
        Ok(())
    }

    fn check_program(&mut self, program: &Program) -> Result<(), String> {
        // Before checking statements, register top-level functions
        // This allows for recursive calls and calls between functions defined at the same level
        for statement in &program.statements {
            if let Statement::Function(func) = statement {
                if let Some(name) = &func.name {
                    // TODO: Store function signature (name, param types, return type) in the symbol table
                    // For now, just a placeholder to avoid "used before declaration" for function calls
                    // This needs a proper function type representation.
                    // self.symbol_table.define_symbol(name.clone(), Type::Error)?; // Placeholder
                }
            }
        }

        for statement in &program.statements {
            self.check_statement(statement)?;
        }
        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> Result<(), String> {
        match statement {
            Statement::Declaration(decl) => self.check_declaration(decl),
            Statement::Assignment(assign) => self.check_assignment(assign),
            Statement::Expression(expr) => {
                // Just check the expression for now, maybe add checks for side effects later
                self.check_expression(expr)?;
                Ok(())
            }
            Statement::If(if_expr) => self.check_if(if_expr),
            Statement::Loop(loop_expr) => self.check_loop(loop_expr),
            Statement::Function(func) => self.check_function(func),
            Statement::Return(expr) => {
                let return_type = self.check_expression(expr)?;
                // TODO: Check if 'return_type' matches the expected return type of the current function
                // This requires tracking the current function's signature during traversal.
                // Example:
                // if let Some(expected_type) = &self.current_return_type {
                //     if !self.are_types_compatible(&return_type, expected_type) {
                //         return Err(format!("Return type {:?} does not match expected function return type {:?}", return_type, expected_type));
                //     }
                // } else {
                //     // Error: return statement outside a function
                //     return Err("Return statement found outside a function".to_string());
                // }
                Ok(())
            }
            Statement::Guard(expr) => {
                let expr_type = self.check_expression(expr)?;
                // Check if guard condition type is boolean
                if expr_type != Type::Bool {
                    return Err(format!("Guard condition must be of type Bool, found {:?}", expr_type));
                }
                Ok(())
            }
            Statement::Input { var_type, identifier, prompt: _ } => {
                // Define the variable in the current scope
                // This will now replace an existing variable with the same name in the current scope.
                self.symbol_table.define_symbol(identifier.clone(), var_type.clone())?;
                // TODO: Check if the 'var_type' is a type that can be read from input.
                // This depends on your language's runtime/standard library capabilities.
                Ok(())
            }
            Statement::AssignInput { identifier, prompt: _ } => {
                // Check if the variable is declared
                let symbol = self.symbol_table.resolve_symbol(identifier)
                     .ok_or_else(|| format!("Variable '{}' used before declaration", identifier))?;
                // TODO: Check if the variable's declared type ('symbol.var_type') is supported for input assignment.
                // This depends on your language's runtime/standard library capabilities.
                Ok(())
            }
            Statement::Output(expr) => {
                // Just check the expression for now
                self.check_expression(expr)?;
                // TODO: Optionally check if the expression's type is supported for output.
                Ok(())
            }
            Statement::Do(expr) => {
                // Just check the expression for now
                self.check_expression(expr)?;
                // TODO: Consider if 'do' expressions should have a specific required type or side effect.
                Ok(())
            }
        }
    }

    fn check_declaration(&mut self, decl: &Declaration) -> Result<(), String> {
        // Check the type of the value being assigned
        let value_type = self.check_expression(&decl.value)?;

        // Check if the declared type matches the value type
        if !self.are_types_compatible(&decl.var_type, &value_type) {
            return Err(format!(
                "Cannot assign value of type {:?} to variable '{}' of type {:?}",
                value_type, decl.identifier, decl.var_type
            ));
        }

        // Define the variable in the current scope.
        // This will now replace an existing variable with the same name in the current scope.
        self.symbol_table.define_symbol(decl.identifier.clone(), decl.var_type.clone())?;

        Ok(())
    }

    fn check_assignment(&mut self, assign: &Assignment) -> Result<(), String> {
        // Check if the variable is declared and get its type
        let symbol = self.symbol_table.resolve_symbol(&assign.identifier)
            .ok_or_else(|| format!("Variable '{}' used before declaration", assign.identifier))?;

        // Check the type of the value being assigned
        let value_type = self.check_expression(&assign.value)?;

        // Check if the variable's declared type matches the value type
        if !self.are_types_compatible(&symbol.var_type, &value_type) {
            return Err(format!(
                "Cannot assign value of type {:?} to variable '{}' of type {:?}",
                value_type, assign.identifier, symbol.var_type
            ));
        }

        Ok(())
    }

    // This function should return the type of the expression
    fn check_expression(&mut self, expr: &Expression) -> Result<Type, String> {
        match expr {
            Expression::Literal(lit) => self.check_literal(lit),
            Expression::Identifier(name) => {
                // Resolve the identifier to get its type
                let symbol = self.symbol_table.resolve_symbol(name)
                    .ok_or_else(|| format!("Variable '{}' used before declaration", name))?;
                Ok(symbol.var_type)
            }
            Expression::BinaryOp(left, op, right) => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                // TODO: Implement comprehensive type checking rules for binary operators
                // This is complex and depends heavily on your language's type system.
                // Example:
                // match op.as_str() {
                //     "+" => match (&left_type, &right_type) {
                //         (Type::Int, Type::Int) => Ok(Type::Int),
                //         (Type::Float, Type::Float) => Ok(Type::Float),
                //         (Type::String, Type::String) => Ok(Type::String), // String concatenation
                //         (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float), // Implicit conversion
                //         _ => Err(format!("Cannot apply operator '{}' to types {:?} and {:?}", op, left_type, right_type)),
                //     },
                //     "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                //          // Check if types are comparable and return Bool
                //          if self.are_types_comparable(&left_type, &right_type) {
                //              Ok(Type::Bool)
                //          } else {
                //              Err(format!("Cannot compare types {:?} and {:?}", left_type, right_type))
                //          }
                //     }
                //     // ... handle other operators
                //     _ => Err(format!("Unknown operator '{}'", op)),
                // }
                 Ok(Type::Error) // Placeholder for now
            }
            Expression::UnaryOp(op, operand) => {
                let operand_type = self.check_expression(operand)?;
                // TODO: Implement type checking rules for unary operators (e.g., negation, logical NOT)
                // Example:
                // match op.as_str() {
                //     "-" => match operand_type {
                //         Type::Int => Ok(Type::Int),
                //         Type::Float => Ok(Type::Float),
                //         _ => Err(format!("Cannot apply unary operator '{}' to type {:?}", op, operand_type)),
                //     },
                //     "!" => match operand_type {
                //         Type::Bool => Ok(Type::Bool),
                //         _ => Err(format!("Cannot apply unary operator '{}' to type {:?}", op, operand_type)),
                //     },
                //     _ => Err(format!("Unknown unary operator '{}'", op)),
                // }
                 Ok(Type::Error) // Placeholder for now
            }
            Expression::FunctionCall { name, args } => {
                // TODO: Resolve function by 'name' using the symbol table (need function lookup)
                // Check the number of arguments matches the function signature.
                // Check the type of each argument matches the corresponding parameter type.
                // Return the function's declared return type.
                 Ok(Type::Error) // Placeholder for now
            }
            Expression::InputOp(_) => {
                // The type of an InputOp depends on where it's used (declaration/assignment).
                // This is handled in check_declaration/check_assignment based on the variable's type.
                // Returning Error here as it shouldn't be checked in isolation.
                Ok(Type::Error)
            }
            Expression::InterpolatedCall(name, args) => {
                // TODO: Similar to FunctionCall, but for interpolated calls.
                // Need to handle how interpolated function names are resolved.
                Ok(Type::Error) // Placeholder for now
            }
            Expression::InterpolatedString(static_parts, expressions) => {
                // Check that all interpolated expressions can be converted to string.
                for expr in expressions {
                    let expr_type = self.check_expression(expr)?;
                    // TODO: Check if expr_type is string or has a defined string conversion.
                    // If not convertible, return an error.
                }
                Ok(Type::String) // Interpolated strings always result in a string
            }
            Expression::ConditionList(conditions) => {
                // Check that all conditions within the list are valid boolean expressions.
                for cond in conditions {
                    self.check_condition(cond)?;
                }
                Ok(Type::Bool) // A list of conditions, combined by && or ||, evaluates to a boolean.
            }
        }
    }

    fn check_literal(&mut self, lit: &Literal) -> Result<Type, String> {
        match lit {
            Literal::Int(_) => Ok(Type::Int),
            Literal::String(_) => Ok(Type::String),
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::Float(_) => Ok(Type::Float),
            Literal::Char(_) => Ok(Type::Char),
            Literal::List(elements) => {
                // Determine the element type(s) of the list literal.
                // This is tricky for lists with mixed types.
                // A simple approach: if all elements are the same type T, the list is List<T>.
                // If mixed, it might be List<Union<T1, T2, ...>> or an error depending on your rules.
                let mut element_types = Vec::new();
                for elem in elements {
                    let elem_type = self.check_expression(elem)?;
                    if !element_types.contains(&elem_type) {
                        element_types.push(elem_type);
                    }
                }
                // TODO: Refine list literal type checking. Decide on rules for mixed types.
                // For now, returning a List type containing the types of its elements.
                Ok(Type::List(element_types))
            }
        }
    }

    fn check_condition(&mut self, cond: &Condition) -> Result<(), String> {
        match cond {
            Condition::Single(expr) => {
                let expr_type = self.check_expression(expr)?;
                // Ensure the expression type is boolean or comparable and the comparison results in a boolean.
                // The check_expression for binary comparison operators should return Type::Bool.
                if !matches!(expr_type, Type::Bool | Type::ConditionList(_) | Type::ConditionFixedList(_, _)) {
                    return Err(format!("Condition expression must evaluate to a boolean, found {:?}", expr_type));
                }
                Ok(())
            }
            Condition::And(left, right) | Condition::Or(left, right) => {
                // Recursively check left and right conditions.
                self.check_condition(left)?;
                self.check_condition(right)?;
                // The check_condition function ensures they are boolean conditions.
                Ok(())
            }
            Condition::Not(inner) => {
                // Recursively check the inner condition.
                self.check_condition(inner)?;
                // The check_condition function ensures it is a boolean condition.
                Ok(())
            }
            Condition::Grouped(inner) => {
                // Recursively check the inner condition.
                self.check_condition(inner)?;
                Ok(())
            }
        }
    }


    fn check_if(&mut self, if_expr: &IfExpr) -> Result<(), String> {
        // Check the condition
        self.check_condition(&if_expr.condition)?;
        // The check_condition function ensures the condition evaluates to a boolean.

        // Check the then block (enter new scope)
        self.symbol_table.enter_scope();
        for stmt in &if_expr.then_block {
            self.check_statement(stmt)?;
        }
        self.symbol_table.exit_scope();

        // Check elif blocks (each in a new scope)
        for (condition, block) in &if_expr.elif_blocks {
            self.check_condition(condition)?;
            // The check_condition function ensures the elif condition evaluates to a boolean.
            self.symbol_table.enter_scope();
            for stmt in block {
                self.check_statement(stmt)?;
            }
            self.symbol_table.exit_scope();
        }

        // Check the else block (in a new scope)
        if let Some(else_block) = &if_expr.else_block {
            self.symbol_table.enter_scope();
            for stmt in else_block {
                self.check_statement(stmt)?;
            }
            self.symbol_table.exit_scope();
        }

        Ok(())
    }

    fn check_loop(&mut self, loop_expr: &LoopExpr) -> Result<(), String> {
        // Loops introduce a new scope for their body
        self.symbol_table.enter_scope();

        match loop_expr {
            LoopExpr::Range { var, start, end, step, body } => {
                // Define the loop variable in the loop's scope.
                // TODO: Determine the type of the loop variable based on the range expressions.
                // Usually int or float, depending on the language's range semantics.
                self.symbol_table.define_symbol(var.clone(), Type::Int)?; // Assuming int for now

                // Check start, end, and step expressions
                let start_type = self.check_expression(start)?;
                let end_type = self.check_expression(end)?;
                // TODO: Ensure start and end types are compatible for range (e.g., both int or float).
                // Ensure they are numeric.

                if let Some(step_expr) = step {
                    let step_type = self.check_expression(step_expr)?;
                    // TODO: Ensure step type is compatible with start/end types and is numeric.
                }

                // Check the loop body
                for stmt in body {
                    self.check_statement(stmt)?;
                }
            }
            LoopExpr::Infinite { body } => {
                // Check the loop body
                for stmt in body {
                    self.check_statement(stmt)?;
                }
            }
            LoopExpr::ForEach { var, iterable, body } => {
                // Check the iterable expression
                let iterable_type = self.check_expression(iterable)?;
                // TODO: Ensure iterable_type is a list or something iterable.
                // Extract the element type from the iterable_type.

                // Define the loop variable in the loop's scope
                // TODO: Determine the type of the loop variable based on the iterable's element type.
                self.symbol_table.define_symbol(var.clone(), Type::Error)?; // Placeholder type

                // Check the loop body
                for stmt in body {
                    self.check_statement(stmt)?;
                }
            }
            LoopExpr::Condition { condition, body } => {
                // Check the condition expression
                // The check_condition function ensures the condition evaluates to a boolean.
                for cond in condition {
                    self.check_condition(cond)?;
                }

                // Check the loop body
                for stmt in body {
                    self.check_statement(stmt)?;
                }
            }
        }

        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_function(&mut self, func: &Function) -> Result<(), String> {
        // Functions introduce a new scope for parameters and body.
        // TODO: When entering a function, set the analyzer's current_return_type.
        // let original_return_type = self.current_return_type.take();
        // self.current_return_type = Some(func.return_type.clone());

        self.symbol_table.enter_scope();

        // Define parameters in the function's scope
        for param in &func.parameters {
            self.symbol_table.define_symbol(param.param_name.clone(), param.param_type.clone())?;
            // Check default value expression if present
            if let Some(default_expr) = &param.default {
                let default_type = self.check_expression(default_expr)?;
                // TODO: Check if default_type matches param.param_type and is compatible.
                 if !self.are_types_compatible(&param.param_type, &default_type) {
                     return Err(format!(
                         "Default value type {:?} for parameter '{}' does not match parameter type {:?}",
                         default_type, param.param_name, param.param_type
                     ));
                 }
            }
        }

        // Check the function body
        for stmt in &func.body {
            self.check_statement(stmt)?;
        }

        self.symbol_table.exit_scope();

        // TODO: Restore the original return type after exiting the function.
        // self.current_return_type = original_return_type;

        // If it's a named function, define its signature in the outer scope (where the function is declared)
        // This allows for recursive calls and calls from outside the function.
        if let Some(name) = &func.name {
             // TODO: Define the function signature (name, param types, return type) in the outer scope's symbol table.
             // This requires a proper way to represent function types and store them alongside variable symbols.
             // For now, this is handled in check_program for top-level functions as a placeholder.
        }

        Ok(())
    }

    // Helper to check if type2 is compatible with type1 (e.g., for assignment type1 = value_of_type2)
    fn are_types_compatible(&self, type1: &Type, type2: &Type) -> bool {
        // This is where your language's type compatibility and implicit conversion rules go.
        // Simple equality check for now:
        if type1 == type2 {
            return true;
        }

        // TODO: Add rules for implicit conversions (e.g., Int to Float)
        match (type1, type2) {
            // (Type::Float, Type::Int) => true, // Allow assigning Int to Float
            (Type::List(list1), Type::List(list2)) | (Type::FixedList(list1, _), Type::FixedList(list2, _)) => {
                // list2 must be a subset of list1
                list2.iter().all(|t2| list1.contains(t2))
            }

            (Type::Union(list), t) => list.contains(t),

            (Type::Bool, Type::ConditionList(_)) | (Type::Bool, Type::ConditionFixedList(_,_))=> true,

            _ => false,
        }
    }

    // TODO: Add a helper to check if two types are comparable (used for comparison operators)
    // fn are_types_comparable(&self, type1: &Type, type2: &Type) -> bool { ... }
}

// Example Usage (in your main.rs or lib.rs)
/*
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::semantic_analyzer::SemanticAnalyzer;

fn main() {
    let code = r#"
int x = 10;
string x = "hello"; // This should now be allowed and replace the int x
guard x == "hello"; // This should pass semantic analysis
"#;
    let lexer = Lexer::new(code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    println!("{:#?}", program); // Print the AST

    let mut analyzer = SemanticAnalyzer::new();
    match analyzer.analyze(&program) {
        Ok(_) => println!("Semantic analysis successful!"),
        Err(e) => eprintln!("Semantic analysis failed: {}", e),
    }
}
*/
