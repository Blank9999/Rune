use std::collections::HashMap;

use crate::ast::{
    Program, Statement, Declaration, Assignment, Expression, Literal, Type, Function, Parameter,
    IfExpr, LoopExpr, Condition,
};
// Assuming you have the lexer and ast modules correctly defined in your project.
// use crate::lexer::{Token, Operator, ComparisonOperator, LogicalOperator, ArithmeticOperator};
// use crate::ast::{Type, Literal, Expression, Declaration, Assignment, Program, Statement, IfExpr, LoopExpr, Function, Parameter, Condition};


// Define a specific type for function signatures
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}

// A simple representation of a symbol in the symbol table
#[derive(Debug, Clone)]
enum SymbolKind {
    Variable { var_type: Type },
    Function { signature: FunctionSignature },
    // Add other kinds like constants, types, etc. if needed
}

#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    kind: SymbolKind,
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
            // Consider a more robust error handling mechanism in a real compiler
            panic!("Attempted to exit global scope");
        }
    }

    // Define a variable symbol in the current scope.
    // If a symbol with the same name already exists in the current scope, it is replaced.
    fn define_variable(&mut self, name: String, var_type: Type) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().expect("No active scope");

        // Check if a function with the same name exists in the current scope
        if let Some(existing_symbol) = current_scope.get(&name) {
             if let SymbolKind::Function { .. } = existing_symbol.kind {
                 return Err(format!("Variable/Function '{}' collides with existing function/variable name in the same scope.", name));
             }
        }

        // Insert or update the symbol in the current scope.
        // The HashMap's insert method automatically handles replacement if the key exists.
        current_scope.insert(name.clone(), Symbol { name, kind: SymbolKind::Variable { var_type } });

        Ok(())
    }

     // Define a function symbol in the current scope.
    // If a symbol with the same name already exists in the current scope, it is an error.
    fn define_function(&mut self, name: String, signature: FunctionSignature) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().expect("No active scope");

        // Check if a symbol with the same name already exists in the current scope
        if current_scope.contains_key(&name) {
             return Err(format!("Function '{}' redefined in the same scope.", name));
        }

        current_scope.insert(name.clone(), Symbol { name, kind: SymbolKind::Function { signature } });

        Ok(())
    }


    // Resolve a symbol, searching from the current scope outwards
    fn resolve_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

     // Resolve a variable symbol, searching from the current scope outwards
    fn resolve_variable(&self, name: &str) -> Result<Type, String> {
        match self.resolve_symbol(name) {
            Some(symbol) => {
                match &symbol.kind {
                    SymbolKind::Variable { var_type } => Ok(var_type.clone()),
                    SymbolKind::Function { .. } => Err(format!("'{}' is a function, not a variable.", name)),
                }
            }
            None => Err(format!("Variable '{}' used before declaration", name)),
        }
    }

    // Resolve a function symbol, searching from the current scope outwards
    fn resolve_function(&self, name: &str) -> Result<FunctionSignature, String> {
         match self.resolve_symbol(name) {
            Some(symbol) => {
                match &symbol.kind {
                    SymbolKind::Function { signature } => Ok(signature.clone()),
                    SymbolKind::Variable { .. } => Err(format!("'{}' is a variable, not a function.", name)),
                }
            }
            None => Err(format!("Function '{}' used before declaration", name)),
        }
    }
}

#[derive(Debug)]
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    // TODO: Add state to track the current function's expected return type
    current_return_type: Option<Type>,
    // Add other state needed for analysis, e.g., error reporting
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            current_return_type: None,
        }
    }

    // Main entry point for semantic analysis
    pub fn analyze(&mut self, program: &Program) -> Result<(), String> {
        // Enter the global scope before checking the program
        // self.symbol_table.enter_scope(); // Already done in SymbolTable::new()

        // Before checking statements, register top-level functions
        // This allows for recursive calls and calls between functions defined at the same level
        for statement in &program.statements {
            if let Statement::Function(func) = statement {
                if let Some(name) = &func.name {
                    let param_types = func.parameters.iter().map(|p| p.param_type.clone()).collect();
                    let signature = FunctionSignature {
                        parameter_types: param_types,
                        return_type: func.return_type.clone(),
                    };
                    // TODO: Store function signature (name, param types, return type) in the symbol table
                    self.symbol_table.define_function(name.clone(), signature)?;
                } else {
                    // TODO: Handle top-level anonymous functions if your language supports them
                    // They might not be callable by name.
                    println!("Warning: Top-level anonymous function encountered. Cannot be called by name.");
                }
            }
        }


        self.check_program(program)?;

        // Exit the global scope
        // self.symbol_table.exit_scope(); // Should be the last scope left
        if self.symbol_table.scopes.len() != 1 { // Check if only the global scope remains
            return Err(format!("Scope imbalance detected: {} scopes remaining after analysis.", self.symbol_table.scopes.len()));
        }
        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_program(&mut self, program: &Program) -> Result<(), String> {
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
                // TODO: Consider if 'do' expressions should have a specific required type or side effect.
                // 'do' is handled below, this arm is for bare expressions which might be allowed for side effects.
                Ok(())
            }
            Statement::If(if_expr) => self.check_if(if_expr),
            Statement::Loop(loop_expr) => self.check_loop(loop_expr),
            Statement::Function(func) => {
                // Handle nested named functions here
                 if let Some(name) = &func.name {
                     let param_types = func.parameters.iter().map(|p| p.param_type.clone()).collect();
                     let signature = FunctionSignature {
                         parameter_types: param_types,
                         return_type: func.return_type.clone(),
                     };
                     // Define the named function in the *current* scope where it is declared
                     self.symbol_table.define_function(name.clone(), signature)?;
                 } else {
                     // Handle nested anonymous functions
                      println!("Warning: Nested anonymous function encountered.");
                      // TODO: If anonymous functions can be assigned or passed, check that.
                 }
                 // Now check the function body and parameters within its own scope
                 self.check_function_body(func)
            },
            Statement::Return(expr) => {
                let return_type = self.check_expression(expr)?;
                // TODO: Check if 'return_type' matches the expected return type of the current function
                if let Some(expected_type) = &self.current_return_type {
                    if !self.are_types_compatible(&expected_type, &return_type) {
                         return Err(format!("Return type {:?} does not match expected function return type {:?}", return_type, expected_type));
                    }
                 } else {
                     // Error: return statement outside a function
                     return Err("Return statement found outside a function".to_string());
                 }
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
                // Check if the 'var_type' is a type that can be read from input.
                if !matches!(var_type, Type::Int | Type::String | Type::Bool | Type::Float | Type::Char) {
                    return Err(format!("Input cannot be used with type {:?}", var_type));
                }

                // Define the variable in the current scope
                // This will now replace an existing variable with the same name in the current scope.
                self.symbol_table.define_variable(identifier.clone(), var_type.clone())?;

                Ok(())
            }
            Statement::AssignInput { identifier, prompt: _ } => {
                // Check if the variable is declared
                let var_type = self.symbol_table.resolve_variable(identifier)?;

                // Check if the variable's declared type ('symbol.var_type') is supported for input assignment.
                if !matches!(var_type, Type::Int | Type::String | Type::Bool | Type::Float | Type::Char) {
                    return Err(format!("Cannot assign input to variable '{}' of unsupported type {:?}", identifier, var_type));
                }
                Ok(())
            }
            Statement::Output(expr) => {
                // Just check the expression for now
                let expr_type = self.check_expression(expr)?;
                // TODO: Optionally check if the expression's type is supported for output.
                // Assuming all primitive types and lists are printable for now.
                match expr_type {
                    Type::Int | Type::String | Type::Bool | Type::Float | Type::Char | Type::List(_) | Type::FixedList(_, _) => {
                        // Supported types for output
                        Ok(())
                    }

                    Type::Union(_) => {
                        println!("Warning: Outputting a Union type. Behavior depends on runtime implementation.");
                        Ok(())
                    }

                    _ => {
                       // Consider allowing Union output by printing compatible types
                        Ok(())
                        // Err(format!("Output cannot be used with type {:?}", expr_type))
                    }
                }
            }
            Statement::Do(expr) => {
                // Just check the expression for now
                self.check_expression(expr)?;
                // TODO: Consider if 'do' expressions should have a specific required type or side effect.
                // Assuming 'do' expressions are primarily for side effects, no strict type check needed on the result.
                Ok(())
            }
        }
    }

    // check_function_body is a helper to handle the internal checking of a function's parameters and body
    fn check_function_body(&mut self, func: &Function) -> Result<(), String> {
        // Enter the function's internal scope for parameters and body
       self.symbol_table.enter_scope();

       // Define parameters in the function's *new* scope
       for param in &func.parameters {
           // Ensure parameter names are not duplicated within parameters (already handled by define_variable implicitly in the new scope)
           self.symbol_table.define_variable(param.param_name.clone(), param.param_type.clone())?;

           // Check default value expression if present
           if let Some(default_expr) = &param.default {
               let default_type = self.check_expression(default_expr)?;
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

       // Exit the function's internal scope
       self.symbol_table.exit_scope();

       Ok(())
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
        self.symbol_table.define_variable(decl.identifier.clone(), decl.var_type.clone())?;

        Ok(())
    }

    fn check_assignment(&mut self, assign: &Assignment) -> Result<(), String> {
        // Check if the variable is declared and get its type
        let var_type = self.symbol_table.resolve_variable(&assign.identifier)?;

        // Check the type of the value being assigned
        let value_type = self.check_expression(&assign.value)?;

        // Check if the variable's declared type matches the value type
        if !self.are_types_compatible(&var_type, &value_type) {
            return Err(format!(
                "Cannot assign value of type {:?} to variable '{}' of type {:?}",
                value_type, assign.identifier, var_type
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
                self.symbol_table.resolve_variable(name)
            }
            Expression::BinaryOp(left, op, right) => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                // TODO: Implement comprehensive type checking rules for binary operators
                // This is complex and depends heavily on your language's type system.
                match op.as_str() {
                    "+" | "-" | "*" | "/" => { // Arithmetic Operators
                        match (&left_type, &right_type) {
                            (Type::Int, Type::Int) => Ok(Type::Int),
                            (Type::Float, Type::Float) => Ok(Type::Float),
                            (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float), // Implicit conversion Int to Float
                            (Type::String, Type::String) if op.as_str() == "+" => Ok(Type::String), // String concatenation
                             _ => Err(format!("Cannot apply operator '{}' to types {:?} and {:?}", op, left_type, right_type)),
                        }
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => { // Comparison Operators
                         // Check if types are comparable and return Bool
                         if self.are_types_comparable(&left_type, &right_type) {
                             Ok(Type::Bool)
                         } else {
                             Err(format!("Cannot compare types {:?} and {:?}", left_type, right_type))
                         }
                    }
                    "&&" | "||" => { // Logical Operators
                        match (&left_type, &right_type) {
                            (Type::Bool, Type::Bool) => Ok(Type::Bool),
                            (Type::Bool, Type::ConditionList(_))| (Type::ConditionList(_), Type::Bool) => Ok(Type::Bool),
                            (Type::Bool, Type::ConditionFixedList(_, _))| (Type::ConditionFixedList(_, _), Type::Bool) => Ok(Type::Bool),
                            _ => Err(format!("Cannot apply logical operator '{}' to types {:?} and {:?}", op, left_type, right_type)),
                        }
                    }
                    _ => Err(format!("Unknown operator '{}'", op)),
                }
            }
            Expression::UnaryOp(op, operand) => {
                let operand_type = self.check_expression(operand)?;
                // TODO: Implement type checking rules for unary operators (e.g., negation, logical NOT)
                match op.as_str() {
                    "-" => match operand_type {
                         Type::Int => Ok(Type::Int),
                         Type::Float => Ok(Type::Float),
                         _ => Err(format!("Cannot apply unary operator '{}' to type {:?}", op, operand_type)),
                    },
                    "!" => match operand_type {
                         Type::Bool | Type::ConditionList(_) | Type::ConditionFixedList(_, _) => Ok(Type::Bool),
                         _ => Err(format!("Cannot apply unary operator '{}' to type {:?}", op, operand_type)),
                    },
                     _ => Err(format!("Unknown unary operator '{}'", op)),
                }
            }
            Expression::FunctionCall { name, args } => {
                 // TODO: Resolve function by 'name' using the symbol table (need function lookup)
                 let signature = self.symbol_table.resolve_function(name)?;

                 // Check the number of arguments matches the function signature.
                 if args.len() != signature.parameter_types.len() {
                     return Err(format!("Function '{}' expected {} arguments, but received {}", name, signature.parameter_types.len(), args.len()));
                 }

                 // Check the type of each argument matches the corresponding parameter type.
                 for (i, arg) in args.iter().enumerate() {
                     let arg_type = self.check_expression(arg)?;
                     let param_type = &signature.parameter_types[i];
                     if !self.are_types_compatible(param_type, &arg_type) {
                         return Err(format!("Argument {} for function '{}' has type {:?}, but expected {:?}", i + 1, name, arg_type, param_type));
                     }
                 }

                 // Return the function's declared return type.
                 Ok(signature.return_type)
            }
            Expression::InputOp(_) => {
                // The type of an InputOp depends on where it's used (declaration/assignment).
                // This is handled in check_declaration/check_assignment based on the variable's type.
                // Returning Error here as it shouldn't be checked in isolation.
                // This might indicate a parser issue if found outside declaration/assignment.
                Ok(Type::Error)
            }
            Expression::InterpolatedCall(name, args) => {
                // TODO: Similar to FunctionCall, but for interpolated calls.
                // Need to handle how interpolated function names are resolved.
                // Assuming for now it resolves like a regular function call, but the function name
                // itself comes from the string parts + interpolated expressions. This requires runtime
                // resolution or a more complex compile-time approach.
                // For static analysis, we can only check the argument types against *potential* functions.
                // A simplified check: just check the argument types are valid expressions.
                for arg in args {
                     self.check_expression(arg)?;
                }
                // What is the return type of an interpolated call? It depends on the function called.
                // This is hard to determine statically without knowing the function name at compile time.
                // Let's return an Error type for now, or maybe String if the result is always stringified.
                // Assuming it's used within an interpolated string, the result is likely stringified.
                 Ok(Type::String) // Placeholder, actual type depends on the called function.
            }
            Expression::InterpolatedString(static_parts, expressions) => {
                // Check that all interpolated expressions can be converted to string.
                for expr in expressions {
                    let expr_type = self.check_expression(expr)?;
                    // TODO: Check if expr_type is string or has a defined string conversion.
                    // If not convertible, return an error.
                    if !matches!(expr_type, Type::Int | Type::String | Type::Bool | Type::Float | Type::Char) {
                         // Assuming primitive types are convertible to String for interpolation
                         // Lists/Unions might need specific rules.
                         return Err(format!("Expression of type {:?} is not directly convertible to string for interpolation", expr_type));
                    }
                }
                Ok(Type::String) // Interpolated strings always result in a string
            }
            Expression::ConditionList(conditions) => {
                // Check that all conditions within the list are valid boolean expressions.
                for cond in conditions {
                    self.check_condition(cond)?;
                }
                 // A list of conditions, combined by && or ||, evaluates to a boolean.
                Ok(Type::Bool)
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
                // If mixed, it becomes List<Union<T1, T2, ...>>.
                let mut element_types = Vec::new();
                for elem in elements {
                    let elem_type = self.check_expression(elem)?;
                     // Add element type to the list of types if not already present
                    if !element_types.contains(&elem_type) {
                        element_types.push(elem_type);
                    }
                }
                // TODO: Refine list literal type checking. Decide on rules for mixed types.
                // If there's only one unique element type, it's List<T>. Otherwise, List<Union<...>>.
                if element_types.len() == 1 {
                    Ok(Type::List(element_types)) // e.g., List<int>
                } else {
                     // Create a Union type for the list elements
                    let union_type = Type::Union(element_types);
                     Ok(Type::List(vec![union_type])) // e.g., List<Union<int, string>>
                }
            }
        }
    }

    fn check_condition(&mut self, cond: &Condition) -> Result<(), String> {
        match cond {
            Condition::Single(expr) => {
                let expr_type = self.check_expression(expr)?;
                // Ensure the expression type is boolean or a list of conditions that evaluates to boolean.
                if !matches!(expr_type, Type::Bool | Type::ConditionList(_) | Type::ConditionFixedList(_, _)) {
                     // Allow comparison operators which result in bool
                     // Re-checking binary ops specifically might be redundant if check_expression handles it
                    match expr {
                        Expression::BinaryOp(_, op, _) if ["==", "!=", "<", ">", "<=", ">="].contains(&op.as_str()) => {
                            // This case is already handled by check_expression returning Type::Bool
                            Ok(())
                        }
                        _ => Err(format!("Condition expression must evaluate to a boolean, found {:?}", expr_type)),
                    }
                } else {
                    Ok(())
                }
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
                // Check start expression
                let start_type = self.check_expression(start)?;
    
                // Ensure start type is numeric
                if !matches!(start_type, Type::Int | Type::Float) {
                     return Err(format!("Range loop start expression must be numeric, found {:?}", start_type));
                }
    
                let mut loop_var_type = start_type.clone(); // Default loop variable type is start type
    
                match (end, step) {
                    (Some(end_expr), Some(step_expr)) => {
                        // Case: loop i -> start : end : step (Finite loop with explicit step)
                        let end_type = self.check_expression(end_expr)?;
                        let step_type = self.check_expression(step_expr)?;
    
                        // Ensure end type is numeric
                         if !matches!(end_type, Type::Int | Type::Float) {
                             return Err(format!("Range loop end expression must be numeric, found {:?}", end_type));
                         }
                        // Check compatibility between start and end
                         if !self.are_types_compatible(&start_type, &end_type) && !self.are_types_compatible(&end_type, &start_type) {
                              return Err(format!("Start type {:?} and end type {:?} are not compatible for range loop", start_type, end_type));
                         }
    
                        // Determine loop variable type based on start/end
                        loop_var_type = match (&start_type, &end_type) {
                            (Type::Float, _) | (_, Type::Float) => Type::Float,
                            _ => Type::Int,
                        };
    
                        // Ensure step type is numeric
                         if !matches!(step_type, Type::Int | Type::Float) {
                             return Err(format!("Range loop step expression must be numeric, found {:?}", step_type));
                         }
                         // Check compatibility between step and loop variable type
                        if !self.are_types_compatible(&loop_var_type, &step_type) && !self.are_types_compatible(&step_type, &loop_var_type) {
                            return Err(format!("Step type {:?} is not compatible with range type {:?}", step_type, loop_var_type));
                        }
                    }
                    (Some(end_expr), None) => {
                        // Case: loop i -> start : end (Finite loop with implicit step)
                        let end_type = self.check_expression(end_expr)?;
    
                         // Ensure end type is numeric
                         if !matches!(end_type, Type::Int | Type::Float) {
                             return Err(format!("Range loop end expression must be numeric, found {:?}", end_type));
                         }
                        // Check compatibility between start and end
                         if !self.are_types_compatible(&start_type, &end_type) && !self.are_types_compatible(&end_type, &start_type) {
                              return Err(format!("Start type {:?} and end type {:?} are not compatible for range loop", start_type, end_type));
                         }
    
                         // Determine loop variable type based on start/end
                        loop_var_type = match (&start_type, &end_type) {
                            (Type::Float, _) | (_, Type::Float) => Type::Float,
                            _ => Type::Int,
                        };
    
                        // Implicit step is typically 1 (Int). Check compatibility with loop_var_type.
                        if !self.are_types_compatible(&loop_var_type, &Type::Int) {
                             return Err(format!("Implicit step (Int) is not compatible with range type {:?}", loop_var_type));
                        }
    
                    }
                    (None, Some(step_expr)) => {
                        // Case: loop i -> start : : step (Infinite loop with explicit step)
                        let step_type = self.check_expression(step_expr)?;
    
                        // Loop variable type is based on start_type only for infinite loops
                        loop_var_type = start_type.clone();
    
                        // Ensure step type is numeric
                         if !matches!(step_type, Type::Int | Type::Float) {
                             return Err(format!("Range loop step expression must be numeric, found {:?}", step_type));
                         }
                         // Check compatibility between step and loop variable type
                        if !self.are_types_compatible(&loop_var_type, &step_type) && !self.are_types_compatible(&step_type, &loop_var_type) {
                            return Err(format!("Step type {:?} is not compatible with range type {:?}", step_type, loop_var_type));
                        }
                         println!("Warning: Range loop without end expression interpreted as infinite."); // Optional warning
                    }
                    (None, None) => {
                        // Case: loop i -> start : : (Infinite loop with implicit step)
                        // Loop variable type is based on start_type only for infinite loops
                        loop_var_type = start_type.clone();
    
                        // Implicit step is typically 1 (Int). Check compatibility with loop_var_type.
                         if !self.are_types_compatible(&loop_var_type, &Type::Int) {
                             return Err(format!("Implicit step (Int) is not compatible with range type {:?}", loop_var_type));
                        }
                         println!("Warning: Range loop without end or step expression interpreted as infinite with implicit step."); // Optional warning
                    }
                }
    
                // Define the loop variable in the loop's scope with the determined type.
                self.symbol_table.define_variable(var.clone(), loop_var_type)?;
    
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
                let element_type = match iterable_type {
                    Type::List(element_types) => {
                         if element_types.len() == 1 {
                             element_types[0].clone() // List of a single type
                         } else {
                             Type::Union(element_types) // List of mixed types becomes a Union
                         }
                    }
                    Type::FixedList(element_types, _) => {
                         if element_types.len() == 1 {
                             element_types[0].clone() // Fixed list of a single type
                         } else {
                            Type::Union(element_types) // Fixed list of mixed types becomes a Union
                         }
                    }
                    _ => {
                         return Err(format!("ForEach loop requires a list or iterable type, found {:?}", iterable_type));
                    }
                };

                // TODO: Determine the type of the loop variable based on the iterable's element type.
                 // The loop variable takes the type of the elements
                self.symbol_table.define_variable(var.clone(), element_type)?;

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
        let original_return_type = self.current_return_type.take();
        self.current_return_type = Some(func.return_type.clone());

        self.symbol_table.enter_scope();

        // Define parameters in the function's scope
        for param in &func.parameters {
            // Ensure parameter names are not duplicated
            if self.symbol_table.scopes.last().unwrap().contains_key(&param.param_name) {
                 return Err(format!("Duplicate parameter name '{}' in function definition.", param.param_name));
            }
            self.symbol_table.define_variable(param.param_name.clone(), param.param_type.clone())?;

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
        self.current_return_type = original_return_type;

        // If it's a named function, its signature is defined in the outer scope in check_program
        // TODO: Define the function signature (name, param types, return type) in the outer scope's symbol table.
        // This is handled by pre-registering functions in check_program.
        if let Some(_name) = &func.name {
             // Function is already registered in the outer scope.
        } else {
             // TODO: Handle checks for anonymous functions, e.g., if they are assigned to a variable.
        }

        Ok(())
    }

    // Helper to check if value_type is compatible with var_type (e.g., for assignment var_type = value_of_type)
    // Implements 'var_type = value_type' compatibility
    fn are_types_compatible(&self, var_type: &Type, value_type: &Type) -> bool {
        // This is where your language's type compatibility and implicit conversion rules go.

        // Exact match is always compatible
        if var_type == value_type {
            return true;
        }

        // TODO: Add rules for implicit conversions (e.g., Int to Float)
        match (var_type, value_type) {
             (Type::Float, Type::Int) => true, // Allow assigning Int to Float
            (Type::List(var_element_types), Type::List(value_element_types)) => {
                // A List<T> is compatible with List<U> if U is compatible with T.
                // For List<Union<T1, T2...>>, a List<U> is compatible if U is compatible with *any* type in the union.
                // A List<U1, U2...> is compatible with List<T1, T2...> if *every* Ui is compatible with *some* Tj.
                value_element_types.iter().all(|val_t| {
                     var_element_types.iter().any(|var_t| {
                         self.are_types_compatible(var_t, val_t)
                     })
                })
            }
            (Type::FixedList(var_element_types, var_size), Type::FixedList(value_element_types, value_size)) => {
                // Fixed lists must have the same size and compatible element types.
                if var_size != value_size {
                    return false;
                }
                 value_element_types.iter().all(|val_t| {
                     var_element_types.iter().any(|var_t| {
                         self.are_types_compatible(var_t, val_t)
                     })
                })
            }
             (Type::Union(allowed_types), actual_type) => {
                 // A value of 'actual_type' is compatible with a Union if 'actual_type' is compatible with at least one type in the union.
                 allowed_types.iter().any(|allowed_t| self.are_types_compatible(allowed_t, actual_type))
             }
             // Handle the case where the variable type is a list allowing a union, and the value is a list of a single type
             (Type::List(var_element_union), Type::List(value_single_type)) if var_element_union.len() > 1 && value_single_type.len() == 1 => {
                  if let Type::Union(allowed_types) = &var_element_union[0] {
                      allowed_types.iter().any(|allowed_t| self.are_types_compatible(allowed_t, &value_single_type[0]))
                  } else {
                      false // Should not happen if list element types are normalized to Union for mixed types
                  }
             }
              // Handle the case where the variable type is a fixed list allowing a union, and the value is a fixed list of a single type
             (Type::FixedList(var_element_union, var_size), Type::FixedList(value_single_type, value_size)) if var_element_union.len() > 1 && value_single_type.len() == 1 => {
                 if var_size != value_size {
                    return false;
                 }
                  if let Type::Union(allowed_types) = &var_element_union[0] {
                      allowed_types.iter().any(|allowed_t| self.are_types_compatible(allowed_t, &value_single_type[0]))
                  } else {
                      false // Should not happen
                  }
             }

             // Compatibility for boolean types when assigned from ConditionList
             (Type::Bool, Type::ConditionList(_)) |  (Type::ConditionList(_), Type::Bool) => true,
             (Type::Bool, Type::ConditionFixedList(_, _)) | (Type::ConditionFixedList(_, _), Type::Bool)  => true,


            _ => false, // No other implicit conversions or compatibility rules for now
        }
    }

    // TODO: Add a helper to check if two types are comparable (used for comparison operators)
    // Implements 'type1 == type2' comparability
     fn are_types_comparable(&self, type1: &Type, type2: &Type) -> bool {
         // Basic comparability rules:
         // - Identical types are comparable.
         // - Numeric types (Int, Float) are comparable with each other (with implicit conversion).
         // - String is comparable with String.
         // - Bool is comparable with Bool.
         // - Char is comparable with Char.
         // - Lists can be compared if their element types are comparable (deep comparison might be needed at runtime).
         // - Unions are comparable if both sides are unions and there's overlap in comparable types, or if one side is a union and the other is compatible with at least one type in the union. This is complex. Let's simplify for now.

         if type1 == type2 {
             return true;
         }

         match (type1, type2) {
             (Type::Int, Type::Float) | (Type::Float, Type::Int) => true, // Numeric types are comparable

             (Type::List(elements1), Type::List(elements2)) => {
                 // Lists are comparable if their element types are comparable.
                 // For simplicity, let's say lists are comparable if *any* element type from list1 is comparable with *any* element type from list2.
                 // A more strict rule might require all element types to be mutually comparable.
                 elements1.iter().any(|e1| elements2.iter().any(|e2| self.are_types_comparable(e1, e2)))
             }
             (Type::FixedList(elements1, size1), Type::FixedList(elements2, size2)) => {
                  // Fixed lists must have the same size and comparable element types.
                  if size1 != size2 {
                      return false;
                  }
                  elements1.iter().any(|e1| elements2.iter().any(|e2| self.are_types_comparable(e1, e2)))
             }

             (Type::Union(types1), type2) => {
                  // A union is comparable with type2 if any type in the union is comparable with type2.
                 types1.iter().any(|t1| self.are_types_comparable(t1, type2))
             }
              (type1, Type::Union(types2)) => {
                  // type1 is comparable with a union if type1 is comparable with any type in the union.
                 types2.iter().any(|t2| self.are_types_comparable(type1, t2))
             }

             // If both are unions, check if any type from union1 is comparable with any type from union2.
             (Type::Union(types1), Type::Union(types2)) => {
                 types1.iter().any(|t1| types2.iter().any(|t2| self.are_types_comparable(t1, t2)))
             }

             _ => false, // No other types are comparable with each other directly (e.g., Int vs String)
         }
     }
}

// Example Usage (in your main.rs or lib.rs)
/*
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::semantic_analyzer::SemanticAnalyzer;

fn main() {
    let code = r#"
int x = 10;
string y = "hello";
float z = x + 5.0; // Int to Float implicit conversion
bool b = x == 10;
bool c = y != "world";
bool d = b && c;

func int add(int a, int b) {
    return a + b;
}

int result = add(x, 20);

loop i -> 0 : 10 { // Range loop
    output i;
}

list<int> numbers = {1, 2, 3};
loop num -> numbers { // ForEach loop
    output num;
}

if d {
    string message = `Result: {result}`; // Interpolated string
    output message;
} elif x > 5 {
    output "x is greater than 5";
} else {
    output "neither";
}

int input_val >> "Enter an integer: "; // Input statement
output input_val;

string name;
name = "Alice"; // Assignment
output name;

assign name >> "Enter your name: "; // Assign input statement
output name;

guard input_val > 0; // Guard statement

do output "Executing side effect"; // Do statement

"#; // Removed the conflicting redeclaration 'string x = "hello";' for this example

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