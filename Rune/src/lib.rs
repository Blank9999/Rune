pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod nl;
pub mod parser;
pub mod semantic_analyzer;

use ast::Program;
use error::Diagnostic;
use lexer::Lexer;
use parser::Parser;
use semantic_analyzer::SemanticAnalyzer;

pub fn analyze_source(source: &str) -> Result<Program, Diagnostic> {
    let mut parser = Parser::new(Lexer::new(source))?;
    let program = parser.parse_program()?;
    SemanticAnalyzer::new().analyze(&program)?;
    Ok(program)
}

pub fn emit_llvm_ir(source: &str) -> Result<String, Diagnostic> {
    let program = analyze_source(source)?;
    codegen::CodeGenerator::new().generate(&program)
}

pub fn emit_llvm_ir_with_trace(source: &str) -> Result<String, Diagnostic> {
    let program = analyze_source(source)?;
    codegen::CodeGenerator::new().generate_with_trace(&program, Some(source))
}
