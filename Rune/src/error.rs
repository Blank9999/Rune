use ariadne::{Color, Label, Report, ReportKind, Source};
use crate::lexer::Token;

#[derive(Debug)]
pub enum ErrorKind {
    UnknownToken,
    UnexpectedToken {
        expected: Token,
        found: String,
    },
    UndefinedVariable(String),
    UndefinedFunction(String),

    // === Type Errors ===
    TypeMismatch {
        expected: String,
        found: String,
    },
    InfiniteType(String),
    UnknownType(String),
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
    },
    InvalidReturnType {
        expected: String,
        found: String,
    },

    // === Function & Scope Errors ===
    ReturnOutsideFunction,
    BreakOutsideLoop,
    ContinueOutsideLoop,
}