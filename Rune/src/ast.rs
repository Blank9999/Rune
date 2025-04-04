#[derive(Debug)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Char,
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
}

#[derive(Debug)]
pub struct Declaration {
    pub var_type: Type,
    pub identifier: String,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Parameter {
    pub param_type: Type,
    pub param_name: String,
}

#[derive(Debug)]
pub struct Function {
    pub return_type: Type,
    pub name: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Program {
    pub declarations : Vec<Declaration>
}