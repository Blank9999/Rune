#[derive(Debug)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Char,
    Custom(String), // for things like List<int>

    List(Vec<Type>), // Dynamic list of allowed types
    FixedList(Vec<Type>, usize), // Fixed-length list
    Union(Vec<Type>), // Union of types like <int, string>
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
    Float(f64),
    Char(char),
    List(Vec<Expression>),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    BinaryOp(Box<Expression>, String, Box<Expression>),
    FunctionCall { name: String, args: Vec<Expression> },
    InterpolatedCall(String, Vec<Expression>), // for `create{pet}()`
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
    pub default: Option<Expression>,
}

#[derive(Debug)]
pub struct Function {
    pub return_type: Type,
    pub name: Option<String>,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct IfExpr {
    pub conditions: Vec<Expression>,
    pub then_block: Vec<Statement>,
    pub elif_blocks: Vec<(Vec<Expression>, Vec<Statement>)>,
    pub else_block: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub enum LoopExpr {
    Range { // loop i -> 0 : 3 : 1
        var: String,
        start: Expression,
        end: Expression,
        step: Option<Expression>,
        body: Vec<Statement>,
    },
    Infinite { // loop {}
        body: Vec<Statement>,
    },
    ForEach { // loop num -> nums {}
        var: String,
        iterable: Expression,
        body: Vec<Statement>,
    },
    Condition { // loop x < 3 {}
        condition: Expression,
        body: Vec<Statement>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Declaration(Declaration),
    Expression(Expression),
    If(IfExpr),
    Loop(LoopExpr),
    Function(Function),
    Return(Expression),
    Guard(Expression), // this is just a statement that should break the loop
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
