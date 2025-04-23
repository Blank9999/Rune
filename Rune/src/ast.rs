#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Char,
    Error,
    Custom(String), // for things like List<int>
    List(Vec<Type>), // Dynamic list of allowed types
    FixedList(Vec<Type>, usize), // Fixed-length list
    ConditionList(String),
    ConditionFixedList(String,usize),
    // ConditionList(Vec<Condition>),
    // ConditionFixedList(Vec<Condition>,usize),
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
    UnaryOp(String, Box<Expression>),
    FunctionCall { name: String, args: Vec<Expression> },
    InputOp(Box<Expression>),
    InterpolatedCall(String, Vec<Expression>), // for `create{pet}()`
    InterpolatedString(Vec<String>, Vec<Expression>), // New variant for interpolated strings
    // Condition(Condition),
    ConditionList(Vec<Condition>),
}

#[derive(Debug)]
pub enum Condition {
    Single(Expression),
    And(Box<Condition>, Box<Condition>),
    Or(Box<Condition>, Box<Condition>),
    Not(Box<Condition>),
    Grouped(Box<Condition>),
}

#[derive(Debug)]
pub struct Declaration {
    pub var_type: Type,
    pub identifier: String,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Assignment {
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
    pub condition: Condition,
    pub then_block: Vec<Statement>,
    pub elif_blocks: Vec<(Condition, Vec<Statement>)>,
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
        condition: Vec<Condition>,
        body: Vec<Statement>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Declaration(Declaration),
    Assignment(Assignment),
    Expression(Expression),
    If(IfExpr),
    Loop(LoopExpr),
    Function(Function),
    Return(Expression),
    Guard(Expression), // this is just a statement that should break the loop
    Input { var_type: Type, identifier: String, prompt: Option<String> },
    AssignInput { identifier: String, prompt: Option<String> },
    Output(Expression),
    Do(Expression),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
