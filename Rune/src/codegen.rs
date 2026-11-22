use std::collections::HashMap;

use crate::ast::{
    Condition, Declaration, Expression, Function, IfExpr, Literal, LoopExpr, Program, Statement,
    Type,
};
use crate::error::{Diagnostic, Phase, Span};

#[derive(Clone)]
struct FunctionInfo<'a> {
    function: &'a Function,
    llvm_name: String,
    required: usize,
}

#[derive(Clone)]
struct Binding {
    ty: Type,
    pointer: String,
}

#[derive(Clone)]
struct Value {
    ty: Type,
    operand: String,
}

struct LoopTarget {
    label: String,
    used: bool,
}

#[derive(Clone)]
struct Global {
    ty: Type,
    llvm_name: String,
}

#[derive(Default)]
struct StringPool {
    items: Vec<(String, Vec<u8>)>,
}

impl StringPool {
    fn intern(&mut self, value: &str) -> String {
        let bytes = value.as_bytes();
        if let Some((name, _)) = self.items.iter().find(|(_, old)| old.as_slice() == bytes) {
            return name.clone();
        }
        let name = format!(".rune.str.{}", self.items.len());
        self.items.push((name.clone(), bytes.to_vec()));
        name
    }

    fn globals(&self) -> String {
        let mut output = String::new();
        for (name, bytes) in &self.items {
            let mut escaped = String::new();
            for byte in bytes.iter().copied().chain(std::iter::once(0)) {
                if (0x20..=0x7e).contains(&byte) && byte != b'"' && byte != b'\\' {
                    escaped.push(byte as char);
                } else {
                    escaped.push_str(&format!("\\{byte:02X}"));
                }
            }
            output.push_str(&format!(
                "@{name} = private unnamed_addr constant [{} x i8] c\"{}\", align 1\n",
                bytes.len() + 1,
                escaped
            ));
        }
        output
    }
}

#[derive(Default)]
pub struct CodeGenerator;

impl CodeGenerator {
    pub fn new() -> Self {
        Self
    }

    pub fn generate(&self, program: &Program) -> Result<String, Diagnostic> {
        self.generate_with_trace(program, None)
    }

    pub fn generate_with_trace(
        &self,
        program: &Program,
        source: Option<&str>,
    ) -> Result<String, Diagnostic> {
        let mut strings = StringPool::default();
        let mut functions = HashMap::new();
        let mut function_order = Vec::new();
        let mut globals = HashMap::new();
        let mut global_order = Vec::new();

        for statement in &program.statements {
            let inner = unwrap_statement(statement);
            if let Statement::Function(function) = inner {
                let Some(name) = &function.name else {
                    return Err(codegen_error(
                        "anonymous functions are not supported by this backend",
                        statement_span(statement),
                    ));
                };
                if name == "main" {
                    return Err(codegen_error(
                        "the function name 'main' is reserved for top-level Rune statements",
                        statement_span(statement),
                    ));
                }
                if function
                    .parameters
                    .iter()
                    .any(|p| !is_codegen_type(&p.param_type))
                    || !is_codegen_type(&function.return_type)
                {
                    return Err(codegen_error(
                        "function signature uses a type not supported by this backend",
                        statement_span(statement),
                    ));
                }
                let info = FunctionInfo {
                    function,
                    llvm_name: format!("rune.fn.{}", functions.len()),
                    required: function
                        .parameters
                        .iter()
                        .filter(|p| p.default.is_none())
                        .count(),
                };
                function_order.push(name.clone());
                functions.insert(name.clone(), info);
            }
        }

        for statement in &program.statements {
            let inner = unwrap_statement(statement);
            let candidate = match inner {
                Statement::Declaration(decl) => Some((&decl.identifier, &decl.var_type)),
                Statement::Input {
                    identifier,
                    var_type,
                    ..
                } => Some((identifier, var_type)),
                _ => None,
            };
            if let Some((name, ty)) = candidate {
                if !is_codegen_type(ty) {
                    return Err(codegen_error(
                        format!("type {:?} is not supported by this backend", ty),
                        statement_span(statement),
                    ));
                }
                if globals.contains_key(name) {
                    return Err(codegen_error(
                        format!("duplicate global variable '{name}'"),
                        statement_span(statement),
                    ));
                }
                let global = Global {
                    ty: ty.clone(),
                    llvm_name: format!("rune.global.{}", globals.len()),
                };
                global_order.push(name.clone());
                globals.insert(name.clone(), global);
            }
        }

        let mut generated_functions = String::new();
        for name in &function_order {
            let info = functions
                .get(name)
                .expect("function was registered")
                .clone();
            let mut emitter =
                FunctionEmitter::new(&mut strings, &functions, &globals, info.function, source);
            generated_functions.push_str(&emitter.emit_function(&info)?);
            generated_functions.push('\n');
        }

        let mut main_emitter =
            FunctionEmitter::new_main(&mut strings, &functions, &globals, source);
        for statement in &program.statements {
            if matches!(unwrap_statement(statement), Statement::Function(_)) {
                continue;
            }
            main_emitter.emit_statement(statement)?;
        }
        let generated_main = main_emitter.finish_main()?;

        let mut output = String::from(
            "source_filename = \"rune\"\n\n\
             declare i32 @printf(ptr, ...)\n\
             declare i32 @scanf(ptr, ...)\n\
             declare i32 @getchar()\n\
             declare i32 @putchar(i32)\n\
             declare void @llvm.trap()\n\n",
        );
        for name in global_order {
            let global = globals.get(&name).expect("global was registered");
            output.push_str(&format!(
                "@{} = global {} {}\n",
                global.llvm_name,
                llvm_type(&global.ty)?,
                zero_value(&global.ty)?
            ));
        }
        output.push('\n');
        output.push_str(&generated_functions);
        output.push_str(&generated_main);
        output.push_str(&char_runtime_ir());
        output.push_str(&strings.globals());
        Ok(output)
    }
}

fn codegen_error(message: impl Into<String>, span: Span) -> Diagnostic {
    Diagnostic::new(Phase::CodeGeneration, message, span)
}

fn unwrap_statement(statement: &Statement) -> &Statement {
    match statement {
        Statement::Located(inner, _) => unwrap_statement(inner),
        other => other,
    }
}

fn statement_span(statement: &Statement) -> Span {
    match statement {
        Statement::Located(_, span) => span.clone(),
        _ => Span::default(),
    }
}

fn is_codegen_type(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Int | Type::String | Type::Bool | Type::Float | Type::Char | Type::Void
    )
}

fn llvm_type(ty: &Type) -> Result<&'static str, Diagnostic> {
    match ty {
        Type::Int => Ok("i64"),
        Type::Bool => Ok("i1"),
        Type::Float => Ok("double"),
        Type::Char => Ok("i32"),
        Type::String => Ok("ptr"),
        Type::Void => Ok("void"),
        other => Err(codegen_error(
            format!("type {:?} cannot be represented by this backend", other),
            Span::default(),
        )),
    }
}

fn zero_value(ty: &Type) -> Result<&'static str, Diagnostic> {
    match ty {
        Type::String => Ok("null"),
        Type::Float => Ok("0.000000e+00"),
        Type::Void => Err(codegen_error(
            "void values cannot be stored",
            Span::default(),
        )),
        _ if is_codegen_type(ty) => Ok("0"),
        other => Err(codegen_error(
            format!("type {:?} cannot be initialized", other),
            Span::default(),
        )),
    }
}

fn llvm_string_pointer(name: &str, length: usize) -> String {
    format!("getelementptr inbounds ([{length} x i8], ptr @{name}, i64 0, i64 0)")
}

fn string_length(value: &str) -> usize {
    value.as_bytes().len() + 1
}

fn char_runtime_ir() -> &'static str {
    r#"
define internal void @rune.print.char(i32 %cp) {
entry:
  %ascii = icmp ule i32 %cp, 127
  br i1 %ascii, label %one, label %check.two
one:
  %b1 = call i32 @putchar(i32 %cp)
  br label %done
check.two:
  %two = icmp ule i32 %cp, 2047
  br i1 %two, label %two.bytes, label %check.three
two.bytes:
  %s1 = lshr i32 %cp, 6
  %p1 = or i32 %s1, 192
  %s2 = and i32 %cp, 63
  %p2 = or i32 %s2, 128
  %b2 = call i32 @putchar(i32 %p1)
  %b3 = call i32 @putchar(i32 %p2)
  br label %done
check.three:
  %three = icmp ule i32 %cp, 65535
  br i1 %three, label %three.bytes, label %four.bytes
three.bytes:
  %t1 = lshr i32 %cp, 12
  %q1 = or i32 %t1, 224
  %t2s = lshr i32 %cp, 6
  %t2 = and i32 %t2s, 63
  %q2 = or i32 %t2, 128
  %t3 = and i32 %cp, 63
  %q3 = or i32 %t3, 128
  %b4 = call i32 @putchar(i32 %q1)
  %b5 = call i32 @putchar(i32 %q2)
  %b6 = call i32 @putchar(i32 %q3)
  br label %done
four.bytes:
  %f1s = lshr i32 %cp, 18
  %r1 = or i32 %f1s, 240
  %f2s = lshr i32 %cp, 12
  %f2 = and i32 %f2s, 63
  %r2 = or i32 %f2, 128
  %f3s = lshr i32 %cp, 6
  %f3 = and i32 %f3s, 63
  %r3 = or i32 %f3, 128
  %f4 = and i32 %cp, 63
  %r4 = or i32 %f4, 128
  %b7 = call i32 @putchar(i32 %r1)
  %b8 = call i32 @putchar(i32 %r2)
  %b9 = call i32 @putchar(i32 %r3)
  %b10 = call i32 @putchar(i32 %r4)
  br label %done
done:
  ret void
}

; Reads one UTF-8 Unicode scalar and returns U+FFFD for malformed input.
define internal i32 @rune.read.char() {
entry:
  %first = call i32 @getchar()
  br label %skip.whitespace
skip.whitespace:
  %byte = phi i32 [ %first, %entry ], [ %next, %read.next ]
  %eof = icmp eq i32 %byte, -1
  br i1 %eof, label %replacement, label %check.whitespace
check.whitespace:
  %space = icmp eq i32 %byte, 32
  %tab = icmp eq i32 %byte, 9
  %lf = icmp eq i32 %byte, 10
  %cr = icmp eq i32 %byte, 13
  %ws.a = or i1 %space, %tab
  %ws.b = or i1 %lf, %cr
  %ws = or i1 %ws.a, %ws.b
  br i1 %ws, label %read.next, label %classify
read.next:
  %next = call i32 @getchar()
  br label %skip.whitespace
classify:
  %c0 = phi i32 [ %byte, %check.whitespace ]
  %one = icmp ult i32 %c0, 128
  br i1 %one, label %single, label %check.two
single:
  ret i32 %c0
check.two:
  %two = icmp ult i32 %c0, 224
  br i1 %two, label %read.two, label %check.three
read.two:
  %c1 = call i32 @getchar()
  %c1a = and i32 %c1, 63
  %h1 = and i32 %c0, 31
  %h1s = shl i32 %h1, 6
  %v1 = or i32 %h1s, %c1a
  ret i32 %v1
check.three:
  %three = icmp ult i32 %c0, 240
  br i1 %three, label %read.three, label %read.four
read.three:
  %c2 = call i32 @getchar()
  %c3 = call i32 @getchar()
  %m2 = and i32 %c2, 63
  %m3 = and i32 %c3, 63
  %h2 = and i32 %c0, 15
  %h2s = shl i32 %h2, 12
  %m2s = shl i32 %m2, 6
  %v2a = or i32 %h2s, %m2s
  %v2 = or i32 %v2a, %m3
  ret i32 %v2
read.four:
  %c4 = call i32 @getchar()
  %c5 = call i32 @getchar()
  %c6 = call i32 @getchar()
  %m4 = and i32 %c4, 63
  %m5 = and i32 %c5, 63
  %m6 = and i32 %c6, 63
  %h3 = and i32 %c0, 7
  %h3s = shl i32 %h3, 18
  %m4s = shl i32 %m4, 12
  %m5s = shl i32 %m5, 6
  %v3a = or i32 %h3s, %m4s
  %v3b = or i32 %v3a, %m5s
  %v3 = or i32 %v3b, %m6
  ret i32 %v3
replacement:
  ret i32 65533
}
"#
}

struct FunctionEmitter<'g, 'a> {
    strings: &'g mut StringPool,
    functions: &'g HashMap<String, FunctionInfo<'a>>,
    globals: &'g HashMap<String, Global>,
    output: String,
    current_function: Option<FunctionInfo<'a>>,
    scopes: Vec<HashMap<String, Binding>>,
    temp: usize,
    label: usize,
    terminated: bool,
    diagnostic_span: Span,
    loop_exits: Vec<LoopTarget>,
    is_main: bool,
    trace_source: Option<&'a str>,
}

impl<'g, 'a> FunctionEmitter<'g, 'a> {
    fn new(
        strings: &'g mut StringPool,
        functions: &'g HashMap<String, FunctionInfo<'a>>,
        globals: &'g HashMap<String, Global>,
        function: &'a Function,
        trace_source: Option<&'a str>,
    ) -> Self {
        let name = function.name.as_deref().unwrap_or("anonymous");
        let info = functions.get(name).cloned();
        Self {
            strings,
            functions,
            globals,
            output: String::new(),
            current_function: info,
            scopes: vec![HashMap::new()],
            temp: 0,
            label: 0,
            terminated: false,
            diagnostic_span: Span::default(),
            loop_exits: Vec::new(),
            is_main: false,
            trace_source,
        }
    }

    fn new_main(
        strings: &'g mut StringPool,
        functions: &'g HashMap<String, FunctionInfo<'a>>,
        globals: &'g HashMap<String, Global>,
        trace_source: Option<&'a str>,
    ) -> Self {
        Self {
            strings,
            functions,
            globals,
            output: String::from("define i32 @main() {\nentry:\n"),
            current_function: None,
            scopes: vec![HashMap::new()],
            temp: 0,
            label: 0,
            terminated: false,
            diagnostic_span: Span::default(),
            loop_exits: Vec::new(),
            is_main: true,
            trace_source,
        }
    }

    fn fresh_temp(&mut self) -> String {
        let value = format!("%t{}", self.temp);
        self.temp += 1;
        value
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let value = format!("{prefix}.{}", self.label);
        self.label += 1;
        value
    }

    fn emit(&mut self, line: impl AsRef<str>) {
        self.output.push_str("  ");
        self.output.push_str(line.as_ref());
        self.output.push('\n');
    }

    fn begin_block(&mut self, label: &str) {
        self.output.push_str(label);
        self.output.push_str(":\n");
        self.terminated = false;
    }

    fn branch(&mut self, label: &str) {
        if !self.terminated {
            self.emit(format!("br label %{label}"));
            self.terminated = true;
        }
    }

    fn error(&self, message: impl Into<String>) -> Diagnostic {
        codegen_error(message, self.diagnostic_span.clone())
    }

    fn lookup(&self, name: &str) -> Result<Binding, Diagnostic> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Ok(binding.clone());
            }
        }
        if let Some(global) = self.globals.get(name) {
            return Ok(Binding {
                ty: global.ty.clone(),
                pointer: format!("@{}", global.llvm_name),
            });
        }
        Err(self.error(format!("unknown variable '{name}' during code generation")))
    }

    fn allocate(&mut self, ty: &Type, initial: Option<&Value>) -> Result<Binding, Diagnostic> {
        let llvm_ty = llvm_type(ty)?;
        let slot = self.fresh_temp();
        self.emit(format!("{slot} = alloca {llvm_ty}"));
        if let Some(value) = initial {
            let value = self.coerce(value.clone(), ty)?;
            self.emit(format!("store {llvm_ty} {}, ptr {slot}", value.operand));
        }
        Ok(Binding {
            ty: ty.clone(),
            pointer: slot,
        })
    }

    fn load(&mut self, binding: &Binding) -> Result<Value, Diagnostic> {
        let ty = llvm_type(&binding.ty)?;
        let value = self.fresh_temp();
        self.emit(format!("{value} = load {ty}, ptr {}", binding.pointer));
        Ok(Value {
            ty: binding.ty.clone(),
            operand: value,
        })
    }

    fn store(&mut self, binding: &Binding, value: Value) -> Result<(), Diagnostic> {
        let value = self.coerce(value, &binding.ty)?;
        let ty = llvm_type(&binding.ty)?;
        self.emit(format!(
            "store {ty} {}, ptr {}",
            value.operand, binding.pointer
        ));
        Ok(())
    }

    fn coerce(&mut self, value: Value, expected: &Type) -> Result<Value, Diagnostic> {
        if value.ty == *expected {
            return Ok(value);
        }
        match (&value.ty, expected) {
            (Type::Int, Type::Float) => {
                let result = self.fresh_temp();
                self.emit(format!("{result} = sitofp i64 {} to double", value.operand));
                Ok(Value {
                    ty: Type::Float,
                    operand: result,
                })
            }
            _ => Err(self.error(format!(
                "cannot lower conversion from {:?} to {:?}",
                value.ty, expected
            ))),
        }
    }

    fn emit_function(&mut self, info: &FunctionInfo<'a>) -> Result<String, Diagnostic> {
        let function = info.function;
        let result_type = llvm_type(&function.return_type)?;
        let params = function
            .parameters
            .iter()
            .enumerate()
            .map(|(i, p)| Ok(format!("{} %arg{i}", llvm_type(&p.param_type)?)))
            .collect::<Result<Vec<_>, Diagnostic>>()?
            .join(", ");
        self.output = format!(
            "define {result_type} @{}({params}) {{\nentry:\n",
            info.llvm_name
        );
        self.current_function = Some(info.clone());
        for (index, parameter) in function.parameters.iter().enumerate() {
            let value = Value {
                ty: parameter.param_type.clone(),
                operand: format!("%arg{index}"),
            };
            let binding = self.allocate(&parameter.param_type, Some(&value))?;
            self.scopes[0].insert(parameter.param_name.clone(), binding);
        }
        for statement in &function.body {
            if self.terminated {
                break;
            }
            self.emit_statement(statement)?;
        }
        if !self.terminated {
            if function.return_type == Type::Void {
                self.emit("ret void");
            } else {
                return Err(self.error(format!(
                    "non-void function '{}' can fall through",
                    function.name.as_deref().unwrap_or("<anonymous>")
                )));
            }
        }
        self.terminated = true;
        self.output.push_str("}\n");
        Ok(std::mem::take(&mut self.output))
    }

    fn finish_main(mut self) -> Result<String, Diagnostic> {
        if !self.terminated {
            self.emit("ret i32 0");
        }
        self.output.push_str("}\n");
        Ok(self.output)
    }

    fn emit_statement(&mut self, statement: &Statement) -> Result<(), Diagnostic> {
        if self.terminated {
            return Ok(());
        }
        if let Statement::Located(inner, span) = statement {
            self.diagnostic_span = span.clone();
            if let Some(source) = self.trace_source {
                let line = source[..span.0.start.min(source.len())]
                    .bytes()
                    .filter(|byte| *byte == b'\n')
                    .count()
                    + 1;
                let format = self.strings.intern("\u{1e}RUNE_STEP:%lld\n");
                let pointer = llvm_string_pointer(&format, string_length("\u{1e}RUNE_STEP:%lld\n"));
                self.emit(format!(
                    "call i32 (ptr, ...) @printf(ptr {pointer}, i64 {line})"
                ));
            }
            return self.emit_statement(inner);
        }
        match statement {
            Statement::Declaration(Declaration {
                identifier,
                var_type,
                value,
            }) => {
                let value = self.emit_expression(value)?;
                if self.is_main && self.scopes.len() == 1 {
                    let global = self.globals.get(identifier).ok_or_else(|| {
                        self.error(format!("missing storage for global '{identifier}'"))
                    })?;
                    self.store(
                        &Binding {
                            ty: global.ty.clone(),
                            pointer: format!("@{}", global.llvm_name),
                        },
                        value,
                    )?;
                } else {
                    let binding = self.allocate(var_type, Some(&value))?;
                    self.scopes
                        .last_mut()
                        .expect("scope exists")
                        .insert(identifier.clone(), binding);
                }
            }
            Statement::Assignment(assignment) => {
                let binding = self.lookup(&assignment.identifier)?;
                let value = self.emit_expression(&assignment.value)?;
                self.store(&binding, value)?;
            }
            Statement::Expression(expression) | Statement::Do(expression) => {
                let _ = self.emit_expression(expression)?;
            }
            Statement::Output(expression) => {
                let value = self.emit_expression(expression)?;
                self.emit_output(value)?;
            }
            Statement::Input {
                var_type,
                identifier,
                prompt,
            } => {
                let binding = if self.is_main && self.scopes.len() == 1 {
                    let global = self.globals.get(identifier).ok_or_else(|| {
                        self.error(format!("missing storage for input variable '{identifier}'"))
                    })?;
                    Binding {
                        ty: global.ty.clone(),
                        pointer: format!("@{}", global.llvm_name),
                    }
                } else {
                    let binding = self.allocate(var_type, None)?;
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(identifier.clone(), binding.clone());
                    binding
                };
                self.emit_input(var_type, &binding, prompt.as_deref())?;
            }
            Statement::AssignInput { identifier, prompt } => {
                let binding = self.lookup(identifier)?;
                self.emit_input(&binding.ty.clone(), &binding, prompt.as_deref())?;
            }
            Statement::If(if_expr) => self.emit_if(if_expr)?,
            Statement::Loop(loop_expr) => self.emit_loop(loop_expr)?,
            Statement::Return(expression) => {
                let function = self
                    .current_function
                    .as_ref()
                    .ok_or_else(|| self.error("return outside a function"))?
                    .function;
                let expected = function.return_type.clone();
                if expected == Type::Void {
                    return Err(self.error("void function cannot return a value"));
                }
                let value = self.emit_expression(expression)?;
                let value = self.coerce(value, &expected)?;
                self.emit(format!("ret {} {}", llvm_type(&expected)?, value.operand));
                self.terminated = true;
            }
            Statement::Guard(expression) => {
                let exit = self
                    .loop_exits
                    .last()
                    .map(|target| target.label.clone())
                    .ok_or_else(|| self.error("guard outside loop"))?;
                if let Some(target) = self.loop_exits.last_mut() {
                    target.used = true;
                }
                let continue_label = self.fresh_label("guard.continue");
                let value = self.emit_expression(expression)?;
                if value.ty != Type::Bool {
                    return Err(self.error("guard condition must be boolean"));
                }
                self.emit(format!(
                    "br i1 {}, label %{continue_label}, label %{exit}",
                    value.operand
                ));
                self.terminated = true;
                self.begin_block(&continue_label);
            }
            Statement::Function(_) => {
                return Err(
                    self.error("nested or anonymous functions are not supported by this backend")
                );
            }
            Statement::Located(_, _) => unreachable!(),
        }
        Ok(())
    }

    fn emit_expression(&mut self, expression: &Expression) -> Result<Value, Diagnostic> {
        match expression {
            Expression::Literal(literal) => self.emit_literal(literal),
            Expression::Identifier(name) => { let binding = self.lookup(name)?; self.load(&binding) }
            Expression::UnaryOp(operator, operand) => {
                let value = self.emit_expression(operand)?;
                match (operator.as_str(), &value.ty) {
                    ("-", Type::Int) => {
                        let temp = self.fresh_temp();
                        self.emit(format!("{temp} = sub i64 0, {}", value.operand));
                        Ok(Value { ty: Type::Int, operand: temp })
                    }
                    ("-", Type::Float) => {
                        let temp = self.fresh_temp();
                        self.emit(format!("{temp} = fneg double {}", value.operand));
                        Ok(Value { ty: Type::Float, operand: temp })
                    }
                    ("!", Type::Bool) => {
                        let temp = self.fresh_temp();
                        self.emit(format!("{temp} = xor i1 {}, true", value.operand));
                        Ok(Value { ty: Type::Bool, operand: temp })
                    }
                    _ => Err(self.error(format!("unsupported unary operation '{operator}' on {:?}", value.ty))),
                }
            }
            Expression::BinaryOp(left, operator, right) => self.emit_binary(left, operator, right),
            Expression::FunctionCall { name, args } => self.emit_call(name, args),
            Expression::InterpolatedCall(_, _)
            | Expression::InterpolatedString(_, _) | Expression::ConditionList(_) | Expression::InputOp(_) => {
                Err(self.error("lists, interpolation, condition lists, and expression input are not supported by this backend"))
            }
        }
    }

    fn emit_literal(&mut self, literal: &Literal) -> Result<Value, Diagnostic> {
        match literal {
            Literal::Int(value) => Ok(Value {
                ty: Type::Int,
                operand: value.to_string(),
            }),
            Literal::Bool(value) => Ok(Value {
                ty: Type::Bool,
                operand: if *value { "true" } else { "false" }.into(),
            }),
            Literal::Float(value) => Ok(Value {
                ty: Type::Float,
                operand: format!("{value:.17e}"),
            }),
            Literal::Char(value) => Ok(Value {
                ty: Type::Char,
                operand: (*value as u32).to_string(),
            }),
            Literal::String(value) => {
                let name = self.strings.intern(value);
                let pointer = llvm_string_pointer(&name, string_length(value));
                Ok(Value {
                    ty: Type::String,
                    operand: pointer,
                })
            }
            Literal::List(_) => Err(self.error("list literals are not supported by this backend")),
        }
    }

    fn emit_binary(
        &mut self,
        left: &Expression,
        operator: &str,
        right: &Expression,
    ) -> Result<Value, Diagnostic> {
        if operator == "&&" || operator == "||" {
            return self.emit_short_circuit(left, operator, right);
        }
        let mut lhs = self.emit_expression(left)?;
        let mut rhs = self.emit_expression(right)?;
        if lhs.ty == Type::Int && rhs.ty == Type::Float {
            lhs = self.coerce(lhs, &Type::Float)?;
        }
        if lhs.ty == Type::Float && rhs.ty == Type::Int {
            rhs = self.coerce(rhs, &Type::Float)?;
        }
        if lhs.ty != rhs.ty {
            return Err(self.error(format!("incompatible operands for '{operator}'")));
        }
        let temp = self.fresh_temp();
        let instruction = match (&lhs.ty, operator) {
            (Type::Int, "+") => format!("add i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "-") => format!("sub i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "*") => format!("mul i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "/") => format!("sdiv i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "+") => format!("fadd double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "-") => format!("fsub double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "*") => format!("fmul double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "/") => format!("fdiv double {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "==") => format!("icmp eq i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "!=") => format!("icmp ne i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "<") => format!("icmp slt i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, ">") => format!("icmp sgt i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, "<=") => format!("icmp sle i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Int, ">=") => format!("icmp sge i64 {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "==") => format!("fcmp oeq double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "!=") => format!("fcmp une double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "<") => format!("fcmp olt double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, ">") => format!("fcmp ogt double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, "<=") => format!("fcmp ole double {}, {}", lhs.operand, rhs.operand),
            (Type::Float, ">=") => format!("fcmp oge double {}, {}", lhs.operand, rhs.operand),
            (Type::Bool, "==") => format!("icmp eq i1 {}, {}", lhs.operand, rhs.operand),
            (Type::Bool, "!=") => format!("icmp ne i1 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, "==") => format!("icmp eq i32 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, "!=") => format!("icmp ne i32 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, "<") => format!("icmp ult i32 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, ">") => format!("icmp ugt i32 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, "<=") => format!("icmp ule i32 {}, {}", lhs.operand, rhs.operand),
            (Type::Char, ">=") => format!("icmp uge i32 {}, {}", lhs.operand, rhs.operand),
            (Type::String, "==" | "!=" | "<" | ">" | "<=" | ">=") => {
                return Err(self.error("string comparisons are not supported by this backend"));
            }
            (Type::String, "+") => {
                return Err(self.error("string concatenation is not supported by this backend"));
            }
            _ => {
                return Err(self.error(format!(
                    "operator '{operator}' is not supported for {:?}",
                    lhs.ty
                )));
            }
        };
        self.emit(format!("{temp} = {instruction}"));
        let result_type = if ["==", "!=", "<", ">", "<=", ">="].contains(&operator) {
            Type::Bool
        } else {
            lhs.ty
        };
        Ok(Value {
            ty: result_type,
            operand: temp,
        })
    }

    fn emit_short_circuit(
        &mut self,
        left: &Expression,
        operator: &str,
        right: &Expression,
    ) -> Result<Value, Diagnostic> {
        let left_value = self.emit_expression(left)?;
        if left_value.ty != Type::Bool {
            return Err(self.error("logical operands must be boolean"));
        }
        let rhs_label = self.fresh_label("logic.rhs");
        let merge_label = self.fresh_label("logic.end");
        let left_block = self.current_block_name();
        if operator == "&&" {
            self.emit(format!(
                "br i1 {}, label %{rhs_label}, label %{merge_label}",
                left_value.operand
            ));
        } else {
            self.emit(format!(
                "br i1 {}, label %{merge_label}, label %{rhs_label}",
                left_value.operand
            ));
        }
        self.terminated = true;
        self.begin_block(&rhs_label);
        let right_value = self.emit_expression(right)?;
        if right_value.ty != Type::Bool {
            return Err(self.error("logical operands must be boolean"));
        }
        let rhs_end = self.current_block_name();
        self.branch(&merge_label);
        self.begin_block(&merge_label);
        let temp = self.fresh_temp();
        let short_value = if operator == "&&" { "false" } else { "true" };
        self.emit(format!(
            "{temp} = phi i1 [ {short_value}, %{left_block} ], [ {}, %{rhs_end} ]",
            right_value.operand
        ));
        Ok(Value {
            ty: Type::Bool,
            operand: temp,
        })
    }

    fn current_block_name(&self) -> String {
        self.output
            .lines()
            .rev()
            .find_map(|line| line.strip_suffix(':').map(str::to_owned))
            .unwrap_or_else(|| "entry".into())
    }

    fn emit_call(&mut self, name: &str, args: &[Expression]) -> Result<Value, Diagnostic> {
        let info = self.functions.get(name).cloned().ok_or_else(|| {
            self.error(format!("unknown function '{name}' during code generation"))
        })?;
        if args.len() < info.required || args.len() > info.function.parameters.len() {
            return Err(self.error(format!("incorrect argument count for '{name}'")));
        }
        self.scopes.push(HashMap::new());
        let mut values = Vec::new();
        for (index, parameter) in info.function.parameters.iter().enumerate() {
            let value = if let Some(argument) = args.get(index) {
                self.emit_expression(argument)?
            } else {
                let default = parameter
                    .default
                    .as_ref()
                    .ok_or_else(|| self.error("missing default argument"))?;
                self.emit_expression(default)?
            };
            let value = self.coerce(value, &parameter.param_type)?;
            let binding = self.allocate(&parameter.param_type, Some(&value))?;
            self.scopes
                .last_mut()
                .expect("scope exists")
                .insert(parameter.param_name.clone(), binding);
            values.push(format!(
                "{} {}",
                llvm_type(&parameter.param_type)?,
                value.operand
            ));
        }
        self.scopes.pop();
        let result_type = &info.function.return_type;
        let params = values.join(", ");
        if *result_type == Type::Void {
            self.emit(format!("call void @{}({params})", info.llvm_name));
            Ok(Value {
                ty: Type::Void,
                operand: String::new(),
            })
        } else {
            let temp = self.fresh_temp();
            self.emit(format!(
                "{temp} = call {} @{}({params})",
                llvm_type(result_type)?,
                info.llvm_name
            ));
            Ok(Value {
                ty: result_type.clone(),
                operand: temp,
            })
        }
    }

    fn emit_condition_branch(
        &mut self,
        condition: &Condition,
        yes: &str,
        no: &str,
    ) -> Result<(), Diagnostic> {
        match condition {
            Condition::And(left, right) => {
                let rhs = self.fresh_label("and.rhs");
                self.emit_condition_branch(left, &rhs, no)?;
                self.begin_block(&rhs);
                self.emit_condition_branch(right, yes, no)
            }
            Condition::Or(left, right) => {
                let rhs = self.fresh_label("or.rhs");
                self.emit_condition_branch(left, yes, &rhs)?;
                self.begin_block(&rhs);
                self.emit_condition_branch(right, yes, no)
            }
            Condition::Not(inner) => self.emit_condition_branch(inner, no, yes),
            Condition::Grouped(inner) => self.emit_condition_branch(inner, yes, no),
            Condition::Single(expression) => {
                let value = self.emit_expression(expression)?;
                if value.ty != Type::Bool {
                    return Err(self.error("condition did not lower to bool"));
                }
                self.emit(format!(
                    "br i1 {}, label %{yes}, label %{no}",
                    value.operand
                ));
                self.terminated = true;
                Ok(())
            }
        }
    }

    fn emit_if(&mut self, if_expr: &IfExpr) -> Result<(), Diagnostic> {
        let merge = self.fresh_label("if.end");
        let mut next_condition = self.fresh_label("if.then");
        let mut has_incoming = false;
        let mut branches = Vec::with_capacity(1 + if_expr.elif_blocks.len());
        branches.push((&if_expr.condition, &if_expr.then_block));
        for (condition, body) in &if_expr.elif_blocks {
            branches.push((condition, body));
        }
        for (condition, body) in branches {
            let body_label = next_condition;
            let next_label = self.fresh_label("if.next");
            self.emit_condition_branch(condition, &body_label, &next_label)?;
            self.begin_block(&body_label);
            self.scopes.push(HashMap::new());
            self.emit_block(body)?;
            self.scopes.pop();
            if !self.terminated {
                self.branch(&merge);
                has_incoming = true;
            }
            self.begin_block(&next_label);
            next_condition = next_label;
        }
        if let Some(else_body) = &if_expr.else_block {
            self.scopes.push(HashMap::new());
            self.emit_block(else_body)?;
            self.scopes.pop();
            if !self.terminated {
                self.branch(&merge);
                has_incoming = true;
            }
        } else {
            self.branch(&merge);
            has_incoming = true;
        }
        self.begin_block(&merge);
        if !has_incoming {
            self.emit("unreachable");
            self.terminated = true;
        }
        Ok(())
    }

    fn emit_block(&mut self, statements: &[Statement]) -> Result<(), Diagnostic> {
        for statement in statements {
            if self.terminated {
                break;
            }
            self.emit_statement(statement)?;
        }
        Ok(())
    }

    fn emit_loop(&mut self, loop_expr: &LoopExpr) -> Result<(), Diagnostic> {
        match loop_expr {
            LoopExpr::Condition { condition, body } => {
                let head = self.fresh_label("loop.cond");
                let body_label = self.fresh_label("loop.body");
                let exit = self.fresh_label("loop.end");
                self.branch(&head);
                self.begin_block(&head);
                if condition.is_empty() {
                    return Err(self.error("empty loop condition"));
                }
                self.emit_condition_branch(&condition[0], &body_label, &exit)?;
                self.begin_block(&body_label);
                self.loop_exits.push(LoopTarget {
                    label: exit.clone(),
                    used: false,
                });
                self.scopes.push(HashMap::new());
                self.emit_block(body)?;
                self.scopes.pop();
                self.loop_exits.pop();
                self.branch(&head);
                self.begin_block(&exit);
            }
            LoopExpr::Infinite { body } => {
                let head = self.fresh_label("loop.infinite");
                let exit = self.fresh_label("loop.end");
                self.branch(&head);
                self.begin_block(&head);
                self.loop_exits.push(LoopTarget {
                    label: exit.clone(),
                    used: false,
                });
                self.scopes.push(HashMap::new());
                self.emit_block(body)?;
                self.scopes.pop();
                let target = self.loop_exits.pop().expect("loop target exists");
                self.branch(&head);
                self.begin_block(&exit);
                if !target.used {
                    self.emit("unreachable");
                    self.terminated = true;
                }
            }
            LoopExpr::Range {
                var,
                start,
                end,
                step,
                body,
            } => self.emit_range_loop(var, start, end.as_ref(), step.as_ref(), body)?,
            LoopExpr::ForEach { .. } => {
                return Err(self.error(
                    "foreach loops require list support, which is not available in this backend",
                ));
            }
        }
        Ok(())
    }

    fn emit_range_loop(
        &mut self,
        var: &str,
        start: &Expression,
        end: Option<&Expression>,
        step: Option<&Expression>,
        body: &[Statement],
    ) -> Result<(), Diagnostic> {
        let start_value = self.emit_expression(start)?;
        if !matches!(start_value.ty, Type::Int | Type::Float) {
            return Err(self.error("range loop start must be numeric"));
        }
        let raw_end = if let Some(end) = end {
            Some(self.emit_expression(end)?)
        } else {
            None
        };
        let raw_step = if let Some(step) = step {
            Some(self.emit_expression(step)?)
        } else {
            None
        };
        if raw_end
            .as_ref()
            .is_some_and(|v| !matches!(v.ty, Type::Int | Type::Float))
        {
            return Err(self.error("range loop end must be numeric"));
        }
        if raw_step
            .as_ref()
            .is_some_and(|v| !matches!(v.ty, Type::Int | Type::Float))
        {
            return Err(self.error("range loop step must be numeric"));
        }
        let loop_type = if matches!(start_value.ty, Type::Float)
            || raw_end.as_ref().is_some_and(|v| v.ty == Type::Float)
            || raw_step.as_ref().is_some_and(|v| v.ty == Type::Float)
        {
            Type::Float
        } else {
            Type::Int
        };
        let start_value = self.coerce(start_value, &loop_type)?;
        let step_value = if let Some(value) = raw_step {
            self.coerce(value, &loop_type)?
        } else if loop_type == Type::Float {
            Value {
                ty: Type::Float,
                operand: "1.000000e+00".into(),
            }
        } else {
            Value {
                ty: Type::Int,
                operand: "1".into(),
            }
        };
        let end_value = if let Some(value) = raw_end {
            Some(self.coerce(value, &loop_type)?)
        } else {
            None
        };
        let current = self.allocate(&loop_type, Some(&start_value))?;
        let step_slot = self.allocate(&loop_type, Some(&step_value))?;
        let binding = Binding {
            ty: loop_type.clone(),
            pointer: current.pointer.clone(),
        };
        self.scopes.push(HashMap::new());
        self.scopes
            .last_mut()
            .unwrap()
            .insert(var.to_string(), binding);
        let header = self.fresh_label("range.cond");
        let body_label = self.fresh_label("range.body");
        let increment = self.fresh_label("range.step");
        let exit = self.fresh_label("range.end");
        let zero = if loop_type == Type::Int {
            "0"
        } else {
            "0.000000e+00"
        };
        let step_now = self.load(&step_slot)?;
        let zero_test = self.fresh_temp();
        let zero_cmp = if loop_type == Type::Int {
            format!("icmp eq i64 {}, 0", step_now.operand)
        } else {
            format!("fcmp oeq double {}, 0.000000e+00", step_now.operand)
        };
        self.emit(format!("{zero_test} = {zero_cmp}"));
        let trap = self.fresh_label("range.zero.step");
        let valid_step = self.fresh_label("range.nonzero.step");
        self.emit(format!(
            "br i1 {zero_test}, label %{trap}, label %{valid_step}"
        ));
        self.terminated = true;
        self.begin_block(&trap);
        self.emit("call void @llvm.trap()");
        self.emit("unreachable");
        self.terminated = true;
        self.begin_block(&valid_step);
        self.branch(&header);
        self.begin_block(&header);
        if let Some(end) = end_value {
            let current_value = self.load(&current)?;
            let step_value = self.load(&step_slot)?;
            let positive = self.fresh_temp();
            let is_positive = if loop_type == Type::Int {
                format!("icmp sgt i64 {}, 0", step_value.operand)
            } else {
                format!("fcmp ogt double {}, {zero}", step_value.operand)
            };
            self.emit(format!("{positive} = {is_positive}"));
            let compare = self.fresh_temp();
            let pos_label = self.fresh_label("range.pos");
            let neg_label = self.fresh_label("range.neg");
            self.emit(format!(
                "br i1 {positive}, label %{pos_label}, label %{neg_label}"
            ));
            self.terminated = true;
            self.begin_block(&pos_label);
            let pos_cmp = self.fresh_temp();
            let pos_instruction = if loop_type == Type::Int {
                format!("icmp slt i64 {}, {}", current_value.operand, end.operand)
            } else {
                format!("fcmp olt double {}, {}", current_value.operand, end.operand)
            };
            self.emit(format!("{pos_cmp} = {pos_instruction}"));
            let pos_end = self.current_block_name();
            let cmp_merge = self.fresh_label("range.test");
            self.branch(&cmp_merge);
            self.begin_block(&neg_label);
            let neg_cmp = self.fresh_temp();
            let neg_instruction = if loop_type == Type::Int {
                format!("icmp sgt i64 {}, {}", current_value.operand, end.operand)
            } else {
                format!("fcmp ogt double {}, {}", current_value.operand, end.operand)
            };
            self.emit(format!("{neg_cmp} = {neg_instruction}"));
            let neg_end = self.current_block_name();
            self.branch(&cmp_merge);
            self.begin_block(&cmp_merge);
            self.emit(format!(
                "{compare} = phi i1 [ {pos_cmp}, %{pos_end} ], [ {neg_cmp}, %{neg_end} ]"
            ));
            self.emit(format!(
                "br i1 {compare}, label %{body_label}, label %{exit}"
            ));
            self.terminated = true;
        } else {
            self.branch(&body_label);
        }
        self.begin_block(&body_label);
        self.loop_exits.push(LoopTarget {
            label: exit.clone(),
            used: false,
        });
        self.scopes.push(HashMap::new());
        // Loop variable storage is visible to the body and updated at the latch.
        self.scopes
            .last_mut()
            .unwrap()
            .insert(var.to_string(), current.clone());
        self.emit_block(body)?;
        self.scopes.pop();
        let target = self.loop_exits.pop().expect("loop target exists");
        self.branch(&increment);
        self.begin_block(&increment);
        let old = self.load(&current)?;
        let step_now = self.load(&step_slot)?;
        let next = self.fresh_temp();
        let instruction = if loop_type == Type::Int {
            format!("add i64 {}, {}", old.operand, step_now.operand)
        } else {
            format!("fadd double {}, {}", old.operand, step_now.operand)
        };
        self.emit(format!("{next} = {instruction}"));
        self.store(
            &current,
            Value {
                ty: loop_type.clone(),
                operand: next,
            },
        )?;
        self.branch(&header);
        self.begin_block(&exit);
        if end.is_none() && !target.used {
            self.emit("unreachable");
            self.terminated = true;
        }
        self.scopes.pop();
        Ok(())
    }

    fn emit_output(&mut self, value: Value) -> Result<(), Diagnostic> {
        match value.ty {
            Type::Int => {
                let f = self.format_ptr("%lld\n");
                self.emit(format!(
                    "call i32 (ptr, ...) @printf(ptr {f}, i64 {})",
                    value.operand
                ));
            }
            Type::Float => {
                let f = self.format_ptr("%g\n");
                self.emit(format!(
                    "call i32 (ptr, ...) @printf(ptr {f}, double {})",
                    value.operand
                ));
            }
            Type::Char => {
                self.emit(format!("call void @rune.print.char(i32 {})", value.operand));
                self.emit("call i32 @putchar(i32 10)");
            }
            Type::Bool => {
                let yes = self.strings.intern("true");
                let no = self.strings.intern("false");
                let yes_ptr = llvm_string_pointer(&yes, string_length("true"));
                let no_ptr = llvm_string_pointer(&no, string_length("false"));
                let selected = self.fresh_temp();
                self.emit(format!(
                    "{selected} = select i1 {}, ptr {}, ptr {}",
                    value.operand, yes_ptr, no_ptr
                ));
                let fmt = self.format_ptr("%s\n");
                self.emit(format!(
                    "call i32 (ptr, ...) @printf(ptr {fmt}, ptr {selected})"
                ));
            }
            Type::String => {
                let f = self.format_ptr("%s\n");
                self.emit(format!(
                    "call i32 (ptr, ...) @printf(ptr {f}, ptr {})",
                    value.operand
                ));
            }
            other => return Err(self.error(format!("cannot output values of type {:?}", other))),
        }
        Ok(())
    }

    fn format_ptr(&mut self, format: &str) -> String {
        let name = self.strings.intern(format);
        llvm_string_pointer(&name, string_length(format))
    }

    fn emit_input(
        &mut self,
        ty: &Type,
        binding: &Binding,
        prompt: Option<&str>,
    ) -> Result<(), Diagnostic> {
        if let Some(prompt) = prompt {
            let name = self.strings.intern(prompt);
            let pointer = llvm_string_pointer(&name, string_length(prompt));
            let fmt = self.format_ptr("%s");
            self.emit(format!(
                "call i32 (ptr, ...) @printf(ptr {fmt}, ptr {pointer})"
            ));
        }
        match ty {
            Type::Int => {
                let fmt = self.format_ptr("%lld");
                self.emit(format!(
                    "call i32 (ptr, ...) @scanf(ptr {fmt}, ptr {})",
                    binding.pointer
                ));
            }
            Type::Float => {
                let fmt = self.format_ptr("%lf");
                self.emit(format!(
                    "call i32 (ptr, ...) @scanf(ptr {fmt}, ptr {})",
                    binding.pointer
                ));
            }
            Type::Bool => {
                let slot = self.fresh_temp();
                self.emit(format!("{slot} = alloca i32"));
                let fmt = self.format_ptr("%d");
                self.emit(format!("call i32 (ptr, ...) @scanf(ptr {fmt}, ptr {slot})"));
                let input = self.fresh_temp();
                self.emit(format!("{input} = load i32, ptr {slot}"));
                let value = self.fresh_temp();
                self.emit(format!("{value} = icmp ne i32 {input}, 0"));
                self.emit(format!("store i1 {value}, ptr {}", binding.pointer));
            }
            Type::Char => {
                let value = self.fresh_temp();
                self.emit(format!("{value} = call i32 @rune.read.char()"));
                self.emit(format!("store i32 {value}, ptr {}", binding.pointer));
            }
            Type::String => return Err(self.error("string input is not supported by this backend")),
            _ => return Err(self.error(format!("input type {:?} is not supported", ty))),
        }
        Ok(())
    }
}
