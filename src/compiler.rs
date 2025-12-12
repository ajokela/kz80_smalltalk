// Tiny Smalltalk Bytecode Compiler
// Compiles AST to stack-based bytecode

use crate::parser::Expr;

/// Bytecode instructions for the Tiny Smalltalk VM
#[derive(Debug, Clone)]
pub enum Bytecode {
    // Stack operations
    PushInt(i32),           // Push integer literal
    PushFloat(i32),         // Push float as 16.16 fixed-point
    PushString(String),     // Push string literal
    PushSymbol(String),     // Push symbol literal
    PushChar(u8),           // Push character literal
    PushTrue,               // Push true
    PushFalse,              // Push false
    PushNil,                // Push nil
    PushSelf,               // Push self

    // Variables
    LoadVar(String),        // Push variable value
    StoreVar(String),       // Pop and store to variable

    // Messages
    SendUnary(String),      // Send unary message (pop receiver, push result)
    SendBinary(String),     // Send binary message (pop arg, pop receiver, push result)
    SendKeyword(String, u8),// Send keyword message (name, arg count)

    // Blocks
    MakeBlock(u16),         // Create block, arg is bytecode offset to block end
    BlockArg(u8),           // Load block argument by index

    // Control
    Return,                 // Return from method/block
    Pop,                    // Discard top of stack
    Dup,                    // Duplicate top of stack

    // Built-in primitives (integer)
    PrimAdd,                // Integer +
    PrimSub,                // Integer -
    PrimMul,                // Integer *
    PrimDiv,                // Integer /
    PrimMod,                // Integer mod
    PrimLt,                 // Integer <
    PrimGt,                 // Integer >
    PrimEq,                 // Object =

    // Built-in primitives (float - 16.16 fixed-point)
    PrimFAdd,               // Float +
    PrimFSub,               // Float -
    PrimFMul,               // Float *
    PrimFDiv,               // Float /

    PrimPrint,              // Print object
    PrimPrintln,            // Print object with newline
    PrimCr,                 // Print newline

    // Jumps (for conditionals)
    Jump(i16),              // Unconditional jump
    JumpIfFalse(i16),       // Jump if top of stack is false
    JumpIfTrue(i16),        // Jump if top of stack is true

    Halt,                   // Stop execution
}

/// Compiled bytecode program
#[derive(Debug)]
pub struct CompiledProgram {
    pub bytecode: Vec<Bytecode>,
    pub strings: Vec<String>,       // String constant pool
    pub symbols: Vec<String>,       // Symbol constant pool
}

pub struct Compiler {
    bytecode: Vec<Bytecode>,
    strings: Vec<String>,
    symbols: Vec<String>,
    locals: Vec<String>,            // Local variable names
    block_args: Vec<Vec<String>>,   // Stack of block argument names
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            bytecode: Vec::new(),
            strings: Vec::new(),
            symbols: Vec::new(),
            locals: Vec::new(),
            block_args: Vec::new(),
        }
    }

    fn emit(&mut self, bc: Bytecode) {
        self.bytecode.push(bc);
    }

    fn add_string(&mut self, s: String) -> usize {
        if let Some(idx) = self.strings.iter().position(|x| x == &s) {
            idx
        } else {
            self.strings.push(s);
            self.strings.len() - 1
        }
    }

    fn add_symbol(&mut self, s: String) -> usize {
        if let Some(idx) = self.symbols.iter().position(|x| x == &s) {
            idx
        } else {
            self.symbols.push(s);
            self.symbols.len() - 1
        }
    }

    fn find_block_arg(&self, name: &str) -> Option<u8> {
        for args in self.block_args.iter().rev() {
            if let Some(idx) = args.iter().position(|x| x == name) {
                return Some(idx as u8);
            }
        }
        None
    }

    pub fn compile_program(&mut self, expr: &Expr) -> Result<(), String> {
        self.compile_expr(expr)?;
        // Print result and halt
        self.emit(Bytecode::PrimPrintln);
        self.emit(Bytecode::Halt);
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Integer(n) => {
                self.emit(Bytecode::PushInt(*n));
            }

            Expr::Float(f) => {
                // Convert to 16.16 fixed-point format
                // Integer part in high 16 bits, fractional in low 16 bits
                let fixed = (*f * 65536.0) as i32;
                self.emit(Bytecode::PushFloat(fixed));
            }

            Expr::String(s) => {
                self.emit(Bytecode::PushString(s.clone()));
            }

            Expr::Symbol(s) => {
                self.emit(Bytecode::PushSymbol(s.clone()));
            }

            Expr::Character(c) => {
                self.emit(Bytecode::PushChar(*c as u8));
            }

            Expr::True => {
                self.emit(Bytecode::PushTrue);
            }

            Expr::False => {
                self.emit(Bytecode::PushFalse);
            }

            Expr::Nil => {
                self.emit(Bytecode::PushNil);
            }

            Expr::Self_ => {
                self.emit(Bytecode::PushSelf);
            }

            Expr::Variable(name) => {
                // Check if it's a block argument
                if let Some(idx) = self.find_block_arg(name) {
                    self.emit(Bytecode::BlockArg(idx));
                } else {
                    self.emit(Bytecode::LoadVar(name.clone()));
                }
            }

            Expr::Assign(name, value) => {
                self.compile_expr(value)?;
                self.emit(Bytecode::Dup);  // Leave value on stack
                self.emit(Bytecode::StoreVar(name.clone()));
                if !self.locals.contains(name) {
                    self.locals.push(name.clone());
                }
            }

            Expr::UnaryMessage(receiver, msg) => {
                self.compile_expr(receiver)?;
                // Check for built-in messages
                match msg.as_str() {
                    "print" => self.emit(Bytecode::PrimPrint),
                    "println" => self.emit(Bytecode::PrimPrintln),
                    "cr" => self.emit(Bytecode::PrimCr),
                    "negated" => {
                        self.emit(Bytecode::PushInt(0));
                        self.emit(Bytecode::SendBinary("-".to_string()));
                    }
                    "abs" => {
                        // Duplicate, check if negative, negate if so
                        self.emit(Bytecode::SendUnary("abs".to_string()));
                    }
                    _ => self.emit(Bytecode::SendUnary(msg.clone())),
                }
            }

            Expr::BinaryMessage(receiver, op, arg) => {
                // Check if either operand is a float to use float operations
                let is_float = matches!(receiver.as_ref(), Expr::Float(_))
                    || matches!(arg.as_ref(), Expr::Float(_));

                self.compile_expr(receiver)?;
                self.compile_expr(arg)?;

                // Check for primitive operations
                if is_float {
                    // Float operations
                    match op.as_str() {
                        "+" => self.emit(Bytecode::PrimFAdd),
                        "-" => self.emit(Bytecode::PrimFSub),
                        "*" => self.emit(Bytecode::PrimFMul),
                        "/" => self.emit(Bytecode::PrimFDiv),
                        _ => self.emit(Bytecode::SendBinary(op.clone())),
                    }
                } else {
                    // Integer operations
                    match op.as_str() {
                        "+" => self.emit(Bytecode::PrimAdd),
                        "-" => self.emit(Bytecode::PrimSub),
                        "*" => self.emit(Bytecode::PrimMul),
                        "/" => self.emit(Bytecode::PrimDiv),
                        "\\\\" => self.emit(Bytecode::PrimMod),
                        "<" => self.emit(Bytecode::PrimLt),
                        ">" => self.emit(Bytecode::PrimGt),
                        "=" => self.emit(Bytecode::PrimEq),
                        _ => self.emit(Bytecode::SendBinary(op.clone())),
                    }
                }
            }

            Expr::KeywordMessage(receiver, keywords) => {
                self.compile_expr(receiver)?;
                // Compile all arguments
                for (_, arg) in keywords {
                    self.compile_expr(arg)?;
                }
                // Build full keyword selector
                let selector: String = keywords.iter().map(|(kw, _)| kw.as_str()).collect();
                let arg_count = keywords.len() as u8;

                // Check for built-in keyword messages
                match selector.as_str() {
                    "value:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "value:value:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "ifTrue:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "ifFalse:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "ifTrue:ifFalse:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "timesRepeat:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    "to:do:" => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                    _ => self.emit(Bytecode::SendKeyword(selector, arg_count)),
                }
            }

            Expr::Block(args, body) => {
                // Push block arguments context
                self.block_args.push(args.clone());

                // Record where block starts
                let block_start = self.bytecode.len();
                self.emit(Bytecode::MakeBlock(0)); // Placeholder for offset

                // Compile block body
                for (i, stmt) in body.iter().enumerate() {
                    self.compile_expr(stmt)?;
                    // Pop intermediate results except last
                    if i < body.len() - 1 {
                        self.emit(Bytecode::Pop);
                    }
                }
                self.emit(Bytecode::Return);

                // Patch the block offset
                let block_end = self.bytecode.len();
                let offset = (block_end - block_start) as u16;
                self.bytecode[block_start] = Bytecode::MakeBlock(offset);

                self.block_args.pop();
            }

            Expr::Return(value) => {
                self.compile_expr(value)?;
                self.emit(Bytecode::Return);
            }

            Expr::Sequence(statements) => {
                for (i, stmt) in statements.iter().enumerate() {
                    self.compile_expr(stmt)?;
                    // Pop intermediate results except last
                    if i < statements.len() - 1 {
                        self.emit(Bytecode::Pop);
                    }
                }
            }
        }

        Ok(())
    }
}

pub fn compile(ast: &Expr) -> Result<CompiledProgram, String> {
    let mut compiler = Compiler::new();
    compiler.compile_program(ast)?;

    Ok(CompiledProgram {
        bytecode: compiler.bytecode,
        strings: compiler.strings,
        symbols: compiler.symbols,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::parser::parse;

    #[test]
    fn test_compile_integer() {
        let tokens = tokenize("42").unwrap();
        let ast = parse(&tokens).unwrap();
        let program = compile(&ast).unwrap();
        assert!(matches!(program.bytecode[0], Bytecode::PushInt(42)));
    }

    #[test]
    fn test_compile_addition() {
        let tokens = tokenize("3 + 4").unwrap();
        let ast = parse(&tokens).unwrap();
        let program = compile(&ast).unwrap();
        assert!(matches!(program.bytecode[0], Bytecode::PushInt(3)));
        assert!(matches!(program.bytecode[1], Bytecode::PushInt(4)));
        assert!(matches!(program.bytecode[2], Bytecode::PrimAdd));
    }
}
