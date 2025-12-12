// Tiny Smalltalk to Z80 Compiler
// A minimal Smalltalk implementation for RetroShield Z80

mod lexer;
mod parser;
mod compiler;
mod codegen;

use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Tiny Smalltalk to Z80 Compiler");
        eprintln!("Usage: {} <input.st> [-o output.bin]", args[0]);
        eprintln!();
        eprintln!("Example:");
        eprintln!("  3 + 4           -> prints 7");
        eprintln!("  'hello' print   -> prints hello");
        eprintln!("  [:x | x * x] value: 5  -> prints 25");
        process::exit(1);
    }

    let input_file = &args[1];
    let output_file = if args.len() >= 4 && args[2] == "-o" {
        args[3].clone()
    } else {
        input_file.replace(".st", ".bin")
    };

    // Read source
    let source = match fs::read_to_string(input_file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {}: {}", input_file, e);
            process::exit(1);
        }
    };

    // Compile
    match compile(&source) {
        Ok(binary) => {
            if let Err(e) = fs::write(&output_file, &binary) {
                eprintln!("Error writing {}: {}", output_file, e);
                process::exit(1);
            }
            println!("Compiled {} -> {} ({} bytes)", input_file, output_file, binary.len());
        }
        Err(e) => {
            eprintln!("Compile error: {}", e);
            process::exit(1);
        }
    }
}

fn compile(source: &str) -> Result<Vec<u8>, String> {
    // Tokenize
    let tokens = lexer::tokenize(source)?;

    // Parse
    let ast = parser::parse(&tokens)?;

    // Compile to bytecode
    let bytecode = compiler::compile(&ast)?;

    // Generate Z80 code
    let binary = codegen::generate(&bytecode)?;

    Ok(binary)
}
