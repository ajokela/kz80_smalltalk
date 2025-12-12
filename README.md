# kz80_smalltalk

A Tiny Smalltalk compiler for the Z80 processor, targeting the RetroShield hardware platform.

## Features

- Smalltalk subset compiler generating native Z80 machine code
- BCD (Binary Coded Decimal) arithmetic using the Z80's DAA instruction
- 4-byte packed BCD integers supporting up to 8 decimal digits
- Arithmetic operations: `+`, `-`, `*`
- Comparison operations: `<`, `>`, `=`
- String literals and printing
- Generates 32KB ROM images

## Installation

```bash
cargo install kz80_smalltalk
```

Or build from source:

```bash
git clone https://github.com/ajokela/kz80_smalltalk
cd kz80_smalltalk
cargo build --release
```

## Usage

```bash
# Compile a Smalltalk file to Z80 binary
kz80_smalltalk input.st -o output.bin

# Example
echo "42 + 7" > test.st
kz80_smalltalk test.st -o test.bin
```

## Smalltalk Subset

The compiler supports a minimal subset of Smalltalk:

```smalltalk
"Integer literals"
42
12345

"Arithmetic"
3 + 4
10 - 3
6 * 7

"Comparisons (return 1 for true, 0 for false)"
3 < 5
5 > 3
42 = 42

"Strings"
'Hello World'

"Chained operations (left to right evaluation)"
1 + 2 + 3
```

## Architecture

The compiler works in several stages:

1. **Lexer** (`lexer.rs`) - Tokenizes Smalltalk source code
2. **Parser** (`parser.rs`) - Builds an AST from tokens
3. **Compiler** (`compiler.rs`) - Generates bytecode from AST
4. **Code Generator** (`codegen.rs`) - Emits Z80 machine code with bytecode interpreter

### BCD Arithmetic

All integer arithmetic uses packed BCD format with the Z80's DAA (Decimal Adjust Accumulator) instruction for correct decimal results. This matches the approach used in other kz80 language implementations.

## Testing

```bash
# Run all tests (10 unit + 38 integration)
cargo test
```

## License

BSD-3-Clause

## Related Projects

Part of the kz80 family of retro language compilers for Z80-based RetroShield hardware.
