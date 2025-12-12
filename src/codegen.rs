// Tiny Smalltalk Z80 Code Generator
// Generates a Z80 bytecode interpreter with embedded bytecode

use crate::compiler::{Bytecode, CompiledProgram};
use std::collections::HashMap;

/// Memory layout for RetroShield Z80 (32KB ROM + 32KB RAM)
/// 0x0000-0x00FF: RST vectors and startup
/// 0x0100-0x7FFF: Z80 code (interpreter + bytecode + constants)
/// 0x8000-0x80FF: Object stack (256 bytes, 128 object refs)
/// 0x8100-0x81FF: Call stack (256 bytes, return addresses)
/// 0x8200-0x82FF: Local variables
/// 0x8300-0x83FF: Block arguments
/// 0x8400-0x8FFF: Object heap (~3KB)
/// 0x9000-0xFFFF: Free RAM

const CODE_START: u16 = 0x0100;
const OBJ_STACK: u16 = 0x8100;      // Object reference stack (256 bytes)
const OBJ_STACK_PTR: u16 = 0x8000;  // Pointer to current obj stack position (2 bytes)
const CALL_STACK: u16 = 0x8200;     // Return address stack
const LOCALS: u16 = 0x8300;         // Local variables
const BLOCK_ARGS: u16 = 0x8400;     // Block argument storage
const HEAP_START: u16 = 0x8500;     // Object heap
const BC_PC: u16 = 0x8002;          // Bytecode PC (2 bytes) - absolute address
const HEAP_PTR: u16 = 0x8004;       // Heap pointer (2 bytes)
const TEMP1: u16 = 0x8006;          // Temp variable 1 (2 bytes)
const TEMP2: u16 = 0x8008;          // Temp variable 2 (2 bytes)

// ACIA ports
const ACIA_CTRL: u8 = 0x80;
const ACIA_DATA: u8 = 0x81;

// Object tags (first byte of object)
const TAG_INT: u8 = 0x01;           // BCD integer (tag + 4 bytes packed BCD = 8 digits)
const TAG_STRING: u8 = 0x02;        // String (tag + len + bytes)
const TAG_SYMBOL: u8 = 0x03;        // Symbol (tag + len + bytes)
const TAG_TRUE: u8 = 0x04;          // true singleton
const TAG_FALSE: u8 = 0x05;         // false singleton
const TAG_NIL: u8 = 0x06;           // nil singleton
const TAG_BLOCK: u8 = 0x07;         // Block (tag + bytecode offset)
const TAG_CHAR: u8 = 0x08;          // Character (tag + byte)
const TAG_FLOAT: u8 = 0x09;         // 16.16 fixed-point float (tag + 4 bytes)

pub struct CodeGen {
    code: Vec<u8>,
    pc: u16,
    labels: HashMap<String, u16>,
    forward_refs: Vec<(u16, String)>,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            pc: 0,
            labels: HashMap::new(),
            forward_refs: Vec::new(),
        }
    }

    fn emit(&mut self, byte: u8) {
        self.code.push(byte);
        self.pc += 1;
    }

    fn emit16(&mut self, word: u16) {
        self.emit((word & 0xFF) as u8);
        self.emit((word >> 8) as u8);
    }

    fn label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.pc);
    }

    fn jp_label(&mut self, name: &str) {
        self.emit(0xC3);  // JP nn
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit16(0);
    }

    fn call_label(&mut self, name: &str) {
        self.emit(0xCD);  // CALL nn
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit16(0);
    }

    fn jr_nz(&mut self, name: &str) {
        self.emit(0x20);  // JR NZ, n
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit(0);
    }

    fn jr_z(&mut self, name: &str) {
        self.emit(0x28);  // JR Z, n
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit(0);
    }

    fn jr_label(&mut self, name: &str) {
        self.emit(0x18);  // JR n
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit(0);
    }

    fn resolve_refs(&mut self) -> Result<(), String> {
        for (addr, name) in &self.forward_refs {
            let target = self.labels.get(name)
                .ok_or_else(|| format!("Undefined label: {}", name))?;

            // Check if it's a relative jump (JR)
            let opcode = self.code[*addr as usize - 1];
            if opcode == 0x18 || opcode == 0x20 || opcode == 0x28 || opcode == 0x30 || opcode == 0x38 {
                // Relative jump
                let offset = (*target as i32) - (*addr as i32) - 1;
                if offset < -128 || offset > 127 {
                    return Err(format!("Jump too far for JR: {} to {}", addr, target));
                }
                self.code[*addr as usize] = offset as u8;
            } else {
                // Absolute jump/call
                self.code[*addr as usize] = (*target & 0xFF) as u8;
                self.code[*addr as usize + 1] = (*target >> 8) as u8;
            }
        }
        Ok(())
    }

    fn ld_a_n(&mut self, n: u8) {
        self.emit(0x3E);
        self.emit(n);
    }

    fn ld_hl_nn(&mut self, nn: u16) {
        self.emit(0x21);
        self.emit16(nn);
    }

    fn ld_de_nn(&mut self, nn: u16) {
        self.emit(0x11);
        self.emit16(nn);
    }

    fn ld_bc_nn(&mut self, nn: u16) {
        self.emit(0x01);
        self.emit16(nn);
    }

    fn ld_sp_nn(&mut self, nn: u16) {
        self.emit(0x31);
        self.emit16(nn);
    }

    fn out_n_a(&mut self, port: u8) {
        self.emit(0xD3);
        self.emit(port);
    }

    fn in_a_n(&mut self, port: u8) {
        self.emit(0xDB);
        self.emit(port);
    }

    pub fn generate(&mut self, program: &CompiledProgram) -> Result<Vec<u8>, String> {
        // Jump vector at 0x0000
        self.emit(0xC3);  // JP CODE_START
        self.emit16(CODE_START);

        // Pad to CODE_START
        while self.pc < CODE_START {
            self.emit(0x00);
        }

        // Generate initialization
        self.generate_init();

        // Generate runtime/interpreter
        self.generate_runtime();

        // Generate bytecode interpreter dispatch
        self.generate_interpreter();

        // Embed bytecode
        self.label("bytecode_start");
        self.generate_bytecode(program)?;
        self.label("bytecode_end");

        // Embed string constants
        self.label("strings_start");
        for s in &program.strings {
            self.emit(s.len() as u8);
            for b in s.bytes() {
                self.emit(b);
            }
        }

        // Embed symbol constants
        self.label("symbols_start");
        for s in &program.symbols {
            self.emit(s.len() as u8);
            for b in s.bytes() {
                self.emit(b);
            }
        }

        // Resolve forward references
        self.resolve_refs()?;

        // Create 32KB ROM image
        let mut rom = vec![0u8; 32768];
        for (i, byte) in self.code.iter().enumerate() {
            if i < rom.len() {
                rom[i] = *byte;
            }
        }

        Ok(rom)
    }

    fn generate_init(&mut self) {
        self.label("init");

        // Set stack pointer
        self.ld_sp_nn(0x0000);  // Wraps to top of RAM

        // Initialize ACIA
        self.call_label("acia_init");

        // Initialize heap pointer
        self.call_label("init_heap");

        // Initialize object stack pointer
        self.ld_hl_nn(OBJ_STACK);
        self.emit(0x22);  // LD (nn), HL
        self.emit16(OBJ_STACK_PTR);

        // Print banner
        self.call_label("print_banner");

        // Initialize bytecode PC to start of bytecode
        self.ld_hl_nn(0);  // Will be patched to bytecode_start
        self.forward_refs.push((self.pc - 2, "bytecode_start".to_string()));
        self.emit(0x22);  // LD (BC_PC), HL
        self.emit16(BC_PC);

        // Start interpreter
        self.jp_label("interpreter_loop");
    }

    fn generate_runtime(&mut self) {
        // ACIA init
        self.label("acia_init");
        self.ld_a_n(0x03);  // Master reset
        self.out_n_a(ACIA_CTRL);
        self.ld_a_n(0x15);  // 8N1, /16
        self.out_n_a(ACIA_CTRL);
        self.emit(0xC9);  // RET

        // Print character in A
        self.label("print_char");
        self.emit(0xF5);  // PUSH AF
        self.label("print_wait");
        self.in_a_n(ACIA_CTRL);
        self.emit(0xE6); self.emit(0x02);  // AND 2
        self.jr_z("print_wait");
        self.emit(0xF1);  // POP AF
        self.out_n_a(ACIA_DATA);
        self.emit(0xC9);  // RET

        // Print banner
        self.label("print_banner");
        self.ld_hl_nn(0);  // Will be patched
        self.forward_refs.push((self.pc - 2, "banner_str".to_string()));
        self.label("print_str_loop");
        self.emit(0x7E);  // LD A, (HL)
        self.emit(0xB7);  // OR A
        self.emit(0xC8);  // RET Z
        self.call_label("print_char");
        self.emit(0x23);  // INC HL
        self.jr_label("print_str_loop");

        // Banner string
        self.label("banner_str");
        for b in b"Tiny Smalltalk on Z80\r\n" {
            self.emit(*b);
        }
        self.emit(0);

        // Print integer in HL
        self.label("print_int");
        // Check if negative
        self.emit(0x7C);  // LD A, H
        self.emit(0xB7);  // OR A
        self.emit(0xF2);  // JP P, print_int_pos
        self.forward_refs.push((self.pc, "print_int_pos".to_string()));
        self.emit16(0);
        // Negative: print minus and negate
        self.ld_a_n(b'-');
        self.call_label("print_char");
        // Negate HL
        self.emit(0xAF);  // XOR A
        self.emit(0x95);  // SUB L
        self.emit(0x6F);  // LD L, A
        self.emit(0x9F);  // SBC A, A
        self.emit(0x94);  // SUB H
        self.emit(0x67);  // LD H, A

        self.label("print_int_pos");
        // Convert to decimal and print
        self.ld_de_nn(10000);
        self.call_label("print_digit");
        self.ld_de_nn(1000);
        self.call_label("print_digit");
        self.ld_de_nn(100);
        self.call_label("print_digit");
        self.ld_de_nn(10);
        self.call_label("print_digit");
        // Last digit
        self.emit(0x7D);  // LD A, L
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        self.emit(0xC9);  // RET

        // Print single decimal digit (HL / DE), updates HL to remainder
        self.label("print_digit");
        self.emit(0xAF);  // XOR A (digit counter)
        self.emit(0x47);  // LD B, A
        self.label("print_digit_loop");
        self.emit(0xB7);  // OR A (clear carry)
        self.emit(0xED); self.emit(0x52);  // SBC HL, DE
        self.emit(0x38);  // JR C, print_digit_done
        self.forward_refs.push((self.pc, "print_digit_done".to_string()));
        self.emit(0);
        self.emit(0x04);  // INC B
        self.jr_label("print_digit_loop");
        self.label("print_digit_done");
        self.emit(0x19);  // ADD HL, DE (restore)
        self.emit(0x78);  // LD A, B
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        self.emit(0xC9);  // RET

        // Print string at HL (length-prefixed)
        self.label("print_string");
        self.emit(0x46);  // LD B, (HL)
        self.emit(0x23);  // INC HL
        self.label("print_string_loop");
        self.emit(0x78);  // LD A, B
        self.emit(0xB7);  // OR A
        self.emit(0xC8);  // RET Z
        self.emit(0x7E);  // LD A, (HL)
        self.call_label("print_char");
        self.emit(0x23);  // INC HL
        self.emit(0x05);  // DEC B
        self.jr_label("print_string_loop");

        // Print newline
        self.label("print_cr");
        self.ld_a_n(b'\r');
        self.call_label("print_char");
        self.ld_a_n(b'\n');
        self.call_label("print_char");
        self.emit(0xC9);  // RET

        // Object stack push (DE = object ptr)
        self.label("obj_push");
        self.emit(0x2A);  // LD HL, (OBJ_STACK_PTR)
        self.emit16(OBJ_STACK_PTR);
        self.emit(0x73);  // LD (HL), E
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D
        self.emit(0x23);  // INC HL
        self.emit(0x22);  // LD (OBJ_STACK_PTR), HL
        self.emit16(OBJ_STACK_PTR);
        self.emit(0xC9);  // RET

        // Object stack pop (returns object ptr in DE)
        self.label("obj_pop");
        self.emit(0x2A);  // LD HL, (OBJ_STACK_PTR)
        self.emit16(OBJ_STACK_PTR);
        self.emit(0x2B);  // DEC HL
        self.emit(0x56);  // LD D, (HL)
        self.emit(0x2B);  // DEC HL
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x22);  // LD (OBJ_STACK_PTR), HL
        self.emit16(OBJ_STACK_PTR);
        self.emit(0xC9);  // RET

        // Allocate object: BC = size, returns pointer in DE
        self.label("alloc");
        self.emit(0x2A);  // LD HL, (heap_ptr)
        self.emit16(HEAP_PTR);
        self.emit(0x54);  // LD D, H
        self.emit(0x5D);  // LD E, L (now DE = HL = old heap ptr)
        // Now DE = old heap ptr (return value), HL still = old heap ptr
        self.emit(0x09);  // ADD HL, BC (HL = new heap ptr)
        self.emit(0x22);  // LD (heap_ptr), HL
        self.emit16(HEAP_PTR);
        self.emit(0xC9);  // RET

        // Initialize heap pointer
        self.label("init_heap");
        self.ld_hl_nn(HEAP_START);
        self.emit(0x22);
        self.emit16(HEAP_PTR);
        self.emit(0xC9);  // RET

        // Binary to BCD conversion
        // Input: HL = 16-bit unsigned binary value
        // Output: DE = pointer to 4-byte BCD result (caller provides buffer)
        // Layout: byte0 (MSB) to byte3 (LSB)
        // For 65535: byte0=0x00, byte1=0x06, byte2=0x55, byte3=0x35
        // For 42:    byte0=0x00, byte1=0x00, byte2=0x00, byte3=0x42
        self.label("bin_to_bcd");
        self.emit(0xD5);  // PUSH DE (save result ptr)
        // Clear result buffer (4 bytes)
        self.emit(0xAF);  // XOR A
        self.emit(0x12);  // LD (DE), A - byte0
        self.emit(0x13);  // INC DE
        self.emit(0x12);  // LD (DE), A - byte1
        self.emit(0x13);  // INC DE
        self.emit(0x12);  // LD (DE), A - byte2
        self.emit(0x13);  // INC DE
        self.emit(0x12);  // LD (DE), A - byte3
        self.emit(0xD1);  // POP DE (restore result ptr)
        self.emit(0xD5);  // PUSH DE (save again for return)

        // Standard packed BCD layout (4 bytes = 8 digits):
        // byte0 = digit7:digit6 (10M:1M) - unused for 16-bit, always 0
        // byte1 = digit5:digit4 (100k:10k) - for 16-bit, high always 0, low = 10000s
        // byte2 = digit3:digit2 (1k:100) - 1000s and 100s
        // byte3 = digit1:digit0 (10:1) - 10s and units

        // Process 10000s digit (low nibble of byte1)
        self.ld_bc_nn(10000);
        self.call_label("bcd_div_digit");
        // A = 10000s digit (0-6 for 16-bit values)
        self.emit(0x13);  // INC DE (point to byte1)
        self.emit(0x12);  // LD (DE), A (store 0/10000s in byte1)

        // Process 1000s digit (high nibble of byte2)
        self.ld_bc_nn(1000);
        self.call_label("bcd_div_digit");
        self.emit(0xCB); self.emit(0x27);  // SLA A x4 (shift to high nibble)
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xF5);  // PUSH AF (save 1000s in high nibble)

        // Process 100s digit (low nibble of byte2)
        self.ld_bc_nn(100);
        self.call_label("bcd_div_digit");
        // A has 100s digit
        self.emit(0xC1);  // POP BC (B = 1000s shifted, C = garbage)
        self.emit(0xB0);  // OR B (combine: B=high nibble, A=low nibble)
        self.emit(0x13);  // INC DE (point to byte2)
        self.emit(0x12);  // LD (DE), A (store 1000s/100s in byte2)

        // Process 10s digit (high nibble of byte3)
        self.ld_bc_nn(10);
        self.call_label("bcd_div_digit");
        self.emit(0xCB); self.emit(0x27);  // SLA A x4
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xCB); self.emit(0x27);
        self.emit(0xF5);  // PUSH AF (save 10s in high nibble)

        // Units digit (low nibble of byte3)
        self.emit(0x7D);  // LD A, L (remainder = units digit)
        self.emit(0xC1);  // POP BC (B = 10s shifted)
        self.emit(0xB0);  // OR B (combine 10s and units)
        self.emit(0x13);  // INC DE (point to byte3)
        self.emit(0x12);  // LD (DE), A (store 10s/units in byte3)

        self.emit(0xD1);  // POP DE (restore original result ptr)
        self.emit(0xC9);  // RET

        // Helper: divide HL by BC, return quotient digit in A, remainder in HL
        // Uses repeated subtraction
        self.label("bcd_div_digit");
        self.emit(0xAF);  // XOR A (digit counter = 0)
        self.label("bcd_div_loop");
        self.emit(0xB7);  // OR A (clear carry for SBC)
        self.emit(0xED); self.emit(0x42);  // SBC HL, BC
        self.emit(0x38);  // JR C, bcd_div_done (if borrow/carry, we over-subtracted)
        self.forward_refs.push((self.pc, "bcd_div_done".to_string()));
        self.emit(0);
        self.emit(0x3C);  // INC A (count successful subtraction)
        self.jr_label("bcd_div_loop");
        self.label("bcd_div_done");
        self.emit(0x09);  // ADD HL, BC (restore after over-subtraction)
        self.emit(0xC9);  // RET

        // BCD add: add 4-byte BCD at (HL) to 4-byte BCD at (DE)
        // Result stored at (DE)
        // Must start from LSB (byte3) and work backwards to propagate carry
        self.label("bcd_add");
        // Move pointers to byte3 (LSB)
        self.emit(0x23);  // INC HL x3
        self.emit(0x23);
        self.emit(0x23);
        self.emit(0x13);  // INC DE x3
        self.emit(0x13);
        self.emit(0x13);
        self.emit(0x06); self.emit(4);  // LD B, 4 (4 bytes)
        self.emit(0xB7);  // OR A (clear carry)
        self.label("bcd_add_loop");
        self.emit(0x1A);  // LD A, (DE)
        self.emit(0x8E);  // ADC A, (HL)
        self.emit(0x27);  // DAA
        self.emit(0x12);  // LD (DE), A
        self.emit(0x2B);  // DEC HL (move to next higher byte)
        self.emit(0x1B);  // DEC DE
        // Preserve carry across loop
        self.emit(0xF5);  // PUSH AF
        self.emit(0x05);  // DEC B
        self.jr_z("bcd_add_done");
        self.emit(0xF1);  // POP AF
        self.jr_label("bcd_add_loop");
        self.label("bcd_add_done");
        self.emit(0xF1);  // POP AF (clean stack)
        self.emit(0xC9);  // RET

        // BCD subtract: subtract 4-byte BCD at (HL) from 4-byte BCD at (DE)
        // Result stored at (DE), i.e., (DE) = (DE) - (HL)
        // Must start from LSB (byte3) and work backwards to propagate borrow
        self.label("bcd_sub");
        // Move pointers to byte3 (LSB)
        self.emit(0x23);  // INC HL x3
        self.emit(0x23);
        self.emit(0x23);
        self.emit(0x13);  // INC DE x3
        self.emit(0x13);
        self.emit(0x13);
        self.emit(0x06); self.emit(4);  // LD B, 4
        self.emit(0xB7);  // OR A (clear carry/borrow)
        self.label("bcd_sub_loop");
        self.emit(0x1A);  // LD A, (DE)
        self.emit(0x9E);  // SBC A, (HL)
        self.emit(0x27);  // DAA
        self.emit(0x12);  // LD (DE), A
        self.emit(0x2B);  // DEC HL
        self.emit(0x1B);  // DEC DE
        self.emit(0xF5);  // PUSH AF
        self.emit(0x05);  // DEC B
        self.jr_z("bcd_sub_done");
        self.emit(0xF1);  // POP AF
        self.jr_label("bcd_sub_loop");
        self.label("bcd_sub_done");
        self.emit(0xF1);  // POP AF
        self.emit(0xC9);  // RET

        // BCD compare: compare 4-byte BCD at (HL) with 4-byte BCD at (DE)
        // Returns: A=0 and Z=1 if equal, A<0 (sign set) if (HL)<(DE), A>0 if (HL)>(DE)
        // Compares from MSB to LSB
        self.label("bcd_compare");
        self.emit(0x06); self.emit(4);  // LD B, 4 (4 bytes)
        self.label("bcd_cmp_loop");
        self.emit(0x1A);  // LD A, (DE) - get DE byte
        self.emit(0x96);  // SUB (HL)   - subtract HL byte
        self.jr_nz("bcd_cmp_done");  // If not equal, done
        self.emit(0x23);  // INC HL
        self.emit(0x13);  // INC DE
        self.emit(0x05);  // DEC B
        self.jr_nz("bcd_cmp_loop");
        // All bytes equal, A=0, Z=1
        self.label("bcd_cmp_done");
        self.emit(0xC9);  // RET

        // push_bool_result: create BCD integer object with value in B (0 or 1)
        // and push to object stack
        self.label("push_bool_result");
        self.emit(0xC5);  // PUSH BC (save result value)
        self.ld_bc_nn(5);  // Allocate 5 bytes
        self.call_label("alloc");
        self.emit(0xEB);  // EX DE, HL (HL = obj ptr)
        self.ld_a_n(TAG_INT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0xAF);  // XOR A
        self.emit(0x77);  // LD (HL), A - byte0 = 0
        self.emit(0x23);  // INC HL
        self.emit(0x77);  // LD (HL), A - byte1 = 0
        self.emit(0x23);  // INC HL
        self.emit(0x77);  // LD (HL), A - byte2 = 0
        self.emit(0x23);  // INC HL
        self.emit(0xC1);  // POP BC (B = result: 0 or 1)
        self.emit(0x70);  // LD (HL), B - byte3 = 0 or 1
        self.emit(0x2B);  // DEC HL x4 to get back to tag
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0xEB);  // EX DE, HL (DE = obj ptr)
        self.call_label("obj_push");
        self.emit(0xC9);  // RET

        // BCD print: print 4-byte packed BCD at (HL)
        // Skips leading zeros
        self.label("bcd_print");
        self.emit(0x06); self.emit(0);  // LD B, 0 (printed flag)
        self.emit(0x0E); self.emit(4);  // LD C, 4 (4 bytes)
        self.label("bcd_print_loop");
        self.emit(0x7E);  // LD A, (HL)
        // High nibble
        self.emit(0xF5);  // PUSH AF (save byte)
        self.emit(0xCB); self.emit(0x3F);  // SRL A x4
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xB0);  // OR B (check if printed yet)
        self.jr_z("bcd_print_skip_hi");
        self.emit(0x06); self.emit(1);  // LD B, 1 (mark printed)
        self.emit(0xF1);  // POP AF
        self.emit(0xF5);  // PUSH AF
        self.emit(0xCB); self.emit(0x3F);  // SRL A x4
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        self.label("bcd_print_skip_hi");
        self.emit(0xF1);  // POP AF
        // Low nibble
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F
        self.emit(0xB0);  // OR B
        self.jr_z("bcd_print_skip_lo");
        self.emit(0x06); self.emit(1);  // LD B, 1
        self.emit(0x7E);  // LD A, (HL)
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        self.label("bcd_print_skip_lo");
        self.emit(0x23);  // INC HL
        self.emit(0x0D);  // DEC C
        self.jr_nz("bcd_print_loop");
        // If nothing printed, print 0
        self.emit(0x78);  // LD A, B
        self.emit(0xB7);  // OR A
        self.emit(0xC0);  // RET NZ
        self.ld_a_n(b'0');
        self.call_label("print_char");
        self.emit(0xC9);  // RET
    }

    fn generate_interpreter(&mut self) {
        // Helper: increment BC_PC by 1
        self.label("bc_pc_inc1");
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL
        self.emit(0x22);  // LD (BC_PC), HL
        self.emit16(BC_PC);
        self.emit(0xC9);  // RET

        // Helper: increment BC_PC by 3 (opcode + 16-bit value)
        self.label("bc_pc_inc3");
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x22);  // LD (BC_PC), HL
        self.emit16(BC_PC);
        self.emit(0xC9);  // RET

        // Helper: increment BC_PC by 5 (opcode + 32-bit value)
        self.label("bc_pc_inc5");
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x23);  // INC HL
        self.emit(0x22);  // LD (BC_PC), HL
        self.emit16(BC_PC);
        self.emit(0xC9);  // RET

        // Main interpreter loop
        // BC_PC memory location holds absolute address of current bytecode
        self.label("interpreter_loop");

        // Load current bytecode opcode
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x7E);  // LD A, (HL) - bytecode opcode

        // Dispatch based on opcode
        self.emit(0xFE); self.emit(0x01);  // CP 1 (PushInt)
        self.jp_z("bc_push_int");

        self.emit(0xFE); self.emit(0x02);  // CP 2 (PushString)
        self.jp_z("bc_push_string");

        self.emit(0xFE); self.emit(0x03);  // CP 3 (PushFloat)
        self.jp_z("bc_push_float");

        self.emit(0xFE); self.emit(0x10);  // CP 16 (PrimAdd)
        self.jp_z("bc_prim_add");

        self.emit(0xFE); self.emit(0x11);  // CP 17 (PrimSub)
        self.jp_z("bc_prim_sub");

        self.emit(0xFE); self.emit(0x12);  // CP 18 (PrimMul)
        self.jp_z("bc_prim_mul");

        self.emit(0xFE); self.emit(0x18);  // CP 24 (PrimFAdd)
        self.jp_z("bc_prim_fadd");

        self.emit(0xFE); self.emit(0x19);  // CP 25 (PrimFSub)
        self.jp_z("bc_prim_fsub");

        self.emit(0xFE); self.emit(0x1A);  // CP 26 (PrimFMul)
        self.jp_z("bc_prim_fmul");

        self.emit(0xFE); self.emit(0x1B);  // CP 27 (PrimFDiv)
        self.jp_z("bc_prim_fdiv");

        self.emit(0xFE); self.emit(0x15);  // CP 21 (PrimLt)
        self.jp_z("bc_prim_lt");

        self.emit(0xFE); self.emit(0x16);  // CP 22 (PrimGt)
        self.jp_z("bc_prim_gt");

        self.emit(0xFE); self.emit(0x17);  // CP 23 (PrimEq)
        self.jp_z("bc_prim_eq");

        self.emit(0xFE); self.emit(0x20);  // CP 32 (PrimPrint)
        self.jp_z("bc_prim_print");

        self.emit(0xFE); self.emit(0x21);  // CP 33 (PrimPrintln)
        self.jp_z("bc_prim_println");

        self.emit(0xFE); self.emit(0xFF);  // CP 255 (Halt)
        self.jp_z("bc_halt");

        // Unknown opcode - halt
        self.jp_label("bc_halt");

        // PushInt: next 2 bytes are the integer value (binary)
        // Convert to 4-byte packed BCD and store in 5-byte object (tag + 4 BCD bytes)
        self.label("bc_push_int");
        // Load BC_PC and read binary integer value
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL (skip opcode, point to low byte)
        self.emit(0x5E);  // LD E, (HL) - low byte
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - high byte
        // DE = binary integer value
        self.emit(0xEB);  // EX DE, HL (HL = binary value)
        self.emit(0xE5);  // PUSH HL (save binary value)
        // Allocate integer object: 5 bytes (tag + 4 BCD bytes)
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = object ptr
        self.emit(0xEB);  // EX DE, HL (HL = object ptr)
        self.ld_a_n(TAG_INT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL (point to BCD data area)
        self.emit(0xEB);  // EX DE, HL (DE = BCD data ptr)
        self.emit(0xE1);  // POP HL (binary value)
        // Convert binary HL to BCD at (DE)
        self.call_label("bin_to_bcd");
        // DE still points to start of BCD data, back up to object start
        self.emit(0x1B);  // DEC DE (back to tag)
        // Push object pointer to stack
        self.call_label("obj_push");
        // Advance BC_PC by 3 (opcode + 2 byte integer)
        self.call_label("bc_pc_inc3");
        self.jp_label("interpreter_loop");

        // PushString: next byte is length, followed by string bytes
        // Bytecode format: [0x02] [len] [char0] [char1] ...
        // Object layout: [tag, len, char0, char1, ...]
        self.label("bc_push_string");

        // Step 1: Read length from bytecode
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL (skip opcode, point to length)
        self.emit(0x7E);  // LD A, (HL) - A = length
        self.emit(0x23);  // INC HL (point to first char)
        // Save source ptr to TEMP1
        self.emit(0x22);  // LD (TEMP1), HL
        self.emit16(TEMP1);

        // Step 2: Allocate object: tag(1) + len(1) + string bytes = length + 2
        self.emit(0xC6); self.emit(2);  // ADD A, 2
        self.emit(0x4F);  // LD C, A (C = size)
        self.emit(0x06); self.emit(0x00);  // LD B, 0
        self.call_label("alloc");
        // DE = object ptr - save to TEMP2
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP2), HL
        self.emit16(TEMP2);

        // Step 3: Write tag to object
        self.ld_a_n(TAG_STRING);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL

        // Step 4: Read length again and write it
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL (skip opcode, point to length)
        self.emit(0x7E);  // LD A, (HL) - A = length
        self.emit(0x4F);  // LD C, A (save length in C)
        // Write length to object
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x77);  // LD (HL), A - store length byte
        self.emit(0x23);  // INC HL (point to first char dest)
        self.emit(0xEB);  // EX DE, HL (DE = dest)

        // Step 5: Copy string bytes
        // C = length, DE = dest, need HL = source
        self.emit(0x2A);  // LD HL, (TEMP1)
        self.emit16(TEMP1);
        self.emit(0x06); self.emit(0x00);  // LD B, 0 (BC = length)
        // LDIR: copy BC bytes from (HL) to (DE)
        self.emit(0x79);  // LD A, C (check if length is 0)
        self.emit(0xB7);  // OR A
        self.jr_z("push_str_done");
        self.emit(0xED); self.emit(0xB0);  // LDIR
        self.label("push_str_done");

        // Step 6: Push object ptr to object stack
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0xEB);  // EX DE, HL (DE = object ptr)
        self.call_label("obj_push");

        // Step 7: Advance BC_PC past opcode + length byte + string bytes
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL (skip opcode)
        self.emit(0x4E);  // LD C, (HL) - length
        self.emit(0x06); self.emit(0x00);  // LD B, 0
        self.emit(0x23);  // INC HL (skip length byte)
        self.emit(0x09);  // ADD HL, BC (skip string bytes)
        self.emit(0x22);  // LD (BC_PC), HL
        self.emit16(BC_PC);
        self.jp_label("interpreter_loop");

        // PushFloat: next 4 bytes are the 16.16 fixed-point value
        self.label("bc_push_float");
        // Load BC_PC and read 32-bit float value
        self.emit(0x2A);  // LD HL, (BC_PC)
        self.emit16(BC_PC);
        self.emit(0x23);  // INC HL (skip opcode)
        // Read low word into DE
        self.emit(0x5E);  // LD E, (HL) - byte 0
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - byte 1
        self.emit(0xD5);  // PUSH DE (save low word)
        self.emit(0x23);  // INC HL
        // Read high word into DE
        self.emit(0x5E);  // LD E, (HL) - byte 2
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - byte 3
        self.emit(0xD5);  // PUSH DE (save high word)
        // Allocate float object: 5 bytes (tag + 4 bytes)
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = object ptr
        self.emit(0xEB);  // EX DE, HL (HL = object ptr)
        self.ld_a_n(TAG_FLOAT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0xD1);  // POP DE (high word)
        self.emit(0x73);  // LD (HL), E - byte 2
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - byte 3
        self.emit(0x23);  // INC HL
        self.emit(0xD1);  // POP DE (low word)
        self.emit(0x73);  // LD (HL), E - byte 0
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - byte 1
        // Get back to object start
        self.emit(0x2B);  // DEC HL
        self.emit(0x2B);  // DEC HL
        self.emit(0x2B);  // DEC HL
        self.emit(0x2B);  // DEC HL
        // Push object pointer to stack
        self.emit(0xEB);  // EX DE, HL (DE = object ptr)
        self.call_label("obj_push");
        // Advance BC_PC by 5 (opcode + 4 byte float)
        self.call_label("bc_pc_inc5");
        self.jp_label("interpreter_loop");

        // PrimAdd: pop two BCD ints, add using DAA, push result
        // Object format: [tag, bcd0, bcd1, bcd2, bcd3] (5 bytes)
        // Uses TEMP1 for arg2 BCD ptr, TEMP2 for result obj ptr
        self.label("bc_prim_add");
        // Pop arg2 and save its BCD data pointer to TEMP1
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0x13);  // INC DE (skip tag, point to BCD)
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP1), HL
        self.emit16(TEMP1);
        // Pop arg1 - keep in HL
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag, point to BCD)
        self.emit(0xE5);  // PUSH HL (save arg1 BCD ptr)
        // Allocate result object (5 bytes)
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = new object ptr, save to TEMP2
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP2), HL
        self.emit16(TEMP2);
        // Write tag
        self.ld_a_n(TAG_INT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL (point to result BCD area)
        self.emit(0xEB);  // EX DE, HL (DE = result BCD ptr)
        // Copy arg1 BCD to result: LDIR copies from (HL) to (DE)
        self.emit(0xE1);  // POP HL (arg1 BCD ptr)
        self.ld_bc_nn(4);
        self.emit(0xED); self.emit(0xB0);  // LDIR: copy 4 bytes
        // Now add arg2 to result
        // DE points past result, need to reset to start of result BCD
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0xEB);  // EX DE, HL (DE = result BCD ptr)
        self.emit(0x2A);  // LD HL, (TEMP1)
        self.emit16(TEMP1);  // HL = arg2 BCD ptr
        // bcd_add: (DE) = (DE) + (HL)
        self.call_label("bcd_add");
        // Push result object to stack
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimSub: subtract BCD using DAA
        // result = arg1 - arg2
        self.label("bc_prim_sub");
        // Pop arg2 (subtrahend) and save to TEMP1
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0x13);  // INC DE (skip tag)
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP1), HL
        self.emit16(TEMP1);
        // Pop arg1 (minuend)
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0xE5);  // PUSH HL (save arg1 BCD ptr)
        // Allocate result object
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = result obj ptr, save to TEMP2
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP2), HL
        self.emit16(TEMP2);
        self.ld_a_n(TAG_INT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0xEB);  // EX DE, HL (DE = result BCD ptr)
        // Copy arg1 to result
        self.emit(0xE1);  // POP HL (arg1 BCD ptr)
        self.ld_bc_nn(4);
        self.emit(0xED); self.emit(0xB0);  // LDIR
        // Subtract arg2 from result
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0xEB);  // EX DE, HL (DE = result BCD ptr)
        self.emit(0x2A);  // LD HL, (TEMP1)
        self.emit16(TEMP1);  // HL = arg2 BCD ptr
        // bcd_sub: (DE) = (DE) - (HL)
        self.call_label("bcd_sub");
        // Push result
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimMul: BCD multiply using repeated addition
        // For simplicity, we convert multiplier to binary and use repeated bcd_add
        // This is slow but correct for small numbers
        self.label("bc_prim_mul");
        // Pop arg2 (multiplier) - we'll convert to binary counter
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0x13);  // INC DE (skip tag)
        self.emit(0xEB);  // EX DE, HL
        // Convert BCD to binary for loop counter
        // BCD layout: byte0=unused, byte1=10000s, byte2=1000s/100s, byte3=10s/units
        // For now, only support multipliers up to 99 (read byte3 only)
        self.emit(0x23);  // INC HL (skip byte0)
        self.emit(0x23);  // INC HL (skip byte1)
        self.emit(0x23);  // INC HL (skip byte2)
        self.emit(0x5E);  // LD E, (HL) - tens/units byte (byte3)
        // Convert packed BCD in E to binary
        // E = 0xTU where T=tens, U=units, value = T*10 + U
        self.emit(0x7B);  // LD A, E
        self.emit(0xE6); self.emit(0x0F);  // AND 0x0F (units)
        self.emit(0x4F);  // LD C, A (save units)
        self.emit(0x7B);  // LD A, E
        self.emit(0xCB); self.emit(0x3F);  // SRL A x4
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);
        self.emit(0xCB); self.emit(0x3F);  // A = tens
        // Multiply tens by 10: A * 10
        self.emit(0x47);  // LD B, A
        self.emit(0x87);  // ADD A, A (A*2)
        self.emit(0x87);  // ADD A, A (A*4)
        self.emit(0x80);  // ADD A, B (A*5)
        self.emit(0x87);  // ADD A, A (A*10)
        self.emit(0x81);  // ADD A, C (A*10 + units)
        self.emit(0x4F);  // LD C, A
        self.emit(0x06); self.emit(0);  // LD B, 0 (BC = multiplier as binary)
        self.emit(0xC5);  // PUSH BC (save multiplier counter)
        // Pop arg1 (multiplicand) and save to TEMP1
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0x13);  // INC DE (skip tag)
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP1), HL
        self.emit16(TEMP1);
        // Allocate result object (initialized to 0)
        self.ld_bc_nn(5);
        self.call_label("alloc");
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x22);  // LD (TEMP2), HL
        self.emit16(TEMP2);
        self.ld_a_n(TAG_INT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        // Clear BCD bytes
        self.emit(0xAF);  // XOR A
        self.emit(0x77);  // LD (HL), A
        self.emit(0x23);  // INC HL
        self.emit(0x77);  // LD (HL), A
        self.emit(0x23);  // INC HL
        self.emit(0x77);  // LD (HL), A
        self.emit(0x23);  // INC HL
        self.emit(0x77);  // LD (HL), A
        // Multiply loop
        self.emit(0xC1);  // POP BC (multiplier counter)
        self.label("bcd_mul_loop");
        self.emit(0x78);  // LD A, B
        self.emit(0xB1);  // OR C
        self.jr_z("bcd_mul_done");
        // Add multiplicand to result
        self.emit(0xC5);  // PUSH BC
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0xEB);  // EX DE, HL (DE = result BCD)
        self.emit(0x2A);  // LD HL, (TEMP1)
        self.emit16(TEMP1);  // HL = multiplicand BCD
        self.call_label("bcd_add");
        self.emit(0xC1);  // POP BC
        self.emit(0x0B);  // DEC BC
        self.jr_label("bcd_mul_loop");
        self.label("bcd_mul_done");
        // Push result
        self.emit(0x2A);  // LD HL, (TEMP2)
        self.emit16(TEMP2);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimFAdd: 32-bit fixed-point addition
        // For simplicity, just add the integer parts (high words)
        // Object layout: [tag, high_lo, high_hi, low_lo, low_hi]
        self.label("bc_prim_fadd");

        // Pop arg2, get its high word (integer part)
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg2 high word
        self.emit(0xD5);  // PUSH DE (save arg2 high)

        // Pop arg1, get its high word
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg1 high word

        // Add high words: DE (arg1) + stack (arg2)
        self.emit(0xE1);  // POP HL (arg2 high)
        self.emit(0x19);  // ADD HL, DE (HL = sum of integer parts)
        self.emit(0xE5);  // PUSH HL (result high word)
        self.ld_hl_nn(0);  // Low word = 0 (no fractional for now)
        self.emit(0xE5);  // PUSH HL (result low word)

        // Allocate result object: 5 bytes
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = object ptr from alloc
        // Stack: low_word (top), high_word
        self.emit(0xE1);  // POP HL (low word)
        self.emit(0xE3);  // EX (SP), HL (swap low word with high word on stack)
        self.emit(0xE5);  // PUSH HL (push low word back, now high word is in HL conceptually)
        self.emit(0xE1);  // POP HL (get high word)
        self.emit(0xD5);  // PUSH DE (save object ptr)
        // Now HL = high word, stack: low_word, obj_ptr
        self.emit(0xEB);  // EX DE, HL (DE = high word, HL from alloc)
        self.emit(0xE1);  // POP HL (obj ptr)
        self.ld_a_n(TAG_FLOAT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0x73);  // LD (HL), E - high word low byte
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - high word high byte
        self.emit(0x23);  // INC HL
        self.emit(0xD1);  // POP DE (low word)
        self.emit(0x73);  // LD (HL), E - low word low byte
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - low word high byte
        self.emit(0x2B);  // DEC HL x4
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimFSub: 32-bit fixed-point subtraction (integer part only)
        self.label("bc_prim_fsub");

        // Pop arg2, get its high word (integer part)
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg2 high word
        self.emit(0xD5);  // PUSH DE (save arg2 high)

        // Pop arg1, get its high word
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg1 high word
        self.emit(0xEB);  // EX DE, HL (HL = arg1 high)

        // Subtract: arg1 - arg2
        self.emit(0xD1);  // POP DE (arg2 high)
        self.emit(0xB7);  // OR A (clear carry)
        self.emit(0xED); self.emit(0x52);  // SBC HL, DE (HL = arg1 - arg2)
        self.emit(0xE5);  // PUSH HL (result high word)
        self.ld_hl_nn(0);  // Low word = 0
        self.emit(0xE5);  // PUSH HL (result low word)

        // Allocate result object: 5 bytes
        self.ld_bc_nn(5);
        self.call_label("alloc");
        // DE = object ptr from alloc
        // Stack: low_word (top), high_word
        self.emit(0xE1);  // POP HL (low word)
        self.emit(0xE3);  // EX (SP), HL (swap low word with high word on stack)
        self.emit(0xE5);  // PUSH HL (push low word back)
        self.emit(0xE1);  // POP HL (get high word)
        self.emit(0xD5);  // PUSH DE (save object ptr)
        self.emit(0xEB);  // EX DE, HL (DE = high word)
        self.emit(0xE1);  // POP HL (obj ptr)
        self.ld_a_n(TAG_FLOAT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0x73);  // LD (HL), E - high word low byte
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - high word high byte
        self.emit(0x23);  // INC HL
        self.emit(0xD1);  // POP DE (low word)
        self.emit(0x73);  // LD (HL), E - low word low byte
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D - low word high byte
        self.emit(0x2B);  // DEC HL x4
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0x2B);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimFMul: 32-bit fixed-point multiply (integer parts only)
        self.label("bc_prim_fmul");

        // Pop arg2, get its high word (integer part)
        self.call_label("obj_pop");  // DE = arg2 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg2 high word
        self.emit(0xD5);  // PUSH DE (save arg2)

        // Pop arg1, get its high word
        self.call_label("obj_pop");  // DE = arg1 object ptr
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x23);  // INC HL (skip tag)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL) - DE = arg1 high word (multiplicand)

        self.emit(0xE1);  // POP HL (arg2 = multiplier)
        // Simple 16x16 multiply: DE * HL -> HL
        self.emit(0x44);  // LD B, H
        self.emit(0x4D);  // LD C, L (BC = multiplier)
        self.emit(0x21); self.emit16(0);  // LD HL, 0 (result)
        self.label("fmul_loop");
        self.emit(0x78);  // LD A, B
        self.emit(0xB1);  // OR C
        self.jr_z("fmul_done");
        self.emit(0x19);  // ADD HL, DE
        self.emit(0x0B);  // DEC BC
        self.jr_label("fmul_loop");
        self.label("fmul_done");
        // HL = result integer part
        self.emit(0xE5);  // PUSH HL (result high word)
        self.ld_hl_nn(0);  // Low word = 0
        self.emit(0xE5);  // PUSH HL (result low word)

        // Allocate result object: 5 bytes
        self.ld_bc_nn(5);
        self.call_label("alloc");
        self.emit(0xE1);  // POP HL (low word)
        self.emit(0xE3);  // EX (SP), HL
        self.emit(0xE5);  // PUSH HL
        self.emit(0xE1);  // POP HL (high word)
        self.emit(0xD5);  // PUSH DE (save object ptr)
        self.emit(0xEB);  // EX DE, HL (DE = high word)
        self.emit(0xE1);  // POP HL (obj ptr)
        self.ld_a_n(TAG_FLOAT);
        self.emit(0x77);  // LD (HL), A - tag
        self.emit(0x23);  // INC HL
        self.emit(0x73);  // LD (HL), E
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D
        self.emit(0x23);  // INC HL
        self.emit(0xD1);  // POP DE (low word)
        self.emit(0x73);  // LD (HL), E
        self.emit(0x23);  // INC HL
        self.emit(0x72);  // LD (HL), D
        self.emit(0x2B); self.emit(0x2B); self.emit(0x2B); self.emit(0x2B);
        self.emit(0xEB);  // EX DE, HL
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimFDiv: 32-bit fixed-point divide (stub - returns 0 for now)
        self.label("bc_prim_fdiv");
        self.call_label("obj_pop");  // discard arg2
        self.call_label("obj_pop");  // discard arg1
        // Push 0.0 result
        self.ld_bc_nn(5);
        self.call_label("alloc");
        self.emit(0xEB);
        self.ld_a_n(TAG_FLOAT);
        self.emit(0x77);
        self.emit(0x23);
        self.emit(0xAF);  // XOR A = 0
        self.emit(0x77); self.emit(0x23); self.emit(0x77); self.emit(0x23);
        self.emit(0x77); self.emit(0x23); self.emit(0x77);
        self.emit(0x2B); self.emit(0x2B); self.emit(0x2B); self.emit(0x2B);
        self.emit(0xEB);
        self.call_label("obj_push");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimLt: BCD less than comparison
        // Returns true (1) if arg1 < arg2, false (0) otherwise
        self.label("bc_prim_lt");
        self.call_label("obj_pop");  // DE = arg2 obj ptr
        self.emit(0x13);  // INC DE (skip tag, point to BCD)
        self.emit(0xD5);  // PUSH DE (save arg2 BCD ptr)
        self.call_label("obj_pop");  // DE = arg1 obj ptr
        self.emit(0x13);  // INC DE (skip tag)
        self.emit(0xE1);  // POP HL (HL = arg2 BCD ptr)
        // Now DE = arg1 BCD, HL = arg2 BCD
        // bcd_compare: A = (DE) - (HL) = arg1 - arg2
        // If arg1 < arg2, A is negative (sign bit set)
        // If arg1 >= arg2, A is 0 or positive
        self.call_label("bcd_compare");
        // Check if arg1 < arg2: A has sign bit set (negative result)
        self.emit(0xB7);  // OR A (set flags)
        self.jr_z("lt_false");  // If equal, not less than
        self.emit(0xFA);  // JP M, lt_true (if negative, arg1 < arg2)
        self.forward_refs.push((self.pc, "lt_true".to_string()));
        self.emit16(0);
        self.jr_label("lt_false");
        self.label("lt_true");
        self.emit(0x06); self.emit(0x01);  // LD B, 1 (result = 1)
        self.jr_label("lt_done");
        self.label("lt_false");
        self.emit(0x06); self.emit(0x00);  // LD B, 0 (result = 0)
        self.label("lt_done");
        // Create BCD result object with value in B (0 or 1)
        self.call_label("push_bool_result");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimGt: BCD greater than comparison
        // Returns true (1) if arg1 > arg2, false (0) otherwise
        self.label("bc_prim_gt");
        self.call_label("obj_pop");  // DE = arg2 obj ptr
        self.emit(0x13);  // INC DE
        self.emit(0xD5);  // PUSH DE
        self.call_label("obj_pop");  // DE = arg1 obj ptr
        self.emit(0x13);  // INC DE
        self.emit(0xE1);  // POP HL (HL = arg2 BCD, DE = arg1 BCD)
        // bcd_compare: A = DE - HL, sign set if DE < HL
        // We want arg1 > arg2, i.e., HL=arg2 < DE=arg1
        // So check if result has sign bit set (DE > HL means arg2 < arg1)
        // Wait, bcd_compare does: A = (DE) - (HL), but we have HL=arg2, DE=arg1
        // A = arg1 - arg2. If sign set, arg1 < arg2. If sign clear and NZ, arg1 > arg2.
        self.call_label("bcd_compare");  // A = arg1 - arg2 (DE - HL)
        // Wait, bcd_compare does SUB (HL) from A where A=(DE), so A = DE_byte - HL_byte
        // Result: if first non-matching byte has DE > HL, A is positive
        // If DE < HL, A is negative (sign bit set)
        // We want arg1 > arg2. DE=arg1, HL=arg2. If arg1 > arg2, first diff byte has arg1 > arg2
        // So A would be positive (no sign). But we need to also check Z flag for equal case.
        // Actually, the compare returns A>0 when (DE)>(HL), A<0 when (DE)<(HL), A=0 when equal
        // We want (DE)>(HL), i.e., A>0 and sign bit clear, and A != 0
        self.emit(0xB7);  // OR A (set flags based on A)
        self.jr_z("gt_false");  // If equal, not greater
        self.emit(0xF2);  // JP P, gt_true (if positive, arg1 > arg2)
        self.forward_refs.push((self.pc, "gt_true".to_string()));
        self.emit16(0);
        self.jr_label("gt_false");
        self.label("gt_true");
        self.emit(0x06); self.emit(0x01);  // LD B, 1
        self.jr_label("gt_done");
        self.label("gt_false");
        self.emit(0x06); self.emit(0x00);  // LD B, 0
        self.label("gt_done");
        self.call_label("push_bool_result");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimEq: BCD equality comparison
        // Returns true (1) if arg1 = arg2, false (0) otherwise
        self.label("bc_prim_eq");
        self.call_label("obj_pop");  // DE = arg2 obj ptr
        self.emit(0x13);  // INC DE
        self.emit(0xD5);  // PUSH DE
        self.call_label("obj_pop");  // DE = arg1 obj ptr
        self.emit(0x13);  // INC DE
        self.emit(0xE1);  // POP HL (HL = arg2 BCD, DE = arg1 BCD)
        self.call_label("bcd_compare");
        // If A = 0 and Z flag set, they're equal
        self.jr_nz("eq_false");
        self.emit(0x06); self.emit(0x01);  // LD B, 1
        self.jr_label("eq_done");
        self.label("eq_false");
        self.emit(0x06); self.emit(0x00);  // LD B, 0
        self.label("eq_done");
        self.call_label("push_bool_result");
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimPrint: print top of stack (no newline)
        self.label("bc_prim_print");
        self.call_label("obj_pop");
        self.emit(0xEB);  // EX DE, HL
        self.emit(0x7E);  // LD A, (HL) - get tag
        self.emit(0xFE); self.emit(TAG_INT);  // CP TAG_INT
        self.jr_nz("print_not_int");
        // Print BCD integer
        self.emit(0x23);  // INC HL (skip tag, point to BCD)
        self.call_label("bcd_print");
        self.jr_label("print_done");
        self.label("print_not_int");
        // Check for string
        self.emit(0xFE); self.emit(TAG_STRING);
        self.jr_nz("print_done");
        self.emit(0x23);  // INC HL
        self.call_label("print_string");
        self.label("print_done");
        // Push nil as result
        self.ld_de_nn(0);
        self.call_label("obj_push");
        // Advance BC_PC by 1
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // PrimPrintln: print with newline
        self.label("bc_prim_println");
        self.call_label("obj_pop");
        self.emit(0xEB);  // EX DE, HL (HL = object ptr)
        self.emit(0x7E);  // LD A, (HL) - get tag
        self.emit(0xFE); self.emit(TAG_INT);
        self.jr_nz("println_not_int");
        // Print BCD integer
        self.emit(0x23);  // INC HL (skip tag, point to BCD data)
        self.call_label("bcd_print");
        self.jr_label("println_cr");
        self.label("println_not_int");
        self.emit(0xFE); self.emit(TAG_FLOAT);
        self.jr_nz("println_not_float");
        // Print float: integer.fraction format
        self.emit(0x23);  // INC HL (skip tag)
        // Read high word (integer part)
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL)
        // Save HL and print integer part
        self.emit(0xE5);  // PUSH HL
        self.emit(0xD5);  // PUSH DE
        self.emit(0xEB);  // EX DE, HL
        self.call_label("print_int");
        // Print decimal point
        self.ld_a_n(b'.');
        self.call_label("print_char");
        // Restore and read fractional part
        self.emit(0xD1);  // POP DE (discard)
        self.emit(0xE1);  // POP HL
        self.emit(0x23);  // INC HL
        self.emit(0x5E);  // LD E, (HL)
        self.emit(0x23);  // INC HL
        self.emit(0x56);  // LD D, (HL)
        // Convert fractional part to decimal (multiply by 10000 / 65536)
        // For simplicity, just divide by 6553.6 to get 4 decimal digits
        // We'll print the high byte as 2 digits for now
        self.emit(0x7A);  // LD A, D (high byte of fraction)
        // Multiply by 100 / 256 ≈ fraction * 0.39
        // Just print 2 digits based on high byte
        self.emit(0x47);  // LD B, A (save)
        self.emit(0x0E); self.emit(100);  // LD C, 100
        // A * 100 / 256 - simple approximation
        // Just use the high nibble * 6 + low nibble / 3
        self.emit(0xCB); self.emit(0x3F);  // SRL A (A >>= 1)
        self.emit(0xCB); self.emit(0x3F);  // SRL A
        self.emit(0xCB); self.emit(0x3F);  // SRL A (A = high byte >> 3 = approx * 0.125)
        // Print as padded digits
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        // Second digit from remainder
        self.emit(0x78);  // LD A, B (restore original)
        self.emit(0xE6); self.emit(0x07);  // AND 0x07 (low 3 bits)
        self.emit(0xC6); self.emit(b'0');  // ADD A, '0'
        self.call_label("print_char");
        self.jr_label("println_cr");
        self.label("println_not_float");
        self.emit(0xFE); self.emit(TAG_STRING);
        self.jr_nz("println_cr");
        self.emit(0x23);  // INC HL
        self.call_label("print_string");
        self.label("println_cr");
        self.call_label("print_cr");
        self.ld_de_nn(0);
        self.call_label("obj_push");
        // Advance BC_PC by 1
        self.call_label("bc_pc_inc1");
        self.jp_label("interpreter_loop");

        // Halt
        self.label("bc_halt");
        self.emit(0x76);  // HALT
        self.jp_label("bc_halt");
    }

    fn jp_z(&mut self, name: &str) {
        self.emit(0xCA);  // JP Z, nn
        self.forward_refs.push((self.pc, name.to_string()));
        self.emit16(0);
    }

    fn generate_bytecode(&mut self, program: &CompiledProgram) -> Result<(), String> {
        for bc in &program.bytecode {
            match bc {
                Bytecode::PushInt(n) => {
                    self.emit(0x01);  // Opcode 1
                    self.emit16(*n as u16);
                }
                Bytecode::PushString(s) => {
                    self.emit(0x02);  // Opcode 2
                    self.emit(s.len() as u8);  // Length byte
                    for b in s.bytes() {
                        self.emit(b);  // String bytes
                    }
                }
                Bytecode::PushFloat(f) => {
                    self.emit(0x03);  // Opcode 3
                    // Emit 32-bit fixed-point value (little-endian)
                    self.emit((*f & 0xFF) as u8);
                    self.emit(((*f >> 8) & 0xFF) as u8);
                    self.emit(((*f >> 16) & 0xFF) as u8);
                    self.emit(((*f >> 24) & 0xFF) as u8);
                }
                Bytecode::PrimAdd => self.emit(0x10),
                Bytecode::PrimSub => self.emit(0x11),
                Bytecode::PrimMul => self.emit(0x12),
                Bytecode::PrimDiv => self.emit(0x13),
                Bytecode::PrimMod => self.emit(0x14),
                Bytecode::PrimLt => self.emit(0x15),
                Bytecode::PrimGt => self.emit(0x16),
                Bytecode::PrimEq => self.emit(0x17),
                Bytecode::PrimFAdd => self.emit(0x18),
                Bytecode::PrimFSub => self.emit(0x19),
                Bytecode::PrimFMul => self.emit(0x1A),
                Bytecode::PrimFDiv => self.emit(0x1B),
                Bytecode::PrimPrint => self.emit(0x20),
                Bytecode::PrimPrintln => self.emit(0x21),
                Bytecode::PrimCr => self.emit(0x22),
                Bytecode::Halt => self.emit(0xFF),
                _ => {
                    // Other bytecodes not yet implemented
                    self.emit(0xFF);  // Treat as halt for now
                }
            }
        }
        Ok(())
    }
}

pub fn generate(program: &CompiledProgram) -> Result<Vec<u8>, String> {
    let mut codegen = CodeGen::new();
    codegen.generate(program)
}
