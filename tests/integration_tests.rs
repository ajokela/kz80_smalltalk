use std::process::Command;
use std::fs;
use std::sync::atomic::{AtomicUsize, Ordering};

const EMULATOR: &str = "/Users/alexjokela/projects/retroshield-arduino/kz80/emulator/retroshield";
const COMPILER: &str = env!("CARGO_BIN_EXE_kz80_smalltalk");

static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn compile_and_run(source: &str) -> String {
    // Use unique file names for each test to avoid race conditions
    let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
    let tid = std::thread::current().id();
    let source_file = format!("/tmp/test_smalltalk_{:?}_{}.st", tid, id);
    let binary_file = format!("/tmp/test_smalltalk_{:?}_{}.bin", tid, id);

    // Write source
    fs::write(&source_file, source).expect("Failed to write source file");

    // Compile
    let compile_result = Command::new(COMPILER)
        .args([&source_file, "-o", &binary_file])
        .output()
        .expect("Failed to run compiler");

    if !compile_result.status.success() {
        let _ = fs::remove_file(&source_file);
        panic!("Compilation failed: {}", String::from_utf8_lossy(&compile_result.stderr));
    }

    // Run emulator
    let run_result = Command::new(EMULATOR)
        .args(["-l", &binary_file])
        .output()
        .expect("Failed to run emulator");

    // Cleanup
    let _ = fs::remove_file(&source_file);
    let _ = fs::remove_file(&binary_file);

    let output = String::from_utf8_lossy(&run_result.stdout).to_string();

    // The output is: "Tiny Smalltalk on Z80\r\n<result>\r\n"
    // We want the second-to-last line (before the final empty line), which is the result
    let lines: Vec<&str> = output.lines().collect();

    // Find the line after "Tiny Smalltalk on Z80"
    for (i, line) in lines.iter().enumerate() {
        if line.contains("Tiny Smalltalk on Z80") && i + 1 < lines.len() {
            return lines[i + 1].to_string();
        }
    }

    // Fallback: return last non-empty line
    lines.iter().rev().find(|l| !l.is_empty()).unwrap_or(&"").to_string()
}

// Integer literal tests
#[test]
fn test_integer_zero() {
    assert_eq!(compile_and_run("0"), "0");
}

#[test]
fn test_integer_single_digit() {
    assert_eq!(compile_and_run("7"), "7");
}

#[test]
fn test_integer_two_digits() {
    assert_eq!(compile_and_run("42"), "42");
}

#[test]
fn test_integer_three_digits() {
    assert_eq!(compile_and_run("123"), "123");
}

#[test]
fn test_integer_five_digits() {
    assert_eq!(compile_and_run("12345"), "12345");
}

#[test]
fn test_integer_max() {
    assert_eq!(compile_and_run("65535"), "65535");
}

// Addition tests
#[test]
fn test_add_simple() {
    assert_eq!(compile_and_run("1 + 2"), "3");
}

#[test]
fn test_add_with_carry() {
    assert_eq!(compile_and_run("99 + 1"), "100");
}

#[test]
fn test_add_multi_carry() {
    assert_eq!(compile_and_run("999 + 1"), "1000");
}

#[test]
fn test_add_large() {
    assert_eq!(compile_and_run("50000 + 15535"), "65535");
}

#[test]
fn test_add_zeros() {
    assert_eq!(compile_and_run("0 + 0"), "0");
}

// Subtraction tests
#[test]
fn test_sub_simple() {
    assert_eq!(compile_and_run("5 - 3"), "2");
}

#[test]
fn test_sub_with_borrow() {
    assert_eq!(compile_and_run("100 - 1"), "99");
}

#[test]
fn test_sub_multi_borrow() {
    assert_eq!(compile_and_run("1000 - 1"), "999");
}

#[test]
fn test_sub_to_zero() {
    assert_eq!(compile_and_run("42 - 42"), "0");
}

#[test]
fn test_sub_large() {
    assert_eq!(compile_and_run("65535 - 1"), "65534");
}

// Multiplication tests
#[test]
fn test_mul_simple() {
    assert_eq!(compile_and_run("2 * 3"), "6");
}

#[test]
fn test_mul_by_one() {
    assert_eq!(compile_and_run("42 * 1"), "42");
}

#[test]
fn test_mul_by_zero() {
    assert_eq!(compile_and_run("42 * 0"), "0");
}

#[test]
fn test_mul_larger() {
    assert_eq!(compile_and_run("12 * 11"), "132");
}

#[test]
fn test_mul_max_multiplier() {
    assert_eq!(compile_and_run("99 * 99"), "9801");
}

// Equality tests
#[test]
fn test_eq_true() {
    assert_eq!(compile_and_run("3 = 3"), "1");
}

#[test]
fn test_eq_false() {
    assert_eq!(compile_and_run("3 = 5"), "0");
}

#[test]
fn test_eq_large_true() {
    assert_eq!(compile_and_run("12345 = 12345"), "1");
}

#[test]
fn test_eq_large_false() {
    assert_eq!(compile_and_run("12345 = 12346"), "0");
}

// Less than tests
#[test]
fn test_lt_true() {
    assert_eq!(compile_and_run("3 < 5"), "1");
}

#[test]
fn test_lt_false() {
    assert_eq!(compile_and_run("5 < 3"), "0");
}

#[test]
fn test_lt_equal() {
    assert_eq!(compile_and_run("5 < 5"), "0");
}

#[test]
fn test_lt_large() {
    assert_eq!(compile_and_run("1000 < 10000"), "1");
}

// Greater than tests
#[test]
fn test_gt_true() {
    assert_eq!(compile_and_run("5 > 3"), "1");
}

#[test]
fn test_gt_false() {
    assert_eq!(compile_and_run("3 > 5"), "0");
}

#[test]
fn test_gt_equal() {
    assert_eq!(compile_and_run("5 > 5"), "0");
}

#[test]
fn test_gt_large() {
    assert_eq!(compile_and_run("10000 > 1000"), "1");
}

// String tests
#[test]
fn test_string_hello() {
    assert_eq!(compile_and_run("'Hello'"), "Hello");
}

#[test]
fn test_string_with_spaces() {
    assert_eq!(compile_and_run("'Hello World'"), "Hello World");
}

// Complex expressions
#[test]
fn test_chained_add() {
    assert_eq!(compile_and_run("1 + 2 + 3"), "6");
}

#[test]
fn test_chained_sub() {
    assert_eq!(compile_and_run("10 - 3 - 2"), "5");
}

#[test]
fn test_mixed_ops() {
    // In Smalltalk, operators are left-to-right: (1 + 2) * 3 = 9
    assert_eq!(compile_and_run("1 + 2 * 3"), "9");
}
