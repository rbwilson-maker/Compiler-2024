// L1 Compiler
//! File emission
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use crate::codegen;
use std::env;
use std::fs::File;
use std::io::prelude::*;

// Fetches the env variable name. For those developing on Macs, you'll
// want to export the UNAME env variable as 'Darwin' in ~/.bashrc or ~/.bash_profile.
#[allow(dead_code)]
fn get_main() -> &'static str {
  if env::var("UNAME") == Ok(String::from("Darwin")) {
    "__c0_main"
  } else {
    "_c0_main"
  }
}

#[allow(dead_code)]
pub fn emit_x86(filename: &str) -> std::io::Result<()> {
  let main = get_main();
  let mut file = File::create(format!("{}.s", filename)).unwrap();
  writeln!(file, ".file\t{:?}\t", filename)?;
  writeln!(file, ".globl {}\n", main)?;
  writeln!(file, "{}:\n", main)?;
  // Your x86 would look great here!
  writeln!(file, ".ident\t\"Rust Interpreter\"\n")?;
  Ok(())
}

pub fn emit_abs(filename: &str, ctx: codegen::Context) -> std::io::Result<()> {
  let mut file = File::create(format!("{}.abs", filename)).unwrap();
  writeln!(file, "// 15-411 L1 Compiler\n")?;
  for ins in ctx.instrs {
    writeln!(file, "{}", ins)?;
  }
  Ok(())
}
