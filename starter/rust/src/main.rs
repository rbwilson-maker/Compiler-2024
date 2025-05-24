// L1 Compiler
//! Top Level Environment
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

mod args;
mod asm;
mod ast;
mod c0;
mod checkpoint;
mod codegen;
mod emit;
mod error;
mod lex;
mod parse;
mod tc;

use args::EmitTarget;
use std::thread;
use std::time;

use crate::parse::parser;

fn main() {
  let cfg = args::parse_args();

  /// Take a filename and parse it into an AST!
  fn make_program(filename: &str) -> std::result::Result<ast::Program, error::Error> {
    parser::parse(filename.to_string()).map_err(|e| error::Error::Message(e))
  }

  // Helper macro to time evaluating an expression (like a function call.)
  macro_rules! time {
    ( $x:expr ) => {{
      let t1 = time::SystemTime::now();
      let result = $x;
      (result, t1.elapsed().unwrap())
    }};
  }

  // We call with a large stack to avoid any overflows caused
  // by overzealous 15-411 students with large test cases.
  let child = thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      let filename = &cfg.file.unwrap();
      if cfg.regalloc_only {
        checkpoint::allocate(filename);
        return 0;
      }
      let (program, parse_time) = time!(make_program(filename));
      let program = match program {
        Ok(prog) => prog,
        Err(e) => {
          eprintln!("{}", e);
          return 1; // Parse failed!
        }
      };

      let (valid_ast, tc_time) = time!(tc::valid_ast(&program));
      if cfg.dump_ast {
        println!("{:?}", program);
      }

      if !valid_ast {
        eprintln!("Invalid AST!");
        return 1; // Tc failed (sad!)
      }

      if cfg.tc_only {
        println!("Pass typecheck.");
        return 0;
      }

      let (ctx, cg_time) = time!(codegen::munch_ast(program));
      if cfg.dump_assem {
        ctx.print_instrs();
      }

      if cfg.verbose {
        println!("Parse time: {} us", parse_time.as_micros());
        println!("Typecheck: {} us", tc_time.as_micros());
        println!("Codegen: {} us", cg_time.as_micros());
      }

      match cfg.emit {
        EmitTarget::Abstract => emit::emit_abs(filename, ctx).is_err() as i32,
        EmitTarget::X86 => {
          eprintln!("Target x86-64 not yet supported!");
          1
        }
      }
    })
    .unwrap();
  // Return the value from the child thread as the return value of the compiler.
  std::process::exit(child.join().expect("Couldn't join spawned thread"));
}
