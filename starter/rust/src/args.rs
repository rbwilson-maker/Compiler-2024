// L1 Compiler
//! Parse command line arguments
//! We expect this to be good enough for this course.
//! You could also use something like clap at the expense of bad compile times.
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use std::env;

pub enum EmitTarget {
  Abstract,
  X86,
}

/// Configuration options for this compiler run.
pub struct Config {
  pub verbose: bool,
  pub regalloc_only: bool,
  pub tc_only: bool,
  pub dump_ast: bool,
  pub dump_assem: bool,

  pub emit: EmitTarget,
  pub file: Option<String>,
}

impl Config {
  /// Set your defaults here!
  fn default() -> Self {
    Config {
      verbose: false,       // Get extra output from the compiler
      regalloc_only: false, // Register allocate for checkpoint
      tc_only: false,       // Stop after parsing & checking types.
      dump_ast: false,      // Print the AST
      dump_assem: false,    // Print the generated abstract assembly.

      emit: EmitTarget::Abstract, // Type of file to output
      file: None,                 // Source file to compile.
    }
  }
}

/// Parses command line input into a configuration. Panics on invalid args.
pub fn parse_args() -> Config {
  let args: Vec<String> = env::args().collect();
  let mut config = Config::default();
  let mut index = 1;
  while index < args.len() {
    match args[index].as_str() {
      "-v" | "--verbose" => config.verbose = true,
      "--dump-ast" => config.dump_ast = true,
      "--dump-assem" => config.dump_assem = true,
      "-t" | "--typecheck-only" => config.tc_only = true,
      "-r" | "--regalloc-only" => config.regalloc_only = true,
      "-e" | "--emit" => {
        // Allow for the emit type to be the next space-delimited token.
        if index + 1 < args.len() {
          match args[index + 1].as_str() {
            "abs" => config.emit = EmitTarget::Abstract,
            "x86-64" => config.emit = EmitTarget::X86,
            other => {
              panic!("Unkown emit type : {}", other);
            }
          };
          index += 1;
        } else {
          panic!("Expected emit type");
        };
      }
      // Account for funky spacing in the grading script.
      "-eabs" => config.emit = EmitTarget::Abstract,
      "-ex86-64" => config.emit = EmitTarget::X86,
      file => {
        if let Some('-') = file.chars().next() {
        } else {
          config.file = Some(file.to_string())
        }
      }
    };
    index += 1;
  }

  if config.file.is_none() {
    panic!("Expected file input");
  }

  config
}
