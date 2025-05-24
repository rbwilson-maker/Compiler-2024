// L1 Compiler
//! Parser
// Author: Miles Conn <mconn@andrew.cmu.edu>

// We rely on [lalrpop](https://github.com/lalrpop/lalrpop) for parsing.
// Lalrpop generates a LR(1) paraser the grammar can be found in c0.lalrpop
// and the generated code in c0.rs

pub mod parser {
  use logos::{Logos, Span};
  use std::fs;

  use crate::ast::Program;
  use crate::c0;
  use crate::lex::Token;

  fn parse_string(input: String) -> Result<Program, String> {
    // You'll need lexer_with_extras later trust me :)
    let lex_stream = Token::lexer_with_extras(&input, ())
      .spanned()
      .map(|(t, y): (Token, Span)| (y.start, t, y.end));

    c0::ProgramParser::new()
      .parse(lex_stream)
      .map_err(|e| format!("Couldn't parse file. Failed with message {:?}", e))
  }
  pub fn parse(file_name: String) -> Result<Program, String> {
    let str_file = fs::read_to_string(file_name).expect("Couldn't read file");

    parse_string(str_file)
  }
}
