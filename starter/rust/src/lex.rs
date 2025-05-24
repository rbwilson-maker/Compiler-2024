// L1 Compiler
//! Lexer
// Author: Miles Conn <mconn@andrew.cmu.edu>

// Update this file to lex the necessary keywords and other tokens
// in order to make the grammar forward compatible with C0.
// Note this project relies on logos 0.12.1 see docs [here]
// (https://docs.rs/logos/0.12.1/logos/index.html)

#![allow(clippy::upper_case_acronyms)]
use enum_as_inner::EnumAsInner;
use logos::{Lexer, Logos, Skip};
use std::{fmt, num::ParseIntError};
use strum_macros::AsRefStr;

// Handles parsing a hex string without the leading 0x
fn from_hex_string(without_prefix: &str) -> Result<i64, String> {
  // For Rust reasons we parse as i64 and then cast
  let res = u64::from_str_radix(without_prefix, 16);
  if let Ok(x) = res {
    if x <= 0xffffffff {
      Ok(x as i64)
    } else {
      // You're not allowed to pass an ParseIntError
      Err(format!("{} is > 0xffffffff", without_prefix))
    }
  } else {
    Err(format!(
      "Failed Conversion with Error {:?}",
      res.unwrap_err()
    ))
  }
}

fn from_hex<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<i64, String> {
  let slice = lex.slice();
  // Skip over the 0x in a hex number
  let without_prefix = &slice[2..slice.len()];
  from_hex_string(without_prefix)
}

fn from_num<'b>(lex: &mut Lexer<'b, Token<'b>>) -> Result<i64, String> {
  let slice = lex.slice();

  let res: Result<i64, ParseIntError> = slice.parse();

  if res.is_err() {
    return Err(format!("Parsing failed wtih Error {:?}", res.unwrap_err()));
  }
  let out = res.unwrap();
  if out > ((i32::MIN as i64).abs()) {
    // All numbers are positive because - is lexed seperately
    return Err(format!("Number {} is out of bounds", out));
  }
  Ok(out)
}

fn skip_multi_line_comments<'b>(lex: &mut Lexer<'b, Token<'b>>) -> Skip {
  use logos::internal::LexerInternal;
  let mut balanced_comments: isize = 1;
  if lex.slice() == "/*" {
    loop {
      // Read the current value
      let x: Option<u8> = lex.read();
      match x {
        Some(0) => panic!("Reached end of file or not?"),
        Some(b'*') => {
          lex.bump_unchecked(1);
          if let Some(b'/') = lex.read() {
            lex.bump_unchecked(1);
            balanced_comments -= 1;
            if balanced_comments == 0 {
              // No more comments
              break;
            }
          }
        }
        Some(b'/') => {
          lex.bump(1);
          if let Some(b'*') = lex.read() {
            lex.bump_unchecked(1);
            // We just started a new comment
            balanced_comments += 1;
          }
        }
        None => panic!("Reached end of file"),
        _ => {
          lex.bump_unchecked(1);
        }
      }
    }
  }
  Skip
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:#?}", self)
  }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Logos, Debug, PartialEq, AsRefStr, EnumAsInner)]
#[logos(subpattern identifier = r"[A-Za-z_][A-Za-z0-9_]*")]
pub enum Token<'a> {
  #[regex(r"(?&identifier)")]
  Ident(&'a str),

  #[regex(r"0|[1-9][0-9]*", from_num)]
  #[regex(r"0[xX][0-9a-fA-F]+", from_hex)]
  Number(i64),

  #[token("-")]
  Minus,
  #[token("+")]
  Plus,
  #[token("*")]
  Asterisk,
  #[token("/")]
  Div,
  #[token("%")]
  Mod,
  #[token("=")]
  Assgn,
  #[token("+=")]
  PlusEq,
  #[token("-=")]
  MinusEq,
  #[token("*=")]
  TimesEq,
  #[token("/=")]
  DivEq,
  #[token("%=")]
  ModEq,
  #[token("--")]
  MinusMinus,
  #[token("(")]
  LParen,
  #[token(")")]
  RParen,
  #[token(";")]
  Semicolon,
  #[token(",")]
  COMMA,

  // Reserved Keywords
  #[token("return")]
  Return,
  #[token("continue")]
  CONTINUE,
  #[token("break")]
  BREAK,
  #[token("true")]
  TRUE,
  #[token("false")]
  FALSE,
  #[token("bool")]
  bool,
  #[token("if")]
  IF,
  #[token("else")]
  ELSE,
  #[token("while")]
  WHILE,
  #[token("for")]
  FOR,
  #[token("int")]
  Int,
  #[token("void")]
  void,
  #[token("{")]
  LBrace,
  #[token("}")]
  RBrace,
  #[token("NULL")]
  NULL,
  #[token("struct")]
  Struct,
  #[token("typedef")]
  Typedef,

  #[regex(r"\s*", logos::skip)]
  #[regex(r#"(//)[^\n]*"#, logos::skip)] // Regex for a single line comment
  // Yes there is regex for this no I could not get it to work
  #[token("/*", skip_multi_line_comments)] // Match start of multiline
  Comment,

  #[token("assert")]
  ASSERT,
  #[token("alloc")]
  ALLOC,
  #[token("alloc_array")]
  ALLOC_ARRAY,

  #[error]
  #[token("char")]
  #[token("string")]
  #[regex(r"[ \t\n\v\r\f]", logos::skip)] // Whitespace
  #[regex(r#"[^\x00-\x7F]"#)] // Error on non ascii characters
  Error,
}
