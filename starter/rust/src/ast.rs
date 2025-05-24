// L1 Compiler
//! Abstract Syntax Trees
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>
#![allow(clippy::enum_variant_names)]

use std::fmt::{Debug, Display, Error, Formatter};

pub type Var = String;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Typ {
  Int,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
  Add, // +
  Sub, // -
  Mul, // *
  Div, // /
  Mod, // %
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
  Neg, // -
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AsnOp {
  PlusEq,  // +=
  MinusEq, // -=
  TimesEq, // *=
  DivEq,   // /=
  ModEq,   // %=
}

/// Helper function to convert assignment operators to their original operator.
pub fn to_op(op: AsnOp) -> BinOp {
  use AsnOp::*;
  use BinOp::*;

  match op {
    PlusEq => Add,
    MinusEq => Sub,
    TimesEq => Mul,
    DivEq => Div,
    ModEq => Mod,
  }
}

pub type Program = Vec<Stmt>;

/// Local statement (declare variable, define, assign, block, etc.)
#[derive(Clone, Debug)]
pub enum Stmt {
  Decl(Typ, Var),
  Defn(Typ, Var, Expr),
  Asgn(Var, Option<AsnOp>, Expr),
  Block(Vec<Stmt>),
  Ret(Expr),
}

/// Expression tree.
// This uses boxes to enable the recursive type. Pattern-matching against
// boxes requires #[feature(box_patterns)], which exists only in nightly Rust.
// You may want to adjust the starter code to make use of this, or adopt
// a different approach entirely.
// https://doc.rust-lang.org/unstable-book/language-features/box-patterns.html
#[derive(Clone, Debug)]
pub enum Expr {
  Number(i32),
  Variable(Var),
  Binop(Box<Expr>, BinOp, Box<Expr>),
  Unop(UnOp, Box<Expr>),
}

// Some display functionality. You'll likely want to implement the Display
// trait in a way that you can print your compiler's version of the AST
// in a way that closely resembles indented and annotated source code.

impl Display for Typ {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      Typ::Int => write!(fmt, "int"),
    }
  }
}

impl Display for BinOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      BinOp::Mul => write!(fmt, "*"),
      BinOp::Div => write!(fmt, "/"),
      BinOp::Mod => write!(fmt, "%"),
      BinOp::Add => write!(fmt, "+"),
      BinOp::Sub => write!(fmt, "-"),
    }
  }
}

impl Display for UnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      UnOp::Neg => write!(fmt, "-"),
    }
  }
}

impl Display for AsnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      AsnOp::TimesEq => write!(fmt, "*="),
      AsnOp::DivEq => write!(fmt, "/="),
      AsnOp::ModEq => write!(fmt, "%="),
      AsnOp::PlusEq => write!(fmt, "+="),
      AsnOp::MinusEq => write!(fmt, "-="),
    }
  }
}

impl Debug for Typ {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}

impl Debug for BinOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}
impl Debug for UnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}

impl Debug for AsnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}
