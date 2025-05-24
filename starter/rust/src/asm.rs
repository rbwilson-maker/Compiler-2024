// L1 Compiler
//! Abstract Assembly Type (Triples)
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use crate::ast;
use std::fmt::{Display, Error, Formatter};

/// Argument (can be either a temp or a constant.)
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Operand {
  Const(i32),
  Temp(Dest),
}

/// Destination (Temp number)
// This is its own type, so it doesn't get mixed up with
// any other u32s that might be flying around...
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Dest(pub u32);

/// Abstract Assembly Instruction
pub enum Instr {
  BinOp {
    op: ast::BinOp,
    dest: Dest,
    src1: Operand,
    src2: Operand,
  },
  UnOp {
    op: ast::UnOp,
    dest: Dest,
    src: Operand,
  },
  Mov {
    dest: Dest,
    src: Operand,
  },
  Return(Operand),
}

// Pretty Printing.

impl Display for Operand {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use Operand::*;
    match *self {
      Const(n) => write!(fmt, "${}", n),
      Temp(s) => write!(fmt, "T{}", s.0),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      Instr::BinOp {
        op,
        dest,
        src1,
        src2,
      } => write!(fmt, "T{} <- {} {} {}", dest.0, src1, op, src2),
      Instr::UnOp { op, dest, src } => write!(fmt, "T{} <- {}{}", dest.0, op, src),
      Instr::Mov { dest, src } => write!(fmt, "T{} <- {}", dest.0, src),
      Instr::Return(dest) => write!(fmt, "RET {}", dest),
    }
  }
}
