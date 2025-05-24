// L1 Compiler
// Assembly code generator for fake assembly, similar to the triples discussed in class.
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>
// Implements a 'Convenient Munch' algorithm.

//! Flattens the AST into a series of simple abstract instructions.
use crate::asm::{Dest, Instr, Operand};
use crate::ast;
use std::collections::HashMap;

/// Code generation context that contains a counter for creating new temps,
/// the list of currently-generated instructions, and the mapping from variable
/// names to temps. In a valid L1 program, it's OK to reuse the temp.
pub struct Context {
  pub temp_index: u32,
  pub instrs: Vec<Instr>,
  var_temp_map: HashMap<String, Dest>,
}

impl Context {
  fn new() -> Self {
    Context {
      temp_index: 0,
      instrs: Vec::new(),
      var_temp_map: HashMap::new(),
    }
  }

  /// Create a new temp in this context.
  fn temp(&mut self) -> Dest {
    let result = self.temp_index;
    self.temp_index += 1;
    Dest(result)
  }

  /// Create or fetch a variable.
  fn var(&mut self, var: String) -> Dest {
    match self.var_temp_map.get(&var) {
      Some(d) => *d,
      None => {
        let result = self.temp();
        self.var_temp_map.insert(var, result);
        result
      }
    }
  }

  fn add_instr(&mut self, instr: Instr) {
    self.instrs.push(instr);
  }

  pub fn print_instrs(&self) {
    for (i, inst) in self.instrs.iter().enumerate() {
      println!("{}:\t {}", i, inst);
    }
  }
}

/// Transform an expression into a series of instructions in a context.
fn munch_expr(ctx: &mut Context, dest: Dest, expr: ast::Expr) {
  use ast::Expr::*;
  use Instr::*;
  use Operand::*;

  match expr {
    Number(n) => ctx.add_instr(Mov {
      dest,
      src: Const(n),
    }),
    Variable(v) => {
      let src = ctx.var(v);
      if dest != src {
        let src = Temp(src);
        ctx.add_instr(Mov { dest, src })
      }
    }
    Binop(lhs, op, rhs) => {
      let (src1, src2) = (ctx.temp(), ctx.temp());
      munch_expr(ctx, src1, *lhs);
      munch_expr(ctx, src2, *rhs);
      ctx.add_instr(BinOp {
        op,
        dest,
        src1: Temp(src1),
        src2: Temp(src2),
      });
    }
    Unop(op, rhs) => {
      let src = ctx.temp();
      munch_expr(ctx, src, *rhs);
      ctx.add_instr(Instr::UnOp {
        op,
        dest,
        src: Temp(src),
      });
    }
  }
}

/// Transform a statement into a series of instructions in a context.
fn munch_stmt(ctx: &mut Context, stmt: ast::Stmt) {
  use ast::Stmt::*;
  use Operand::*;

  match stmt {
    Decl(_, var) => {
      ctx.var(var);
    }
    Defn(_, var, expr) => {
      let dest = ctx.var(var);
      munch_expr(ctx, dest, expr);
    }
    Asgn(var, op, expr) => {
      let dest = ctx.var(var);
      match op {
        // Overwrite the variable directly
        None => munch_expr(ctx, dest, expr),
        Some(op) => {
          // Use it's existing value responsibly.
          let src2 = ctx.temp();
          munch_expr(ctx, src2, expr);

          let asop = Instr::BinOp {
            op: ast::to_op(op),
            dest,
            src1: Temp(dest),
            src2: Temp(src2),
          };
          ctx.add_instr(asop);
        }
      }
    }
    Ret(expr) => {
      let dest = ctx.temp();
      munch_expr(ctx, dest, expr);
      ctx.add_instr(Instr::Return(Temp(dest)));
    }
    Block(stmts) => {
      munch_stmts(ctx, stmts);
    }
  };
}

fn munch_stmts(ctx: &mut Context, stmts: Vec<ast::Stmt>) {
  for stmt in stmts {
    munch_stmt(ctx, stmt);
  }
}

/// Convert an AST into an abstract-assembly IR.
pub fn munch_ast(program: ast::Program) -> Context {
  let mut ctx = Context::new();
  munch_stmts(&mut ctx, program);
  ctx
}
