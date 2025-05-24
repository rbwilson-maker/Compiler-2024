// L1 Compiler
//! Type Checker
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

// Simple typechecker that checks three properties:
//  (1) If a variable is initialized, it has previously been declared.
//  (2) If a variable is used, it has previously been initialized.
//  (3) Every program has a return statement.
// This is sufficient for now, since only int types are available in L1.

use crate::ast;
use std::collections::HashMap;

/// L1 doesn't have all that much in the way of type information.
#[derive(Copy, Clone, Debug)]
struct Status {
  typ: ast::Typ,
  defn: bool,
}

/// We use a mutable type-checking context, modified as the typechecker progresses.
/// L1 does not support nested scopes, so we keep everything at the same level.
struct Context {
  table: HashMap<ast::Var, Status>,
}

impl Context {
  fn new() -> Self {
    Context {
      table: HashMap::new(),
    }
  }
  /// Search through all of the currently available scopes for
  /// this variable, starting from the deepest and going up.
  fn get(&mut self, var: &str) -> Option<Status> {
    self.table.get(var).copied()
  }

  /// Declare a variable, in the current scope.
  fn declare(&mut self, s: ast::Var, t: ast::Typ) {
    let status = Status {
      typ: t,
      defn: false,
    };
    self.table.insert(s, status);
  }

  /// Define a variable, in the current scope.
  fn define(&mut self, s: ast::Var, t: ast::Typ) {
    let status = Status { typ: t, defn: true };
    self.table.insert(s, status);
  }

  /// Create scope with all declared variables defined in it.
  fn define_all(&mut self) {
    for (_, status) in self.table.iter_mut() {
      status.defn = true
    }
  }

  fn is_declared(&self, var: &str) -> bool {
    self.table.get(var).is_some()
  }

  fn is_defined(&self, var: &str) -> bool {
    matches!(self.table.get(var), Some(Status { defn: true, .. }))
  }
}

// Since this is L1, everything is an int. We don't need to return any
// information except a boolean of validity. Later on, we might use types.
fn tc_expr(ctx: &Context, expr: &ast::Expr) -> bool {
  use ast::Expr::*;

  match *expr {
    Number(_) => true,
    Variable(ref var) => ctx.is_defined(var),
    Unop(_, ref rhs) => tc_expr(ctx, rhs),
    Binop(ref lhs, _, ref rhs) => tc_expr(ctx, lhs) && tc_expr(ctx, rhs),
  }
}

/// Check the validity of a statement.
// In future labs, you will likely want to extend the context and the return
// type to contain much more useful information than this.
fn tc_stmt(ctx: &mut Context, stmt: &ast::Stmt) -> (bool, bool) {
  use ast::Stmt::*;

  match *stmt {
    Decl(typ, ref var) => {
      if !ctx.is_declared(var) {
        ctx.declare(var.clone(), typ);
        (true, false)
      } else {
        eprintln!("Variable {} already declared in stmt {:?}", var, stmt);
        (false, false)
      }
    }
    Defn(typ, ref var, ref expr) => {
      if !ctx.is_declared(var) && tc_expr(ctx, expr) {
        ctx.define(var.clone(), typ);
        (true, false)
      } else {
        eprintln!("Invalid stmt {:?}", stmt);
        (false, false)
      }
    }
    // If it's defined, anything goes. If it's just declared, we're not allowed to use its value.
    Asgn(ref var, op, ref expr) => match (ctx.get(var), op) {
      (Some(Status { typ, defn: true }), _) | (Some(Status { typ, defn: false }), None) => {
        if tc_expr(ctx, expr) {
          ctx.define(var.clone(), typ);
          (true, false)
        } else {
          eprintln!("Invalid asnop: {:?}", stmt);
          (false, false)
        }
      }
      status => {
        eprintln!("Invalid asnop {:?} : [{:?}]", stmt, status);
        (false, false)
      }
    },
    Ret(ref expr) => {
      if tc_expr(ctx, expr) {
        ctx.define_all();
        (true, true)
      } else {
        eprintln!("Invalid expr: return {:?}", expr);
        (false, true)
      }
    }
    // We allow things to be defined and declared in a block only
    // if that block contains a return (i.e. we can never return
    // to a higher scope on this control flow path.)
    Block(ref block_stms) => {
      let (ok, ret) = tc_stmts(ctx, block_stms);
      if !ret {
        for stmt in block_stms {
          match stmt {
            Decl(..) | Defn(..) => {
              eprintln!("L1 doesn't support nested scopes!");
              return (false, false);
            }
            _ => (),
          }
        }
        (true, false)
      } else {
        (ok, ret)
      }
    }
  }
}

/// Validate a series of statements, updating the context of defined variables
/// according to the results we have from validating each statement.
fn tc_stmts(ctx: &mut Context, stmts: &[ast::Stmt]) -> (bool, bool) {
  let mut has_ret = false;
  for stmt in stmts.iter() {
    let (ok, ret) = tc_stmt(ctx, stmt);
    has_ret |= ret;
    if !ok {
      eprintln!("Invalid statement: {:?}", stmt);
      return (false, has_ret);
    }
  }
  (true, has_ret)
}

/// Typecheck!
pub fn valid_ast(stmts: &[ast::Stmt]) -> bool {
  let mut ctx = Context::new();
  let (ok, ret) = tc_stmts(&mut ctx, stmts);
  ok && ret
}
