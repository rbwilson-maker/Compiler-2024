// this file contains all the routines used to translate from AST/CST -> IR

#include "cst.h"
#include "bb.h"
#include "cfg.h"

// translate a series of statements into basic IR
bool Statements::translate(CFG* cfg) {
  // translate CST to IR
  BasicBlock* bb = new BasicBlock(scope);
  cfg->add(bb);
  for (auto it = stmts.rbegin(); it != stmts.rend(); it++) {
    (*it)->translate(bb);
  }
  return true;
}

// translate AST for this function to abstract assembly
bool Function::translate(CFG* cfg) {
  // at this point a function is just a set of statements, so we can
  // just punt and use Statements for now.
  return stmts->translate(cfg);
}

// translate an assignment
void Assignment::translate(BasicBlock* container) {
  Expr* re = rval->getAsRVal(container);
  Expr* le = lval->getAsLVal(container);
  Stmt* s = new MoveStmt(le, re);
  s->setLine(lineno);
  // add to basic block
  container->append(s);
}

// translate a return
void Return::translate(BasicBlock* container) {
  Expr* re = exp->getAsRVal(container);
  Stmt* s = new ReturnStmt(re);
  s->setLine(lineno);
  // add to basic block
  container->append(s);
}

// translate a decl (if there is an initialization expr) otherwise,
// just put in symbol table.
void Declaration::translate(BasicBlock* container) {
  Expr* le = var->getFreshPsuedoReg();
  if (exp == 0) return;
  Expr* re = exp->getAsRVal(container);
  Stmt* s = new MoveStmt(le, re);
  s->setLine(lineno);
  // add to basic block
  container->append(s);
}

// translate this into an LVAL.  No difference between lval and rval
// if we don't care about SSA or some such.
Expr* Identifier::getAsLVal(BasicBlock* container) {
  return getAsRVal(container);
}

// translate this into an RVAL
Expr* Identifier::getAsRVal(BasicBlock* container) {
  return name->getPsuedoReg();
}

// translate this into an RVAL
Expr* IntConstant::getAsRVal(BasicBlock* container) {
  Expr* ir = new Const(Type::getIntType(), intvalue);
  ir->setLine(lineno);
  return ir;
}

// translate this into an RVAL
Expr* Expression::getAsRVal(BasicBlock* container) {
  auto l = left->getAsRVal(container);
  Expr* ir;
  if (right == 0) {
    // this is a uniop
    ir = new Uniop(oper, l);
  } else {
    auto r = right->getAsRVal(container);
    ir = new Binop(oper, l, r);
  }
  ir->setLine(lineno);
  return ir;
}

