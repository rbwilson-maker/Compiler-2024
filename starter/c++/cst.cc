#include "cst.h"
#include "bb.h"
#include "options.h"
#include <stdio.h>

// init a decl
void Declaration::initDecl(Symbol v) {
  assert(!SymbolTable::declaredHere(v));
  var = SymbolTable::insertHere(type, v);
  if (exp != 0) var->setInitialized();
}


// CST dump code

void Expression::show(int indent) const
{
  printf("%*sExp:%d (%s)\n", indent, " ", lineno, token2name(oper)); 
  left->show(indent+1); 
  if (right) right->show(indent+1); 
}

void IntConstant::show(int indent) const
{
  printf("%*sNumber:%d (%d)\n", indent, " ", lineno, intvalue); 
}

void Identifier::show(int indent) const
{ 
  printf("%*sID:%d (%s)\n", indent, " ", lineno, name->symbol()); 
}

void Return::show(int indent) const
{
  printf("%*sReturn:%d\n", indent, " ", lineno);
  if (exp) exp->show(indent+1);
}

void Declaration::show(int indent) const
{
  printf("%*sDECL:%d %s %s\n", indent, " ", lineno, type->toString(), var->symbol()); 
}


void Assignment::show(int indent) const
{ 
  printf("%*sAsn:%d\n", indent, " ", lineno); 
  lval->show(indent+1); 
  if (rval) rval->show(indent+1); 
}


void Statements::show(int indent) const
 { 
    printf("%*sStmts\n", indent, " "); 
    for (auto it = stmts.rbegin(); it != stmts.rend(); it++) {
      (*it)->show(indent+2);
    }
  }

void Function::show(int indent) const
{ 
  printf("%*sFunction:%d %s %s\n", indent, " ", lineno, name, type->toString());
  stmts->show(indent+1);
}
