#include <assert.h>
#include "symtab.h"

SymbolTable* SymbolTable::current = 0;

bool SymbolTable::declaredHere(Symbol s) {
  auto entry = current->table.find(s);
  return entry != current->table.end();
}

void SymbolTable::enterScope(void) {
  current = new SymbolTable(current);
}

void SymbolTable::exitScope(void) {
  assert(current != 0);
  current = current->parent;
}

Entry* SymbolTable::insertHere(Type* t, Symbol s) {
  assert(current != 0);
  Entry* e = new Entry(s, t);
  current->table.insert({s, e});
  return e;
}

Entry* SymbolTable::lookup(Symbol s) {
  assert(current != 0);
  for (auto p = current; p; p=p->parent) {
    auto entry = p->table.find(s);
    if (entry != p->table.end()) 
      return entry->second;
  }
  return 0;
}

// convert this to a psuedoreg
//PsuedoReg* Entry::psuedoreg(int version) { 
//  return new PsuedoReg(this, version); 
//}


Expr* 
Entry::getFreshPsuedoReg(void)
{
  // this should not have a psuedoreg yet
  assert(asreg == 0);
  asreg = new PsuedoReg(this);
  return asreg;
}

Expr* 
Entry::getPsuedoReg(void)
{
  // this should already have a psuedoreg
  assert(asreg != 0);
  return asreg;
}
