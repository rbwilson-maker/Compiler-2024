#ifndef _SYMBOLTABLE_H_
#define _SYMBOLTABLE_H_

class BasicBlock;
class Expr;
class PsuedoReg;
#include <unordered_map>
using namespace std;
#include "symbols.h"
#include "type.h"
#include "bb.h"

// This is an entry in the symbol table for a declared varaible, ...

class Entry {
 private:
  bool initialized;		/* has it been initialized? */
  Symbol sym;			/* the symbol of this variable */
  Type* type;			/* its type */
  //std::unordered_map<BasicBlock*, PsuedoReg*> versions;
  PsuedoReg* asreg;		/* psuedoreg used for this entry */
  
 public:
  Entry(Symbol s, Type* t) : sym(s), type(t), asreg(0) { }
  void setInitialized(void) { initialized = true; }
  bool isInitialized(void) { return initialized; }
  Symbol symbol(void) { return sym; }

  // convert this entry to a psuedoreg
  Expr* getFreshPsuedoReg(void);
  Expr* getPsuedoReg(void);
};

// used to represent the chain of scopes

class SymbolTable {
 private:
  SymbolTable* parent;
  std::unordered_map<const char*, Entry*> table;

  static SymbolTable* current;
 public:
  SymbolTable(SymbolTable* p) : parent(p) {}
  static bool declaredHere(Symbol s);
  static void enterScope(void);
  static void exitScope(void);
  static Entry* insertHere(Type* t, Symbol s);
  static Entry* lookup(Symbol s);
  static SymbolTable* getCurrentScope(void) { return current; }
};

#endif
