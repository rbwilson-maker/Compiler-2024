#ifndef _BB_H_
#define _BB_H_

#include "tokentypes.h"
#include "options.h"
#include <vector>
using namespace std;

#include "asm.h"

class Entry;
class SymbolTable;
class PsuedoReg;
class Munch;
class Constraint;

#include "symtab.h"
#include "util.h"

class IR {
 protected:
  int originalLine;
  IR(void) : originalLine(-1) {}
  
 public:
  virtual void show(int indent) const = 0;
  virtual unsigned int size(void) = 0;
  virtual bool match(Munch* m, Constraint* c) = 0;
  virtual char*toString(void);
  virtual const char* toAsm(void) { internalError("Can't represent as ASM: %s", typeName(*this)); return 0; }
  virtual bool isReg(void) const { return false; }
  void setLine(int ln) { originalLine = ln; }
};

class Expr : public IR {
 public:
  virtual void show(int indent)  const = 0;
};

typedef enum yytokentype OpType;

class Binop : public Expr {
 protected:
  OpType op;
  Expr* left;
  Expr* right;
 public:
  Binop(OpType oper, Expr* l, Expr* r) : op(oper), left(l), right(r) {}
  Binop(int oper, Expr* l, Expr* r) : op(static_cast<OpType>(oper)), left(l), right(r) {}
  virtual void show(int indent = 0)  const;
  virtual unsigned int size(void) { return 2; }
  virtual bool match(Munch* m, Constraint* c);
};

class Uniop : public Binop {
 public:
 Uniop(OpType oper, Expr* l) : Binop(oper, l, 0) {}
 Uniop(int oper, Expr* l) : Binop(oper, l, 0) {}
  virtual unsigned int size(void) { return 1; }
  virtual void show(int indent = 0)  const;
  virtual bool match(Munch* m, Constraint* c);
};

class Const : public Expr {
 private:
  Type* type;
  union {
    int i;
    double d;
  } value;
 public:
 Const(Type* t, int v) : type(t) { value.i = v; }
  virtual void show(int indent = 0)  const;
  virtual unsigned int size(void) { return 0; }
  virtual bool match(Munch* m, Constraint* c);
  virtual char*toString(void);
  virtual const char* toAsm(void);
};


class PsuedoReg : public Expr {
 protected:
  Entry* name;
  unsigned int regno;

  static vector<PsuedoReg*> num2reg;
  static vector<const char*> num2name;

  PsuedoReg(void) : name(0), regno(0) {  }
  PsuedoReg(unsigned int r) : name(0), regno(r) {  }
  PsuedoReg(PsuedoReg* base) : name(base->name), regno(base->regno) {}
 public:
  PsuedoReg(Entry* base) : name(base) {  }
  virtual void show(int indent = 0) const;
  virtual unsigned int size(void) { return 0; }
  virtual bool match(Munch* m, Constraint* c);
  virtual char*toString(void);
  virtual const char* toAsm(void);
  virtual bool isReg(void) const { return true; }
};

class Temporary : public PsuedoReg {
  static unsigned int uid;
 public:
  Temporary(int num) : PsuedoReg(num) {}
  static IR* getGP(void);
  virtual char*toString(void);
};

class Stmt : public IR {
 public:
  virtual void show(int indent) const = 0;
};

class MoveStmt : public Stmt {
 private:
  Expr* dest;
  Expr* src;
  
 public:
 MoveStmt(Expr* l, Expr* r) : dest(l), src(r) {}
  virtual void show(int indent = 0) const;
  virtual unsigned int size(void) { return 2; }
  virtual bool match(Munch* m, Constraint* c);
};

class ReturnStmt : public Stmt {
 private:
  Expr* src;
  
 public:
  ReturnStmt(Expr* r) : src(r) {}
  virtual void show(int indent = 0) const;
  virtual unsigned int size(void) { return 1; }
  virtual bool match(Munch* m, Constraint* c);
};

class Asm;
class Output;
typedef std::vector<Asm*> AsmList;

// represent a basic block in the control flow graph.  Since we only
// have one basic block in L1, there are no control flow edges.  This
// is essentially just a list of statements (or triples) and a pointer
// to a symbol table
class BasicBlock {
 private:
  std::vector<Stmt*> stmts;	/* the Statements in this basic block */
  SymbolTable* scope;		/* inner most scope of this BB */
  int id;			/* unique ID of this block */
  AsmList asmlist;		/* list of triples for this BB */

  static int uid;		/* used to generate unique IDs for each BB */
 public:
  BasicBlock(SymbolTable* s) : scope(s) { id = uid++; }
  void append(Stmt* s) { stmts.push_back(s); }
  void show(void);
  Expr* getFreshPsuedoReg(Entry* name); /* create a new entry in closest symbol table for this name */
  Expr* getPsuedoReg(Entry* name);	/* get an already created entry for this name */
  void toTriples(void);		/* convert stmts to asmlist */
  void peephole(void);		/* do peephole optimization on the triples */
  void dump(void) const;
  void dumpTriples(void) const;
  void emit(Output* ofile);
};



#endif
