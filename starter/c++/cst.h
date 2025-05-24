// Used by c0.y to build the concrete/abstract syntax tree

#ifndef _CST_H_
#define _CST_H_

#include <vector>
#include "symbols.h"
#include "symtab.h"
extern const char* token2name(int x);
class CFG;


class CST {
 protected:
  int lineno;			/* approximate line number for this CST */
 public:
  virtual ~CST() {}
  virtual void show(int indent) const = 0;
  void setLineNo(int ln) { lineno = ln; }
  int ln(void) { return lineno; }  
  // dump the CST
  virtual void dump(int x = 0) { show(x); }

  // used to translate to IR
  virtual void translate(void) { assert(0); }
  virtual bool translate(CFG* c) { assert(0); }

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void) { assert(0); }
};

class Expression : public CST {
 private:
  int oper;			/* the operator of this expression */
  Expression* left;		/* left hand side of expr */
  Expression* right;		/* right hand side of expr */
 public:
  Expression(int op, Expression* l, Expression* r) : oper(op), left(l), right(r) {  }
  Expression(int op, Expression* l) : oper(op), left(l), right(0) {}
  Expression() : oper(0), left(0), right(0) {}
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual void translate(BasicBlock* container) { nyi("Translate");  }
  virtual Expr* getAsRVal(BasicBlock* container);
  virtual Expr* getAsLVal(BasicBlock* container) { assert(0); }

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

class IntConstant : public Expression {
 private:
  int intvalue;			/* the value of the constant */
 public:
  IntConstant(int val) : intvalue(val) { }
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual Expr* getAsRVal(BasicBlock* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void) { return true; }
};

class Identifier : public Expression {
 private:
  Entry* name;			/* Ptr to the symbol table entry for this identifier */
 public:
  Identifier(Entry* id) : name(id) {}
  virtual void show(int indent = 0) const;
  Entry* getEntry(void) { return name; }

  // used to translate to IR
  virtual Expr* getAsLVal(BasicBlock* container);
  virtual Expr* getAsRVal(BasicBlock* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

// all statements are derived from this abstract class
class Statement : public CST {
 public:
  Statement() {}

  // used to translate to IR
  virtual void translate(BasicBlock* container) = 0;
};

class Return : public Statement {
 private:
  Expression* exp;		/* the expression (if any) returned by this return statement */
 public:
  Return(Expression* e) : exp(e) {}
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual void translate(BasicBlock* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

class Declaration : public Statement {
 private:
  Type* type;			/* type for this decl */
  Expression* exp;		/* init expression (if any) */
  Entry* var;			/* variable being declared (or possibly defined) */
  void initDecl(Symbol s);
  
 public:
  Declaration(Type* t, Symbol v) : type(t), exp(0) { initDecl(v); }
  Declaration(Type* t, Symbol v, Expression* e) : type(t), exp(e) { initDecl(v); }
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual void translate(BasicBlock* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

class Assignment : public Statement {
  Expression* lval;		/* left hand side of assignment */
  Expression* rval;		/* right hand side of assignment */
 public:
 Assignment(Expression* l, Expression* r) : lval(l), rval(r) {  }
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual void translate(BasicBlock* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

class Statements : public CST {
  SymbolTable* scope;		 /* closest symbol table for this block of statements */
  std::vector<Statement*> stmts; /* statements stored in reverse order */
 public:
  Statements(void) : scope(SymbolTable::getCurrentScope()) {}
  void prepend(Statement* s) { stmts.push_back(s); }
  virtual void show(int indent = 0) const;

  // used to translate to IR
  virtual bool translate(CFG* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(void);
};

class SourceFile;

// This is the container for a Function
class Function : public CST {
 private:
  FunctionType* type;		/* type signature of the Function */
  Symbol name;			/* name of the function */
  Statements* stmts;		/* list of stmt blocks */
 public:
  Function(FunctionType* t, Symbol fname, Statements* s) : type(t), name(fname), stmts(s) {}
  virtual void show(int indent = 0) const;
  Symbol getName(void) { return name; }

  // used to translate to IR
  virtual bool translate(CFG* container);

  // used for typechecking.  At this point only the vars are initialized before being used.
  virtual bool typecheck(SourceFile* src);
};

#endif
