// Used to implement maximal munch for converting from IR -> abstract assembly

#ifndef _MUNCH_H_
#define _MUNCH_H_

#include "bb.h"

class Munch {
 private:
  AsmList* out;
 public:
  Munch(AsmList* al) : out(al) {}
  bool match(IR* node);		/* match node, no constraints */
  bool match(IR* node, Constraint* c);		/* match node, output must match constraint c */  
  void show(void) const;
  void add(Asm* a) { out->push_back(a); };
  AsmList* getasm(void) const { return out; }
  void addTriple(AsmOp opcode, IR* dest, IR* op1, IR* op2, int ln);
};

// defines the constraints used for matching

class Constraint {
 public:
  virtual IR* asReg(void) const = 0;
  virtual IR* asConst(void) const = 0;
  virtual const char* toString(void) = 0;
};

class RegConstraint : public Constraint {
 private:
  IR* reg;
 public:
  RegConstraint(void) : reg(0) {}
  virtual IR* asReg(void) const { return reg; }
  virtual IR* asConst(void) const { assert(0); }
  void set(void) { /* create a new register */ reg = Temporary::getGP(); }
  void set(PsuedoReg* v) { reg = v; }
  virtual const char* toString(void);
};

class ConstConstraint : public Constraint {
  Type* constType;
  Const* val;
 public:
 ConstConstraint(Type* t) : constType(t), val(0) {}
  void set(Const* v) { val = v; }
  virtual IR* asConst(void) const { return val; }
  virtual IR* asReg(void) const { assert(0); }
  virtual const char* toString(void);
};



#endif
