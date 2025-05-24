// code to convert tree-based IR to triple based IR

#include "util.h"
#include "munch.h"
#include "options.h"

/* match node, no constraints */
bool 
Munch::match(IR* node) {
  return node->match(this, NULL);
}

/* match node, output must match constraint c */  
bool 
Munch::match(IR* node, Constraint* c) {
  if (verbose > 1) 
    printf("Match: %s against %s\n", node->toString(), c->toString());
  return node->match(this, c);
}

void 
Munch::show(void) const {
  for (const auto& a : *out) {
    a->show();
  }
}


bool
ReturnStmt::match(Munch* m, Constraint* c) {
  // constraint doesn't matter (probably should be null?)
  assert(c == 0);
  RegConstraint gp;
  m->match(src, &gp);
  Asm* retl = new Asm(RETL, gp.asReg());
  retl->setLine(originalLine);
  m->add(retl);
  return true;
}

/*
  RegConstraint - any general purpose int register
  FloatRegConstraint - any general purpose floating point register
  ConstConstraint(type) - any constant of appropraite type  (should have one with a particular range)
  SpecRegConstraint(reg) - a particular register
*/

bool
MoveStmt::match(Munch* m, Constraint* c) {
  if (verbose > 1) show();
  // constraint doesn't matter (probably should be null?)
  assert(c == 0);
  // different depth patterns would be listed in order here
  // for now both src and dest have to be in registers
  RegConstraint s;
  RegConstraint d;
  ConstConstraint cc(Type::getIntType());

  if (m->match(src, &cc) && m->match(dest, &d)) {
    m->addTriple(MOVL, d.asReg(), cc.asConst(), 0, originalLine);
    return true;
  } else if (m->match(src, &s) && m->match(dest, &d)) {
    m->addTriple(MOVL, d.asReg(), s.asReg(), 0, originalLine);
    return true;
  } 
  return false;
}

// at the end of this, assuming answer is true, c will have register
// holding constant, or if we asked for constant itself it will hold
// constant.
bool
Const::match(Munch* m, Constraint* c) {
  ConstConstraint* cc = dynamic_cast<ConstConstraint* >(c);
  if (cc == 0) {
    // constraint was not for a constant, see if it is a register?
    RegConstraint* r = dynamic_cast<RegConstraint*>(c);
    if (r == 0) return false;
    // want constant in a register (int? or float?)
    assert(type == Type::getIntType());
    r->set();
    m->addTriple(MOVL, r->asReg(), this, 0, originalLine);
    return true;
  } else {
    // constraint asks for a constant
    if (verbose > 1) fprintf(stderr, "Setting CC <- %p\n", this);
    cc->set(this);
    return true;
  }
}

bool
PsuedoReg::match(Munch* m, Constraint* c) {
    RegConstraint* r = dynamic_cast<RegConstraint*>(c);
    if (r == 0)
      return false;		// constraint wanted something other than a register
    r->set(this);
    return true;
}

bool
Uniop::match(Munch* m, Constraint* c) {
  ConstConstraint* cc = dynamic_cast<ConstConstraint* >(c);
  if (cc) return false;
  RegConstraint* r = dynamic_cast<RegConstraint*>(c);
  if (r == 0)
    internalError("PsuedoReg expected RegConstraint, but got: %s", typeName(c));
  r->set();
  RegConstraint s;
  switch (op) {
  case MINUS:
    if (m->match(left, &s)) {
      m->addTriple(NEGL, r->asReg(), s.asReg(), 0, originalLine);
      return true;
    } 
    break;
  default:
    nyi();
  }
  return false;
}

void
Munch::addTriple(AsmOp opcode, IR* dest, IR* op1, IR* op2, int ln) {
  Asm* a = new Asm(opcode, dest, op1, op2);
  a->setLine(ln);
  this->add(a);
}

bool
Binop::match(Munch* m, Constraint* c) {
  ConstConstraint* cc = dynamic_cast<ConstConstraint* >(c);
  if (cc) return false;
  RegConstraint* r = dynamic_cast<RegConstraint*>(c);
  if (r == 0)
    internalError("PsuedoReg expected RegConstraint, but got: %s", typeName(c));
  // should probably typecheck the constraint to make sure we are generating ints
  // for now just match ops against registers
  RegConstraint s;
  RegConstraint d;
  r->set();
  switch (op) {
  case PLUS:
    if (m->match(left, &s) && m->match(right, &d)) {
      m->addTriple(ADDL, r->asReg(), s.asReg(), d.asReg(), originalLine);
      return true;
    } 
    break;
  case MINUS:
    if (m->match(left, &s) && m->match(right, &d)) {
      m->addTriple(SUBL, r->asReg(), s.asReg(), d.asReg(), originalLine);
      return true;
    } 
    break;
  case STAR:
    if (m->match(left, &s) && m->match(right, &d)) {
      m->addTriple(IMUL, r->asReg(), s.asReg(), d.asReg(), originalLine);
      return true;
    } 
    break;
  case MOD:
    // left % right
    // edx:eax <- left
    // psuedo <- right
    // idiv ebx -> remainder in eax
    if (m->match(left, &s) && m->match(right, &d)) {
      m->addTriple(IMOD, r->asReg(), s.asReg(), d.asReg(), originalLine);
      return true;
    } 
    break;
  case SLASH:
    // left / right
    // edx:eax <- left
    // psuedo <- right
    // idiv ebx -> queotient in edx
    if (m->match(left, &s) && m->match(right, &d)) {
      m->addTriple(IDIV, r->asReg(), s.asReg(), d.asReg(), originalLine);
      return true;
    } 
    break;

  default:
    nyi();
  }
  return false;
}

const char*
ConstConstraint::toString(void)
{
  static char buffer[100];
  sprintf(buffer, "{CC:%s-%s}", constType ? constType->toString() : "*", val ? val->toString() : "*");
  return buffer;
}

const char*
RegConstraint::toString(void)
{
  static char buffer[100];
  sprintf(buffer, "{RC:%s}", reg ? reg->toString() : "*");
  return buffer;
}

