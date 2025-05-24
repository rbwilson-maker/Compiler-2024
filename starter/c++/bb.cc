#include "bb.h"
#include "munch.h"
#include <typeinfo>
#include "util.h"
#include "output.h"

extern const char* token2name(int x);
int BasicBlock::uid = 0;

void BasicBlock::show(void) {
  printf("BB#%d\n", id);
  for (auto it = stmts.begin(); it != stmts.end(); it++) {
    (*it)->show(1);
  }
  printf("\n");
}

void Binop::show(int indent) const {
  printf("%*s%s\n", indent, " ", token2name(op));
  left->show(indent+1);
  right->show(indent+1);
}

void Uniop::show(int indent) const {
  printf("%*s%s\n", indent, " ", token2name(op));
  left->show(indent+1);
}

void Const::show(int indent) const  {
  printf("%*s%d\n", indent, " ", value.i);
}

void PsuedoReg::show(int indent)  const {
  printf("%*s%s\n", indent, " ", name->symbol());
}

void MoveStmt::show(int indent) const  {
  printf("%*sMove\n", indent, " ");
  dest->show(indent+1);
  src->show(indent+1);
}

void ReturnStmt::show(int indent) const  {
  printf("%*sReturn\n", indent, " ");
  src->show(indent+1);
}


void 
BasicBlock::peephole(void)
{
}

////////////////////////////////////////////////////////////////
// to triples
////////////////////////////////////////////////////////////////

void BasicBlock::toTriples(void) {
  Munch muncher(&asmlist);
  
  for (auto it = stmts.begin(); it != stmts.end(); it++) {
    auto ir = *it;
    muncher.match(ir);
  }
}

void BasicBlock::emit(Output* ofile) {
  FILE* handle = ofile->getFILE();
  for (const auto& a : asmlist) {
    a->showasm(handle);
  }
}


void BasicBlock::dump(void) const {
  for (const auto& a : asmlist) {
    a->showasm();
  }
}

void BasicBlock::dumpTriples(void) const {
  for (const auto& a : asmlist) {
    a->show();
  }
}


vector<const char*> PsuedoReg::num2name = {
  "???",
  "%rax",
  "%rbx",
  "%rcx",
  "%rdx",
  "%rsi",
  "%rdi",
  "%r8",
  "%r9",
  "%rsp"
};

vector<PsuedoReg*> PsuedoReg::num2reg = {
  new Temporary(0),		// NOT A REG
  new Temporary(1),
  new Temporary(2),
  new Temporary(3),
  new Temporary(4),
  new Temporary(5),
  new Temporary(6),
  new Temporary(7),
  new Temporary(8),
  new Temporary(9)  
};

unsigned int Temporary::uid = 16;

IR* Temporary::getGP() {
  unsigned long int old = num2reg.size();
  if (uid >= old) {
    num2reg.resize(uid+16);
  }
  Temporary* t = new Temporary(uid);
  num2reg[uid] = t;
  uid++;
  return t;
}

// get string rep of this register
const char* PsuedoReg::toAsm(void) {
  unsigned long int old = num2name.size();
  if (regno >= old) {
    num2name.resize(regno+10);
    for (unsigned long int i=old; i<regno+10; i++) {
      char* buffer = new char[8];
      sprintf(buffer, "g%ld", i);
      num2name[i] = buffer;
    }
  }
  return num2name[regno];
}

char*
IR::toString(void) {
  static char buffer[128];
  sprintf(buffer, "[IR:%s]", typeName(*this));
  return buffer;
}

char*
Const::toString(void) {
  static char buffer[128];
  if (type == Type::getIntType())
    sprintf(buffer, "[%d]", value.i);
  else
    sprintf(buffer, "[%f]", value.d);
  return buffer;
}

char*
PsuedoReg::toString(void) {
  static char* buffer = 0;
  static unsigned long int blen = 0;
  
  const char* v = name->symbol();
  if (strlen(v)+10 > blen) {
    if (buffer) delete[] buffer;
    blen = 2*strlen(v)+10;
    buffer = new char[blen];
  }
  sprintf(buffer, "[%s]", name->symbol());
  return buffer;
}

char*
Temporary::toString(void) {
  static char buffer[128];
  sprintf(buffer, "[%%%s]", toAsm());
  return buffer;
}
