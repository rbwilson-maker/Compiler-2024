#ifndef _ASM_H_
#define _ASM_H_

#include <stdio.h>
#include <vector>
using namespace std;

class IR;
class PsuedoReg;

enum AsmOp {
  MOVL,
  ADDL,
  IMUL,
  RETL,
  CDQ,
  IDIV,
  NEGL,
  SUBL,
  IMOD,
  LDL,
  STL
};

class Asm {
 private:
  AsmOp op;
  IR* one;			/* src1 (if present) */
  IR* two;			/* src2 (if present) */
  IR* dest;			/* dest (if present) */
  bool deleted;			/* can flag if this asm instruciton has been removed */
  int ln;		        /* approx line number of original code */

 public:
  Asm(AsmOp opcode, IR* d, IR* op1, IR* op2) : op(opcode), one(op1), two(op2), dest(d), deleted(false), ln(-1) {};
  Asm(AsmOp opcode, IR* op1, IR* op2) : op(opcode), one(op1), two(op2), dest(0), deleted(false), ln(-1) {};
  Asm(AsmOp  opcode)  : op(opcode), one(0), two(0), dest(0), deleted(false), ln(-1) {};
  Asm(AsmOp opcode, IR* op1)  : op(opcode), one(op1), two(0), dest(0), deleted(false), ln(-1) {};

  void show(FILE* f = 0) const;
  const char* opName(void) const;
  void showasm(FILE* f = 0) const;
  bool isMove(void) const { return op == MOVL; }
  void setLine(int orig) { ln = orig; }
};

typedef vector<Asm*> AsmList;

#endif
