#include "asm.h"
#include "bb.h"

// get name of asm opcode
const char* 
Asm::opName(void) const {
  switch (op) {
  case MOVL:
    return "movl";
  case CDQ:
    return "cdq";
  case IDIV:
    return "idiv";
  case IMOD:
    return "imod";
  case ADDL:
    return "addl";
  case IMUL:
    return "imul";
  case RETL:
    return "retl";
  case SUBL:
    return "subl";
  case NEGL:
    return "negl";
  case LDL:
    return "ldl";
  case STL:
    return "stl";
  }
  return "????";
}

// debugging and dumping
void
Asm::show(FILE* f) const {
  if (f == 0) f = stdout;
  if (dest != 0)
    fprintf(f, "%14s\t<- ", dest->toString());
  else
    fprintf(f, "\t\t   ");
  fprintf(f, "\t%s", opName());
  if (one != 0)
    fprintf(f, "\t%s", one->toString());
  if (two != 0)
    fprintf(f, ", %s", two->toString());
  fprintf(f, "\t%sln:%d %s\n", (two == 0) ? "\t" : "", ln, deleted ? "DEL" : "");
}

// print out actual assembly code?
void
Asm::showasm(FILE* f) const {
  show(f);
  return;
  if (f == 0) f = stdout;
  char buffer[256];
  char* p = buffer;
  *p = 0;
  fprintf(f, "\t%s%s", deleted ? "; " : "", opName());
  if (one != 0) {
    fprintf(f, "\t%s", one->toAsm());
    sprintf(buffer, "\t; %s ", one->toString());
    p += strlen(buffer);
  }
  if (two != 0) {
    fprintf(f, ", %s", two->toAsm());
    const char* prefix = (p == buffer) ? "\t; " : ", ";
    sprintf(p, "%s%s", prefix, two->toString());
    p += strlen(p);
  }
  if (ln != -1) {
    sprintf(p, "%s @%d", ((p == buffer) ? "\t;" : ""), ln);
  }
  fprintf(f, "%s\n", buffer);
}

const char* Const::toAsm(void) {
  static char buffer[128];
  sprintf(buffer, "$%d", value.i);
  return buffer;
}


