// routines for manipulating the control flow graph of a function
// at this point, only to output it

#include "cfg.h"
#include "output.h"
#include "cst.h"

void 
CFG::emitasm(Output* of)
{
  Function* f = dynamic_cast<Function*>(owner);
  assert(f);
  const char* name = f->getName();
  of->genFunctionHeader(name);
  nodes->emit(of);
  of->genFunctionTrailer(name);
}



