#ifndef _CFG_H_
#define _CFG_H_

// Used to represent a control-flow graph for a function.  We only
// have a single straight-line block in Lab1, so this is basically a
// single node at this point.  Might want to make a namespace for this?

using namespace std;
#include "bb.h"

class CST;
class Output;

class CFG {
 private:
  CST* owner;			/* so we know what function this cfg belongs to */
  BasicBlock* nodes;		/* the nodes of the cfg.  At this point there is only one. */
 public:
  CFG(CST* toplevel) : owner(toplevel), nodes(0) {}
  void add(BasicBlock* bb) { assert(nodes == 0); nodes=bb; }
  void dump(void) { nodes->dump(); }
  void dumpTriples(void) { nodes->dumpTriples(); }
  void regalloc(void) {  }
  int size(void) { return nodes ? 1 : 0; }
  void toTriples(void) { if (nodes) nodes->toTriples(); }
  void peephole(void) { if (nodes) nodes->peephole(); }  
  void emitasm(Output* of);
};

#endif
