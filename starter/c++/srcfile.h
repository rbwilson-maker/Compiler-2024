#ifndef _SRCFILE_H_
#define _SRCFILE_H_

#include <vector>
#include "cst.h"
#include "cfg.h"
using namespace std; 

class Output;

// This is how we control the compiler.  Instantiate an instance of
// this class with the path to the sourcefile that is being compiled
// and then invoke the compile method which will invoke all the passes
// on the sourcefile an instance of this class represents.

class SourceFile {
 private:
  char* path;
  vector<CST*> topast;
  vector<CFG*> topcfg;
 public:
  SourceFile(char* name) : path(name) {}
  void addFunction(Function* f) { topast.push_back(f); }
  bool typecheck(void);
  bool translate(void);
  bool regalloc(void);
  bool totriples(void);
  bool peephole(void);
  bool outputAsm(void);
  bool outputTriples(void);
  const char* getInputFile(void) { return path; }
  bool compile(void);
  void dumpAST(void);
  void dumpAsm(void);
  void dumpIR(void);
  void dumpAbstractAsm(void);
  bool parse(void);
  const char* getInputFilepath(void) { return path; }
};

#endif
