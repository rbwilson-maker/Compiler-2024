#include "srcfile.h"
#include "output.h"
#include "options.h"
#include "cfg.h"
#include "l1ckpt.h"
#include <stdio.h>

bool 
SourceFile::typecheck(void)
{
  bool result = true;
  for (auto cst : topast) {
    Function* f = static_cast<Function*>(cst);
    assert(f);			// at this point we only have functions at toplevel
    result &= f->typecheck(this);
  }
  return result;
}

void
SourceFile::dumpAST(void)
{
  for (const auto cst : topast) cst->dump();
}

void
SourceFile::dumpIR(void)
{
  for (const auto top : topcfg) top->dump();
}

void
SourceFile::dumpAsm(void)
{
  for (const auto top : topcfg) top->dump();
}

void
SourceFile::dumpAbstractAsm(void)
{
  for (const auto top : topcfg) top->dumpTriples();
}


bool
SourceFile::totriples(void)
{
  for (const auto top : topcfg) top->toTriples();
  return true;
}

bool
SourceFile::peephole(void)
{
  for (const auto top : topcfg) top->peephole();
  return true;
}


// First step in converting CST to IR assembly
bool 
SourceFile::translate(void)
{
  CFG* cfg = 0;
  for (auto it : topast) {
    if (cfg == 0) cfg = new CFG(it);
    bool result = it->translate(cfg);
    if (!result) return result;
    if (cfg->size() > 0) {
      topcfg.push_back(cfg);
      cfg = 0;
    }
  }
  if (cfg) delete cfg;
  return true;
}

// register allocation
bool
SourceFile::regalloc(void) {
  for (auto it : topcfg) {
    it->regalloc();
  }
  return false;
}

bool 
SourceFile::outputAsm()
{
  if (strcmp(outputType, "x86-64")) return true;

  Output of(path, ".s");
  of.genFileHeader();
  for (auto it : topcfg) {
    it->emitasm(&of);
  }
  of.genFileTrailer();
  return true;
}

bool 
SourceFile::outputTriples(void)
{
  if (strcmp(outputType, "abs")) return true;

  Output of(path, ".triple");
  return false;
}


extern int yyparse(SourceFile*);  


bool
SourceFile::parse(void)
{
  int x = yyparse(this);
  return (x == 0);
}

typedef bool (SourceFile::*SourceFilePassMethod)(void);
typedef void (SourceFile::*SourceFileDumpMethod)(void);

// used to define passes that we want to run
class Pass {
public:
private:
  SourceFilePassMethod passptr;
  SourceFileDumpMethod dumper;
  const char* name;
  bool* dump;
public:
  Pass(const char* n, SourceFilePassMethod passfunc, SourceFileDumpMethod dumpfunc = 0, bool* dflag = 0) : passptr(passfunc), dumper(dumpfunc), name(n), dump(dflag) {}
  Pass(void) : passptr(0), dumper(0), name(0), dump(0) {}

  // run the pass specified by this.
  bool run(SourceFile* sf) const {
    if (verbose > 0) fprintf(stderr, "== Running Pass: %s\n", name);
    bool result = (sf->*passptr)();
    if (!result) {
      // pass failed
      die("%s:%s Error", sf->getInputFilepath(), name);
    } else if (dump && *dump) {
      (sf->*dumper)();
    }
    return result;
  }

  // return true if this is just a marker for being done
  bool isEnd(void) const { return (name == 0) && (passptr == 0); }
};

Pass passes[] = {
  {"Parse", &SourceFile::parse, &SourceFile::dumpAST, &dumpAST},
  {"Typecheck", &SourceFile::typecheck, &SourceFile::dumpAST, &dumpIR},
  {"2IR", &SourceFile::translate, &SourceFile::dumpAsm, &dumpIR },		 
  {"2Triples", &SourceFile::totriples, &SourceFile::dumpAbstractAsm, &dumpAbstractAsm },		 
  //  {"RegAlloc", &SourceFile::regalloc, &SourceFile::dumpAsm, &dumpAbstractAsm },		 
  //  {"Peephole", &SourceFile::peephole, &SourceFile::dumpAsm, &dumpAbstractAsm },		 
  {"OutputTriples", &SourceFile::outputTriples},		 
  {"Outputx86", &SourceFile::outputAsm},
  {}
};



// run all the passes on the source file.  Choose to do each pass on
// the complete source unit instead of on a per function basis.
bool
SourceFile::compile(void)
{
  if (checkpointOnly) {
    L1Ckpt::runWithLivenessInput(path);
    return true;
  }
  if (typecheckOnly) {
    // hack to stop processing after typechecking
    passes[2] = Pass();
  }
  for (const auto p : passes) {
    if (p.isEnd()) break;
    if (!p.run(this)) return false;
  }
  return true;
}

