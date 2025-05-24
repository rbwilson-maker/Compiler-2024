#include <stdio.h>
#include "arg.h"
#define DEFINE_OPTIONS 1
#include "options.h"
#undef DEFINE_OPTIONS
#include "srcfile.h"
#include "output.h"
#include "l1ckpt.h"

extern int yylex();
extern char* yytext;
extern FILE* yyin;
extern const char* token2name(int x);
extern int yydebug;


//If anyone sees and understands the variable below please replace it with a better text!
static const char* progdesc = "C0 compiler Starter code";

ArgDesc argdescs[] = {
  {"-r", "checkpoint-only", "special flag for checkpoint", ArgDesc::Flag, &checkpointOnly, ArgDesc::Optional },
  {"-t", "typecheck-only", "exit after typechecking", ArgDesc::Flag, &typecheckOnly, ArgDesc::Optional },
  {"-O", "opt",            "Optimization level 0", ArgDesc::Int, &opt, ArgDesc::Optional },    
  {"-v", "verbose",        "increase verbosity", ArgDesc::Incremement, &verbose, ArgDesc::Optional },    
  {"",   "dump-ast",       "pretty print the AST", ArgDesc::Flag, &dumpAST, ArgDesc::Optional },    
  {"",   "dump-all",       "pretty print the AST", ArgDesc::Flag, &dumpALL, ArgDesc::Optional },    
  {"",   "dump-ir",        "pretty print the IR", ArgDesc::Flag, &dumpIR, ArgDesc::Optional },    
  {"",   "dump-assem",     "pretty print the abstract assembly", ArgDesc::Flag, &dumpAbstractAsm, ArgDesc::Optional },    
  {"-e", "emit",           "control type of output (abs or x86)", ArgDesc::String, &outputType, ArgDesc::Optional },    
  {"-d", "",               "set yydebug to 1", ArgDesc::Flag, &yydebug, ArgDesc::Optional },    
  { "-h" },
  { 0,   "path",           "Path of file to be compiled", ArgDesc::Pointer, &compilefile, ArgDesc::Required},
  { ArgDesc::END }
};


int
main(int argc, char** argv) 
{
  // process command line arguments
  ArgDesc::procargs(argc, argv, argdescs, progdesc);
  // check arguments that need extra processing
  if (dumpALL) {
    dumpAST = dumpIR = dumpAbstractAsm = true;
  }
  if (strcmp(outputType, "abs")&&strcmp(outputType, "x86-64")) {
    die("--emit needs to be either 'abs' or 'x86-64', not '%s'", outputType);
  }

  // open file
  yyin = fopen(compilefile, "r");
  if (yyin == NULL) {
    die("Could not open %s.", compilefile);
  }
  SourceFile sf(compilefile);
  // compile file
  bool result = sf.compile();
  return (result ? 0 : -1);
}
