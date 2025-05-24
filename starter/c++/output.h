#ifndef _OUTPUT_H_
#define _OUTPUT_H_

#include <stdio.h>

class Output {
private:
  const char* inpath;
  char* outpath;  
  FILE* out;

  void initOutput(const char* extension);
  
public:
 Output(const char* inputpath, const char* ext = 0) : inpath(inputpath), outpath(NULL), out(NULL) {
    initOutput(ext);
  }
  Output(const char* inputpath, FILE* outfile) : inpath(inputpath), outpath((char*)"-"), out(outfile) {
  }
  ~Output(void);
  void genFileHeader(void);
  void genFunctionHeader(const char*& funcName);
  void genFunctionTrailer(const char*& funcName);
  void genFileTrailer(void);
  FILE* getFILE(void) const { return out; }
};

#endif

