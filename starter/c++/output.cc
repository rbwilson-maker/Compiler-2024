#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <error.h>
#include "util.h"
#include "output.h"

// setup for output based on input name (and potentially the output name).
// open output file
void 
Output::initOutput(const char* extension)
{
  if (extension == 0) {
    extension = (const char*)".s";
  }
  if (extension[0] != '.') {
    // extension is actually a complete path
    outpath = (char *)extension;
  }
  if (outpath == NULL) {
    // no specific output path specified, so make default naming scheme
    outpath = new char[strlen(inpath)+10];
    strcpy(outpath, inpath);
    char* p = strrchr(outpath, '.');
    if (p == NULL) p = outpath+strlen(outpath);
    strcpy(p, extension);
  }
  out = fopen(outpath, "w");
  if (out == NULL) {
    die("Cannot open %s: %s", outpath, strerror(errno));
  }
}

// close output file if it is open
Output::~Output(void)
{
  if (out) fclose(out);
}

// output asm needed at top of each asm file
void Output::genFileHeader(void)
{
  fprintf(out, "\t.file\t\"%s\"\n", inpath);
  fprintf(out, "\t.text\n");
}

// output asm needed at top of each function
void Output::genFunctionHeader(const char*& funcName)
{
  fprintf(out, "\t.globl\t%s\n", funcName);
  fprintf(out, "\t.type\t%s, @function\n", funcName);
  fprintf(out, "%s:\n", funcName);
  
}

void Output::genFunctionTrailer(const char*& funcName)
{
  fprintf(out, "\t.size\t%s, .-%s\n", funcName, funcName);
}

void Output::genFileTrailer(void)
{
  fprintf(out, "\t.ident\t\"15-411 L1 compiler\"\n");
}

  

