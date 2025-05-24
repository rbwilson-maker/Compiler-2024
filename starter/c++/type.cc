#include "type.h"
#include "util.h"

// setup primitive types for lab1
Type Type::intType = { 1 };
ArgListType ArgListType::voidListType = {};
FunctionType FunctionType::void2int = { &Type::intType, &ArgListType::voidListType };

// the toString functions below are for debugging purposes.
const char* 
Type::toString(void) 
{
  switch (base) {
  case 1:
    return (const char*)"int";
  case 0:
    return (const char*)"void";
  }
  return (const char*)"?";
}

const char* 
ArgListType::toString(void) 
{
  // special case of no arguments
  if (args.size() == 0)
    return (const char*)"(void)";
  nyi("Cannot handle arbitrary argument lists");
  return (const char*)"?";
}


const char* 
FunctionType::toString(void)
{
  static char buffer[128];
  sprintf(buffer, "%s->%s", arglist->toString(), returnType->toString());
  return buffer;
}

