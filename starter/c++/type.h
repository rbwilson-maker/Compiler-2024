// Used to represent types.  At this point everything is hardwired just for lab1

#ifndef _TYPE_H_
#define _TYPE_H_

#include <vector>
using namespace std; 

class Type {
 private:
  int base;
 public:
 Type(void) : base(0) {}
 Type(int b) : base(b) {}  
  virtual const char* toString(void);
  static Type intType;
  static Type* getIntType(void) { return &intType; }
};

class ArgListType : public Type {
 private:
  vector<Type*> args;
  ArgListType(void) {}
 public:
  static ArgListType voidListType;
  virtual const char* toString(void);
};

class FunctionType : public Type {
 private:
  Type* returnType;
  ArgListType* arglist;
 FunctionType(Type* ret, ArgListType* l) : returnType(ret), arglist(l) {}
  static FunctionType void2int;
 public:
  static FunctionType* getMainType(void) { return &void2int; }
  virtual const char* toString(void);
};

#endif
