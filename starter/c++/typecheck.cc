#include "util.h"
#include "cst.h"
#include "srcfile.h"

// used for typechecking.  At this point only the vars are initialized before being used.

static SourceFile* currentSource;
static Function* currentFunction;

static void 
reportTypeError(int ln, const char* fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    char fbuffer[256];
    snprintf(fbuffer, 256, "%s:%d:in %s:%s", currentSource->getInputFile(), ln, currentFunction->getName(), fmt);
    vdie(fbuffer, ap);
    va_end(ap);
}

static void 
reportTypeError(const char* fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    char fbuffer[256];
    snprintf(fbuffer, 256, "%s:in %s:%s", currentSource->getInputFile(), currentFunction->getName(), fmt);
    vdie(fbuffer, ap);
    va_end(ap);
}

bool Expression::typecheck(void) {
  bool result = true;
  if (left) result &= left->typecheck();
  if (right) result &= right->typecheck();
  return result;
}

bool Identifier::typecheck(void) {
  if (!getEntry()->isInitialized()) {
    reportTypeError(lineno, "%s is unitialized", name->symbol());
    return false;
  }
  return true;
}

bool Return::typecheck(void) {
  if (exp) return exp->typecheck();
  return true;
}

bool Declaration::typecheck(void) {
  bool result = true;
  if (exp) {
    result = exp->typecheck();
    if (result) var->setInitialized();
  }
  return result;
}

bool Assignment::typecheck(void) {
  Identifier* id = static_cast<Identifier*>(lval);
  if (!id) return false;
  bool result = rval->typecheck();
  if (!result) return false;
  id->getEntry()->setInitialized();
  return true;
}

bool Statements::typecheck(void) {
  bool result = true;
  for (auto it = stmts.rbegin(); it != stmts.rend(); it++) {
    result &= (*it)->typecheck();
    if (dynamic_cast<Return*>(*it)) {
      return result;
    }
  }
  // never got a return statement
  reportTypeError("No return statement.");
  return false;
}

bool Function::typecheck(SourceFile* src) {
  currentFunction = this;
  currentSource = src;
  return stmts->typecheck();
}



