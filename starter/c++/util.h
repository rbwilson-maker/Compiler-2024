#ifndef _UTIL_H_
#define _UTIL_H_

#include <assert.h>
#include <string>
#include <typeinfo>

#define internalError(fmt...) _internalError(__FILE__, __LINE__, fmt)
#define nyi(x) internalError("Not Yet Implemented: " x)

void _internalError(const char* fn, int ln, const char* fmt, ...);
void die(const char* fmt, ...);
void vdie(const char* fmt, va_list ap);



std::string demangle(const char* name);

template <class T>
const char* typeName(const T& t) {

  return demangle(typeid(t).name()).c_str();
}

#endif
