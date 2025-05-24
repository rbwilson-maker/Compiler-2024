#include "util.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string>
#include "stacktrace.h"

void
_internalError(const char* fn, int ln, const char* fmt, ...) 
{
    va_list ap;

    va_start(ap, fmt);
    fprintf(stderr, "%s:%d:Internal Error:", fn, ln);
    vfprintf(stderr, fmt, ap);
    va_end (ap);
    fprintf(stderr, "\n");
    print_stacktrace();
    exit(-1);
}


void
die(const char* fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end (ap);
    fprintf(stderr, "\n");
    exit(-1);
}

void
vdie(const char* fmt, va_list ap)
{
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(-1);
}


#ifdef __GNUG__
#include <cxxabi.h>
#include <cstdlib>
#include <memory>

std::string demangle(const char* name) 
{

    int status = -4; // some arbitrary value to eliminate the compiler warning

    // enable c++11 by passing the flag -std=c++11 to g++
    std::unique_ptr<char, void(*)(void*)> res {
        abi::__cxa_demangle(name, NULL, NULL, &status),
        std::free
    };

    return (status==0) ? res.get() : name ;
}

#else

// does nothing if not g++
std::string demangle(const char* name) 
{
    return name;
}

#endif
