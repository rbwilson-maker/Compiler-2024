#pragma once

#include <stdio.h>
#include <vector>
#include <stdarg.h>

class ArgDesc {
public:
    // type signature of function to handle arguments when type is FuncType
    typedef const char* (ArgHandler)(const char* potentialFirstParam, const char** argv);
    typedef enum {
        PosType,
        FlagType,
        EndOfArgs,
        HelpType,
        PointerType,
        IntType,
        RequiredType,
        OptionalType,
        RestType,
        VectorType,
        FuncType,
        IncType,
        FloatType
    } ArgType;
    static ArgType const Position = PosType;//how to use?
    static ArgType const Flag = FlagType;//sets pointer to true, no args necessary
    static ArgType const END = EndOfArgs;//ends the argdesc array
    static ArgType const Help = HelpType;//help is defaulted to argdesc flag with no other argumnets
    static ArgType const Pointer = PointerType;//saves arg to pointer
    static ArgType const String =  PointerType;//saves arg to pointer
    static ArgType const Int = IntType;//change an int to what is specified in the arg
    static ArgType const Required = RequiredType;//if argdesc ends with this, it should make it mandatory ***
    static ArgType const Optional = OptionalType;//ends the argdesc line
    static ArgType const Rest = RestType;//rest of the arguments go to the pointer
    static ArgType const Vector = VectorType;//pushes arg to back of vector pointer
    static ArgType const Func = FuncType;//sends char**argv to function specified
    static ArgType const Incremement = IncType;//increments variable specifed, also this is a typo
    static ArgType const Float = FloatType;//set float pointer as arg
private:
    int kind;
    const char* key;            // short option
    const char* longkey;        // long option if option, else help name of positional
    int position;               // position if not a flag
    int type;
    union {
        int* asint;
        bool* asbool;
        double* asDouble;
        void** asptr;
        void* vptr;
        ArgHandler* fptr;
        std::vector<const char*>* vectorPtr;
    } dest;
    bool reqd;
    const char* info;

public:
    // for the special markers, e.g., end, help, etc. (END)
    ArgDesc(ArgType ki) : kind(ki),
                          key(0), longkey(0), position(-1), 
                          type(-1), reqd(false), info(0) 
    	{dest.vptr = 0;  }

    // for positional args.
    ArgDesc(int po, const char* n, const char* i, int ty, void* v, int re) :
        kind(PosType), key(0), longkey(n), position(po), type(ty), reqd(re == ArgDesc::Required ? true : false), info(i) 
    	{ dest.vptr = v;  }

    // for rest of positional args.
    ArgDesc(int po, const char* n, const char* i, int ty, int re) :
        kind(PosType), key(0), longkey(n), position(po), type(ty), reqd(re == ArgDesc::Required ? true : false), info(i) 
    { blank2null(); dest.vptr = 0;  }

    // for flags
    ArgDesc(const char* ke, const char* lk, const char* i, int ty, void* v, int re) :
        kind(FlagType), key(ke), longkey(lk), type(ty), reqd(re == ArgDesc::Required ? true : false), info(i) 
    { blank2null(); dest.vptr = v; }

    // for flags which call handlers
    ArgDesc(const char* ke, const char* lk, const char* i, int ty, ArgHandler* v, int re) :
        kind(FlagType), key(ke), longkey(lk), type(ty), reqd(re == ArgDesc::Required ? true : false), info(i) 
	{ blank2null(); dest.fptr = v; }

    // for special type help
    ArgDesc(const char* ke) :
        kind(FlagType), key(ke), longkey("help"), type(HelpType), reqd(false), info(0) { dest.vptr = 0; }

    // general
    ArgDesc(int ki, const char* ke, const char* lk, int po, 
            const char* in, int ty, void* v, int re) : kind(ki), key(ke), 
                                                       longkey(lk), position(po), 
                                                       type(ty), 
                                                       reqd(re == ArgDesc::Required ? true : false), info(in)  {dest.vptr = v; }

    static void procargs(int argc, char** argv, ArgDesc* desc, const char* helpmsg = 0);
    static void badArguments(const char* msg, ...);

    static char* getProgramName(void);
    static char* getCommandLine(void);
    static void onexit(void);

    static int positionalArgCount(void);
    static char* arg(int pos);
private:
    static char* progname;
    static int verbose;
    static const char* helpinfo;
    static char** posargv;
    static int posargc;
    static ArgDesc* list;

    static char* commandLine;
    static void makeCommandline(int argc, char** argv);
    static void usage(ArgDesc* desc, int ec);
    typedef enum {
        Either,
        Short,
        Long
    } WhichKey;
    char* helpkey(WhichKey keytype, bool includebraks = false);
    void blank2null(void);
    bool isPotentialMatch(const char* arg, const char** next);
    const char* keyname(void);


};

// Local Variables:
// mode: c++
// indent-tabs-mode: nil
// c-basic-offset: 4
// End:
