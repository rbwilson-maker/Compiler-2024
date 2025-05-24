#include "arg.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

// create a user friendly descriptor for this arg.   if key is absent, then use it.  Otherwise use longkey
char*
ArgDesc::helpkey(WhichKey keytype, bool includebraks)
{
    static char buffer[128];
    char* p = buffer;
    bool setsomething = false;
    if (includebraks && !reqd) {
        *p++ = '[';
    } 
    *p = 0;
    if (kind == ArgDesc::Flag) {
        if (key && (keytype != Long)) {
            strcpy(p, key);
            setsomething = true;
        } else if (longkey && (keytype != Short)) {
            sprintf(p, "--%s", longkey);
            setsomething = true;
        } else 
            *p = 0;
        p += strlen(p);
        switch (type) {
        case ArgDesc::Vector:
            sprintf(p, " <string>+");
            break;

        case Func:
            sprintf(p, " %s", (char*)((dest.fptr)(NULL, NULL)));
            break;

        case ArgDesc::Pointer:
            sprintf(p, " <string>");
            break;

        case ArgDesc::Help:
        case ArgDesc::Flag:
        case ArgDesc::Incremement:
	    break;

        case ArgDesc::Int:
            sprintf(p, " <int>");
	    break;

        case ArgDesc::Float:
            sprintf(p, " <float>");
	    break;

        default:
	    fprintf(stderr, "NIY: type0\n");
	    exit(-1);
        }
        p += strlen(p);
    } else if (kind == ArgDesc::Position) {
        if (keytype != Long) {
            setsomething = true;
            switch (type) {
            case ArgDesc::Pointer:
                sprintf(p, "<%s>", longkey);
                break;

            case Rest:
                sprintf(p, "<%s>...", longkey);
                break;

            default:
                fprintf(stderr, "NIY: positional arg of type:%d\n", type);
                exit(-1);
            }
            p += strlen(p);
        }
    } else {
        fprintf(stderr, "Huh?\n");
        assert(0);
    }
    sprintf(p, (includebraks&&!reqd) ? "]" : "");
    if (setsomething == false) *buffer = 0;
    return buffer;
}

void
ArgDesc::usage(ArgDesc* desc, int ec)
{
    fprintf(stderr, "\n%s ", progname);
    for (int j=0; desc[j].kind != ArgDesc::END; j++) {
	if (desc[j].kind != ArgDesc::Flag) continue;
        fprintf(stderr, " %s", desc[j].helpkey(Either, true));
    }
    for (int j=0; desc[j].kind != ArgDesc::END; j++) {
	if (desc[j].kind != ArgDesc::Position) continue;
        fprintf(stderr, " %s", desc[j].helpkey(Either, true));
    }
    fprintf(stderr, "\n");
    for (int j=0; desc[j].kind != ArgDesc::END; j++) {
        fprintf(stderr, "  %-12s\t", desc[j].helpkey(Short));
        fprintf(stderr, "%-20s\t%s", desc[j].helpkey(Long), desc[j].info ? desc[j].info : "");
        if (!((desc[j].kind == ArgDesc::Position)||(desc[j].kind == ArgDesc::Rest))) {
            switch (desc[j].type) {
            case ArgDesc::Pointer:
                fprintf(stderr, "\t(default: %s)", *((char**)desc[j].dest.asptr));
                break;
            case ArgDesc::Incremement:
            case ArgDesc::Int:
                fprintf(stderr, "\t(default: %d)", *(desc[j].dest.asint));
                break;
            case ArgDesc::Flag:
                fprintf(stderr, "\t(default: %s)", *(desc[j].dest.asbool) ? "true" : "false");
                break;
            case ArgDesc::Float:
                fprintf(stderr, "\t(default: %lf)", *(desc[j].dest.asDouble));
                break;
            default:
                break;
            }
        }
        fprintf(stderr, "\n");
    }

    // print out general info about the program, if there is any.
    if (helpinfo) {
        fprintf(stderr, "\n");
        char buffer[256];
        int col = 0;
        const char* p = helpinfo;
        while (*p) {
            const char* next = p;
            while (*next && !isspace(*next) && !ispunct(*next)) next++;
            if (next == p) {
                if (*next == 0) break;
                next++;
            }
            if ((col + next - p) > 60) {
                fprintf(stderr, "\n");
                col = 0;
            }
            strncpy(buffer, p, next-p);
            buffer[next-p] = 0;
            if (buffer[0] == '\n') col = 0;
            col = col + (int)(next - p);
            fprintf(stderr, "%s", buffer);
            p = next;
        }
        fprintf(stderr, "\n");
    }
    exit(ec);
}

void
ArgDesc::badArguments(const char* msg, ...)
{
    va_list ap;

    va_start(ap, msg);
    fprintf(stderr, "%s: Error: ", progname);
    vfprintf(stderr, msg, ap);
    fprintf(stderr, "\n");
    usage(list, 1);
    va_end(ap);
}

// return either short key or long key
const char*
ArgDesc::keyname(void)
{
    if (key) return key;
    if (longkey) return longkey;
    assert(0);
}

// try and match argv[pos] to this descriptor and return pointer to paremeter (if there is one)
// if expecting a parameter, then if key can either be immediately after or as argv[pos+1]
//                                if longkey can come after '=' or as argv[pos+1]
// update pos accordingly

bool
ArgDesc::isPotentialMatch(const char* arg, const char** next)
{
    // first make sure starts with a '-'
    if (arg[0] != '-') return false;
    // try short if possible
    int arglen;                 // length of arg name if matches
    bool havematch = false;
    if (arg[1] != '-') {
        // try short key
        if (key == 0) return false;
        arglen = (int)strlen(key);
        if (strncmp(arg, key, arglen) == 0) havematch = true;
    } else {
        // try long key
        if (longkey == 0) return false;
        arglen = (int)strlen(longkey);
        if (strncmp(arg+2, longkey, arglen) == 0) havematch = true;
        arglen += 2;
    }
    if (!havematch) return false;
    // we found a match, point to possible parameter for this switch
    *next = arg+arglen;
    return true;
}
        

// stash and process all command line arguments
void
ArgDesc::procargs(int argc, char** argv, ArgDesc* desc, const char* progdesc)
{
  progname = argv[0];
  helpinfo = progdesc;
  list = desc;
  makeCommandline(argc, argv);

  argv++; argc--;

  // track which args have been consumed
  bool* used = new bool[argc];
  for (int i=0; i<argc; i++) used[i] = false;
  // track which flags are required
  int descCount = 0;
  for (int j=0; desc[j].kind != ArgDesc::END; j++) {
      descCount++;
  }
  int* req = new int[descCount];
  for (int j=0; desc[j].kind != ArgDesc::END; j++) {
      req[j] = desc[j].reqd ? 0 : 1;
  }

  // process flags & specific position args
  if (ArgDesc::verbose) printf("Processing args for %s: %d\n", progname, argc);
  //int maxarg = argc;
  int i;
  for (i=0; i<argc; i++) {
    char* arg = argv[i];
    if (ArgDesc::verbose) printf("%d -> [%s]\n", i, arg);
    if (arg[0] != '-') break;
    bool ok = false;
    used[i] = true;
    for (int j=0; desc[j].kind != ArgDesc::END; j++) {
	if (desc[j].kind != ArgDesc::Flag) continue;
        const char* paramptr;
        bool matched = desc[j].isPotentialMatch(argv[i], &paramptr);
        if (matched) {
            if (ArgDesc::verbose) printf("Matched %s (%s) and rest is %s\n", desc[j].key, desc[j].longkey, paramptr);
            bool isLong = (argv[i][0] == '-') && (argv[i][1] == '-');
            matched = false;
            // process it
            switch (desc[j].type) {
            case Incremement:
                if (*paramptr == 0) {
                    matched = true;
                    if (ArgDesc::verbose) printf("incremement %s\n", desc[j].keyname());
                    int* p = (int*)desc[j].dest.asptr;
                    (*p)++;
                }
                break;

            case Func:
                // call function with pointer to argv.  Expect # of argv consumed as return result.
                matched = true;
                if (ArgDesc::verbose) printf("calling function %s\n", desc[j].keyname());
                {
                    const char* ret = (*(desc[j].dest.fptr))(paramptr, (const char**)argv+i);
                    long int consumed = (long int)ret;
                    for (int j=i; j<=i+consumed; j++) used[j] = true;
                    i += (int)consumed;
                }
                break;

            case Vector:
                if (*paramptr == 0) {
                    i++;
                    paramptr = argv[i];
                    used[i] = true;
                } else if (isLong) {
                    if (*paramptr != '=') break;
                    paramptr++;
                }
                matched = true;
                if (ArgDesc::verbose) printf("Pushing %s to %s\n", paramptr, desc[j].keyname());
                (desc[j].dest.vectorPtr)->push_back(paramptr);
                break;

            case Pointer:
                if (*paramptr == 0) {
                    i++;
                    paramptr = argv[i];
                    used[i] = true;
                } else if (isLong) {
                    if (*paramptr != '=') break;
                    paramptr++;
                }
                matched = true;
                if (ArgDesc::verbose) printf("Saving %s to %s\n", paramptr, desc[j].keyname());
                *((const void**)desc[j].dest.asptr) = paramptr;
                break;

            case Flag:
                if (*paramptr == 0) {
                    matched = true;
                    if (ArgDesc::verbose) printf("Setting %s to 1\n", desc[j].keyname());
                    *(desc[j].dest.asbool) = true;
                    break;
                }

            case Float:
                if (*paramptr == 0) {
                    i++;
                    paramptr = argv[i];
                    used[i] = true;
                } else if (isLong) {
                    if (*paramptr != '=') break;
                    paramptr++;
                }
                matched = true;
                if (ArgDesc::verbose) printf("float -> [%s] = %f\n", paramptr, atof(paramptr));
                *(desc[j].dest.asDouble) = atof(paramptr);
                break;

            case Int:
                if (*paramptr == 0) {
                    i++;
                    paramptr = argv[i];
                    used[i] = true;
                } else if (isLong) {
                    if (*paramptr != '=') break;
                    paramptr++;
                }
                matched = true;
                if (ArgDesc::verbose) printf("int -> [%s] = %d\n", paramptr, atoi(paramptr));
                *(desc[j].dest.asint) = atoi(paramptr);
                break;

            case Help:
                matched = true;
                if (*paramptr == 0) {
                    usage(desc, 0);
                    exit(-1);
                }
                break;

            default:
                fprintf(stderr, "NIY: type\n");
                exit(-1);
            }
            // check to see if we still think there was a match
            if (matched) {
                ok = true;
                req[j] = 1;
            }
	}/*else if (desc[j].reqd == ArgDesc::Required) {
           fprintf(stderr, "No argument %d given. descj %d\n", desc[j].position, j);
           usage(desc, -1);
           }*/
	//THERE IS NO REQUIRED CHECKIING FOR FLAG TTYPES
    }
    if (!ok) {
        // check to see if this is a 'help' flag
        for (int j=0; desc[j].kind != ArgDesc::END; j++) {
            if ((desc[j].kind != ArgDesc::Flag)||(desc[j].kind != ArgDesc::Help)) continue;
            if (strcmp(desc[j].key, arg) == 0) {
                usage(desc, 0);
            }
        }
        fprintf(stderr, "Do not understand the flag [%s]\n", arg);
        usage(desc, -1);
    }
  }

  // now we are dealing with positional args
  int base = i;
  for (int j=0; desc[j].kind != ArgDesc::END; j++) {
      if (ArgDesc::verbose) printf("j=%d %s kind=%d\n", j, desc[j].key, desc[j].kind);
      if (desc[j].kind != ArgDesc::Position) continue;
      if (base+desc[j].position < argc) {
        if (ArgDesc::verbose) printf("%d: using %d (%d)\n", j, base+desc[j].position, desc[j].type);
        if (desc[j].type != Rest) used[base+desc[j].position] = true;
        req[j] = 1;
        switch (desc[j].type) {
        case ArgDesc::Vector:
            (desc[j].dest.vectorPtr)->push_back(argv[base+desc[j].position]);
            break;

        case ArgDesc::Pointer:
            *((void**)desc[j].dest.asptr) = argv[base+desc[j].position];
            break;

        case ArgDesc::Int:
            if (ArgDesc::verbose) printf("Assinging an int %d %d\n", base+i, j);
            *(desc[j].dest.asint) = atoi(argv[base+desc[j].position]);
            i++;
            break;

        case ArgDesc::Flag:
            printf("Flag in position??\n");
            exit(-1);
            break;

        case RestType:
            // rest of these should be returned to user
            break;

        default:
            printf("NIY: type %d\n", desc[j].type);
            exit(-1);
        }
    } 
  }
  // see if we have a variable number of args at end
  for (int j=0; desc[j].kind != ArgDesc::END; j++) {
      if ((desc[j].kind != ArgDesc::Position) || (desc[j].type != ArgDesc::Rest)) continue;

      // we have a case of a variable number of positional arguments at the end, collect them
      int num = 0;
      for (int i=0; i<argc; i++) {
          if (used[i] == false) num++;
      }
      posargc = num;
      posargv = new char*[num+1];
      int offset = 0;
      for (int i=0; i<num+1; i++) posargv[i] = NULL;
      for (int i=0; i<argc; i++) {
          if (used[i] == false) posargv[offset++] = argv[i];
      }
  }
  delete[] used;

  // check to make sure all reqd args were given
  bool allreq = true;
  for (int j=0; desc[j].kind != ArgDesc::END; j++) {
      if (ArgDesc::verbose) printf("%d:%d %d\n", j, desc[j].kind, req[j]);
      if (req[j] == 0) {
          allreq = false;
          switch (desc[j].kind) {
          case ArgDesc::Position:
              fprintf(stderr, "Argument %d required, but not present\n", desc[j].position);
              break;
          case ArgDesc::Flag:
              fprintf(stderr, "Flag %s required, but not present\n", desc[j].key);
              break;
          case ArgDesc::Rest:
              fprintf(stderr, "Variable number of parameters required, but none present\n");
              break;
          default:
              fprintf(stderr, "Unknown desc kind at %d: absent, but required\n", j);
              break;
          }
      }
  }
  if (!allreq) usage(desc, -1);
  delete[] req;
}

char* ArgDesc::progname = 0;
int ArgDesc::verbose = 0;
const char* ArgDesc::helpinfo = 0;
char** ArgDesc::posargv;
int ArgDesc::posargc;
char* ArgDesc::commandLine = 0;
ArgDesc* ArgDesc::list = 0;

void
ArgDesc::makeCommandline(int argc, char** argv)
{
  int len = 2;
  for (int i=0; i<argc; i++) {
      len += (int)(1 + strlen(argv[i]));
  }
  char* p = new char[len+1];
  len = 0;
  for (int i=0; i<argc; i++) {
    strcpy(p+len, argv[i]);
    len += (int)strlen(argv[i]);
    p[len++] = ' ';
  }
  p[len-1] = 0;
  if (verbose > 5) fprintf(stderr, "[%s]\n", p);
  commandLine = p;
}

char*
ArgDesc::getCommandLine(void)
{
    return commandLine;
}

char*
ArgDesc::getProgramName()
{
    return progname;
}

int
ArgDesc::positionalArgCount()
{
    return posargc;
}

char*
ArgDesc::arg(int x)
{
    return posargv[x];
}

void
ArgDesc::onexit(void)
{
    delete[] commandLine;
}

// convert any strings of 0 length to null
void
ArgDesc::blank2null(void)
{
    if (key[0] == 0) key = 0;
    if (longkey[0] == 0) longkey = 0;
    assert(key || longkey);
}


// Local Variables:
// indent-tabs-mode: nil
// c-basic-offset: 4
// End:
