// global options

#ifndef _OPTIONS_H_
#define _OPTIONS_H_

#ifdef DEFINE_OPTIONS
# define OPTIONDEFINITION(opttype, name, defaultValue) opttype name = defaultValue
#else
# define OPTIONDEFINITION(opttype, name, defaultValue) extern opttype name
#endif

OPTIONDEFINITION(bool,  checkpointOnly,     false);
OPTIONDEFINITION(bool, 	typecheckOnly, 		false);
OPTIONDEFINITION(int, 	opt, 			0);
OPTIONDEFINITION(int, 	verbose, 		0);
OPTIONDEFINITION(bool, 	dumpAST, 		false);
OPTIONDEFINITION(bool, 	dumpIR, 		false);
OPTIONDEFINITION(bool, 	dumpAbstractAsm, 	false);
OPTIONDEFINITION(char*, outputType,		(char*)"x86-64");
OPTIONDEFINITION(char*, compilefile, 		(char *)"?");
OPTIONDEFINITION(bool,	dumpALL,		false);

#endif

