//
//  Yacc.h
//  L1 Compiler
//  Declarations used by both Swift and the lexer/parser
//  Author: Jonathan Burns <jaburns@andrew.cmu.edu>
//

#ifndef Yacc_h
#define Yacc_h

#import "C0ParseNode.h"

#include "y.tab.h"

#define YYMAXDEPTH 100000

extern C0ParseNode *parseResult;
extern char *parseError;

typedef void parse_callback_fn();

extern parse_callback_fn* parse_callback;

void parse_success(C0ParseNode *result);

void yyerror(char *s);

typedef struct yy_buffer_state *YY_BUFFER_STATE;
YY_BUFFER_STATE yy_scan_bytes(const char *yy_str, int len);

int yyparse();
void yy_delete_buffer(YY_BUFFER_STATE b);

#endif /* Yacc_h */
