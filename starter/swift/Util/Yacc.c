//
//  Yacc.m
//  L1 Compiler
//  Handle lexer/parser success and failure, and pass the
//  result into the appropriate Swift function.
//  Author: Jonathan Burns <jaburns@andrew.cmu.edu>
//

#include <stdlib.h>
#include "Yacc.h"

C0ParseNode *parseResult = NULL;
char *parseError = NULL;

parse_callback_fn *parse_callback = NULL;

void parse_success(C0ParseNode *result) {
    parseResult = result;
    if (parse_callback != NULL) (*parse_callback)();
}

void yyerror(char *s) {
    parseError = s;
    if (parse_callback != NULL) (*parse_callback)();
}
