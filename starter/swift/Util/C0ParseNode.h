//
//  C0ParseNode.h
//  L1 Compiler
//  Because bison cannot interface directly with Swift, we use this
//  intermediary C datatype to store the results of parsing and immediately
//  translate the result into Swift enum types.
//  Author: Jonathan Burns <jaburns@andrew.cmu.edu>
//

#ifndef C0ParseNode_h
#define C0ParseNode_h

#include "../Parse/C0TokenType.h"

typedef struct C0ParseNode {
    C0TokenType token;
    char *value;
    struct C0ParseNode *firstChild;
    struct C0ParseNode *nextSibling;
} C0ParseNode;


C0ParseNode *node_new(C0TokenType type);
void node_prepend_child(C0ParseNode *node, C0ParseNode *child);
void node_append_child(C0ParseNode *node, C0ParseNode *child);
void node_free(C0ParseNode *node);

char *string_new(char *contents);
void free_all_strings();

#endif
