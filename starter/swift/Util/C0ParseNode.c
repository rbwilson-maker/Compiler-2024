//
//  C0ParseNode.m
//  L1 Compiler
//  Because bison cannot interface directly with Swift, we use
//  this intermediary C datatype to store the results of
//  parsing and immediately translate the result into Swift enum
//  types.
//  Author: Jonathan Burns <jaburns@andrew.cmu.edu>
//

#include <stdlib.h>
#include <string.h>
#include "C0ParseNode.h"

struct string_item {
    char *string;
    struct string_item* next;
};

struct string_item* first_string = NULL;

C0ParseNode *node_new(C0TokenType type) {
    C0ParseNode *node = malloc(sizeof(C0ParseNode));
    node->token = type;
    node->value = NULL;
    node->firstChild = NULL;
    node->nextSibling = NULL;
    return node;
}

void node_prepend_child(C0ParseNode *node, C0ParseNode *child) {
    child->nextSibling = node->firstChild;
    node->firstChild = child;
}

void node_append_child(C0ParseNode *node, C0ParseNode *child) {
    C0ParseNode *lastChild = node->firstChild;
    if (lastChild == NULL) {
        node->firstChild = child;
        return;
    }
    while (lastChild->nextSibling != NULL) {
        lastChild = lastChild->nextSibling;
    }
    lastChild->nextSibling = child;
}

void node_free(C0ParseNode *node) {
    if (node == NULL) return;
    node_free(node->firstChild);
    node_free(node->nextSibling);
    free(node);
}

char *string_new(char *contents) {
    char *result = malloc(strlen(contents)+1);
    strncpy(result, contents, strlen(contents)+1);
    struct string_item *item = malloc(sizeof(struct string_item));
    item->string = result;
    item->next = first_string;
    first_string = item;
    return result;
}

void free_all_strings() {
    struct string_item *curr = first_string;
    struct string_item *next = NULL;
    while (curr != NULL) {
        next = curr->next;
        free(curr->string);
        free(curr);
        curr = next;
    }
}
