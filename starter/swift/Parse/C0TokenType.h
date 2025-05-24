//
//  C0ParseNode.h
//  L1 Compiler
//  Declares the available tokens for use in the bison parser
//  Author: Jonathan Burns <jaburns@andrew.cmu.edu>
//

#ifndef C0TokenType_h
#define C0TokenType_h

typedef enum {
    C0TokenTypeStmts,
    C0TokenTypeStmtReturn,
    C0TokenTypeStmtNewVar,
    C0TokenTypeStmtInit,
    C0TokenTypeStmtAssignment,
    
    C0TokenTypeSymbol,
    C0TokenTypeDecConstant,
    C0TokenTypeHexConstant,
    C0TokenTypePlus,
    C0TokenTypeMinus,
    C0TokenTypeTimes,
    C0TokenTypeDividedBy,
    C0TokenTypeMod,
    C0TokenTypeNegative
} C0TokenType;

#endif /* C0TokenType_h */
