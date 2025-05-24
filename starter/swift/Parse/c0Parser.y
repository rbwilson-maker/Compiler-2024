%{
//  c0parser.ym
//  L1 Compiler
//  L1 grammar
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//
//  Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
//  Now conforms to the L1 fragment of C0
//
//  Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
//  Should be more up-to-date with 2014 spec
//
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>

#import "Yacc.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"
#pragma GCC diagnostic ignored "-Wunreachable-code"
#pragma GCC diagnostic ignored "-Wincompatible-pointer-types-discards-qualifiers"

%}

%union {
    char *token;
    C0ParseNode* ast;
}

%token <token> STRUCT TYPEDEF IF ELSE WHILE FOR CONTINUE BREAK
%token <token> ASSERT TTRUE TFALSE TNULL ALLOC ALLOCARRY
%token <token> TBOOL TVOID TCHAR TSTRING
%token <token> SEMI
%token <token> DECCONST
%token <token> HEXCONST
%token <token> IDENT
%token <token> YRETURN
%token <token> TINT
%token <token> MAIN
%token <token> ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ
%token <token> LBRACE RBRACE
%token <token> LPAREN RPAREN
%token <token> ASNOP
%token <token> MINUSMINUS
%token <token> TEOF
/* UNARY and ASNOP are dummy terminals.
 * We need dummy terminals if we wish to assign a precedence
 * to a rule that does not correspond to the precedence of
 * the rightmost terminal in that rule.
 * Implicit in this is that precedence can only be infered
 * terminals. Therefore, don't try to assign precedence to "rules"
 *
 * MINUSMINUS is a dummy terminal to parse fail on.
 */

%left PLUS MINUS
%left STAR SLASH PERCENT
%right UNARY
%left LPAREN

%type <ast> program stmts stmt decl simp lvalue exp
%type <ast> intconst
%type <token> asnop

%start program

%error-verbose

%%

program :
    TINT MAIN LPAREN RPAREN LBRACE stmts RBRACE TEOF { parse_success($6); }
    ;

stmts :
    /* empty */                   { $$ = node_new(C0TokenTypeStmts);}
    | stmt stmts                  { node_prepend_child($2, $1); $$ = $2; }
    | LBRACE stmts RBRACE         { $$ = $2; }
    ;

stmt :
    decl SEMI                     { $$ = $1; }
    | simp SEMI                   { $$ = $1; }
    | YRETURN exp SEMI            { $$ = node_new(C0TokenTypeStmtReturn);
                                    node_append_child($$, $2); }
    ;

decl :
    TINT IDENT                    { $$ = node_new(C0TokenTypeStmtNewVar);
                                    $$->value = $2; }
    | TINT IDENT ASSIGN exp       { $$ = node_new(C0TokenTypeStmtInit);
                                    $$->value = $2; node_append_child($$, $4); }
    | TINT MAIN                   { $$ = node_new(C0TokenTypeStmtNewVar);
                                    $$->value = "main"; }
    | TINT MAIN ASSIGN exp        { $$ = node_new(C0TokenTypeStmtInit);
                                    $$->value = "main"; node_append_child($$, $4); }
    ;

simp :
    lvalue asnop exp %prec ASNOP  { $$ = node_new(C0TokenTypeStmtAssignment);
                                    $$->value = $2; node_append_child($$, $1); node_append_child($$, $3); }
  ;

lvalue :
    IDENT                         { $$ = node_new(C0TokenTypeSymbol); $$->value = $1; }
    | MAIN                        { $$ = node_new(C0TokenTypeSymbol); $$->value = "main"; }
    | LPAREN lvalue RPAREN        { $$ = $2; }
    ;

exp :
    LPAREN exp RPAREN             { $$ = $2; }
    | intconst                    { $$ = $1; }
    | MAIN                        { $$ = node_new(C0TokenTypeSymbol); $$->value = "main"; }
    | IDENT                       { $$ = node_new(C0TokenTypeSymbol); $$->value = $1; }
    | exp PLUS exp                { $$ = node_new(C0TokenTypePlus);
                                    node_append_child($$, $1); node_append_child($$, $3); }
    | exp MINUS exp               { $$ = node_new(C0TokenTypeMinus);
                                    node_append_child($$, $1); node_append_child($$, $3); }
    | exp STAR exp                { $$ = node_new(C0TokenTypeTimes);
                                    node_append_child($$, $1); node_append_child($$, $3); }
    | exp SLASH exp               { $$ = node_new(C0TokenTypeDividedBy);
                                    node_append_child($$, $1); node_append_child($$, $3); }
    | exp PERCENT exp             { $$ = node_new(C0TokenTypeMod);
                                    node_append_child($$, $1); node_append_child($$, $3); }
    | MINUS exp %prec UNARY       { $$ = node_new(C0TokenTypeNegative);
                                    node_append_child($$, $2); }
    ;

intconst :
    DECCONST                      { $$ = node_new(C0TokenTypeDecConstant); $$->value = $1; }
    | HEXCONST                    { $$ = node_new(C0TokenTypeHexConstant); $$->value = $1; }
    ;

asnop :
    ASSIGN                        { $$ = "="; }
    | PLUSEQ                      { $$ = "+="; }
    | MINUSEQ                     { $$ = "-="; }
    | STAREQ                      { $$ = "*="; }
    | SLASHEQ                     { $$ = "/="; }
    | PERCENTEQ                   { $$ = "%="; }
    ;

%%

#pragma GCC diagnostic pop
