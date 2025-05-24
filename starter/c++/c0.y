%{

#include <vector>
#include "type.h"
#include "cst.h"
#include "symbols.h"
#include "symtab.h"
#include "srcfile.h"
  
using namespace std;

extern int yylineno;
int yylex(void);
 void yyerror(SourceFile* f, char const *s);
int yyparse(SourceFile* f);
 int assignop2op(int aop);

 static Expression* makeBinop(Expression* left, int op, Expression* right)
 {
   Expression* e = new Expression(op, left, right);
   e->setLineNo(left->ln());
   return e;
 }


%}

%locations
%parse-param {SourceFile* srcfile}

%union {
  int ival;
  char *sval;
  Symbol sym;
  CST* cst;
  Expression* exp;
  Statements* stmts;
  Statement* stmt;
  Type* type;
}

%token <ival> STRUCT TYPEDEF IF ELSE WHILE FOR CONTINUE BREAK RETURN ASSERT TRUE FALSE NULLTOKEN ALLOC ALLOC_ARRAY 
%token <ival> INT BOOL VOID CHAR STRING PLUS MINUS STAR SLASH MOD ASSIGN PLUSASSIGN MINUSASSIGN 
%token <sym> ID 
%token <ival> STARASSIGN SLASHASSIGN MODASSIGN DECR CLOSEPARIN OPENPARIN OPENBRACE CLOSEBRACE MAIN SEMICOLON
%token <ival> NUM

%type <ival> assignop
%type <cst> program 
%type <stmts> stmts block
%type <exp> exp lval constant
%type <stmt>  stmt decl simp
%type <type> type

/* Precedence information to resolve ambiguity */
%left PLUS MINUS
%left STAR SLASH MOD

%start program

%%

assignop
	: PLUSASSIGN	{ $$ = $1; }
	| MINUSASSIGN	{ $$ = $1; }
	| STARASSIGN	{ $$ = $1; }
	| SLASHASSIGN	{ $$ = $1; }
	| MODASSIGN	{ $$ = $1; }
	;

constant: NUM
	{ 
	  $$ = new IntConstant($1); 
	  $$->setLineNo(yylineno);
	}
	;

exp
	: OPENPARIN exp CLOSEPARIN
	{ 
	  $$ = $2; 
	}
	| constant
	{
	  $$ = $1;
	}
	| ID
	{ 
	  Entry* e = SymbolTable::lookup($1);
	  if (e == 0) {
	    fprintf(stderr, "%d:%s not declared\n", yylineno, $1);
	    YYERROR;
	  } 
	  $$ = new Identifier(e); 
	  $$->setLineNo(yylineno);
	}
	| exp PLUS exp
	{ 
	  $$ = makeBinop($1, $2, $3);
	}
	| exp MINUS exp
	{ 
	  $$ = makeBinop($1, $2, $3);
	}
	| exp STAR exp
	{ 
	  $$ = makeBinop($1, $2, $3);
	}
	| exp SLASH exp
	{ 
	  $$ = makeBinop($1, $2, $3);
	}
	| exp MOD exp
	{ 
	  $$ = makeBinop($1, $2, $3);
	}
	| MINUS exp %prec STAR
	{ 
	  $$ = new Expression($1, $2); 
	  $$->setLineNo($2->ln());
	}
	;

lval
	: ID
	{ 
	  Entry* e = SymbolTable::lookup($1);
	  if (e == 0) {
	    fprintf(stderr, "%d:%s not declared\n", yylineno, $1);
	    YYERROR;
	  } 
	  $$ = new Identifier(e); 
	  $$->setLineNo(yylineno);
	}
	| OPENPARIN lval CLOSEPARIN
	{ $$ = $2; }
	;

simp
	: lval assignop exp
	{ 
	  Identifier* id = (Identifier*)$1;
	  Expression* expr = new Expression(assignop2op($2), id, $3);
	  expr->setLineNo($3->ln());
	  $$ = new Assignment(id, expr); 
	  $$->setLineNo($1->ln());
	}
	| lval ASSIGN exp
	{ 
	  Identifier* id = (Identifier*)$1;
	  //id->getEntry()->setInitialized();
	  $$ = new Assignment(id, $3); 
	  $$->setLineNo($1->ln());
	}
	;

type
	: INT
	{ $$ = new Type(); }
	;


decl
	: type ID
	{ 
	  if (SymbolTable::declaredHere($2)) {
	    fprintf(stderr, "%s already declared in current scope\n", $2);
	    YYERROR;
	  }
	  $$ = new Declaration($1, $2);
	  $$->setLineNo(yylineno);
	}
	| type ID ASSIGN exp
	{ 
	  if (SymbolTable::declaredHere($2)) {
	    fprintf(stderr, "%s already declared in current scope\n", $2);
	    YYERROR;
	  }
	  $$ = new Declaration($1, $2, $4);
	  $$->setLineNo(yylineno);
	}
	;

stmt
	: decl SEMICOLON
	{ $$ = $1; }
	| simp  SEMICOLON
	{ $$ = $1; }
	| RETURN exp  SEMICOLON
	{ 
	  //printf("reducing return: %d\n", yylineno); 
	  $$ = new Return($2); 
	  $$->setLineNo(yylineno);
	}
	;

stmts
	: block
	{ $$ = $1; }
	| stmt stmts
	{ //printf("reduced stmt: %d\n", $1->ln()); 
	  $2->prepend($1); 
	  $$=$2; 
	}
	| /* empty */
	{ 
	  $$ = new Statements(); 
	  $$->setLineNo(yylineno);
	}
	;

block
	: OPENBRACE 
	{ SymbolTable::enterScope(); }
	  stmts CLOSEBRACE
	  { 
	    $$ = $3;
	    SymbolTable::exitScope();
	  }
	;

program
	: INT ID
	{ $<ival>$ = yylineno; }
	  OPENPARIN CLOSEPARIN block
	{ 
	  if ($2 != Symbols::intern("main")) {
	    fprintf(stderr, "Expected a 'main' function, instead got '%s'\n", $2);
	    YYERROR;
	  }
	  Function* f = new Function(FunctionType::getMainType(), $2, $6);
	  f->setLineNo($<ival>3);
	  srcfile->addFunction(f);
	}
	;

%%

int assignop2op(int aop) {
  switch(aop) {
  case PLUSASSIGN: return PLUS;
  case MINUSASSIGN: return MINUS;
  case STARASSIGN: return STAR;
  case SLASHASSIGN: return SLASH;
  case MODASSIGN: return MOD;
  }
  assert(0);
  return -1;
}

#include <stdio.h>
extern char* yytext;
void yyerror(SourceFile* f, char const *s)
{
	fflush(stdout);
	printf("Error:%s:%d:%s (%s)\n", f->getInputFile(), yylineno, s, yytext);
	fflush(stdout);
}
