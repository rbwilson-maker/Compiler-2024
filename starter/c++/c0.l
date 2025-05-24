%{
#include <stdlib.h>
#include <stdio.h>
#include "cst.h"
#include "srcfile.h"
#include "symbols.h"
using namespace std;
#include "c0.tab.h"
int numVal;
int commentDepth = 0;
#define YY_NO_UNPUT   /* keep g++ happy */
%}

%option yylineno
%x 		InComment
%option noyywrap

alpha		[a-zA-Z_]
digit		[0-9]
decnum		0|[1-9]{digit}*
hexnum		0[xX][0-9a-fA-F]+

%%
"/*"             	{ BEGIN(InComment); }
<InComment>{
	[\n\f]         		{ /*yylineno = yylineno + 1;*/ }
	"/*"           		{ ++commentDepth; }
	"*"+"/"        		{ if (commentDepth) --commentDepth;
                   		  else BEGIN(INITIAL); }
	"*"+           		;
	[^*/]+	       		;
	[/]            		;
}

[ \t\r\v]		;
[ \n\f]   		{ /*yylineno = yylineno + 1;*/ }
"struct"		return STRUCT;
"typedef"		return TYPEDEF;
"if"			return IF;
"else"			return ELSE;
"while"			return WHILE;
"for"			return FOR;
"continue"		return CONTINUE;
"break"			return BREAK;
"return"		return RETURN;
"assert"		return ASSERT;
"true"			return TRUE;
"false"			return FALSE;
"NULL"			return NULLTOKEN;
"alloc"			return ALLOC;
"alloc_array"		return ALLOC_ARRAY;
"int"			return INT;
"bool"			return BOOL;
"void"			return VOID;
"char"			return CHAR;
"string"		return STRING;
{decnum}       		{yylval.ival = atoi(yytext); return NUM;}
{hexnum}       		{yylval.ival = (int)strtol(yytext, NULL, 16); return NUM;}
{alpha}({alpha}|{digit})*		{yylval.sym = Symbols::intern(yytext); return ID;}
"("			return OPENPARIN;
")"			return CLOSEPARIN;
"{"			return OPENBRACE;
"}"			return CLOSEBRACE;
"+"			{ yylval.ival =  PLUS; return PLUS; }
"-"			{ yylval.ival =  MINUS; return MINUS; }
"*"			{ yylval.ival = STAR; return STAR; }
"/"			{ yylval.ival =  SLASH; return SLASH; }
"%"			{ yylval.ival =  MOD; return MOD; }
"="			{ yylval.ival =  ASSIGN; return ASSIGN; }
"+="			{ yylval.ival =  PLUSASSIGN; return PLUSASSIGN; }
"-="			{ yylval.ival =  MINUSASSIGN; return MINUSASSIGN; }
"*="			{ yylval.ival =  STARASSIGN; return STARASSIGN; }
"/="			{ yylval.ival =  SLASHASSIGN; return SLASHASSIGN; }
"%="			{ yylval.ival =  MODASSIGN; return MODASSIGN; }
"--"			{ yylval.ival =  DECR; return DECR; }
";"			return SEMICOLON;
\/\/.*			;
.       		return yytext[0];

%%

