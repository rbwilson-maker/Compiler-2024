%{
//  c0lexer.lm
//  L1 Compiler
//  Lexer
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//
//  Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
//  Lexes forward compatible fragment of C0
//
//  Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
//  Updated to match 2014 spec
//
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//
//  Update this file to lex the necessary keywords and other tokens
//  in order to make the grammar forward compatible with C0.
//

#import "Yacc.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdocumentation"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wimplicit-function-declaration"

int comment_level = 0;

%}

%option noyywrap

%x block_comment
%x line_comment
%x endstate

%%

"{"                     { return LBRACE; }
"}"                     { return RBRACE; }
"("                     { return LPAREN; }
")"                     { return RPAREN; }
";"                     { return SEMI; }

"="                     { return ASSIGN; }
"+="                    { return PLUSEQ; }
"-="                    { return MINUSEQ; }
"*="                    { return STAREQ; }
"/="                    { return SLASHEQ; }
"%="                    { return PERCENTEQ; }

"+"                     { return PLUS; }
"-"                     { return MINUS; }
"*"                     { return STAR; }
"/"                     { return SLASH; }
"%"                     { return PERCENT; }

"struct"                { yyerror("Found reserved keyword in file"); }
"typedef"               { yyerror("Found reserved keyword in file"); }
"if"                    { yyerror("Found reserved keyword in file"); }
"else"                  { yyerror("Found reserved keyword in file"); }
"while"                 { yyerror("Found reserved keyword in file"); }
"for"                   { yyerror("Found reserved keyword in file"); }
"continue"              { yyerror("Found reserved keyword in file"); }
"break"                 { yyerror("Found reserved keyword in file"); }
"assert"                { yyerror("Found reserved keyword in file"); }
"true"                  { yyerror("Found reserved keyword in file"); }
"false"                 { yyerror("Found reserved keyword in file"); }
"NULL"                  { yyerror("Found reserved keyword in file"); }
"alloc"                 { yyerror("Found reserved keyword in file"); }
"alloc_array"           { yyerror("Found reserved keyword in file"); }
"bool"                  { yyerror("Found reserved keyword in file"); }
"void"                  { yyerror("Found reserved keyword in file"); }
"char"                  { yyerror("Found reserved keyword in file"); }
"string"                { yyerror("Found reserved keyword in file"); }
"--"                    { return MINUSMINUS; }   /* Illegal */

"return"                { return YRETURN; }
"int"                   { return TINT; }
"main"                  { return MAIN; }

"/*"                    { BEGIN(block_comment); comment_level++; }
"*/"                    { yyerror("unbalanced comments"); }
"//"                    { BEGIN(line_comment); }

<INITIAL><<EOF>>        { BEGIN(endstate); return TEOF; }

([A-Za-z_][A-Za-z0-9_]*)  { yylval.token = string_new(yytext); return IDENT; }
(0|[1-9]([0-9]*))         { yylval.token = string_new(yytext); return DECCONST; }
(0[xX][0-9a-fA-F]+)       { yylval.token = string_new(yytext); return HEXCONST; }

[ \t\r\x9-\xD]          { /* whitespace */ }
[\x00-\xFF]             { printf("Unexpected character: 0x%hhX\n", *(char*)yytext); yyerror("Unexpected character in file"); }

<line_comment>"\n"      { BEGIN(INITIAL); }
<line_comment>[^\n]+    { }
<line_comment><<EOF>>   { BEGIN(endstate); return TEOF; }
<block_comment>"/*"     { comment_level++; }
<block_comment>"*/"     { comment_level--; if (comment_level == 0) { BEGIN(INITIAL); } }
<block_comment>.        { }
<block_comment>"\n"     { }
<block_comment><<EOF>>  { yyerror("unterminated comment"); }

<endstate><<EOF>>       { return 0; }

%%

#pragma GCC diagnostic pop
