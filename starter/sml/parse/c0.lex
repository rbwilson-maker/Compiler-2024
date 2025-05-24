(* L1 Compiler
 * Lexer
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Modified: Taegyun Kim <taegyunk@cmu.edu> Fall 2014
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 * Lexes forward compatible fragment of C0.
 *
 * Update this file to lex the necessary keywords and other tokens
 * in order to make the grammar forward compatible with C0.
 *)

structure A = Ast
structure S = Symbol

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

local
  val commentLevel = ref 0
  val commentPos = ref 0
in
  fun enterComment yypos =
    ( commentLevel := !commentLevel + 1
    ; commentPos := yypos
    )

  fun exitComment () =
    ( commentLevel := !commentLevel - 1
    ; !commentLevel = 0
    )

  fun number (yyt, yyp) =
    let
      val max: Word32.word = 0wx80000000
      val min: Word32.word = 0wx00000000
      val ext = ParseState.ext (yyp, yyp + size yyt)
      val numOpt = Word32Signed.fromString yyt
                      handle Overflow => NONE
    in
      case numOpt of
        NONE => ( ErrorMsg.error ext
                    ("cannot parse integral constant `" ^ yyt ^ "'")
                ; raise ErrorMsg.Error
                )
      | SOME n =>
          if (n <= max andalso n >= min)
          then Tokens.INTCONST (n,yyp,yyp + size yyt)
          else ( ErrorMsg.error ext ("integer out of range `" ^ yyt)
               ; raise ErrorMsg.Error
               )
    end

  fun hexnumber (yyt, yyp) =
    let
      val ext = ParseState.ext (yyp, yyp + size yyt)
      val numOpt = Word32.fromString yyt handle Overflow => NONE
    in
      case numOpt of
        NONE => ( ErrorMsg.error ext
                    ("Hexadecimal constant out of range `" ^ yyt ^ "'")
                ; raise ErrorMsg.Error
                )
      | SOME n => Tokens.INTCONST (n, yyp, yyp + size yyt)
    end

  fun eof () =
    ( if (!commentLevel > 0)
      then (ErrorMsg.error (ParseState.ext (!commentPos,!commentPos)) "unterminated comment")
      else ()
    ; Tokens.EOF (0,0) (* bogus position information; unused *)
    )

end

%%
%header (functor L1LexFn(structure Tokens : L1_TOKENS));
%full
%s COMMENT COMMENT_LINE;

id = [A-Za-z_][A-Za-z0-9_]*;
decnum = 0 | [1-9][0-9]*;
hexnum = 0[xX][0-9a-fA-F]+;

ws = [\ \t\011\012\013\014\015];

%%

<INITIAL> {ws}+       => (lex ());
<INITIAL> \n          => (ParseState.newline(yypos); lex());

<INITIAL> "{"         => (Tokens.LBRACE (yypos, yypos + size yytext));
<INITIAL> "}"         => (Tokens.RBRACE (yypos, yypos + size yytext));
<INITIAL> "("         => (Tokens.LPAREN (yypos, yypos + size yytext));
<INITIAL> ")"         => (Tokens.RPAREN (yypos, yypos + size yytext));

<INITIAL> ";"         => (Tokens.SEMI (yypos, yypos + size yytext));

<INITIAL> "="         => (Tokens.ASSIGN (yypos, yypos + size yytext));
<INITIAL> "+="        => (Tokens.PLUSEQ (yypos, yypos + size yytext));
<INITIAL> "-="        => (Tokens.MINUSEQ (yypos, yypos + size yytext));
<INITIAL> "*="        => (Tokens.STAREQ (yypos, yypos + size yytext));
<INITIAL> "/="        => (Tokens.SLASHEQ (yypos, yypos + size yytext));
<INITIAL> "%="        => (Tokens.PERCENTEQ (yypos, yypos + size yytext));

<INITIAL> "+"         => (Tokens.PLUS (yypos, yypos + size yytext));
<INITIAL> "-"         => (Tokens.MINUS (yypos, yypos + size yytext));
<INITIAL> "*"         => (Tokens.STAR (yypos, yypos + size yytext));
<INITIAL> "/"         => (Tokens.SLASH (yypos, yypos + size yytext));
<INITIAL> "%"         => (Tokens.PERCENT (yypos, yypos + size yytext));

<INITIAL> "--"        => (Tokens.MINUSMINUS (yypos, yypos + size yytext));

<INITIAL> "main"      => (Tokens.MAIN(yypos, yypos + size yytext));
<INITIAL> "return"    => (Tokens.RETURN (yypos, yypos + size yytext));
<INITIAL> "int"       => (Tokens.INT (yypos, yypos + size yytext));

<INITIAL> "struct"    => (Tokens.STRUCT (yypos, yypos + size yytext));
<INITIAL> "typedef"   => (Tokens.TYPEDEF (yypos, yypos + size yytext));
<INITIAL> "if"        => (Tokens.IF (yypos, yypos + size yytext));
<INITIAL> "else"      => (Tokens.ELSE (yypos, yypos + size yytext));
<INITIAL> "while"     => (Tokens.WHILE (yypos, yypos + size yytext));
<INITIAL> "for"       => (Tokens.FOR (yypos, yypos + size yytext));
<INITIAL> "continue"  => (Tokens.CONTINUE (yypos, yypos + size yytext));
<INITIAL> "break"     => (Tokens.BREAK (yypos, yypos + size yytext));
<INITIAL> "assert"    => (Tokens.ASSERT (yypos, yypos + size yytext));
<INITIAL> "true"      => (Tokens.TRUE (yypos, yypos + size yytext));
<INITIAL> "false"     => (Tokens.FALSE (yypos, yypos + size yytext));
<INITIAL> "NULL"      => (Tokens.NULL (yypos, yypos + size yytext));
<INITIAL> "alloc"     => (Tokens.ALLOC (yypos, yypos + size yytext));
<INITIAL> "alloc_array" => (Tokens.ALLOCARR (yypos, yypos + size yytext));
<INITIAL> "bool"      => (Tokens.BOOL (yypos, yypos + size yytext));
<INITIAL> "void"      => (Tokens.VOID (yypos, yypos + size yytext));
<INITIAL> "char"      => (Tokens.CHAR (yypos, yypos + size yytext));
<INITIAL> "string"    => (Tokens.STRING (yypos, yypos + size yytext));

<INITIAL> {decnum}    => (number (yytext, yypos));
<INITIAL> {hexnum}    => (hexnumber (yytext, yypos));

<INITIAL> {id}        => (let
                            val id = Symbol.symbol yytext
                          in
                            Tokens.IDENT (id, yypos, yypos + size yytext)
                          end);

<INITIAL> "/*"        => (YYBEGIN COMMENT; enterComment yypos; lex());
<INITIAL> "*/"        => (ErrorMsg.error (ParseState.ext (yypos, yypos)) "unbalanced comments";
                          raise ErrorMsg.Error);

<INITIAL> "//"        => (YYBEGIN COMMENT_LINE; lex());
<INITIAL> .           => (ErrorMsg.error (ParseState.ext (yypos,yypos))
                              ("illegal character: \"" ^ yytext ^ "\"");
                          raise ErrorMsg.Error);

<COMMENT> "/*"        => (enterComment yypos; lex());
<COMMENT> "*/"        => (if exitComment () then YYBEGIN INITIAL else (); lex());
<COMMENT> \n          => (ParseState.newline yypos; lex ());
<COMMENT> .           => (lex());

<COMMENT_LINE> \n     => (ParseState.newline yypos; YYBEGIN INITIAL; lex());
<COMMENT_LINE> .      => (lex());
