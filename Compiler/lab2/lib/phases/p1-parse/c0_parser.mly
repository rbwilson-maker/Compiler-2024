%{
(* L1 Compiler
 * L1 grammar
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now conforms to the L1 fragment of C0
 *
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with 2014 spec
 *
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 *   - Update to use Core instead of Core.Std and ppx
 *
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Update to use menhir instead of ocamlyacc.
 *   - Improve presentation of marked asts.
 *
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

let mark
  (data : 'a)
  (start_pos : Lexing.position)
  (end_pos : Lexing.position) : 'a Mark.t =
  let src_span = Mark.of_positions start_pos end_pos in
  Mark.mark data src_span

(* expand_asnop (id, "op=", exp) region = "id = id op exps"
 * or = "id = exp" if asnop is "="
 * syntactically expands a compound assignment operator
 *)
let expand_asnop ~lhs ~op ~rhs
  (start_pos : Lexing.position)
  (end_pos : Lexing.position) =
    match lhs, op, rhs with
    | id, None, exp -> Ast.Assign (Mark.data id, exp)
    | id, Some op, exp ->
      let binop = Ast.Binop {
        op;
        lhs = Mark.map lhs ~f:(fun id -> Ast.Var id);
        rhs = exp;
      } in
      Ast.Assign (Mark.data id, mark binop start_pos end_pos)

let expand_postop ~lhs ~op 
  (start_pos : Lexing.position)
  (end_pos : Lexing.position) = 
    Ast.Assign (
      Mark.data lhs,
      mark (Ast.Binop {
        op; 
        lhs = Mark.map lhs ~f:(fun id -> Ast.Var id);
        rhs = Mark.naked (Ast.Const (Int32.of_int 1))
      }) start_pos end_pos
    )
%}

%token Eof
%token Semicolon
%token <Int32.t> Dec_const
%token <Int32.t> Hex_const
%token <Symbol.t> Ident
%token If Else While For (* Control keywords *)
%token Return
%token Int Bool
%token Plus Minus Star Slash Percent (* arithmetic operators *)
%token Assign Plus_eq Minus_eq Star_eq Slash_eq Percent_eq
%token Plus_plus Minus_minus
%token True False
%token Ampersands Bars Double_eq Exclamation Less Greater LEQ GEQ NEQ (* boolean operators *)
%token Question Colon
%token Left_arrows Right_arrows Bar Ampersand Caret Tilde (* bitwise operators *)
%token Larrs_eq Rarrs_eq Bar_eq Ampersand_eq Caret_eq
%token L_brace R_brace
%token L_paren R_paren
%token Unary

(* Unary is a dummy terminal.
 * We need dummy terminals if we wish to assign a precedence
 * to a production that does not correspond to the precedence of
 * the rightmost terminal in that production.
 * Implicit in this is that precedence can only be inferred for
 * terminals. Therefore, don't try to assign precedence to "rules"
 * or "productions".
 *)
%nonassoc Low_prec
%right Question Colon Else
%left Bars
%left Ampersands
%left Bar
%left Caret
%left Ampersand
%left NEQ Double_eq
%left Less Greater LEQ GEQ
%left Left_arrows Right_arrows
%left Plus Minus
%left Star Slash Percent
%left R_paren // Unsure about associativity
%right Unary

%start program

(* It's only necessary to provide the type of the start rule,
 * but it can improve the quality of parser type errors to annotate
 * the types of other rules.
 *)
%type <Ast.program> program
%type <Ast.stms> block
%type <Ast.stms> stms
%type <Ast.stm> stm
%type <Ast.mstm> m(stm)
%type <Ast.control> control
%type <Ast.declaration> decl
%type <Ast.simple> simp
%type <Ast.simple option> opt(simp)
%type <Symbol.t> lvalue
%type <Symbol.t Mark.t> m(lvalue)
%type <Ast.exp> exp
%type <Ast.mexp> m(exp)
%type <Ast.typ> typ
%type <Core.Int32.t> int_const
%type <Ast.binop> binop
%type <Ast.binop option> asnop
%type <Ast.stm> els
%type <Ast.mstm> m(els)
%type <Ast.mstm option> opt(m(els))
%type <Ast.exp> bool
%type <Ast.binop> postop

%%

program :
  | Int;
    ident = Ident;
    L_paren R_paren;
    L_brace;
    body = stms;
    R_brace;
    Eof;
      { (ident, body) }
  ;

(* This higher-order rule produces a marked result of whatever the
 * rule passed as argument will produce.
 *)
m(x) :
  | x = x;
      (* $startpos(s) and $endpos(s) are menhir's replacements for
       * Parsing.symbol_start_pos and Parsing.symbol_end_pos, but,
       * unfortunately, they can only be called from productions. *)
      { mark x $startpos(x) $endpos(x) }
  ;

(* Rachel is so smart for this one *)
opt(x) :
  | (* empty *) %prec Low_prec
      { None }
  | x = x;
      { Some x }
  ;

block :
  | L_brace; body = stms; R_brace;
      { body }
  ;

stms :
  | (* empty *)
      { [] }
  | hd = m(stm); tl = stms;
      { hd :: tl }
  ;

stm :
  | s = simp; Semicolon;
      { Ast.Simple s }
  | b = block;
      { Ast.Block b }
  | c = control;
      { Ast.Control c }
  ;

control :
  (* as is, defaults to shift, which is how C0 parses it *)
  | If; L_paren; e = m(exp); R_paren; s1 = m(stm); s2 = opt(m(els))
      { Ast.If (e, s1, s2) }
  | While; L_paren; e = m(exp); R_paren; s = m(stm);
      { Ast.While (e, s) }
  | For; L_paren; init = opt(simp); Semicolon; cmp = m(exp); Semicolon; change=opt(simp); R_paren; body = m(stm);
      { Ast.For (init, cmp, change, body) }
  | Return; e = m(exp); Semicolon;
      { Ast.Return e }
  ;

els :
  | Else; s = stm;
      { s }

typ : 
  | Int 
      { Ast.Int }
  | Bool
      { Ast.Bool }

simp :
  | d = decl;
      { Ast.Declare d }
  | lhs = m(lvalue);
    op = asnop;
    rhs = m(exp);
      { expand_asnop ~lhs ~op ~rhs $startpos(lhs) $endpos(rhs) }
  | e = m(exp);
      { Ast.Do e }
  | lhs = m(lvalue); op = postop;
      { expand_postop ~lhs ~op $startpos(lhs) $endpos(op)}
  ;

decl :
  | t = typ; ident = Ident;
      { Ast.New_var (t, ident) }
  | t = typ; ident = Ident; Assign; e = m(exp);
      { Ast.Init (t, ident, e) }
  ;

lvalue :
  | ident = Ident;
      { ident }
  | L_paren; lhs = lvalue; R_paren;
      { lhs }
  ;

exp :
  | L_paren; e = exp; R_paren;
      { e }
  | c = int_const;
      { Ast.Const c }
  | b = bool;
      { b }
  | ident = lvalue; %prec Low_prec
      { Ast.Var ident }
  | lhs = m(exp);
    op = binop;
    rhs = m(exp);
      { Ast.Binop { op; lhs; rhs; } }
  | Minus; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.Negative; operand = e; } }
  | Exclamation; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.Not; operand = e; } }
  | Tilde; e = m(exp); %prec Unary
      { Ast.Unop { op = Ast.Bit_not; operand = e; } }
  | i = m(exp); Question; t = m(exp); Colon; e = m(exp)
      { Ast.Ternary { if_exp = i; then_exp = t; else_exp = e; } }
  ;

int_const :
  | c = Dec_const;
      { c }
  | c = Hex_const;
      { c }
  ;

bool :
  | True;
      { Ast.True }
  | False;
      { Ast.False }

(* See the menhir documentation for %inline.
 * This allows us to factor out binary operators while still
 * having the correct precedence for binary operator expressions.
 *)
%inline
binop :
  | Plus;
      { Ast.Plus }
  | Minus;
      { Ast.Minus }
  | Star;
      { Ast.Times }
  | Slash;
      { Ast.Divided_by }
  | Percent;
      { Ast.Modulo }
  | Double_eq
      { Ast.Equal}
  | Less
      { Ast.Less }
  | Greater
      { Ast.Greater }
  | LEQ
      { Ast.Leq }
  | GEQ
      { Ast.Geq }
  | NEQ
      { Ast.Neq }
  | Ampersands
      { Ast.And }
  | Bars
      { Ast.Or }
  | Left_arrows
      { Ast.Lshift }
  | Right_arrows
      { Ast.Rshift }
  | Ampersand
      { Ast.Bit_and }
  | Caret
      { Ast.Bit_xor }
  | Bar
      { Ast.Bit_or }
  ;

asnop :
  | Assign
      { None }
  | Plus_eq
      { Some Ast.Plus }
  | Minus_eq
      { Some Ast.Minus }
  | Star_eq
      { Some Ast.Times }
  | Slash_eq
      { Some Ast.Divided_by }
  | Percent_eq
      { Some Ast.Modulo }
  | Larrs_eq
      { Some Ast.Lshift }
  | Rarrs_eq
      { Some Ast.Rshift }
  | Ampersand_eq
      { Some Ast.Bit_and }
  | Bar_eq
      { Some Ast.Bit_or }
  | Caret_eq
      { Some Ast.Bit_xor }
  ;

postop : 
  | Plus_plus
      { Ast.Plus }
  | Minus_minus
      { Ast.Minus }

%%
