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

let expand_postop ~lhs ~op = 
  Asts.Marked.Asnop 
  { lhs
  ; op
  ; rhs = Mark.naked (Asts.Marked.Const Int32.one)
  }

let mexp_to_mmem (exp : Asts.Marked.mexp) : Asts.Marked.mmem = match (Mark.data exp) with
| Mem m -> m
| _ -> failwith "Parse error : expected mem found other exp"

let rec exp_to_lval (exp : Asts.Marked.exp) : Asts.Marked.lval = match exp with
| Var x -> LVar x
| Mem m -> mem_to_lval (Mark.data m)
| _ -> failwith "Parse error"

and mem_to_lval (mem : Asts.Marked.mem_exp) : Asts.Marked.lval = match mem with
| Get_field (mmem, field) -> LGet_field (Mark.map ~f:mem_to_lval mmem, field)
| Deref_field (mexp, field) -> LDeref_field (mexp_to_mlval mexp, field)
| Deref mexp -> LDeref (mexp_to_mlval mexp)
| Index {array; index} -> 
    LIndex 
    { array = mexp_to_mlval array
    ; index
    }

and mexp_to_mlval (mexp : Asts.Marked.mexp) : Asts.Marked.mlval = 
    Mark.map ~f:exp_to_lval mexp

%}

%token Eof
%token Semicolon Comma
%token <Int32.t> Dec_const
%token <Int32.t> Hex_const
%token <Symbol.t> Ident
%token <Symbol.t> Type_ident
%token If Else While For Assert (* Control keywords *)
%token Return
%token Int Bool Void
%token Plus Minus Star Slash Percent (* arithmetic operators *)
%token Assign Plus_eq Minus_eq Star_eq Slash_eq Percent_eq
%token Plus_plus Minus_minus
%token True False
%token Ampersands Bars Double_eq Exclamation Less Greater LEQ GEQ NEQ (* boolean operators *)
%token Question Colon
%token Left_arrows Right_arrows Bar Ampersand Caret Tilde (* bitwise operators *)
%token Larrs_eq Rarrs_eq Bar_eq Ampersand_eq Caret_eq
%token Null Alloc Alloc_array Struct Typedef
%token Dot Arrow (* Star as well *)
%token L_bracket R_bracket
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
%right Unary
%left Plus_plus Minus_minus
%right R_paren
%nonassoc Dot Arrow L_bracket

%start program

(* It's only necessary to provide the type of the start rule,
 * but it can improve the quality of parser type errors to annotate
 * the types of other rules.
 *)
%type <Asts.Marked.program> program
%type <Asts.Marked.gdecl> gdecl
%type <Asts.Marked.gdecl> fdecl
%type <Asts.Marked.gdecl> fdefn
%type <Symbol.t> field_ident
%type <Asts.Marked.field> field
%type <Asts.Marked.field list> field_list
%type <Asts.Marked.param> param
%type <Asts.Marked.param list> param_list_follow
%type <Asts.Marked.param list> param_list
%type <Asts.Marked.gdecl> typedef
%type <Asts.Marked.typ> typ
%type <Asts.Marked.ret_typ> ret_type
%type <Asts.Marked.stms> block
%type <Asts.Marked.stms> stms
%type <Asts.Marked.stm> stm
%type <Asts.Marked.mstm> m(stm)
%type <Asts.Marked.control> control
%type <Asts.Marked.declaration> decl
%type <Asts.Marked.simple> simp
%type <Asts.Marked.simple option> opt(simp)
%type <Asts.Marked.mexp list> arg_list_follow
%type <Asts.Marked.mexp list> arg_list
%type <Asts.Marked.exp> exp
%type <Asts.Marked.mexp> m(exp)
%type <Asts.Marked.mem_exp> mem_exp
%type <Asts.Marked.mmem> m(mem_exp)
%type <Core.Int32.t> int_const
%type <Asts.Marked.unop> unop
%type <Asts.Marked.binop> binop
%type <Asts.Marked.asnop> asnop
%type <Asts.Marked.stm> els
%type <Asts.Marked.mstm> m(els)
%type <Asts.Marked.mstm option> opt(m(els))
%type <Asts.Marked.exp> bool
%type <Asts.Marked.asnop> postop

%%

program :
  | Eof;
      { [] }
  | gdecl = gdecl; gdecls = program; Eof;
      { gdecl :: gdecls }
  ;

(* =============== Higher order rules =============== *)

(* This higher-order rule produces a marked result of whatever the
 * rule passed as argument will produce.
 *)
m(x) :
  | x = x; %prec Unary
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

(* ================================================== *)

gdecl :
  | fdecl = fdecl;
      { fdecl }
  | fdefn = fdefn;
      { fdefn }
  | typedef = typedef;
      { typedef }
  | Struct; s = Ident; Semicolon;
      { Asts.Marked.Sdecl s }
  | Struct; s = Ident; L_brace; fields = field_list; R_brace; Semicolon;
      { Asts.Marked.Sdefn (s, fields) }

fdecl : 
  | typ = ret_type;
    ident = Ident;
    params = param_list;
    Semicolon;
      { Asts.Marked.Fdecl (typ, ident, params) }

fdefn :
  | typ = ret_type;
    ident = Ident;
    params = param_list;
    body = block;
      { Asts.Marked.Fdefn (typ, ident, params, body) }

field_ident : (* fields have their own name space *)
  | ident = Ident;
      { ident }
  | typ_ident = Type_ident;
      { typ_ident }

field :
  | typ = typ; ident = field_ident; Semicolon;
      { (ident, typ) }

field_list :
  | (* empty *) 
      { [] }
  | field = field; rest = field_list; 
      { field::rest }

param :
  | typ = typ; ident = Ident;
      { (typ, ident) }

param_list_follow :
  | (* empty *)
      { [] }
  | Comma; param = param; rest = param_list_follow;
      { param :: rest }

param_list :
  | L_paren; R_paren;
      { [] }
  | L_paren; param = param; rest = param_list_follow; R_paren;
      { param :: rest }

typedef :
  | Typedef; typ = typ; ident = Type_ident; Semicolon;
      { Asts.Marked.Typedef (typ, ident) }

typ : 
  | Int 
      { Type.Int }
  | Bool
      { Type.Bool }
  | ident = Type_ident;
      { Type.Ident ident }
  | Struct; s = Ident;
      { Type.Struct s }
  | typ = typ; Star;
      { Type.Ptr typ }
  | typ = typ; L_bracket; R_bracket;
      { Type.Array (typ) }

ret_type :
  | typ = typ;
      { Type.Typ typ }
  | Void;
      { Type.Void }

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
      { Asts.Marked.Simple s }
  | b = block;
      { Asts.Marked.Block b }
  | c = control;
      { Asts.Marked.Control c }
  ;

control :
  (* as is, defaults to shift, which is how C0 parses it *)
  | If; L_paren; e = m(exp); R_paren; s1 = m(stm); s2 = opt(m(els))
      { Asts.Marked.If (e, s1, s2) }
  | While; L_paren; e = m(exp); R_paren; s = m(stm);
      { Asts.Marked.While (e, s) }
  | For; L_paren; init = opt(simp); Semicolon; cmp = m(exp); Semicolon; change=opt(simp); R_paren; body = m(stm);
      { Asts.Marked.For (init, cmp, change, body) }
  | Return; e = m(exp); Semicolon;
      { Asts.Marked.Return (Some e) }
  | Return; Semicolon;
      { Asts.Marked.Return None }
  | Assert; L_paren; e = m(exp); R_paren; Semicolon;
      { Asts.Marked.Assert e }
  ;

els :
  | Else; s = stm;
      { s }
  ;

simp :
  | d = decl;
      { Asts.Marked.Declare d }
  | lhs = m(exp); Assign; rhs = m(exp);
      { Asts.Marked.Assign (mexp_to_mlval lhs, rhs) }
  | lhs = m(exp); op = asnop; rhs = m(exp);
      { Asts.Marked.Asnop {lhs = mexp_to_mlval lhs; op; rhs} }
  | e = m(exp);
      { Asts.Marked.Do e }
  | lhs = m(exp); op = postop;
      { expand_postop ~lhs:(mexp_to_mlval lhs) ~op }
  (* specifically reject *x++ and *x-- -- not sure if this works anymore*)
  | Star; exp; postop; 
      { assert false }
  ;

decl :
  | t = typ; ident = Ident;
      { Asts.Marked.New_var (t, ident) }
  | t = typ; ident = Ident; Assign; e = m(exp);
      { Asts.Marked.Init (t, ident, e) }
  ;

arg_list_follow :
  | (* empty *)
      { [] }
  | Comma; exp = m(exp); rest = arg_list_follow;
      { exp :: rest }

arg_list :
  | L_paren; R_paren;
      { [] }
  | L_paren; exp = m(exp); rest = arg_list_follow; R_paren;
      { exp :: rest }
  ;

exp :
  | L_paren; e = exp; R_paren;
      { e }
  | id = Ident;
      { Asts.Marked.Var id }
  | c = int_const;
      { Asts.Marked.Const c }
  | b = bool;
      { b }
  | Alloc; L_paren; typ = typ; R_paren;
      { Asts.Marked.Alloc (typ) }
  | Alloc_array; L_paren; typ = typ; Comma; size = m(exp); R_paren
      { Asts.Marked.Alloc_array {typ; size} }
  | ident = Ident; args = arg_list;
      { Asts.Marked.Fn_call (ident, args) }
  | Null;
      { Asts.Marked.Null }
  | lhs = m(exp); op = binop; rhs = m(exp);
      { Asts.Marked.Binop { op; lhs; rhs; } }
  | op = unop; e = m(exp); %prec Unary
      { Asts.Marked.Unop { op; operand = e; } }
  | i = m(exp); Question; t = m(exp); Colon; e = m(exp)
      { Asts.Marked.Ternary { if_exp = i; then_exp = t; else_exp = e; } }
  | mem = m(mem_exp)
      { Asts.Marked.Mem mem}

mem_exp :
  | L_paren; mem = mem_exp; R_paren;
      { mem }
  | Star; e = m(exp); 
      { Asts.Marked.Deref e }
  | e = m(exp); Dot; ident = field_ident;
      { Asts.Marked.Get_field (mexp_to_mmem e, ident) }
  | e = m(exp); Arrow; ident = field_ident;
      { Asts.Marked.Deref_field (e, ident) }
  | array = m(exp); L_bracket; index = m(exp); R_bracket;
      { Asts.Marked.Index ({array; index}) : Asts.Marked.mem_exp }
  ;

int_const :
  | c = Dec_const;
      { c }
  | c = Hex_const;
      { c }
  ;

bool :
  | True;
      { Asts.Marked.True }
  | False;
      { Asts.Marked.False }

(* See the menhir documentation for %inline.
 * This allows us to factor out binary operators while still
 * having the correct precedence for binary operator expressions.
 *)
%inline
unop :
  | Minus;
      { Asts.Marked.Negative }
  | Exclamation;
      { Asts.Marked.Not }
  | Tilde;
      { Asts.Marked.Bit_not }

%inline
binop :
  | Plus;
      { Asts.Marked.Plus }
  | Minus;
      { Asts.Marked.Minus }
  | Star;
      { Asts.Marked.Times }
  | Slash;
      { Asts.Marked.Divided_by }
  | Percent;
      { Asts.Marked.Modulo }
  | Double_eq
      { Asts.Marked.Equal}
  | Less
      { Asts.Marked.Less }
  | Greater
      { Asts.Marked.Greater }
  | LEQ
      { Asts.Marked.Leq }
  | GEQ
      { Asts.Marked.Geq }
  | NEQ
      { Asts.Marked.Neq }
  | Ampersands
      { Asts.Marked.And }
  | Bars
      { Asts.Marked.Or }
  | Left_arrows
      { Asts.Marked.Lshift }
  | Right_arrows
      { Asts.Marked.Rshift }
  | Ampersand
      { Asts.Marked.Bit_and }
  | Caret
      { Asts.Marked.Bit_xor }
  | Bar
      { Asts.Marked.Bit_or }
  ;

asnop :
  | Plus_eq
      { Asts.Marked.Plus_eq }
  | Minus_eq
      { Asts.Marked.Minus_eq }
  | Star_eq
      { Asts.Marked.Times_eq }
  | Slash_eq
      { Asts.Marked.Div_eq }
  | Percent_eq
      { Asts.Marked.Mod_eq }
  | Larrs_eq
      { Asts.Marked.Lshift_eq }
  | Rarrs_eq
      { Asts.Marked.Rshift_eq }
  | Ampersand_eq
      { Asts.Marked.Bit_and_eq }
  | Bar_eq
      { Asts.Marked.Bit_or_eq }
  | Caret_eq
      { Asts.Marked.Bit_xor_eq }
  ;

postop : 
  | Plus_plus
      { Asts.Marked.Plus_eq }
  | Minus_minus
      { Asts.Marked.Minus_eq }

%%