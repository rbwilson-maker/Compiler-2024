(* Rachompicole L3 Compiler
 * Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * The AST resembles the parser as close as possible to maintain
 * the original program. Typechecking is done on this interface.
 *
 *)

open Core

(* Notice that we use subexpressions and statements that are marked.
 * (That is, the subexpressions are of type exp Mark.t, not just
 * type exp.) This means that source code location (a src_span) is
 * associated with the subexpression. Currently, the typechecker uses
 * this source location to print more helpful error messages.
 *
 * It's the parser and lexer's job to associate src_span locations with each
 * ast. It's instructive, but not necessary, to closely read the source code
 * for c0_parser.mly, c0_lexer.mll, and parse.ml to get a good idea of how
 * src_spans are created.
 *)

type todo = |

type typ =
  | Int
  | Bool
  | Void
  | Ident of Symbol.t
[@@deriving equal]

(* Operator *)
type binop =
  | Plus
  | Minus
  | Times
  | Divided_by
  | Modulo
  | Less
  | Greater
  | Equal
  | Leq
  | Geq
  | Neq
  | And
  | Or
  | Bit_and
  | Bit_or
  | Bit_xor
  | Lshift
  | Rshift

type unop =
  | Negative
  | Not
  | Bit_not

(* Expression *)
type exp =
  | Var of Symbol.t
  | Const of Int32.t
  | True
  | False
  | Binop of
      { op : binop
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }
  | Ternary of
      { if_exp : mexp
      ; then_exp : mexp
      ; else_exp : mexp
      }
  | Fn_call of Symbol.t * mexp list

(* Expression plus src file location *)
and mexp = exp Mark.t

type declaration =
  (* typ x; *)
  | New_var of typ * Symbol.t
  (* typ x = e; *)
  | Init of typ * Symbol.t * mexp

  type simple =
  | Declare of declaration
  (* x = e; *)
  | Assign of Symbol.t * mexp
  (* e; *)
  | Do of mexp

and control =
  (* if (condition) {...} else {...} *)
  | If of mexp * mstm * mstm option
  (* while (condition) {...} *)
  | While of mexp * mstm
  (* for (s1;condition;s2) {...} *)
  | For of simple option * mexp * simple option * mstm
  | Return of mexp option
  | Assert of mexp

(* Statement *)
and stm =
  (* { s1; s2; ... } *)
  | Block of stms
  (* single stm, no nested blocks *)
  | Simple of simple
  (* loops and jumps with nested blocks *)
  | Control of control

(* Statement plus src file location *)
and mstm = stm Mark.t

(* Block of statements *)
and stms = mstm list

type param = typ * Symbol.t

type gdecl =
  | Fdecl of typ * Symbol.t * param list
  | Fdefn of typ * Symbol.t * param list * stms
  | Typedef of typ * Symbol.t

type program = gdecl list

module Print = struct
  open Print

  let pp_binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divided_by -> "/"
    | Modulo -> "%"
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "=="
    | Leq -> "<="
    | Geq -> ">="
    | Neq -> "!="
    | And -> "&&"
    | Or -> "||"
    | Lshift -> "<<"
    | Rshift -> ">>"
    | Bit_or -> "|"
    | Bit_and -> "&"
    | Bit_xor -> "^"
  ;;

  let pp_unop = function
    | Negative -> "-"
    | Not -> "!"
    | Bit_not -> "~"
  ;;

  let rec pp_exp = function
    | Var id -> Symbol.name id
    | Const c -> Int32.to_string c
    | True -> "true"
    | False -> "false"
    | Unop unop -> sprintf "%s(%s)" (pp_unop unop.op) (pp_mexp unop.operand)
    | Binop binop ->
      sprintf "(%s %s %s)" (pp_mexp binop.lhs) (pp_binop binop.op) (pp_mexp binop.rhs)
    | Ternary t ->
      sprintf
        "(%s ? %s : %s)"
        (pp_mexp t.if_exp)
        (pp_mexp t.then_exp)
        (pp_mexp t.else_exp)
    | Fn_call (fn, args) ->
      sprintf "%s(%s)" (Symbol.name fn) (pp_list pp_mexp args)

  and pp_mexp e = pp_exp (Mark.data e)

  let pp_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Void -> "void"
    | Ident s -> Symbol.name s;
  ;;

  let pp_decl = function
    | New_var (typ, id) -> sprintf "%s %s;" (pp_typ typ) (Symbol.name id)
    | Init (typ, id, e) -> sprintf "%s %s = %s;" (pp_typ typ) (Symbol.name id) (pp_mexp e)
  ;;

  let pp_simple = function
    | Declare d -> pp_decl d
    | Do e -> sprintf "%s;" (pp_mexp e)
    | Assign (id, e) -> sprintf "%s = %s;" (Symbol.name id) (pp_mexp e)
  ;;

  let rec n_str s n =
    if n <= 0 then ""
    else s ^ n_str s (n - 1)
  ;;

  let rec pp_control c d = 
  match c with
    | Return None -> "return;"
    | Return (Some e) -> sprintf "return %s;" (pp_mexp e)
    | If (cond, s1, s2) -> sprintf "(if (%s) %s %s)" (pp_mexp cond) (pp_mstm s1 (d+1))
      (match s2 with None -> "" | Some s2 -> "else " ^ (pp_mstm s2 (d+1)))
    | While (cond, s1) -> sprintf "while (%s) %s" (pp_mexp cond) (pp_mstm s1 (d+1))
    | For (decl, cond, change, body) -> sprintf "for (%s%s;%s) %s" 
      (match decl with None -> "" | Some decl -> pp_simple decl) (pp_mexp cond) 
      (match change with None -> "" | Some x -> pp_simple x) (pp_mstm body (d+1))
    | Assert e -> sprintf "assert(%s);" (pp_mexp e)

  and pp_stm stm d = 
  let tabs = n_str "  " d in
  tabs ^
  match stm with
    | Block s -> sprintf "{\n%s%s}" (pp_stms s (d+1)) tabs
    | Control c -> pp_control c d
    | Simple s -> pp_simple s
    
  and pp_mstm stm d = pp_stm (Mark.data stm) d
  and pp_stms stms d = String.concat (List.map ~f:(fun stm -> pp_mstm stm d ^ "\n") stms)

  and pp_gdecl decl =
    let pp_param (typ, name) = (pp_typ typ) ^ " " ^ (Symbol.name name) in
    let pp_fn_header (typ, name, params) =
      pp_param (typ, name) ^ "(" ^ pp_list pp_param params ^ ")" in
    match decl with
    | Fdecl (typ, name, params) -> pp_fn_header (typ, name, params) ^ ";"
    | Fdefn (typ, name, params, body) -> pp_fn_header (typ, name, params) ^ " {\n" ^ pp_stms body 1 ^ "}"
    | Typedef (typ, name) -> "typedef " ^ pp_param (typ, name) ^ ";";
  ;;

  let pp_program decls = List.fold decls ~init:""
    ~f:(fun s -> fun decl -> s ^ "\n\n" ^ (pp_gdecl decl))
  (* let pp_program (fn, stms) = "int " ^ Symbol.name fn ^ " {\n" ^ pp_stms stms 1^ "}" *)
end
