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

module type ExpType =
sig
  type 'a t
  val data : 'a t -> 'a
end

module Make (ET : ExpType) =
  struct
  type typ = Type.t

  type ret_typ = Type.ret_typ

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

  type asnop = 
    | Plus_eq
    | Minus_eq
    | Times_eq
    | Div_eq
    | Mod_eq
    | Bit_and_eq
    | Bit_or_eq
    | Bit_xor_eq
    | Lshift_eq
    | Rshift_eq

  (* Expression *)
  type exp =
    | Var of Symbol.t
    | Fn_name of Symbol.t
    | Const of Int32.t
    | Lam of ret_typ * param list * stms
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
    | Fn_call of mexp * mexp list
    | Mem of mmem
    | Null
    | Alloc of typ
    | Alloc_array of
        { typ : typ
        ; size : mexp
        }

  and mem_exp = 
    | Get_field of mmem * Symbol.t
    | Deref_field of mexp * Symbol.t
    | Deref of mexp
    | Index of
        { array : mexp
        ; index : mexp
        }

  (* Expression plus auxiliary information *)
  and mexp = exp ET.t
  and mmem = mem_exp ET.t

  and lval = 
    | LVar of Symbol.t
    | LGet_field of mlval * Symbol.t
    | LDeref_field of mlval * Symbol.t
    | LDeref of mlval
    | LIndex of
        { array : mlval
        ; index : mexp
        }

  and mlval = lval ET.t

  and declaration =
    (* typ x; *)
    | New_var of typ * Symbol.t
    (* typ x = e; *)
    | Init of typ * Symbol.t * mexp

  and simple =
    | Declare of declaration
    (* x = e; or *x = e; or x->field = e; guaranteed by parser *)
    | Assign of mlval * mexp
    (* x ?= e; *)
    | Asnop of 
        { lhs : mlval
        ; op : asnop
        ; rhs : mexp
        }
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
  and mstm = stm ET.t

  (* Block of statements *)
  and stms = mstm list

  and param = typ * Symbol.t
  and field = Symbol.t * typ

  type gdecl =
    | Fdecl   of ret_typ * Symbol.t * param list
    | Fdefn   of ret_typ * Symbol.t * param list * stms
    | Typedef of typ * Symbol.t
    | Sdecl   of Symbol.t
    | Sdefn   of Symbol.t * field list

  type program = gdecl list

  module Print = struct
    open Print

    let exp_depth = ref 0;;

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

    let pp_asnop = function
      | Plus_eq -> "+="
      | Minus_eq -> "-="
      | Times_eq -> "*="
      | Div_eq -> "/="
      | Mod_eq -> "%="
      | Bit_and_eq -> "&="
      | Bit_or_eq -> "|="
      | Bit_xor_eq -> "^="
      | Lshift_eq -> "<<="
      | Rshift_eq -> ">>="
    ;;

    let pp_typ = Type.pp_typ
    let pp_ret_typ = Type.pp_ret_typ

    let pp_param (typ, name) = sprintf "%s %s" (pp_typ typ) (Symbol.name name);;

    let rec pp_exp : exp -> int -> string = fun e i -> match e with
      | Var id -> Symbol.name id
      | Fn_name id -> Symbol.name id
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
        sprintf "%s(%s)" (pp_mexp fn) (pp_list pp_mexp args)
      | Mem m -> pp_mmem m
      | Null -> "NULL"
      | Alloc typ -> sprintf "alloc(%s)" (pp_typ typ)
      | Alloc_array {typ; size} -> sprintf "alloc_array(%s, %s)" (pp_typ typ) (pp_mexp size)
      | Lam (ret, params, body) -> sprintf "fn (%s) -> %s {\n%s%s}" (pp_list pp_param params) (pp_ret_typ ret) (pp_stms body (i+1)) (n_str "  " i)

    and pp_mem : mem_exp -> string = function
      | Get_field (e, field) -> sprintf "(%s).%s" (pp_mmem e) (Symbol.name field)
      | Deref_field (e, field) -> sprintf "(%s)->%s" (pp_mexp e) (Symbol.name field)
      | Deref e -> sprintf "*(%s)" (pp_mexp e)
      | Index {array; index; _} -> 
        sprintf "%s[%s]" (pp_mexp array) (pp_mexp index)

    and pp_mexp ?i e = 
      (match i with
      | None -> ()
      | Some i -> exp_depth := i);
      pp_exp (ET.data e) (!exp_depth)
    and pp_mmem m = pp_mem (ET.data m)

    and pp_lval : lval -> string = function
    | LVar id -> Symbol.name id
    | LGet_field (e, field) -> sprintf "(%s).%s" (pp_mlval e) (Symbol.name field)
    | LDeref_field (e, field) -> sprintf "(%s)->%s" (pp_mlval e) (Symbol.name field)
    | LDeref e -> sprintf "*(%s)" (pp_mlval e)
    | LIndex {array; index} -> 
      sprintf "%s[%s]" (pp_mlval array) (pp_mexp index)

    and pp_mlval lv = pp_lval (ET.data lv)

    and pp_decl d i = match d with
      | New_var (typ, id) -> sprintf "%s %s;" (pp_typ typ) (Symbol.name id)
      | Init (typ, id, e) -> sprintf "%s %s = %s;" (pp_typ typ) (Symbol.name id) (pp_mexp ~i e)

    and pp_simple s i = match s with
      | Declare d -> pp_decl d i
      | Do e -> sprintf "%s;" (pp_mexp ~i e)
      | Assign (ml, e) -> sprintf "%s = %s;" (pp_mlval ml) (pp_mexp ~i e)
      | Asnop {lhs; op; rhs} -> sprintf "%s %s %s" (pp_mlval lhs) (pp_asnop op) (pp_mexp ~i rhs)

    and n_str s n =
      if n <= 0 then ""
      else s ^ n_str s (n - 1)

    and pp_control c i = 
    match c with
      | Return None -> "return;"
      | Return (Some e) -> sprintf "return %s;" (pp_mexp ~i e)
      | If (cond, s1, s2) -> sprintf "(if (%s) %s %s)" (pp_mexp ~i cond) (pp_mstm s1 (i+1))
        (match s2 with None -> "" | Some s2 -> "else " ^ (pp_mstm s2 (i+1)))
      | While (cond, s1) -> sprintf "while (%s) %s" (pp_mexp ~i cond) (pp_mstm s1 (i+1))
      | For (decl, cond, change, body) -> sprintf "for (%s%s;%s) %s" 
        (match decl with None -> "" | Some decl -> pp_simple decl i) (pp_mexp ~i cond) 
        (match change with None -> "" | Some x -> pp_simple x i) (pp_mstm body (i+1))
      | Assert e -> sprintf "assert(%s);" (pp_mexp ~i e)

    and pp_stm stm i = 
    let tabs = n_str "  " i in
    tabs ^
    match stm with
      | Block s -> sprintf "{\n%s%s}" (pp_stms s (i+1)) tabs
      | Control c -> pp_control c i
      | Simple s -> pp_simple s i
      
    and pp_mstm stm i = pp_stm (ET.data stm) i
    and pp_stms stms i = String.concat (List.map ~f:(fun stm -> pp_mstm stm i ^ "\n") stms)

    and pp_gdecl decl =
      let pp_field (name, typ) = sprintf "  %s %s;" (pp_typ typ) (Symbol.name name) in
      
      let pp_ret (typ, name) = sprintf "%s %s" (pp_ret_typ typ) (Symbol.name name) in
      let pp_fn_header (typ, name, params) =
        sprintf "%s (%s)" (pp_ret (typ, name)) (pp_list pp_param params) in
      match decl with
      | Fdecl (typ, name, params) -> pp_fn_header (typ, name, params) ^ ";"
      | Fdefn (typ, name, params, body) -> pp_fn_header (typ, name, params) ^ " {\n" ^ pp_stms body 1 ^ "}"
      | Typedef (typ, name) -> "typedef " ^ pp_param (typ, name) ^ ";"
      | Sdecl s -> sprintf "struct %s;" (Symbol.name s)
      | Sdefn (s, fields) -> 
        sprintf "struct %s {\n%s\n}" 
        (Symbol.name s) 
        (pp_list ~btwn:"\n" pp_field fields )
    ;;

    let pp_program decls = List.fold decls ~init:""
      ~f:(fun s -> fun decl -> s ^ "\n\n" ^ (pp_gdecl decl))
  end
end