(* Rachompicole L3 Compiler
 * Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * The AST resembles the parser as close as possible to maintain
 * the original program. Typechecking is done on this interface.
 *)
open Core

module type ExpType =
sig
  type 'a t
  val data : 'a t -> 'a
end

module Make (ET: ExpType) :
sig
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

  type lval = 
    | LVar of Symbol.t
    | LGet_field of mlval * Symbol.t
    | LDeref_field of mlval * Symbol.t
    | LDeref of mlval
    | LIndex of
        { array : mlval
        ; index : mexp
        }

  and mlval = lval ET.t

  type declaration =
    (* typ x; *)
    | New_var of typ * Symbol.t
    (* typ x = e; *)
    | Init of typ * Symbol.t * mexp

  type simple =
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

  type param = typ * Symbol.t
  type field = Symbol.t * typ

  type gdecl =
    | Fdecl   of ret_typ * Symbol.t * param list
    | Fdefn   of ret_typ * Symbol.t * param list * stms
    | Typedef of typ * Symbol.t
    | Sdecl   of Symbol.t
    | Sdefn   of Symbol.t * field list

  type program = gdecl list

  (* Print as source, with redundant parentheses *)
  module Print : sig
    val pp_typ : typ -> string
    val pp_exp : exp -> string
    val pp_lval : lval -> string
    val pp_stm : stm -> int -> string
    val pp_program : program -> string
  end
end