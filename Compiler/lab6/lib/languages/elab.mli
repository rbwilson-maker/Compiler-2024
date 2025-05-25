(* Rachompicole L3 Compiler
 * Elaborated Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Similar to the AST but with for-loops and unary ops removed
 * and a more intuitive structure than the Asts.Marked.
 *)
open Core

type field = Symbol.t * Symbol.t
type big_size = Size.data_size
type small_size = Size.t
type param = Symbol.t * small_size

(* Long operators *)
type binop =
  | Plus | Minus | Times | Divided_by | Modulo
  | Less | Greater | Equal | Leq | Geq | Neq 
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 

type asnop = 
  | Plus_eq | Minus_eq | Times_eq | Div_eq | Mod_eq
  | Bit_and_eq | Bit_or_eq | Bit_xor_eq | Lshift_eq | Rshift_eq

type signal = Sigfpe | Sigabrt

type exp =
  | Var of Symbol.t * Size.t
  | Fn_name of Symbol.t
  | Const of Int32.t (* implicit size Long *)
  | True | False (* implicit size Long (for now) *)
  | Binop of
      { op : binop
      ; lhs : exp
      ; rhs : exp
      ; op_size : small_size
      }
  | Ternary of
      { if_exp : exp
      ; then_exp : exp
      ; else_exp : exp
      ; size : small_size
      }
  | F of 
      { name : [ `Fn of Symbol.t * bool | `Computed of exp ]
      ; params : (exp * Size.t) list
      ; ret_size : small_size option (* None for void fns *)
      }
  | Raise of signal
  | Mem of (mem_exp * Size.t)
  | Null
  | Alloc of big_size
  | Alloc_array of big_size * exp

and mem_exp = 
  | Get_field of 
      { strct : mem_exp (* structs only exist in memory *)
      ; field : field
      }
  | Deref of exp
  | Index of
      { array : exp
      ; index : exp
      ; elem_size : big_size
      }

type lval =
  | Var of Symbol.t
  | Field of 
    { strct : lval
    ; field : field
    }
  | Deref of lval
  | Index of 
    { array : lval
    ; index : exp
    ; elem_size : big_size
    }

type dest = lval * small_size

(* Statement *)
type stm =
  (* { s1; s2; ... } *)
  | Block of stms
  | Declare of 
      { lhs : Symbol.t * small_size
      ; rhs : exp option
      }
  (* x = e; *)
  | Assign of dest * exp
  | Asnop of 
      { lhs : dest
      ; op : asnop
      ; rhs : exp
      }
  | Assert of exp
  (* e; *)
  | Do of exp
  (* if (condition) {...} else {...} *)
  | If of exp * stm * stm option
  (* while (condition) {...} *)
  | While of exp * stm
  | Return of (exp * small_size) option

(* Block of statements *)
and stms = stm list

type fndef = Symbol.t * param list * stms

type program = fndef list

(* Print as source, with redundant parentheses *)
module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> int -> string
  val pp_program : program -> string
end