(* Rachompicole L3 Compiler
 * Elaborated Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Similar to the AST but with for-loops and unary ops removed
 * and a more intuitive structure than the AST.
 *)

type param = Symbol.t

(* Operator *)
type binop =
  | Plus | Minus | Times | Divided_by | Modulo
  | Less | Greater | Equal | Leq | Geq | Neq 
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 

type signal = Sigfpe | Sigabrt

type exp =
| Var of Symbol.t
| Const of Int32.t
| True | False
| Binop of
    { op : binop
    ; lhs : exp
    ; rhs : exp
    }
| Ternary of
    { if_exp : exp
    ; then_exp : exp
    ; else_exp : exp
    }
| F of Symbol.t * exp list
| Void_f of Symbol.t * exp list
| Raise of signal

(* Statement *)
type stm =
  (* { s1; s2; ... } *)
  | Block of stms
  | Declare of Symbol.t * exp option
  (* x = e; *)
  | Assign of Symbol.t * exp
  (* e; *)
  | Do of exp
  (* if (condition) {...} else {...} *)
  | If of exp * stm * stm option
  (* while (condition) {...} *)
  | While of exp * stm
  | Return of exp option

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