(* Rachompicole L2 Compiler
 * IR Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * The IR Tree now turns everything into branches and jumps thus
 * removing a distinction between loops and conditionals.
 * 
 * We unfortunately still have ternary expressions, so this is
 * not quite in basic block form yet for program analysis.
 *)
type label = Symbol.t

type binop =
  | Add | Sub | Mul
  | Div | Mod 
  | Less | Greater | Equal | Leq | Geq | Neq
  | Bit_and | Bit_or | Bit_xor
  | Lshift | Rshift

(* Pure expressions (except div/mod/lshfit/rshift)*)
type exp =
  | Const of Int32.t
  | True | False
  | Temp of Temp.t
  | Binop of
      { lhs : exp
      ; op : binop
      ; rhs : exp
      }
  | Ternary of
      { if_exp : exp
      ; then_exp : exp
      ; else_exp : exp
      }

(* Effectful commands *)
and stm =
  | Move of
      { dest : Temp.t
      ; src : exp
      }
  | Branch of 
      { condition : exp
      ; if_label : label
      ; else_label : label option
      ; after_label : label
      }
  | Label of label
  | Goto of label
  | Return of exp

type program = stm list

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
