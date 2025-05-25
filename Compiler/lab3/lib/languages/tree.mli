(* Rachompicole L3 Compiler
 * IR Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * The IR Tree turns everything into branches and jumps thus
 * removing a distinction between loops and conditionals.
 * 
 * Still modally separates pure expressions and commands.
 * Still includes ternary expressions, so this is
 * not quite in basic block form yet for program analysis.
 *)
type label = Symbol.t

type binop =
  | Add | Sub | Mul 
  | Less | Greater | Equal | Leq | Geq | Neq
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift

type effect_op = Div | Mod

type signal = Sigfpe | Sigabrt

type exp =
  | Const of Int32.t
  | True | False
  | Temp of Temp.t
  | Binop of
      { lhs : exp
      ; op : binop
      ; rhs : exp
      }

(* Effectful commands *)
and stm =
  | Move of
    { dest : Temp.t
    ; src : exp
    }
  | Effect_move of 
    { dest : Temp.t
    ; lhs : exp
    ; op : effect_op
    ; rhs : exp 
    }
  | Fn_move of
    { dest : Temp.t
    ; fn : label
    ; args : exp list
    }
  | Do_fn of
    { fn : label
    ; args : exp list
    }
  | Branch of 
    { condition : exp
    ; if_label : label
    ; else_label : label option
    ; after_label : label
    }
  | Label of label
  | Goto of label
  | Return of exp option
  | Raise of signal

type param = Temp.t
type fun_stms = label * param list * stm list
type program = fun_stms list

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
