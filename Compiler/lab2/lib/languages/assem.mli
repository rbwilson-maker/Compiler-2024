(* Rachompicole L2 Compiler
 * Assembly language
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Pseudo assembly language with 3-operand
 * instructions, arbitrarily many temps, 
 * branches and jumps.
 * 
 * Branches are now limited to either single operands
 * or comparison operators like equality which need
 * to be kept around for deciding what type of jump is.
 *)

type label = Symbol.t
type operand =
  | Imm of Int32.t
  | Reg of Reg.t
  | Temp of Temp.t
  (* booleans are now ints *)

type compop = Less | Greater | Equal | Leq | Geq | Neq

type comparison  = 
{ lhs : operand
; op : compop
; rhs : operand
}

type operation =
  | Add | Sub | Mul | Div | Mod
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift
  | Comp of compop

type instr =
  | Return
  (* dest <- lhs op rhs *)
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  (* dest <- src *)
  | Mov of
      { dest : operand
      ; src : operand
      }
  | Branch of 
      { if_label : label
      ; else_label : label option
      ; after_label : label
      (* The condition is either a single boolean operand
         or a comparison expression *)
      ; cond : [`Single of operand | `Comparison of comparison]
      }
  | Jump of label
  | Label of label
  (* Assembly directive. *)
  | Directive of string
  (* Human-friendly comment. *)
  | Comment of string

val format : instr -> string
