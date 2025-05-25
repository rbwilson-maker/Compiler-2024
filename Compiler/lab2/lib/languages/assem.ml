(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *)

open Core

type label = Symbol.t
type operand =
  | Imm of Int32.t
  | Reg of Reg.t
  | Temp of Temp.t

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
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | Mov of
      { dest : operand
      ; src : operand
      }
  | Branch of 
    { if_label : label
    ; else_label : label option
    ; after_label : label
    ; cond : [`Single of operand | `Comparison of comparison]
    }
  | Jump of label
  | Label of label
  | Directive of string
  | Comment of string


(* functions that format assembly output *)

let format_reg = Reg.format

let format_comp = function
| Less -> "<"
| Greater -> ">"
| Equal -> "=="
| Leq -> "<="
| Geq -> ">="
| Neq -> "!="
;;

let format_binop = function
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| Lshift -> "<<"
| Rshift -> ">>"
| Bit_or -> "|"
| Bit_and -> "&"
| Bit_xor -> "^"
| Comp c -> format_comp c
;;

let format_operand = function
  | Imm n -> "$" ^ Int32.to_string n
  | Temp t -> Temp.name t
  | Reg r -> format_reg r Reg.Quad
;;

let format = function
  | Return -> "return"
  | Binop binop ->
    sprintf
      "%s <-- %s %s %s"
      (format_operand binop.dest)
      (format_operand binop.lhs)
      (format_binop binop.op)
      (format_operand binop.rhs)
  | Mov mv -> sprintf "%s <-- %s" (format_operand mv.dest) (format_operand mv.src)
  | Branch b -> sprintf "if %s then %s else %s."
    (match b.cond with
     | `Single op -> format_operand op
     | `Comparison c -> 
        sprintf "%s %s %s" 
        (format_operand c.lhs)
        (format_comp c.op)
        (format_operand c.rhs))
    (Symbol.name b.if_label)
    (match b.else_label with 
    | Some x -> Symbol.name x 
    | None -> Symbol.name b.after_label)
  | Jump l -> "goto " ^ Symbol.name l
  | Label l -> Symbol.name l
  | Directive dir -> dir
  | Comment comment -> sprintf "/* %s */" comment
;;
