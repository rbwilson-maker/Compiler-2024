(*
 * This represents x86 assembly code.
 *)

open Core

(* Size information is stored in the operator,
 * because the operator may impose sizing restrictions *)
type size = Reg.size
type reg = Reg.t


         (* 8/16/32b       32b   32b          (1)/2/4/8 *)
         (* disp          (base, index,       scale)    *)
type addr = int32 option * reg * reg option * int option

and operand =
  | Reg of reg
  | Mem of addr
  | Imm of int32

type line =
  | Label of string
  | Directive of string
  | Nop
  | Ret
  | Leave
  | Cltd (* %eax dword -> %edx:%eax *)
  | Pop of reg (* Always quad. *)
  | Push of operand (* Always quad. *)
  | Mov of size * operand * operand
               (* r/m/i,    reg/mem *)
  | Add of size * operand * operand
  | Sub of size * operand * operand
                (* imm,           reg/mem   reg *)
  | Imul of size * int32 option * operand * reg
                (* reg/mem *)
  | Idiv of size * operand

  let format_size : size -> string = function
  | Byte -> "b"
  | Word -> "w"
  | Long -> "l"
  | Quad -> "q"
;;

let format_reg = Reg.format

let format_op s = function
  | Reg r -> format_reg r s
  | Imm i -> sprintf "$%d" (Int32.to_int_exn i)
  | Mem (d, b, i, s') -> 
    (match d with
     | None -> ""
     | Some d' -> sprintf "%d" (Int32.to_int_exn d'))
    ^ sprintf "(%s" (format_reg b Reg.Quad)
    ^ (match i with
     | None -> ""
     | Some r -> "," ^ format_reg r Reg.Quad )
    ^ (match s' with
     | None -> ""
     | Some i -> Int.to_string i)
    ^ ")"

let format : line -> string = function
  | Label label -> sprintf "%s:" label
  | Directive s -> s
  | Nop -> "\tnop"
  | Ret -> "\tret"
  | Leave -> "\tleave"
  | Cltd -> "\tcltd"
  | Pop reg -> sprintf "\tpopq\t%s" (format_reg reg Quad)
  | Push op -> sprintf "\tpushq\t%s" (format_op Quad op)
  | Mov (s, src, dest) -> 
    sprintf "\tmov%s\t%s, %s" (format_size s) (format_op s src) (format_op s dest)
  | Add (s, src, dest) -> 
    sprintf "\tadd%s\t%s, %s" (format_size s) (format_op s src) (format_op s dest)
  | Sub (s, src, dest) -> 
    sprintf "\tsub%s\t%s, %s" (format_size s) (format_op s src) (format_op s dest)
  | Imul (s, i, op, reg) -> 
    sprintf "\timul%s\t" (format_size s)
    ^ (match i with None -> "" | Some i -> sprintf "%d, " (Int32.to_int_exn i))
    ^ (format_op s op) ^ ", "
    ^ (format_reg reg s)
  | Idiv (s, op) -> sprintf "\tidiv%s\t%s" (format_size s) (format_op s op)