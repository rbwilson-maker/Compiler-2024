(*
 * This represents x86 assembly code.
 *)

open Core

type size = Reg.size

(* Size information is stored in the operator,
 * because the operator may impose sizing restrictions *)
type reg = Reg.t

         (* 8/16/32b       32b   32b          (1)/2/4/8 *)
         (* disp          (base, index,       scale)    *)
type addr = int32 option * reg * reg option * int option

and operand =
  | Reg of reg
  | Mem of addr
  | Imm of int32

and cc =
  | Less    (* l  *)
  | Leq     (* le *)
  | Greater (* g  *)
  | Geq     (* ge *)
  | Equal   (* e  *)
  | Neq     (* ne *)

type line =
  | Label of string
  | Directive of string
  | Nop
  | Ret
  | Leave
  | Cltd (* %eax dword -> %edx:%eax *)
  | Jmp of string
  | Jcc of cc * string
  | Pop of reg (* Always quad. *)
  | Push of operand (* Always quad. *)
  | Mov of size * operand * operand
     (* 8 -> 64    16 -> 64
      * 8 -> 32    16 -> 32
      * 8 -> 16     r/m      *)
  | Movzx of size * operand * size * reg
  (* ===== Arithmetic ===== *)
               (* r/m/i,    reg/mem *)
  | Add of size * operand * operand
  | Sub of size * operand * operand
                (* imm,           reg/mem   reg *)
  | Imul of size * int32 option * operand * reg
                (* reg/mem *)
  | Idiv of size * operand
  (* ===== Comparison ===== *)
               (* r/m8 *)
  | Setcc of cc * operand
               (* r/m/i,    r/m*)
  | Cmp of size * operand * operand
  (* ===== Bitwise ===== *)
               (* r/m/i,    r/m *)
  | And of size * operand * operand
  | Or  of size * operand * operand
  | Xor of size * operand * operand
               (* r/m,      rcx/imm *)
  | Sal of size * operand * operand
  | Sar of size * operand * operand (* Signed *)
  | Shr of size * operand * operand (* Unsigned *)

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

let format_cc = function
  | Less    -> "l"
  | Leq     -> "le"
  | Greater -> "g"
  | Geq     -> "ge"
  | Equal   -> "e"
  | Neq     -> "ne"

let format : line -> string = function
  | Label label -> sprintf "%s:" label
  | Directive s -> sprintf "\t.%s" s
  | Nop -> "\tnop"
  | Ret -> "\tret"
  | Leave -> "\tleave"
  | Cltd -> "\tcltd"
  | Jmp label -> sprintf "\tjmp\t%s" label
  | Jcc (cc, label) -> sprintf "\tj%s\t%s" (format_cc cc) label
  | Pop reg -> sprintf "\tpopq\t%s" (format_reg reg Quad)
  | Push op -> sprintf "\tpushq\t%s" (format_op Quad op)
  | Mov (s, src, dest) -> 
    sprintf "\tmov%s\t%s, %s" (format_size s) (format_op s src) (format_op s dest)
  | Movzx (s1, src, s2, reg) ->
    sprintf "\tmovz%s%s\t%s, %s" (format_size s1) (format_size s2) (format_op s1 src) (format_reg reg s2)
    (* ===== Arithmetic ===== *)
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
  (* ===== Comparison ===== *)
  | Setcc (cc, op) -> sprintf "\tset%s\t%s" (format_cc cc) (format_op Byte op)
  | Cmp (s, op1, op2) -> sprintf "\tcmp%s\t%s, %s" (format_size s) (format_op s op1) (format_op s op2)
  (* ===== Bitwise ===== *)
  | And (s, op1, op2) -> sprintf "\tand\t%s, %s" (format_op s op1) (format_op s op2)
  | Or (s, op1, op2) -> sprintf "\tor\t%s, %s" (format_op s op1) (format_op s op2)
  | Xor (s, op1, op2) -> sprintf "\txor\t%s, %s" (format_op s op1) (format_op s op2)
  | Sal (s, op1, op2) -> sprintf "\tsal%s\t%s, %s" (format_size s) (format_op Byte op1) (format_op s op2)
  | Sar (s, op1, op2) -> sprintf "\tsar%s\t%s, %s" (format_size s) (format_op Byte op1) (format_op s op2)
  | Shr (s, op1, op2) -> sprintf "\tshr%s\t%s, %s" (format_size s) (format_op Byte op1) (format_op s op2)