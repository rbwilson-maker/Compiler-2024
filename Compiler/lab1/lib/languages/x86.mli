(*
 * This represents x86 assembly code.
 *)

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

val format : line -> string