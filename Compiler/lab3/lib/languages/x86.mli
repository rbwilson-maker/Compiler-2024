(* Rachompicole L3 Compiler
 * x86-64 Assembly
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * This is the lowest level of the compiler,
 * representing real assembly instructions, their 
 * sizes, and their arguments.
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
  (* rsp <- rbp; pop rbp *)
  | Leave
  (* %eax dword -> %edx:%eax *)
  | Cltd 
  | Jmp of string
  | Jcc of cc * string
  (* Pop from the stack into reg. Always quad. *)
  | Pop of reg
  (* Push from operand onto the stack. Always quad *)
  | Push of operand
  (* Mov size source dest *)
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
               (* rcx/imm,  r/m *)
  | Sal of size * operand * operand
  | Sar of size * operand * operand (* Signed *)
  | Shr of size * operand * operand (* Unsigned *)
  (* ==== Functions ==== *)
  | Call of size * string

type fun_lines = line list
type program = fun_lines list
  
module Print : sig
  val pp_line : line -> string
  val pp_program : program -> string
end