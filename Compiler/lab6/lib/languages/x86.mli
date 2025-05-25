(* Rachompicole L3 Compiler
 * x86-64 Assembly
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * This is the lowest level of the compiler,
 * representing real assembly instructions, their 
 * sizes, and their arguments.
 *)

(* Size information is stored in the operator,
* because the operator may impose sizing restrictions *)

        (* 8/16/32b       32b   32b          (1)/2/4/8 *)
        (* disp          (base, index,       scale)    *)
type addr = int32 option * Reg.t * Reg.t option * int option
and operand =
  | Reg of Reg.t
  | Mem of addr
  | Imm of [`Long of int32 | `Quad of int64]
[@@deriving equal]

(* label(%rip) *)
type rel_addr = Symbol.t
(* label or *%reg *)
type fn_addr = Label of Symbol.t | Value of Reg.t

and cc =
  | Less    (* l  *)
  | Leq     (* le *)
  | Greater (* g  *)
  | Geq     (* ge *)
  | Equal   (* e  *)
  | Neq     (* ne *)

type line =
  | Label of Symbol.t
  | Directive of string
  | Nop
  | Ret
  | Leave
  | Cltd (* %eax dword -> %edx:%eax *)
  | Jmp of fn_addr
  | Jcc of cc * fn_addr
  | Pop of Reg.t (* Always quad. *)
  | Push of operand (* Always quad. *)
  (* Move size addr(source) dest *)
  | Lea of Size.t * [`A of operand | `R of rel_addr] * operand
  (* Move size source dest *)
  | Mov of Size.t * operand * operand
    (* 8 -> 64    16 -> 64
      * 8 -> 32    16 -> 32
      * 8 -> 16     r/m      *)
  | Movzx of Size.t * operand * Size.t * Reg.t
  (* ===== Arithmetic ===== *)
              (* r/m/i,    reg/mem *)
  | Add of Size.t * operand * operand
  | Sub of Size.t * operand * operand
                (* imm,           reg/mem   reg *)
  | Imul of Size.t * int32 option * operand * Reg.t
                (* reg/mem *)
  | Idiv of Size.t * operand
  (* ===== Comparison ===== *)
              (* r/m8 *)
  | Setcc of cc * operand
              (* r/m/i,    r/m*)
  | Cmp of Size.t * operand * operand
  (* ===== Bitwise ===== *)
              (* r/m/i,    r/m *)
  | And of Size.t * operand * operand
  | Or  of Size.t * operand * operand
  | Xor of Size.t * operand * operand
              (* r/m,      rcx/imm *)
  | Sal of Size.t * operand * operand
  | Sar of Size.t * operand * operand (* Signed *)
  | Shr of Size.t * operand * operand (* Unsigned *)
  (* ==== Functions ==== *)
  | Call of Size.t * fn_addr

type fun_lines = line list
type program = fun_lines list
  
module Print : sig
  val pp_line : line -> string
  val pp_program : program -> string
end