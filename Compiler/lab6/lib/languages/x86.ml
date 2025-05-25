(* Rachompicole L3 Compiler
 * x86-64 Assembly
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * This is the lowest level of the compiler,
 * representing real assembly instructions, their 
 * sizes, and their arguments.
 *)

open Core

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

module Print = struct
  open Print
  let pp_size : Size.t -> string = Size.format

  let pp_reg = Reg.format

  let pp_fn_addr = function
    | Value r -> "*" ^ pp_reg r Quad
    | Label l -> Symbol.name l

  let rec pp_op s = function
    | Reg r -> pp_reg r s
    | Imm i -> sprintf "$%s" 
     (match i with
      | `Long i -> (Int32.to_string i)
      | `Quad i -> (Int64.to_string i)
    )
    | Mem addr -> pp_addr addr
  and pp_addr : addr -> string = fun (d, b, i, s') -> 
      (match d with
        | None -> ""
        | Some d' -> sprintf "%d" (Int32.to_int_exn d'))
      ^ sprintf "(%s" (pp_reg b Size.Quad)
      ^ (match i with
        | None -> ""
        | Some r -> "," ^ pp_reg r Size.Quad )
      ^ (match s' with
        | None -> ""
        | Some i -> "," ^ Int.to_string i)
      ^ ")"

  let pp_cc = function
    | Less    -> "l"
    | Leq     -> "le"
    | Greater -> "g"
    | Geq     -> "ge"
    | Equal   -> "e"
    | Neq     -> "ne"

  let pp_line : line -> string = function
    | Label label -> sprintf "%s:" (Symbol.name label)
    | Directive s -> sprintf "\t.%s" s
    | Nop -> "\tnop"
    | Ret -> "\tret"
    | Leave -> "\tleave"
    | Cltd -> "\tcltd"
    | Jmp label -> sprintf "\tjmp\t%s" (pp_fn_addr label)
    | Jcc (cc, label) -> sprintf "\tj%s\t%s" (pp_cc cc) (pp_fn_addr label)
    | Pop reg -> sprintf "\tpopq\t%s" (pp_reg reg Quad)
    | Push op -> sprintf "\tpushq\t%s" (pp_op Quad op)
    | Lea (s, src, dest) -> 
      sprintf "\tlea%s\t%s, %s" (pp_size s) 
        (match src with 
        | `A a -> pp_op Quad a 
        | `R r -> Symbol.name r ^ "(%rip)"
        ) (pp_op s dest)
    | Mov (s, src, dest) -> 
      sprintf "\tmov%s\t%s, %s" (pp_size s) (pp_op s src) (pp_op s dest)
    | Movzx (s1, src, s2, reg) ->
      sprintf "\tmovz%s%s\t%s, %s" (pp_size s1) (pp_size s2) (pp_op s1 src) (pp_reg reg s2)
      (* ===== Arithmetic ===== *)
    | Add (s, src, dest) -> 
      sprintf "\tadd%s\t%s, %s" (pp_size s) (pp_op s src) (pp_op s dest)
    | Sub (s, src, dest) -> 
      sprintf "\tsub%s\t%s, %s" (pp_size s) (pp_op s src) (pp_op s dest)
    | Imul (s, i, op, reg) -> 
      sprintf "\timul%s\t" (pp_size s)
      ^ (match i with None -> "" | Some i -> sprintf "$%d, " (Int32.to_int_exn i))
      ^ (pp_op s op) ^ ", "
      ^ (pp_reg reg s)
    | Idiv (s, op) -> sprintf "\tidiv%s\t%s" (pp_size s) (pp_op s op)
    (* ===== Comparison ===== *)
    | Setcc (cc, op) -> sprintf "\tset%s\t%s" (pp_cc cc) (pp_op Byte op)
    | Cmp (s, op1, op2) -> sprintf "\tcmp%s\t%s, %s" (pp_size s) (pp_op s op1) (pp_op s op2)
    (* ===== Bitwise ===== *)
    | And (s, op1, op2) -> sprintf "\tand\t%s, %s" (pp_op s op1) (pp_op s op2)
    | Or (s, op1, op2) -> sprintf "\tor\t%s, %s" (pp_op s op1) (pp_op s op2)
    | Xor (s, op1, op2) -> sprintf "\txor\t%s, %s" (pp_op s op1) (pp_op s op2)
    | Sal (s, op1, op2) -> sprintf "\tsal%s\t%s, %s" (pp_size s) (pp_op Byte op1) (pp_op s op2)
    | Sar (s, op1, op2) -> sprintf "\tsar%s\t%s, %s" (pp_size s) (pp_op Byte op1) (pp_op s op2)
    | Shr (s, op1, op2) -> sprintf "\tshr%s\t%s, %s" (pp_size s) (pp_op Byte op1) (pp_op s op2)
    (* ==== Functions ==== *)
    | Call (s, fn) -> sprintf "\tcall%s\t%s" (pp_size s) (pp_fn_addr fn)
    
  let pp_program (p : program) =
    pp_list ~btwn:"\n\n" (pp_list ~btwn:"\n" pp_line) p
end
