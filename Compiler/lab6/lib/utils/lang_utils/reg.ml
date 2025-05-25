(* Library for representing registers. Each register
 * is represented by its 64-bit form, but can be 
 * easily resized and formatted. For use across phases of the compiler
 * 
 * Author: Rachel Wilson
 *)
open Core

module Register =
struct
 (* we use the 64-bit version to represent the register *)
  type t = RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
            | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  [@@deriving compare, equal, hash, sexp]
end
include Register

let fn_args = [RDI; RSI; RDX; RCX; R8; R9]

let callee_saves = Hash_set.of_list (module Register) [RBX; RSP; RBP; R12; R13; R14; R15]
(* caller_saves excludes R11 since it's reserved *)
let caller_saves = [R10; RDI; RSI; RDX; RCX; R8; R9; RAX]

(* currently excludes RSP, RBP, R11 
 * the order determines allocating priorities.
 * caller saved are prioritized, then fn args, then callee saved *)
let available_regs = Array.of_list [RAX; RDX; RCX; RDI; RSI; R8; R9; R10; RBX; R12; R13; R14; R15;]
let precolor = let h = Hashtbl.create (module Register) in
  Array.iteri available_regs ~f:(fun i r -> Hashtbl.add_exn h ~key:r ~data:i); h

(* This encodes the matrix mapping registers and sizes to their names *)
let format : t -> Size.t -> string = function
| RAX -> (function 
    Byte -> "%al" | Word -> "%ax" | Long -> "%eax" | Quad -> "%rax")
| RBX -> (function
     Byte -> "%bl" | Word -> "%bx" | Long -> "%ebx" | Quad -> "%rbx")
| RCX -> (function
     Byte -> "%cl" | Word -> "%cx" | Long -> "%ecx" | Quad -> "%rcx")
| RDX -> (function
     Byte -> "%dl" | Word -> "%dx" | Long -> "%edx" | Quad -> "%rdx")
| RSI -> (function
     Byte -> "%sil" | Word -> "%si" | Long -> "%esi" | Quad -> "%rsi")
| RDI -> (function
     Byte -> "%dil" | Word -> "%di" | Long -> "%edi" | Quad -> "%rdi")
| RBP -> (function
     Byte -> "%bpl" | Word -> "%bp" | Long -> "%ebp" | Quad -> "%rbp")
| RSP -> (function
     Byte -> "%spl" | Word -> "%sp" | Long -> "%esp" | Quad -> "%rsp")

(* On AMD64 (macs) the byte sized registers are rXl *)
| R8 -> (function
     Byte -> "%r8b" | Word -> "%r8w" | Long -> "%r8d" | Quad -> "%r8")
| R9 -> (function
     Byte -> "%r9b" | Word -> "%r9w" | Long -> "%r9d" | Quad -> "%r9")
| R10 -> (function
     Byte -> "%r10b" | Word -> "%r10w" | Long -> "%r10d" | Quad -> "%r10")
| R11 -> (function
     Byte -> "%r11b" | Word -> "%r11w" | Long -> "%r11d" | Quad -> "%r11")
| R12 -> (function
     Byte -> "%r12b" | Word -> "%r12w" | Long -> "%r12d" | Quad -> "%r12")
| R13 -> (function
     Byte -> "%r13b" | Word -> "%r13w" | Long -> "%r13d" | Quad -> "%r13")
| R14 -> (function
     Byte -> "%r14b" | Word -> "%r14w" | Long -> "%r14d" | Quad -> "%r14")
| R15 -> (function
     Byte -> "%r15b" | Word -> "%r15w" | Long -> "%r15d" | Quad -> "%r15")

include Comparable.Make (Register)