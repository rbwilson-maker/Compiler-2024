(* Library for representing registers. Each register
 * is represented by its 64-bit form, but can be 
 * easily resized and formatted. For use across phases of the compiler
 * 
 * Author: Rachel Wilson
 *)
open Core

type t = RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
         | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
[@@deriving compare, equal, hash, sexp]



(** [RDI; RSI; RDX; RCX; R8; R9] Input registers to functions *)
val fn_args : t list

(** callee saved registers *)
val callee_saves : t Hash_set.t

(** caller saved registers *)
val caller_saves : t list

(** An array of the registers which may be allocated to 
 Arrays allow for easy association of ints with registers for coloring. *)
val available_regs : t Array.t

(** Precoloring of registers matching the indices of available_regs *)
val precolor : (t, int) Hashtbl.t

(** Formats register names according to the required size, 
   Uses [%rXb] not [%rXl] for byte size of registers [R8-R15] *)
val format : t -> Size.t -> string