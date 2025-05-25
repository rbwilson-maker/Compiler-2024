(* Rachompicole L3 Compiler
 * Abstract Assembly -> x86-64
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * Uses a reversed accumulator to generate assembly
 * instructions. Assembly instructions take a size parameter
 * which determines which registers are used. 
 *)

(** Converts an abstract assembly program to x86-64 assembly. It assumes the 
    abstract assembly is elaborated and has all guarantees listed in elaborate.mli *)
val convert : Assem.program -> X86.program