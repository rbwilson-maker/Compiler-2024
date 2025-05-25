(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * assem -> live_assem
 * This file performs liveness analysis on an abstract assembly program.
 * It is implemented using lots of imperative programming and iterating over
 * a computed hashmap.
 *)

(** Computes a hashmap mapping lines to liveness information for
  a particular function *)
val compute_live_outs : Assem.fun_instrs -> Live_assem.fun_lines

(** Computes liveness information for all functions in a program *)
val compute_liveness : Assem.program -> Live_assem.program
