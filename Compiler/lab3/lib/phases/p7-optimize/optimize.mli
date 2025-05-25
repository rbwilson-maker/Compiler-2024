(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang 
 * 
 * This is a meta phase which runs all optimizations
 * done on the 3-address abstract assembly.
 * Currently this only includes liveness and regalloc.
 *)

(** Generates liveness information on an assem program. Takes a debug flag which
    prints the liveness information if present. *)
val run_liveness : bool -> Assem.program -> Live_assem.program

(** Allocates registers on an assem program using a liveness analysis. It assumes
    that the input liveness information reflects the input program. 
    Takes a debug flag which prints the liveness information if present. *)
val run_regalloc : bool -> Assem.program -> Live_assem.program -> Assem.program

(** Runs all optimizations on an assem program. *)
val run_all : bool -> bool -> Assem.program -> Assem.program
