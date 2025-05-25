(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * assem -> live_assem
 * This file performs liveness analysis on an abstract assembly program.
 * It is implemented using lots of imperative programming and iterating over
 * a computed hashmap.
 *)
open Core
type dest = Cfg.Dest.t

type line = 
  { live_in : dest Hash_set.t
  ; live_out : dest Hash_set.t
  }

type fun_lines = 
  { liveness : (Symbol.t, line) Hashtbl.t
  ; body : ((Symbol.t * Symbol.t) * Assem.instr) list
  }
type program = fun_lines list

(** Computes a hashmap mapping lines to liveness information for
  a particular function *)
val compute_live_outs : Cfg.fun_cfg -> fun_lines

(** Computes liveness information for all functions in a program. The bool is whether to regalloc or not *)
val compute_liveness : Cfg.cfgs -> program * bool

val pp_fun_lines : fun_lines -> string
