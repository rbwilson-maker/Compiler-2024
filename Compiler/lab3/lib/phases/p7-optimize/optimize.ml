(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang 
 * 
 * This is a meta phase which runs all optimizations
 * done on the 3-address abstract assembly.
 * Currently this only includes liveness and regalloc.
 *)
open Core
module AS = Assem

(** Generates liveness information on an assem program. Takes a debug flag which
    prints the liveness information if present. *)
let run_liveness (dbg : bool) (p : AS.program) : Live_assem.program = 
  let liveness = Liveness.compute_liveness p in
  if dbg then
    List.iter liveness ~f:(fun f -> 
      prerr_endline (Live_assem.Print.pp_fun_lines f)
    ); 
  liveness

(** Allocates registers on an assem program using a liveness analysis. It assumes
    that the input liveness information reflects the input program. 
    Takes a debug flag which prints the liveness information if present. *)
let run_regalloc (dbg : bool) (p : AS.program) (liveness : Live_assem.program) = 
  Regalloc.regalloc dbg (liveness, p)

(** Runs all optimizations on an assem program. *)
let run_all (_verbose as v : bool) (_debug as dbg : bool) (p : AS.program) : AS.program = 
  if v then prerr_endline ("\tAnalyzing liveness...");
  let liveness = run_liveness dbg p in 
  if v then prerr_endline ("\tRegister Allocating...");
  run_regalloc dbg p liveness
  
 