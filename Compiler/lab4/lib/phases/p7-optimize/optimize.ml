(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang 
 * 
 * This is a meta phase which runs all optimizations
 * done on the 3-address abstract assembly.
 * Currently this only includes liveness and regalloc.
 *)
open Core
module AS = Assem
module Live = Live_assem

(** Generates liveness information on an assem program. Takes a debug flag which
    prints the liveness information if present. *)
let run_liveness (dbg : bool) (p : AS.program) : Live.program = 
  let liveness = Liveness.compute_liveness p in
  if dbg then
    List.iter liveness ~f:(fun f -> 
      prerr_endline (Live.Print.pp_fun_lines f)
    ); 
  liveness
;;

let _run_deadcode (_dbg : bool) (p : AS.program) (liveness : Live.program) = 
  Deadcode.assem_remove (liveness, p)
;;


(** Allocates registers on an assem program using a liveness analysis. It assumes
    that the input liveness information reflects the input program. 
    Takes a debug flag which prints the liveness information if present. *)
let run_regalloc (dbg : bool) (p : AS.program) (liveness : Live.program) = 
  Regalloc.regalloc dbg (liveness, p)
;;

(** Runs all optimizations on an assem program. *)
let run_all (_verbose as v : bool) (_debug as dbg : bool) (program : AS.program) : AS.program = 
  let say_if b msg = if b then prerr_endline msg in

  say_if v ("\tAnalyzing liveness...");
  let liveness = run_liveness dbg program in 

  (* say_if v ("\tRemoving dead code...");
  let program = run_deadcode dbg program liveness in *)

  say_if v ("\tRegister allocating...");
  let program = run_regalloc dbg program liveness in 

  say_if v ("\tRemoving self moves...");
  Selfmoves.assem_remove program
  
 