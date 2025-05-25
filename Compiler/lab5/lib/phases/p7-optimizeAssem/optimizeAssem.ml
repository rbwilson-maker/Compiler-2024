(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang 
 * 
 * This is a meta phase which runs all optimizations
 * done on the 3-address abstract assembly.
 * Currently this only includes liveness and regalloc.
 *)
open Core
module AS = Assem

let run_cfg (dbg : bool) (p : AS.program) : Cfg.cfgs = 
  let cfgs = Cfg.make_cfgs p in
  if dbg then prerr_endline (Cfg.pp_cfgs cfgs);
  cfgs
;;

(** Generates liveness information on an assem program. Takes a debug flag which
    prints the liveness information if present. *)
let run_liveness (dbg : bool) (p : Cfg.cfgs) : Liveness.program * bool = 
  let liveness, b = Liveness.compute_liveness p in
  if dbg then
    List.iter liveness ~f:(fun f -> 
      prerr_endline (Liveness.pp_fun_lines f)
    ); 
  liveness, b
;;

(** Allocates registers on an assem program using a liveness analysis. It assumes
    that the input liveness information reflects the input program. 
    Takes a debug flag which prints the liveness information if present. *)
let run_regalloc (dbg : bool) (cfg : Cfg.cfgs) (liveness : Liveness.program) = 
  Regalloc.regalloc dbg (cfg, liveness)
;;

let run_tailcall (_dbg : bool) (p : Cfg.cfgs) : Cfg.cfgs= 
  Tailcall.optimize_no_spills p

(** Runs all optimizations on an assem program. *)
let run_all (_verbose as v : bool) (_debug as dbg : bool) (program : AS.program) : AS.program = 
  let say_if b msg = if b then prerr_endline msg in

  say_if v ("\tComputing Control Flow...");
  let cfgs = run_cfg dbg program in

  say_if v ("\tTailcall optimizing...");
  let cfgs = run_tailcall dbg cfgs in

  say_if v ("\tAnalyzing liveness...");
  let liveness, do_regalloc = run_liveness dbg cfgs in 

  say_if v ("\tRegister allocating...");
  let program = 
    if do_regalloc
    then run_regalloc dbg cfgs liveness  
    else ((* get program from tail call cfg *)
      List.map cfgs ~f:(fun f_cfg -> 
        let out : AS.fun_instrs =
        { name = f_cfg.name
        ; body = List.map f_cfg.body ~f:snd
        ; spills = f_cfg.spills
        ; regs_used = [] }
        in out
        )
      )
  in

  say_if v ("\tRemoving self moves...");
  Selfmoves.remove_assem program
  
 