(* Rachompicole L3 Compiler
 * Register Allocator
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Does:
 * 1. Construct Interference Graph
 * 2. Greedy Graph Coloring
 * 3. Assign registers
 * 4. Spill temps (by not assigning a register)
 * 5. Report which registers were allocated
 *
 * Uses an abstract program and its liveness analysis to reconstruct the program
 * with temps replaced by registers. Spilled temps are left as temps.
 *)

(** Do register allocation on an abstract assembly program using
    data from its liveness analysis *)
val regalloc : bool -> Cfg.cfgs * Liveness.program -> Assem.program
