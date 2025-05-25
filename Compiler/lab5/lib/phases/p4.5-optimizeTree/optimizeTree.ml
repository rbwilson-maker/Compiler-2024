(* L5 Rachompicole Compiler *)
open Core

let run_all (_verbose as v : bool) (_debug as d : bool) (program : Tree.program) : Tree.program =
  let say_if b msg = if b then prerr_endline msg in

  say_if v ("\tCoalescing constant expressions...");
  let coalesced = Const.coalesce_constants d program in

  coalesced
