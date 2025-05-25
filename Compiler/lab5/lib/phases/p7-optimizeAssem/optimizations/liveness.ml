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

let threshold = 200;;
exception DontRegalloc

(* Computes a hashmap mapping line labels to liveness information for
  a particular function *)
let compute_live_outs (cfg : Cfg.fun_cfg) : fun_lines = 
  let liveness = Hashtbl.create (module Symbol) in
  let default () = 
    { live_in = Hash_set.create (module Cfg.Dest)
    ; live_out = Hash_set.create (module Cfg.Dest)
    }
  in
  (* live_in formula *)
  let get_live_in (line : Cfg.line) live_out =
    match line.defs with
    | `None -> Hash_set.union live_out line.uses
    | `One d -> 
      Hash_set.remove live_out d; Hash_set.union live_out line.uses
    | `Many ds -> 
      List.iter ds ~f:(fun d -> Hash_set.remove live_out d);
      Hash_set.union live_out line.uses
  in
  (* repeatedly traverse the labeled program in reverse *)
  let did_update = ref true in
  while !did_update do
    did_update := false;
    List.iter (List.rev cfg.body) ~f:(fun ((block, line), _) -> 
      (* get uses and defs data from the cfg *)
      let line_data = Cfg.get_line cfg (block, line) in
      (* update the liveness of the current line *)
      Hashtbl.update liveness line ~f:(fun line_opt ->
        (* get current liveness data or initialize it *)
        let line_liveness = (
          match line_opt with 
          | None -> default ()
          | Some line_liveness -> line_liveness
        ) in
        (* find successor liveness data*)
        match line_data.succ with
        | None -> {line_liveness with live_in = line_data.uses}
        | One (_, succ) -> 
          let succ_liveness = Hashtbl.find_or_add liveness succ ~default in
          let live_out = 
            Hash_set.union line_liveness.live_out succ_liveness.live_in 
          in
          (* exit early if the live_out set is too large *)
          if (Hash_set.length live_out > threshold)
          then raise DontRegalloc;
          
          (* continue constructing live-in *)
          let live_in = get_live_in line_data live_out in

          (* was live_in updated? *)
          if not ((Hash_set.length live_in) = (Hash_set.length line_liveness.live_in))
          then did_update := true;

          (* return the line *)
          {live_in; live_out}

        | Two (succ1, succ2) -> 
          let succ1_liveness = Hashtbl.find_or_add liveness (snd succ1) ~default in
          let succ2_liveness = Hashtbl.find_or_add liveness (snd succ2) ~default in
          let live_out = 
            Hash_set.union line_liveness.live_out (
              Hash_set.union succ1_liveness.live_in succ2_liveness.live_in
            )
          in
          let live_in = get_live_in line_data live_out in

          (* was live_in updated? *)
          if not ((Hash_set.length live_in) = (Hash_set.length line_liveness.live_in))
          then did_update := true;

          (* return the line *)
          {live_in; live_out}
      )
    );
  done;
  { liveness; body = cfg.body }
;;

let compute_liveness (cfgs : Cfg.cfgs) : program * bool = 
  try
    List.map cfgs ~f:compute_live_outs, true
  with
  | DontRegalloc -> [], false

;;


let pp_fun_lines (p : fun_lines) =
  Print.pp_grid [25;45;25] ["label"; "instruction"; "live_in"]
  (List.fold ~init:[] p.body ~f:(fun acc ((_, line), instr) ->
    let data = Hashtbl.find_exn p.liveness line in
    [Symbol.name line; 
     Assem.Print.pp_instr instr; 
     Print.pp_list Cfg.Dest.to_string (Hash_set.to_list data.live_in)
    ]::acc))
;;