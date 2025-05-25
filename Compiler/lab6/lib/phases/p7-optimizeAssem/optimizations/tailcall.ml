open Core
module AS = Assem

(* need to look at two instructions at a time *)
(* TODO: store if a function is external/internal this only works on internal funs *)
let rec convert_body (cfg : Cfg.fun_cfg) (instrs : Cfg.fun_lines) (rev_acc : Cfg.fun_lines) : Cfg.fun_lines =
  match instrs with
  | [] -> rev_acc
  | (loc, instr) :: instrs ->
    match instr with
    (* transform call when it was defined internally, and does not need to spill args, and is recursive SO SPECIFIC *)
    | AS.Call c ->
      (match c.fn, c.params with
      | (Label (fn, extern), []) when (not extern && Symbol.equal fn cfg.name) ->
        let line_info = Cfg.get_line cfg loc in
        (if line_info.tail_call
        then 
          let tail_label = Symbol.symbol (Symbol.name fn ^ "_tail_call") in
          (loc, AS.Jump (Label (tail_label, extern))) :: rev_acc
        else 
          (loc, instr) :: rev_acc
        ) |> convert_body cfg instrs
      (* TODO: tail calls to lambda values ? -- they are never guaranteed recursive *)
      | _ -> convert_body cfg instrs ((loc, instr)::rev_acc)
      )
    | _ -> convert_body cfg instrs ((loc, instr)::rev_acc)
;;


let optimize_no_spills (p : Cfg.cfgs) : Cfg.cfgs = 
  let p_with_labels = List.map p ~f:(fun f ->
      let new_label = Symbol.symbol (Symbol.name f.name ^ "_tail_call") in
      let new_root = (fst f.root, new_label) in
      let fst_block = Hashtbl.find_exn f.graph (fst f.root) in
      Hashtbl.add_exn fst_block.lines ~key:new_label ~data:(
        { instr = AS.Label new_label
        ; uses = Hash_set.create (module Cfg.Dest)
        ; defs = `None
        ; succ = One f.root
        ; tail_call = false
        }
      );
      {f with body = (new_root, AS.Label new_label) :: f.body; root = new_root}
  ) in
  List.map p_with_labels ~f:(fun f ->
    (* the cfg itself will be storing incorrect instructions at the lines,
       but that's ok so long as the body field has the correct program *)
    match f.spills with 
    | [] -> {f with body = List.rev (
        convert_body f f.body []
      )}  
    | _ -> f
  )

;;