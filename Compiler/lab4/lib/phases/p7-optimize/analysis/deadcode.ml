open Core
module AS = Assem

(* MUTATES LIVENESS INPUT --- NOT CORRECT *)
(* this function needs to remove entire basic blocks that are never reached, not just 
   remove single instructions *)
let assem_remove (liveness, p : Live_assem.program * AS.program) : AS.program =
  let remove_from_fun (fun_liveness, f : Live_assem.fun_lines * AS.fun_instrs) = 
    let successors = Hash_set.create (module Symbol) in
    Hashtbl.fold fun_liveness ~init:() ~f:(fun ~key:_ ~data () -> 
      match data.succ with `None -> ()
      | `One l -> Hash_set.add successors l
      | `Two (l1, l2) -> Hash_set.add successors l1; Hash_set.add successors l2
    ); 
    Hashtbl.filter_keys_inplace fun_liveness ~f:(fun line -> 
      Hash_set.mem successors line
    );
    let new_fun_instrs = List.map (
      List.sort (Hashtbl.to_alist fun_liveness) 
      ~compare:(fun (_, line1) (_, line2) -> Int.compare line1.num line2.num)
    ) ~f:(fun (_, line) -> line.instr)
    in 
    {f with body = new_fun_instrs}
  in
  List.map2_exn liveness p ~f:(fun fun_liveness f -> 
    remove_from_fun (fun_liveness, f)
    )
;;