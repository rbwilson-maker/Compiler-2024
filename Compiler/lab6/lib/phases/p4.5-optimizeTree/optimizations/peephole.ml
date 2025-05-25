(* let rec reduce_temps (p : Tree.stm list) (rev_acc : Tree.stm list) = 
  match p with
  | [] -> rev_acc
  | [x] -> x :: rev_acc
  | stm1 :: (Move {dest = dest1; src} as stm2) :: stms -> (
    match src with
    | Tree.Temp t -> (
      let don't_squash () = reduce_temps (stm2 :: stms) (stm1 :: rev_acc) in
      let squash_dest_src dest2 stm = 
        (* for cases like 
           t2 <-- alloc(...)
           t1 <-- t2 
          which is very common *)
        if (Temp.equal (fst t) (fst dest2) && (Temp.(>) (fst t) (fst dest1)))
        then reduce_temps (stm :: stms) rev_acc
        else don't_squash ()
      in
      match stm1 with
      (* These instructions move to a temp *)
      | Move mv -> squash_dest_src mv.dest (Move {mv with dest = dest1})
      | Effect_move mv -> squash_dest_src mv.dest (Effect_move {mv with dest = dest1})
      | Fn_move mv -> (
        match mv.dest with
        | None -> don't_squash ()
        | Some d -> squash_dest_src d (Fn_move {mv with dest = Some dest1})
      )
      | Read_mem mv -> squash_dest_src mv.dest (Read_mem {mv with dest = dest1})
      | Array_addr mv -> squash_dest_src (mv.dest, 0) (Array_addr {mv with dest = fst dest1})
      | Alloc mv -> squash_dest_src mv.dest (Alloc {mv with dest = dest1})
      (* the rest don't *)
      | _ -> don't_squash ()
    )
    | _ -> reduce_temps (stm2 :: stms) (stm1 :: rev_acc)
  )
  | stm1 :: stm2 :: stms -> reduce_temps (stm2 :: stms) (stm1 :: rev_acc)
;;

let reduce_program_temps (p : Tree.program) = 
  List.map p ~f:(fun (fn, params, body) ->
    (fn, params, List.rev (reduce_temps body []))
  ) *)