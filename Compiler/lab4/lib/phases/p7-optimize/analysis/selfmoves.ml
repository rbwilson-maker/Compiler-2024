open Core
module AS = Assem
let assem_remove (p : AS.program) : AS.program = 
  let remove_from_body = List.filter ~f:(fun instr ->
    match instr with
    | AS.Mov {dest; src; _} ->
      not (AS.equal_operand dest src)
    | _ -> true
  ) in
  List.map p ~f:(fun f -> 
    {f with body = remove_from_body f.body}
  )
;;
