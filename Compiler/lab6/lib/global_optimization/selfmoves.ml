open Core
module AS = Assem
let remove_assem (p : AS.program) : AS.program = 
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

module X = X86
let remove_x86 (p : X.program) : X.program = 
  let remove_from_funs = List.fold_right ~init:[] ~f:(fun instr acc ->
    match instr with
    (* mov src src ==> nop*)
    | X.Mov (_, src, dest) when (X.equal_operand src dest) -> acc
    (* lea 0(src) src ==> nop *)
    | X.Lea (_, `A (X.Mem (disp, base, index, scale)), X.Reg dest) when (Reg.equal base dest) -> 
      (match disp, index, scale with
      | (None, None, None) -> acc
      | (Some x, None, None) when (Int32.equal x 0l) -> acc
      | _ -> instr :: acc
      )
    (* sub n rsp; push 0 ==> sub n+8 rsp 
       sub 0 rsp ==> nop *)
    | X.Sub (s, X.Imm (`Long n), X.Reg RSP) -> (
      match acc with
      | X.Push (X.Imm (`Long 0l)) :: rest -> 
        X.Sub (s, X.Imm (`Long (Int32.(n + 8l))), X.Reg RSP) :: rest
      | _ -> 
        if (Int32.equal n 0l) 
        then acc 
        else instr :: acc
    )
    (* push r; pop r ==> nop *)
    | X.Push X.Reg r1 -> (
      match acc with
      | X.Pop r2 :: rest when (Reg.equal r1 r2) -> rest
      | _ -> instr :: acc
    )
    | X.Pop r1 -> (
      match acc with
      | X.Push X.Reg r2 :: rest when (Reg.equal r1 r2) -> rest
      | _ -> instr :: acc
    )
    (* mov reg, r11; cmp lhs r11 ==> cmp lhs reg *)
    | X.Mov (s1, (X.Reg _ as rhs), X.Reg R11) -> (
      match acc with
      | X.Cmp (s2, lhs, X.Reg R11) :: rest ->
        assert (Size.equal s1 s2);
        X.Cmp (s2, lhs, rhs) :: rest
      | _ -> instr :: acc
    )
    (* jmp x; jmp y ==> jmp x *)
    | X.Jmp l1 -> (
      match acc with 
      | X.Jmp _ :: rest -> X.Jmp l1 :: rest
      | _ -> instr :: acc
    )
    | _ -> instr :: acc
  ) in
  List.map p ~f:remove_from_funs
;;