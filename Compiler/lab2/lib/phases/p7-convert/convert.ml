open Core
open Reg

module AS = Assem
module X = X86

(* This type is passed through all the functions and includes information
 * relevant to converting into x86-64 code.
 *  *)
type conversion_info = {
  (* Stack operations, reversed. This has to do with *)
  sops_r : X.line list; (* Stack operations, reversed. *)
  ops_r  : X.line list; (* Operations, reversed. *)
  table : (Temp.t, X.addr) Hashtbl.t; (* Mutable *)
}

let get_add_temp info (t : Temp.t) =
  let stack_size = Hashtbl.length info.table in
  let make_new_addr () : X.addr = (
    Some (Option.value_exn (Int32.of_int ((stack_size + 1) * -4))),
    RBP,
    None,
    None
  ) in
  let addr = Hashtbl.find_or_add info.table t ~default:make_new_addr in
  (* Return the (unchanged) info and X.Mem operand *)
  (info, X.Mem addr)

let conv_opand info = function
  | AS.Imm n -> (info, X.Imm n)
  | AS.Reg r -> (info, X.Reg r)
  | AS.Temp t -> get_add_temp info t

let conv_mov info (dest, src) =
  let (info, dest) = conv_opand info dest in
  let (info, src) = conv_opand info src in
  match (src, dest) with
    | (X.Mem _, X.Mem _) ->
      {
        sops_r = info.sops_r;
        ops_r = (
          (X.Mov (Long, X.Reg R11, dest)) ::
          (X.Mov (Long, src, X.Reg R11)) :: (* todo check for usage *)
          info.ops_r
        );
        table = info.table
      }
    | _ ->
      {
        sops_r = info.sops_r;
        ops_r = (X.Mov (Long, src, dest)) :: info.ops_r;
        table = info.table
      }

let conv_add info dest (rhs, lhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (X.Add (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_sub info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Sub (Long, rhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_mul info dest =
  let imm_on_left = (function
    | (X.Imm n, rhs) ->
      (* IMUL r32, r/m32, imm32 is r32 := r/m32 * imm32 *)
      let ops_r = List.rev_append (
        (X.Imul (Long, Some n, rhs, R11)) ::
        (X.Mov (Long, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}
    | _ -> failwith "Unexpected input")
  in function
    | ((X.Imm _ as lhs), rhs) -> imm_on_left (lhs, rhs)
    | (lhs, (X.Imm _ as rhs)) -> imm_on_left (rhs, lhs)
    | (lhs, rhs) -> (* IMUL r64, r/m64 is r64 *= r/m64 *)
      let ops_r = List.rev_append (
        (X.Mov (Long, lhs, X.Reg R11)) ::
        (X.Imul (Long, None, rhs, R11)) ::
        (X.Mov (Long, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}

let conv_div info dest (lhs, rhs) =
  let dest_is_RAX = match dest with
    | X.Reg RAX -> true
    | _ -> false
  in
  let dest_is_RDX = match dest with
    | X.Reg RDX -> true
    | _ -> false
  in
  let need_to_save_rhs_in_r11 = match rhs with
    | X.Reg RAX -> true (* RAX gets overwritten *)
    | X.Reg RDX -> true (* RDX gets overwritten *)
    | X.Imm _ -> true   (* idiv can't take imm operand *)
    | _ -> false
  in
  let ops_r = List.rev_append (
    ( (* Save RDX on stack. *)
      if dest_is_RAX
      then []
      else [(X.Push (X.Reg RAX))]
    ) @ (
      if dest_is_RDX
      then []
      else [(X.Push (X.Reg RDX))]
    ) @ (
      if need_to_save_rhs_in_r11
      then [X.Mov (Long, rhs, X.Reg R11)]
      else []
    ) @ (
      (X.Mov (Long, lhs, X.Reg RAX)) ::
      (X.Cltd) :: []
    ) @ (
      if need_to_save_rhs_in_r11
      then [X.Idiv (Long, X.Reg R11)]
      else [X.Idiv (Long, rhs)]
    ) @ (
      if dest_is_RDX
      then []
      else (X.Pop (RDX)) :: []
    ) @ ( (* Idiv quotient is in RAX *)
      if dest_is_RAX
      then []
      else (X.Mov (Long, X.Reg RAX, dest)) ::
           (X.Pop (RAX)) :: []
    )
  ) info.ops_r
  in {info with ops_r}

  let conv_mod info dest (lhs, rhs) =
    let dest_is_RDX = match dest with
      | X.Reg RDX -> true
      | _ -> false
    in 
    let dest_is_RAX = match dest with
      | X.Reg RAX -> true
      | _ -> false
    in
    let need_to_save_rhs_in_r11 = match rhs with
      | X.Reg RAX -> true (* RAX gets overwritten *)
      | X.Reg RDX -> true (* RDX gets overwritten *)
      | X.Imm _ -> true   (* idiv can't take imm operand *)
      | _ -> false
    in
    let ops_r = List.rev_append (
      ((* Save RDX and RAX on stack if it isn't dest *)
        if dest_is_RAX
        then []
        else [(X.Push (X.Reg RAX))]
      ) @ (
        if dest_is_RDX
        then []
        else [(X.Push (X.Reg RDX))]
      ) @ (
        if need_to_save_rhs_in_r11
        then [X.Mov (Long, rhs, X.Reg R11)]
        else []
      ) @ (
        (X.Mov (Long, lhs, X.Reg RAX)) ::
        (X.Cltd) :: []
      ) @ (
        if need_to_save_rhs_in_r11
        then [X.Idiv (Long, X.Reg R11)]
        else [X.Idiv (Long, rhs)]
      ) @ ( (* Idiv remainder is in RDX *)
        if dest_is_RDX
        then []
        else (X.Mov (Long, X.Reg RDX, dest)) ::
             (X.Pop (RDX)) :: []
      ) @ (
        if dest_is_RAX
        then []
        else (X.Pop (RAX)) :: []
      )
    ) info.ops_r
    in {info with ops_r}

let conv_bitand info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (X.And (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_bitor info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (X.Or (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_bitxor info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (X.Xor (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_lshift info dest (lhs, rhs) =
  let dest_is_RCX = match dest with
    | X.Reg RCX -> true
    | _ -> false
  in
  let ops_r = List.rev_append (
    (
      if dest_is_RCX
      then []
      else [(X.Push (X.Reg RCX))]
    ) @ (
      (X.Mov (Long, lhs, X.Reg R11)) ::
      (X.Mov (Long, rhs, X.Reg RCX)) ::
      (X.Sal (Long, X.Reg RCX, X.Reg R11)) :: []
    ) @ (
      if dest_is_RCX
      then []
      else (X.Pop RCX) ::
           (X.Mov (Long, X.Reg R11, dest)) :: []
    ) @ []
  ) info.ops_r
  in {info with ops_r}

let conv_rshift info dest (lhs, rhs) =
  let dest_is_RCX = match dest with
    | X.Reg RCX -> true
    | _ -> false
  in
  let ops_r = List.rev_append (
    (
      if dest_is_RCX
      then []
      else [(X.Push (X.Reg RCX))]
    ) @ (
      (X.Mov (Long, lhs, X.Reg R11)) ::
      (X.Mov (Long, rhs, X.Reg RCX)) ::
      (X.Sar (Long, X.Reg RCX, X.Reg R11)) :: []
    ) @ (
      if dest_is_RCX
      then []
      else (X.Pop RCX) ::
           (X.Mov (Long, X.Reg R11, dest)) :: []
    ) @ []
  ) info.ops_r
  in {info with ops_r}

let conv_less info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Less, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_greater info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Greater, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_equal info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Equal, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_leq info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Leq, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_geq info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Geq, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_neq info dest (lhs, rhs) =
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (Neq, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}

let conv_binop info (op : AS.operation) dest_a (lhs, rhs) =
    let (info, lhs) = conv_opand info lhs in
    let (info, rhs) = conv_opand info rhs in
    let (info, dest) = conv_opand info dest_a in (
    match (lhs, rhs) with
      | (X.Imm m, X.Imm n) -> conv_mov info (dest_a, AS.Imm (Int32.(m + n)))
      | _ -> match op with
        | Add -> conv_add info dest (lhs, rhs)
        | Sub -> conv_sub info dest (lhs, rhs)
        | Mul -> conv_mul info dest (lhs, rhs)
        | Div -> conv_div info dest (lhs, rhs)
        | Mod -> conv_mod info dest (lhs, rhs)
        | Bit_and -> conv_bitand info dest (lhs, rhs)
        | Bit_or  -> conv_bitor info dest (lhs, rhs)
        | Bit_xor -> conv_bitxor info dest (lhs, rhs)
        | Lshift -> conv_lshift info dest (lhs, rhs)
        | Rshift -> conv_rshift info dest (lhs, rhs)
        | Comp Less -> conv_less info dest (lhs, rhs)
        | Comp Greater -> conv_greater info dest (lhs, rhs)
        | Comp Equal -> conv_equal info dest (lhs, rhs)
        | Comp Leq -> conv_leq info dest (lhs, rhs)
        | Comp Geq -> conv_geq info dest (lhs, rhs)
        | Comp Neq -> conv_neq info dest (lhs, rhs)
    )

let label_to_str = Symbol.name

let conv_branch info (_if_label, else_label, after_label, cond) =
  match cond with
  | `Single opand ->
    let (info, opand) = conv_opand info opand in
    let cond_unmet_label = match else_label with
      | None -> label_to_str after_label
      | Some else_label -> label_to_str else_label
    in
    let ops_r = List.rev_append (
      (X.Cmp (Long, X.Imm 0l, opand)) ::
      (X.Jcc (X.Equal, cond_unmet_label)) :: []
    ) info.ops_r
    in {info with ops_r}
  | `Comparison AS.{lhs; op; rhs} ->
    let (info, lhs) = conv_opand info lhs in
    let (info, rhs) = conv_opand info rhs in
    let ops_r = List.rev_append (
      (X.Mov (Long, lhs, X.Reg R11)) ::
      (X.Cmp (Long, rhs, X.Reg R11)) ::
      (
        let cond_unmet_label = match else_label with
          | None -> label_to_str after_label
          | Some else_label -> label_to_str else_label
        in
        match op with
          | Less -> X.Jcc (X.Geq, cond_unmet_label)
          | Greater -> X.Jcc (X.Leq, cond_unmet_label)
          | Equal -> X.Jcc (X.Neq, cond_unmet_label)
          | Leq -> X.Jcc (X.Greater, cond_unmet_label)
          | Geq -> X.Jcc (X.Less, cond_unmet_label)
          | Neq -> X.Jcc (X.Equal, cond_unmet_label)
      ) :: []
    ) info.ops_r
    in {info with ops_r}

let exit_label = "exit"

let conv_instr info = function
  | AS.Return -> { info with ops_r = (X.Jmp exit_label) :: info.ops_r }
  | AS.Binop {op; dest; lhs; rhs} -> conv_binop info op dest (lhs, rhs)
  | AS.Mov {dest; src} -> conv_mov info (dest, src)
  | AS.Branch {if_label; else_label; after_label; cond} ->
      conv_branch info (if_label, else_label, after_label, cond)
  | AS.Jump label ->
    { info with ops_r = (X.Jmp (label_to_str label)) :: info.ops_r }
  | AS.Label label ->
    { info with ops_r = (X.Label (label_to_str label)) :: info.ops_r }
  | AS.Directive _ -> info (* Do nothing *) (* todo *)
  | AS.Comment _ -> info (* Do nothing *)

let convert (l : AS.instr list) =
  let stack_ops_rev = [] in
  let ops_rev = [] in
  let table = Hashtbl.create (module Temp) in
  let f = conv_instr in
  List.fold l ~init:{sops_r = stack_ops_rev; ops_r = ops_rev; table} ~f
   |> fun {sops_r; ops_r; table} -> (* Append exit label, leave, and return to the end. *)
      {sops_r; ops_r = X.Ret :: (X.Leave :: (X.Label exit_label :: ops_r)); table}
   |> fun {sops_r; ops_r; table} -> (* Add starting rbp-saving instructions. *)
      (table, List.rev_append sops_r (List.rev ops_r))
   |> fun (table, instr_list) ->
      let stack_size_bytes = Option.value_exn (Int32.of_int (
        (((Hashtbl.length table) * 4 - 1) / 8 + 1) * 8 (* Ceil up to an 8x *)
      )) in
      (X.Push (X.Reg RBP)) ::
      (X.Mov (Quad, X.Reg RSP, X.Reg RBP)) ::
      (X.Sub (Quad, X.Imm stack_size_bytes, X.Reg RSP)) ::
      instr_list