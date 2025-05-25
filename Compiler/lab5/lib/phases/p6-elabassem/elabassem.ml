(* Rachompicole L3 Compiler 
 * Elaborated Abstract Assembly
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * The goal of this phase is to take the abstract assembly
 * and insert any instructions that the convert phase shouldn't do.
 * It will "elaborate" mod, div, and fn calls to do appropriate moves before and after.
 * Similarly for sar and sal it will move to rcx. 
 * This will allow for more accurate liveness analysis which will aid in
 * register allocation.  
 *)

open Reg
open Core
module AS = Assem

(* The only register that should appear in the input Assem is RAX *)
let rec elab_assem_acc (l : AS.instr list) (rev_acc : AS.instr list) : AS.instr list = 
match l with
| Binop b :: instrs -> (
  match b.op with
  (* read each case bottom up *)
  | Div -> (
    (* rax <-- lhs
     * rax <-- rax / rhs
     * dest <-- rax
     *)
    AS.Mov { dest = b.dest; src = AS.Reg RAX; size = Long } ::
    AS.Binop { b with dest = AS.Reg RAX; lhs = AS.Reg RAX } ::
    AS.Mov { dest = AS.Reg RAX; src = b.lhs; size = Long } ::
    rev_acc
  )
  | Mod -> (
    (* rax <-- lhs
     * rdx <-- rax % rhs
     * dest <-- rdx
     *)
    AS.Mov { dest = b.dest; src = AS.Reg RDX; size = Long } ::
    AS.Binop { b with dest = AS.Reg RDX; lhs = AS.Reg RAX } ::
    AS.Mov { dest = AS.Reg RAX; src = b.lhs; size = Long } ::
    rev_acc
  )
  | Lshift | Rshift -> (
    (* rcx <-- rhs
     * dest <-- rcx >>/<< rhs
     *)
    AS.Binop { b with rhs = AS.Reg RCX} ::
    AS.Mov { dest = AS.Reg RCX; src = b.rhs; size = Long } ::
    rev_acc 
  )
  | _ -> AS.Binop b :: rev_acc
) |> elab_assem_acc instrs
| Call f :: instrs -> 
  (* rdi <-- arg1
   * rsi <-- arg2
   * rdx <-- arg3
   * rcx <-- arg4
   * r8  <-- arg5
   * r9  <-- arg6
   * rax <-- f (arg7, arg8, arg9, ...)
   * dest <-- rax 
   *)
  (let (_, rev_acc', spill_temps) =
  List.fold (f.params) ~init:(fn_args, rev_acc, []) ~f:(fun (regs, acc, spill) (p, s) ->
    match regs with
    | [] -> (regs, acc, (p, s)::spill)
    | r::regs -> 
      (regs, AS.Mov {dest = AS.Reg r; src = p; size = s} :: acc, spill)
  ) in
  let rev_acc'' = 
  AS.Call {f with dest = AS.Reg RAX; params = List.rev spill_temps} ::
  rev_acc'
  in (match f.size with
  | None -> rev_acc''
  | Some size -> (
    match f.dest with 
    | AS.Reg RAX -> rev_acc''
    | _ -> AS.Mov {dest = f.dest; src = AS.Reg RAX; size } :: rev_acc'')
  )
  |> elab_assem_acc instrs
)
| AS.Mov_addr {src; dest} as inst :: instrs ->
  (match src.disp, src.index, src.scale with
  | (None, None, None) -> 
    AS.Mov { src = src.base; dest = dest; size = Quad }
  | (Some x, None, None) when (Int32.equal x 0l) -> 
    AS.Mov { src = src.base; dest = dest; size = Quad }
  | _ -> inst
  ) :: rev_acc |> elab_assem_acc instrs 
| AS.Return :: instrs -> 
  (* find the next label and keep going from there, since we know each
     basic block starts with a label. *)
  let next_instrs = List.drop_while instrs ~f:(fun instr -> 
    match instr with AS.Label _ -> false | _ -> true
  ) in
  (AS.Return) :: rev_acc |> elab_assem_acc next_instrs
| AS.Raise _ as r :: instrs ->
  let next_instrs = List.drop_while instrs ~f:(fun instr -> 
    match instr with AS.Label _ -> false | _ -> true
  ) in
  (r :: rev_acc) |> elab_assem_acc next_instrs
| x :: instrs -> elab_assem_acc instrs (x::rev_acc) 
| [] -> rev_acc
;;

let elab_fun_args (args : (Temp.t * Size.t) list) = 
  (* Moves all arguments into their correct temps.
   * Makes new temps for spilled args to shorten their live range.
   * arg1 <-- rdi
   * arg2 <-- rsi
   * arg3 <-- rdx
   * arg4 <-- rcx
   * arg5 <-- r8
   * arg6 <-- r9
   * arg7 <-- newarg7
   * arg8 <-- newarg8
   *)
  let (_, moves, spills) = 
  List.fold args ~init:(fn_args, [], []) ~f:(fun (regs, movs, spill) (temp, size) ->
   match regs with
   | [] -> 
     let new_spill = Temp.create (), size in
     let move_temp = AS.Mov {src = AS.Temp new_spill; dest = AS.Temp (temp, size); size} in
     (regs, move_temp :: movs, new_spill::spill)
   | r::regs -> 
     (regs, AS.Mov {src = AS.Reg r; dest = AS.Temp (temp, size); size } :: movs, spill)
 ) in (moves, List.rev spills)
;;

let elab_instrs (f : AS.fun_instrs) : AS.fun_instrs =
  let prologue, spills = elab_fun_args f.spills in
  {f with body = List.rev (elab_assem_acc f.body prologue); spills }
;;

let elab_assem (funs : AS.program) : AS.program =
  List.map funs ~f:elab_instrs
