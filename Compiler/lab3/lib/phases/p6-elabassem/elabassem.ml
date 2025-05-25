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

(* updated by each function, used globally *)
let exit_label = ref (Symbol.symbol "TEMPORARY")

(* The only register that should appear in Assem before, is RAX *)
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
    AS.Mov { dest = b.dest; src = AS.Reg RAX } ::
    AS.Binop { b with dest = AS.Reg RAX; lhs = AS.Reg RAX } ::
    AS.Mov { dest = AS.Reg RAX; src = b.lhs } ::
    rev_acc
  )
  | Mod -> (
    (* rax <-- lhs
     * rdx <-- rax % rhs
     * dest <-- rdx
     *)
    AS.Mov { dest = b.dest; src = AS.Reg RDX } ::
    AS.Binop { b with dest = AS.Reg RDX; lhs = AS.Reg RAX } ::
    AS.Mov { dest = AS.Reg RAX; src = b.lhs } ::
    rev_acc
  )
  | Lshift | Rshift -> (
    (* rcx <-- rhs
     * dest <-- rcx >>/<< rhs
     *)
    AS.Binop { b with rhs = AS.Reg RCX} ::
    AS.Mov { dest = AS.Reg RCX; src = b.rhs } ::
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
  List.fold (f.params) ~init:(fn_args, rev_acc, []) ~f:(fun (regs, acc, spill) p ->
    match regs with
    | [] -> (regs, acc, p::spill)
    | r::regs -> 
      (regs, AS.Mov {dest = AS.Reg r; src = p} :: acc, spill)
  ) in
  AS.Mov {dest = f.dest; src = AS.Reg RAX} ::
  AS.Call {dest = AS.Reg RAX; fn = f.fn; params = List.rev spill_temps} ::
  rev_acc'
  |> elab_assem_acc instrs
)
| AS.Return :: instrs -> 
  (AS.Jump !exit_label) :: rev_acc |> elab_assem_acc instrs
| AS.Raise Sigfpe :: instrs -> 
    (* Div will get elaborated so not both lhs and rhs are imms *)
    (AS.Binop 
    { op = Div
    ; dest = Temp (Temp.create ())
    ; lhs = Imm (Int32.one)
    ; rhs = Imm (Int32.zero) 
    }) :: (AS.Jump !exit_label) :: instrs 
    |> (fun instrs -> elab_assem_acc instrs rev_acc)
| AS.Raise Sigabrt :: instrs -> 
    let abort_sym = (
      match Sys.getenv "UNAME" with
      | Some "Darwin" -> Symbol.symbol "_abort"
      | _ -> Symbol.symbol "abort"
    ) in
    (AS.Jump !exit_label) :: 
    (AS.Call 
    { dest = Temp (Temp.create ())
    ; fn = abort_sym
    ; params = []
    }) :: rev_acc |> elab_assem_acc instrs
| x :: instrs -> elab_assem_acc instrs (x::rev_acc) 
| [] -> rev_acc
;;

let elab_fun_args (args : Temp.t list) = 
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
  List.fold args ~init:(fn_args, [], []) ~f:(fun (regs, movs, spill) temp ->
   match regs with
   | [] -> 
     let new_spill = Temp.create () in
     let move_temp = AS.Mov {src = AS.Temp new_spill; dest = AS.Temp temp} in
     (regs, move_temp :: movs, new_spill::spill)
   | r::regs -> 
     (regs, AS.Mov {src = AS.Reg r; dest = AS.Temp temp} :: movs, spill)
 ) in (moves, List.rev spills)
;;

let elab_instrs (f : AS.fun_instrs) : AS.fun_instrs =
  exit_label := Symbol.unique_symbol "exit";
  let prologue, spills = elab_fun_args f.spills in
  let out = 
    (AS.Label !exit_label) ::
    elab_assem_acc f.body prologue
  in
  {f with body = List.rev (out); spills}
;;

let elab_assem (funs : AS.program) : AS.program =
  List.map funs ~f:elab_instrs
