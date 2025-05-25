(* L1 Compiler
 * Register Allocator
 *
 * Does:
 * 1. Detect Liveness
 * 2. Construct Interference Graph
 * 3. Greedy Graph Coloring
 * 4. Assign registers
 * 5. Spill temps
 *
 * Converts from Assem.instr list -> Assem.instr list
 * with temps replaced by registers. Spilled temps are left as temps.
 *)
open Core
module L = Lab1_checkpoint
module AS = Assem
module G = Graph

module Dest = struct
  type t = Reg of Reg.t | Temp of Temp.t
  [@@deriving compare, equal, hash, sexp]
  let to_string (d : t) = 
    match d with
    | Reg r -> Reg.format r Reg.Quad
    | Temp t -> Temp.name t
end

type line = 
{ uses : Dest.t list
; defs : Dest.t option (* None if the line defines more than one, or is a directive *)
; live_out : Dest.t list
; line : int
; instr : AS.instr option (* None when running checkpoint *)
}
let available_regs : Reg.t Array.t = 
  Array.of_list Reg.[RBX; RCX; RDX; RDI; RSI; R8; R9; R10; R12; R13; R14; R15]  
let alloc_threshold = 75

exception NotADest
let op_to_dest (op : AS.operand) : Dest.t = 
  match op with
  | AS.Imm _ -> raise NotADest
  | AS.Reg r -> Dest.Reg r
  | AS.Temp t -> Dest.Temp t

let compute_live_outs (code : AS.instr list) : line list = 
  let defs (l : AS.instr) : Dest.t option = 
    match l with
    | Binop { dest = d; _ } -> Some (op_to_dest d)
    | Mov { dest = d; _ } -> Some (op_to_dest d)
    | _ -> None

  in let uses (l : AS.instr) : Dest.t list  = 
    match l with
    | Binop {lhs = d1; rhs = d2; _ } -> 
      ( try [op_to_dest d1] with
        | NotADest -> []
        ) @
      ( try [op_to_dest d2] with
        | NotADest -> []
        )
    | Mov { src = s; _ } -> (
      try [op_to_dest s] with
      | NotADest -> []
    )
    | _ -> []
  
  (* live_outs is updated by live_out
   * live_out uses live_in
   *)
  in let live_outs : Dest.t list Array.t = Array.create ~len:(List.length code) []
  in let rec live_out (code : AS.instr list) (i : int) : unit =
    match code with 
    | [] -> ()
    | _::[] -> () (* reach ret *)
    | _::next::rest -> Array.set live_outs i (live_in (next::rest) (i+1))
  
  and live_in (code : AS.instr list) (i : int): Dest.t list = 
    match code with
    | [] -> []
    (* Compute live out of current line then construct current result *)
    | l::ls -> live_out (l::ls) i; (
      match defs(l) with
      | None -> live_outs.(i) @ uses(l)
      | Some d -> 
        List.filter live_outs.(i) ~f:(fun x -> not (Dest.equal d x))
        @ uses (l) 
        (* Append creates a multi-set; graph creation removes dups *)
      )
  in
    live_out code 0;
    List.mapi code ~f:(fun i l -> 
      { uses = uses(l)
      ; defs = defs(l)
      ; live_out = live_outs.(i)
      ; line = i
      ; instr = Some l
      }
    )  
;;

let assign_regs (coloring : (Dest.t, int) Hashtbl.t) : (int, AS.operand) Hashtbl.t = 
  
  let colors_to_regs = Hashtbl.create (module Int) in
  Hashtbl.iteri coloring ~f:(fun ~key:d ~data:c -> 
    match d with
    | Dest.Reg r -> Hashtbl.set colors_to_regs ~key:c ~data:(AS.Reg r)
    | Dest.Temp t -> (
      match Hashtbl.find colors_to_regs c with
      | None -> (
        Hashtbl.set colors_to_regs ~key:c ~data:(
          try Reg (available_regs.(c)) with
        | Invalid_argument _ -> AS.Temp t
        )
      )
      | _ -> () (* already assigned a register *)
    )
  ); colors_to_regs
;;

let map_instr (coloring : (Dest.t, int) Hashtbl.t) (colors_to_regs : (int, AS.operand) Hashtbl.t) =
  let op_map (op : AS.operand) =
    match op with
    | AS.Imm i -> AS.Imm i
    | x -> (
      match Hashtbl.find coloring (op_to_dest x) with
      | None -> failwith "dest in instruction list somehow not given a color"
      | Some c -> (
        match Hashtbl.find colors_to_regs c with
        | None -> failwith "allocated color not given a register"
        | Some new_op -> new_op
      )
    )
  in
  function (inst : AS.instr) -> 
    match inst with
    | AS.Binop {op; dest; lhs; rhs} -> AS.Binop 
      { op 
      ; dest=op_map(dest)
      ; lhs=op_map(lhs)
      ; rhs=op_map(rhs)
      }
    | AS.Mov {dest; src} -> AS.Mov 
      { dest=op_map(dest)
      ; src=op_map(src)
      }
    | x -> x
;;

let regalloc_lines (lines : line list) = 
  let module X = G.Make (Dest) in
  let interferences = List.fold lines 
  ~init:[]
  ~f:(fun l -> fun line ->
      match line.defs with
      | None -> l
      | Some d -> (d, line.live_out)::l
  ) in
  let graph = X.make_from_adjlist interferences in
  let nodes = X.get_nodes graph in
  let inits = (
    List.filter (nodes) 
        ~f:(fun n -> match n with Dest.Reg _ -> true | _ -> false)
    |> List.mapi ~f:(fun i x -> (x, i))
  ) in
    if List.length (X.get_nodes graph) > alloc_threshold
    then None
    else (
      let coloring = (X.color ~inits graph) in
      let colors_to_regs = assign_regs coloring in
      Some (coloring, colors_to_regs)
    )
;;

let regalloc (program : AS.instr list) : AS.instr list =
  let lines = compute_live_outs program in
  match regalloc_lines lines with
  | None -> program (* Do not allocate, just leave as temps to spill *)
  | Some (coloring, colors_to_regs) ->
  let map_instr' = map_instr coloring colors_to_regs
  in List.map program ~f:map_instr'
;;

(* L1 Checkpoint no longer needed *)
let regalloc_checkpoint (_input : L.program) : L.allocations = 
  failwith "Checkpoint code not in use"
 (* let reg_map (r : L.reg) : Dest.t = 
    match r with
    | "%eax" -> Dest.Reg RAX
    | "%edx" -> Dest.Reg RDX
    | s -> Dest.Temp (Temp.create_with_name s)
  in let lines =
  List.map input ~f:(fun ll -> 
    { uses = List.map ll.uses ~f:reg_map
    ; defs = (match ll.defines with x::[] -> Some (reg_map x) | _ -> None)
    ; live_out = List.map ll.live_out ~f:reg_map
    ; line = ll.line_number
    ; instr = None})
  in let coloring, colors_to_regs = regalloc_lines lines (* Handle None *)
  in
  List.fold_right lines ~init:[] ~f:(fun l acc ->
    match l.defs with
    | None -> None::acc
    | Some d -> (
      match d with
      | Dest.Reg _ -> None::acc
      | Dest.Temp t -> (
      match Hashtbl.find coloring d with
      | None -> failwith "All temps should be assigned colors"
      | Some c -> (
      match Hashtbl.find colors_to_regs c with
      | None -> failwith "every color should be assigned a register"
      | Some r -> (Some (
        match r with
      | AS.Temp t' -> (Temp.name t, Temp.name t')
      | AS.Reg r' -> (Temp.name t, Reg.format r' Reg.Quad)
      | _ -> failwith "How did you assign a color to an immutable?"
      ))::acc
      )
      )
    ))
;; *)
