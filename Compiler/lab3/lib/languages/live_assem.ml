(* Rachompicole L3 Compiler 
 * Liveness Interface
 * Authors: Rachel Wilson
 * 
 * This interface encodes an abstract assembly
 * program with its control flow graph
 * with extra liveness information.
 *)
open Core
module AS = Assem

module Dest = struct
  type t = Reg of Reg.t | Temp of Temp.t
  [@@deriving compare, equal, hash, sexp]
  let to_string (d : t) = 
    match d with
    | Reg r -> Reg.format r Reg.Quad
    | Temp t -> Temp.name t
end

let op_to_dest (op : AS.operand) : Dest.t option = 
  match op with
  | AS.Imm _ -> None
  | AS.Reg r -> Some (Dest.Reg r)
  | AS.Temp t -> Some (Dest.Temp t)
;;

let dest_to_op (d : Dest.t) : AS.operand =
  match d with
  | Dest.Reg r -> AS.Reg r
  | Dest.Temp t -> AS.Temp t
;;

type line = 
  { uses : Dest.t Hash_set.t
  ; defs : [`None | `One of Dest.t | `Many of Dest.t list]
  ; succ : [`None | `One of Symbol.t | `Two of Symbol.t * Symbol.t]
  ; live_in : Dest.t Hash_set.t
  ; live_out : Dest.t Hash_set.t
  ; instr : AS.instr
  }

type fun_lines = (Symbol.t, line) Hashtbl.t
type program = fun_lines list

module Print = struct
  open Print
  let pp_succ = function
  |`None -> "None"
  | `One s -> Symbol.name s
  | `Two (s1, s2) -> pp_list Symbol.name [s1;s2]
  let pp_line label line = 
    sprintf "%-15s%-40s%-20s%-15s\n" 
    (Symbol.name label)
    (AS.Print.pp_instr line.instr) 
    (pp_succ line.succ)
    (pp_list Dest.to_string (Hash_set.to_list line.live_in)) 

  let pp_fun_lines (p : fun_lines) =
    let order = List.sort ~compare:Symbol.ascending (Hashtbl.keys p) in
    pp_grid [15;40;20;15] ["label"; "instruction"; "succ"; "live_in"]
    (List.fold ~init:[] order ~f:(fun acc key ->
      let data = Hashtbl.find_exn p key in
      [Symbol.name key; 
       AS.Print.pp_instr data.instr; 
       pp_succ data.succ; 
       pp_list Dest.to_string (Hash_set.to_list data.live_in)
      ]::acc))

  let pp_dest_table (h : (Dest.t, 'a) Hashtbl.t) (a_to_string : 'a -> string) = 
    sprintf "{\n%s}" (
      Hashtbl.fold h ~init:"" ~f:(fun ~key ~data acc ->
        acc ^ sprintf "%s: %s\n" (Dest.to_string key) (a_to_string data)
      )
    )
end