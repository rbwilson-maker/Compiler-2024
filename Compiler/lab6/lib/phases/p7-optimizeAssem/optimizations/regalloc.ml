(* Rachompicole L3 Compiler
 * Register Allocator
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Does:
 * 1. Construct Interference Graph
 * 2. Greedy Graph Coloring
 * 3. Assign registers
 * 4. Spill temps (by not assigning a register)
 * 5. Report which registers were allocated
 *
 * Uses an abstract program and its liveness analysis to reconstruct the program
 * with temps replaced by registers. Spilled temps are left as temps.
 *)
open Core
module Dest = Cfg.Dest
module AS = Assem
module G = Graph

let assign_regs (coloring : (Dest.t, int) Hashtbl.t) : (int, AS.operand) Hashtbl.t = 
  let colors_to_regs = Hashtbl.create (module Int) in
  (* for each register and its color...*)
  Hashtbl.iteri coloring ~f:(fun ~key:d ~data:c -> 
    (* check if it's a reg or a temp. *)
    match d with
    | Dest.Reg r -> 
      (* if it's a reg, map its color to itself 
         (all equal registers should have the same color) 
      *)
      Hashtbl.set colors_to_regs ~key:c ~data:(AS.Reg r)
    | Dest.Temp (t, s) -> (
      (* if it's a temp, look up its color... *)
      match Hashtbl.find colors_to_regs c with
      | None -> (
        (* if its color does not have a register yet,
           try to find the corresponding register 
        *)
        Hashtbl.set colors_to_regs ~key:c ~data:(
          try Reg (Reg.available_regs.(c)) with
        | Invalid_argument _ -> AS.Temp (t, s)
        )
      )
      (* if its color does have one, check the sizes *)
      | Some (Temp (t2, s2)) -> (
        match s, s2 with 
        | Quad, Long -> 
          Hashtbl.set colors_to_regs ~key:c ~data:(AS.Temp (t2, Quad))
        | _ -> ()
      )
      | Some _ -> () (* already assigned a register *)
      

    )
  ); colors_to_regs
;;

(** Using a coloring, maps an instruction to its register allocated verstion. *)
let map_instr (spills : Temp.t Hash_set.t) (coloring : (Dest.t, int) Hashtbl.t) (colors_to_regs : (int, AS.operand) Hashtbl.t) =
  let rec op_map (op : AS.operand) =
    match op with
    | AS.Imm i -> AS.Imm i
    (* Registers either:
     * A) interfere with temps and are mapped to themselves in the coloring
     * B) don't interfere at all and thus can be left as themselves *)
    | AS.Reg r -> AS.Reg r
    | AS.Temp ((t1, _) as t) -> 
    if Hash_set.mem spills t1
    then AS.Temp t
    else (
      let d = Dest.Temp t in
      match Hashtbl.find coloring d with
      (* if d has no color, it's probably dead code, so map it to R11 to not waste stack space *)
      | None -> AS.Reg R11
      | Some c -> (
        match Hashtbl.find colors_to_regs c with
        | None -> failwith ("Color " ^ Int.to_string c ^ " not assigned a register")
        | Some new_op -> new_op
      )
    )
    | AS.Addr addr -> AS.Addr (addr_map addr)
  and addr_map (addr : AS.addr) =
    {addr with base = op_map addr.base; index = Option.map ~f:op_map addr.index}
  and fn_addr_map (f : AS.fn_addr) = match f with
    | Label _ as l -> l
    | Value v -> Value (op_map v)
  in
  function (inst : AS.instr) ->
    match inst with
    | AS.Binop {op; dest; lhs; rhs} -> AS.Binop 
      { op 
      ; dest = op_map dest
      ; lhs = op_map lhs
      ; rhs = op_map rhs
      }
    | AS.Equal {dest; lhs; rhs; size; op} -> AS.Equal
      { dest = op_map dest
      ; lhs = op_map lhs
      ; rhs = op_map rhs
      ; size; op
      }
    | AS.Mov {dest; src; size} -> AS.Mov 
      { dest = op_map dest
      ; src = op_map src
      ; size
      }
    | AS.Mov_addr {dest; src} -> AS.Mov_addr
      { dest = op_map dest
      ; src = addr_map src
      }
    | AS.Mov_reladdr {dest; src} -> AS.Mov_reladdr
      { dest = op_map dest
      ; src
      }
    (* dest should be mapped to RAX since dest should BE RAX *)
    | AS.Call f -> AS.Call
      { f with 
        dest = op_map f.dest
      ; params = List.map f.params ~f:(fun (op, s) -> op_map op, s)
      ; fn = fn_addr_map f.fn
      }
    | AS.Branch b -> AS.Branch {b with cond = 
      match b.cond with
      | `Single (op, s) -> `Single (op_map op, s)
      | `Comparison c -> `Comparison 
        { c with lhs = op_map c.lhs; rhs = op_map c.rhs }
      }
    | AS.Jump l -> AS.Jump (fn_addr_map l)
    | x -> x
;;

let regalloc_fun (d : bool) (cfg : Cfg.fun_cfg) (liveness : Liveness.fun_lines) = 
  let module G_dest = G.Make(Dest) in
  (* calculate interferences *)
  let interferences = List.fold liveness.body
  ~init:[]
  ~f:(fun acc ((block, line), _) ->
      let line_liveness = Hashtbl.find_exn liveness.liveness line in
      let line_data = Cfg.get_line cfg (block, line) in
      match line_data.defs with
      | `None -> acc
      | `One d -> (d, line_liveness.live_out)::acc
      | `Many ds -> List.fold ds ~init:acc ~f:(fun acc d -> (d, line_liveness.live_out)::acc) 
  ) in
  (* create the interference graph *)
  let graph = G_dest.make_from_adjlist interferences in
  if d then prerr_endline (G_dest.to_string graph);
  let nodes = G_dest.get_nodes graph in
  (* do maximum cardinality search *)
  let _, ordering = G_dest.mcs graph in
  if d then prerr_endline (Print.pp_list Dest.to_string ordering);
  (* precolor some registers *)
  let inits = (
    List.filter_map nodes
        ~f:(fun n -> match n with Dest.Reg r -> Some r | _ -> None)
    |> List.map ~f:(fun r -> (Dest.Reg r, Hashtbl.find_exn Reg.precolor r))
  ) in
  let coloring = (G_dest.color ~inits graph) in
  let colors_to_regs = assign_regs coloring in
  (coloring, colors_to_regs)
;;

let regalloc (d : bool) (cfg, liveness : Cfg.cfgs * Liveness.program) : AS.program =
  let live_fun = List.zip liveness cfg in
  match live_fun with 
  | Unequal_lengths -> failwith "liveness data given to regalloc does not match the length of the cfg"
  | Ok both ->
    List.map both ~f:(fun (liveness, fun_cfg) -> 
      let (coloring, colors_to_regs)  = regalloc_fun d fun_cfg liveness in
      let spills = List.map fun_cfg.spills ~f:fst in
      let map_instr' = 
        map_instr (Hash_set.of_list (module Temp) spills) coloring colors_to_regs 
      in
      (* figure out which registers were used *)
      let regs_used = List.filter_map (Hashtbl.data colors_to_regs) 
        ~f:(fun op -> match op with Reg r -> Some r | _ -> None) 
      in
      if d then prerr_endline (Print.pp_list (fun r -> Reg.format r Quad) regs_used);
      let body = List.map fun_cfg.body ~f:(fun i -> map_instr' (snd i)) in
      let out : AS.fun_instrs = 
        { name = fun_cfg.name
        ; body
        ; spills = fun_cfg.spills
        ; regs_used
        }
      in out
    )
;;
