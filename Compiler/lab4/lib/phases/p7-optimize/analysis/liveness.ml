(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * assem -> live_assem
 * This file performs liveness analysis on an abstract assembly program.
 * It is implemented using lots of imperative programming and iterating over
 * a computed hashmap.
 *)
open Core
open Live_assem
module AS = Assem

(* Computes a hashmap mapping line labels to liveness information for
  a particular function *)
let compute_live_outs (code : AS.fun_instrs) : Live_assem.fun_lines = 
  let liveness = Hashtbl.create (module Symbol) in
  let did_update = ref true in
  let length = List.length code.body in
  (* initialize each line with its label and successor *)
  let next = ref (Symbol.symbol ("temporary")) in (* should get overridden *)
  List.iteri (List.rev code.body) ~f:(fun i line -> 
    let curr = Symbol.symbol ("line" ^ (Int.to_string i)) in
    let next1 = if Int.equal i 0 then `None else `One (!next) in
    let set_next () = next := curr in
    let key, succ = match line with 
      | AS.Label l -> next := l; l, next1
      | Return | Raise _ -> set_next (); curr, `None
      | Jump l -> set_next (); curr, `One l
      | Branch {if_label; else_label; after_label; _} -> set_next (); 
        curr, 
        `Two (if_label, (match else_label with 
        |  None -> after_label 
        | Some else_label -> else_label))
      | _ -> set_next (); curr, next1
    in
    let f () = Hash_set.create (module Dest) in
    Hashtbl.add_exn liveness ~key ~data:{
      uses = f (); defs = `None; succ = succ; 
      live_in = f (); live_out = f (); instr = line;
      num = length - i
    }; 
  );
  (* code is a list of labels which act as pointers to their line of code 
   * it is not necessarily in order! *) 
  let code = Hashtbl.keys liveness in

  (* Updates uses with all used variables, and returns a dest *)
  let update_uses_defs line uses = 
    let rec uses_op op = 
      match op with
      | AS.Addr {base; index; _} ->
        uses_op base; Option.iter ~f:uses_op index
      | _ -> 
        (match op_to_dest op with
        | None -> () 
        | Some d -> Hash_set.add uses d
        )
    in let defs_op op = 
      match op with
      | AS.Addr {base; index; _} ->
        uses_op base; Option.iter ~f:uses_op index;
        `None
      | _ ->
        (match op_to_dest op with
        | None -> `None
        | Some d -> `One d
        )
    in match line.instr with
    | AS.Mov {src; dest; _} -> uses_op src; defs_op dest;
    | AS.Mov_addr {src; dest} -> uses_op (AS.Addr src); defs_op dest;
    | AS.Binop {dest; rhs; lhs; op} -> (
      match op with 
      | AS.Div | AS.Mod -> 
        uses_op rhs; uses_op lhs; `Many [Dest.Reg RAX; Dest.Reg RDX] (* dest should be RAX/RDX *)
      | _ -> uses_op rhs; uses_op lhs; defs_op dest
    )
    | AS.Equal {dest; lhs; rhs; _} -> 
      uses_op rhs; uses_op lhs; defs_op dest
    | AS.Call {params; _} -> List.iter params ~f:(fun (op, _) -> uses_op op); (* dest should be RAX *) 
      `Many (List.map Reg.caller_saves ~f:(fun r -> Dest.Reg r))
    | AS.Branch {cond = `Single (op, _); _} -> uses_op op; `None
    | AS.Branch {cond = `Comparison {lhs; rhs; _}; _} -> uses_op lhs; uses_op rhs; `None
    | AS.Return -> Hash_set.add uses (Reg RAX); `None
    | _ -> `None
  in
  (* update each line with its uses and defs *)
  let _ : Symbol.t list = List.fold_right code ~init:[] ~f:(fun x rest ->
    (Hashtbl.update liveness x ~f:(fun line -> 
      match line with None -> failwith "x should be in the hashtbl"
      | Some line -> 
        let defs = update_uses_defs line line.uses in
        {line with defs}
    ); x::rest)
  ) in
  (* fold right over the labels, starting from the end of the program, 
   * using the live-in of their successor to determine the live-out *)
  while !did_update do
    did_update := false;
    List.fold_right code ~init:() ~f:(fun l () -> 
      Hashtbl.update liveness l ~f:(fun line ->
        (* find the current line *)
        match line with None -> failwith "successor label should be in the hashtbl"
        | Some line -> 
          let get_live_in live_out = 
            match line.defs with
              | `None -> Hash_set.union live_out line.uses
              | `One d -> 
                Hash_set.remove live_out d; Hash_set.union live_out line.uses
              | `Many ds -> 
                List.iter ds ~f:(fun d -> Hash_set.remove live_out d);
                Hash_set.union live_out line.uses
          in
          (* find the successor(s) of the current line *)
          (match line.succ with
          | `None -> {line with live_in = line.uses} (* aliasing should be ok here..... *)
          | `One succ  -> (
            let succ = Hashtbl.find_exn liveness succ in
            (* calculate live_out *)
            let live_out = Hash_set.union line.live_out succ.live_in in
            (* calculate live_in, check if it updated *)
            let live_in = get_live_in live_out in
            if Int.equal (Hash_set.length live_in) (Hash_set.length line.live_in)
            then ()
            else did_update := true;
            {line with live_in; live_out}
          )
          | `Two (succ1, succ2) -> 
            let succ1, succ2 = Hashtbl.find_exn liveness succ1, Hashtbl.find_exn liveness succ2 in
            (* calculate live_out *)
            let live_out = Hash_set.union line.live_out (Hash_set.union succ1.live_in succ2.live_in) in
            (* calculate live_in, check if it updated *)
            let live_in = get_live_in live_out in
            if Int.equal (Hash_set.length live_in) (Hash_set.length line.live_in)
            then ()
            else did_update := true;
            {line with live_in; live_out}
          )
      )
    );
  done;
  liveness
;;

let compute_liveness (p : AS.program) : Live_assem.program = 
  List.map p ~f:compute_live_outs