open Core
module AS = Assem

module Dest = struct
  type t = Reg of Reg.t | Temp of (Temp.t * Size.t)
  [@@deriving compare, equal, hash, sexp]
  let to_string (d : t) = 
    match d with
    | Reg r -> Reg.format r Size.Quad
    | Temp (t, s) -> Temp.name t ^ (Size.format s)
end

let op_to_dest (op : AS.operand) : Dest.t option = 
  match op with
  | AS.Imm _ -> None
  | AS.Reg r -> Some (Dest.Reg r)
  | AS.Temp t -> Some (Dest.Temp t)
  | AS.Addr _ -> None
;;

let dest_to_op (d : Dest.t) : AS.operand =
  match d with
  | Dest.Reg r -> AS.Reg r
  | Dest.Temp t -> AS.Temp t
;;

type loc = Symbol.t * Symbol.t
type 'a succ = None | One of 'a | Two of 'a * 'a
let succ_map f s = match s with
  | None -> None
  | One a -> One (f a)
  | Two (a1, a2) -> Two (f a1, f a2)
;;
type line = {
  uses : Dest.t Hash_set.t;
  defs : [ `Many of Dest.t list | `None | `One of Dest.t ];
  succ : loc succ;
  instr : AS.instr;
  tail_call : bool
}

type basic_block = {
  lines : (Symbol.t, line) Hashtbl.t;
  succ : Symbol.t succ;
}
type fun_lines = (loc * AS.instr) list
type fun_cfg = {
  graph : (Symbol.t, basic_block) Hashtbl.t;
  root : loc;
  body : fun_lines;
  spills : (Temp.t * Size.t) list;
  name : Symbol.t;
}
type cfgs = fun_cfg list

let update_uses_defs (line : AS.instr) (uses : Dest.t Hash_set.t) = 
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
  in match line with
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
;;

let make_fun_cfg (f : AS.fun_instrs) : fun_cfg = 
  (* label each line with a label and its block's label *)
  let rev_body, _ = 
  List.foldi f.body ~init:([], f.name) ~f:(fun i (acc, block) instr ->
    match instr with
    | AS.Label l -> ((l, l), instr)::acc, l
    | _ ->
      let line_sym = Symbol.symbol ("line" ^ Int.to_string i) in
      ((block, line_sym), instr)::acc, block
  ) in

  (* Initialize the graphs *)
  let blocks_table = Hashtbl.create (module Symbol) in
  let current_basic_block = ref (Hashtbl.create (module Symbol)) in
  let next_basic_block = ref (None) in
  let line_is_block_end = ref true in
  let default_next_line = ref (None) in
  let prev_line_is_tail_call = ref true in

  (* Traverse in post-order to construct the graph*)
  List.iter rev_body ~f:(fun ((b, curr_sym), line) ->
    let tail_call = (
      match line with
      | Call _ -> !prev_line_is_tail_call
      | _ -> false
    ) in
    let succ = (
      match line with
      | Return | Raise _ -> prev_line_is_tail_call := true; None
      | Jump l -> One (b, l)
      | Branch {if_label; else_label; after_label; _} -> 
        prev_line_is_tail_call := false;
        (match else_label with
        | None -> Two ((b, if_label), (b, after_label))
        | Some else_label -> Two ((b, if_label), (b, else_label))
        )
      (* propagate the tail call through *)
      | Label _ -> !default_next_line
      (* | Mov mv when (AS.equal_operand mv.dest (Reg RAX)) -> !default_next_line *)
      (* don't propagate the tail call *)
      | _ -> 
        prev_line_is_tail_call := false; 
        !default_next_line
    ) in
    default_next_line := One (b, curr_sym);
    if !line_is_block_end then next_basic_block := (succ_map fst succ);
    
    (* update the current block with the current line *)
    let uses = Hash_set.create (module Dest) in
    let defs = update_uses_defs line uses in
    Hashtbl.add_exn !current_basic_block 
      ~key:curr_sym 
      ~data:{uses; defs; succ; instr = line; tail_call}
    ;

    (* Finish off the basic block if the current line is a label *)
    (match line with
    | Label _ -> (
      Hashtbl.add_exn blocks_table 
        ~key:curr_sym
        ~data:{lines = !current_basic_block; succ = !next_basic_block}
      ; 
      line_is_block_end := true;
    )
    | _ -> line_is_block_end := false;
    );
    
  );
  (* Add the last basic block (the start of the function) *)
  let (root_block, _) as root = 
    (match !default_next_line with 
      | One (_, l) -> (f.name, l)
      | _ -> failwith "should only be one first line"
    ) in
  Hashtbl.add_exn blocks_table
    ~key:(root_block)
    ~data:{lines = !current_basic_block; succ = !next_basic_block}
  ;

  { graph = blocks_table; 
    root; 
    body = List.rev rev_body; 
    spills = f.spills;
    name = f.name
  }
;;

let make_cfgs (p : AS.program) : cfgs = 
  List.map p ~f:make_fun_cfg
;;

let get_line cfg (block, line) = 
  Hashtbl.find_exn (Hashtbl.find_exn cfg.graph block).lines line 
;;

let pp_fun_cfg (cfg : fun_cfg) : string =
  let str = Symbol.name in
  let pp_instr = AS.Print.pp_instr in
  let pp_succ s = (
    match s with
    | None -> "None"
    | One l -> str l
    | Two (l1, l2) -> sprintf "%s, %s" (str l1) (str l2)
  ) in
  let rows, _ = (
    List.fold cfg.body ~init:([], fst cfg.root) ~f:(fun (acc, prev_block) ((block, line), instr) ->
      let line_info = get_line cfg (block, line) in
      match instr with
      | Label _ -> (* new block *)
        let b = Hashtbl.find_exn cfg.graph prev_block in
        [str block; str line; pp_instr instr; ""] ::
        ["block successor: "; pp_succ b.succ; ""; "\n"] :: acc, block
      | _ -> [str block; str line; pp_instr instr; Bool.to_string (line_info.tail_call)]::acc, prev_block
    )
  ) in
  Print.pp_grid [25;25;45;15] ["block";"line"; "lines";"tail_call"] (List.rev rows)
;;

let pp_cfgs (cfgs : cfgs) : string = 
  Print.pp_list ~btwn:"\n" pp_fun_cfg cfgs
;;

  



