open Core
module AS = Assem

module Dest :
  sig
    type t = Reg of Reg.t | Temp of (Temp.t * Size.t)
    [@@deriving compare, equal, hash, sexp]
    
    val to_string : t -> string
  end
val op_to_dest : AS.operand -> Dest.t option
val dest_to_op : Dest.t -> AS.operand

type loc = Symbol.t * Symbol.t
type 'a succ = None | One of 'a | Two of 'a * 'a
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

(** Constructs a control flow graph for each function in a program *)
val make_cfgs : AS.program -> cfgs

val get_line : fun_cfg -> loc -> line

val pp_fun_cfg : fun_cfg -> string
val pp_cfgs : cfgs -> string
