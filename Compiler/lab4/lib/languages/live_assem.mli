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
module Dest : sig
    type t = Reg of Reg.t | Temp of (Temp.t * Size.t)
    [@@deriving compare, equal, hash, sexp]
    
    val to_string : t -> string
end

val op_to_dest : AS.operand -> Dest.t option
val dest_to_op : Dest.t -> AS.operand

type line = 
  { uses : Dest.t Hash_set.t
   (* a call considers all caller-saved regs as "defined" since they might be changed *)
  ; defs : [`None | `One of Dest.t | `Many of Dest.t list]
  ; succ : [`None | `One of Symbol.t | `Two of Symbol.t * Symbol.t]
  ; live_in : Dest.t Hash_set.t
  ; live_out : Dest.t Hash_set.t
  ; instr : AS.instr
  ; num : int
  }

type fun_lines = (Symbol.t, line) Hashtbl.t
type program = fun_lines list

module Print : sig 
  val pp_line : Symbol.t -> line -> string 
  val pp_fun_lines : fun_lines -> string 
  val pp_dest_table : (Dest.t, 'a) Hashtbl.t -> ('a -> string) -> string
end
