(* Rachompicole L3 Compiler
 * Assembly language
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 compiler
 *
 * Pseudo assembly language with 3-operand
 * instructions, arbitrarily many temps, 
 * branches and jumps.
 * 
 * Branches are now limited to either single operands
 * or comparison operators like equality which need
 * to be kept around for deciding what type of jump is.
 *)

type label = Symbol.t
 
type operand =
  | Imm of [`Long of Int32.t | `Quad of Int64.t]
  | Reg of Reg.t
  | Temp of (Temp.t * Size.t)
  | Addr of addr

(* Addr represents the addressing mode you will see in assembly.
 * Its interpretation depends on how it is used. It can either be
 * an address calculation or a memory dereference. 
 * 
 * An addr operand inside a addr should be converted to an lea, 
 * since we need to calculate the address, not access the memory.
 *)
and addr =
  (* D(%RB, %RI, S) *)
  { disp : Int32.t option
  ; base : operand
  ; index : operand option
  ; scale : Size.t option
  }
    
[@@deriving equal]

type signal = Sigfpe | Sigabrt | Sigusr
type compop = Less | Greater | Leq | Geq | Equal | Neq
type comparison  = 
  { lhs : operand
  ; op : compop
  ; rhs : operand
  ; size : Size.t
  }

type operation =
  | Add | Sub | Mul | Div | Mod
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift
  | Comp of compop

type instr =
  | Return
  (* dest <- lhs op rhs *)
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | Equal of
      { dest : operand
      ; lhs : operand
      ; rhs : operand
      ; op : [`Eq | `Neq]
      ; size : Size.t
      }
  | Call of 
      { dest : operand
      ; size : Size.t option (* None for void returns *)
      ; fn : label
      (* After assem elaboration params only contains spilled args *)
      ; params : (operand * Size.t) list
      }
  (* dest <- src *)
  | Mov of
      { dest : operand
      ; size : Size.t
      ; src : operand
      }
  | Mov_addr of 
      { dest : operand
      ; src : addr
      }
  | Branch of 
      { if_label : label
      ; else_label : label option
      ; after_label : label
      (* The condition is either a single boolean operand
         or a comparison expression *)
      ; cond : [`Single of (operand * Size.t) | `Comparison of comparison]
      }
  | Jump of label
  | Label of label
  | Raise of signal
  (* Assembly directive *)
  | Directive of string
  (* Human-friendly comment *)
  | Comment of string

type fun_instrs = 
  { name : label
  ; body : instr list
  ; spills : (Temp.t * Size.t) list (* includes all args until elaboration *)
  ; regs_used : Reg.t list (* which regs need to be caller/callee saved *)
  }
type program = fun_instrs list

module Print : sig
  val pp_operand : operand -> string
  val pp_instr : instr -> string
  val pp_fun_header : fun_instrs -> string
  val pp_program : program -> string
end
