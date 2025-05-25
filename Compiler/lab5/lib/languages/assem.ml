(* Rachompicole L3 Compiler
 * Assembly language
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Pseudo assembly language with 3-operand
 * instructions, arbitrarily many temps, 
 * branches and jumps.
 * 
 * Branches are now limited to either single operands
 * or comparison operators like equality which need
 * to be kept around for deciding what type of jump is.
 *)

open Core

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
      ; extern : bool
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
module Print = struct
  open Print
  let pp_reg = Reg.format

  let pp_comp = function
  | Less -> "<"
  | Greater -> ">"
  | Equal -> "=="
  | Leq -> "<="
  | Geq -> ">="
  | Neq -> "!="
  ;;

  let pp_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lshift -> "<<"
  | Rshift -> ">>"
  | Bit_or -> "|"
  | Bit_and -> "&"
  | Bit_xor -> "^"
  | Comp c -> pp_comp c
  ;;

  let pp_temp t = 
    sprintf "%s" (Temp.name t)

  let pp_param (t, s) = 
    sprintf "%s{%s}" (Temp.name t) (Size.format s) 

  let rec pp_operand = function
    | Imm n -> sprintf "$%s" 
      (match n with
      | `Long n -> Int32.to_string n
      | `Quad n -> Int64.to_string n
      )
    | Temp (t, s) -> pp_temp t ^ (Size.format s)
    | Reg r -> pp_reg r Size.Quad
    | Addr addr -> pp_addr addr

  and pp_addr mem = 
      sprintf "%s(%s%s)"
      (pp_opt Int32.to_string mem.disp)
      (pp_operand mem.base)
      (match mem.index, mem.scale with
      | None, None -> ""
      | Some op, None -> 
        sprintf ", %s" (pp_operand op)
      | None, Some scale ->
        sprintf ", ,%s" (Size.format scale)
      | Some op, Some scale ->
        sprintf ", %s, %s" (pp_operand op) (Size.format scale))
  ;;
  let pp_arg (op, s) = 
    sprintf "%s{%s}" (pp_operand op) (Size.format s) 

  let pp_instr = function
    | Return -> "return"
    | Binop binop ->
      sprintf
        "%s <-- %s %s %s"
        (pp_operand binop.dest)
        (pp_operand binop.lhs)
        (pp_binop binop.op)
        (pp_operand binop.rhs)
    | Equal binop -> 
      sprintf
        "%s <-- %s ==%s %s"
        (pp_operand binop.dest)
        (pp_operand binop.lhs)
        (Size.format binop.size)
        (pp_operand binop.rhs)
    | Call f -> 
      sprintf "%s <-- %s(%s)"
      (pp_operand f.dest)
      (Symbol.name f.fn)
      (pp_list pp_arg f.params)
    | Mov mv -> sprintf "%s <-- %s" (pp_operand mv.dest) (pp_operand mv.src)
    | Mov_addr mv -> sprintf "%s <--addr %s" (pp_operand mv.dest) (pp_addr mv.src)
    | Branch b -> sprintf "if %s then %s else %s."
      (match b.cond with
      | `Single (op, _) -> pp_operand op
      | `Comparison c -> 
          sprintf "%s %s %s" 
          (pp_operand c.lhs)
          (pp_comp c.op)
          (pp_operand c.rhs))
      (Symbol.name b.if_label)
      (match b.else_label with 
      | Some x -> Symbol.name x 
      | None -> Symbol.name b.after_label)
    | Jump l -> "goto " ^ Symbol.name l
    | Label l -> "." ^ Symbol.name l ^ ":"
    | Raise Sigfpe -> "t <-- div by zero"
    | Raise Sigabrt -> "t <-- abort()"
    | Raise Sigusr -> "t <-- raise sigusr"
    | Directive dir -> dir
    | Comment comment -> sprintf "/* %s */" comment
  ;;

  let pp_fun_header f = 
    sprintf ".%s(%s):" (Symbol.name f.name) (pp_list pp_param f.spills)

  let pp_program (p : program) = "\t" ^
    pp_list ~btwn:"\n\n\t" 
    (fun f -> pp_list 
      ~btwn:"\n\t" 
      ~around:(pp_fun_header f, "")
      pp_instr 
      f.body) p
end


