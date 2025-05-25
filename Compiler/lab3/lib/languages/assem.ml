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
type signal = Sigfpe | Sigabrt
type operand =
  | Imm of Int32.t
  | Reg of Reg.t
  | Temp of Temp.t

type compop = Less | Greater | Equal | Leq | Geq | Neq

type comparison  = 
    { lhs : operand
    ; op : compop
    ; rhs : operand
    }

type operation =
  | Add | Sub | Mul | Div | Mod
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift
  | Comp of compop

type instr =
  | Return
  | Binop of
      { op : operation
      ; dest : operand
      ; lhs : operand
      ; rhs : operand
      }
  | Call of 
      { dest : operand
      ; fn : label
      (* After assem elaboration params only contains spilled args *)
      ; params : operand list
      }
  | Mov of
      { dest : operand
      ; src : operand
      }
  | Branch of 
    { if_label : label
    ; else_label : label option
    ; after_label : label
    ; cond : [`Single of operand | `Comparison of comparison]
    }
  | Jump of label
  | Label of label
  | Raise of signal
  | Directive of string
  | Comment of string

type fun_instrs = 
  { name : label
  ; body : instr list
  ; spills : Temp.t list
  ; regs_used : Reg.t list
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

  let pp_operand = function
    | Imm n -> "$" ^ Int32.to_string n
    | Temp t -> Temp.name t
    | Reg r -> pp_reg r Reg.Quad
  ;;

  let pp_instr = function
    | Return -> "return"
    | Binop binop ->
      sprintf
        "%s <-- %s %s %s"
        (pp_operand binop.dest)
        (pp_operand binop.lhs)
        (pp_binop binop.op)
        (pp_operand binop.rhs)
    | Call f -> 
      sprintf "%s <-- %s(%s)"
      (pp_operand f.dest)
      (Symbol.name f.fn)
      (pp_list pp_operand f.params)
    | Mov mv -> sprintf "%s <-- %s" (pp_operand mv.dest) (pp_operand mv.src)
    | Branch b -> sprintf "if %s then %s else %s."
      (match b.cond with
      | `Single op -> pp_operand op
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
    | Directive dir -> dir
    | Comment comment -> sprintf "/* %s */" comment
  ;;

  let pp_fun_header f = 
    sprintf ".%s(%s):" (Symbol.name f.name) (pp_list Temp.name f.spills)

  let pp_program (p : program) = "\t" ^
    pp_list ~btwn:"\n\n\t" 
    (fun f -> pp_list 
      ~btwn:"\n\t" 
      ~around:(pp_fun_header f, "")
      pp_instr 
      f.body) p
end


