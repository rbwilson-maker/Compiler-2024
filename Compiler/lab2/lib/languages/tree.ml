(* L2 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)

type label = Symbol.t

type binop =
| Add | Sub | Mul | Div | Mod
| Less | Greater | Equal | Leq | Geq | Neq
| Bit_and | Bit_or | Bit_xor | Lshift | Rshift

type exp =
 | Const of Int32.t
 | True | False
 | Temp of Temp.t
 | Binop of
     { lhs : exp
     ; op : binop
     ; rhs : exp
     }
 | Ternary of
    { if_exp : exp
    ; then_exp : exp
    ; else_exp : exp
    }

and stm =
 | Move of
     { dest : Temp.t
     ; src : exp
     }
 | Branch of 
     { condition : exp
     ; if_label : label
     ; else_label : label option
     ; after_label : label
     }
 | Label of label
 | Goto of label
 | Return of exp

type program = stm list

module Print = struct
  let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Less -> "<"
    | Greater -> ">"
    | Equal -> "=="
    | Leq -> "<="
    | Geq -> ">="
    | Neq -> "!="
    | Lshift -> "<<"
    | Rshift -> ">>"
    | Bit_or -> "|"
    | Bit_and -> "&"
    | Bit_xor -> "^"
  ;;

  let rec pp_exp = function
    | Const x -> Int32.to_string x
    | Temp t -> Temp.name t
    | True -> "true" | False -> "false"
    | Binop binop ->
      Printf.sprintf
        "(%s %s %s)"
        (pp_exp binop.lhs)
        (pp_binop binop.op)
        (pp_exp binop.rhs)
    | Ternary t ->
      Printf.sprintf
        "(%s) ? (%s) : (%s)"
        (pp_exp t.if_exp)
        (pp_exp t.then_exp)
        (pp_exp t.else_exp)
    
  ;;

  let pp_stm = function
    | Move mv -> "\t" ^ Temp.name mv.dest ^ "  <--  " ^ pp_exp mv.src
    | Return e -> "\treturn " ^ pp_exp e
    | Branch branch ->
      Printf.sprintf "\tif %s then %s else %s"
        (pp_exp branch.condition)
        (Symbol.name branch.if_label)
        (match branch.else_label with
        | Some l -> (Symbol.name l)
        | None -> (Symbol.name branch.after_label))
    | Goto label -> Printf.sprintf "\tgoto %s" (Symbol.name label)
    | Label label -> "." ^ Symbol.name label ^ ":"
  ;;
  let rec pp_program = function
    | [] -> ""
    | stm :: stms -> pp_stm stm ^ "\n" ^ pp_program stms
  ;;
end
