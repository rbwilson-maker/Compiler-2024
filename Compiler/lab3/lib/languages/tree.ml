(* Rachompicole L3 Compiler
 * IR Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * The IR Tree turns everything into branches and jumps thus
 * removing a distinction between loops and conditionals.
 * 
 * Still modally separates pure expressions and commands.
 * Still includes ternary expressions, so this is
 * not quite in basic block form yet for program analysis.
 *)

type label = Symbol.t

type binop =
| Add | Sub | Mul
| Less | Greater | Equal | Leq | Geq | Neq
| Bit_and | Bit_or | Bit_xor | Lshift | Rshift

type effect_op =  Div | Mod

type signal = Sigfpe | Sigabrt

type exp =
 | Const of Int32.t
 | True | False
 | Temp of Temp.t
 | Binop of
     { lhs : exp
     ; op : binop
     ; rhs : exp
     }

and stm =
  | Move of
     { dest : Temp.t
     ; src : exp
     }
  | Effect_move of 
    { dest : Temp.t
    ; lhs : exp
    ; op : effect_op
    ; rhs : exp 
    }
  | Fn_move of
    { dest : Temp.t
    ; fn : Symbol.t
    ; args : exp list
    }
  | Do_fn of
    { fn : label
    ; args : exp list
    }
  | Branch of 
    { condition : exp
    ; if_label : label
    ; else_label : label option
    ; after_label : label
    }
  | Label of label
  | Goto of label
  | Return of exp option
  | Raise of signal

type param = Temp.t
type fun_stms = Symbol.t * param list * stm list

type program = fun_stms list

module Print = struct
  open Print
  let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
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

  let pp_effectop = function
  | Div -> "/"
  | Mod -> "%"
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
  ;;

  let pp_stm = function
    | Move mv -> "\t" ^ Temp.name mv.dest ^ "  <--  " ^ pp_exp mv.src
    | Effect_move mv -> 
      Printf.sprintf
        "\t%s <-- (%s %s %s)"
        (Temp.name mv.dest)
        (pp_exp mv.lhs)
        (pp_effectop mv.op)
        (pp_exp mv.rhs)
    | Fn_move mv ->
      Printf.sprintf
        "\t%s <-- %s(%s)"
        (Temp.name mv.dest)
        (Symbol.name mv.fn)
        (pp_list pp_exp mv.args)
    | Do_fn f ->
      Printf.sprintf
        "\t%s(%s);" (Symbol.name f.fn) (pp_list pp_exp f.args)
    | Return e -> "\treturn " ^ (
      match e with None -> "" | Some e -> pp_exp e)
    | Branch branch ->
      Printf.sprintf "\tif %s then %s else %s"
        (pp_exp branch.condition)
        (Symbol.name branch.if_label)
        (match branch.else_label with
        | Some l -> (Symbol.name l)
        | None -> (Symbol.name branch.after_label))
    | Goto label -> Printf.sprintf "\tgoto %s" (Symbol.name label)
    | Label label -> "." ^ Symbol.name label ^ ":"
    | Raise Sigfpe -> "raise sigfpe"
    | Raise Sigabrt -> "raise sigabrt"
  ;;
  let pp_fun_stms (sym, args, body) = 
    Printf.sprintf "%s(%s):\n%s"
    (Symbol.name sym)
    (pp_list Temp.name args)
    (pp_list ~btwn:"\n" pp_stm body)
  ;;

  let pp_program = pp_list ~btwn:"\n\n" pp_fun_stms
end
