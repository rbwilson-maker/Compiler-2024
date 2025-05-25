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
open Core

type label = Symbol.t
type field = Symbol.t * Symbol.t
type temp = Temp.t * Size.t
type param = Temp.t * Size.t

type binop =
  | Add | Sub | Mul 
  | Less | Greater | Leq | Geq 
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift
type equal_op = Equal | Neq
type effect_op = Div | Mod
type signal = Sigfpe | Sigabrt

(* This is something that is represented by a pure address.
* It's address value should be statically calculable .
*
* This definition of an address ensures safety of 
* pointer dereferences and array accesses can be checked.
* Unsafe addresses would include array indexes and 
* derefs.
*)
type addr = 
  (* The value of a struct is represented by its address *)
  | Field_addr of 
    { strct : addr
    ; field : field
    }
  (* This is the address base case *)
  | Addr_temp of Temp.t
  | Unsafe of unsafe_addr

and unsafe_addr = 
  (* array_addr is an address calculation *)
  | Array_addr of
    { array : exp
    ; index : exp
    ; elem_size : Size.data_size
    }
  (* deref addresses are very special
   * translate will not generate them naturally, 
   * since they are sensitive to order of evaluation (the contents of a pointer may change)
   *
   * thus in optimization, we may find instructions such as:
   * %t8q <-- M[addr%t1.field]
   * %t9q <-- M[addr%t8.arr]
   * which are back to back and can be converted to
   * t8q <-- M[*t1.field.arr] for simplicity
   *)
  | Deref of addr

(* exp is truly pure *)
and exp =
  | Const of Int32.t
  | True | False
  | Temp of temp
  | Binop of 
    { lhs : exp
    ; op : binop
    ; rhs : exp
    }
  | Equality of
    { lhs : exp
    ; rhs : exp
    ; op : equal_op
    ; size : Size.t
    }
  | Addr of addr
  | Null

(* Effectful commands *)
type stm =
  (* dest <-- src*)
  | Move of
    { dest : temp
    ; src : exp
    }
  (* dest <-- lhs div/mod rhs *)
  | Effect_move of 
    { dest : temp
    ; lhs : exp
    ; op : effect_op
    ; rhs : exp 
    }
  (* dest <-- fn(args) *)
  | Fn_move of
    { dest : temp option (* None for void fns *)
    ; fn : label
    ; args : (exp * Size.t) list
    }
  (* dest <-- M[src] *)
  | Read_mem of
    { dest : temp
    ; src : addr
    }
  (* M[dest] <--size src *)
  | Write_mem of 
    { dest : addr
    ; size : Size.t
    ; src : exp 
    }
  | Check_null of exp
  (* dest <-- array{elem_size}[index] *)
  | Array_addr of
    { dest : Temp.t
    ; array : exp
    ; index : exp
    ; elem_size : Size.data_size
    }
  (* dest <-- alloc(elem_size, array_len) *)
  | Alloc of 
    { dest : temp
    ; elem_size : Size.data_size
    ; array_len : exp option (* length, None if not an array *)
    }
  | Branch of 
    { condition : exp
    ; if_label : label
    ; else_label : label option
    ; after_label : label
    }
  | Label of label
  | Goto of label
  | Return of (exp * Size.t) option
  | Raise of signal

type fun_stms = label * param list * stm list
type program = fun_stms list

module Print = struct
  open Print
  let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Less -> "<"
    | Greater -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | Lshift -> "<<"
    | Rshift -> ">>"
    | Bit_or -> "|"
    | Bit_and -> "&"
    | Bit_xor -> "^"
  ;;

  let pp_equalop = function
  | Equal -> "=="
  | Neq -> "!="

  let pp_effectop = function
    | Div -> "/"
    | Mod -> "%"
  ;;

  let pp_temp (t, s) = 
    sprintf "%s%s" (Temp.name t) (Size.format s)
  ;;

  let pp_param (t, _s) = 
    sprintf "%s" (Temp.name t)
  ;;

  let pp_field (_s, f) =
    sprintf "%s" (Symbol.name f)
  ;;

  let rec pp_addr m = match m with
    | Field_addr f -> sprintf "%s.%s" (pp_addr f.strct) (pp_field f.field)
    | Addr_temp t -> "addr" ^  Temp.name t
    | Unsafe u -> pp_unsafe u
  
  and pp_unsafe u = match u with
    | Array_addr a ->
      sprintf "%s[%s]"
      (pp_exp a.array)
      (pp_exp a.index)
    | Deref a -> 
      sprintf "*%s" (pp_addr a)

  and pp_exp e = match e with
    | Const x -> Int32.to_string x
    | Temp (t, s) -> pp_temp (t, s)
    | True -> "true" | False -> "false"
    | Binop binop ->
      sprintf
        "(%s %s %s)"
        (pp_exp binop.lhs)
        (pp_binop binop.op)
        (pp_exp binop.rhs)
    | Equality eq ->
      sprintf 
        "(%s %s%s %s)"
        (pp_exp eq.lhs)
        (pp_equalop eq.op)
        (Size.format eq.size)
        (pp_exp eq.rhs)
    | Addr addr -> pp_addr addr
    | Null -> "NULL"
  ;;

  let pp_stm = function
    | Move mv -> 
      sprintf "\t%s <-- %s" 
      (pp_temp mv.dest) 
      (pp_exp mv.src)
    | Effect_move mv -> 
      sprintf
        "\t%s <-- (%s %s %s)"
        (pp_temp mv.dest)
        (pp_exp mv.lhs)
        (pp_effectop mv.op)
        (pp_exp mv.rhs)
    | Fn_move mv ->
      sprintf
        "\t%s <-- %s(%s)"
        (pp_opt ~default:"%rax" pp_temp mv.dest)
        (Symbol.name mv.fn)
        (pp_list (fun (e, _) -> 
          sprintf "%s" (pp_exp e)
          ) mv.args)
    | Check_null e ->
      sprintf "\tNull check %s" (pp_exp e)
    | Read_mem mv ->
      sprintf "\t%s <-- M[%s]"
      (pp_temp mv.dest)
      (pp_addr mv.src)
    | Write_mem mv ->
      sprintf "\tM[%s] <--%s %s" 
      (pp_addr mv.dest)
      (Size.format mv.size)
      (pp_exp mv.src)
    | Array_addr a ->
      sprintf "\t%s <--addr %s[%s]"
      (Temp.name a.dest)
      (pp_exp a.array)
      (pp_exp a.index)
    | Alloc a ->
      sprintf "\t%s <-- alloc%s" (pp_temp a.dest)
      (match a.array_len with 
      | None -> sprintf "(%s)" (Size.format_data a.elem_size)
      | Some l -> sprintf "_array(%s, %s)" (Size.format_data a.elem_size) (pp_exp l)
      )
    | Return e -> "\treturn " ^ (
      match e with None -> "" | Some (e, s) -> 
        sprintf "{%s}%s" (Size.format s) (pp_exp e))
    | Branch branch ->
      sprintf "\tif %s then %s else %s"
        (pp_exp branch.condition)
        (Symbol.name branch.if_label)
        (match branch.else_label with
        | Some l -> (Symbol.name l)
        | None -> (Symbol.name branch.after_label))
    | Goto label -> sprintf "\tgoto %s" (Symbol.name label)
    | Label label -> "." ^ Symbol.name label ^ ":"
    | Raise Sigfpe -> "\traise sigfpe"
    | Raise Sigabrt -> "\traise sigabrt"
  ;;
  let pp_fun_stms (sym, args, body) = 
    sprintf "%s(%s):\n%s"
    (Symbol.name sym)
    (pp_list pp_param args)
    (pp_list ~btwn:"\n" pp_stm body)
  ;;

  let pp_program = pp_list ~btwn:"\n\n" pp_fun_stms
end
 