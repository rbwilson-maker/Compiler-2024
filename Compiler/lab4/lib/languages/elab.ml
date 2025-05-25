(* Rachompicole L3 Compiler
 * Elaborated Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Similar to the AST but with for-loops and unary ops removed,
 * only the function definitions, and a more intuitive structure than the AST.
 * The typechecker guarantees all declarations are valid.
 *)

(* Rachompicole L3 Compiler
 * Elaborated Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Similar to the AST but with for-loops and unary ops removed
 * and a more intuitive structure than the AST.
 *)
open Core

type field = Symbol.t * Symbol.t
type big_size = Size.data_size
type small_size = Size.t
type param = Symbol.t * small_size

(* Long operators *)
type binop =
  | Plus | Minus | Times | Divided_by | Modulo
  | Less | Greater | Equal | Leq | Geq | Neq 
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 

type asnop = 
  | Plus_eq | Minus_eq | Times_eq | Div_eq | Mod_eq
  | Bit_and_eq | Bit_or_eq | Bit_xor_eq | Lshift_eq | Rshift_eq

type signal = Sigfpe | Sigabrt

type exp =
  | Var of Symbol.t * Size.t
  | Const of Int32.t (* implicit size Long *)
  | True | False (* implicit size Long (for now) *)
  | Binop of
      { op : binop
      ; lhs : exp
      ; rhs : exp
      ; op_size : small_size
      }
  | Ternary of
      { if_exp : exp
      ; then_exp : exp
      ; else_exp : exp
      ; size : small_size
      }
  | F of 
      { name : Symbol.t
      ; params : (exp * Size.t) list
      ; ret_size : small_size option (* None for void fns *)
      }
  | Raise of signal
  | Mem of (mem_exp * Size.t)
  | Null
  | Alloc of big_size
  | Alloc_array of big_size * exp

and mem_exp = 
  | Get_field of 
      { strct : mem_exp (* structs only exist in memory *)
      ; field : field
      }
  | Deref of exp
  | Index of
      { array : exp
      ; index : exp
      ; elem_size : big_size
      }

type lval =
  | Var of Symbol.t
  | Field of 
    { strct : lval
    ; field : field
    }
  | Deref of lval
  | Index of 
    { array : lval
    ; index : exp
    ; elem_size : big_size
    }

type dest = lval * small_size

(* Statement *)
type stm =
  (* { s1; s2; ... } *)
  | Block of stms
  | Declare of 
      { lhs : Symbol.t * small_size
      ; rhs : exp option
      }
  (* x = e; *)
  | Assign of dest * exp
  | Asnop of 
      { lhs : dest
      ; op : asnop
      ; rhs : exp
      }
  (* e; *)
  | Do of exp
  (* if (condition) {...} else {...} *)
  | If of exp * stm * stm option
  (* while (condition) {...} *)
  | While of exp * stm
  | Return of (exp * small_size) option

(* Block of statements *)
and stms = stm list

type fndef = Symbol.t * param list * stms

type program = fndef list

module Print = struct
open Print
  let pp_binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divided_by -> "/"
    | Modulo -> "%"
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

  let pp_asnop = function
    | Plus_eq -> "+="
    | Minus_eq -> "-="
    | Times_eq -> "*="
    | Div_eq -> "/="
    | Mod_eq -> "%="
    | Bit_and_eq -> "&="
    | Bit_or_eq -> "|="
    | Bit_xor_eq -> "^="
    | Lshift_eq -> "<<="
    | Rshift_eq -> ">>="
  ;;

  let pp_param = fun (p, s) ->
    sprintf "{%s}%s" (Size.format s) (Symbol.name p) 

  let pp_field = fun (s, f) ->
    sprintf "{%s}%s" (Symbol.name s) (Symbol.name f)

  let rec pp_exp : exp -> string = function
    | Var (id, _) -> Symbol.name id
    | Const c -> Int32.to_string c
    | True -> "true"
    | False -> "false"
    | Binop binop ->
      sprintf "(%s %s%s %s)" (pp_exp binop.lhs) (pp_binop binop.op) (Size.format binop.op_size) (pp_exp binop.rhs)
    | Ternary t ->
      sprintf "{%s}(%s ? %s : %s)" (Size.format t.size) (pp_exp t.if_exp) (pp_exp t.then_exp) (pp_exp t.else_exp)
    | F f -> sprintf "{%s}%s(%s)" (pp_opt Size.format f.ret_size) (Symbol.name f.name) (pp_list (fun (e, _) -> pp_exp e) f.params)
    | Raise Sigfpe -> "1/0"
    | Raise Sigabrt -> "abort()"
    | Null -> "NULL"
    | Alloc s -> sprintf "alloc(%s)" (Size.format_data s)
    | Alloc_array (s, l) -> sprintf "alloc_array(%s, %s)" (Size.format_data s) (pp_exp l)
    | Mem (m, s) -> sprintf "{%s}%s" (Size.format s) (pp_mem m)
  
  and pp_mem : mem_exp -> string = function
    | Get_field {strct; field} -> sprintf "%s.%s" (pp_mem strct) (pp_field field)
    | Deref e -> sprintf "*%s" (pp_exp e)
    | Index {array; index; elem_size} -> sprintf "%s{%s}[%s]" (pp_exp array) (Size.format_data elem_size) (pp_exp index) 

  let rec pp_lval : lval -> string = function
    | Var (id) -> (Symbol.name id)
    | Field {strct; field} ->  sprintf "%s.%s" (pp_lval strct) (pp_field field)
    | Deref lval -> sprintf "*%s" (pp_lval lval)
    | Index {array; index; elem_size} -> 
      sprintf "%s{%s}[%s]" (pp_lval array) (Size.format_data elem_size) (pp_exp index)
  let pp_dest (d, s : dest) = 
    sprintf "{%s} %s" (Size.format s) (pp_lval d) 
  ;;
  
  let rec pp_stm stm d = 
    let tabs = n_str "  " d in
    tabs ^
    (match stm with
      | Block s -> sprintf "{\n%s%s}" (pp_stms s (d+1)) tabs
      | Return e -> pp_opt ~default:"return;" (fun (e, _s) -> sprintf "return %s;" (pp_exp e)) e
      | If (cond, s1, s2) -> sprintf "if %s\n%s\n%s" (pp_exp cond) (pp_stm s1 (d+1))
        (pp_opt (fun s2 -> tabs ^ "else\n" ^ (pp_stm s2 (d+1))) s2)
      | While (cond, s1) -> sprintf "while %s %s" (pp_exp cond) (pp_stm s1 (d+1))
      | Declare {lhs = (x, s); rhs} -> sprintf "decl {%s}%s%s;" 
        (Size.format s) (Symbol.name x) (pp_opt (fun s -> " = "^(pp_exp s)) rhs)
      | Assign (lhs, e) -> sprintf "%s = %s;" (pp_dest lhs) (pp_exp e)
      | Do e -> sprintf "%s;" (pp_exp e)
      | Asnop {lhs; op; rhs} -> sprintf "%s %s %s;" (pp_dest lhs) (pp_asnop op) (pp_exp rhs)
    ) 
  and pp_stms stms d = String.concat (List.map ~f:(fun stm -> pp_stm stm d ^ "\n") stms)
  ;;

  let pp_fn (fn, params, stms) = 
    sprintf "fun %s(%s) {\n%s}"
    (Symbol.name fn)
    (pp_list pp_param params)
    (pp_stms stms 1)
  ;;

  let pp_program (p : fndef list) = 
    pp_list ~btwn:"\n\n" pp_fn p
  ;;
  
end
 