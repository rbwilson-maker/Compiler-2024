(* Rachompicole L3 Compiler
 * Elaborated Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 *
 * Similar to the AST but with for-loops and unary ops removed,
 * only the function definitions, and a more intuitive structure than the AST.
 * The typechecker guarantees all declarations are valid.
 *)

 open Core

type param = Symbol.t

(* Boolean operators get elaborated to ternary expressions *)
type binop =
  | Plus | Minus | Times | Divided_by | Modulo
  | Less | Greater | Equal | Leq | Geq | Neq
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 

type signal = Sigfpe | Sigabrt

(* Expression *)
type exp =
  | Var of Symbol.t
  | Const of Int32.t
  | True
  | False
  | Binop of
      { op : binop
      ; lhs : exp
      ; rhs : exp
      }
  | Ternary of
      { if_exp : exp
      ; then_exp : exp
      ; else_exp : exp
      }
  | F of Symbol.t * exp list
  | Void_f of Symbol.t * exp list
  | Raise of signal

(* For loops get elaborated to while loops *)
type stm =
  (* { s1; s2; ... } *)
  | Block of stms
  (* Scopes the declaration in stms *)
  | Declare of Symbol.t * exp option
  (* x = e; *)
  | Assign of Symbol.t * exp
  (* e; *)
  | Do of exp
  (* if (condition) {...} else {...} *)
  | If of exp * stm * stm option
  (* while (condition) {...} *)
  | While of exp * stm
  | Return of exp option

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

  let rec pp_exp = function
    | Var id -> Symbol.name id
    | Const c -> Int32.to_string c
    | True -> "true"
    | False -> "false"
    | Binop binop ->
      sprintf "(%s %s %s)" (pp_exp binop.lhs) (pp_binop binop.op) (pp_exp binop.rhs)
    | Ternary t ->
      sprintf "(%s ? %s : %s)" (pp_exp t.if_exp) (pp_exp t.then_exp) (pp_exp t.else_exp)
    | F (sym, args) -> sprintf "%s(%s)" (Symbol.name sym) (pp_list pp_exp args)
    | Void_f (sym, args) -> sprintf "%s(%s)" (Symbol.name sym) (pp_list pp_exp args)
    | Raise Sigfpe -> "1/0"
    | Raise Sigabrt -> "abort()"

  let rec pp_stm stm d = 
  let tabs = n_str "  " d in
  tabs ^
  (match stm with
    | Block s -> sprintf "{\n%s%s}" (pp_stms s (d+1)) tabs
    | Return e -> 
      (match e with None -> "return;" 
      | Some e -> sprintf "return %s;" (pp_exp e))
    | If (cond, s1, s2) -> sprintf "if %s\n%s\n%s" (pp_exp cond) (pp_stm s1 (d+1))
      (match s2 with None -> "" | Some s2 -> tabs ^ "else\n" ^ (pp_stm s2 (d+1)))
    | While (cond, s1) -> sprintf "while %s %s" (pp_exp cond) (pp_stm s1 (d+1))
    | Declare (x, s) -> sprintf "decl %s%s;" (Symbol.name x)
      (match s with None -> "" | Some s -> " = "^(pp_exp s))
    | Assign (id, e) -> sprintf "%s = %s;" (Symbol.name id) (pp_exp e)
    | Do e -> sprintf "%s;" (pp_exp e)
  )
    
  and pp_stms stms d = String.concat (List.map ~f:(fun stm -> pp_stm stm d ^ "\n") stms)

  let pp_fn (fn, params, stms) = 
    sprintf "fun %s(%s) {\n%s}"
    (Symbol.name fn)
    (pp_list Symbol.name params)
    (pp_stms stms 1)
  ;;

  let pp_program (p : fndef list) = 
    pp_list ~btwn:"\n\n" pp_fn p
  ;;
  
end
 