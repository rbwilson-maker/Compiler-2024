(* L2 Compiler
 * Elaborated ASTs
 * Authors: Rachel Wilson and Nicole Fang
 * 
 *)

 open Core

 (* Notice that we use subexpressions and statements that are marked.
  * (That is, the subexpressions are of type exp Mark.t, not just
  * type exp.) This means that source code location (a src_span) is
  * associated with the subexpression. Currently, the typechecker uses
  * this source location to print more helpful error messages.
  *
  * It's the parser and lexer's job to associate src_span locations with each
  * ast. It's instructive, but not necessary, to closely read the source code
  * for c0_parser.mly, c0_lexer.mll, and parse.ml to get a good idea of how
  * src_spans are created.
  *)
  type typ = Int | Bool
 
  (* Boolean operators get elaborated to ternary expressions *)
  type binop =
    | Plus | Minus | Times | Divided_by | Modulo
    | Less | Greater | Equal | Leq | Geq | Neq
    | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 
  
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
  
  (* For loops get elaborated to while loops *)
  type stm =
    (* { s1; s2; ... } *)
    | Block of stms
    (* Scopes the declaration in stms *)
    | Declare of typ * Symbol.t * exp option
    (* x = e; *)
    | Assign of Symbol.t * exp
    (* e; *)
    | Do of exp
    (* if (condition) {...} else {...} *)
    | If of exp * stm * stm option
    (* while (condition) {...} *)
    | While of exp * stm
    | Return of exp
  
  (* Block of statements *)
  and stms = stm list
  
  type program = Symbol.t * stms
 
 module Print = struct
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
 
   let pp_typ = function
     | Int -> "int"
     | Bool -> "bool"
 
   let rec pp_stm = function
     | Block s -> sprintf "{\n%s}" (pp_stms s)
     | Return e -> sprintf "return %s;" (pp_exp e)
     | If (cond, s1, s2) -> sprintf "(if %s %s %s)" (pp_exp cond) (pp_stm s1)
       (match s2 with None -> "" | Some s2 -> "else " ^ (pp_stm s2))
     | While (cond, s1) -> sprintf "while %s %s" (pp_exp cond) (pp_stm s1)
     | Declare (t, x, s) -> sprintf "%s %s%s;" (pp_typ t) (Symbol.name x)
       (match s with None -> "" | Some s -> " = "^(pp_exp s))
     | Assign (id, e) -> sprintf "%s = %s;" (Symbol.name id) (pp_exp e)
     | Do e -> sprintf "%s;" (pp_exp e)
     
   and pp_stms stms = String.concat (List.map ~f:(fun stm -> pp_stm stm ^ "\n") stms)
 
   let pp_program (fn, stms) = "int " ^ Symbol.name fn ^ " {\n" ^ pp_stms stms ^ "}"
 end
 