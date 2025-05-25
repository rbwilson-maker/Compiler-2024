(* Rachompicole L2 Compiler
 * Abstract Syntax Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * The AST resembles the parser as close as possible to maintain
 * the original program. Typechecking is done on this interface.
 *)

type typ = Int | Bool
[@@deriving equal]

(* Operator *)
type binop =
  | Plus | Minus | Times | Divided_by | Modulo
  | Less | Greater | Equal | Leq | Geq | Neq 
  | And | Or
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift 

type unop = 
  | Negative
  | Not
  | Bit_not

(* Expression *)
type exp =
  | Var of Symbol.t
  | Const of Int32.t
  | True
  | False
  | Binop of
      { op : binop
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }
  | Ternary of
      { if_exp : mexp
      ; then_exp : mexp
      ; else_exp : mexp
      }
(* Expression plus src file location *)
and mexp = exp Mark.t

type declaration =
  (* typ x; *)
  | New_var of typ * Symbol.t
  (* typ x = e; *)
  | Init of typ * Symbol.t * mexp

type simple = 
  | Declare of declaration
  (* x = e; *)
  | Assign of Symbol.t * mexp
  (* e; *)
  | Do of mexp

and control = 
  (* if (condition) {...} else {...} *)
  | If of mexp * mstm * mstm option
  (* while (condition) {...} *)
  | While of mexp * mstm
  (* for (s1;condition;s2) {...} *)
  | For of simple option * mexp * simple option * mstm
  | Return of mexp

(* Statement *)
and stm =
  (* { s1; s2; ... } *)
  | Block of stms
  (* single stm, no nested blocks *)
  | Simple of simple
  (* loops and jumps with nested blocks *)
  | Control of control
  
(* Statement plus src file location *)
and mstm = stm Mark.t

(* Block of statements *)
and stms = mstm list

type program = Symbol.t * stms

(* Print as source, with redundant parentheses *)
module Print : sig
  val pp_typ : typ -> string
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
