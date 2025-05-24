(* L1 Compiler
 * Abstract Syntax Trees
 * Author: Alex Vaynberg
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Modified: Taegyun Kim <taegyunk@cmu.edu> Fall 2014
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *
 * Consider using smlnj's pretty printing library --
 * you might find it useful to deal with indentation, spacing, etc.
 * This is especially useful for large programs when string concatenation
 * may get very slow.
 *)
structure Ast :> AST =
struct
  type ident = Symbol.symbol

  datatype oper =
     PLUS
   | MINUS
   | TIMES
   | DIVIDEDBY
   | MODULO
   | NEGATIVE			(* unary minus *)

  datatype exp =
     Var of ident
   | ConstExp of Word32.word
   | OpExp of oper * exp list
   | Marked of exp Mark.marked
  and stm =
     Decl of ident
   | DeclAssn of ident * exp
   | Assign of ident * exp
   | Return of exp
   | Markeds of stm Mark.marked

  type program = stm list

  (* print programs and expressions in source form
   * using redundant parentheses to clarify precedence
   *)
  structure Print =
  struct
    fun pp_ident id = Symbol.name id

    val pp_oper =
     fn PLUS      => "+"
      | MINUS     => "-"
      | TIMES     => "*"
      | DIVIDEDBY => "/"
      | MODULO    => "%"
      | NEGATIVE  => "-"

    fun pp_exp exp =
      case exp of
        Var id                => pp_ident id
      | ConstExp c            => Word32Signed.toString c
      | OpExp (oper, [e])     => pp_oper oper ^ "(" ^ pp_exp e ^ ")"
      | OpExp (oper, [e1,e2]) =>
          "(" ^ pp_exp e1 ^ " " ^ pp_oper oper
          ^ " " ^ pp_exp e2 ^ ")"
      | OpExp _               => "bad expresison"
      | Marked marked_exp     => pp_exp (Mark.data marked_exp)

    fun pp_decl d = "int " ^ pp_ident d

    fun pp_stm stm =
      case stm of
        Decl id             => pp_decl id ^ ";"
      | DeclAssn(id, e)     => pp_decl id ^ " = " ^ pp_exp e ^ ";"
      | Assign (id,e)       => pp_ident id ^ " = " ^ pp_exp e ^ ";"
      | Return e            => "return " ^ pp_exp e ^ ";"
      | Markeds marked_stm  => pp_stm (Mark.data marked_stm)

    fun pp_stms [] = ""
      | pp_stms (s::ss) = pp_stm s ^ "\n" ^ pp_stms ss

    fun pp_program stms = "int main() {\n" ^ pp_stms stms ^ "}"
  end
end
