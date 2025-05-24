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
signature AST =
sig
  type ident = Symbol.symbol

  datatype oper =
     PLUS
   | MINUS
   | TIMES
   | DIVIDEDBY
   | MODULO
   | NEGATIVE  (* unary minus *)

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

  (* print as source, with redundant parentheses *)
  structure Print :
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end
end
