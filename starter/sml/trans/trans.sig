(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick ;) <tbrick@andrew.cmu.edu>
 *)
signature TRANS =
sig
  (* translate abstract syntax tree to IR tree *)
  val translate : Ast.program -> Tree.stm list
end
