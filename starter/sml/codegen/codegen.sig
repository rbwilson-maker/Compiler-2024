(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *)
signature CODEGEN =
sig
  val codegen : Tree.stm list -> Assem.instr list
end
