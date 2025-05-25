(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Implements a "convenient munch" algorithm.
 * It munches ternary expressions to keep around
 * comparison operators. The whole thing is
 * implemented with a reversed accumulator.
 *)

(** Translate the IR tree to 3-address abstract assembly. *)
val codegen : unsafe:bool -> Size.structs -> Tree.program -> Assem.program
