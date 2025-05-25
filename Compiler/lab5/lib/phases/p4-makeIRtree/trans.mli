(* Rachompicole L3 Compiler
 * Elaborated AST -> IR Translator
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * Uses a reversed accumulator to generate 
 * pseudo instructions using branching, while still
 * maintaining separation between pure and effectful expressions.
 * There is no more nesting of statements.
 *)

(** Translate an abstract syntax tree to IR tree *)
val translate : unsafe:bool -> Elab.program -> Tree.program
