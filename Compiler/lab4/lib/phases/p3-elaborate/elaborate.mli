(* Rachompicole L3 Compiler
 * Elaborator
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Elaboration removes for loops, unary operators, and performs additional
 * checks such as with Lshift and Rshift.
 *)

(** Elaborates the ast of a program. Takes an optional header file. When there,
    the header file is used to determine which functions are 'external' and should
    not have their name changed. *)
val elaborate : tc_data:Typechecker.output-> Elab.program * Size.structs
