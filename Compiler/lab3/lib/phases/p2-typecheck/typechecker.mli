(* Rachompicole L3 Compiler
 * TypeChecker
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Typechecker that checks the following properties:
 *  (1) All variables have the correct type and functions return the right type
 *  (1) If a variable is initialized, it has previously been declared.
 *  (2) If a variable is used, it has previously been initialized.
 *  (3) All control paths end in a return.
 *  (4) Correct types of statements are used when not otherwise checked by the parser
 *)

(** Prints error message and raises ErrorMsg.error if typecheck error found *)
val typecheck : header:Ast.program option -> Ast.program -> unit
