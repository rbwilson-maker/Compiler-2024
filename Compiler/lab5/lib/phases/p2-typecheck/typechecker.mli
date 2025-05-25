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

open! Core

type output = 
{ tagged_ast : Asts.Tagged.program
; header_decls : Symbol.t Hash_set.t 
; fn_types : (Symbol.t, Asts.Marked.ret_typ) Hashtbl.t
; typedefs : (Symbol.t, Asts.Marked.typ) Hashtbl.t 
; structs : Size.structs
}

(** Prints error message and raises ErrorMsg.error if typecheck error found *)
val typecheck : header:Asts.Marked.program option -> Asts.Marked.program -> output
