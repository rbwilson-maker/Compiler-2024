(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Modified: Taegyun Kim <taegyunk@cmu.edu> Fall 2014
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *
 * Simple typechecker that is based on a unit Symbol.table
 * This is all that is needed since there is only an integer type present
 * Also, since only straightline code is accepted, we hack our way
 * around initialization checks here.
 *)

structure TypeChecker :> TYPE_CHECK =
struct
  structure A = Ast

  (* tc_exp : unit Symbol.table -> Ast.exp -> Mark.ext option -> unit *)
  fun tc_exp env exp ext =
    case exp of
      A.Var id => (
        case Symbol.look env id of
          NONE =>
            ( ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'")
            ; raise ErrorMsg.Error
            )
        | SOME false =>
            ( ErrorMsg.error ext ("uninitialized variable `" ^ Symbol.name id ^ "'")
            ; raise ErrorMsg.Error
            )
          | SOME true => ()
      )
    | A.ConstExp _ => ()
    | A.OpExp (_,es) =>
      (* Note: it is syntactically impossible in this language to
       * apply an operator to an incorrect number of arguments
       * so we only check each of the arguments
       *)
      List.app (fn e => tc_exp env e ext) es
    | A.Marked marked_exp =>
        tc_exp env (Mark.data marked_exp) (Mark.ext marked_exp)

  (* tc_stms : unit Symbol.table -> Ast.program -> Mark.ext option -> bool -> unit *)
  fun tc_stms _ [] _ ret = ret
    | tc_stms env (stm::stms) ext ret =
      let
        (* chk_var_decl : (Ast.ident * bool) -> unit Symbol.table *)
        fun chk_var_decl (id, init) =
          case Symbol.look env id of
            NONE => (Symbol.bind env (id, init))
          | SOME _ =>
            ( ErrorMsg.error ext ("redeclared variable `" ^ Symbol.name id ^ "'")
            ; raise ErrorMsg.Error
            )
      in
        case stm of
          A.Decl id => tc_stms (chk_var_decl (id, false)) stms ext ret
        | A.DeclAssn (id, e) =>
            tc_stms env (A.Decl id::A.Assign (id, e)::stms) ext ret
        | A.Assign(id, e) =>
            ( tc_exp env e ext
            ; case Symbol.look env id of
                NONE =>
                  ( ErrorMsg.error ext ("undeclared variable `" ^ Symbol.name id ^ "'")
                  ; raise ErrorMsg.Error
                  )
                | SOME false =>
                    tc_stms (Symbol.bind env (id, true)) stms ext ret
                | SOME true => tc_stms env stms ext ret
            )
        | A.Markeds marked_stm =>
            tc_stms env ((Mark.data marked_stm)::stms) (Mark.ext marked_stm) ret
        | A.Return e =>
            ( tc_exp env e ext
            ; let
                (* A hacky way to get around test cases having code after
                 * the return statement. We only need to check for whether
                 * a variable after the return is declared or not. No need
                 * to check for proper initialization.
                 *)
                val env' =
                  Symbol.digest (map (fn x => (x, true)) (Symbol.keys env))
              in
                tc_stms env' stms ext true
              end
            )
      end

  fun typecheck prog =
    if tc_stms Symbol.empty prog NONE false then ()
    else (ErrorMsg.error NONE "main does not return\n"; raise ErrorMsg.Error)

end
