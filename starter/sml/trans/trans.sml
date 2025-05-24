(* L1 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick ;) <tbrick@andrew.cmu.edu>
 *)
structure Trans :> TRANS =
struct

  structure A = Ast
  structure T = Tree

  fun trans_oper oper =
    case oper of
      A.PLUS      => T.ADD
    | A.MINUS     => T.SUB
    | A.TIMES     => T.MUL
    | A.DIVIDEDBY => T.DIV
    | A.MODULO    => T.MOD
    | A.NEGATIVE  => T.SUB (* unary to binary! *)

  and trans_exp env exp =
    case exp of
      A.Var id =>
        (* after type-checking, id must be declared; do not guard lookup *)
        T.TEMP (Symbol.look' env id)
    | A.ConstExp c => T.CONST c
    | A.OpExp (oper, [e1, e2]) =>
        T.BINOP (trans_oper oper, trans_exp env e1, trans_exp env e2)
    | A.OpExp (A.NEGATIVE, [e]) =>
        T.BINOP ( trans_oper A.NEGATIVE
                , T.CONST Word32Signed.ZERO
                , trans_exp env e
                )
    | A.OpExp _ =>
        (* anything else should be impossible *)
        raise Fail ("Ill-formed exp: " ^ A.Print.pp_exp exp)
    | A.Marked marked_exp => trans_exp env (Mark.data marked_exp)

  (* translate the statement *)
  (* trans_stms : Temp.temp Symbol.table -> A.stm list -> Tree.stm list *)
  fun trans_stms _ [] = raise Fail "no return in translated statements"
    | trans_stms env (stm::stms) =
    case stm of
      A.Decl id => trans_stms (Symbol.bind env (id, Temp.new ())) stms
    | A.Assign (id,e) =>
        let
          val t = Temp.new()
          val env' = Symbol.bind env (id, t)
        in
          T.MOVE (t, trans_exp env e) :: trans_stms env' stms
        end
    | A.DeclAssn (id,e) => trans_stms env (A.Decl id::A.Assign(id,e)::stms)
    | A.Return e =>
        (* ignore code after return *)
        T.RETURN (trans_exp env e) :: []
    | A.Markeds marked_stm => trans_stms env ((Mark.data marked_stm)::stms)

  fun translate stms = trans_stms Symbol.empty stms

end
