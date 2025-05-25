(* L2 Compiler
 * AST -> IR Translator
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *)
open Core
module E = Elab
module S = Symbol.Map
module L = Symbol
module T = Tree

let trans_binop = function
  | E.Plus -> T.Add
  | E.Minus -> T.Sub
  | E.Times -> T.Mul
  | E.Divided_by -> T.Div
  | E.Modulo -> T.Mod
  | E.Less -> T.Less
  | E.Greater -> T.Greater
  | E.Equal -> T.Equal
  | E.Leq -> T.Leq
  | E.Geq -> T.Geq
  | E.Neq -> T.Neq
  | E.Bit_and -> T.Bit_and
  | E.Bit_or -> T.Bit_or
  | E.Bit_xor -> T.Bit_xor
  | E.Lshift -> T.Lshift
  | E.Rshift -> T.Rshift
;;

let rec trans_exp temps = function
  (* after type-checking, id must be declared; do not guard lookup *)
  | E.Var id -> T.Temp (S.find_exn temps id)
  | E.Const c -> T.Const c
  | E.True -> T.True | E.False -> T.False
  | E.Binop binop ->
      T.Binop
        { op = trans_binop binop.op
        ; lhs = trans_exp temps binop.lhs
        ; rhs = trans_exp temps binop.rhs
        }
  | E.Ternary t ->
    T.Ternary 
      { if_exp = trans_exp temps t.if_exp
      ; then_exp = trans_exp temps t.then_exp
      ; else_exp = trans_exp temps t.else_exp
      }

(* translate the statement *)
let rec trans_stms_acc (env : Temp.t S.t) (ast : E.stm list) (rev_acc : T.stm list): T.stm list =
  match ast with
  | [] -> rev_acc
  | E.Return e :: _ ->
    (* ignore code after return *)
    T.Return (trans_exp env e) :: rev_acc
  | Block block :: stms -> 
    trans_stms_acc env block rev_acc |> trans_stms_acc env stms
  | E.Declare (_, x, e) :: stms -> 
    let t = Temp.create () in 
    let temps' = S.set env ~key:x ~data:t in
    (match e with 
     | None -> trans_stms_acc temps' stms rev_acc
     | Some e -> trans_stms_acc temps' (E.Assign (x, e)::stms) rev_acc
    )
  | E.Assign (x, e) :: stms ->
    (* typechecker guarantees all variables were declared properly, thus have temps in env *)
    T.Move { dest = S.find_exn env x; src = trans_exp env e } :: rev_acc
    |> trans_stms_acc env stms
  | E.Do e :: stms -> 
    (* create a new temp that is never used nor kept around *)
    let t = Temp.create () in
    T.Move { dest = t; src = trans_exp env e } :: rev_acc
    |> trans_stms_acc env stms
  | E.If (cond, if_branch, else_branch) :: stms ->
    (* This represents the reverse of:
     * ``branch (e) (next instr) (else) (after)
     *   l_if:
     *     if_branch statements ...
     * |   goto l_after
     * | l_else:
     * |   else_branch statements ...
     * |   goto l_after
     *   l_after:
     *     stms ...
     * ``
     * where the `|` lines are optional
     *)
    let l_if, l_else, l_after =  L.unique_symbol "if", L.unique_symbol "else", L.unique_symbol "after" in
    let rev_acc' = 
      T.Label l_if
      :: T.Branch 
      { condition = trans_exp env cond
      ; if_label = l_if
      ; else_label = Option.map else_branch ~f:(fun _ -> l_else)
      ; after_label = l_after
      } :: rev_acc
      |> (trans_stms_acc env [if_branch]) 
    in let rev_acc'' =
      (match else_branch with 
      | None -> rev_acc'
      | Some else_branch -> 
        trans_stms_acc env [else_branch] (
          T.Label l_else :: T.Goto l_after :: rev_acc'
        )) 
    in (trans_stms_acc env stms) ( T.Label l_after:: T.Goto l_after :: rev_acc'')
  | E.While (e, body) :: stms -> 
    (* This represents the reverse of:
     * ``l_start:
     *   branch (e)
     *   l_body:
     *     body ...
     *     goto l_start
     *   l_end:
     *     stms ...
     * ``
     *)
    let l_start, l_body, l_end = L.unique_symbol "start", L.unique_symbol "body", L.unique_symbol "end" in
    let rev_acc' = 
      T.Label l_body
      :: T.Branch 
      { condition = trans_exp env e
      ; if_label = l_body
      ; else_label = None
      ; after_label = l_end} 
      :: (T.Label l_start) 
      :: rev_acc
      |> (trans_stms_acc env [body])
    in 
      T.Label l_end 
      :: T.Goto l_start
      :: rev_acc'
      |> (trans_stms_acc env stms)

let translate ((_fn, stms) : E.program) : T.program = 
  List.rev (trans_stms_acc S.empty stms [])
