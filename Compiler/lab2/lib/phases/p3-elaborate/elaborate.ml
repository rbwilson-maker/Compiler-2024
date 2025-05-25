(* L2 elaborator *)
open Core

module A = Ast
module E = Elab

let map_binop : A.binop -> E.binop = function
| Plus -> E.Plus | Minus -> E.Minus | Times -> E.Times 
| Divided_by -> E.Divided_by | Modulo -> E.Modulo
| Less -> E.Less | Greater -> E.Greater
| Equal -> E.Equal | Leq  -> E.Leq | Geq -> E.Geq | Neq -> E.Neq
| Bit_and -> E.Bit_and | Bit_or -> E.Bit_or | Bit_xor -> E.Bit_xor 
| Lshift -> E.Lshift | Rshift -> E.Rshift
| _ -> failwith "And and Or are being elaborated to ternary exps"
;;

let map_typ : A.typ -> E.typ = function
| Int -> E.Int
| Bool -> E.Bool

let rec elab_mexp (e : A.mexp) : E.exp = 
match Mark.data e with
| A.Var x -> E.Var x
| A.Const i -> E.Const i
| A.True -> E.True
| A.False -> E.False
| A.Ternary t -> E.Ternary
    { if_exp = (elab_mexp t.if_exp)
    ; then_exp = (elab_mexp t.then_exp)
    ; else_exp = (elab_mexp t.else_exp)
    }
| A.Binop b -> (match b.op with
  | A.And -> E.Ternary 
    { if_exp = (elab_mexp b.lhs)
    ; then_exp = (elab_mexp b.rhs)
    ; else_exp = E.False
    }
  | A.Or -> E.Ternary 
    { if_exp = (elab_mexp b.lhs)
    ; then_exp = E.True
    ; else_exp = (elab_mexp b.rhs)
    }
  | A.Lshift | A.Rshift ->
    let rhs = elab_mexp b.rhs in
    (* (0 <= k < 32) ? lhs <</>> k : 1/0 *)
    E.Ternary { 
      (* (0<=k) ? (k < 32) : false *)
      if_exp = E.Ternary 
        { if_exp = (E.Binop
        { op = E.Leq
        ; lhs = E.Const Int32.zero
        ; rhs = rhs
        })
      ; then_exp = E.Binop
        { op = E.Less
        ; lhs = rhs
        ; rhs = E.Const 32l
        }
      ; else_exp = E.False
      }
    ; then_exp = E.Binop
      { op = map_binop b.op
      ; lhs = elab_mexp b.lhs
      ; rhs = rhs
      }
    ; else_exp = E.Binop
      { op = E.Divided_by
      ; lhs = E.Const Int32.one
      ; rhs = E.Const Int32.zero
      }
    }
  | x -> E.Binop 
    {op = map_binop x
    ; lhs = (elab_mexp b.lhs)
    ; rhs = (elab_mexp b.rhs)
    }
  )
| A.Unop u -> (match u.op with
  | A.Not -> E.Ternary 
    { if_exp = (elab_mexp u.operand) 
    ; then_exp = E.False
    ; else_exp = E.True
    }
  | A.Bit_not -> E.Binop
    { op = E.Bit_xor
    ; lhs = elab_mexp u.operand
    ; rhs = E.Const Int32.minus_one
    }
  | A.Negative -> E.Binop
    { op = E.Minus
    ; lhs = E.Const Int32.zero
    ; rhs = elab_mexp u.operand
    }
  )
;;

let elab_simple (simp : A.simple) : E.stm = match simp with
| A.Declare (A.New_var (t, x)) -> E.Declare (map_typ t, x, None)
| A.Declare (A.Init (t, x, e)) -> E.Declare (map_typ t, x, Some (elab_mexp e))
| A.Assign (x, e) -> E.Assign (x, elab_mexp e)
| A.Do (e) -> E.Do (elab_mexp e)
;;

let rec elab_stm (s : A.mstm) : E.stm = 
match Mark.data s with
| A.Block (stms) -> E.Block (elab_stms stms)
| A.Simple (simp) -> elab_simple simp
| A.Control (cont) -> elab_control cont

and elab_control (c : A.control) : E.stm = match c with
| A.If (cond, s1, s2) -> 
  E.If (elab_mexp cond, elab_stm s1, Option.map s2 ~f:elab_stm)
| A.While (cond, body) -> E.While (elab_mexp cond, elab_stm body)
| A.Return e -> E.Return (elab_mexp e)
| A.For (simp1, cond, simp2, body) ->
  E.Block (
    (match simp1 with None -> [] | Some s -> [elab_simple s])
    @ [E.While (elab_mexp cond,
  E.Block (
      [(elab_stm body)]
    @ (match simp2 with None -> [] | Some s -> [elab_simple s])
      )
    )]
  )


and elab_stms (stms : A.stms) : E.stms = 
  List.fold_right stms ~init:[] 
    ~f:(fun stm acc -> (elab_stm stm) :: acc)
;;

let elaborate ((sym, body) : A.program) : E.program = 
  (sym, elab_stms body)
;;