(* Rachompicole L3 Compiler
 * Elaborator
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Elaboration removes for loops, unary operators, and performs additional
 * checks such as with Lshift and Rshift.
 *)
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

let map_param (_, sym) = sym

let map_fn_name = ref (fun x -> x)
let fn_types = ref (Hashtbl.create (module Symbol))

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
  | A.Fn_call (sym, exps) -> 
    match (Hashtbl.find_exn !fn_types sym) with
    | A.Void -> E.Void_f (!map_fn_name sym, List.map exps ~f:elab_mexp)
    | _ -> E.F (!map_fn_name sym, List.map exps ~f:elab_mexp)
;;

let elab_simple (simp : A.simple) : E.stm = match simp with
| A.Declare (A.New_var (_, x)) -> E.Declare (x, None)
| A.Declare (A.Init (_, x, e)) -> E.Declare (x, Some (elab_mexp e))
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
  E.If (elab_mexp cond, elab_stm s1, Option.map s2 ~f:(elab_stm))
| A.While (cond, body) -> E.While (elab_mexp cond, elab_stm body)
| A.Return e -> E.Return (Option.map ~f:(elab_mexp) e)
| A.Assert e -> (* if (!e) abort();*)
  E.If (elab_mexp (Mark.naked (Ast.Unop {op=Ast.Not; operand=e})), 
    E.Do (E.Raise Sigabrt), None)
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

let elaborate ~header (p : A.program) : E.program = 
  (* Create a gloabl function for adding _c0_ to all internally defined
   * functions and underscores to everything if on mac 
   *)
  let header_decls = Hash_set.create (module Symbol) in
  (match header with 
  | None -> ()
  | Some header -> 
      List.iter header ~f:(fun gdecl ->
        match gdecl with
        | A.Fdecl (typ, name, _) -> 
          Hashtbl.set (!fn_types) ~key:name ~data:typ;
          Hash_set.add header_decls name
        | A.Fdefn (typ, name, _, _) -> 
          Hashtbl.set (!fn_types) ~key:name ~data:typ;
        | _ -> ()
      )
  );
  List.iter p ~f:(fun gdecl ->
    match gdecl with
    | A.Fdecl (typ, name, _) -> 
      Hashtbl.set (!fn_types) ~key:name ~data:typ;
    | A.Fdefn (typ, name, _, _) -> 
      Hashtbl.set (!fn_types) ~key:name ~data:typ;
    | _ -> ()
  );

  (* Create global mapping names function *)
  let map_name sym = 
    if Hash_set.mem header_decls sym
    then (
      match Sys.getenv "UNAME" with
      | Some "Darwin" -> Symbol.symbol ("_" ^ Symbol.name sym)
      | _ -> sym
    )
    else ( 
      let name = Symbol.name sym in
      match Sys.getenv "UNAME" with
      | Some "Darwin" -> Symbol.symbol ("__c0_" ^ name)
      | _ -> Symbol.symbol ("_c0_" ^ name)
    )
  in
  map_fn_name := map_name;

  (* Elaborate *)
  List.fold_right p ~init:[] ~f:(fun f acc -> 
  match f with
  | A.Fdefn (_, symb, params, stms) -> 
    (!map_fn_name symb, List.map params ~f:map_param, elab_stms stms)
    :: acc
  | _ -> acc)
;;