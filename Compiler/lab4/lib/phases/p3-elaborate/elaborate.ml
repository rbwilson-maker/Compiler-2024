(* Rachompicole L3 Compiler
 * Elaborator
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Elaboration removes for loops, unary operators, and performs additional
 * checks such as with Lshift and Rshift.
 *)
open Core

module A = Asts.Tagged
module T = Type.Tag
module E = Elab
module S = Symbol

type global_state_typ = 
{ map_name : S.t -> S.t
; fn_types : (S.t, A.ret_typ) Hashtbl.t
; type_to_size : A.typ -> Size.data_size
; type_to_small_size : A.typ -> Size.t
}

let globl = ref 
{ map_name = (fun x -> x)
; fn_types = Hashtbl.create (module S)
; type_to_size = (fun _ -> Size.Small Word)
; type_to_small_size = (fun _ -> Size.Word)
}

let map_binop : A.binop -> E.binop = function
| Plus -> E.Plus | Minus -> E.Minus | Times -> E.Times 
| Divided_by -> E.Divided_by | Modulo -> E.Modulo
| Less -> E.Less | Greater -> E.Greater
| Equal -> E.Equal | Leq  -> E.Leq | Geq -> E.Geq | Neq -> E.Neq
| Bit_and -> E.Bit_and | Bit_or -> E.Bit_or | Bit_xor -> E.Bit_xor 
| Lshift -> E.Lshift | Rshift -> E.Rshift
| _ -> failwith "And and Or are being elaborated to ternary exps"
;;

let _map_asnop : A.asnop -> E.asnop = function
| Plus_eq -> E.Plus_eq
| Minus_eq -> E.Minus_eq
| Times_eq -> E.Times_eq
| Div_eq -> E.Div_eq
| Mod_eq -> E.Mod_eq
| Bit_and_eq -> E.Bit_and_eq
| Bit_or_eq -> E.Bit_or_eq
| Bit_xor_eq -> E.Bit_xor_eq
| Lshift_eq -> E.Lshift_eq
| Rshift_eq -> E.Rshift_eq
;;

let map_asnop : A.asnop -> E.asnop = function
| Plus_eq -> E.Plus_eq
| Minus_eq -> E.Minus_eq
| Times_eq -> E.Times_eq
| Div_eq -> E.Div_eq
| Mod_eq -> E.Mod_eq
| Bit_and_eq -> E.Bit_and_eq
| Bit_or_eq -> E.Bit_or_eq
| Bit_xor_eq -> E.Bit_xor_eq
| Lshift_eq -> E.Lshift_eq
| Rshift_eq -> E.Rshift_eq
;;

let rec elab_mexp (e : A.mexp) : E.exp = 
let e, typ = T.split e in
(match e with
| A.Var x -> E.Var (x, !globl.type_to_small_size typ)
| A.Const i -> E.Const i
| A.True -> E.True | A.False -> E.False
| A.Ternary t -> E.Ternary
    { if_exp = (elab_mexp t.if_exp)
    ; then_exp = (elab_mexp t.then_exp)
    ; else_exp = (elab_mexp t.else_exp)
    ; size = !globl.type_to_small_size typ
    }
| A.Binop b -> (match b.op with
  | A.And -> E.Ternary 
    { if_exp = (elab_mexp b.lhs)
    ; then_exp = (elab_mexp b.rhs)
    ; else_exp = E.False
    ; size = !globl.type_to_small_size typ
    }
  | A.Or -> E.Ternary 
    { if_exp = (elab_mexp b.lhs)
    ; then_exp = E.True
    ; else_exp = (elab_mexp b.rhs)
    ; size = !globl.type_to_small_size typ
    }
  | x -> 
    let lhs, op_size = elab_with_small_size b.lhs in
    E.Binop 
    {op = map_binop x
    ; lhs
    ; rhs = (elab_mexp b.rhs)
    ; op_size
    }
  )
| A.Unop u -> (match u.op with
  | A.Not -> E.Ternary 
    { if_exp = (elab_mexp u.operand) 
    ; then_exp = E.False
    ; else_exp = E.True
    ; size = !globl.type_to_small_size typ
    }
  | A.Bit_not -> E.Binop
    { op = E.Bit_xor
    ; lhs = elab_mexp u.operand
    ; rhs = E.Const Int32.minus_one
    ; op_size = Long
    }
  | A.Negative -> E.Binop
    { op = E.Minus
    ; lhs = E.Const Int32.zero
    ; rhs = elab_mexp u.operand
    ; op_size = Long
    }
  )
| A.Fn_call (sym, exps) ->
  E.F 
    { name = !globl.map_name sym
    ; params = List.map exps ~f:elab_with_small_size
    ; ret_size = Some (!globl.type_to_small_size typ) 
    }
| A.Mem m -> E.Mem (elab_mmem m, !globl.type_to_small_size typ)
| A.Alloc typ -> E.Alloc (!globl.type_to_size typ)
| A.Alloc_array {typ; size} -> 
  E.Alloc_array (!globl.type_to_size typ, elab_mexp size)
| A.Null -> E.Null
)

and elab_mmem (m : A.mmem) : E.mem_exp =
  let m, typ = T.split m in
  match m with
  | A.Get_field (e, field) -> 
    let s = (match T.untag e with
    | Type.Struct s -> s
    | _ -> failwith "e must have type")
    in
    E.Get_field {strct = elab_mmem e; field = (s, field)}
  | A.Deref_field (e, field) -> 
    let s = (match T.untag e with
    | Type.Ptr (Type.Struct s) -> s
    | _ -> failwith "e must be a pointer to struct")
    in
    E.Get_field {strct = E.Deref (elab_mexp e); field = (s, field)}
  | A.Deref e -> E.Deref (elab_mexp e)
  | A.Index {array; index} -> 
    E.Index 
      { array = elab_mexp array
      ; index = elab_mexp index
      ; elem_size = !globl.type_to_size typ}

and elab_with_small_size (e : A.mexp) : E.exp * Size.t =
  elab_mexp e, !globl.type_to_small_size (T.untag e)
;;

let rec elab_mlval (e : A.mlval) :  E.lval = 
let e, typ = T.split e in
match e with 
| LVar x -> E.Var x
| LGet_field (mlval, field) -> 
  let s = (match T.untag mlval with
  | Type.Struct s -> s
  | s -> failwith (sprintf 
    "%s should have struct type but has type %s" 
    (A.Print.pp_lval (T.data mlval))
    (Type.pp_typ s)
  ))
  in
  E.Field {strct = elab_mlval mlval; field = (s, field)}
| LDeref_field (mlval, field) -> 
  let s_name = (match T.untag mlval with
  | Type.Ptr (Type.Struct s) -> s
  | s -> failwith (sprintf 
    "%s should have struct ptr type but has type %s" 
    (A.Print.pp_lval (T.data mlval))
    (Type.pp_typ s)
  ))
  in
  E.Field {strct = E.Deref (elab_mlval mlval); field = (s_name, field)}
| LDeref mlval -> E.Deref (elab_mlval mlval)
| LIndex {array; index} -> 
  E.Index 
    { array = elab_mlval array
    ; index = elab_mexp index
    ; elem_size = !globl.type_to_size typ
    }
  
let elab_simple (simp : A.simple) : E.stm = match simp with
| A.Declare (A.New_var (typ, x)) -> 
  E.Declare {lhs = (x, !globl.type_to_small_size typ); rhs = None}
| A.Declare (A.Init (typ, x, e)) -> 
  E.Declare {lhs = (x, !globl.type_to_small_size typ); rhs = Some (elab_mexp e)}
| A.Assign (x, e) -> 
  let s =  !globl.type_to_small_size (T.untag x) in
  E.Assign ((elab_mlval x, s), elab_mexp e)
| A.Asnop {lhs; op; rhs} ->
  let s =  !globl.type_to_small_size (T.untag lhs) in
  E.Asnop {lhs = (elab_mlval lhs, s); op = map_asnop op; rhs = elab_mexp rhs}
| A.Do (e) -> E.Do
  (match T.data e with 
  | A.Fn_call (sym, exps) ->
    (match (Hashtbl.find_exn !globl.fn_types sym) with
    | Type.Void -> 
      E.F 
        { name = !globl.map_name sym
        ; params = List.map exps ~f:elab_with_small_size
        ; ret_size = None
        }
    | _ -> elab_mexp e
    )
  | _ -> elab_mexp e
  )
;;

let rec elab_stm (s : A.mstm) : E.stm = 
match Type.Tag.data s with
| A.Block (stms) -> E.Block (elab_stms stms)
| A.Simple (simp) -> elab_simple simp
| A.Control (cont) -> elab_control cont

and elab_control (c : A.control) : E.stm = match c with
| A.If (cond, s1, s2) -> 
  E.If (elab_mexp cond, elab_stm s1, Option.map s2 ~f:(elab_stm))
| A.While (cond, body) -> E.While (elab_mexp cond, elab_stm body)
| A.Return e -> E.Return (Option.map ~f:(elab_with_small_size) e)
| A.Assert e -> (* if (!e) abort();*)
  E.If (elab_mexp (T.tag (A.Unop {op=A.Not; operand=e}) Type.Bool), 
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

and elab_stms ?add_ret (stms : A.stms) : E.stms = 
  let init = match add_ret with None -> [] | Some () -> [E.Return None] in
  List.fold_right stms ~init
    ~f:(fun stm acc -> (elab_stm stm) :: acc)
;;

let elaborate ~(tc_data : Typechecker.output) : Elab.program * Size.structs = 
  (* Create global mapping names function *)
  let map_name sym = 
    if Hash_set.mem tc_data.header_decls sym
    then (
      match Sys.getenv "UNAME" with
      | Some "Darwin" -> S.symbol ("_" ^ S.name sym)
      | _ -> sym
    )
    else ( 
      let name = S.name sym in
      match Sys.getenv "UNAME" with
      | Some "Darwin" -> S.symbol ("__c0_" ^ name)
      | _ -> S.symbol ("_c0_" ^ name)
    )
  in

  (* todo can move this out into the global context instead of ref cel *)
  let type_to_size : A.typ -> Size.data_size = function
  | Int -> Size.Small Size.Long
  | Bool -> Size.Small Size.Long
  | Ident _id -> failwith "typedefs got lost somewhere good luck"
  | Ptr _ -> Size.Small Size.Quad 
  | Array _ -> Size.Small Size.Quad
  | Struct s -> Size.Struct (snd (Hashtbl.find_exn tc_data.structs s))
  | Anyptr -> Size.Small Size.Quad
  in

  let rec type_to_small_size : A.typ -> Size.t = function
  | Int -> Size.Long
  | Bool -> Size.Long
  | Ident id -> type_to_small_size (Hashtbl.find_exn tc_data.typedefs id)
  | Ptr _ -> Size.Quad 
  | Array _ -> Size.Quad
  | Struct _ -> failwith "structs shouldn't be passed directly to functions"
  | Anyptr -> Size.Quad
  in

  globl := {
    map_name;
    type_to_size;
    type_to_small_size;
    fn_types = tc_data.fn_types;
  };

  (* Elaborate *)
  List.fold_right tc_data.tagged_ast ~init:[] ~f:(fun f acc -> 
  match f with
  | A.Fdefn (typ, symb, params, stms) -> 
    let new_params = List.map params ~f:(fun (typ, x) -> 
      let size = type_to_small_size typ in
      (x, size)
    ) in
    let new_stms = 
      (match typ with
      | Type.Void -> elab_stms ~add_ret:() stms
      | _ -> elab_stms stms
      ) in
    (map_name symb, new_params, new_stms)
    :: acc
  | _ -> acc),
   tc_data.structs (* todo tc returns this now wahoo *)
;;