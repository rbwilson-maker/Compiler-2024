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
open Core
module E = Elab
module S = Symbol.Map
module L = Symbol
module T = Tree

let trans_binop = function
  | E.Plus -> T.Add
  | E.Minus -> T.Sub
  | E.Times -> T.Mul
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
  | _ -> failwith "div and mod handled separately"
;;

let trans_sig = function
  | E.Sigfpe -> T.Sigfpe
  | E.Sigabrt -> T.Sigabrt

let trans_effectop = function
| E.Divided_by -> T.Div
| E.Modulo -> T.Mod
| _ -> failwith "binary op should only be an effect op here"


let rec trans_exp_acc temps e rev_acc = match e with
  (* after type-checking, id must be declared; do not guard lookup *)
  | E.Var id -> (rev_acc, T.Temp (S.find_exn temps id))
  | E.Const c -> (rev_acc, T.Const c)
  | E.True -> (rev_acc, T.True) | E.False -> (rev_acc, T.False)
  | E.Binop binop ->
    let (s1, lhs) = trans_exp_acc temps binop.lhs rev_acc in
    let (rev_acc', rhs) = trans_exp_acc temps binop.rhs s1 in
    ( match binop.op with
    | E.Divided_by | E.Modulo -> 
      let t = Temp.create () in
      (T.Effect_move 
        { dest = t
        ; op = trans_effectop binop.op
        ; lhs; rhs
        } :: rev_acc', T.Temp t)
    | E.Lshift | E.Rshift ->
      let dest = Temp.create () in
      let if_label, else_label, after_label =  L.unique_symbol "if", L.unique_symbol "else", L.unique_symbol "after" in
      (* Read from bottom to top *)
      ( T.Label after_label ::
        T.Goto after_label ::
        T.Move 
          { dest 
          ; src = T.Binop { lhs; op = trans_binop binop.op; rhs}
          } ::
        T.Label else_label ::
        T.Goto after_label ::
        T.Raise Sigfpe ::
        T.Label if_label ::
        T.Branch
        { condition = T.Binop 
            { lhs = T.Const (Int32.of_int32 (0xFFFFFFE0l))
            ; op = T.Bit_and
            ; rhs
            }
        ; if_label; else_label = Some else_label; after_label
        } :: rev_acc', T.Temp dest)
    | _ -> (rev_acc', T.Binop {op=trans_binop binop.op; lhs; rhs})
    )
  | E.Ternary t ->
    let dest = Temp.create () in
    let if_label, else_label, after_label =  L.unique_symbol "if", L.unique_symbol "else", L.unique_symbol "after" in
    let (rev_acc', condition) = trans_exp_acc temps t.if_exp rev_acc in
    let rev_acc'', then_exp = 
      T.Label if_label
      :: T.Branch { condition ; if_label; else_label = Some else_label ; after_label} 
      :: rev_acc'
      |> (trans_exp_acc temps t.then_exp)
    in let rev_acc''', else_exp = 
      T.Label else_label :: T.Goto after_label :: T.Move {dest; src=then_exp} :: rev_acc''
      |> trans_exp_acc temps t.else_exp
    in (
      T.Label after_label ::
      T.Goto after_label :: 
      T.Move {dest; src=else_exp} 
      :: rev_acc'''
      , T.Temp dest)
  | E.F (fn, args) -> 
    let dest = Temp.create () in
    let rev_acc', args' = 
    List.fold args ~init:(rev_acc, []) ~f:(fun (acc, ps) e ->
      let stms, p = trans_exp_acc temps e acc in (stms, p::ps))
    in let args = List.rev args' in
    (T.Fn_move { dest; fn; args } :: rev_acc', T.Temp dest)
  | E.Void_f (fn, args) ->
    let rev_acc', args' = 
    List.fold args ~init:(rev_acc, []) ~f:(fun (acc, ps) e ->
      let stms, p = trans_exp_acc temps e acc in (stms, p::ps))
    in let t = T.Temp (Temp.create_with_name "this temp will not be used") in
    (T.Do_fn {fn; args = List.rev args'}::rev_acc', t)
  | E.Raise s -> 
    let t = T.Temp (Temp.create_with_name "this will not be reached") in
    (T.Raise (trans_sig s) :: rev_acc, t)

let rec trans_stms_acc (env : Temp.t S.t) (ast : E.stm list) (rev_acc : T.stm list): T.stm list =
  match ast with
  | [] -> rev_acc
  | E.Return e :: _ ->
    (* ignore code after return *)
    (match e with 
     | None -> T.Return None :: rev_acc
     | Some e ->
    let (rev_acc', p) = trans_exp_acc env e rev_acc in
    T.Return (Some p) :: rev_acc')
  | Block block :: stms -> 
    trans_stms_acc env block rev_acc |> trans_stms_acc env stms
  | E.Declare (x, e) :: stms -> 
    let t = Temp.create () in 
    let temps' = S.set env ~key:x ~data:t in
    (match e with 
     | None -> trans_stms_acc temps' stms rev_acc
     | Some e -> trans_stms_acc temps' (E.Assign (x, e)::stms) rev_acc
    )
  | E.Assign (x, e) :: stms ->
    let (rev_acc', p) = trans_exp_acc env e rev_acc in
    T.Move { dest = S.find_exn env x; src = p } :: rev_acc'
    |> trans_stms_acc env stms
  | E.Do e :: stms -> 
    let (rev_acc', _) = trans_exp_acc env e rev_acc in rev_acc'
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
    let (s, p) = trans_exp_acc env cond rev_acc in
    let rev_acc' = 
      T.Label l_if
      :: T.Branch 
      { condition = p
      ; if_label = l_if
      ; else_label = Option.map else_branch ~f:(fun _ -> l_else)
      ; after_label = l_after
      } :: s
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
    let (s, p) = trans_exp_acc env e ((T.Label l_start)::rev_acc) in
    let rev_acc' = 
      T.Label l_body
      :: T.Branch 
      { condition = p
      ; if_label = l_body
      ; else_label = None
      ; after_label = l_end} 
      :: s
      |> (trans_stms_acc env [body])
    in 
      T.Label l_end 
      :: T.Goto l_start
      :: rev_acc'
      |> (trans_stms_acc env stms)

let translate (p  : E.program) : T.program = 
  List.fold_right p ~init:[] ~f:(fun (name, params, body) stms ->
    let new_params = List.map params ~f:(fun _ -> (Temp.create())) in
    let temps = S.of_alist_exn (List.zip_exn params new_params) in
    let body' = trans_stms_acc temps body [] |> List.rev in
    (name, new_params, body')::stms
    )
  (* List.rev (trans_stms_acc S.empty stms []) *)
