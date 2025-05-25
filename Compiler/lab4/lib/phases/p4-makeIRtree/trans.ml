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

let unsafe_mode = ref false;;

let trans_binop = function
  | E.Plus -> T.Add
  | E.Minus -> T.Sub
  | E.Times -> T.Mul
  | E.Less -> T.Less
  | E.Greater -> T.Greater
  | E.Leq -> T.Leq
  | E.Geq -> T.Geq
  | E.Bit_and -> T.Bit_and
  | E.Bit_or -> T.Bit_or
  | E.Bit_xor -> T.Bit_xor
  | E.Lshift -> T.Lshift
  | E.Rshift -> T.Rshift
  | _ -> failwith "div and mod handled separately"
;;

let trans_asnop_self = function
  | E.Plus_eq -> E.Plus
  | E.Minus_eq -> E.Minus
  | E.Times_eq -> Times
  | Div_eq -> Divided_by
  | Mod_eq -> Modulo
  | Bit_and_eq -> Bit_and
  | Bit_or_eq -> Bit_or
  | Bit_xor_eq -> Bit_xor
  | Lshift_eq -> Lshift
  | Rshift_eq -> Rshift
;;

let trans_asnop = function
  | E.Plus_eq -> T.Add
  | E.Minus_eq -> T.Sub
  | E.Times_eq -> T.Mul
  | Bit_and_eq -> Bit_and
  | Bit_or_eq -> Bit_or
  | Bit_xor_eq -> Bit_xor
  | Lshift_eq -> Lshift
  | Rshift_eq -> Rshift
  | _ -> failwith "Div and mod should be effectops"
;;

let trans_asnop_effop = function
  | E.Div_eq -> T.Div
  | E.Mod_eq -> T.Mod
  | _ -> failwith "only for div and mod"

let trans_sig = function
  | E.Sigfpe -> T.Sigfpe
  | E.Sigabrt -> T.Sigabrt
;;

let trans_effectop = function
| E.Divided_by -> T.Div
| E.Modulo -> T.Mod
| _ -> failwith "binary op should only be an effect op here"

(* trans_exp_acc separates out effectful operations from
 * pure expressions, and it returns an updated list of
 * instructions as well as a pure expression *)
let rec trans_exp_acc temps (e : E.exp) rev_acc = match e with
  (* after type-checking, id must be declared; do not guard lookup *)
  | E.Var (id, size) -> (rev_acc, T.Temp (S.find_exn temps id, size))
  | E.Const c -> (rev_acc, T.Const c)
  | E.True -> (rev_acc, T.True) | E.False -> (rev_acc, T.False)
  | E.Binop binop ->
    let (s1, lhs) = trans_exp_acc temps binop.lhs rev_acc in
    let (rev_acc', rhs) = trans_exp_acc temps binop.rhs s1 in
    ( match binop.op with
    | E.Divided_by | E.Modulo -> 
      let t = (Temp.create (), Size.Long) in
      (T.Effect_move 
        { dest = t
        ; op = trans_effectop binop.op
        ; lhs; rhs
        } :: rev_acc', T.Temp t)
    | E.Lshift | E.Rshift ->
      let dest = Temp.create (), Size.Long in
      if !unsafe_mode
      then (
        T.Move 
        { dest 
        ; src = T.Binop { lhs; op = trans_binop binop.op; rhs}
        } :: rev_acc', T.Temp dest
      ) else (
      let if_label, else_label, after_label =  L.unique_symbol "if", L.unique_symbol "else", L.unique_symbol "after" in
      (* Read from bottom to top *)
      ( T.Label after_label ::
        T.Move 
          { dest 
          ; src = T.Binop { lhs; op = trans_binop binop.op; rhs}
          } ::
        T.Label else_label ::
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
      )
    | E.Equal ->
      (rev_acc', T.Equality {lhs; rhs; op = T.Equal; size = binop.op_size})
    | E.Neq ->
      (rev_acc', T.Equality {lhs; rhs; op = T.Neq; size = binop.op_size})
    | _ -> (rev_acc', T.Binop {op=trans_binop binop.op; lhs; rhs})
    )
  | E.Ternary t ->
    let dest = Temp.create (), t.size in
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
      T.Move {dest; src=else_exp} 
      :: rev_acc'''
      , T.Temp dest)
  | E.F f -> 
    let dest = Option.map f.ret_size ~f:(fun s -> Temp.create (), s) in
    let rev_acc', args' = 
    List.fold f.params ~init:(rev_acc, []) ~f:(fun (acc, ps) (e, s) ->
      let stms, p = trans_exp_acc temps e acc in (stms, (p, s)::ps))
    in let args = List.rev args' in
    (T.Fn_move { dest; fn = f.name; args } :: rev_acc', 
      match dest with
      | None -> T.Temp (Temp.create_with_name "this will not be reached", Word)
      | Some dest -> T.Temp dest
    )
  | E.Raise s -> 
    let t = T.Temp (Temp.create_with_name "this will not be reached", Word) in
    (T.Raise (trans_sig s) :: rev_acc, t)
  | E.Null -> (rev_acc, T.Null)
  | E.Mem (m, s) -> 
    let (rev_acc', addr, check_null) = addr_of_memexp temps m rev_acc in
    if !unsafe_mode 
    then (
      let dest = Temp.create (), s in
      T.Read_mem {dest; src = addr} :: rev_acc', T.Temp dest
    ) else (
      let dest = Temp.create (), s in
      let rev_acc'' = 
        if check_null
        then (match addr with 
        | T.Addr_temp t -> T.Check_null (T.Temp (t, Quad)) :: rev_acc'
        | _ -> failwith "Null should have been checked already"
        )
        else rev_acc'
      in (T.Read_mem {dest; src = addr} :: rev_acc'', T.Temp dest)
    )
  | Alloc elem_size -> 
    let dest = Temp.create (), Size.Quad in
    (T.Alloc {dest; elem_size; array_len = None}::rev_acc, T.Temp dest)
  | Alloc_array (elem_size, e) ->
    let dest = Temp.create (), Size.Quad in
    let (rev_acc', e) = trans_exp_acc temps e rev_acc in
    (T.Alloc {dest; elem_size; array_len = Some e}::rev_acc', T.Temp dest)


(* *x.f *)   
and addr_of_memexp temps (mem : E.mem_exp) (rev_acc : T.stm list) : T.stm list * T.addr * bool =
match mem with
  | E.Get_field {strct; field} -> 
    let (rev_acc', strct_addr, check_null) = addr_of_memexp temps strct rev_acc in
    let rev_acc'' = 
      if check_null 
      then (match strct_addr with 
      | T.Addr_temp t -> T.Check_null (T.Temp (t, Quad)) :: rev_acc'
      | _ -> failwith "Null should have been checked already"
      )
      else rev_acc'
    in
    (rev_acc'', T.Field_addr {strct = strct_addr; field}, false)
  | E.Deref e ->
    (* address of deref is always just e *)
    let (rev_acc', e) = trans_exp_acc temps e rev_acc in
    let addr : Temp.t = Temp.create () in
    (T.Move {dest = (addr, Quad); src = e} :: rev_acc'
    , T.Addr_temp addr, (not !unsafe_mode))
  | E.Index {array; index; elem_size} ->
    let (rev_acc', array) = trans_exp_acc temps array rev_acc in
    let (rev_acc'', index) = trans_exp_acc temps index rev_acc' in
    if !unsafe_mode
    then (
      (rev_acc'', T.Unsafe (T.Array_addr {array; index; elem_size}), false)
    ) else (
    (* 1) purify array
     * 2) purify index
     * 3) check if array is null
     * 4) move pure array index exp into a temp
     * 5) return the temp, but it doesn't need null checking 
     *)
    let addr = Temp.create () in
    (T.Array_addr {dest = addr; array; index; elem_size} 
     :: T.Check_null array
     :: rev_acc''
    , T.Addr_temp addr, false)
    )
;;

(* translating an lval should always return an address which we will write to *)
let rec trans_lval_acc temps (lval : E.lval) (rev_acc : T.stm list) : T.stm list * T.addr =
  match lval with
  | E.Var x -> (rev_acc, T.Addr_temp (S.find_exn temps x))
  | E.Deref lval ->
    (* we are calculating the actual value returned by the deref, which should be an address *)
    let (rev_acc', addr1) = trans_lval_acc temps lval rev_acc in
    if !unsafe_mode
    then (
      let addr2 = Temp.create () in
      (T.Read_mem { dest = addr2, Quad; src = addr1} :: rev_acc'
      , T.Addr_temp addr2)
    ) else (
      let addr2 = Temp.create () in
      (T.Read_mem { dest = addr2, Quad; src = addr1} :: T.Check_null (T.Addr addr1):: rev_acc'
      , T.Addr_temp addr2)
    )
  | E.Field {strct; field; _} ->
    let (rev_acc', struct_addr, check_null) = addr_of_lval temps strct rev_acc in
    let rev_acc'' = 
      if check_null && (not !unsafe_mode)
      then (match struct_addr with 
      | T.Addr_temp t -> T.Check_null (T.Temp (t, Quad)) :: rev_acc'
      | _ -> failwith "Null should have been checked already"
      )
      else rev_acc'
    in
    let addr2 = Temp.create () in
    ( T.Read_mem 
      { dest = addr2, Quad
      ; src = T.Field_addr {strct = struct_addr; field} 
      }
    :: rev_acc'', T.Addr_temp addr2
    )
  | E.Index {array; index; elem_size} -> 
    (* we are calculating the actual value returned by indexing, which should be an address *)
    let (rev_acc', array_addr) = trans_lval_acc temps array rev_acc in
    let (rev_acc'', index) = trans_exp_acc temps index rev_acc' in
    if !unsafe_mode
    then (
      let addr = Temp.create () in
      T.Read_mem {dest = addr, Quad; src = T.Unsafe (T.Array_addr {array = T.Addr array_addr; index; elem_size})}
      :: rev_acc'', T.Addr_temp addr
    ) else (
      let addr1 = Temp.create () in
      let addr2 = Temp.create () in
      ( T.Read_mem {dest = addr2, Quad; src = T.Addr_temp addr1}
        :: T.Array_addr {dest = addr1; array = T.Addr array_addr; index; elem_size} 
        :: T.Check_null (T.Addr array_addr)
        :: rev_acc''
      , T.Addr_temp addr2)
    )

and addr_of_lval temps (lval : E.lval) (rev_acc : T.stm list) : T.stm list * T.addr * bool = 
  match lval with
  | E.Deref lval ->
    let (rev_acc, addr) = trans_lval_acc temps lval rev_acc in
    (rev_acc, addr, (not !unsafe_mode))
  | E.Index {array; index; elem_size} -> 
    let (rev_acc', array_addr) = trans_lval_acc temps array rev_acc in
    let (rev_acc'', index) = trans_exp_acc temps index rev_acc' in
    if !unsafe_mode
    then (
      rev_acc'', (T.Unsafe (T.Array_addr {array = T.Addr array_addr; index; elem_size})), false
    ) else (
      let addr = Temp.create () in
      (T.Array_addr {dest = addr; array = T.Addr array_addr; index; elem_size} 
      :: T.Check_null (T.Addr array_addr)
      :: rev_acc''
      , T.Addr_temp addr, false)
    )
  | E.Field {strct; field; _} ->
    let (rev_acc', strct_addr, check_null) = addr_of_lval temps strct rev_acc in
    let rev_acc'' = 
      if check_null && (not !unsafe_mode)
      then (match strct_addr with 
      | T.Addr_temp t -> T.Check_null (T.Temp (t, Quad)) :: rev_acc'
      | _ -> failwith "Null should have been checked already"
      )
      else rev_acc'
    in
    (rev_acc'', T.Field_addr {strct = strct_addr; field}, false)
  | E.Var _ -> failwith "you should not be taking the address of a var"
;;

let rec trans_stms_acc (env : Temp.t S.t) (ast : E.stm list) (rev_acc : T.stm list): T.stm list =
  match ast with
  | [] -> rev_acc
  | E.Return e :: _ ->
    (* ignore code after return *)
    (match e with 
     | None -> T.Return None :: rev_acc
     | Some (e, s) ->
    let (rev_acc', p) = trans_exp_acc env e rev_acc in
    T.Return (Some (p, s)) :: rev_acc')
  | Block block :: stms -> 
    trans_stms_acc env block rev_acc |> trans_stms_acc env stms
  | E.Declare {lhs = (x, s); rhs = e} :: stms -> 
    let t = Temp.create () in 
    let temps' = S.set env ~key:x ~data:t in
    (match e with 
     | None -> trans_stms_acc temps' stms rev_acc
     | Some e -> trans_stms_acc temps' (E.Assign ((E.Var x, s), e)::stms) rev_acc
    )
  | E.Assign ((x, size), e) :: stms ->
    (match x with 
    | E.Var x ->
      let (rev_acc', src) = trans_exp_acc env e rev_acc in
      T.Move { dest = (S.find_exn env x, size); src } :: rev_acc'
    | lval -> 
      (* 1) address of lval
         2) translate e
         3) check_null on address
         4) write to memory *)
      let (rev_acc', addr, check_null) = addr_of_lval env lval rev_acc in
      let (rev_acc'', e) = trans_exp_acc env e rev_acc' in
      let rev_acc''' = 
        if check_null && (not !unsafe_mode)
        then (match addr with 
        | T.Addr_temp t -> T.Check_null (T.Temp (t, Quad)) :: rev_acc''
        | _ -> failwith "Null should have been checked already"
        )
        else rev_acc''
      in
      T.Write_mem 
        { dest = addr
        ; src = e
        ; size
        } 
      :: rev_acc'''
    )
    |> trans_stms_acc env stms
  | E.Asnop {lhs = (x, s); op; rhs} :: stms ->
    (* x is either a variable or an address *)
    (match x with
    | E.Var x -> 
      let (rev_acc', src) = trans_exp_acc env (
        E.Binop
          { op = trans_asnop_self op
          ; lhs = E.Var (x, s)
          ; rhs
          ; op_size = s
          }
      ) rev_acc in
      let dest = (S.find_exn env x, s) in
      T.Move {dest; src} :: rev_acc'
    | lval ->
      let (rev_acc', addr, check_null) = addr_of_lval env lval rev_acc in
      let (rev_acc'', rhs) = trans_exp_acc env rhs rev_acc' in
      let lhs = Temp.create (), s in
      let rev_acc''' = 
        if check_null && (not !unsafe_mode)
        then (match addr with 
          | T.Addr_temp t ->
            T.Read_mem { dest = lhs; src = addr}
            :: T.Check_null (T.Temp (t, Quad)) 
            :: rev_acc''
          | _ -> failwith "Null should have been checked already"
        )
        (* even in unsafe mode, must enforce eval order *)
        else T.Read_mem {dest = lhs; src = addr} :: rev_acc''
      in
      (match op with 
      | E.Div_eq | E.Mod_eq ->
        T.Write_mem
          { dest = addr
          ; size = s
          ; src = T.Temp lhs
          } ::
        T.Effect_move 
          { dest = lhs
          ; lhs = T.Temp lhs
          ; op = trans_asnop_effop op
          ; rhs
          } ::
        rev_acc'''
      | E.Lshift_eq | E.Rshift_eq ->
        if !unsafe_mode 
        then (
          T.Write_mem
            { dest = addr
            ; size = s
            ; src = T.Binop
              { lhs = T.Temp lhs
              ; op = trans_asnop op
              ; rhs
              }
            } :: rev_acc'''
        ) else (
          let if_label, after_label =  L.unique_symbol "if", L.unique_symbol "after" in
          T.Write_mem
            { dest = addr
            ; size = s
            ; src = T.Binop
              { lhs = T.Temp lhs
              ; op = trans_asnop op
              ; rhs
              }
            } ::
            (* Read from bottom to top *)
            T.Label after_label ::
            T.Raise Sigfpe ::
            T.Label if_label ::
            T.Branch
            { condition = T.Binop 
                { lhs = T.Const (Int32.of_int32 (0xFFFFFFE0l))
                ; op = T.Bit_and
                ; rhs
                }
            ; if_label; else_label = None; after_label
            } ::
          rev_acc'''
        )
      | _ ->
        T.Write_mem
          { dest = addr
          ; size = s
          ; src = T.Binop
            { lhs = T.Temp lhs
            ; op = trans_asnop op
            ; rhs
            }
          } ::
        rev_acc'''
      )
    )
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
     *   l_after:
     *     stms ...
     * ``
     * where the lines marked `|` are optional
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
    in (trans_stms_acc env stms) ( T.Label l_after :: rev_acc'')
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
;;

let translate ~(unsafe : bool) (p  : E.program) : T.program =
  unsafe_mode := unsafe;
  List.fold_right p ~init:[] ~f:(fun (name, params, body) stms ->
    let pair_temps = List.map params ~f:(fun (x, s) -> 
      let t = Temp.create () in ((x, t), (t, s))
    ) in
    let temps = S.of_alist_exn (List.map pair_temps ~f:fst) in
    let body' = trans_stms_acc temps body [] |> List.rev in
    (name, List.map pair_temps ~f:snd, body')::stms
  )
;;
