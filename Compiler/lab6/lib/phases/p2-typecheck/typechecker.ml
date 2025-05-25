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
module AM = Asts.Marked
module AT = Asts.Tagged
module T = Type
module S = Symbol.Map

type output = 
{ tagged_ast : Asts.Tagged.program
; header_decls : Symbol.t Hash_set.t 
; fn_types : (Symbol.t, Type.ret_typ) Hashtbl.t
; typedefs : (Symbol.t, Type.t) Hashtbl.t
; structs : Size.structs
}

(* ************************************* *)
(* *************** Types *************** *)
(* ************************************* *)

type init_status =
  | Decl of Type.t
  | Init of Type.t

type defn_status =
  | Unused
  | Used    (* Bad ending state *)
  | Defined
type fn_info = Type.ret_typ * AM.param list

type fn_env =
  { vars : init_status S.t
  ; returns : bool
  ; return_type : Type.ret_typ
  }

(** Compare with Size.struct_fields *)
type struct_fields_typed = (Type.t * Size.data_size * Int64.t) Symbol.Map.t * Int64.t

(* ************************************************** *)
(* *************** Global information *************** *)
(* ************************************************** *)

let header_fns :  Symbol.t Hash_set.t                        = Hash_set.create (module Symbol)
let fn_decls   : (Symbol.t, defn_status * fn_info) Hashtbl.t = Hashtbl.create  (module Symbol)
let typedefs   : (Symbol.t, Type.t) Hashtbl.t                = Hashtbl.create  (module Symbol)
let structs    : (Symbol.t, struct_fields_typed) Hashtbl.t   = Hashtbl.create  (module Symbol)

(* *************************************** *)
(* *************** Helpers *************** *)
(* *************************************** *)

let tc_errors : Error_msg.t = Error_msg.create ()
let tc_error ?marked msg =
  Error_msg.error tc_errors ~msg
    (match marked with None -> None | Some m -> Mark.src_span m);
  raise Error_msg.Error

let defn_status_max ds1 ds2 = match (ds1, ds2) with
  | (Defined, _) | (_, Defined) -> Defined
  | (Unused, ds) | (ds, Unused) -> ds
  | (Used, Used) -> Used

let equal_typ (t1 : T.t) (t2 : T.t) : bool =
  match (t1, t2) with
  | (Anyptr, Ptr _) | (Ptr _, Anyptr) | (Anyptr, Anyptr) -> true
  | _ -> T.equal t1 t2

let equal_ret_typ (t1 : T.ret_typ) (t2 : T.ret_typ) : bool =
  match (t1, t2) with
  | (Void, Void) -> true
  | (Typ t1, Typ t2) -> equal_typ t1 t2
  | _ -> false

let big_type_error typ msg =
  match typ with
    | T.Struct _ -> tc_error msg
    | _ -> ()

(** Raises a typecheck error if the name is found in the context. *)
let tc_error_if_declared name (ctx : (Symbol.t, 'a) Hashtbl.t) =
  match Hashtbl.find ctx name with
  | None -> ()
  | Some _ -> tc_error (sprintf "Repeat declaration of %s" (Symbol.name name)) 

(**
Check if a function declaration is valid.
@requires The input ret_type and params are pure :-)
*)
let check_valid_fn_decl ?no_prev_defn fn_name ((ret_typ : T.ret_typ), params) =
  (* Make sure the types ain't big *)
  (match ret_typ with
    | Typ (Struct _) -> tc_error (sprintf "Return type of %s must be small" (Symbol.name fn_name))
    | _ -> ());
  List.iter params ~f:(fun (typ,_) -> big_type_error typ (sprintf "Parameter types of %s must be small" (Symbol.name fn_name)));
  (* Make sure the function is not shadowing a type. *)
  tc_error_if_declared fn_name typedefs;
  (* See if the function is declared already and make sure params match. *)
  (match Hashtbl.find fn_decls fn_name with
  | None -> ()
  | Some (defn_status, (ret_typ', params')) ->
    (* If wanted, check that the function hasn't been previously defined *)
    (match (no_prev_defn, defn_status) with
    | (Some (), Defined) -> tc_error (sprintf "Function %s is defined twice" (Symbol.name fn_name))
    | _ -> ()
    );
    (* Check for ret_type equality. The type in fn_decls should be converted. *)
    if equal_ret_typ ret_typ ret_typ'
    (* Check for parameter equality *)
    then match List.iter2 params params' ~f:(fun (t1, _) (t2, _) -> (* Param name doesn't matter *)
        if equal_typ t1 t2
        then ()
        else tc_error (sprintf "Function %s does not match previous declaration" (Symbol.name fn_name))
      ) with
      | Ok _ -> ()
      | Unequal_lengths -> tc_error (sprintf "Function %s does not match previous declaration" (Symbol.name fn_name))
    else tc_error (sprintf "Function %s does not match previous declaration" (Symbol.name fn_name))
  );
  (* Iterate through each parameter. Check that:
    * 1. The param name is not shadowing a type.
    * 2. Each type is defined.
    * 3. Each param name is used only once. *)
  let params_hs = Hash_set.create (module Symbol) in
  List.iter params ~f:(fun (typ, param) ->
    (* 1 *)
    tc_error_if_declared param typedefs;
    (* 2 *)
    (match typ with
    | T.Ident typ ->
      (match Hashtbl.find typedefs typ with
      | None -> tc_error (sprintf "Type %s is not defined" (Symbol.name typ))
      | Some _ -> ())
    | _ -> ()
    );
    (* 3 *)
    (match Hash_set.strict_add params_hs param with
    | Ok _ -> ()
    | Error _ -> tc_error (sprintf "Function %s uses parameter name twice" (Symbol.name fn_name))
    )
  )

(**
Get the typedef-aware base type and raise a marked error if it isn't defined.    
*)
let rec get_typedef_defn ?marked typ =
  match typ with
    | T.Ident name -> (
      match (Hashtbl.find typedefs name, marked) with
      | (None, Some marked) -> tc_error ~marked:marked (sprintf "Type %s isn't defined" (Symbol.name name))
      | (None, None) -> tc_error (sprintf "Type %s isn't defined" (Symbol.name name))
      | (Some typ, _) -> typ
    )
    | Ptr typ -> T.Ptr (get_typedef_defn typ)
    | Array typ -> T.Array (get_typedef_defn typ)
    | typ -> typ

let get_ret_typedef_defn ?marked ret_typ =
  match (ret_typ, marked) with
    | (T.Typ t, None) -> T.Typ (get_typedef_defn t)
    | (T.Typ t, Some marked) -> T.Typ (get_typedef_defn ~marked t)
    | _ -> ret_typ
;;

let rec calc_struct_size (sname : Symbol.t) (fields : AM.field list) : struct_fields_typed =
  let (fields, total_bytes) = 
  List.fold fields ~init:(Symbol.Map.empty, Int64.zero) ~f:(fun (fields, offset) (field, typ) ->
    let typ = get_typedef_defn typ in
    let size = type_to_size typ in
    let offset = 
      if Int64.(offset % 8L = 0L)
      then offset
      else (* if it's not 8 aligned it will be 4 aligned *)
      (match size with
      | Small Byte | Small Long | Small Word -> offset
      | _ -> Int64.(offset + 4L)
      )
    in
    (match Symbol.Map.add fields ~key:field ~data:(typ, size, offset) with
      | `Ok fields -> fields
      | `Duplicate -> tc_error (sprintf "Declaration of struct %s has duplicate fields" (Symbol.name sname))
    ), Int64.(offset + Size.to_int64 size)
  ) in
  (* align the struct to 8 bytes by adding padding -- assumes 4 or 8 alignment *)
  (fields, Int64.(total_bytes + (total_bytes % 8L)))

and type_to_size : AM.typ -> Size.data_size = function
  | Int -> Small Size.Long
  | Bool -> Small Size.Long
  | Ident _id -> failwith "typedefs got lost somewhere good luck"
  | Ptr _ -> Small Size.Quad 
  | Array _ -> Small Size.Quad
  | Struct s -> (
    match Hashtbl.find structs s with
    | None -> tc_error (sprintf "Struct %s not defined before use in other struct" (Symbol.name s))
    | Some (_, size) -> Struct size
  )
  | Arrow _ -> Small Size.Quad
  | Anyptr -> failwith "Tried to get size of anyptr type"
;;

let get_field_type sname field ~marked : T.t =
  (match Hashtbl.find structs sname with
          | None -> tc_error ~marked (sprintf "Struct %s is not defined" (Symbol.name sname))
          | Some (field_map, _ssize) -> match Symbol.Map.find field_map field with
            | None -> tc_error ~marked (sprintf "Struct %s doesn't have field %s" (Symbol.name sname) (Symbol.name field))
            | Some (typ, _size, _offset) -> typ
        )
;;

(* ***************************************** *)
(* ********** Typecheck functions ********** *)
(* ***************************************** *)

(* let rec check_mem (mem : AM.mmem) (typ : T.t) : AT.mmem =
  let tmem = infer_mem mem in
  if equal_typ typ (T.Tag.untag tmem)
  then tmem
  else
    tc_error ~marked:mem
      (sprintf "Expected lval to be type %s but got type %s"
        (AM.Print.pp_typ typ)
        (AM.Print.pp_typ (T.Tag.untag tmem))) *)

let rec infer_mem (mem : AM.mmem) vars : AT.mmem = match (Mark.data mem) with
  | Get_field (mmem, field) ->
    let tmem = infer_mem mmem vars in (
      match T.Tag.untag tmem with
      | Struct sname ->
        let typ = get_field_type sname field ~marked:mmem in
        T.Tag.tag (AT.Get_field (tmem, field)) typ
      | t -> tc_error ~marked:mem (sprintf "Expected struct but got %s" (T.pp_typ t))
    )
  | Deref_field (mexp, field) ->
    let texp = infer_exp mexp vars in (
      match T.Tag.untag texp with
      | Ptr (Struct sname) ->
        let typ = get_field_type sname field ~marked:mexp in
        T.Tag.tag (AT.Deref_field (texp, field)) typ
      | t -> tc_error ~marked:mem (sprintf "Expected struct ptr but got %s" (T.pp_typ t))
    )
  | Deref mexp ->
    let texp = infer_exp mexp vars in (
      match T.Tag.untag texp with
      | Ptr t -> T.Tag.tag (AT.Deref texp) t
      | t -> tc_error ~marked:mem (sprintf "Expected ptr but got %s" (T.pp_typ t))
    )
  | Index { array; index } ->
    let array = infer_exp array vars in
    let index = check_exp index T.Int vars in
    match T.Tag.untag array with
    | Array t -> T.Tag.tag (AT.Index {array; index}) t
    | t -> tc_error ~marked:mem (sprintf "Expected array but got %s" (T.pp_typ t))

and check_lval (lval : AM.mlval) (typ : T.t) vars : AT.mlval =
  let tlval = infer_lval lval vars in
  if equal_typ typ (T.Tag.untag tlval)
  then tlval
  else
    tc_error ~marked:lval
      (sprintf "Expected lval to be type %s but got type %s"
        (AM.Print.pp_typ typ)
        (AM.Print.pp_typ (T.Tag.untag tlval)))

and infer_lval (lval : AM.mlval) vars : AT.mlval = match (Mark.data lval) with
  | LVar id ->
    (match S.find vars id with
     | None -> tc_error ~marked:lval (sprintf "Not declared before use: `%s`" (Symbol.name id))
     | Some (Decl _) -> tc_error ~marked:lval (sprintf "Not initialized before use: `%s`" (Symbol.name id))
     | Some (Init t) -> T.Tag.tag (AT.LVar id) t)
  | LGet_field (mlval, field) ->
    let tlval = infer_lval mlval vars in (
      match (T.Tag.untag tlval) with
      | Struct sname ->
        let typ = get_field_type sname field ~marked:lval in
        T.Tag.tag (AT.LGet_field (tlval, field)) typ
      | t -> tc_error ~marked:lval (sprintf "Expected struct but got %s" (T.pp_typ t))
    )
  | LDeref_field (mlval, field) ->
    let tlval = infer_lval mlval vars in (
      match T.Tag.untag tlval with
      | Ptr (Struct sname) ->
        let typ = get_field_type sname field ~marked:lval in
        T.Tag.tag (AT.LDeref_field (tlval, field)) typ
      | t -> tc_error ~marked:lval (sprintf "Expected struct ptr but got %s" (T.pp_typ t))
    )
  | LDeref mlval ->
    let tlval = infer_lval mlval vars in (
      match T.Tag.untag tlval with
      | Ptr t -> T.Tag.tag (AT.LDeref tlval) t
      | t -> tc_error ~marked:lval (sprintf "Expected non-void ptr but got %s" (T.pp_typ t))
    )
  | LIndex { array; index } ->
    let array = infer_lval array vars in
    let index = check_exp index T.Int vars in
    match T.Tag.untag array with
    | Array t -> T.Tag.tag (AT.LIndex {array; index}) t
    | t -> tc_error ~marked:lval (sprintf "Expected array but got %s" (T.pp_typ t))

and check_exp exp (typ : T.t) vars : AT.mexp =
  let texp = infer_exp exp vars in
  if equal_typ typ (T.Tag.untag texp)
  then texp
  else
    tc_error ~marked:exp
      (sprintf "Expected expression to be type %s but got type %s"
        (AM.Print.pp_typ typ)
        (AM.Print.pp_typ (T.Tag.untag texp)))

and infer_exp ?(void_ok=false) (ast : AM.mexp) (vars : init_status S.t) : AT.mexp =
  let res =
  match Mark.data ast with
  | AM.True -> T.Tag.tag AT.True T.Bool
  | AM.False -> T.Tag.tag AT.False T.Bool
  | AM.Const n -> T.Tag.tag (AT.Const n) T.Int
  | AM.Fn_name _ -> failwith "Parser parsed Fn_name but it should parse all Var's into Var"
  | AM.Binop { op; lhs; rhs } ->
    let expect lhs_typ rhs_typ output_typ op =
      let lhs = check_exp lhs lhs_typ vars in
      let rhs = check_exp rhs rhs_typ vars in
      T.Tag.tag (AT.Binop { op; lhs; rhs }) output_typ
    in
    (match op with
     (* int * int -> int *)
     | Plus -> expect T.Int T.Int T.Int AT.Plus
     | Minus -> expect T.Int T.Int T.Int AT.Minus
     | Times -> expect T.Int T.Int T.Int AT.Times
     | Divided_by -> expect T.Int T.Int T.Int AT.Divided_by
     | Modulo -> expect T.Int T.Int T.Int AT.Modulo
     | Bit_and -> expect T.Int T.Int T.Int AT.Bit_and
     | Bit_or -> expect T.Int T.Int T.Int AT.Bit_or
     | Bit_xor -> expect T.Int T.Int T.Int AT.Bit_xor
     | Lshift -> expect T.Int T.Int T.Int AT.Lshift
     | Rshift -> expect T.Int T.Int T.Int AT.Rshift
     (* int * int -> bool *)
     | Less -> expect T.Int T.Int T.Bool AT.Less
     | Greater -> expect T.Int T.Int T.Bool AT.Greater
     | Leq -> expect T.Int T.Int T.Bool AT.Leq
     | Geq -> expect T.Int T.Int T.Bool AT.Geq
     (* 'a * 'a -> bool; just make sure lhs rhs agree *)
     | Equal ->
        let lhs = infer_exp lhs vars in
        let rhs = check_exp rhs (T.Tag.untag lhs) vars in
        T.Tag.tag (AT.Binop { op=AT.Equal; lhs; rhs }) T.Bool
     | Neq ->
        let lhs = infer_exp lhs vars in
        let rhs = check_exp rhs (T.Tag.untag lhs) vars in
        T.Tag.tag (AT.Binop { op=AT.Neq; lhs; rhs }) T.Bool
     (* bool * bool -> bool *)
     | And -> expect T.Bool T.Bool T.Bool AT.And
     | Or -> expect T.Bool T.Bool T.Bool AT.Or)
  | AM.Unop { op; operand } ->
    let expect operand_typ output_typ op =
      let operand = check_exp operand operand_typ vars in
      T.Tag.tag (AT.Unop { op; operand }) output_typ
    in
    (match op with
     (* bool -> bool *)
     | Not -> expect T.Bool T.Bool AT.Not
     (* int -> int *)
     | Negative -> expect T.Int T.Int AT.Negative
     | Bit_not -> expect T.Int T.Int AT.Bit_not)
  | AM.Ternary { if_exp; then_exp; else_exp } ->
    let if_exp = check_exp if_exp T.Bool vars in
    let then_exp = infer_exp then_exp vars in
    let else_exp = check_exp else_exp (T.Tag.untag then_exp) vars in
    let typ = match (T.Tag.untag then_exp, T.Tag.untag else_exp) with
        | (Anyptr, Ptr t) | (Ptr t, Anyptr) -> T.Ptr t
        | (t, _) -> t
    in
    T.Tag.tag (AT.Ternary { if_exp; then_exp; else_exp }) typ
  | AM.Var id ->
    (match S.find vars id with
     | None -> (
      match Hashtbl.find fn_decls id with
      | None -> tc_error ~marked:ast (sprintf "Not declared before use: `%s`" (Symbol.name id))
      | Some (defn_status, (ret_typ, params)) ->
        Hashtbl.set fn_decls ~key:id ~data:(defn_status_max defn_status Used, (ret_typ, params));
        T.Tag.tag (AT.Fn_name id) (T.Arrow (List.map params ~f:fst, ret_typ))
      )
     | Some (Decl _) -> tc_error ~marked:ast (sprintf "Not initialized before use: `%s`" (Symbol.name id))
     | Some (Init t) -> T.Tag.tag (AT.Var id) t)
  | AM.Fn_call (exp, args) ->
    let exp = infer_exp exp vars in (
    match T.Tag.untag_opt exp with
    | Some (T.Arrow (par_typs, ret_typ)) -> (
      match List.map2 par_typs args ~f:(fun param_typ arg_mexp ->
        check_exp arg_mexp param_typ vars
      ) with
      | Unequal_lengths -> tc_error ~marked:ast "Function call has an unexpected number of arguments."
      | Ok args ->
        (match T.Tag.data exp with
        | AT.Fn_name name -> (
          match Hashtbl.find fn_decls name with
          | None -> tc_error ~marked:ast (sprintf "Not declared before use: `%s`" (Symbol.name name))
          | Some (defn_status, (ret_typ, params)) ->
            Hashtbl.set fn_decls ~key:name ~data:(defn_status_max defn_status Used, (ret_typ, params))
        )
        | _ -> ());
        match ret_typ with
        | T.Void when void_ok -> T.Tag.tag_naked (AT.Fn_call (exp, args))
        | T.Void -> tc_error ~marked:ast "Unexpected void type."
        | T.Typ t -> T.Tag.tag (AT.Fn_call (exp, args)) t
    )
    | None | Some _ -> tc_error ~marked:ast "Not a function type, but is being applied."
  )
  | Mem mmem -> let mmem = infer_mem mmem vars in T.Tag.tag (AT.Mem mmem) (T.Tag.untag mmem)
  | Null -> T.Tag.tag AT.Null T.Anyptr
  | Alloc t ->
    let t = get_typedef_defn t in
    (match t with
      | Struct s -> (match Hashtbl.find structs s with
        | Some _ -> ()
        | None -> tc_error (sprintf "Size of struct %s not known during allocation" (Symbol.name s)))
      | _ -> ()
    );
    T.Tag.tag (AT.Alloc t) (T.Ptr t)
  | Alloc_array { typ; size=mexp } ->
    let typ = get_typedef_defn typ in
    (match typ with
      | Struct s -> (match Hashtbl.find structs s with
        | Some _ -> ()
        | None -> tc_error (sprintf "Size of struct %s not known during allocation" (Symbol.name s)))
      | _ -> ()
    );
    let texp = check_exp mexp T.Int vars in
    T.Tag.tag (AT.Alloc_array {typ; size=texp}) (T.Array typ)
  | Lam (ret_typ, params, stms) ->
    let (ret_typ, _, params, body) = tc_fn (ret_typ, "<lambda>", params, stms) in
    T.Tag.tag (AT.Lam (ret_typ, params, body)) (T.Arrow (List.map params ~f:fst, ret_typ))
  in
  match T.Tag.untag_opt res with
    | Some (T.Struct s) -> tc_error (sprintf "Cannot use big type struct %s directly" (Symbol.name s))
    | _ -> res

and mark_to_tag_asnop (op : AM.asnop) : AT.asnop = match op with
  | Plus_eq -> Plus_eq
  | Minus_eq -> Minus_eq
  | Times_eq -> Times_eq
  | Div_eq -> Div_eq
  | Mod_eq -> Mod_eq
  | Bit_and_eq -> Bit_and_eq
  | Bit_or_eq -> Bit_or_eq
  | Bit_xor_eq -> Bit_xor_eq
  | Lshift_eq -> Lshift_eq
  | Rshift_eq -> Rshift_eq

and tc_simple mark s env : fn_env * (AT.simple) = match s with
  | AM.Declare (AM.New_var (typ, id)) ->
    tc_error_if_declared id typedefs;
    (* Make sure the type is defined. *)
    let typ = get_typedef_defn typ ~marked:mark in
    (* Make sure type isn't big. *)
    (match type_to_size typ with
      | Small _ -> ()
      | Struct _ -> tc_error ~marked:mark "Big type size not allowed on stack"
    );
    (match S.find env.vars id with
    | Some _ -> tc_error ~marked:mark (sprintf "Already declared: `%s`" (Symbol.name id))
    | None ->
      let new_env = { env with vars = S.set env.vars ~key:id ~data:(Decl typ) } in
      (new_env, AT.Declare (AT.New_var (typ, id)))
    )
  | AM.Declare (AM.Init (typ, id, e)) ->
    tc_error_if_declared id typedefs;
    let typ = get_typedef_defn typ ~marked:mark in
    (match S.find env.vars id with
    | Some _ -> tc_error ~marked:mark (sprintf "Already declared: `%s`" (Symbol.name id))
    | None -> (
      let e = check_exp e typ env.vars in
      let new_env = { env with vars = S.set env.vars ~key:id ~data:(Init typ) } in
      (new_env, AT.Declare (AT.Init (typ, id, e)))
    ))
  | AM.Assign (lval, e) -> (
    match Mark.data lval with 
    | LVar id ->
      (match S.find env.vars id with
      | None -> tc_error ~marked:mark (sprintf "Not declared before initialization: `%s`" (Symbol.name id))
      | Some (Decl t) ->
        let t = get_typedef_defn t in
        let e = check_exp e t env.vars in
        let new_env = { env with vars = S.set env.vars ~key:id ~data:(Init t) } in
        let lval = infer_lval lval new_env.vars in
        (new_env, AT.Assign (lval, e))
      | Some (Init t) ->
        let e = check_exp e t env.vars in
        let lval = infer_lval lval env.vars in
        (env, AT.Assign (lval, e)))
    | _ ->
      let lval = infer_lval lval env.vars in
      let e = check_exp e (T.Tag.untag lval) env.vars in
      (env, AT.Assign (lval, e))
    )
  | AM.Do mexp ->
    let mexp = infer_exp ~void_ok:true mexp env.vars in
    (env, AT.Do mexp)
  | Asnop {lhs=mlval; op; rhs=mexp} ->
    let tlval = check_lval mlval (T.Int) env.vars in
    let texp = check_exp mexp (T.Int) env.vars in
    (* Don't update env.vars because lval should be Init already. *)
    (env, AT.Asnop {lhs=tlval; op=mark_to_tag_asnop op; rhs=texp})

(* tc_stms env ast
 *   env.vars: environment under which to consider the ast, where:
 *     find env.vars id = Some Init if id is declared and initialized
 *     find env.vars id = Some Decl if id is declared but not initialized
 *     find env.vars id = None      if id is not declared
 *
 *   env.returns
 *     whether the previous statements returned.
 *
 *   ast: the sequence of statements to typecheck.
 *)
and tc_stm (stm : AM.mstm) (env : fn_env) : fn_env * AT.mstm =
  (* Use this function when exiting a scope.
  * It saves initializations and removes declarations. *)
  let merge_ctx ~outer ~inner =
    let f ~key:_ = function
      | `Both (_, Init t) -> Some (Init t)
      | `Both (v, Decl _) -> Some v
      | `Left v -> Some v
      | `Right _ -> None
    in
    S.merge outer inner ~f
  in
  (match Mark.data stm with

  | AM.Block block_stms ->
    let (env', tagged_block) = tc_stms block_stms env in
    let new_vars = merge_ctx ~outer:env.vars ~inner:env'.vars in
    let new_env = { returns = env'.returns; vars = new_vars; return_type = env.return_type } in
    (new_env, Type.Tag.tag_naked (AT.Block tagged_block))

  | AM.Simple s -> let (env, simple) = tc_simple stm s env in
    (env, T.Tag.tag_naked (AT.Simple simple))

  | Control c ->
    (match c with
      | If (cond, if_stm, None) ->
        let cond = check_exp cond T.Bool env.vars in
        let (_, if_stm) = tc_stm if_stm env in
        (env, T.Tag.tag_naked (AT.Control (AT.If (cond, if_stm, None))))
      | If (cond, if_stm, Some else_stm) ->
        let cond = check_exp cond T.Bool env.vars in
        let (if_env, if_stm) = tc_stm if_stm env in
        let (else_env, else_stm) = tc_stm else_stm env in
        let f ~key:_ = function
          | `Both (Init t, Init _) -> Some (Init t)
          | _ -> None
        in
        let intersect_vars = S.merge ~f if_env.vars else_env.vars in
        let new_env =
          { returns = if_env.returns && else_env.returns
          ; return_type = env.return_type
          ; vars = merge_ctx ~outer:env.vars ~inner:intersect_vars
          }
        in
        (new_env, T.Tag.tag_naked (AT.Control (AT.If (cond, if_stm, Some else_stm))))
      | While (cond, body_stm) ->
        let cond = check_exp cond T.Bool env.vars in
        let (_, body_stm) = tc_stm body_stm env in
        (env, T.Tag.tag_naked (AT.Control (AT.While (cond, body_stm))))
      | For (init_stm, cond, iter_stm, body_stm) ->
        (* First process init_stm since it may affect context *)
        let (loop_env, init_stm) = match init_stm with
          | None -> (env, None)
          | Some simple -> let (env, stm) = tc_simple stm simple env in (env, Some stm)
        in
        (* Check that the condition is a boolean *)
        let cond = check_exp cond T.Bool loop_env.vars in
        (* Check the body_stm and use the updated environment to check iter_stm *)
        let (iter_env, body_stm) = tc_stm body_stm loop_env in
        (* Check iter_stm *)
        let iter_stm = match iter_stm with
          | None -> None
          | Some (Declare _) -> tc_error ~marked:stm "The third for-loop argument can't be a declaration."
          | Some simple -> Some (snd (tc_simple stm simple iter_env)) in
        (* Update env *)
        let final_env = { env with vars = merge_ctx ~outer:env.vars ~inner:loop_env.vars } in
        (final_env, T.Tag.tag_naked (AT.Control (AT.For (init_stm, cond, iter_stm, body_stm))))
      | Return mexp_opt -> (
        let (ret_type, mexp_opt) = match mexp_opt with
          | None -> (T.Void, None)
          | Some mexp -> let mexp = infer_exp mexp env.vars in
            (T.Typ (T.Tag.untag mexp), Some mexp)
        in
        (* Make sure we are returning the correct type *)
        if not (equal_ret_typ ret_type env.return_type)
        then tc_error ~marked:stm (sprintf "Expected return type %s but got %s" (T.pp_ret_typ env.return_type) (T.pp_ret_typ ret_type))
        ;
        let final_env =
          { vars =
              (* Init everything when we return *)
              S.map env.vars ~f:(function
                | Decl t -> Init t
                | x -> x)
          ; returns = true
          ; return_type = env.return_type
          }
        in
        (final_env, T.Tag.tag_naked (AT.Control (AT.Return mexp_opt))))
      | Assert mexp ->
        let mexp = check_exp mexp T.Bool env.vars in
        (env, T.Tag.tag_naked (AT.Control (AT.Assert mexp)))
    ))

and tc_stms (ast : AM.mstm list) (env : fn_env) : fn_env * AT.stms =
  List.fold ast ~init:(env, []) ~f:(fun (env, tstms) stm ->
    let (env, tstm) = tc_stm stm env in
    (env, tstm :: tstms)
  ) |> (fun (env, stms) -> (env, List.rev stms))

and tc_fn (ret_type, name_str, params, stms) =
  let vars = S.of_alist_exn (List.map params ~f:(fun (typ, arg) -> (arg, Init typ))) in
  let (env, stms) = tc_stms stms {vars; returns=false; return_type=ret_type} in
  if not env.returns && not (equal_ret_typ env.return_type T.Void)
  then tc_error (sprintf "function %s does not return" name_str)
  else (ret_type, name_str, params, stms)
;;

(** Typechecks gdecl and updates the contexts in the global_env. *)
let tc_gdecl
  ?(is_header=false)
  (gdecl : AM.gdecl)
: AT.gdecl =
  match gdecl with
  | Fdecl (ret_typ, name, params) ->
    let ret_typ = get_ret_typedef_defn ret_typ in
    let params  = List.map params ~f:(fun (typ, name) -> (get_typedef_defn typ, name)) in
    check_valid_fn_decl name (ret_typ, params);
    (* If we are in the header, we need to set everything to Defined. *)
    let defn_status = if is_header then Defined else Unused in
    (* If we are in the header, update header_fns with the decl *)
    if is_header then Hash_set.add header_fns name;
    (* Update the defn_status. *)
    Hashtbl.update fn_decls name
      ~f:(fun opt -> match opt with
      | None -> (defn_status, (ret_typ, params))
      | Some (defn_status', _) -> (defn_status_max defn_status defn_status', (ret_typ, params))
      );
    AT.Fdecl (ret_typ, name, params)
  | Fdefn (ret_typ, name, params, body) ->
    let ret_typ = get_ret_typedef_defn ret_typ in
    let params  = List.map params ~f:(fun (typ, name) -> (get_typedef_defn typ, name)) in
    check_valid_fn_decl ~no_prev_defn:() name (ret_typ, params);
    if is_header (* Disallow definition or update defn_status. *)
    then tc_error (sprintf "Defn not allowed in header; found %s." (Symbol.name name))
    else Hashtbl.set fn_decls ~key:name ~data:(Defined, (ret_typ, params));
    let (ret_typ, _, params, body) = tc_fn (ret_typ, Symbol.name name, params, body) in 
    AT.Fdefn (ret_typ, name, params, body) (* Typecheck function body. *)
  | Typedef (typ, name) ->
    tc_error_if_declared name fn_decls;
    let typ = get_typedef_defn typ in
    (match Hashtbl.add typedefs ~key:name ~data:typ with
    | `Duplicate -> tc_error (sprintf "Type %s defined more than once." (Symbol.name name))
    | `Ok -> AT.Typedef (typ, name))
  | Sdecl name -> AT.Sdecl name
  | Sdefn (name, fields) ->
    match Hashtbl.add structs ~key:name ~data:(calc_struct_size name fields) with
    | `Duplicate -> tc_error (sprintf "Struct %s defined more than once" (Symbol.name name))
    | `Ok -> AT.Sdefn (name, fields)

let typecheck ~header ast : output =
  (* Typecheck header and lose tagged ast output *)
  (match header with
    | Some header_ast ->
      List.fold header_ast ~init:() ~f:(fun () gdecl ->
        let _tagged = (tc_gdecl ~is_header:true gdecl) in ()
      )
    | None -> ());
  (* Make sure main is included as expected *)
  Hashtbl.update fn_decls (Symbol.symbol "main")
    ~f:(fun opt -> match opt with
      | None -> (Used, (T.Typ T.Int, []))
      | Some (_) -> tc_error "Function main was declared in header"
    );
  (* Typecheck source *)
  let tagged_ast = List.fold ast ~init:[]
    ~f:(fun gdecls gdecl -> (tc_gdecl gdecl) :: gdecls)
    |> List.rev
  in
  (* Confirm that all functions that were used were defined. *)
  let fn_types = Hashtbl.mapi fn_decls
    ~f:(fun ~key:name ~data:(ds, (typ, _)) -> match ds with
      | Used -> tc_error (sprintf "Function %s is used but not defined" (Symbol.name name))
      | _ -> typ
    )
  in
  let structs = Hashtbl.map structs
    ~f:(fun (fields_map, struct_size) ->
      Symbol.Map.map fields_map
        ~f:(fun (_typ, field_size, field_offset) ->
          field_size, field_offset
        ),
      struct_size
    )
  in
  (* (print_endline
  (Print.pp_symbol_hashtbl ~btwn:"\n" ~back:"\n" structs (fun (map, size) ->
    sprintf "Size %s\n" (Int64.to_string size)
    ^ Symbol.Map.fold map ~init:"" ~f:(fun ~key:field ~data:(size, offset) acc ->
      acc ^
      (sprintf "\t%s size:%s offset:%s\n"
        (Symbol.name field)
        (Size.format_data size)
        (Int64.to_string offset))
    )
  ))); *)
  { tagged_ast
  ; header_decls = header_fns
  ; fn_types
  ; typedefs
  ; structs
  }
;;
