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

open Core
module A = Ast
module S = Symbol.Map

(* ************************************* *)
(* *************** Types *************** *)
(* ************************************* *)

type init_status =
  | Decl of A.typ
  | Init of A.typ

type defn_status =
  | Unused
  | Used    (* Bad ending state *)
  | Defined
type fn_info = A.typ * A.param list
type global_env =
  { fn_decls : (Symbol.t, defn_status * fn_info) Hashtbl.t (* Mutable *)
  ; typedefs : (Symbol.t, A.typ  ) Hashtbl.t (* Mutable *)
  }

type fn_env =
  { vars : init_status S.t
  ; returns : bool
  ; return_type : A.typ
  }

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

let equal_typ typedefs t1 t2 =
  let t1opt = match t1 with
    | A.Ident t -> Hashtbl.find typedefs t
    | t -> Some t
  in
  let t2opt = match t2 with
    | A.Ident t -> Hashtbl.find typedefs t
    | t -> Some t
  in
  match (t1opt, t2opt) with
    | (Some t1, Some t2) -> A.equal_typ t1 t2
    | _ -> false

(** Raises a typecheck error if the name is found in the context. *)
let tc_error_if_declared name (ctx : (Symbol.t, 'a) Hashtbl.t) =
  match Hashtbl.find ctx name with
  | None -> ()
  | Some _ -> tc_error (sprintf "Repeat declaration of %s" (Symbol.name name)) 

let init_global_env () =
  { fn_decls = Hashtbl.create (module Symbol)
  ; typedefs = Hashtbl.create (module Symbol)
  }

(* ***************************************** *)
(* ********** Typecheck functions ********** *)
(* ***************************************** *)

let rec infer_exp ?(void_ok=false) {fn_decls; typedefs} (ast : A.mexp) (vars : init_status S.t) : A.typ =
  match Mark.data ast with
  | A.True -> A.Bool
  | A.False -> A.Bool
  | A.Const _ -> A.Int
  | A.Binop { op; lhs; rhs } ->
    let expect t1 t2 t3 =
      check_exp {fn_decls; typedefs} lhs t1 vars;
      check_exp {fn_decls; typedefs} rhs t2 vars;
      t3
    in
    (match op with
     (* int * int -> int *)
     | Plus -> expect A.Int A.Int A.Int
     | Minus -> expect A.Int A.Int A.Int
     | Times -> expect A.Int A.Int A.Int
     | Divided_by -> expect A.Int A.Int A.Int
     | Modulo -> expect A.Int A.Int A.Int
     | Bit_and -> expect A.Int A.Int A.Int
     | Bit_or -> expect A.Int A.Int A.Int
     | Bit_xor -> expect A.Int A.Int A.Int
     | Lshift -> expect A.Int A.Int A.Int
     | Rshift -> expect A.Int A.Int A.Int
     (* int * int -> bool *)
     | Less -> expect A.Int A.Int A.Bool
     | Greater -> expect A.Int A.Int A.Bool
     | Leq -> expect A.Int A.Int A.Bool
     | Geq -> expect A.Int A.Int A.Bool
     (* 'a * 'a -> bool; just make sure lhs rhs agree *)
     | Equal ->
       check_exp {fn_decls; typedefs} lhs (infer_exp {fn_decls; typedefs} rhs vars) vars;
       A.Bool
     | Neq ->
       check_exp {fn_decls; typedefs} lhs (infer_exp {fn_decls; typedefs} rhs vars) vars;
       A.Bool
     (* bool * bool -> bool *)
     | And -> expect A.Bool A.Bool A.Bool
     | Or -> expect A.Bool A.Bool A.Bool)
  | A.Unop { op; operand } ->
    (match op with
     (* bool -> bool *)
     | Not ->
       check_exp {fn_decls; typedefs} operand A.Bool vars;
       A.Bool
     (* int -> int *)
     | Negative ->
       check_exp {fn_decls; typedefs} operand A.Int vars;
       A.Int
     | Bit_not ->
       check_exp {fn_decls; typedefs} operand A.Int vars;
       A.Int)
  | A.Ternary { if_exp; then_exp; else_exp } ->
    check_exp {fn_decls; typedefs} if_exp A.Bool vars;
    let branch_type = infer_exp {fn_decls; typedefs} then_exp vars in
    check_exp {fn_decls; typedefs} else_exp branch_type vars;
    branch_type
  | A.Var id ->
    (match S.find vars id with
     | None -> tc_error ~marked:ast (sprintf "Not declared before use: `%s`" (Symbol.name id))
     | Some (Decl _) ->
      tc_error ~marked:ast (sprintf "Not initialized before use: `%s`" (Symbol.name id))
     | Some (Init t) -> t)
  | A.Fn_call (name, args) ->
    match infer_fn_exp ast {fn_decls; typedefs} vars (name, args) with
    | A.Void ->
      if void_ok
      then A.Void 
      else tc_error ~marked:ast (sprintf "Expected a return value but %s doesn't return" (Symbol.name name))
    | t -> t

and infer_fn_exp mark {fn_decls; typedefs} vars (name, args) =
  (* print_endline (pp_fn_decls fn_decls); *)
  match Hashtbl.find fn_decls name with
  | None -> tc_error ~marked:mark (sprintf "Not declared before use: `%s`" (Symbol.name name))
  | Some (defn_status, (ret_typ, params)) ->
    match S.find vars name with
    | Some _ -> tc_error ~marked:mark (sprintf "Function %s shadowed by local variable" (Symbol.name name))
    | None ->
      (
        match List.for_all2 params args ~f:(fun (typ, _name) (arg : A.mexp) ->
          (* Check the arg type matches the function param type *)
          check_exp {fn_decls; typedefs} arg typ vars; true
        ) with
        | Ok _ ->
          Hashtbl.set fn_decls ~key:name ~data:(defn_status_max defn_status Used, (ret_typ, params));
          ret_typ
        | Unequal_lengths -> tc_error ~marked:mark "Function call has an unexpected number of arguments."
      )

and check_exp {fn_decls; typedefs} exp typ vars =
  let typ' = infer_exp {fn_decls; typedefs} exp vars in
  if equal_typ typedefs typ typ'
  then ()
  else
    tc_error ~marked:exp
      (sprintf "Expected expression to be type %s but got type %s"
        (A.Print.pp_typ typ)
        (A.Print.pp_typ typ'))
;;

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
let tc_stms {fn_decls; typedefs} = (* Use same global_env, since it's mutable *)
  let rec tc_stms (ast : Ast.mstm list) (env : fn_env) : fn_env =
    match ast with
    | [] -> env
    | stm :: stms ->
      (* Propogate the same mark. *)
      let remark exp = Mark.map stm ~f:(fun _ -> exp) in
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
      | A.Block block_stms ->
        let env' = tc_stms block_stms env in
        let new_vars = merge_ctx ~outer:env.vars ~inner:env'.vars in
        let new_env = { returns = env'.returns; vars = new_vars; return_type = env.return_type } in
        tc_stms stms new_env
      | A.Simple s ->
        (match s with
          | A.Declare (A.New_var (typ, id)) ->
            tc_error_if_declared id typedefs;
            (* Make sure the type is defined. *)
            (match typ with
            | A.Ident name -> (
              match Hashtbl.find typedefs name with
              | None -> tc_error ~marked:stm (sprintf "Type %s isn't defined" (Symbol.name name))
              | Some _ -> ()
            )
            | _ -> ());
            (match S.find env.vars id with
            | Some _ -> tc_error ~marked:stm (sprintf "Already declared: `%s`" (Symbol.name id))
            | None ->
              tc_stms stms { env with vars = S.set env.vars ~key:id ~data:(Decl typ) })
          | A.Declare (A.Init (typ, id, e)) ->
            tc_error_if_declared id typedefs;
            (match S.find env.vars id with
            | Some _ -> tc_error ~marked:stm (sprintf "Already declared: `%s`" (Symbol.name id))
            | None -> 
              check_exp {fn_decls; typedefs} e typ env.vars;
              tc_stms stms { env with vars = S.set env.vars ~key:id ~data:(Init typ) }
            )
          | A.Assign (id, e) ->
            (match S.find env.vars id with
            | None -> tc_error ~marked:stm (sprintf "Not declared before initialization: `%s`" (Symbol.name id))
            | Some (Decl t) ->
              check_exp {fn_decls; typedefs} e t env.vars;
              tc_stms stms { env with vars = S.set env.vars ~key:id ~data:(Init t) }
            | Some (Init t) ->
              check_exp {fn_decls; typedefs} e t env.vars;
              tc_stms stms env)
          | A.Do mexp ->
            let _ : A.typ = infer_exp ~void_ok:true {fn_decls; typedefs} mexp env.vars in
            tc_stms stms env)
      | Control c ->
        (match c with
          | If (cond, if_stm, None) ->
            check_exp {fn_decls; typedefs} cond A.Bool env.vars;
            let _ : fn_env = tc_stms [ if_stm ] env in
            tc_stms stms env
          | If (cond, if_stm, Some else_stm) ->
            check_exp {fn_decls; typedefs} cond A.Bool env.vars;
            let if_env = tc_stms [ if_stm ] env in
            let else_env = tc_stms [ else_stm ] env in
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
            tc_stms stms new_env
          | While (cond, body_stm) ->
            check_exp {fn_decls; typedefs} cond A.Bool env.vars;
            let (_ : fn_env) = tc_stms [ body_stm ] env in
            tc_stms stms env
          | For (init_stm, cond, iter_stm, body_stm) ->
            (* First process init_stm since it may affect context *)
            let loop_env = match init_stm with
              | None -> env
              | Some simple ->
                  let init_stm = remark (A.Simple simple) in
                  tc_stms [init_stm] env
            in
            (* Check that the condition is a boolean *)
            check_exp {fn_decls; typedefs} cond A.Bool loop_env.vars;
            (* Put the iter_stm and body_stm into a list together *)
            let body_stms =
              match iter_stm with
              | None -> [body_stm]
              | Some (Declare _) -> tc_error ~marked:stm "The third for-loop argument can't be a declaration."
              | Some simple ->  [body_stm; (remark (A.Simple simple))]
            in
            (* Check that they are okay, but discard changes to env *)
            let (_ : fn_env) = tc_stms body_stms loop_env in
            (* Continue typechecking *)
            tc_stms stms { env with vars = merge_ctx ~outer:env.vars ~inner:loop_env.vars }
          | Return mexp_opt -> (
            let ret_type = match mexp_opt with
              | None -> A.Void
              | Some mexp -> infer_exp {fn_decls; typedefs} mexp env.vars
            in
            (* Make sure we are returning the correct type *)
            if not (equal_typ typedefs ret_type env.return_type)
            then tc_error ~marked:stm "Unexpected return type"
            ;
            tc_stms
              stms
              { vars =
                  (* Init everything when we return *)
                  S.map env.vars ~f:(function
                    | Decl t -> Init t
                    | x -> x)
              ; returns = true
              ; return_type = env.return_type
              }
            )
          | Assert mexp ->
            check_exp {fn_decls; typedefs} mexp A.Bool env.vars;
            tc_stms stms env
        ))
  in
  tc_stms
;;

let tc_fn global_env (ret_type, name, params, stms) =
  let vars = S.of_alist_exn (List.map params ~f:(fun (typ, arg) -> (arg, Init typ))) in
  let env = tc_stms global_env stms { vars; returns = false; return_type = ret_type } in
  if not env.returns && not (A.equal_typ env.return_type A.Void)
  then tc_error (sprintf "function %s does not return" (Symbol.name name))
;;

(** Check if a function declaration is valid. *)
let check_valid_fn_decl ?no_prev_defn {fn_decls; typedefs} fn_name (typ, params) =
  (* Make sure the function is not shadowing a type. *)
  tc_error_if_declared fn_name typedefs;
  (* See if the function is declared already and make sure params match. *)
  (match Hashtbl.find fn_decls fn_name with
  | None -> ()
  | Some (defn_status, (typ', params')) ->
    (* If wanted, check that the function hasn't been previously defined *)
    (match (no_prev_defn, defn_status) with
    | (Some (), Defined) -> tc_error (sprintf "Function %s is defined twice" (Symbol.name fn_name))
    | _ -> ()
    );
    (* Check for type equality *)
    if equal_typ typedefs typ typ'
    (* Check for parameter equality *)
    then match List.iter2 params params' ~f:(fun (t1, _) (t2, _) -> (* Param name doesn't matter *)
        if equal_typ typedefs t1 t2
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
    | A.Ident typ ->
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

(** Typechecks gdecl and updates the contexts in the global_env. *)
let tc_gdecl
  ?(is_header=false)
  (gdecl : A.gdecl)
  {fn_decls; typedefs;}
: unit =
  match gdecl with
  | Fdecl (ret_typ, name, params) ->
    check_valid_fn_decl {fn_decls; typedefs;} name (ret_typ, params);
    (* If we are in the header, we need to set everything to Defined. *)
    let defn_status = if is_header then Defined else Unused in
    (* Update the defn_status. *)
    Hashtbl.update fn_decls name
      ~f:(fun opt -> match opt with
      | None -> (defn_status, (ret_typ, params))
      | Some (defn_status', _) -> (defn_status_max defn_status defn_status', (ret_typ, params))
      )
  | Fdefn (ret_typ, name, params, body) -> 
    check_valid_fn_decl ~no_prev_defn:() {fn_decls; typedefs;} name (ret_typ, params);
    if is_header (* Disallow definition or update defn_status. *)
    then tc_error (sprintf "Defn not allowed in header; found %s." (Symbol.name name))
    else Hashtbl.set fn_decls ~key:name ~data:(Defined, (ret_typ, params));
    tc_fn {fn_decls; typedefs} (ret_typ, name, params, body) (* Typecheck function body. *)
  | Typedef (typ, name) ->
    let typ = match typ with
      | Ident id -> (
          match Hashtbl.find typedefs id with
          | None -> tc_error (sprintf "The type %s is not defined" (Symbol.name id))
          | Some t -> t
        )
      | _ -> typ
    in
    tc_error_if_declared name fn_decls;
    match Hashtbl.add typedefs ~key:name ~data:typ with
    | `Duplicate -> tc_error (sprintf "Type %s defined more than once." (Symbol.name name))
    | `Ok -> ()

let typecheck ~header ast =
  let global_env = init_global_env () in
  (* Typecheck header *)
  (match header with
  | Some header_ast ->
    List.fold header_ast ~init:() ~f:(fun () gdecl ->
      tc_gdecl ~is_header:true gdecl global_env
    )
  | None -> ());
  (* Make sure main is included as expected *)
  Hashtbl.update global_env.fn_decls (Symbol.symbol "main")
    ~f:(fun opt -> match opt with
      | None -> (Used, (A.Int, []))
      | Some (_) -> tc_error "Function main was declared in header"
    );
  (* Typecheck source *)
  List.fold ast ~init:()
    ~f:(fun () gdecl -> tc_gdecl gdecl global_env);
  (* Confirm that all functions that were used were defined. *)
  Hashtbl.iteri global_env.fn_decls
    ~f:(fun ~key:name ~data:(ds, _) -> match ds with
      | Used -> tc_error (sprintf "Function %s is used but not defined" (Symbol.name name))
      | _ -> ()
    )
;;
