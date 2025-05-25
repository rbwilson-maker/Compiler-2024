(* L1 Compiler
 * TypeChecker
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 *
 * Simple typechecker that checks two properties:
 *  (1) If a variable is initialized, it has previously been declared.
 *  (2) If a variable is used, it has previously been initialized.
 * This is sufficient for now, since only int types are available in L1.
 *
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
 * Now distinguishes between declarations and initialization
 * Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
 * Should be more up-to-date with modern spec.
 * Modified: Matt Bryant <mbryant@andrew.cmu.edu> Fall 2015
 * Handles undefined variables in unreachable code, significant simplifications
 * Modified: Alice Rao <alrao@andrew.cmu.edu> Fall 2017
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu> Fall 2018
 *   Use records, redo marks.
 *)

open Core
module A = Ast
module S = Symbol.Map

type init_status =
  | Decl of A.typ
  | Init of A.typ

type env =
  { vars : init_status S.t
  ; returns : bool
  }

let tc_errors : Error_msg.t = Error_msg.create ()

let rec tc_exp (ast : A.mexp) (vars : init_status S.t) : A.typ =
  let error ~msg =
    Error_msg.error tc_errors (Mark.src_span ast) ~msg;
    raise Error_msg.Error
  in
  match Mark.data ast with
  | A.True -> A.Bool
  | A.False -> A.Bool
  | A.Const _ -> A.Int
  | A.Binop { op; lhs; rhs } ->
    let expect t1 t2 t3 =
      check_type lhs t1 vars;
      check_type rhs t2 vars;
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
       check_type lhs (tc_exp rhs vars) vars;
       A.Bool
     | Neq ->
       check_type lhs (tc_exp rhs vars) vars;
       A.Bool
     (* bool * bool -> bool *)
     | And -> expect A.Bool A.Bool A.Bool
     | Or -> expect A.Bool A.Bool A.Bool)
  | A.Unop { op; operand } ->
    (match op with
     (* bool -> bool *)
     | Not ->
       check_type operand A.Bool vars;
       A.Bool
     (* int -> int *)
     | Negative ->
       check_type operand A.Int vars;
       A.Int
     | Bit_not ->
       check_type operand A.Int vars;
       A.Int)
  | A.Ternary { if_exp; then_exp; else_exp } ->
    check_type if_exp A.Bool vars;
    let branch_type = tc_exp then_exp vars in
    check_type else_exp branch_type vars;
    branch_type
  | A.Var id ->
    (match S.find vars id with
     | None -> error ~msg:(sprintf "Not declared before use: `%s`" (Symbol.name id))
     | Some (Decl _) ->
       error ~msg:(sprintf "Not initialized before use: `%s`" (Symbol.name id))
     | Some (Init t) -> t)

and check_type exp typ vars =
  let error ~msg =
    Error_msg.error tc_errors (Mark.src_span exp) ~msg;
    raise Error_msg.Error
  in
  let typ' = tc_exp exp vars in
  if A.equal_typ typ typ'
  then ()
  else
    error
      ~msg:
        (sprintf
           "Expected expression to be type %s but got type %s"
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
let rec tc_stms (ast : Ast.mstm list) (env : env) : env =
  match ast with
  | [] -> env
  | stm :: stms ->
    let error ~msg =
      Error_msg.error tc_errors (Mark.src_span stm) ~msg;
      raise Error_msg.Error
    in
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
       let new_env = { returns = env'.returns; vars = new_vars } in
       tc_stms stms new_env
     | A.Simple s ->
       (match s with
        | A.Declare (A.New_var (typ, id)) ->
          (match S.find env.vars id with
           | Some _ -> error ~msg:(sprintf "Already declared: `%s`" (Symbol.name id))
           | None ->
             tc_stms stms { env with vars = S.set env.vars ~key:id ~data:(Decl typ) })
        | A.Declare (A.Init (typ, id, e)) ->
          (* The following translation is okay (until we add function calls):
           * int x = expr;  ===>   int x;
           *                       x = expr;
           *)
          let stms =
            remark (A.Simple (A.Declare (A.New_var (typ, id))))
            :: remark (A.Simple (A.Assign (id, e)))
            :: stms
          in
          tc_stms stms env
        | A.Assign (id, e) ->
          (match S.find env.vars id with
           | None ->
             error
               ~msg:(sprintf "Not declared before initialization: `%s`" (Symbol.name id))
           | Some (Decl t) ->
             check_type e t env.vars;
             tc_stms stms { env with vars = S.set env.vars ~key:id ~data:(Init t) }
           | Some (Init t) ->
             check_type e t env.vars;
             tc_stms stms env)
        | A.Do mexp ->
          let _ : A.typ = tc_exp mexp env.vars in
          tc_stms stms env)
     | Control c ->
       (match c with
        | If (cond, if_stm, None) ->
          check_type cond A.Bool env.vars;
          let _ : env = tc_stms [ if_stm ] env in
          tc_stms stms env
        | If (cond, if_stm, Some else_stm) ->
          check_type cond A.Bool env.vars;
          let if_env = tc_stms [ if_stm ] env in
          let else_env = tc_stms [ else_stm ] env in
          let f ~key:_ = function
            | `Both (Init t, Init _) -> Some (Init t)
            | _ -> None
          in
          let intersect_vars = S.merge ~f if_env.vars else_env.vars in
          let new_env =
            { returns = if_env.returns && else_env.returns
            ; vars = merge_ctx ~outer:env.vars ~inner:intersect_vars
            }
          in
          tc_stms stms new_env
        | While (cond, body_stm) ->
          check_type cond A.Bool env.vars;
          let (_ : env) = tc_stms [ body_stm ] env in
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
          check_type cond A.Bool loop_env.vars;
          (* Put the iter_stm and body_stm into a list together *)
          let body_stms =
            match iter_stm with
            | None -> [body_stm]
            | Some (Declare _) ->
              error ~msg:"The third for-loop argument can't be a declaration."
            | Some simple ->  [body_stm; (remark (A.Simple simple))]
          in
          (* Check that they are okay, but discard changes to env *)
          let (_ : env) = tc_stms body_stms loop_env in
          (* Continue typechecking *)
          tc_stms stms { env with vars = merge_ctx ~outer:env.vars ~inner:loop_env.vars }
        | Return mexp ->
          check_type mexp A.Int env.vars;
          tc_stms
            stms
            { vars =
                S.map env.vars ~f:(function
                  | Decl t -> Init t
                  | x -> x)
            ; returns = true
            }))
;;

let typecheck (fn, stms) =
  if not Symbol.(fn = symbol "main")
  then (
    Error_msg.error tc_errors None ~msg:"missing main function";
    raise Error_msg.Error)
  else (
    let env = tc_stms stms { vars = S.empty; returns = false } in
    if not env.returns
    then (
      Error_msg.error tc_errors None ~msg:"main does not return";
      raise Error_msg.Error))
;;
