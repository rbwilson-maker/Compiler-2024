(* Rachompicole L5 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * 
 * tree -> tree
 * This file performs coalesces expressions with only constants into constants.
 *)
open Core
let debug = ref false;;

let tree_bool b : Tree.exp = match b with true -> True | false -> False
let rec process_exp (exp : Tree.exp) : Tree.exp = match exp with
  | Binop {lhs; op; rhs} -> (
    match (process_exp lhs, process_exp rhs) with
    | (Const n, Const m) -> Int32.(
      match op with
      | Add -> Const (n + m)
      | Sub -> Const (n - m)
      | Mul -> Const (n * m)
      | Less -> tree_bool (n < m)
      | Greater -> tree_bool (n > m)
      | Leq -> tree_bool (n <= m)
      | Geq -> tree_bool (n >= m)
      | Bit_and -> Const (n land m)
      | Bit_or -> Const (n lor m)
      | Bit_xor -> Const (n lxor m)
      | Lshift -> Const (n lsl (Int.of_int32_exn m))
      | Rshift -> Const (n asr (Int.of_int32_exn m))
    )
    | (lhs, rhs) -> Binop {lhs; op; rhs}
  )
  | Equality {lhs; rhs; op; size} -> (
    let lhs = process_exp lhs in
    let rhs = process_exp rhs in
    let b = 
      match (lhs, rhs) with
        | (True, True) | (False, False) -> Some true
        | (Const n, Const m) when Int32.(n = m) -> Some true
        | (True, False) | (False, True) | (Const _, Const _) -> Some false
        | _ -> None
    in
    match (b, op) with
      | (None, _) -> Equality {lhs; rhs; op; size}
      | (Some b, Equal) -> tree_bool b
      | (Some b, Neq) -> tree_bool (not b)
  )
  | e -> e
;;

let process_stm (stm : Tree.stm) : Tree.stm =
  let exp = process_exp in
  match stm with
  | Move {dest; src} -> 
    Move {dest; src = 
    (match src with
    | Pure e -> Pure (exp e)
    | Effect_binop {lhs; op; rhs} -> Effect_binop { lhs = exp lhs; op; rhs = exp rhs}
    | Fn f -> Fn {f with args = List.map f.args ~f:(fun (e, s) -> (exp e, s))}
    | Array_addr a -> Array_addr {a with array = exp a.array; index = exp a.index}
    | Alloc a -> Alloc {a with array_len=Option.map a.array_len ~f:exp}
    | s -> s
    )}
  | Write_mem {dest; size; src=e} -> Write_mem {dest; size; src=exp e}
  | Check_null e -> Check_null (exp e)
  | Branch {condition=e1; if_label; else_label; after_label} -> (
    match exp e1 with
    | True -> Goto if_label
    | False -> (
      match else_label with
      | Some label -> Goto label
      | None -> Goto after_label
    )
    | condition -> Branch {condition; if_label; else_label; after_label}
  )
  | Return opt -> 
    Return (Option.map opt ~f:(fun (imp, s) -> 
      match imp with
      | Pure e -> (Tree.Pure (exp e), s)
      | imp -> (imp, s)
      )
    )
  | s -> s
;;

let coalesce_constants (d : bool) (program : Tree.program) : Tree.program =
  debug := d;
  List.map program ~f:(fun (label, params, stms) -> 
    (label, params, List.map stms ~f:process_stm)
  )
;;