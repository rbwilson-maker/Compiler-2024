(* Rachompicole L3 Compiler
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 *
 * Implements a "convenient munch" algorithm.
 * It munches ternary expressions to keep around
 * comparison operators. The whole thing is
 * implemented with a reversed accumulator.
 *)

open Core
module T = Tree
module AS = Assem
module L = Symbol

let munch_op = function
  | T.Add -> AS.Add
  | T.Sub -> AS.Sub
  | T.Mul -> AS.Mul
  | T.Bit_and -> AS.Bit_and
  | T.Bit_or -> AS.Bit_or
  | T.Bit_xor -> AS.Bit_xor
  | T.Lshift -> AS.Lshift
  | T.Rshift -> AS.Rshift
  | T.Less -> AS.Comp Less
  | T.Greater -> AS.Comp Greater
  | T.Equal -> AS.Comp Equal
  | T.Leq -> AS.Comp Leq
  | T.Geq -> AS.Comp Geq
  | T.Neq -> AS.Comp Neq
;;

let munch_effectop = function
| T.Div -> AS.Div
| T.Mod -> AS.Mod

let munch_sig = function
| T.Sigfpe -> AS.Sigfpe
| T.Sigabrt -> AS.Sigabrt

(* generates instructions to achieve dest <- e *)
let rec munch_exp_acc (dest : AS.operand) (exp : T.exp) (rev_acc : AS.instr list)
  : AS.instr list
  =
  match exp with
  | T.Const n -> AS.Mov { dest; src = AS.Imm n } :: rev_acc
  | T.Temp t -> AS.Mov { dest; src = AS.Temp t } :: rev_acc
  | T.Binop binop -> munch_binop_acc dest (binop.op, binop.lhs, binop.rhs) rev_acc
  | T.True -> AS.Mov { dest; src = AS.Imm Int32.one } :: rev_acc
  | T.False -> AS.Mov { dest; src = AS.Imm Int32.zero } :: rev_acc

(* generates instructions to achieve dest <- e1 binop e2 *)
and munch_binop_acc
  (dest : AS.operand)
  ((binop, e1, e2) : T.binop * T.exp * T.exp)
  (rev_acc : AS.instr list)
  : AS.instr list
  =
  let t1 = AS.Temp (Temp.create ()) in
  let t2 = AS.Temp (Temp.create ()) in
  let rev_acc' = rev_acc |> munch_exp_acc t1 e1 |> munch_exp_acc t2 e2 in
  AS.Binop { op = munch_op binop; dest; lhs = t1; rhs = t2 } :: rev_acc'

(* For branches, generations instructions for computing a comparison expression.
   If the expression is a bool or a temp, it becomes a `Single, and if it uses
   a comparison operator like <, it's a `Comparison, which is needed to generate
   assembly instruction
*)
and munch_comp_acc
  (e : T.exp) 
  (rev_acc : AS.instr list) 
  : AS.instr list * [`Single of AS.operand | `Comparison of AS.comparison]
= match e with
| T.Binop b -> (match munch_op b.op with
  | AS.Comp c -> 
    let t1, t2 = AS.Temp (Temp.create ()), AS.Temp (Temp.create ()) in
    (rev_acc |> (munch_exp_acc t1 b.lhs) |> (munch_exp_acc t2 b.rhs), `Comparison {lhs=t1; rhs=t2; op=c})
  | _ -> let t = AS.Temp (Temp.create ()) in (munch_exp_acc t e rev_acc, `Single t)
)
| e -> let t = AS.Temp (Temp.create ()) in (munch_exp_acc t e rev_acc, `Single t)
;;

(* munch_stm : T.stm -> AS.instr list *)
(* munch_stm stm generates code to execute stm *)
let rec munch_stms_acc (stms : T.stm list) (rev_acc : AS.instr list) : AS.instr list =
  match stms with 
  | T.Move mv :: stms -> munch_exp_acc (AS.Temp mv.dest) mv.src rev_acc |> munch_stms_acc stms
  | T.Return e :: stms -> (match e with
    | None -> AS.Return :: rev_acc |> munch_stms_acc stms
    | Some e -> 
      (AS.Return) :: (munch_exp_acc (AS.Reg RAX) e rev_acc) 
      |> munch_stms_acc stms
    )
  | T.Label l :: stms -> AS.Label l :: rev_acc |> munch_stms_acc stms
  | T.Effect_move mv :: stms ->
    let t1, t2 = AS.Temp (Temp.create ()), AS.Temp (Temp.create ()) in
    let rev_acc' = munch_exp_acc t1 (mv.lhs) rev_acc |> munch_exp_acc t2 (mv.rhs) in
    AS.Binop {dest = AS.Temp mv.dest; op = munch_effectop mv.op; lhs = t1; rhs = t2}
    :: rev_acc'
    |> munch_stms_acc stms
  | T.Fn_move mv :: stms -> 
    let (args, rev_acc') = List.fold mv.args ~init:([], rev_acc) 
      ~f:(fun (args, acc) e ->
        let t = AS.Temp (Temp.create ()) in 
        let stms = munch_exp_acc t e acc in
        (t::args, stms)
      )
    in AS.Call {dest = AS.Temp mv.dest; fn = mv.fn; params = List.rev args} :: rev_acc'
    |> munch_stms_acc stms
  | T.Do_fn f :: stms ->
    let (args, rev_acc') = List.fold f.args ~init:([], rev_acc) 
      ~f:(fun (args, acc) e ->
        let t = AS.Temp (Temp.create ()) in 
        let stms = munch_exp_acc t e acc in
        (t::args, stms)
      )
    (* we throw away the result *)
    in AS.Call {dest = AS.Reg RAX; fn = f.fn; params = List.rev args} :: rev_acc'
    |> munch_stms_acc stms
  | T.Branch b :: stms -> 
    let rev_acc', cond = munch_comp_acc b.condition rev_acc in
      AS.Branch
       { if_label = b.if_label
       ; else_label = b.else_label
       ; after_label = b.after_label
       ; cond
       } :: rev_acc'
       |> munch_stms_acc stms  
  | T.Goto l :: stms -> AS.Jump l ::rev_acc |> munch_stms_acc stms
  | T.Raise s :: stms -> AS.Raise (munch_sig s) :: rev_acc |> munch_stms_acc stms
  | [] -> rev_acc

(* To codegen a series of statements, just concatenate the results of
 * codegen-ing each statement, and reverse it. *)
let codegen (t : Tree.program) : AS.program = 
  (* [f_stms, g_stms, h_stms] -> [rev_hstms, rev_gstms, rev_fstms] *)
  List.fold_right t ~init:[] ~f:(fun (fn, args, fn_body) funs ->
    let fun_stms = munch_stms_acc fn_body [] |> List.rev in
    let f : AS.fun_instrs =
      { name = fn
      ; body = fun_stms
      ; spills = args
      ; regs_used = []
      } 
    in f :: funs
  )
