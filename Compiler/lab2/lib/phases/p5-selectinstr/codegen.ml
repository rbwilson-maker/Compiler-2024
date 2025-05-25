(* L2 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Use a linear, not quadratic, algorithm.
 *
 * Implements a "convenient munch" algorithm
 *)

open Core
module T = Tree
module AS = Assem
module L = Symbol

let munch_op = function
  | T.Add -> AS.Add
  | T.Sub -> AS.Sub
  | T.Mul -> AS.Mul
  | T.Div -> AS.Div
  | T.Mod -> AS.Mod
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

(* munch_exp_acc dest exp rev_acc
  *
  * Suppose we have the statement:
  *   dest <-- exp
  *
  * If the codegened statements for this are:
  *   s1; s2; s3; s4;
  *
  * Then this function returns the result:
  *   s4 :: s3 :: s2 :: s1 :: rev_acc
  *
  * I.e., rev_acc is an accumulator argument where the codegen'ed
  * statements are built in reverse. This allows us to create the
  * statements in linear time rather than quadratic time (for highly
  * nested expressions).
  *)
let rec munch_exp_acc (dest : AS.operand) (exp : T.exp) (rev_acc : AS.instr list)
  : AS.instr list
  =
  match exp with
  | T.Const n -> AS.Mov { dest; src = AS.Imm n } :: rev_acc
  | T.Temp t -> AS.Mov { dest; src = AS.Temp t } :: rev_acc
  | T.Binop binop -> munch_binop_acc dest (binop.op, binop.lhs, binop.rhs) rev_acc
  | T.True -> AS.Mov { dest; src = AS.Imm Int32.one } :: rev_acc
  | T.False -> AS.Mov { dest; src = AS.Imm Int32.zero } :: rev_acc
  | T.Ternary t -> munch_ternary_acc dest (t.if_exp, t.then_exp, t.else_exp) rev_acc

(* munch_binop_acc dest (binop, e1, e2) rev_acc
  *
  * generates instructions to achieve dest <- e1 binop e2
  *
  * Much like munch_exp, this returns the result of appending the
  * instructions in reverse to the accumulator argument, rev_acc.
  *)
and munch_binop_acc
  (dest : AS.operand)
  ((binop, e1, e2) : T.binop * T.exp * T.exp)
  (rev_acc : AS.instr list)
  : AS.instr list
  =
  let op = munch_op binop in
  let t1 = AS.Temp (Temp.create ()) in
  let t2 = AS.Temp (Temp.create ()) in
  let rev_acc' = rev_acc |> munch_exp_acc t1 e1 |> munch_exp_acc t2 e2 in
  AS.Binop { op; dest; lhs = t1; rhs = t2 } :: rev_acc'

and munch_ternary_acc
  (dest: AS.operand)
  ((i, t, e) : T.exp * T.exp * T.exp)
  (rev_acc : AS.instr list)
  : AS.instr list
  = 
  let l1, l2, l3 = L.unique_symbol "if", L.unique_symbol "else", L.unique_symbol "after" in
  (* munch the conditional expression *)
  let rev_acc', cond = munch_comp_acc i rev_acc in
  (* compute the if body with the label and branch above*)
  let rev_acc'' = munch_exp_acc dest t 
    (AS.Label l1
    :: AS.Branch {
      if_label = l1
    ; else_label = Some l2
    ; after_label = l3
    ; cond = cond
    } :: rev_acc') in
  let rev_acc''' = munch_exp_acc dest e (AS.Label l2 :: AS.Jump l3 :: rev_acc'') in
    AS.Label l3 :: AS.Jump l3 :: rev_acc'''

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
  | T.Return e :: stms -> (AS.Return) :: (munch_exp_acc (AS.Reg RAX) e rev_acc) |> munch_stms_acc stms
  | T.Label l :: stms -> AS.Label l :: rev_acc |> munch_stms_acc stms
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
  | [] -> rev_acc

(* To codegen a series of statements, just concatenate the results of
 * codegen-ing each statement. *)
let codegen (t : Tree.stm list) = List.rev (munch_stms_acc t [])

