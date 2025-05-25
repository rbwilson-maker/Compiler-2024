(* Rachompicole L3 Compiler
 * Abstract Assembly -> x86-64
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * Uses a reversed accumulator to generate assembly
 * instructions. Assembly instructions take a size parameter
 * which determines which registers are used. 
 * Jumps are also determined by the comparison operator on a branch.
 *)

open Core
open Reg

module AS = Assem
module X = X86

(* This type is passed through all the functions and includes information
 * relevant to converting into x86-64 code.
 *  *)
type conversion_info = {
  ops_r  : X.line list; (* Operations, reversed. *)
  table : (Temp.t, X.addr) Hashtbl.t; (* Mutable *)
  num_spills : int;
}

let get_temp_addr info (t : Temp.t) =
  let stack_size = Hashtbl.length info.table in
  let make_new_addr () : X.addr = (
    Some (Option.value_exn (Int32.of_int ((stack_size - info.num_spills + 1) * -4))),
    RBP,
    None,
    None
  ) in
  let addr = Hashtbl.find_or_add info.table t ~default:make_new_addr in
  (* Return the (unchanged) info and X.Mem operand *)
  (info, X.Mem addr)
;;

let conv_operand info = function
  | AS.Imm n -> (info, X.Imm n)
  | AS.Reg r -> (info, X.Reg r)
  | AS.Temp t -> get_temp_addr info t

let conv_mov info (dest, src) =
  let (info, dest) = conv_operand info dest in
  let (info, src) = conv_operand info src in
  { info with 
    ops_r = (
      match (src, dest) with
      (* Cannot move directly from memory to memory *)
      | (X.Mem _, X.Mem _) -> 
        (X.Mov (Long, X.Reg R11, dest)) ::
        (X.Mov (Long, src, X.Reg R11)) ::
        info.ops_r
      | _ -> 
        (X.Mov (Long, src, dest)) :: info.ops_r
    );
  }
;;

(* Convert add and subtract to x86 *)
let conv_arithmetic info dest (rhs, lhs) op =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (op (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}
;;

(* In AT&T syntax, this is how imul works:         *
 * imul   imm regmem reg:   reg <- regmem * imm    *
 * imul       regmem reg:   reg <- regmem * reg    *)
let conv_mul info dest =
  let imm_on_left = (function
    | (X.Imm n, rhs) ->
      (* IMUL r32, r/m32, imm32 is r32 := r/m32 * imm32 *)
      let ops_r = List.rev_append (
        (X.Imul (Long, Some n, rhs, R11)) ::
        (X.Mov (Long, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}
    | _ -> failwith "Unexpected input")
  in function
    | ((X.Imm _ as lhs), rhs) -> imm_on_left (lhs, rhs)
    | (lhs, (X.Imm _ as rhs)) -> imm_on_left (rhs, lhs)
    | (lhs, rhs) -> (* IMUL r64, r/m64 is r64 *= r/m64 *)
      let ops_r = List.rev_append (
        (X.Mov (Long, lhs, X.Reg R11)) ::
        (X.Imul (Long, None, rhs, R11)) ::
        (X.Mov (Long, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}
;;

(* Convert div and mod.
 * Input looks like:
 * `Div: rax <- rax / rhs (rdx may be overwritten)
 * `Mod: rdx <- rax % rhs (it's ok that both are overwritten)
 *)
let conv_divmod info _dest (_lhs, rhs) =
  let ops_r = List.rev_append (
    X.Mov (Long, rhs, X.Reg R11) ::
    X.Cltd ::
    X.Idiv (Long, X.Reg R11) :: []
  ) info.ops_r
  in {info with ops_r}
;;

(* Convert and, xor, or, lshift, rshift to x86 
 * for shifts, rhs = rcx guaranteed by elabassem 
 *)
let conv_bitwise info dest (lhs, rhs) op =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (op (Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}
;;

let conv_compop = function
| AS.Less -> X.Less
| Greater -> Greater
| Equal -> Equal
| Leq -> Leq
| Geq -> Geq
| Neq -> Neq
;;

let conv_comparison info dest (lhs, rhs) comp = 
  let ops_r = List.rev_append (
    (X.Mov (Long, lhs, X.Reg R11)) ::
    (X.Cmp (Long, rhs, X.Reg R11)) ::
    (X.Setcc (comp, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}
;;

let conv_binop info (op : AS.operation) dest_a (lhs, rhs) =
  let (info, lhs) = conv_operand info lhs in
  let (info, rhs) = conv_operand info rhs in
  let (info, dest) = conv_operand info dest_a in (
  match (lhs, rhs) with
    | (X.Imm _, X.Imm _) -> failwith "imm binop imm should be handled in an earlier phase, or never reached."
    | _ -> match op with
      (* Ops with special treatment *)
      | Mul -> conv_mul info dest (lhs, rhs)
      | Div -> conv_divmod info dest (lhs, rhs)
      | Mod -> conv_divmod info dest (lhs, rhs)
      | Comp c -> conv_comparison info dest (lhs, rhs) (conv_compop c)
      (* Ops with a constructor passed as a function *)
      | Add -> conv_arithmetic info dest (lhs, rhs) (fun (x, y, z) -> X.Add (x, y, z))
      | Sub -> conv_arithmetic info dest (lhs, rhs) (fun (x, y, z) -> X.Sub (x, y, z))
      | Bit_and -> conv_bitwise info dest (lhs, rhs) (fun (x, y, z) -> X.And (x, y, z))
      | Bit_or  -> conv_bitwise info dest (lhs, rhs) (fun (x, y, z) -> X.Or (x, y, z))
      | Bit_xor -> conv_bitwise info dest (lhs, rhs) (fun (x, y, z) -> X.Xor (x, y, z))
      (* Sal and sar need the shift value on the left *)
      | Lshift -> conv_bitwise info dest (rhs, lhs) (fun (x, y, z) -> X.Sal (x, y, z))
      | Rshift -> conv_bitwise info dest (rhs, lhs) (fun (x, y, z) -> X.Sar (x, y, z))  
  )
;;

let label_to_str = Symbol.name

let conv_branch info (_if_label, else_label, after_label, cond) =
  match cond with
  | `Single opand ->
    let (info, opand) = conv_operand info opand in
    let unmet_cond_label = match else_label with
      | None -> label_to_str after_label
      | Some else_label -> label_to_str else_label
    in
    let ops_r = List.rev_append (
      (X.Cmp (Long, X.Imm 0l, opand)) ::
      (X.Jcc (X.Equal, unmet_cond_label)) :: []
    ) info.ops_r
    in {info with ops_r}
  | `Comparison AS.{lhs; op; rhs} ->
    let (info, lhs) = conv_operand info lhs in
    let (info, rhs) = conv_operand info rhs in
    let ops_r = List.rev_append (
      (X.Mov (Long, lhs, X.Reg R11)) ::
      (X.Cmp (Long, rhs, X.Reg R11)) ::
      (
        let unmet_cond_label = match else_label with
          | None -> label_to_str after_label
          | Some else_label -> label_to_str else_label
        in
        match op with
          | Less -> X.Jcc (X.Geq, unmet_cond_label)
          | Greater -> X.Jcc (X.Leq, unmet_cond_label)
          | Equal -> X.Jcc (X.Neq, unmet_cond_label)
          | Leq -> X.Jcc (X.Greater, unmet_cond_label)
          | Geq -> X.Jcc (X.Less, unmet_cond_label)
          | Neq -> X.Jcc (X.Equal, unmet_cond_label)
      ) :: []
    ) info.ops_r
    in {info with ops_r}
;;

let conv_call info (_dest, fn, spills : AS.operand * Symbol.t * AS.operand list) = 
  (* This function saves spilled registers to the stack, makes a function call,
   * and adjusts the stack pointer. 
   * Dest is guaranteed to be RAX by the assembly elaboration phase.
   * 
   * rax <-- call f(t1, t2, t3, ...)    =>    push  t3
   *                                          push  t2
   *                                          push  t1
   *                                          callq f
   *                                          addq  rsp, spills*8
   *)
  match spills with 
  | [] -> {info with ops_r = X.Call (Quad, label_to_str fn) :: info.ops_r}
  | _::_ -> 
  let ops_r = 
    let num_spills = List.length spills * 8 in
    let spills_align_16, push_extra = 
      if Int.equal (num_spills % 16) 0
      then num_spills, false
      else num_spills + 8, true
    in
    let stack = if push_extra then (X.Push (X.Imm (Int32.zero))::info.ops_r) else info.ops_r in
    X.Add (Quad, X.Imm (Int32.of_int_exn spills_align_16), X.Reg RSP) ::
    X.Call (Quad, label_to_str fn) ::
    List.fold_right spills ~init:stack ~f:(fun t acc ->
      let _, t' = conv_operand info t in 
      X.Push t' ::  acc
    ) 
  in {info with ops_r}
;;

let conv_instr info = function
| AS.Binop {op; dest; lhs; rhs} -> conv_binop info op dest (lhs, rhs)
| AS.Mov {dest; src} -> conv_mov info (dest, src)
| AS.Branch {if_label; else_label; after_label; cond} ->
    conv_branch info (if_label, else_label, after_label, cond)
| AS.Jump label ->
  { info with ops_r = (X.Jmp (label_to_str label)) :: info.ops_r }
| AS.Label label ->
  { info with ops_r = (X.Label (label_to_str label)) :: info.ops_r }
| AS.Call {dest; fn; params} -> conv_call info (dest, fn, params)
| AS.Directive _ -> info
| AS.Comment _ -> info
| AS.Raise _ -> failwith "all raises should be elaborated away"
| AS.Return -> failwith "all returns should be elaborated out"
;;

let convert_function (f : AS.fun_instrs) =
  (* === Convert function body === *)
  let used_callee_saves = List.filter ~f:(Hash_set.mem callee_saves) f.regs_used in
  let callee_aligned = (List.length used_callee_saves) % 2 = 0 in
  let ops_rev = [] in
  let num_spills = List.length f.spills in
  let table = Hashtbl.create (module Temp) in
  List.iteri f.spills ~f:(fun i t -> 
    Hashtbl.add_exn table ~key:t 
    ~data:(Some (Option.value_exn (Int32.of_int (((i+1) * 8) + 8))), RBP, None, None)
  );
  List.fold f.body 
    ~init:{ops_r = ops_rev; table; num_spills} 
    ~f:conv_instr
  (* === Write function epilogue === *)
  |> fun info -> 
    (* pop callee saves; leave; ret *)
    let ops_r' = List.fold used_callee_saves ~init:info.ops_r 
      ~f:(fun acc r -> X.Pop r :: acc) in
    {info with ops_r = 
      X.Ret :: 
      X.Leave :: 
      ( if callee_aligned (* assumes prologue is aligned *)
        then ops_r'
        else (X.Add (Quad, X.Imm (Int32.of_int_exn 8), X.Reg RSP))::ops_r'
      )
    }
  |> fun info -> (info.table, List.rev info.ops_r)
  |> fun (table, instr_list) ->
    (* Ceil up to 8x *)
    let temp_bytes = Int32.of_int_exn (
      (((Hashtbl.length table - num_spills) * 4 + 4) / 8 * 8)
    ) in 
    let instr_list' = List.fold used_callee_saves ~init:instr_list 
      ~f:(fun acc r -> X.Push (X.Reg r) :: acc) in
    (* PROLOGUE make globl, fn header, push rbp, move rsp, push callee saves *)
    (X.Directive (sprintf "globl \"%s\"" (label_to_str f.name))) ::
    (X.Label (label_to_str f.name)) ::
    (X.Push (X.Reg RBP)) ::
    (X.Mov (Quad, X.Reg RSP, X.Reg RBP)) ::
    (X.Sub (Quad, X.Imm temp_bytes, X.Reg RSP)) :: 
    ( if Bool.equal callee_aligned Int32.(temp_bytes % 16l = 0l) 
      then instr_list'
      else X.Push (X.Imm (Int32.zero)) :: instr_list'
    )

let convert (p : AS.program) = 
  List.map p ~f:convert_function