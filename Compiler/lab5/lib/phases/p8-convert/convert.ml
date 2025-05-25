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
  ops_r  : X.line list; (* Working list of assembly operations, reversed. *)
  table : (Temp.t, X.addr) Hashtbl.t; (* Mutable *)
  num_spills : int;
  spilled_temp_bytes : int ref;
  exit_label : Symbol.t
}
(* Labels for creating global jumps for more readable (and shorter) output code *)
let sigusr_label = Symbol.unique_symbol "memerr";;
let sigfpe_label = Symbol.unique_symbol "sigfpe";;
let abort_sym, raise_sym, sigusr2 = (
  match Sys.getenv "UNAME" with
  | Some "Darwin" -> "_abort", "_raise", 31l
  | Some "Linux" -> "abort", "raise", 12l
  | _ -> "abort", "raise", 31l
)
;;

let get_temp_addr info ((t, s) : Temp.t * Size.t) =
  let make_new_addr () : X.addr = 
  info.spilled_temp_bytes := !(info.spilled_temp_bytes) + Size.to_int s;
  (Some (Int32.of_int_exn (Int.neg !(info.spilled_temp_bytes))),
    RBP,
    None,
    None
  ) in
  let addr = Hashtbl.find_or_add info.table t ~default:make_new_addr in
  (info, X.Mem addr)
;;

let addr_uses_reg (addr : AS.addr) reg : bool =
  match addr.base, addr.index with
  | AS.Reg r1, Some AS.Reg r2 -> Reg.equal reg r1 || Reg.equal reg r2
  | AS.Reg r, _ -> Reg.equal reg r
  | _, Some AS.Reg r -> Reg.equal reg r
  | _ -> false
  
;;

(* dest = `R11 Reg means R11 is the dest and Reg is some auxiliary register 
   dest = `Reg R means R is the dest and R11 is the auxiliary register
*)
let conv_operand ?mov_data info = function
  | AS.Imm n -> (info, X.Imm n)
  | AS.Reg r -> (info, X.Reg r)
  | AS.Temp t -> get_temp_addr info t
  | AS.Addr m -> 
    let unwrap_mov_data () = 
      (match mov_data with
      | Some (R11, helper_reg, None) -> 
        R11, helper_reg, None
      | Some (dest_reg, helper_reg, Some dest_needs_popping) ->
        let dest_reg = if addr_uses_reg m dest_reg then helper_reg else dest_reg in
        dest_needs_popping := Some dest_reg;
        dest_reg, helper_reg, !dest_needs_popping
      | _ -> 
        failwith "Mov_data should only be needed in mov and lea. 
        Some invariants about where temps are used/generated in instructions is probably broken or changed.
        Addressing modes should rarely appear at all, let alone ones that use stack locations." 
      ) in
    let conv_one_temp ((t, s), base, index, dest_reg, push_dest) = 
      let dest_reg = match push_dest with Some dest_reg -> dest_reg | None -> dest_reg in
      let (info, t1) = get_temp_addr info (t, s) in
      {info with ops_r =
      X.Lea 
        (Quad
        , X.Mem
          ( m.disp
          , base
          , index
          , Option.map ~f:Size.to_int (m.scale)
          )
        , Reg dest_reg)
      :: X.Mov (s, t1, Reg dest_reg)
      :: (
        match push_dest with
        | Some dest_reg -> X.Push (Reg dest_reg) :: info.ops_r
        | None -> info.ops_r
      )
      }, X.Mem (None, dest_reg, None, None)
    in
    (match m.base, m.index with
    (* x(%t1, %t2, s)  ==> 
       dest <-- t1
       helper <--t2
       lea x(dest, helper, s), dest
     *)
    | AS.Temp (t1, s1), Some (AS.Temp (t2, s2)) -> 
      let (dest_reg, helper_reg, dest_needs_pushing) = unwrap_mov_data () in
      let (info, t1) = get_temp_addr info (t1, s1) in
      let (info, t2) = get_temp_addr info (t2, s2) in
      {info with ops_r =
      X.Pop helper_reg
      :: X.Lea
        (Quad
        , X.Mem
          ( m.disp
          , dest_reg
          , Some helper_reg
          , Option.map ~f:Size.to_int (m.scale)
          )
        , Reg dest_reg)
      :: X.Mov (s2, t2, Reg helper_reg)
      :: X.Mov (s1, t1, Reg dest_reg)
      :: X.Push (Reg helper_reg)
      :: (
        match dest_needs_pushing with
        | Some dest_reg -> X.Push (Reg dest_reg) :: info.ops_r
        | None -> info.ops_r
      )
      }, X.Mem (None, dest_reg, None, None)
    (* x(%t1, %rx, s) ==> 
       dest <-- t1
       lea x(dest, %rx, s), dest
     *)
    | AS.Temp t1, r ->
      let (dest_reg, _, dest_needs_pushing) = unwrap_mov_data () in
      conv_one_temp 
      ( t1, dest_reg
      , Option.map r ~f:(function (AS.Reg r2) -> r2 | _ -> failwith "Not possible")
      , dest_reg, dest_needs_pushing
      )
    (* x(%rx, %t1, s) ==> 
       dest <-- t1
       lea x(%rx, dest, %s), dest
     *)
    | AS.Reg r, Some (AS.Temp t1) ->
      let (dest_reg, _, dest_needs_pushing) = unwrap_mov_data () in
      conv_one_temp (t1, r, Some dest_reg, dest_reg, dest_needs_pushing)

    | AS.Reg r1, r2 ->
      info, 
      X.Mem 
        ( m.disp
        , r1
        , Option.map r2 ~f:(function (AS.Reg r2) -> r2 | _ -> failwith "Not possible")
        , Option.map ~f:Size.to_int (m.scale)
        )
    | b, i -> failwith (sprintf "mem has base:%s index:%s, who generated this??"
      (AS.Print.pp_operand b) (Print.pp_opt ~default:"None" AS.Print.pp_operand i)
    ))
;;

let conv_mov info (dest, src, size) =
  (* Pick registers to overwrite if memory addr needs to be converted *)
  let (dest_reg, src_reg) : (Reg.t option) * (Reg.t option) =
    match (dest, src) with
    | (AS.Addr a, AS.Addr _) -> 
      if (addr_uses_reg a R15)
      then (Some R14, Some R11)
      else (Some R15, Some R11)
    | (AS.Reg R11, AS.Addr a) -> 
      if (addr_uses_reg a R15)
      then (None, Some R14)
      else (None, Some R15)
    | (AS.Addr a, AS.Reg R11) -> 
      if (addr_uses_reg a R15) 
      then (Some R14, None) 
      else (Some R15, None)
    | (AS.Addr _, AS.Reg R15) -> (Some R11, None)
    | (_, AS.Addr _) -> (None, Some R11)
    | (AS.Addr a, _) ->
      if (addr_uses_reg a R15) 
      then (Some R14, None) 
      else (Some R15, None)
    | _ -> (None, None)
  in
  (* If we need to translate memory addr, need to save the register we overwrite *)
  let conv_mov_opand info op should_pop_ref may_overwrite_reg = 
    match may_overwrite_reg with
    | None -> conv_operand info op
    | Some R11 -> 
      conv_operand ~mov_data:(R11, R15, None) info op
    | Some r ->
      conv_operand ~mov_data:(r, R11, Some should_pop_ref) info op
  in
  let dest_needs_popping, src_needs_popping = ref None, ref None in
  let (info, dest) = conv_mov_opand info dest dest_needs_popping dest_reg in
  let (info, src) = conv_mov_opand info src src_needs_popping src_reg in
  { info with 
    ops_r = (
      (match dest_reg with
      | None | Some R11 -> []
      | _ -> match !dest_needs_popping with | Some r -> [X.Pop r] | None -> [])
      @
      (match src_reg with
      | None | Some R11 -> []
      | _ -> match !dest_needs_popping with | Some r -> [X.Pop r] | None -> [])
      @
      (match (src, dest) with
      (* cannot move mem to mem *)
      | (Mem _, Mem _) | (Imm (`Quad _), Mem _)->
        [ X.Mov (size, Reg R11, dest)
        ; X.Mov (size, src, Reg R11)
        ]
      | _ -> [X.Mov (size, src, dest)]
      )
      @
      info.ops_r
    )
  }
;;

(* lea is only addr into reg, so we use R11 *)
let conv_lea info (dest, src) = 
  (* convert source which must be an address *)
  let (info, src) = (match src with
  | AS.Addr a ->
    if addr_uses_reg a R15 
    then conv_operand ~mov_data:(R11, R14, None) info src
    else conv_operand ~mov_data:(R11, R15, None) info src
  | _ -> failwith "LEA must be used on addresses only"
  ) in
  let (info, dest) = conv_operand info dest in
  { info with
    ops_r = ( 
      X.Mov (Quad, Reg R11, dest)
      :: X.Lea (Quad, src, Reg R11)
      :: info.ops_r
    )
  }
  

(* Convert add and subtract to x86 *)
let conv_arithmetic info dest (rhs, lhs) op =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (op (Size.Long, lhs, X.Reg R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}
;;

(* In AT&T syntax, this is how imul works:         *
 * imul   imm regmem reg:   reg <- regmem * imm    *
 * imul       regmem reg:   reg <- regmem * reg    *)
let conv_mul info dest =
  let mov_left (lhs, rhs) s =
    (* IMUL r32, r/m32, imm32 is r32 := r/m32 * imm32 *)
    let ops_r = List.rev_append (
      (X.Mov (s, lhs, X.Reg R11)) ::
      (X.Imul (s, None, rhs, R11)) ::
      (X.Mov (s, X.Reg R11, dest)) :: []
    ) info.ops_r
    in {info with ops_r}
  in function
    | ((X.Imm (`Long n)), (X.Imm _ as rhs)) -> 
      let ops_r = List.rev_append (
        (X.Mov (Long, rhs, X.Reg R11)) ::
        (X.Imul (Long, Some n, X.Reg R11, R11)) ::
        (X.Mov (Long, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}
    | ((X.Imm (`Quad _) as lhs), (X.Imm (`Long n))) ->
      let ops_r = List.rev_append (
        (X.Mov (Quad, lhs, X.Reg R11)) ::
        (X.Imul (Quad, Some n, X.Reg R11, R11)) ::
        (X.Mov (Quad, X.Reg R11, dest)) :: []
      ) info.ops_r
      in {info with ops_r}
    | ((X.Imm (`Quad _)), (X.Imm (`Quad _))) -> 
      failwith "Should never multiply two quads in C0"
    | ((X.Imm (`Quad _) as lhs), rhs) -> mov_left (lhs, rhs) Quad
    | (lhs, (X.Imm _ as rhs)) -> mov_left (rhs, lhs) Long
    | (lhs, rhs) -> mov_left (lhs, rhs) Long
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
 * for shifts, lhs = rcx guaranteed by elabassem 
 *)
let conv_bitwise info dest (lhs, rhs) op =
  let ops_r = List.rev_append (
    (X.Mov (Long, rhs, X.Reg R11)) ::
    (op (Size.Long, lhs, X.Reg R11)) ::
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

let conv_comparison ?size info dest (lhs, rhs) comp = 
  let size = match size with 
  | None -> Size.Long
  | Some s -> s 
  in
  let ops_r = List.rev_append (
    (X.Mov (size, lhs, X.Reg R11)) ::
    (X.Cmp (size, rhs, X.Reg R11)) ::
    (X.Setcc (comp, X.Reg R11)) ::
    (X.Movzx (Byte, X.Reg R11, Long, R11)) ::
    (X.Mov (Long, X.Reg R11, dest)) :: []
  ) info.ops_r
  in {info with ops_r}
;;

let conv_binop info (op : AS.operation) dest_a (lhs, rhs) =
  (* All binops move their result into R11, so we will never see mem <-- mem *)
  (* all address calculations are separated out in translation *)
  let (info, lhs) = conv_operand info lhs in
  let (info, rhs) = conv_operand info rhs in

  (* dest might be an address calculation -- put dest in R12 *)
  let should_pop_dest = ref None in
  let (info, dest) = conv_operand ~mov_data:(R12, R13, Some should_pop_dest) info dest_a in 
  let info = (match op with
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
  ) in
  match !should_pop_dest with
  | Some r -> {info with ops_r = (X.Pop r) :: info.ops_r}
  | None -> info
;;

let label_to_str = Symbol.name

let conv_branch info (_if_label, else_label, after_label, cond) =
  match cond with
  | `Single (opand, s) ->
    let (info, opand) = conv_operand info opand in
    let unmet_cond_label = match else_label with
      | None -> after_label
      | Some else_label -> else_label
    in
    let ops_r = List.rev_append (
      match opand with 
      | X.Imm _ -> 
        (X.Mov (s, opand, X.Reg R11)) ::
        (X.Cmp (s, X.Imm (`Long 0l), X.Reg R11)) ::
        (X.Jcc (X.Equal, unmet_cond_label)) :: []
      | _ -> 
        (X.Cmp (s, X.Imm (`Long 0l), opand)) ::
        (X.Jcc (X.Equal, unmet_cond_label)) :: []
    ) info.ops_r
    in {info with ops_r}
  | `Comparison AS.{lhs; op; rhs; size} ->
    let (info, lhs) = conv_operand info lhs in
    let (info, rhs) = conv_operand info rhs in
    let ops_r = List.rev_append (
      (X.Mov (size, lhs, X.Reg R11)) ::
      (X.Cmp (size, rhs, X.Reg R11)) ::
      (
        let unmet_cond_label = match else_label with
          | None -> after_label
          | Some else_label -> else_label
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

let conv_call info (_dest, fn, spills : AS.operand * Symbol.t * (AS.operand * X.size) list) = 
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
      if (num_spills % 16) = 0
      then num_spills, false
      else num_spills + 8, true
    in
    let stack = if push_extra then (X.Push (X.Imm (`Long Int32.zero))::info.ops_r) else info.ops_r in
    X.Add (Quad, X.Imm (`Long (Int32.of_int_exn spills_align_16)), X.Reg RSP) ::
    X.Call (Quad, label_to_str fn) ::
    List.fold_right spills ~init:stack ~f:(fun (t, _) acc ->
      let _, t' = conv_operand info t in 
      X.Push t' ::  acc
    ) 
  in {info with ops_r}
;;

let conv_instr info = function
| AS.Binop {op; dest; lhs; rhs} -> conv_binop info op dest (lhs, rhs)
| AS.Equal {dest; lhs; rhs; size; op} -> 
  let (info, lhs) = conv_operand info lhs in
  let (info, rhs) = conv_operand info rhs in

  let should_pop_dest = ref None in
  let (info, dest) = 
  conv_operand ~mov_data:(R12, R13, Some should_pop_dest) info dest in 
  let info = conv_comparison ~size info dest (lhs, rhs) (match op with `Eq -> Equal | `Neq -> Neq) in
  (match !should_pop_dest with
  | Some r -> {info with ops_r = (X.Pop r) :: info.ops_r}
  | None -> info
  )
| AS.Mov {dest; src; size} -> conv_mov info (dest, src, size)
| AS.Mov_addr {dest; src}-> conv_lea info (dest, AS.Addr src)
| AS.Branch {if_label; else_label; after_label; cond} ->
    conv_branch info (if_label, else_label, after_label, cond)
| AS.Jump label ->
  { info with ops_r = (X.Jmp label) :: info.ops_r }
| AS.Label label ->
  { info with ops_r = (X.Label label) :: info.ops_r }
| AS.Call {dest; fn; params; _} -> conv_call info (dest, fn, params)
| AS.Return -> {info with ops_r = X.Jmp (info.exit_label)::info.ops_r}
| AS.Directive _ -> info
| AS.Comment _ -> info
| AS.Raise Sigabrt -> {info with ops_r =  X.Call (Quad, abort_sym) :: info.ops_r}
| AS.Raise Sigusr -> {info with ops_r = X.Jmp (sigusr_label)::info.ops_r}
| AS.Raise Sigfpe -> {info with ops_r = X.Jmp (sigfpe_label)::info.ops_r}
;;

let convert_function (f : AS.fun_instrs) =
  (* === Convert function body === *)
  let used_callee_saves = List.filter ~f:(Hash_set.mem callee_saves) f.regs_used in
  let callee_aligned = (List.length used_callee_saves) % 2 = 0 in
  let ops_rev = [] in
  let num_spills = List.length f.spills in
  let table = Hashtbl.create (module Temp) in
  let exit_label = Symbol.unique_symbol "exit" in
  List.iteri f.spills ~f:(fun i (t, _) -> 
    Hashtbl.add_exn table ~key:t 
    ~data:(Some (Option.value_exn (Int32.of_int (((i+1) * 8) + 8))), RBP, None, None)
  );
  List.fold f.body 
    ~init:{ops_r = ops_rev; table; num_spills; spilled_temp_bytes = ref 0; exit_label} 
    ~f:conv_instr
  (* === Write function epilogue === *)
  |> fun info -> 
    (* exit_label; pop callee saves; leave; ret *)
    let ops_r' = List.fold used_callee_saves ~init:(X.Label exit_label :: info.ops_r) 
      ~f:(fun acc r -> X.Pop r :: acc) in
    {info with ops_r = 
      X.Ret :: 
      X.Leave :: 
      ( if callee_aligned (* assumes prologue is aligned *)
        then ops_r'
        else 
          (X.Add (Quad, X.Imm (`Long 8l), X.Reg RSP)) :: ops_r'
      )
    }
  |> fun info -> (List.rev info.ops_r, !(info.spilled_temp_bytes))
  |> fun (instr_list, spilled_temp_bytes) ->
    (* Ceil up to 8x *)
    let temp_bytes = Int32.of_int_exn (
      ((spilled_temp_bytes + 4) / 8 * 8)
    ) in 
    let instr_list' = List.fold used_callee_saves ~init:instr_list 
      ~f:(fun acc r -> X.Push (X.Reg r) :: acc) in
    (* PROLOGUE make globl, fn header, push rbp, move rsp, push callee saves *)
    (X.Directive (sprintf "globl \"%s\"" (label_to_str f.name))) ::
    (X.Label f.name) ::
    (X.Push (X.Reg RBP)) ::
    (X.Mov (Quad, X.Reg RSP, X.Reg RBP)) ::
    (X.Sub (Quad, X.Imm (`Long temp_bytes), X.Reg RSP)) :: 
    ( if Bool.equal callee_aligned Int32.(temp_bytes % 16l = 0l) 
      then instr_list'
      else X.Push (X.Imm (`Long Int32.zero)) :: instr_list'
    )

let convert (p : AS.program) = 
    (* add the global labels like functions *)
    (* [ X.Directive (sprintf "globl \"%s\"" (Symbol.name sigusr_label)) *)
    [ X.Label sigusr_label
    ; X.Mov (Long, X.Imm (`Long sigusr2), X.Reg RDI)
    ; X.Call (Quad, raise_sym)
    ] ::
    (* [ X.Directive (sprintf "globl \"%s\"" (Symbol.name sigfpe_label)) *)
    [ X.Label sigfpe_label
    ; X.Mov (Long, X.Imm (`Long 0l), X.Reg RAX)
    ; X.Cltd
    ; X.Idiv (Long, X.Reg RAX)
    ] ::
    (List.map p ~f:convert_function)