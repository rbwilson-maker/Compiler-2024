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

let unsafe_mode = ref false;;

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
  | T.Leq -> AS.Comp Leq
  | T.Geq -> AS.Comp Geq
;;

let newL = Symbol.unique_symbol

let munch_effectop = function
| T.Div -> AS.Div
| T.Mod -> AS.Mod

let munch_sig = function
| T.Sigfpe -> AS.Sigfpe
| T.Sigabrt -> AS.Sigabrt

let structs : Size.structs option ref = ref None
let get_offset (struct_name : Symbol.t) (field : Symbol.t) : int64 = 
  match !structs with
  | None -> failwith "structs should be initialized"
  | Some structs ->
    let fields, _size = Hashtbl.find_exn structs struct_name in
    snd (Symbol.Map.find_exn fields field)
;;

(* generates instructions to achieve dest <- e *)
let rec munch_exp_acc ?dest (size : Size.t) (exp : T.exp) (rev_acc : AS.instr list)
  : AS.instr list * AS.operand
  =
  let single_op = function
  | T.Const n -> AS.Imm (`Long n)
  | T.Temp t -> AS.Temp t (* will we need the size ??? *)
  | T.True -> AS.Imm (`Long Int32.one)
  | T.False -> AS.Imm (`Long Int32.zero)
  | T.Null -> AS.Imm (`Long Int32.zero)
  | _ -> failwith "do not call single_op on binops or addresses"
  in

  match exp with
  | T.Binop binop -> 
    munch_binop_acc dest (binop.op, binop.lhs, binop.rhs) rev_acc
  | T.Equality eq ->
    munch_eq_acc dest (eq.lhs, eq.op, eq.rhs, eq.size) rev_acc
  | T.Addr addr -> 
    (* return an operand containing the address itself *)
    (match addr with
    | T.Addr_temp t -> rev_acc, AS.Temp (t, Quad)
    | T.Unsafe (T.Deref addr) ->
      if not !unsafe_mode then failwith "Do no generate unsafe addresses in safe mode";
      let (rev_acc', src) = munch_addr_acc addr rev_acc in
      let dest = AS.Temp (Temp.create (), Quad) in
      (AS.Mov {dest; src = AS.Addr src; size} :: rev_acc', dest)
    | _ -> 
      let (rev_acc', src) = munch_addr_acc addr rev_acc in
      let dest = AS.Temp (Temp.create (), Quad) in
      (AS.Mov_addr {dest; src} :: rev_acc', dest)
    )
  | x -> (match dest with 
    | None -> rev_acc, single_op x
    | Some dest -> AS.Mov {dest; src = single_op x; size} :: rev_acc, dest
  )

(* creates a destination for the binop *)
and munch_binop_acc
  (dest : AS.operand option)
  ((binop, e1, e2) : T.binop * T.exp * T.exp)
  (rev_acc : AS.instr list)
  : AS.instr list * AS.operand
  =
  let rev_acc', t1 = munch_exp_acc Long e1 rev_acc in
  let rev_acc'', t2 = munch_exp_acc Long e2 rev_acc' in
  let dest = match dest with None -> AS.Temp (Temp.create (), Long) | Some d -> d in
  AS.Binop { op = munch_op binop; dest; lhs = t1; rhs = t2 } :: rev_acc'', dest

and munch_eq_acc
  (dest : AS.operand option)
  ((e1, eq_op, e2, s) : T.exp * T.equal_op * T.exp * Size.t)
  (rev_acc : AS.instr list)
  : AS.instr list * AS.operand
  =
  let rev_acc', t1 = munch_exp_acc Long e1 rev_acc in
  let rev_acc'', t2 = munch_exp_acc Long e2 rev_acc' in
  let dest = match dest with None -> AS.Temp (Temp.create (), Long) | Some d -> d in
  AS.Equal 
    { lhs = t1; rhs = t2
    ; dest; size = s
    ; op = match eq_op with Equal -> `Eq | Neq -> `Neq
    } :: rev_acc'', dest

(* For branches, generations instructions for computing a comparison expression.
   If the expression is a bool or a temp, it becomes a `Single, and if it uses
   a comparison operator like <, it's a `Comparison, which is needed to generate
   assembly instruction
*)
and munch_comp_acc
  (e : T.exp) 
  (rev_acc : AS.instr list) 
  : AS.instr list * [`Single of (AS.operand * Size.t) | `Comparison of AS.comparison]
= match e with
| T.Binop b -> 
  (match munch_op b.op with
  | AS.Comp c -> 
    let rev_acc', t1 = munch_exp_acc Long b.lhs rev_acc in
    let rev_acc'', t2 = munch_exp_acc Long b.rhs rev_acc' in
    (rev_acc'', `Comparison {lhs=t1; rhs=t2; op=c; size = Long})
  | _ -> 
    let rev_acc', t = munch_exp_acc Long e rev_acc in
    rev_acc', `Single (t, Long)
  )
| T.Equality e ->
  let rev_acc', t1 = munch_exp_acc Long e.lhs rev_acc in
  let rev_acc'', t2 = munch_exp_acc Long e.rhs rev_acc' in
  let op : AS.compop = (match e.op with Equal -> Equal | Neq -> Neq) in
  (rev_acc'', `Comparison {lhs=t1; rhs=t2; op; size = e.size})
| e ->
  let rev_acc', t = munch_exp_acc Long e rev_acc in
    rev_acc', `Single (t, Long)

(* munch_addr_acc takes an address and returns a memory reference to that
   address represented by an AS.addr *)
and munch_addr_acc
  (addr : T.addr)
  (rev_acc : AS.instr list)
  : AS.instr list * AS.addr
= match addr with
| T.Addr_temp t -> rev_acc, { disp = None; base = AS.Temp (t, Quad); index = None; scale = None }
| T.Field_addr {strct; field = (strct_name, field)} -> 
  let offset = get_offset strct_name field in
  let (rev_acc', addr) = munch_addr_acc strct rev_acc in
  let disp = match addr.disp with 
  | None -> offset
  | Some d -> Int64.(Int32.to_int64 d + offset)
  in
  (* if the displacement is more than 32-bits: (most general)
    t <--q disp
    t <--addr (t, index, scale)
    (base, t)
   *)
    (match Int64.to_int32 disp with
    | Some _ as disp-> rev_acc', {addr with disp}
    | None -> 
      let t = AS.Temp (Temp.create (), Quad) in
      AS.Mov_addr {dest = t; src = {addr with disp = None; base = t}}
      :: AS.Mov {src = AS.Imm (`Quad disp); dest = t; size = Quad}
      ::rev_acc'
      , {disp = None; base = addr.base; index = Some t; scale = None}
    )
| Unsafe u ->
  if !unsafe_mode
  then munch_unsafe_addr_acc u rev_acc
  else failwith "This address should only be generated in unsafe mode"

and munch_unsafe_addr_acc 
  (addr : T.unsafe_addr)
  (rev_acc : AS.instr list)
  : AS.instr list * AS.addr
= match addr with
| Array_addr {array; index; elem_size} -> (
  let rev_acc', array = munch_exp_acc Quad array rev_acc  in
  let rev_acc'', idx = munch_exp_acc Long index rev_acc' in
  match elem_size with
  | Size.Small s ->
    (match idx with
    | AS.Temp _ as i -> rev_acc'', {disp = None; base = array; index = Some i; scale = Some s}
    | _ as i -> 
      let t =  AS.Temp (Temp.create (), Long) in
      AS.Mov {dest = t; src = i; size = Long} :: rev_acc'', 
      {disp = None; base = array; index = Some t; scale = Some s}
    ) 
  | Size.Struct (size) ->
    let length = (
      match Int64.to_int32 size with
      | Some d -> `Long d
      | None -> `Quad size
    ) in
    let offset = AS.Temp (Temp.create (), Long) in
    AS.Binop {op = Mul; dest = offset; lhs = AS.Imm (length); rhs = idx}
    :: rev_acc'', {disp = None; base = array; index = Some offset; scale = None}
  )
| Deref addr -> (
    let rev_acc', addr = munch_addr_acc addr rev_acc in
    let base = AS.Temp (Temp.create (), Quad) in
    AS.Mov { dest = base; src = AS.Addr addr; size = Quad }
    :: rev_acc', {disp = None; base; index = None; scale = None}
  )
;;

(* munch_stm : T.stm -> AS.instr list *)
let rec munch_stms_acc (stms : T.stm list) (rev_acc : AS.instr list) : AS.instr list =
  match stms with 
  | T.Move {dest = (t, s); src} :: stms -> 
    let rev_acc', _ = munch_exp_acc ~dest:(AS.Temp (t, s)) s src rev_acc in
    munch_stms_acc stms rev_acc'
  | T.Return e :: stms -> (match e with
    | None -> AS.Return :: rev_acc |> munch_stms_acc stms
    | Some (e, s) -> 
      let rev_acc', _ = munch_exp_acc ~dest:(AS.Reg RAX) s e rev_acc in
      AS.Return :: rev_acc' |> munch_stms_acc stms
    )
  | T.Label l :: stms -> AS.Label l :: rev_acc |> munch_stms_acc stms
  | T.Effect_move {dest = t; op; lhs; rhs} :: stms ->
    let rev_acc', t1 = munch_exp_acc Long lhs rev_acc in
    let rev_acc'', t2 = munch_exp_acc Long rhs rev_acc' in
    AS.Binop {dest = AS.Temp t; op = munch_effectop op; lhs = t1; rhs = t2}
    :: rev_acc''
    |> munch_stms_acc stms
  | T.Fn_move mv :: stms -> 
    let (args, rev_acc') = List.fold mv.args ~init:([], rev_acc) 
      ~f:(fun (args, acc) (e, s) ->
        let stms, t = munch_exp_acc s e acc in
        ((t, s)::args, stms)
      )
    in AS.Call 
      {dest = (match mv.dest with 
        | None -> AS.Reg RAX
        | Some t -> AS.Temp t
        )
      ; size = Option.map mv.dest ~f:(fun (_, s) -> s)
      ; fn = mv.fn
      ; params = List.rev args
      } :: rev_acc'
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
  | T.Alloc {dest = (t, s); elem_size; array_len} :: stms ->
    let calloc_sym = (
    match Sys.getenv "UNAME" with
    | Some "Darwin" -> Symbol.symbol "_calloc"
    | _ -> Symbol.symbol "calloc"
    ) in
    (match array_len with
    | None -> 
      (* alloc(size) ==>
       *    t <-- calloc(1, elem_size)
       *)
      AS.Call
      { dest = AS.Temp (t, s)
      ; size = Some s
      ; fn = calloc_sym
      ; params = [
        (* size_t nmemb *)
        (AS.Imm (`Long Int32.one), Long);
        (* size_t size *)
        (AS.Imm (`Quad (Size.to_int64 elem_size)), Quad)
      ]} :: rev_acc

    | Some l -> 
      if !unsafe_mode
      then (
        (* alloc_array(size, len)  ==>
        *     len   <--Long  munch l
        *      t    <--s     calloc(len, size)
        *)
        let rev_acc', len = munch_exp_acc Long l rev_acc in
        AS.Call
        { dest = AS.Temp (t, s)
        ; size = Some s
        ; fn = calloc_sym
        ; params = [
          (* size_t nmemb *)
          (len, Long);
          (* size_t size *)
          (AS.Imm (`Quad (Size.to_int64 elem_size)), Quad)
        ]} :: rev_acc'
      ) else (
      (* alloc_array(size, len)  ==>
       *     len   <--Long  munch l
       *     branch len < 0 ifl afterl
       *     if1:
       *       raise sigusr
       *     after1:
       *     len_1 <--Long  len + {1, 2}  // 2 if elem_size = Long
       *     t     <--s     calloc(len_1, size)
       *     branch t != 0 if after // if calloc returns null, let it fail later
       *     if2: 
       *      M[t]  <--Long  len
       *      t     <--addr  t + len_mem_buffer
       *     after2:
       *)
      let rev_acc', len = munch_exp_acc Long l rev_acc in
      let if1, after1, if2, after2 = newL "if", newL "after", newL "if", newL "after" in
      let len_scale, len_mem_buffer = (
        match elem_size with
        | Small Word | Small Byte -> failwith "shouldn't have tiny array sizes"
        | Small Long -> 2l, 8L
        | s -> 1l, Size.to_int64 s
        ) in
      (* Check for 64-bit displacement *)
      let (rev_acc'', src) : AS.instr list * AS.addr = (
        match Int64.to_int32 len_mem_buffer with
        | Some _ as disp -> rev_acc', {disp; base = AS.Temp (t, s); index = None; scale = None}
        | None ->
          let idx = AS.Temp (Temp.create (), Quad) in
          AS.Mov {dest = idx; src = AS.Imm (`Quad len_mem_buffer); size = Quad}
          :: rev_acc', {disp = None; base = AS.Temp (t, s); index = Some idx; scale = None}
      ) in
      (* len+1 should really be a quad, but our binop does not allow for it *)
      let len_1 = AS.Temp (Temp.create (), Long) in
      AS.Label after2
      :: AS.Mov_addr { dest = AS.Temp (t, s); src }
      :: AS.Mov
        { dest = AS.Addr 
          {disp = None
          ; base = AS.Temp (t, s)
          ; index = None; scale = None
          }
        ; size = Long
        ; src = len
        }
      :: AS.Label if2
      :: AS.Branch 
          { if_label = if2; else_label = None; after_label = after2
          ; cond = `Comparison {lhs = AS.Temp (t, s); rhs = AS.Imm (`Long Int32.zero); op = Neq; size = Quad}}
      :: AS.Call
        { dest = AS.Temp (t, s)
        ; size = Some s
        ; fn = calloc_sym
        ; params = [
          (* size_t nmemb *)
          (len_1, Long);
          (* size_t size *)
          (AS.Imm (`Quad (Size.to_int64 elem_size)), Quad)
        ]} 
      :: AS.Binop
        { op = Add
        ; dest = len_1
        ; lhs = len
        ; rhs = AS.Imm (`Long len_scale)
        }
      :: AS.Label after1
      :: AS.Raise Sigusr
      :: AS.Label if1
      :: AS.Branch 
        { if_label = if1; else_label = None; after_label = after1;
          cond = `Comparison 
          { lhs = len; rhs = AS.Imm (`Long Int32.zero); op = Less; size = Long }}
      :: rev_acc''
      ) 
    ) |> munch_stms_acc stms
  | T.Check_null e :: stms -> 
    if !unsafe_mode
    then rev_acc
    else (
      (* branch e == 0 if_label after_label 
      * if_label:
      *  raise sigusr
      * after_label:
      *)
      let (rev_acc', e) = munch_exp_acc Quad e rev_acc in
      let if_label, after_label = newL "if", newL "after" in
        AS.Label after_label
      :: AS.Raise Sigusr
      :: AS.Label if_label
      :: AS.Branch
        { if_label; else_label = None; after_label
        ; cond = `Comparison {lhs = e; op = Equal; rhs = AS.Imm (`Long Int32.zero); size = Quad}
        }
      :: rev_acc'
    ) |> munch_stms_acc stms
  | T.Write_mem {dest; size; src} :: stms ->
    let rev_acc', dest_mem = munch_addr_acc dest rev_acc in
    let rev_acc'', _ = munch_exp_acc ~dest:(AS.Addr dest_mem) size src rev_acc' in
    munch_stms_acc stms rev_acc''
  | T.Read_mem {dest = (t, size); src} :: stms ->
    let rev_acc', src_mem = munch_addr_acc src rev_acc in
    let dest = AS.Temp (t, size) in
    AS.Mov {dest; src = AS.Addr src_mem; size} :: rev_acc'
    |> munch_stms_acc stms
  | T.Array_addr {dest; array; index; elem_size} :: stms -> 
   (* 
    * Figure out if the length is a Quad imm or Long.
    * In the comparison, we will rely on i being treated as a quad if so.
    * 
    * a <-- munch array 
    * i <-- munch index 
    * // if elem_size is small
    * dest <--addr (a, i, small_size)
    * // if elem_size is a struct
    * offset <-- size * i
    * dest <--addr (a, offset)
    *)
    if !unsafe_mode 
    then (
      failwith "this insruction should not be generated in unsafe mode"
    ) else (
     (* 
      * Figure out if the length is a Quad imm or Long.
      * In the comparison, we will rely on i being treated as a quad if so.
      * 
      * a <-- munch array 
      * i <-- munch index 
      * len <-- m[array-len_buffer] (* with correct len size for move *)
      * branch i < 0 if_label else1 after_label
      * if_label:
      *   raise sigusr
      * else1: 
      *   branch i < len after_label if_label 
      * after_label:
      * // if elem_size is small
      * dest <--addr (a, i, small_size)
      * // if elem_size is a struct
      * offset <-- size * i
      * dest <--addr (a, offset)
      *)
      let len_buff = 
        (match elem_size with
        | Small Word | Small Byte -> failwith "shouldn't have tiny array sizes"
        | Small Long -> -8L
        | s -> Int64.neg (Size.to_int64 s)
        ) in
      let rev_acc', array = munch_exp_acc Quad array rev_acc  in
      let rev_acc'', i = munch_exp_acc ~dest:(AS.Temp (Temp.create (), Long)) Long index rev_acc' in
      let length_size, last_instrs = (
        match elem_size with
        | Size.Small s -> Size.Long, 
          AS.Mov_addr
            { src = {disp = None; base = array; index = Some i; scale = Some s}
            ; dest = AS.Temp (dest, Quad)
            } :: []
        | Size.Struct (size) ->
          let length, len_size = (
            match Int64.to_int32 size with
            | Some d -> `Long d, Size.Long
            | None -> `Quad size, Size.Quad
          ) in
          let offset = AS.Temp (Temp.create (), Quad) in len_size,
          AS.Binop {op = Mul; dest = offset; lhs = AS.Imm (length); rhs = i}
          :: AS.Mov_addr
            { src = {disp = None; base = array; index = Some offset; scale = None}
            ; dest = AS.Temp (dest, Quad)
            } 
          :: []
    ) in
    (* Check for 64-bit displacement *)
    let (rev_acc''', src_addr) : AS.instr list * AS.addr = (
      match Int64.to_int32 len_buff with
      | Some _ as disp -> rev_acc'', {disp; base = array; index = None; scale = None}
      | None ->
        let idx = AS.Temp (Temp.create (), Quad) in
        AS.Mov {dest = idx; src = AS.Imm (`Quad len_buff); size = Quad}
        :: rev_acc'', {disp = None; base = array; index = Some idx; scale = None}
    ) in
    let len = AS.Temp (Temp.create (), length_size) in
    let if_label, else1, after_label = newL "if", newL "else", newL "after" in 
    let rev_acc'''' = 
      AS.Label after_label
      :: AS.Branch
        { if_label = after_label; else_label = None; after_label = if_label
        ; cond = `Comparison {lhs = i; rhs = len; op = Less; size = length_size}}
      :: AS.Label else1
      :: AS.Raise Sigusr
      :: AS.Label if_label
      :: AS.Branch 
        { if_label; else_label = Some else1; after_label
        ; cond = `Comparison {lhs = i; rhs = AS.Imm (`Long Int32.zero); op = Less; size = Long}
        }
      :: AS.Mov 
        { src = AS.Addr src_addr
        ; dest = len; size = length_size
        }
      :: rev_acc''' 
    in
    List.rev_append last_instrs rev_acc'''' |> munch_stms_acc stms
    )
  | [] -> rev_acc
  

(* To codegen a series of statements, just concatenate the results of
 * codegen-ing each statement, and reverse it. *)
let codegen ~(unsafe : bool) (struct_sizes : Size.structs) (t : Tree.program) : AS.program = 
  structs := Some struct_sizes;
  unsafe_mode := unsafe;
  
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

