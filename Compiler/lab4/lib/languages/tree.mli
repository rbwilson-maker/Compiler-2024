(* Rachompicole L3 Compiler
 * IR Trees
 * Authors: Rachel Wilson and Nicole Fang
 * Adapted from L1 Compiler
 * 
 * The IR Tree turns everything into branches and jumps thus
 * removing a distinction between loops and conditionals.
 * 
 * Still modally separates pure expressions and commands.
 * Still includes ternary expressions, so this is
 * not quite in basic block form yet for program analysis.
 *)
type label = Symbol.t
type field = Symbol.t * Symbol.t
type temp = Temp.t * Size.t
type param = Temp.t * Size.t

type binop =
  | Add | Sub | Mul 
  | Less | Greater | Leq | Geq 
  | Bit_and | Bit_or | Bit_xor | Lshift | Rshift
type equal_op = Equal | Neq
type effect_op = Div | Mod
type signal = Sigfpe | Sigabrt

(* This is something that is represented by a pure address.
* It's address value should be statically calculable .
*
* This definition of an address ensures safety of 
* pointer dereferences and array accesses can be checked.
* Unsafe addresses would include array indexes and 
* derefs.
*)
type addr = 
  (* The value of a struct is represented by its address *)
  | Field_addr of 
    { strct : addr
    ; field : field
    }
  (* This is the address base case *)
  | Addr_temp of Temp.t
  | Unsafe of unsafe_addr

and unsafe_addr = 
  (* exp or just addr? *)
  | Array_addr of
    { array : exp
    ; index : exp
    ; elem_size : Size.data_size
    }
  | Deref of addr

(* exp is truly pure *)
and exp =
  | Const of Int32.t
  | True | False
  | Temp of temp
  | Binop of 
    { lhs : exp
    ; op : binop
    ; rhs : exp
    }
  | Equality of
    { lhs : exp
    ; rhs : exp
    ; op : equal_op
    ; size : Size.t
    }
  | Addr of addr
  | Null

(* Effectful commands *)
type stm =
  (* dest <-- src*)
  | Move of
    { dest : temp
    ; src : exp
    }
  (* dest <-- lhs div/mod rhs *)
  | Effect_move of 
    { dest : temp
    ; lhs : exp
    ; op : effect_op
    ; rhs : exp 
    }
  (* dest <-- fn(args) *)
  | Fn_move of
    { dest : temp option (* None for void fns *)
    ; fn : label
    ; args : (exp * Size.t) list
    }
  (* dest <-- M[src] *)
  | Read_mem of
    { dest : temp
    ; src : addr
    }
  (* M[dest] <--size src *)
  | Write_mem of 
    { dest : addr
    ; size : Size.t
    ; src : exp 
    }
  | Check_null of exp
  (* dest <-- array{elem_size}[index] *)
  | Array_addr of
    { dest : Temp.t
    ; array : exp
    ; index : exp
    ; elem_size : Size.data_size
    }
  (* dest <-- alloc(elem_size, array_len) *)
  | Alloc of 
    { dest : temp
    ; elem_size : Size.data_size
    ; array_len : exp option (* length, None if not an array *)
    }
  | Branch of 
    { condition : exp
    ; if_label : label
    ; else_label : label option
    ; after_label : label
    }
  | Label of label
  | Goto of label
  | Return of (exp * Size.t) option
  | Raise of signal

type fun_stms = label * param list * stm list
type program = fun_stms list

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
 