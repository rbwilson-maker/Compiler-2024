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
  | Fn_name of label
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

type impure_exp = 
  | Pure of exp
  (* lhs div/mod rhs*)
  | Effect_binop of 
    { lhs : exp
    ; op : effect_op
    ; rhs : exp
    }
  (* fn (args) *)
  | Fn of
    { fn : [`Fn of label * bool | `Computed of exp]
    ; args : (exp * Size.t) list
    }
  (* M[addr]*)
  | Read_mem of addr
  | Array_addr of 
    { array : exp
    ; index : exp
    ; elem_size : Size.data_size
    }
  (* alloc(elem_size, array_len)*)
  | Alloc of 
    { elem_size : Size.data_size
    ; array_len : exp option (* length, None if not an array *)
    }

(* Effectful commands *)
type stm =
  | Move of 
    { dest : temp
    ; src : impure_exp
    }
  | Void_fn of 
    { fn : [`Fn of label * bool | `Computed of exp]
    ; args : (exp * Size.t) list
    }
  (* M[dest] <--size src *)
  | Write_mem of 
    { dest : addr
    ; size : Size.t
    ; src : exp 
    }
  | Check_null of exp
  | Branch of 
    { condition : exp
    ; if_label : label
    ; else_label : label option
    ; after_label : label
    }
  | Label of label
  | Goto of label
  (* When we return, the expression does not need to be pure... because we evaluate
     it before returning anyway. How to encode that... *)
  | Return of (impure_exp * Size.t) option
  | Raise of signal

type fun_stms = label * param list * stm list
type program = fun_stms list

module Print : sig
  val pp_exp : exp -> string
  val pp_stm : stm -> string
  val pp_program : program -> string
end
 