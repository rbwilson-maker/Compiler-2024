(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *)
structure Tree :> TREE =
struct

  datatype binop = ADD | SUB | MUL | DIV | MOD

  datatype exp =
      CONST of Word32.word
    | TEMP of Temp.temp
    | BINOP of binop * exp * exp
  and stm =
      MOVE of Temp.temp * exp
    | RETURN of exp

  type program = stm list

  structure Print =
  struct
    val pp_binop =
     fn ADD => "+"
      | SUB => "-"
      | MUL => "*"
      | DIV => "/"
      | MOD => "%"

    fun pp_exp exp =
      case exp of
        CONST x => Word32Signed.toString x
      | TEMP t => Temp.name t
      | BINOP (binop, e1, e2) =>
          "(" ^ pp_exp e1 ^ " " ^ pp_binop binop ^ " " ^ pp_exp e2 ^ ")"

    fun pp_stm stm =
      case stm of
        MOVE (t1,e2) => Temp.name t1 ^ "  <--  " ^ pp_exp e2
      | RETURN e => "return " ^ pp_exp e

    fun pp_program [] = ""
      | pp_program (stm::stms) = pp_stm stm ^ "\n" ^ pp_program stms
  end
end
