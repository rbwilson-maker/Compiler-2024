(* L1 Compiler
 * IR Trees
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *)
signature TREE =
sig
  datatype binop = ADD | SUB | MUL | DIV | MOD

  datatype exp =
      CONST of Word32.word
    | TEMP of Temp.temp
    | BINOP of binop * exp * exp
  and stm =
      MOVE of Temp.temp * exp
    | RETURN of exp

  type program = stm list

  structure Print :
  sig
    val pp_exp : exp -> string
    val pp_stm : stm -> string
    val pp_program : program -> string
  end
end
