(* L1 Compiler
 * Assembly language
 * Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
 * Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *
 * Currently just a pseudo language with 3-operand
 * instructions and arbitrarily many temps
 *
 * We write
 *
 * BINOP  operand1 <- operand2,operand3
 * MOV    operand1 <- operand2
 *
 *)
structure Assem :> ASSEM =
struct

  datatype reg = EAX

  datatype operand =
     IMM of Word32.word
   | REG of reg
   | TEMP of Temp.temp

  datatype operation = ADD | SUB | MUL | DIV | MOD

  datatype instr =
     BINOP of operation * operand * operand * operand
   | MOV of operand * operand
   | DIRECTIVE of string
   | COMMENT of string

  (* functions that format assembly output *)

  fun format_reg EAX = "%eax"

  val format_binop =
   fn ADD => "+"
    | SUB => "-"
    | MUL => "*"
    | DIV => "/"
    | MOD => "%"

  val format_operand =
   fn IMM n   => "$" ^ Word32Signed.toString n
    | TEMP t  => Temp.name t
    | REG r   => format_reg r

  val format =
   fn BINOP(oper, d, s1, s2) =>
      "\t" ^ format_operand d
      ^ " <-- " ^ format_operand s1
      ^ " " ^ format_binop oper
      ^ " " ^ format_operand s2 ^ "\n"
    | MOV(d, s) =>
      "\t" ^ format_operand d
      ^ " <-- " ^ format_operand s ^ "\n"
    | DIRECTIVE str =>
      "\t" ^ str ^ "\n"
    | COMMENT str =>
      "\t" ^ "/* " ^ str ^ "*/\n"

end
