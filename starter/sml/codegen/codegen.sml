(* L1 Compiler
 * Assembly Code Generator for FAKE assembly
 * Author: Alex Vaynberg <alv@andrew.cmu.edu>
 * Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *
 * Implements a "convenient munch" algorithm
 *)
structure Codegen :> CODEGEN =
struct
  (* utility *)
  infix |>
  fun x |> f = f x

  structure T = Tree
  structure AS = Assem

  val munch_op =
   fn T.ADD => AS.ADD
    | T.SUB => AS.SUB
    | T.MUL => AS.MUL
    | T.DIV => AS.DIV
    | T.MOD => AS.MOD

  (* munch_exp : AS.operand -> T.exp -> AS.instr list *)
  (* munch_exp d e
   * generates instructions to achieve d <- e
   * d must be TEMP(t) or REG(r)
   *)
  fun munch_exp dest exp =
    let
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
      fun munch_exp_acc d exp rev_acc =
        case exp of
          T.CONST n => AS.MOV(d, AS.IMM n) :: rev_acc
        | T.TEMP t  => AS.MOV(d, AS.TEMP t) :: rev_acc
        | T.BINOP (binop, e1, e2) => munch_binop_acc d (binop, e1, e2) rev_acc

      (* munch_binop_acc dest (binop, e1, e2) rev_acc
       *
       * generates instructions to achieve dest <- e1 binop e2
       *
       * Much like munch_exp, this returns the result of appending the
       * instructions in reverse to the accumulator argument, rev_acc.
       *)
      and munch_binop_acc d (binop, e1, e2) rev_acc =
        let
          val operator = munch_op binop
          val t1 = AS.TEMP(Temp.new())
          val t2 = AS.TEMP(Temp.new())
          val rev_acc' = rev_acc
                      |> munch_exp_acc t1 e1
                      |> munch_exp_acc t2 e2
        in
          AS.BINOP (operator, d, t1, t2) :: rev_acc'
        end
    in
      munch_exp_acc dest exp []
      |> List.rev
    end

  (* munch_stm : T.stm -> AS.instr list *)
  (* munch_stm stm generates code to execute stm *)
  fun munch_stm stm =
    case stm of
      T.MOVE (t1, e2) => munch_exp (AS.TEMP t1) e2
    | T.RETURN e =>
        (* return e is implemented as %eax <- e *)
        munch_exp (AS.REG AS.EAX) e

  fun codegen [] = []
    | codegen (stm::stms) = munch_stm stm @ codegen stms

end
