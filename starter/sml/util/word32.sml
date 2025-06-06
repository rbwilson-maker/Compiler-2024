(* L1 Compiler
 * Utilities for signed modular arithmetic
 * Author: Frank Pfenning
 *)

(*
 * There are two useful structure in the SML Basis Library
 * Int32, with 2's complement arithmetic,
 *        but it raises Overflow instead of doing modular arithmetic
 * Word32, with unsigned modular arithmetic
 *
 * This structure implements some signed operations on Word32
 *)

signature WORD32_SIGNED =
sig
  val TMAX : Word32.word	(* largest signed positive word, 2^31-1  *)
  val TMIN : Word32.word	(* smallest signed negative word -2^31 *)
  val ZERO : Word32.word	(* 0 *)
  val fromString : string -> Word32.word option	(* parse from string, no sign *)
        (* raises Overflow if not 0 <= n < 2^32 *)
  val toString : Word32.word -> string (* print to string, with sign *)
end

structure Word32Signed :> WORD32_SIGNED =
struct
  val TMIN = Word32.<<(Word32.fromInt 1, Word.fromInt(Word32.wordSize-1))
  val TMAX = Word32.-(TMIN, Word32.fromInt 1)
  val ZERO = Word32.fromInt 0
  fun neg w = Word32.>(w, TMAX)

  (* fromString does not allow leading "-" *)
  fun fromString str =
         (* scanString might also raise Overflow *)
   StringCvt.scanString (Word32.scan StringCvt.DEC) str

  fun toString w =
    if neg w
    then "-" ^ Word32.fmt StringCvt.DEC (Word32.~w)
    else Word32.fmt StringCvt.DEC w
end
