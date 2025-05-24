(* utils/Either.sml : 1.1-27.1 *)
(* molasses-file4.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
in
(* Basic sum type *)
(* Either is either an: error of some value, or a successful result (ok) of some
 * value. *)
structure Either : EITHER =
  struct
    datatype ('a, 'b) t = ERR of 'a | OK of 'b

    fun map f _ (ERR x) = ERR (f x)
      | map _ g (OK x) = OK (g x)

    fun toValue (ERR x) = JSON.OBJECT [("ERR", x)]
      | toValue (OK x) = JSON.OBJECT [("OK", x)]
    fun fromValue (JSON.OBJECT [("ERR", x)]) = ERR x
      | fromValue (JSON.OBJECT [("OK", x)]) = OK x
      | fromValue _ = raise Fail "Either.fromValue"

    fun partition _ [] = ([], [])
      | partition f (x :: xs) =
        let val (errs, oks) = partition f xs in
          case f x of
            ERR err => (err :: errs, oks)
          | OK ok => (errs, ok :: oks)
        end
  end

end
