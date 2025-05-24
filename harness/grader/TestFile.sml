(* This file contains a bunch of information for checking a test file *)
structure TestFile =
struct
  fun validExt ext =
    case ext of
      "l1" => SOME "l1"
    | "l2" => SOME "l2"
    | "l3" => SOME "l3"
    | "l4" => SOME "l4"
    | "c0" => SOME "c0"
    | "c1" => SOME "c1"
    | _    => NONE

  local
    fun goodChar #"_" = true
      | goodChar #"-" = true
      | goodChar c =
          Char.isAlphaNum c andalso (Char.isLower c orelse Char.isDigit c)
  in
    fun validName name =
      List.all goodChar (explode name)
  end

  (* If a test passes at EXT, it should **fail** at prevExt EXT *)
  fun prevExt (SOME "l2") = SOME "l1"
    | prevExt (SOME "l3") = SOME "l2"
    | prevExt (SOME "l4") = SOME "l3"
    | prevExt (SOME "c1") = SOME "c0"
    | prevExt _ = NONE


  datatype testError = SKIP | INVALID

  (* Either we should skip a test or it's invalid *)
  fun testErrorToValue SKIP = JSON.STRING "skip"
    | testErrorToValue INVALID = JSON.STRING "invalid"
  fun testErrorFromValue (JSON.STRING "skip") = SKIP
    | testErrorFromValue (JSON.STRING "invalid") = INVALID
    | testErrorFromValue _ = raise Fail "testErrorFromValue"

end
