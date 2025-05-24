structure Summary :> SUMMARY =
struct
  datatype cat = ERR | TC | COMP | RET | LOOP | RAISE | OR_LOOP of cat
  fun catToString ERR = "err"
    | catToString TC = "tc"
    | catToString COMP = "comp"
    | catToString RET = "ret"
    | catToString LOOP = "loop"
    | catToString RAISE = "raise"
    | catToString (OR_LOOP cat) = catToString cat ^ "-or-loop"
  fun catFromString "err" = ERR
    | catFromString "tc" = TC
    | catFromString "comp" = COMP
    | catFromString "ret" = RET
    | catFromString "loop" = LOOP
    | catFromString "raise" = RAISE
    | catFromString "raise-or-loop" = OR_LOOP RAISE
    | catFromString "ret-or-loop" = OR_LOOP RET
    | catFromString s = raise Fail ("Outcome.catFromString " ^ s)

  datatype res = TIME | SUCC | FAIL | REJECTED_STATIC_ANALYSIS
  fun resToString TIME = "time"
    | resToString SUCC = "succ"
    | resToString FAIL = "fail"
    | resToString REJECTED_STATIC_ANALYSIS = "rej"
  fun resFromString "time" = TIME
    | resFromString "succ" = SUCC
    | resFromString "fail" = FAIL
    | resFromString s = raise Fail ("Outcome.resFromString " ^ s)

  fun catDesc ERR = "typechecker to report an error"
    | catDesc TC = "code to parse and typecheck"
    | catDesc COMP = "code to compile after typechecking"
    | catDesc RET = "successful execution"
    | catDesc LOOP = "nontermination or stack overflow"
    | catDesc RAISE = "a runtime exception"
    | catDesc (OR_LOOP cat) = catDesc cat ^ " or nontermination/stack overflow"

  fun resDesc TIME = "timed out"
    | resDesc SUCC = "succeeded"
    | resDesc FAIL = "failed"
    | resDesc REJECTED_STATIC_ANALYSIS = "rejected by static analysis"

  fun desc (cat, res) = sprintf "Expected %s, %s:"
    [ string (catDesc cat), string (resDesc res) ]

  fun summarize (outcomes as { expected, actual }) =
    let
      open Outcome
      val result = if matches outcomes then SUCC else
        case actual of
          TIMEOUT => TIME
        | TYPECHECK => if expected = ERROR then FAIL else TIME
        | INFLOOP _ => TIME
        | _ => FAIL

      fun toCat outcome =
        case outcome of
          RETURN _ => RET
        | OR_INFLOOP (RETURN _) => OR_LOOP RET
        | ABORT => RAISE
        | ARITH => RAISE
        | MEMERROR => RAISE
        | OR_INFLOOP ABORT => OR_LOOP RAISE
        | OR_INFLOOP ARITH => OR_LOOP RAISE
        | OR_INFLOOP MEMERROR => OR_LOOP RAISE
        | TYPECHECK => TC
        | ERROR => ERR
        | COMPILE => COMP
        | INFLOOP _ => LOOP
        | _ => raise Fail "Impossible expected outcome."
    in (toCat expected, result)
    end

  fun toValue (cat, res) =
     JSON.ARRAY [ JSON.STRING (catToString cat), JSON.STRING (resToString res) ]
  fun fromValue value =
    case value of
      JSON.ARRAY [ JSON.STRING cat, JSON.STRING res ] =>
        (catFromString cat, resFromString res)
    | _ => raise Fail "Outcome.decode"
end
