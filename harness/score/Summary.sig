signature SUMMARY =
sig
  (* Rather than keeping detailed records of all tests, we summarize
   * categories of tests and whether they did the right thing, wrong
   * thing, or timed out. A test that is supposed to compile and do
   * something will be in the category RET, LOOP, or RAISE if it
   * succeeds, but it can end up as a TC (typechecker) test if it
   * fails or times out, since that unit test is exposing an issue
   * with the typechecker. *)

  datatype cat = ERR | TC | COMP | RET | LOOP | RAISE | OR_LOOP of cat
  datatype res = TIME | SUCC | FAIL | REJECTED_STATIC_ANALYSIS

  (* Human readable summary, expected/got format *)
  val desc : cat * res -> string

  val catToString : cat -> string
  val catFromString : string -> cat
  val resToString : res -> string
  val resFromString : string -> res

  val summarize :
    { expected : Outcome.outcome, actual : Outcome.outcome } -> cat * res
  val toValue : cat * res -> JSON.value
  val fromValue : JSON.value -> cat * res
end
