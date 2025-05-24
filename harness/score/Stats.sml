(**************************************************************************)
(* Run forked process with timeout, redirecting standard input and output *)
(**************************************************************************)
structure Stats :> STATS =
struct
  type 'a item =
    { suite: string
    , badTests: (string * string) list
    , goodTests: ((string * 'a * string) * (Summary.cat * Summary.res)) list
    }
  type 'a result = 'a item list

  fun report1 { suite, badTests, goodTests } =
    let
      val n = length badTests + length goodTests
      val (succ, time) = List.foldr (fn ((_, value), (succ, time)) =>
          case value of
            (_, Summary.SUCC) => (succ+1, time)
          | (_, Summary.TIME) => (succ, time+1)
          | _ => (succ, time)
        ) (0, 0) goodTests
      fun frac m = Int.toString m ^ " / " ^ Int.toString n
    in
      printqf 7 "%s: passed %s%s"
        [ string suite
        , string (frac succ)
        , string (if time > 0 then " timed out " ^ frac time else "")
        ]
    end

  fun report stats =
    let
      val badTests = concatMap #badTests stats
      val goodTests = concatMap #goodTests stats
      val _ =
        if null badTests then ()
        else
          ( printq 6 "Ill-formed test files:"
          ; List.app (fn (s, f) => printq 6 ("   " ^ (s // f))) badTests
          )
      val _ = List.app (
         fn ((_, Summary.SUCC), _) => ()
          | ((cat, res), tests) =>
              ( printq 6 (Summary.desc (cat, res))
              ; List.app (fn (s, _, f) => printq 6 ("   " ^ (s // f))) tests
              )
        ) (List.foldr (fn ((name, value), dict) =>
            MultiDict.add dict value name
          ) [] goodTests
        )
    in
      List.app report1 stats
    end
end
