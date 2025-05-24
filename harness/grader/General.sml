structure General =
struct

  (* Generate summaries for every observed outcome. Then,
   * use the summaries to merge outcomes into one. Failure
   * outcomes are always reported; successes are favored over
   * timeouts.
   *)
  fun mergeOutcomes (outcome1, outcome2) =
    let
      val (out1, (typ1, res1)) = outcome1
      val (out2, (typ2, res2)) = outcome2
    in
      case (typ1, res1, typ2, res2) of
        (_, Summary.FAIL, _, _)  => outcome1
      | (_, _, _, Summary.FAIL)  => outcome2
      | (_, Summary.SUCC, _, _)  => outcome1
      | (_, _, _, Summary.SUCC)  => outcome2
      | (Summary.RET, _, _, _)   => outcome1
      | (_, _, Summary.RET, _)   => outcome2
      | (Summary.RAISE, _, _, _) => outcome1
      | (_, _, Summary.RAISE, _) => outcome2
      | (Summary.COMP, _, _, _)  => outcome1
      | (_, _, Summary.COMP, _)  => outcome2
      | _ => outcome1
    end

  (* If we're only typechecking, we only want the typecheck outcome. *)
  fun maybeTypecheck directive =
    case directive of
      Outcome.ERROR => Outcome.ERROR
    | other =>
      if !Config.typecheckOnly then Outcome.TYPECHECK else other

  (* Test one file against student's c0c compiler *)
  fun one (suiteDir, lib, file) =
    let
      val test = suiteDir // file
    in
      case Outcome.readTestDirective test of
        NONE =>
          ( printqf 0 (Util.dull "-- Cannot read directive from %s --") [string test]
          ; Either.ERR TestFile.INVALID
          )
       | SOME expected =>
          if !Config.safeOnly andalso not (Outcome.isSafe expected)
          then Either.ERR TestFile.SKIP
          else if !Config.unsafeOnly andalso Outcome.isSafe expected
          then Either.ERR TestFile.SKIP
          else if !Config.skipFrontendTests
            andalso Outcome.isFrontendTest expected
          then Either.ERR TestFile.SKIP
          else
            let
              val expected = maybeTypecheck expected
              val () = printqf 0 "\n-- Testing %s --" [string test]
              val (actual, log) =
                  Compiler.runC0C (test, lib, !Config.compilerArgs) expected

              fun withActual actual = { expected = expected, actual = actual }
              val (actual, summary) =
                foldl mergeOutcomes
                  (Outcome.TIMEOUT, (Summary.TC, Summary.TIME))
                  (map (fn x => (x, Summary.summarize (withActual x))) actual)
            in
              ( Outcome.report test log (withActual actual)
              ; Either.OK summary
              )
            end
    end

  fun testSuite (suite as { suiteName, suiteDir }) =
    ( printq 6 (Util.bold ("-- Testing compiler on " ^ suiteName ^ " --"))
    ; Suite.map one suite
    )

end
