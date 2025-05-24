(* grader/GeneralTest.sml : 1.1-120.1 *)
(* molasses-file22.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
(* Validates the correctness of a submitted test case *)
structure GeneralTest =
  struct
    datatype strictness = RELAXED of unit -> bool | STRICT of unit -> bool
    val customLib = fn Compiler.HEADER _ => true | _ => false

    fun checks file test lib expected =
      let val {base, ext} = OS.Path.splitBaseExt file in
        [ { check = RELAXED (fn () => size base <= ! Config.MAX_FILENAME_BASE)
          , failMsg = ["Filename too long: " ^ file]
          }
        , { check = RELAXED (fn () => TestFile.validName base)
          , failMsg =
              [ "Disallowed characters in filename " ^ file
              , "(only lowercase letters, numbers, '_' and '-' allowed)"
              ]
          }
        , { check = STRICT (fn () => Option.isNone expected)
          , failMsg = ["Cannot read directive for " ^ file]
          }
        , { check =
              STRICT
                (fn () =>
                   case expected of
                     SOME (Outcome.INFLOOP _) => false
                   | SOME (Outcome.OR_INFLOOP _) => false
                   | _ => true)
          , failMsg = ["Infloop directive not allowed in " ^ file]
          }
        , { check =
              STRICT
                (fn () => Option.map TestFile.validExt ext |> Option.isSome)
          , failMsg = ["Bad extension for " ^ file]
          }
        (* NB: THIS CODE IS NECESSARY FOR SECURITY *)
        (* Files with custom headers can cause essentially arbitrary
        * system behavior, and so it is critical to make sure that the
        * autograder never runs code with custom headers. That's why we
        * only allow 'typecheck' or 'error' outcomes for custom
        * headers. *)
        , { check =
              RELAXED
                (fn () =>
                   not (customLib lib) orelse expected = SOME Outcome.ERROR orelse expected = SOME
                                                                                                Outcome.TYPECHECK)
          , failMsg =
              [ "Bad directive for " ^ file
              , "(tests with custom header files can only test 'error'" ^ " or 'typecheck')"
              ]
          }
        (* Here's where we force tests to have the right extensions: make
        * sure an l3 test is not misclassified as an l4 test, etc. *)
        , { check =
              STRICT
                (fn () =>
                   expected = SOME Outcome.ERROR orelse case TestFile.prevExt
                                                               ext of
                                                          NONE => true
                                                        (* No good definition of 'previous' *)
                                                        | SOME prevLang =>
                                                            let
                                                              val () =
                                                                printqf 0
                                                                  "\n-- Testing %s at language %s --"
                                                                  [ string test
                                                                  , string
                                                                      prevLang
                                                                  ]
                                                              val (actual, log) =
                                                                Compiler.runCC0
                                                                  ( test
                                                                  , lib
                                                                  , [ "--standard=" ^ prevLang
                                                                    ]
                                                                  )
                                                                  Outcome.ERROR
                                                            in
                                                              actual = Outcome.ERROR
                                                            end)
          , failMsg =
              [ sprintf
                  "Test %s misclassified: it belongs to a simpler language"
                  [string test]
              ]
          }
        ]
      end

    fun validateAndMerge (_, acc as SOME _) = acc
      | validateAndMerge ({check, failMsg}, _) =
        case check of
          STRICT f => if f () then NONE else SOME failMsg
        | RELAXED f => if ! Config.relax orelse f () then NONE else SOME failMsg

    (* Print message in red *)
    fun fail msgs =
      ( printq 4 ""
      ; app (fn msg => printqf 4 (Util.red "-- %s --") [string msg]) msgs
      )

    (* Validate one file against the reference cc0 compiler *)
    fun one (suiteDir, lib, file) =
      let
        val test = suiteDir // file
        val expected = Outcome.readTestDirective test

        val checks = checks file test lib expected

      in
        (* Perform validations *)
        case foldl validateAndMerge NONE checks of
          SOME failMsgs => (fail failMsgs; Either.ERR TestFile.INVALID)
        | NONE =>
            let
              val expected = valOf expected
              val () = printqf 0 "\n-- Testing %s --" [string test]
              val (actual, log) = Compiler.runCC0 (test, lib, []) expected
            in
              ( Outcome.report test log {expected = expected, actual = actual}
              ; Either.OK
                  (Summary.summarize {expected = expected, actual = actual})
              )
            end
      end

    fun validateSuite (suite as {suiteName, suiteDir}) =
      ( printqf 6 (Util.bold "-- Validating %s in directory %s --")
          [string suiteName, string suiteDir]
      ; Suite.map one suite
      )
  end

end
