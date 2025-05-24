structure StaticAnalysis =
struct
  (* TODO: clean this up! -- thea s23 *)
  fun run dir { suite, goodTests, badTests } =
    let
      val (passingTests, failingTests) =
        List.partition
          (fn (_, (_, Summary.SUCC)) => true | _ => false)
          goodTests

      (* Generate artifacts for static analysis *)
      (* Unprocessable tests might just be ones that don't parse; that's fine *)
      val (passingTests, unprocessesableTests) = List.partition (fn ((s, _, f), _) =>
          let
            val test = s // f
            val () =
              ( Util.mkdir (s ^ "-fixed")
              ; Suite.prepareLogDirectory (#file (OS.Path.splitDirFile s) ^ "-fixed")
              )
          in
            case Runner.redirect
              { msg = "-- Generating static analysis artifacts for " ^ test ^ " --\n"
              , command = dir // "analyze-tests"
              , args =
                  [ test
                  , "--fix-c0-bug", (s ^ "-fixed") // f
                  ]
              , secs = !Config.COMPILER_TIMEOUT
              , out = Runner.SILENCE
              , err = Runner.SILENCE
              }
            of Posix.Process.W_EXITED => true
              | _ => false
          end
        ) passingTests

      (* Remove tests that duplicate an existing test *)
      val (passingTests, duplicateTests) =
        if not (!Config.failDuplicateTests orelse !Config.warnDuplicateTests)
        then (passingTests, [])
        else
        let
          val testDict =
            foldl (fn (test as ((s, _, f), _), map) =>
              StringMap.insert (map, s // f, test))
              StringMap.empty
              passingTests
          val args = map #1 (StringMap.listItemsi testDict)
          (* file for output where duplicate tests are written *)
          val tempFile = dir // "temp"
        in
          (* This is expensive, so we just run the command once for the whole
           * test suite, rather than running 20 times *)
          printq 3 ("-- Validating uniqueness of " ^ suite ^ " --\n");
          case Runner.redirect
            { msg = ""
            , command = dir // "diff-with-test-suite"
            , args =
                [ "-e", dir // "tests"
                , "-r", ".." // "runtime"
                , "-b", dir // "analyze-tests"
                ] @ args
            , secs = !Config.COMPILER_TIMEOUT
            , out = Runner.REDIRECT (Posix.FileSys.creat (tempFile, Util.rw))
            , err = Runner.UNCHANGED
            }
           of Posix.Process.W_EXITED =>
             let val (accAccept, accRej) =
                   Util.fold (fn (line, (accAccept, accRej)) =>
                     case StringMap.find (accAccept, line) (* Guard against remove exception *)
                       of NONE => (accAccept, accRej)
                        | SOME _ =>

                       let val (accAccept', data) = StringMap.remove (accAccept, line)
                       in
                         printqf 8 (Util.red (
                           "Test %s is a duplicate of an existing test in "
                           ^ "the test suite; please %s it to be a unique "
                           ^ "test."))
                           [ string line
                             (* Soften the language if not failure. *)
                           , string
                               (if !Config.failDuplicateTests
                                then "modify" else "consider modifying")
                           ];
                         (accAccept', StringMap.insert (accRej, line, data))
                       end)
                     (testDict, StringMap.empty)
                     tempFile
                val result as (listAccept, listRej) =
                 (StringMap.listItems accAccept, StringMap.listItems accRej)
             in
               (case (!Config.warnDuplicateTests, listAccept)
                  of (true, _ :: _) =>
                     ( printq 8 (Util.green (
                       "These tests aren't duplicates. We've never seen "
                         ^ "them before. Gold star!"));
                        List.app (fn ((d, _, f), _) =>
                          printqf 8 (Util.green "\t%s") [string (d // f)]) listAccept
                     )
                   | _ => ()
                );
                result
             end
            | _ => raise Fail "Unknown error in performing static analysis"
        end

      (* Remove tests with dodgy execution behavior in places where the
       * reference compiler deviates from the formal semantics. *)
      val (passingTests, dodgyTests) =
        if not (!Config.failDodgyTests) then (passingTests, []) else (
          printq 3 ("-- Validating correct semantics of " ^ suite ^ " --\n");
          List.partition (fn ((s, lib, f), (cat, _)) =>
            (* Don't worry about ones that are a static error. *)
            cat = Summary.ERR orelse
            let val expected = valOf (Outcome.readTestDirective (s // f))
                val (actual, _) =
                  (* Double compiler run timeout because the revised file will
                   * run a bit slower (twice as many bounds checks) *)
                  Util.withRefSetTo Config.COMPILER_TIMEOUT (3 * !Config.COMPILER_TIMEOUT) (fn () =>
                  Util.withRefSetTo Config.RUN_TIMEOUT (2 * !Config.RUN_TIMEOUT) (fn () =>
                      Compiler.runCC0 ((s ^ "-fixed") // f, lib, []) expected))
                val (_, res) =
                  Summary.summarize { actual = actual, expected = expected }
            in
              case res
                of Summary.SUCC => true
                 | _ => (
                     printqf 9 (Util.red (
                       "Test %s exhibits the reference compiler bug documented in "
                         ^ "dist/tests/l4-reference-compiler-bug. Please remove "
                         ^ "statements of the form 'a[x] += e', where the bounds "
                         ^ "check for a[x] fails, but evaluating e causes a "
                         ^ "different exception to be thrown. E.g. 'a[-1] += 1/0' "
                         ^ "is invalid, because the reference compiler raises FPE "
                         ^ "instead of correctly producing a memory error."))
                         [string (s // f)] ;
                      false )
            end)
            passingTests
          )

      val failingTests =
        let val reject =
          fn (x, (cat, _)) => (x, (cat, Summary.REJECTED_STATIC_ANALYSIS))
        in
          failingTests
            @ map (if !Config.failDuplicateTests
                   then reject else fn x => x) duplicateTests
            @ map reject dodgyTests
        end
    in
      { suite = suite
      , goodTests = passingTests @ unprocessesableTests @ failingTests
      , badTests = badTests
      }
    end
end
