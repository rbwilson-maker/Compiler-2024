structure Checkpoint =
struct

  fun verifyOneL1 (suiteDir, lib, file) =
    let
      val test = suiteDir // file
    in
      case Outcome.readL1CheckpointDirective test of
        NONE =>
          ( printqf 0 (Util.dull "-- Cannot read directive from %s --") [string test]
          ; Either.ERR TestFile.INVALID
          )
       | SOME target_score =>
          let
            val () = printqf 0 "\n-- Testing %s --" [string test]
            val outcome = Compiler.runL1Verifier test target_score
            val expected = Outcome.RETURN 0
            val log = NONE (* TODO: maybe don't do this -- thea *)
          in
            ( Outcome.report test log { expected = expected, actual = outcome }
            ; Either.OK (Summary.summarize { expected = expected, actual = outcome })
            )
          end
    end

  fun verifyOneL2 (suiteDir, lib, file) =
    let
      val test = suiteDir // file
      val () = printqf 0 "\n-- Testing %s --" [string test]
      val expected = Outcome.RETURN 0
      val log = NONE (* TODO: maybe don't do this -- thea *)
      val filebase = String.extract (file, 0, SOME (String.size file - 3))
      val refDirs = List.map (fn dir => suiteDir ^ "-" ^ dir) (!Config.dataflowArgs)
      val refFiles = List.map (fn refDir => refDir // filebase ** SOME "out") refDirs

      fun helper args files =
        case (args, files) of
          ([], []) => Outcome.RETURN 0
        | (arg :: args', file :: files') =>
            if not (Util.isRead file)
            then Outcome.COMPILERFAIL ("Reference output file does not exist: " ^ file)
            else
              let
                val outcome = Compiler.runL2Verifier test file ["--r2", arg]
              in
                if outcome = expected then helper args' files' else outcome
              end
        | _ => raise Fail "Impossible"
      val outcome = helper (!Config.dataflowArgs) refFiles
    in
      ( Outcome.reportNoDirective test log { expected = expected, actual = outcome }
      ; Either.OK (Summary.summarize { expected = expected, actual = outcome })
      )
    end

  (* I believe this was used for generating checkpoints tests,
   * specifically L2 checkpoint, but not entirely sure -- thea s23
   *)
  fun generate1 (suiteDir, lib, file) =
    let
      val test = suiteDir // file
      val checkpointDir = suiteDir ^ "-checkpoint"
      val inputFile = checkpointDir // file ** SOME "in"
      val args =
        case lib
          of Compiler.NOLIB "l1" => [ "--gen" ]
           | Compiler.NOLIB "l2" => [ "--gen2" ]
           | _ => raise Fail ("input generator not supported for " ^ suiteDir)
      val outcome1 = Compiler.generateFile test inputFile args
      val outcome2 =
        case (outcome1, lib)
          of (Outcome.COMPILE, Compiler.NOLIB "l2") =>
              Compiler.generateL2RefOutputs suiteDir file
           | _ => outcome1
    in
      Either.OK (Summary.summarize { expected = Outcome.COMPILE, actual = outcome2 })
    end

  fun verifySuite (suite as { suiteName, suiteDir }) =
    ( printq 6 (Util.bold ("-- Verifying compiler on " ^ suiteName ^ " --"))
    ; if String.isPrefix "l1" suiteName
      then Suite.map verifyOneL1 suite
      else if String.isPrefix "l2" suiteName
      then Suite.map verifyOneL2 suite
      else raise Fail ("Checkpoint verifier not supported for " ^ suiteName)
    )

  fun generateL2Suites checkpointDir =
    List.app
      (fn dir => Util.mkdir (".." // "tests" // (checkpointDir ^ "-" ^ dir)))
      (!Config.dataflowArgs)

  fun generateSuite (suite as { suiteName, suiteDir }) =
    let
      val checkpointDir = suiteName ^ "-checkpoint"
    in
      ( printq 6 (Util.bold ("-- Generate checkpoint input for " ^ suiteName ^ " --"))
      ; if String.isPrefix "l2" suiteName then generateL2Suites checkpointDir else ()
      ; Util.mkdir (".." // "tests" // checkpointDir)
      ; Suite.map generate1 suite
      )
    end
end
