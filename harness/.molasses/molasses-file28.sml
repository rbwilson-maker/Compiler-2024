(* top/Grader.sml : 1.1-247.1 *)
(* molasses-file28.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
structure Grader :> GRADER =
  struct
    (* General error handling, shared by all graders *)
    fun fail (Fail s) =
        (printq 7 (Util.red ("-- " ^ s)); Score.failure (); OS.Process.success)
      | fail (Args.BadOpt s) =
        ( TextIO.output (TextIO.stdErr, "Bad option: " ^ s ^ "\n")
        ; TextIO.output (TextIO.stdErr, Args.usage)
        ; OS.Process.failure
        )
      | fail Args.ShowUsage =
        (TextIO.output (TextIO.stdErr, Args.usage); OS.Process.success)
      | fail exn =
        ( TextIO.output
            (TextIO.stdErr, "Unexpected error: " ^ exnMessage exn ^ "\n")
        ; OS.Process.failure
        )

    fun printNoArgsMsg (default : string list) : string list =
      ( printq 7
          ("No args provided; defaulting to looking in directories: " ^ String.concatWith
                                                                          ", "
                                                                          default)
      ; printq 7
          ("To run different tests, provide the test directories as arguments to" ^ " the grading harness.")
      ; default
      )

    fun precompile {inFile, outFile} =
      case Runner.run
             { command = "gcc"
             , args = ["-c", inFile, "-o", outFile]
             , secs = ! Config.MAKE_TIMEOUT
             } of
        Posix.Process.W_EXITED => ()
      | _ => raise Fail ("Precompilation of " ^ inFile ^ " failed.")

    fun makeCompiler () =
      let
        val timer = Timer.startRealTimer ()
        val redirect =
          if ! Config.quiet <= 3 then
            Runner.UNCHANGED
          else
            Runner.SILENCE

        (* Use gcc to compile run411.c to run411.s *)
        val () = List.app precompile Compiler.toPrecompile

        (* If we are to make, then make sure it makes. *)
        val () =
          Option.app
            (fn optarg =>
               let
                 val (args, strarg) =
                   case optarg of
                     NONE => ([], "")
                   | SOME arg => ([arg], " " ^ arg)
               in
                 case Runner.redirect
                        { msg = "-- Building compiler (make" ^ strarg ^ ") --\n"
                        , command = "make"
                        , args = args
                        , secs = ! Config.MAKE_TIMEOUT
                        , out = redirect
                        , err = redirect
                        } of
                   Posix.Process.W_EXITED => ()
                 | _ => raise Fail "make did not succeed"
               end)
            (! Config.make)

        val elapsedTime = Time.toString (Timer.checkRealTimer timer)
      in
        (* Check to see if a valid executable was found *)
        if
          Util.isDir "bin" andalso OS.FileSys.access
                                     ("bin" // "c0c", [OS.FileSys.A_EXEC])
        then
          printq 2 ("-- Compiler built (elapsed time " ^ elapsedTime ^ "s) --")
        else
          raise Fail "make did not produce an executable bin/c0c";
        timer
      end

    fun benchCompiler args =
      let
        val () = printq 7 "-- 15-411 Compiler Benchmarks --"
        val suites =
          case Args.init (String.tokens Char.isSpace args) of
            [] => printNoArgsMsg ["bench"]
          | args => args
        val dirs = Suite.filter suites
        val timer = makeCompiler ()
        val () =
          precompile
            { inFile = ".." // "runtime" // "bench.c"
            , outFile = ".." // "runtime" // "bench.o"
            }
        val results = map Bench.benchSuite dirs
      in
        ( printqf 2 "-- Elapsed time %ss --" [time (Timer.checkRealTimer timer)]
        ; Score.benchmarks results
        ; OS.Process.success
        )
      end
        handle exn => fail exn

    fun selfTestCompiler args =
      let
        val () = printq 7 "-- 15-411 Compiler Tester (student tests) --"
        val () = ignore (Args.init (String.tokens Char.isSpace args))
        val timer = makeCompiler ()
        val () = printq 7 "-- Running 'make test' --"
        val status =
          Runner.run
            {command = "make", args = ["test"], secs = ! Config.MAKE_TIMEOUT}
        val () =
          printqf 2 "-- Elapsed time %ss --" [time (Timer.checkRealTimer timer)]
        val () =
          if not (! Config.autograder) then
            ()
          else
            JSONPrinter.print
              ( TextIO.stdOut
              , JSON.OBJECT
                  [ ("compiles", JSON.BOOL true)
                  , ( "tests"
                    , JSON.INT
                        (if Posix.Process.W_EXITED = status then
                           1
                         else
                           0)
                    )
                  , ("results", JSON.ARRAY [])
                  ]
              )
      in
        OS.Process.success
      end
        handle exn => fail exn

    fun verifyCheckpoint args =
      let
        val () = printq 7 "-- 15-411 Checkpoint Verifier --"
        val suites =
          case Args.init (String.tokens Char.isSpace args) of
            [] => printNoArgsMsg ["tests"]
          | args => args
        val timer = makeCompiler ()
        val dirs = Suite.filter suites
        val results = map Checkpoint.verifySuite dirs
      in
        ( printq 5 ("\n" ^ Util.bold "-- Summary --")
        ; printqf 2 "-- Elapsed time %ss --" [time (Timer.checkRealTimer timer)]
        ; Stats.report results
        ; Score.compiler results
        ; OS.Process.success
        )
      end
        handle exn => fail exn

    fun generateCheckpointInput args =
      let
        val () = printq 7 "-- 15-411 Checkpoint Input Generator --"
        val suites =
          case Args.init (String.tokens Char.isSpace args) of
            [] => printNoArgsMsg ["tests"]
          | args => args
        val timer = makeCompiler ()
        val dirs = Suite.filter suites
        val results = map Checkpoint.generateSuite dirs
      in
        OS.Process.success
      end
        handle exn => fail exn

    fun gradeCompiler args =
      let
        val () = printq 7 "-- 15-411 Compiler Tester --"
        val suites =
          case Args.init (String.tokens Char.isSpace args) of
            [] => printNoArgsMsg ["tests"]
          | args => args
        val dirs = Suite.filter suites
        val timer = makeCompiler ()
        val () =
          if ! Config.PARALLELISM <= 1 then
            ()
          else
            printqf 2 "-- Evaluating bin/c0c compiler using %d cores --"
              [int (! Config.PARALLELISM)]
        val results = map General.testSuite dirs
      in
        ( printq 5 ("\n" ^ Util.bold "-- Summary --")
        ; printqf 2 "-- Elapsed time %ss --" [time (Timer.checkRealTimer timer)]
        ; Stats.report results
        ; Score.compiler results
        ; OS.Process.success
        )
      end
        handle exn => fail exn

    fun gradeTests args =
      let
        val () = printq 7 "-- 15-411 Test Case Verifier --"
        val suites =
          case Args.init (String.tokens Char.isSpace args) of
            [] => printNoArgsMsg ["tests"]
          | args => args
        val dirs = Suite.filter suites

        (* Check reference compiler version *)
        val log = OS.FileSys.tmpName ()
        val () =
          case Runner.log
                 { command = ! Config.cc0
                 , args = ["-V"]
                 , secs = 1
                 , logBase = log
                 } of
            Posix.Process.W_EXITED =>
              let
                val f = TextIO.openIn (log ** SOME "err")
                val vers =
                  List.nth
                    (String.tokens Char.isSpace (valOf (TextIO.inputLine f)), 5) before TextIO.closeIn
                                                                                          f
              in
                case Int.fromString vers of
                  NONE =>
                    failwithf "Could not read %s as a version number (cc0 -V)"
                      [string vers]
                | SOME n =>
                    if n >= Config.MIN_VERSION then
                      printqf 0 "-- Reference compiler: %s version %d --"
                        [string (! Config.cc0), int n]
                    else
                      failwithf "Reference compiler version %d < %d"
                        [int n, int Config.MIN_VERSION]
              end
          | _ =>
              raise
                Fail
                  ("Reference compiler absent or outdated ("
                   ^ ! Config.cc0
                   ^ ")")

        val () =
          if ! Config.PARALLELISM > 1 then
            printqf 2 "-- Validating tests using %d cores --"
              [int (! Config.PARALLELISM)]
          else
            ()
        val results = map GeneralTest.validateSuite dirs

        (* Adjust the pass/failure of results using static analysis. *)
        val results =
          case ! Config.staticAnalysisDir of
            NONE => results
          | SOME dir => map (StaticAnalysis.run dir) results
      in
        ( printq 5 ("\n" ^ Util.bold "-- Summary --")
        ; Stats.report results
        ; if ! Config.autograder then
            JSONPrinter.print (TextIO.stdOut, Score.tests results)
          else
            ()
        ; OS.Process.success
        )
      end
        handle exn => fail exn
  end

end
