(**************************************************************************)
(* Invoking the C0 compiler and running what comes out                    *)
(**************************************************************************)
structure Compiler :> COMPILER =
struct
  datatype lib =
      NOLIB of string
    | DEFAULT of string
    | HEADER of string * string

  type config =
    { safe : bool
    , opt : int option
    }

  type args = string * lib * string list list

  fun archive file =
    let
      val oldfile = file ** SOME "old"
    in
      ( if OS.FileSys.access (oldfile, [])
        then OS.FileSys.remove oldfile
        else ()
      ; if OS.FileSys.access (file, [])
        then OS.FileSys.rename { old = file, new = oldfile }
        else ()
      )
    end

  fun remove file =
    if OS.FileSys.access (file, []) andalso not (!Config.log)
    then OS.FileSys.remove file
    else ()

  fun getLogBase test =
    let
      val { dir = suiteDir, file = testFile } = OS.Path.splitDirFile test
      val { dir = parent, file = suite } = OS.Path.splitDirFile suiteDir
    in
      ".." // "log" // suite // testFile
    end

  (* Munch on the path names and check the state of the world, get a
   * filename for outputs that is scoped to the name of the given
   * file but with 'log' prepended to the path components, remove old
   * test files with that name.
   *)
  fun prepareTestBase test =
    let
      val { dir = suiteDir, file = testFile } = OS.Path.splitDirFile test
      val { dir = parent, file = suite } = OS.Path.splitDirFile suiteDir
      val logDir = ".." // "log" // suite
      val () =
        if Util.isDir logDir
        then ()
        else failwithf "Log directory %s not prepared" [string logDir]

     val (logBase, ext) =
        case OS.Path.splitBaseExt testFile of
           { base, ext = SOME ext } => (base ^ "-" ^ ext, ext)
         | _ => failwithf "Test file has no extension `%s`" [string testFile]
     val testLog = logDir // logBase
    in
      ( archive testLog
      ; archive (testLog ** SOME "s")
      ; archive (testLog ** SOME "abs")
      ; archive (testLog ** SOME "ll")
      ; archive (testLog ** SOME "bc")
      ; archive (testLog ** SOME "exe")
      ; archive (testLog ** SOME "out")
      ; archive (testLog ** SOME "err")
      ; archive (testLog ** SOME "compile")
      ; testLog
      )
    end

  fun destroyTestBase testLog =
    ( remove testLog
    ; remove (testLog ** SOME "s")
    ; remove (testLog ** SOME "abs")
    ; remove (testLog ** SOME "ll")
    ; remove (testLog ** SOME "bc")
    ; remove (testLog ** SOME "exe")
    ; remove (testLog ** SOME "out")
    ; remove (testLog ** SOME "err")
    ; remove (testLog ** SOME "compile")
    )

  val studentCompiler = "bin" // "c0c"
  val l1CheckpointVerifier = ".." // "verifier" // "verifier-"
  val l2CheckpointVerifier = ".." // "verifier" // "l2-verifier-"

  val runtimeNoLib =
    { inFile = ".." // "runtime" // "run411-no-lib.c"
    , outFile = ".." // "runtime" // "run411-no-lib.o"
    }

  val runtimeLib =
    { inFile = ".." // "runtime" // "run411.c"
    , outFile = ".." // "runtime" // "run411.o"
    }

  val runtimeLLVM =
    if OS.FileSys.access ("runllvm.c", [])
    then
      { inFile = "runllvm.c"
      , outFile = "runllvm.o"
      }
    else runtimeLib

  val toPrecompile =
    if runtimeLLVM = runtimeLib
    then [ runtimeNoLib, runtimeLib ]
    else [ runtimeNoLib, runtimeLib, runtimeLLVM ]

  (* Invoke a C0C-style typechecker (should produce no output) *)
  (* Returns: TIMEOUT, ERROR, or TYPECHECK (success) *)
  fun runC0CTypechecker test testLog redirect args =
    let
      val args = "-t" :: args @ [ test ]
      val cmd = String.concatWith " " ("bin/c0c" :: args)
    in
      case Runner.redirect
        { msg = "-- Typechecker: " ^ cmd ^ " --\n"
        , command = studentCompiler
        , args = args
        , secs = !Config.TC_TIMEOUT
        , out = redirect
        , err = redirect
        }
      of
        Posix.Process.W_EXITED => Outcome.TYPECHECK
      | Posix.Process.W_EXITSTATUS w =>
          if List.exists (fn w' => w = w') (!Config.timeStatus)
          then Outcome.TIMEOUT
          else Outcome.ERROR
      | Posix.Process.W_SIGNALED s =>
          if List.exists (fn s' => Util.signalEq (s, s')) (!Config.timeSig)
          then Outcome.TIMEOUT
          else Outcome.ERROR
      | Posix.Process.W_STOPPED _ => Outcome.ERROR
    end

  (* Invoke a C0C-style compiler (produces some file testLog.s) *)
  (* Presumes code passes typechecker *)
  (* Returns COMPILE (indicating successful compilation)
   *         TYPECHECK (indicating timeout)
   *         COMPILERFAIL (indicating other failure)
   *)
  fun runCompilerHelper test testLog redirect args suppressMsg runningCheckpoint =
    let
      val args = args @ [ test ]
      val cmd = String.concatWith " " ("bin/c0c" :: args)
      val msg = if suppressMsg then "" else "-- Compiler: " ^ cmd ^ " --\n"
    in
      case Runner.redirect
        { msg = msg
        , command = studentCompiler
        , args = args
        , secs = !Config.COMPILER_TIMEOUT
        , out = redirect
        , err = redirect
        }
      of
        Posix.Process.W_EXITED => () (* Expected, successful execution *)
      | Posix.Process.W_EXITSTATUS w =>
          if List.exists (fn w' => w = w') (!Config.timeStatus)
          then raise Overflow (* ===> TYPECHECK *)
          else if runningCheckpoint
          then failwithf "Compiler failed to generate .out file and return 0x%x" [ word8 w ]
          else failwithf "Compiler returned 0x%x after typechecking successfully" [ word8 w ]
      | Posix.Process.W_SIGNALED s =>
          if List.exists (fn s' => Util.signalEq (s, s')) (!Config.timeSig)
          then raise Overflow (* ===> TYPECHECK *)
          else if runningCheckpoint
          then failwithf "Compiler failed to generate .out file and raised 0x%s" [ string (SysWord.toString (Posix.Signal.toWord s)) ]
          else failwithf "Compiler raised 0x%s after typechecking successfully" [ string (SysWord.toString (Posix.Signal.toWord s)) ]
      | Posix.Process.W_STOPPED _ =>
          if runningCheckpoint
          then raise Fail "Compiler stopped when trying to generate .out file"
          else raise Fail "Compiler stopped after typechecking successfully"

      (* Move any files that were plausibly created to log directory *)
      ; Util.mv { from = test ** SOME "s", to = testLog ** SOME "s" }
      ; Util.mv { from = test ** SOME "abs", to = testLog ** SOME "abs" }
      ; Util.mv { from = test ** SOME "ll", to = testLog ** SOME "ll" }
      ; Util.mv { from = test ** SOME "bc", to = testLog ** SOME "bc" }
      ; Util.mv { from = test ** SOME "exe", to = testLog ** SOME "exe" }
      ; Outcome.COMPILE
    end handle Overflow => Outcome.TYPECHECK
             | Fail s => Outcome.COMPILERFAIL s

  fun runC0CCompiler test testLog redirect args =
    runCompilerHelper test testLog redirect args false false
  fun runCompilerForCheckpoint test testLog redirect args =
    runCompilerHelper test testLog redirect args false true
  fun runCompilerForCheckpointInputGenerator test testLog redirect args =
    runCompilerHelper test testLog redirect args true false

  fun runExe testLog args =
    let
      val exe = testLog ** SOME "exe"
      val out = testLog ** SOME "out"
    in
      case Runner.log
        { command = exe
        , args = args
        , secs = !Config.RUN_TIMEOUT
        , logBase = testLog
        }
      of
        Posix.Process.W_EXITED => (
          let
            val ins = TextIO.openIn out
            val res =
              Option.mapPartial IntInf.fromString (TextIO.inputLine ins)
          in
            (case (res, TextIO.inputLine ins) of
                (SOME i, NONE) => Outcome.RETURN i
              | _ => Outcome.RUNFAIL ("Error reading " ^ out)
            ) before TextIO.closeIn ins
          end handle _ => Outcome.RUNFAIL ("Error reading " ^ out))
      | Posix.Process.W_EXITSTATUS w => Outcome.RUNFAIL "Bad return code"
      | Posix.Process.W_SIGNALED s =>
          if Util.signalEq (s, Posix.Signal.bus)
          then Outcome.INFLOOP (SOME Outcome.BUS)
          else if Util.signalEq (s, Posix.Signal.segv)
          then Outcome.INFLOOP (SOME Outcome.SEGV)
          else if Util.signalEq (s, Posix.Signal.alrm)
          then Outcome.INFLOOP (SOME Outcome.ALRM)
          else if List.exists (fn s' => Util.signalEq (s, s')) (!Config.timeSig)
          then Outcome.INFLOOP NONE
          else if Util.signalEq (s, Posix.Signal.abrt)
          then Outcome.ABORT
          else if Util.signalEq (s, Posix.Signal.fpe)
          then Outcome.ARITH
          (* 12 is SIGUSR2 on Ubuntu, but we can't count on 12 being
           * SIGUSR2 everywhere. On OSX, it's a non-standard signal
           * SYS, so we cheat *)
          else if Util.signalEq (s, Posix.Signal.usr2)
          then Outcome.MEMERROR
          else if Util.signalEq (s, Posix.Signal.fromWord 0wxC)
          then Outcome.MEMERROR
          else Outcome.RUNFAIL (
            "Unexpected signal 0x" ^ SysWord.toString (Posix.Signal.toWord s)
          )
      | Posix.Process.W_STOPPED _ => Outcome.RUNFAIL "Process stopped"
    end

  fun link testLog runtime redirect =
    let
      val asm = testLog ** SOME "s"
      val exe = testLog ** SOME "exe"
    in
      case Runner.redirect
        { msg = "-- Running linker on " ^ asm ^ " --\n"
        , command = "gcc"
        , args = ["-m64", "-no-pie", runtime, asm, "-o", exe]
        , secs = !Config.GCC_TIMEOUT
        , out = redirect
        , err = redirect
        }
      of
        Posix.Process.W_EXITED => Outcome.COMPILE
      | status =>
          if Util.statusIsAlarm status
          then Outcome.COMPILERFAIL "GCC assembler/linker timed out"
          else Outcome.COMPILERFAIL "GCC assembler/linker failed"
    end

  fun linkAndRun testLog runtime redirect =
    case link testLog runtime redirect of
      Outcome.COMPILE =>
        ( Runner.print
            ( redirect
            , "-- Running executable " ^ (testLog ** SOME "exe") ^ " --\n"
            )
        ; runExe testLog []
        )
    | outcome => outcome

  fun llvm testLog extension arch redirect =
    case Runner.redirect
      { msg = sprintf "-- Running llvm compiler on %s --\n"
          [string (testLog ** SOME extension)]
      , command = "llc"
      , args = [ arch, "-O0", testLog ** SOME extension ]
      , secs = !Config.GCC_TIMEOUT
      , out = redirect
      , err = redirect
      }
    of
      Posix.Process.W_EXITED => Outcome.COMPILE
    | status =>
        if Util.statusIsAlarm status
        then Outcome.COMPILERFAIL "LLVM compiler (llc) timed out"
        else Outcome.COMPILERFAIL "LLVM compiler (llc) failed"

  fun llvmAndRun testLog extension runtime redirect =
    let
      val arch = "-march=x86-64"
    in
      case llvm testLog extension arch redirect of
        Outcome.COMPILE => linkAndRun testLog runtime redirect
      | outcome => outcome
    end

  fun tcOnly Outcome.TYPECHECK = true
    | tcOnly Outcome.ERROR = true
    | tcOnly _ = false

  fun parseScore verifierOutLog =
    let
      val line = Util.head verifierOutLog
    in
      case (String.tokens Char.isSpace) line of
        "Score:" :: num :: other => IntInf.fromString num
      | _ => NONE
    end

  fun verifyL1CheckpointOutput test testLog outfile target_score =
    let
      val verifierOutLog = testLog ** SOME "res"
      val verifierErrLog = testLog ** SOME "err"
      val outFD = Runner.REDIRECT (Posix.FileSys.creat (verifierOutLog, Util.rw))
      val errFD = Runner.REDIRECT (Posix.FileSys.creat (verifierErrLog, Util.rw))
      val system = if (!Config.mac) then "mac" else "ubuntu"
      val outcome =
        case Runner.redirect
          { msg = ""
          , command = l1CheckpointVerifier ^ system
          , args = [test, outfile]
          , secs = !Config.COMPILER_TIMEOUT
          , out = outFD
          , err = errFD
          }
        of
          Posix.Process.W_EXITED =>
            (* verifier ran without failure, score is number of registers used *)
            let
              val score = Option.valOf (parseScore verifierOutLog)
            in
              if score <= target_score
              then
                ( printqf 0 ("-- Target: %s. Registers used: %s --")
                    [ string (IntInf.toString target_score)
                    , string (IntInf.toString score)
                    ]
                ; Outcome.RETURN 0
                )
              else Outcome.RUNFAIL (
                sprintf "number of registers used: %s\n"
                  [string (IntInf.toString score)]
              )
            end
         | _ =>
            let
              val help_msg = ", check dist/log for .out file generated by compiler"
            in
              Outcome.RUNFAIL (Util.head verifierOutLog ^ help_msg)
            end
    in
      ( Util.mv { from = outfile, to = testLog ** SOME "out" }
      ; Runner.close outFD
      ; Runner.close errFD
      ; OS.FileSys.remove verifierOutLog
      ; OS.FileSys.remove verifierErrLog
      ; outcome
      )
    end

  fun verifyL2CheckpointOutput testLog outfile refOutfile =
    let
      val verifierErrLog = testLog ** SOME "err"
      val errFD = Runner.REDIRECT (Posix.FileSys.creat (verifierErrLog, Util.rw))
      val system = if (!Config.mac) then "mac" else "ubuntu"
      val outcome =
        case Runner.redirect
          { msg = ""
          , command = l2CheckpointVerifier ^ system
          , args = [outfile, refOutfile]
          , secs = !Config.COMPILER_TIMEOUT
          , out = errFD
          , err = errFD
          }
        of
          Posix.Process.W_EXITED => Outcome.RETURN 0
        | _ =>
          let
            val help_msg = ", check dist/log for .out file generated by compiler"
          in
            Outcome.RUNFAIL (Util.head verifierErrLog ^ help_msg)
          end
    in
      ( Util.mv { from = outfile, to = testLog ** SOME "out" }
      ; Runner.close errFD
      ; OS.FileSys.remove verifierErrLog
      ; outcome
      )
    end

  fun runL1Verifier test target_score =
    let
      (* change .in to .out *)
      val testbase = String.extract (test, 0, SOME (String.size test - 3))
      val outfile = testbase ** SOME "out"
      val testLog = getLogBase testbase
    in
      case runCompilerForCheckpoint test testLog Runner.UNCHANGED ["-r"] of
        Outcome.COMPILE =>
          let
            val () = printq 0 ("-- Running verifier on " ^ test ^ " and " ^ outfile ^ " --")
          in
            if Util.isRead outfile
            then verifyL1CheckpointOutput test testLog outfile target_score
            else Outcome.COMPILERFAIL
              ("Compiler ran but did not create " ^ outfile)
          end
      | outcome => outcome
    end

  fun runL2Verifier test refOutfile args =
    let
      (* change .in to .out *)
      val testbase = String.extract (test, 0, SOME (String.size test - 3))
      val outfile = testbase ** SOME "out"
      val testLog = getLogBase testbase
    in
      case runCompilerForCheckpoint test testLog Runner.UNCHANGED args of
        Outcome.COMPILE =>
          let
            val () = printq 0 ("-- Running verifier on " ^ outfile ^ " and " ^ refOutfile ^ " --")
          in
            if Util.isRead outfile
            then verifyL2CheckpointOutput testLog outfile refOutfile
            else Outcome.COMPILERFAIL
              ("Compiler ran but did not create " ^ outfile)
          end
      | outcome => outcome
    end

  fun generateFile test targetFile args =
    let
      val testLog = getLogBase test
      val inputFD = Runner.REDIRECT (Posix.FileSys.creat (targetFile, Util.rw))
    in
      case runCompilerForCheckpointInputGenerator test testLog inputFD args of
        Outcome.COMPILE =>
          ( printqf 0 "Successfully generated: %s" [string targetFile]
          ; Outcome.COMPILE
          )
        | outcome =>
          ( printqf 0 "Failed to generate: %s" [string targetFile]
          ; OS.FileSys.remove targetFile
          ; outcome
          )
    end

  fun generateL2RefOutputs suiteDir file =
    let
      val test = suiteDir // file
      val checkpointDir = suiteDir ^ "-checkpoint"
      val inputFile = checkpointDir // file ** SOME "in"
      val refDirs = List.map (fn dir => checkpointDir ^ "-" ^ dir) (!Config.dataflowArgs)
      val refFiles = List.map (fn refDir => refDir // file ** SOME "out") refDirs

      fun helper args files =
        case (args, files) of
          ([], []) => Outcome.COMPILE
        | (arg :: args', file :: files') =>
            let
              val outcome = generateFile test file ["--gen2", "--r2", arg]
            in
              if outcome = Outcome.COMPILE then helper args' files' else outcome
            end
        | _ => raise Fail "Impossible"
    in
      helper (!Config.dataflowArgs) refFiles
    end

  (* Tests a compiler with C0C-style options that produces assembly *)
  fun runC0C (test, lib, configs) expected =
    let
      val testLog = prepareTestBase test
      val compileLog = testLog ** SOME "compile"
      val (redirect, log) =
        if !Config.quiet = 0
        then (Runner.UNCHANGED, NONE)
        else
          ( Runner.REDIRECT (Posix.FileSys.creat (compileLog, Util.rw))
          , SOME compileLog
          )
      val libArgs =
        case lib of
          NOLIB _ => []
        | DEFAULT "l3" => [ "-l", ".." // "runtime" // "15411-l3.h0" ]
        | DEFAULT "l4" => [ "-l", ".." // "runtime" // "15411-l4.h0" ]
        | DEFAULT "l5" => [ "-l", ".." // "runtime" // "15411-l5.h0" ]
        | DEFAULT "l6" => [ "-l", ".." // "runtime" // "15411-l5.h0" ]
        | DEFAULT s => raise Fail ("No default for ." ^ s ^ " (runC0C)")
        | HEADER (dir, file) => [ "-l", dir // (file ** SOME "h0") ]

      (* Select between run411.c and run411-no-lib.c based on header file. *)
      val runtime =
        #outFile (
          case lib of
            NOLIB _ => runtimeNoLib
          | _ => runtimeLib
        )

      val runtimeLLVM = #outFile runtimeLLVM

      (* Run a single set of configurations *)
      fun runConfig output config =
        let
          val outputGeneratingConfig =
            case output of
              Config.X86_64 => ["-ex86-64"] (* Default *)
            | Config.LLVM => [ "-ellvm" ] (* Lab 6, LLVM output *)
            | Config.EXE => [ "-eexe" ] (* Lab 6, LLVM output *)
          val optimisation = [ "-O" ^ (LargeInt.toString (!Config.optimisation)) ]
          val safe = if !Config.unsafe then [ "--unsafe" ] else []
          val allConfig =
            libArgs
            @ outputGeneratingConfig
            @ optimisation
            @ safe
            @ config
          val outcome =
            case runC0CCompiler test testLog redirect allConfig of
              Outcome.COMPILE => (
                if expected = Outcome.COMPILE
                then Outcome.COMPILE
                else
                  (* Only at this point do we need to think about what sort of
                   * artifacts the compiler should have created
                   * (Note that runC0CCompiler should have already moved all
                   * these artifacts into the log directory)
                   *)
                  case output of
                    Config.X86_64 =>
                      if Util.isRead (testLog ** SOME "s")
                      then linkAndRun testLog runtime redirect
                      else Outcome.COMPILERFAIL
                        ("Compiler ran but did not create " ^ test ^ ".s")
                  | Config.LLVM =>
                      if Util.isRead (testLog ** SOME "bc")
                      then llvmAndRun testLog "bc" runtimeLLVM redirect
                      else if Util.isRead (testLog ** SOME "ll")
                      then llvmAndRun testLog "ll" runtimeLLVM redirect
                      else Outcome.COMPILERFAIL
                        ("Compiler ran but did not create " ^ test ^ ".ll "
                             ^ "or " ^ test ^ ".bc")
                  | Config.EXE =>
                      if Util.isExec (testLog ** SOME "exe")
                      then runExe testLog []
                      else Outcome.COMPILERFAIL
                        ("Compiler ran but did not create executable " ^ test
                        ^ ".exe")
                )
            | outcome => outcome
        in
          ( Runner.print (redirect, sprintf "-- Got result: %s\n"
              [string (Outcome.toString outcome)])
          ; outcome
          )
        end

      (* Typecheck and, if successful, run all configurations *)
      val outcomes =
        case runC0CTypechecker test testLog redirect libArgs of
          Outcome.TYPECHECK =>
            if tcOnly expected
            then [ Outcome.TYPECHECK ]
            else map (runConfig (!Config.output)) configs
        | outcome => [ outcome ]
    in
     ( Runner.close redirect
     ; destroyTestBase testLog
     ; (outcomes, log)
     )
    end

  fun getFileSize testLog =
    let
      val exe = testLog ** SOME "exe"
      val out = testLog ** SOME "out"
    in
      case Runner.log
        { command = "size"
        , args = [exe]
        , secs = !Config.MAKE_TIMEOUT
        , logBase = testLog
        }
      of
        Posix.Process.W_EXITED => (
          let
            val outFD = TextIO.openIn out
            val _ = TextIO.inputLine outFD (* skip the first line of the size output *)
            val codesize =
              List.nth
                ( String.tokens Char.isSpace (valOf (TextIO.inputLine outFD))
                , 0
                )
          in
            case IntInf.fromString codesize of
              NONE => raise Fail "Reading size of executable from command line failed."
            | SOME i => i before TextIO.closeIn outFD
          end handle _ => raise Fail ("Reading size of executable " ^ exe ^ " failed."))
      | _ => raise Fail ("Getting size of executable " ^ exe ^ " failed.")
    end

  fun benchmark (test, optimize, safe) =
    let
      val testLog = prepareTestBase test
      val compileLog = testLog ** SOME "compile"
      val redirect =
        if !Config.quiet = 0 then Runner.UNCHANGED else Runner.SILENCE
      val optArgs =
        [ "-ex86-64", "-O" ^ Int.toString optimize ]
        @ (if safe then [] else ["--unsafe"])
        @ (List.concat (!Config.compilerArgs))
      val checksum =
        case Outcome.readTestDirective test of
          SOME (Outcome.RETURN v) => v
        | _ => raise Fail "Cannot read test directive from file"
    in
      case runC0CCompiler test testLog redirect optArgs of
        Outcome.COMPILE => ()
      | _ => raise Fail "Compilation did not succeed"
    ; case link testLog (".." // "runtime" // "bench.o") redirect of
        Outcome.COMPILE => ()
      | _ => raise Fail "Linking did not succeed"
    ; case runExe testLog ["-k"] of
        Outcome.RETURN v =>
          if checksum = v
          then ()
          else raise Fail ( "Checksum incorrect: "
                          ^ IntInf.toString checksum
                          ^ " != " ^ IntInf.toString v
                          )
         | Outcome.INFLOOP (SOME Outcome.ALRM) => ()
         | outcome =>
            raise Fail
              ("Checksum computation failed: " ^ Outcome.toString outcome)
    ; case runExe testLog [] of
        Outcome.RETURN cyc =>
          Outcome.TIME_OK (cyc, getFileSize testLog)
      (* Timing out at is ok. Failure or compiler timeouts are not. *)
      | Outcome.INFLOOP (SOME Outcome.ALRM) => Outcome.TIME_INF
      | outcome =>
        raise Fail ("Cycle counting failed: " ^ Outcome.toString outcome)
    end

  (* Tests a compiler with CC0-style options that produces executables *)
  fun runCC0 (test, lib, config) expected =
    let
      val testLog = prepareTestBase test
      val exe = testLog ** SOME "exe"
      val compileLog = testLog ** SOME "compile"
      val (redirect, log) =
         if !Config.quiet = 0
         then (Runner.UNCHANGED, NONE)
         else ( Runner.REDIRECT (Posix.FileSys.creat (compileLog, Util.rw))
              , SOME compileLog
              )

      val libArgs =
        case lib of
          NOLIB _ => []
        | DEFAULT "l3" => [ "-lfpt", "-ralt" ]
        | DEFAULT "l4" => [ "-lfpt", "-ldub", "-ralt" ]
        | DEFAULT "l5" =>
            [ "-lfpt", "-ldub", "-lstring", "-lconio", "-lfile", "-ralt" ]
        | DEFAULT "l6" =>
            [ "-lfpt", "-ldub", "-lstring", "-lconio", "-lfile", "-ralt" ]
        | DEFAULT s => raise Fail ("No default for ." ^ s ^ " (runCC0)")
        | HEADER (dir, file) => [ "-L" ^ dir, "-l" ^ file ]
      val tcArgs =
        if expected = Outcome.ERROR orelse expected = Outcome.TYPECHECK
        then "--only-typecheck" :: config
        else config
      val outcome =
        case Runner.redirect
          { msg = "-- Running compiler on " ^ test ^ " --\n"
          , command = !Config.cc0
          , args = libArgs @ test :: "-o" :: exe :: tcArgs
          , secs = !Config.COMPILER_TIMEOUT
          , out = redirect
          , err = redirect
          }
        of
          Posix.Process.W_EXITED =>
            if expected = Outcome.ERROR then Outcome.TYPECHECK
            else if expected = Outcome.TYPECHECK then Outcome.TYPECHECK
            else if expected = Outcome.COMPILE then Outcome.COMPILE
            else if OS.FileSys.access (exe, [ OS.FileSys.A_EXEC ])
            then runExe testLog config
            else Outcome.COMPILERFAIL ("cc0 did not create " ^ exe)
        | res =>
            if Util.statusIsAlarm res
            then Outcome.TIMEOUT
            else Outcome.ERROR
    in
      case redirect of
        Runner.REDIRECT desc => Posix.IO.close desc
      | _ => ()
      ; destroyTestBase testLog
      ; (outcome, log)
    end
end
