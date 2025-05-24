(******************************************************
 * Command-line arguments
 ******************************************************)
structure Args =
struct
  exception BadOpt of string
  exception ShowUsage

  (* Adds all the args to all the current fields *)
  fun addArgs args =
    let
      val current = !Config.compilerArgs
      val new = String.fields (fn c => c = #",") args
      fun appendToEnd arg =
        if arg = "" then id else (fn args => args @ [ arg ])
    in
      Config.compilerArgs :=
        concatMap (fn arg => map (appendToEnd arg) current) new
    end

  (* On AFS, we want the reference compiler to NOT be the 122 compiler *)
  fun findCC0 () =
    let
      val afsCompiler = "/afs/cs.cmu.edu/academic/class/15411-f15/cc0/bin/cc0"
    in
      if Util.isExec afsCompiler then afsCompiler else "cc0"
    end

  fun intArg (name, f) =
    let
      val f = fn s =>
        case IntInf.fromString s of
          NONE => raise BadOpt s
        | SOME i => f i
    in
      GetOpt.ReqArg (f, name)
    end

  fun addDataflowArg arg s =
    let
      val args = !Config.dataflowArgs
    in
      if List.length args = 4
      then Config.dataflowArgs := [arg]
      else Config.dataflowArgs := args @ [arg]
    end

  val options =
    [ { short = "c"
      , long = ["color"]
      , desc = GetOpt.ReqArg
          (fn "on" => Config.color := true
            | "off" => Config.color := false
            | c => raise BadOpt ("-c" ^ c), "{on,off}")
      , help = "Terminal coloring"
      }
    , { short = "q"
      , long = []
      , desc = GetOpt.OptArg
          (fn NONE => Config.quiet := 1
            | SOME "q" => Config.quiet := 2
            | SOME "qq" => Config.quiet := 3
            | SOME "qqq" => Config.quiet := 4
            | SOME "qqqq" => Config.quiet := 5
            | SOME "qqqqq" => Config.quiet := 6
            | SOME "qqqqqq" => Config.quiet := 7
            | SOME c => raise BadOpt ("-q" ^ c), "qqqqqq")
      , help = "Quiet (use -q through -qqqqqqq)"
      }
    , { short = ""
      , long = ["follow-symlinks"]
      , desc = GetOpt.NoArg (fn () => Config.symlink := true)
      , help = "Use symlinked files for tests"
      }
    , { short = ""
      , long = ["nomake"]
      , desc = GetOpt.NoArg (fn () => Config.make := NONE)
      , help = "Don't rebuild bin/c0c"
      }
    , { short = "m"
      , long = ["make"]
      , desc = GetOpt.ReqArg (fn arg =>
          Config.make := SOME (SOME arg), "<lab>")
      , help = "Build compiler as 'make <lab>'"
      }
    , { short = ""
      , long = ["cc0"]
      , desc = GetOpt.ReqArg (fn s => Config.cc0 := s, "<path to cc0>")
      , help = "Path to reference compiler"
      }
    , { short = "h"
      , long = ["help"]
      , desc = GetOpt.NoArg (fn () => raise ShowUsage)
      , help = "Show this help message"
      }
    , { short = "j"
      , long = ["parallel"]
      , desc = GetOpt.ReqArg
          (fn i => case Int.fromString i
                     of NONE => raise BadOpt ("-j" ^ i)
                      | SOME i => Config.PARALLELISM := i, "i")
      , help = "Number of tests to run in parallel"
      }
    , { short = "a"
      , long = ["args"]
      , desc = GetOpt.ReqArg (addArgs, "a")
      , help = "Add comma-separated args for compiler"
      }
    , { short = "e"
      , long = ["emit"]
      , desc = GetOpt.ReqArg
          (fn "x86-64" => Config.output := Config.X86_64
            | "exe" => Config.output := Config.EXE
            | "llvm" => Config.output := Config.LLVM
            | c => raise BadOpt ("-o" ^ c), "x86-64")
      , help = "Compiler variant (x86-64, exe, llvm)"
      }
    , { short = ""
      , long = ["unsafe"]
      , desc = GetOpt.NoArg (fn () => Config.unsafe := true)
      , help = "Whether to run the compiler in safe/unsafe mode"
      }
    , { short = "O"
      , long = []
      , desc = intArg ("n", fn i => Config.optimisation := i)
      , help = "Level of optimisation to pass to the compiler"
      }
    , { short = ""
      , long = ["prune"]
      , desc = GetOpt.NoArg (fn () => Config.prune := true)
      , help = "Whether to run only those tests given in keep.txt"
      }
    ,
      { short = ""
      , long = ["mac"]
      , desc = GetOpt.NoArg (fn () => Config.mac := true)
      , help = "Whether to run verifier mac executable"
      }
    ,
    { short = ""
      , long = ["static-analysis-dir"]
      , desc = GetOpt.ReqArg
          (fn s => Config.staticAnalysisDir := SOME s, "dir")
      , help = "The directory containing binaries for performing static analysis"
      }
    , { short = ""
      , long = ["fail-duplicate-tests"]
      , desc = GetOpt.NoArg (fn () => Config.failDuplicateTests := true)
      , help = "Whether to fail duplicate tests. (NOTE: requires static-analysis-dir)"
      }
    , { short = ""
      , long = ["warn-duplicate-tests"]
      , desc = GetOpt.NoArg (fn () => Config.warnDuplicateTests := true)
      , help = "Whether to print a warning message on duplicate tests. (NOTE: requires static-analysis-dir)"
      }
    , { short = ""
      , long = ["fail-dodgy-tests"]
      , desc = GetOpt.NoArg (fn () => Config.failDodgyTests := true)
      , help = "Whether to fail buggy tests. (NOTE: requires static-analysis-dir)"
      }
    , { short = ""
      , long = ["unsafe-only"]
      , desc = GetOpt.NoArg (fn () => Config.unsafeOnly := true)
      , help = "Whether to run only unsafe (i.e., mem-error, div-by-zero) tests"
      }
    , { short = ""
      , long = ["safe-only"]
      , desc = GetOpt.NoArg (fn () => Config.safeOnly := true)
      , help = "Whether to run only safe (i.e., returning, typecheck) tests"
      }
    , { short = ""
      , long = ["typecheck-only"]
      , desc = GetOpt.NoArg (fn () => Config.typecheckOnly := true)
      , help = "Whether to only run each test for typechecking, not runtime."
      }
    , { short = ""
      , long = ["skip-frontend-tests"]
      , desc = GetOpt.NoArg (fn () => Config.skipFrontendTests := true)
      , help = "Skips tests that only check frontend behavior "
      }
    , { short = ""
      , long = ["allow-infloop-tests"]
      , desc = GetOpt.NoArg (fn () => Config.allowInfloopTests := true)
      , help = "If present, allow infloop tests."
      }
    , { short = ""
      , long = ["limit-make"]
      , desc = intArg ("sec", fn i => Config.MAKE_TIMEOUT := i)
      , help = "Compiler build time limit (1800 seconds)"
      }
    , { short = ""
      , long = ["limit-tc"]
      , desc = intArg ("sec", fn i => Config.TC_TIMEOUT := i)
      , help = "Typechecker time limit (4 seconds)"
      }
    , { short = ""
      , long = ["limit-compile"]
      , desc = intArg ("sec", fn i => Config.COMPILER_TIMEOUT := i)
      , help = "Compiler time limit (6 seconds)"
      }
    , { short = ""
      , long = ["limit-link"]
      , desc = intArg ("sec", fn i => Config.GCC_TIMEOUT := i)
      , help = "Linker time limit (8 seconds)"
      }
    , { short = ""
      , long = ["limit-run"]
      , desc = intArg ("sec", fn i => Config.RUN_TIMEOUT := i)
      , help = "Execution time limit (5 seconds)"
      }
    , { short = ""
      , long = ["limit-filename"]
      , desc = intArg ("n", fn i =>
          Config.MAX_FILENAME_BASE := Int.fromLarge i)
      , help = "Max length of a filename (37 chars)"
      }
    , { short = ""
      , long = ["relax"]
      , desc = GetOpt.NoArg (fn () => Config.relax := true)
      , help = "Relaxed test case validation"
      }
    , { short = ""
      , long = ["nolog"]
      , desc = GetOpt.NoArg (fn s => Config.log := false)
      , help = "Delete all log files"
      }
    , { short = ""
      , long = ["debug"]
      , desc = GetOpt.NoArg (fn s => Config.debug := true)
      , help = "Debug information"
      }
    , { short = "f"
      , long = ["filter"]
      , desc = GetOpt.ReqArg (fn ext => Config.filterExt := SOME ext, "lN")
      , help = "Only test specific extension"
      }
    , { short = ""
      , long = ["forward-may"]
      , desc = GetOpt.NoArg (addDataflowArg "forward-may")
      , help = "Run l2 checkpoint for forward-may direction"
      }
    , { short = ""
      , long = ["forward-must"]
      , desc = GetOpt.NoArg (addDataflowArg "forward-must")
      , help = "Run l2 checkpoint for forward-must direction"
      }
    , { short = ""
      , long = ["backward-must"]
      , desc = GetOpt.NoArg (addDataflowArg "backward-must")
      , help = "Run l2 checkpoint for backward-must direction"
      }
    , { short = ""
      , long = ["backward-may"]
      , desc = GetOpt.NoArg (addDataflowArg "backward-may")
      , help = "Run l2 checkpoint for backward-may direction"
      }
    , { short = ""
      , long = ["autograder"]
      , desc = GetOpt.NoArg (fn s => Config.autograder := true)
      , help = "Produce autograder output"
      }
    ]
  val usage =
    GetOpt.usageInfo { header = "411 Autograder", options = options }

  (* General initialization, shared by all graders *)
  fun init args =
    ( Config.debug := false
    ; Config.TC_TIMEOUT := 4
    ; Config.COMPILER_TIMEOUT := 6
    ; Config.MAKE_TIMEOUT := 1800
    ; Config.GCC_TIMEOUT := 8
    ; Config.RUN_TIMEOUT := 5
    ; Config.MAX_FILENAME_BASE := 37
    ; Config.PARALLELISM := 1
    ; Config.quiet := 0
    ; Config.staticAnalysisDir := NONE
    ; Config.failDuplicateTests := false
    ; Config.warnDuplicateTests := false
    ; Config.failDodgyTests := false
    ; Config.skipFrontendTests := false
    ; Config.allowInfloopTests := false
    ; Config.symlink := false
    ; Config.color := true
    ; Config.cc0 := findCC0 ()
    ; Config.make := SOME NONE
    ; Config.filterExt := NONE
    ; Config.safeOnly := false
    ; Config.unsafeOnly := false
    ; Config.autograder := false
    ; Config.relax := false
    ; Config.log := true
    ; Config.compilerArgs := [[]]
    ; Config.output := Config.X86_64
    ; Config.optimisation := 0
    ; Config.unsafe := false
    ; Config.timeSig :=
      [ Posix.Signal.ill, (* Rust compiler, possibly remove -rjs F15 *)
        Posix.Signal.bus, (* Stack overflow on Ubuntu/Autograder *)
        Posix.Signal.segv, (* Stack overflow on OSX/RHEL, sometimes Ubuntu *)
        Posix.Signal.alrm, (* Autograder in normal behavior *)
        Posix.Signal.kill ] (* Backup kill signal *)
    ; Config.timeStatus :=
      [ 0wx8 ] (* SBCL student solution, probably remove -rjs F15 *)
    ; let
        val (_, result) = GetOpt.getOpt
          { argOrder = GetOpt.Permute
          , errFn = fn s => raise BadOpt s
          , options = options
          } args
      in
        if !Config.failDodgyTests andalso not (isSome (!Config.staticAnalysisDir))
        then failwith "--fail-dodgy-tests requires a static analysis dir"
        else ();
        if (!Config.failDuplicateTests orelse !Config.warnDuplicateTests)
              andalso not (isSome (!Config.staticAnalysisDir))
        then failwith "--fail-duplicate-tests (and --warn-duplicate-tests) requires a static analysis dir"
        else ();
        result
      end
    )
end
