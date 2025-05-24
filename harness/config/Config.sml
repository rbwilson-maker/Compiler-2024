(* Mutable state initialized by the call to Args.init.
 * Each entry point (gradeTests, benchCompiler, gradeCompiler) makes a call
 * to Args.init.
 *)
structure Config = struct
  val MAX_TEST_LIMIT: int = 255 (* Database limit, don't change for F15 *)
  val MIN_VERSION: int = 522 (* Minimum correctly configured cc0 vers *)

  val debug : bool ref = ref false
  val TC_TIMEOUT : IntInf.int ref = ref 1
  val COMPILER_TIMEOUT : IntInf.int ref = ref 1
  val MAKE_TIMEOUT : IntInf.int ref = ref 1
  val GCC_TIMEOUT : IntInf.int ref = ref 1
  val RUN_TIMEOUT : IntInf.int ref = ref 1
  val MAX_FILENAME_BASE : int ref = ref 1
  val PARALLELISM : int ref = ref 1
  val quiet : IntInf.int ref = ref 0
  (* quiet 0: no suppression *)
  (* quiet 1+: suppress dull messages, output from PASS tests *)
  (* quiet 2+: suppress everything about PASS tests *)
  (* quiet 3+: suppress output from FAIL tests *)
  (* quiet 4+: suppress summaries of FAIL tests, output from make *)
  (* quiet 5+: suppress everything about FAIL tests *)
  (* quiet 6+: suppress all testing output (only show summary) *)
  (* quiet 7+: suppress detailed summary output (only show stats) *)
  val symlink = ref false
  val prune = ref false
  val mac = ref false
  val color = ref true
  val cc0 = ref "cc0"
  val dataflowArgs = ref ["forward-may", "forward-must", "backward-may", "backward-must"]
  (* directory containing analyze-tests and diff-with-test-suite binaries *)
  val staticAnalysisDir : string option ref = ref NONE

  (* for these to be true, staticAnalysisDir must NOT be NONE *)
  (* fail tests that duplicate others in suite *)
  val failDuplicateTests : bool ref = ref false
  val warnDuplicateTests : bool ref = ref false
  (* fail tests that exercise the compiler bug *)
  val failDodgyTests : bool ref = ref false

  val make : string option option ref = ref (SOME NONE)
  val relax = ref false (* Relaxed validation of test cases *)
  val log = ref true (* Keep all log files *)
  val compilerArgs : string list list ref = ref [[]] (* Extra arguments *)
  val filterExt : string option ref = ref NONE (* Test filter *)
  val autograder = ref false (* Print autograder output *)

  (* whether to run only safe or unsafe tests *)
  val safeOnly : bool ref = ref false
  val unsafeOnly : bool ref = ref false
  val typecheckOnly : bool ref = ref false
  val skipFrontendTests : bool ref = ref false
  val allowInfloopTests : bool ref = ref false (* whether infloop tests are allowed *)

  datatype output = X86_64 | LLVM | EXE
  val output = ref X86_64
  val optimisation : IntInf.int ref = ref 0
  val unsafe = ref false

  (* What signals are allowed to count as process timeout? *)
  val timeSig : Posix.Signal.signal list ref = ref []

  (* What exit statuses are allowed to count as process timeout? *)
  val timeStatus : Word8.word list ref = ref []
end
