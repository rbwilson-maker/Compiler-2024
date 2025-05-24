(* score/Outcome.sml : 1.1-207.1 *)
(* molasses-file10.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
(**************************************************************************)
(* High-level representation of the result of test cases                  *)
(**************************************************************************)
structure Outcome =
  struct
    (* The amount of information we have about the behavior of a test
     * case moves up as we run more tests.
     *
     *      INFLOOP  ABORT  MEMERROR  ARITH  RETURN  RUNFAIL
     *             \      \      |      /      /     /
     *              \      \     |     /      /     /
     * COMPILERFAIL  ---COMPILE---------------------
     *             \       |
     *              TYPECHECK  ERROR
     *                 |     /
     *               TIMEOUT
     *
     *)

    datatype infloop = ALRM | SEGV | BUS
    (* Known infloop outcomes *)
    datatype outcome =
      TIMEOUT
    (* Compiler timeout WITH or WITHOUT -t *)
    | ERROR
    (* Compiler failure WITH or WITHOUT -t *)
    | TYPECHECK
    (* Compiler success WITH -t, timeout otherwise *)
    | COMPILE
    (* Compiler success *)
    | COMPILERFAIL of string
    (* Impossible for this to be correct *)
    | INFLOOP of infloop option
    (* Insufficient resources to finish *)
    | ABORT
    (* Raises SIGABRT *)
    | ARITH
    (* Raises SIGFPT *)
    | MEMERROR
    (* Controlled signaling of a memory error (USR2) *)
    | RETURN of IntInf.int
    (* Returns a 32-bit integer from main *)
    | RUNFAIL of string
    (* Impossible for this to be correct *)
    | OR_INFLOOP of outcome
    (* Either infloops or has outcome. *)
    type t = outcome
    (* first int is runtime. second int is code size in number of bytes *)
    datatype bench_result = TIME_OK of IntInf.int * IntInf.int | TIME_INF

    fun orInfloop res =
      let
        val canMeaningfullyBeCombinedWithInfLoop =
          case res of
            INFLOOP _ => false
          | OR_INFLOOP _ => false
          | RETURN _ => true
          | ABORT => true
          | ARITH => true
          | MEMERROR => true
          | _ => false
      in
        if canMeaningfullyBeCombinedWithInfLoop then
          SOME (OR_INFLOOP res)
        else
          NONE
      end

    (* If we have (INFLOOP, notInfloop) and (notInfloop, INFLOOP),
     * yield OR_INFLOOP (notInfloop) *)
    fun outcomeOfPair o1 o2 =
      case (o1, o2) of
        (INFLOOP _, _) => orInfloop o2
      | (OR_INFLOOP _, _) => NONE
      | (_, INFLOOP _) => orInfloop o1
      | _ => NONE

    (* Try to read the first line of a file to get a test directive *)
    fun readTestDirective test : outcome option =
      let
        val file = TextIO.openIn test
        val line = TextIO.inputLine file before TextIO.closeIn file

        fun loopOr [] result = SOME result
          | loopOr ("||" :: rest) result =
            Option.mapPartial (outcomeOfPair result) (loop rest)
          | loopOr _ _ = NONE

        and loop ("typecheck" :: rest) = loopOr rest TYPECHECK
          | loop ("error" :: rest) = loopOr rest ERROR
          | loop ("compile" :: rest) = loopOr rest COMPILE
          | loop ("infloop" :: rest) = loopOr rest (INFLOOP NONE)
          | loop ("abort" :: rest) = loopOr rest ABORT
          | loop ("memerror" :: rest) = loopOr rest MEMERROR
          | loop ("div-by-zero" :: rest) = loopOr rest ARITH
          | loop ("return" :: n :: rest) =
            Option.mapPartial (loopOr rest)
              (Option.map RETURN (IntInf.fromString n))
          | loop _ = NONE
      in
        case Option.map (String.tokens Char.isSpace) line of
          NONE => NONE
        | SOME ("//test" :: tokens) => loop tokens
        | SOME _ => NONE
      end
        handle _ => NONE

    fun readL1CheckpointDirective test : IntInf.int option =
      let
        val file = TextIO.openIn test
        val line = TextIO.inputLine file before TextIO.closeIn file
      in
        case Option.map (String.tokens Char.isSpace) line of
          NONE => NONE
        | SOME ("//target" :: num :: other) => IntInf.fromString num
        | SOME _ => NONE
      end

    fun toString outcome =
      case outcome of
        TIMEOUT => "compiler timed out"
      | ERROR => "error"
      | TYPECHECK => "typecheck"
      | COMPILE => "compile"
      | COMPILERFAIL s => "compiler failure"
      | INFLOOP NONE => "executable timed out"
      | INFLOOP (SOME ALRM) => "executable timed out (sigalrm)"
      | INFLOOP (SOME SEGV) => "executable segfault"
      | INFLOOP (SOME BUS) => "executable raised sigbus"
      | ABORT => "abort"
      | ARITH => "div-by-zero"
      | MEMERROR => "memerror"
      | RETURN n => "return " ^ IntInf.toString n
      | RUNFAIL s => "runtime failure"
      | OR_INFLOOP outcome => toString outcome ^ " OR executable timed out"

    fun matches {expected : outcome, actual : outcome} : bool =
      case (expected, actual) of
        (INFLOOP _, INFLOOP _) => true
      | (COMPILERFAIL _, COMPILERFAIL _) => false
      (* Impossible? *)
      | (OR_INFLOOP _, INFLOOP _) => true
      | (OR_INFLOOP expected, _) => expected = actual
      | _ => expected = actual

    fun reportHelper test logfile {expected, actual} hasDirective =
      let
        val msg =
          case (expected, actual) of
            (_, TIMEOUT) => "compiler timed out while parsing and typechecking"
          | (ERROR, TYPECHECK) => "code parsed and checked successfully"
          | (ERROR, COMPILE) => "code compiled successfully"
          | (_, TYPECHECK) => "compiler timed out after typechecking"
          | (_, ERROR) => "code rejected as ill-formed"
          | (_, COMPILERFAIL s) => s
          | (_, INFLOOP NONE) => "program failed to terminate"
          | (_, INFLOOP (SOME ALRM)) => "program timed out (sigalrm)"
          | (_, INFLOOP (SOME SEGV)) =>
              "program failed to terminate \
              \due to memory error (possibly stack overflow?)"
          | (_, INFLOOP (SOME BUS)) =>
              "program failed to terminate \
              \due to memory error (probably stack overflow)"
          | (_, ABORT) => "program raised SIGABRT"
          | (_, ARITH) => "program encountered arithmetic error"
          | (_, MEMERROR) => "program signaled a safe memory error"
          | (_, RETURN n) => "return " ^ IntInf.toString n
          | (_, RUNFAIL s) => s
          | _ => "unexpected outcome (contact course staff?)"
      in
        if matches {actual = actual, expected = expected} then
          printqf 1 (Util.green "-- PASS: %s --") [string test]
        else
          case logfile of
            NONE => ()
          | SOME file =>
              ( if ! Config.quiet <= 2 then
                  (print "\n"; Util.cat file)
                else
                  ()
              ; printqf 4 (Util.red "-- FAIL: %s --") [string test]
              ; if hasDirective then
                  (printqf 3 (Util.red "   directive: %s")
                     [string (Util.head test)])
                else
                  ()
              ; printqf 3 (Util.red "     outcome: %s") [string msg]
              )
      end

    fun report test logfile summary = reportHelper test logfile summary true
    fun reportNoDirective test logfile summary =
      reportHelper test logfile summary false

    (* whether a test is considered "safe" (i.e., can be run without bounds
     * checks and div-by-zero checks) *)
    val rec isSafe =
      fn TIMEOUT => false
       | ERROR => true
       | TYPECHECK => true
       | COMPILE => true
       | COMPILERFAIL _ => false
       | INFLOOP _ => true
       | ABORT => true
       | ARITH => false
       | MEMERROR => false
       | RETURN _ => true
       | RUNFAIL _ => false
       | OR_INFLOOP out => isSafe out

    (* tests that are parse/typechecking test only (doesn't check runtime) *)
    val rec isFrontendTest =
      fn TIMEOUT => false
       | ERROR => true
       | TYPECHECK => true
       | COMPILE => false
       | COMPILERFAIL _ => false
       | INFLOOP _ => false
       | ABORT => false
       | ARITH => false
       | MEMERROR => false
       | RETURN _ => false
       | RUNFAIL _ => false
       | OR_INFLOOP out => isFrontendTest out
  end

end
