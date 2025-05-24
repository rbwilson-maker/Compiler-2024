(* grader/Suite.sml : 1.1-132.1 *)
(* molasses-file21.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
structure Suite =
  struct

    fun prepareLogDirectory suiteName =
      (Util.mkdir (".." // "log"); Util.mkdir (".." // "log" // suiteName))

    (* Reads (directory, necessaryLibrary, testfile) information out
     * of a directory *)
    fun sourceFilesInDir suiteDir =
      let
        val keep =
          (* keep all files if no pruning *)
          if not (! Config.prune) then
            (fn _ => true)
          else

            (* Build a set of pruned files to keep. *)
            let
              val toKeep =
                Util.fold
                  (fn (line, toKeep) => StringSet.add (toKeep, Util.trim line))
                  StringSet.empty
                  (suiteDir // "keep.txt")
            in
              printqf 1
                "-- Found %s/keep.txt file: pruning; Keeping %d tests. --"
                [string suiteDir, int (StringSet.numItems toKeep)];
              (fn file => StringSet.member (toKeep, file))
            end
              handle
                  exn =>
                    (* If there's no keep.txt file, we just keep everything. *)
                    ( printqf 1
                        ("-- No %s/keep.txt file found -- " ^ "not pruning; all tests will be run --")
                        [string suiteDir]
                    ; (fn _ => true)
                    )

        (* Primary filter: optional, given on command line.
          *   Also check that file is pruned. *)
        val primaryFilter =
          case ! Config.filterExt of
            NONE => List.filter keep
          | SOME ext =>
              List.filter
                (fn name => OS.Path.ext name = SOME ext andalso keep name)

        (* Secondary filter also checks for headers *)
        fun secondaryFilter filename =
          Option.map (fn x => (suiteDir, x, filename))
            (case OS.Path.splitBaseExt filename of
               {ext = SOME "h0", ...} => NONE
             (* Ignore header files *)
             | {base, ext = SOME "l1"} => SOME (Compiler.NOLIB "l1")
             | {base, ext = SOME "l2"} => SOME (Compiler.NOLIB "l2")
             | {base, ext = SOME ext} =>
                 if Util.isRead (suiteDir // base ** SOME "h0") then
                   SOME (Compiler.HEADER (suiteDir, base))
                 else
                   SOME (Compiler.DEFAULT ext)
             | _ => SOME (Compiler.NOLIB ""))
      in
        List.mapPartial secondaryFilter (primaryFilter (Util.ls suiteDir))
      end

    local
      structure E = Either
      structure S = Summary
    in
      fun mapSuite f {suiteName, suiteDir} =
        let
          val (unknownErrorTests, summaries) =
            ( prepareLogDirectory suiteName
            ; MapReduce.map
                (E.toValue
                 o E.map TestFile.testErrorToValue S.toValue
                 o f)
                (sourceFilesInDir suiteDir)
            )

          (* A summary test is either:
           *   - Ran successfully as a valid test (good)
           *   - Malformed (aka invalid)
           *   - Determined that we should skip (e.g. l4-unsafe skips safe tests)
           * Here, we split summaries into good and invalid tests,
           *   discarding skip tests (NONE)
           *)
          val (invalidTestsOpt, goodTests) =
            E.partition
              (fn (x, v) =>
                 case E.map TestFile.testErrorFromValue S.fromValue
                        (E.fromValue v) of
                   E.ERR TestFile.INVALID => E.ERR (SOME x)
                 | E.ERR TestFile.SKIP => E.ERR NONE
                 | E.OK s => E.OK (x, s))
              summaries

          (* Filter out NONE *)
          val invalidTests = List.mapPartial (fn x => x) invalidTestsOpt
          val badTests = unknownErrorTests @ invalidTests
        in
          { suite = suiteName
          , badTests = map (fn (x, y, z) => (x, z)) badTests
          , goodTests = goodTests
          }
        end
    end

    val map = mapSuite

    (* Checks that all given test suites are really test suites. *)
    (* Returns the suite's name and directory. *)
    val filter =
      List.mapPartial
        (fn file =>
           let
             val file = OS.Path.mkCanonical file
             fun fail msg =
               (printqf 0 (Util.dull "-- %s --") [string msg]; NONE)
           in
             case OS.Path.splitDirFile file of
               {dir = "", file = _} =>
                 if Util.isDir file then
                   SOME {suiteName = file, suiteDir = file}
                 else if Util.isDir (".." // "tests" // file) then
                   SOME {suiteName = file, suiteDir = ".." // "tests" // file}
                 else
                   ( printqf 0 "Cannot find suite %s or ../tests/%s, ignoring"
                       [string file, string file]
                   ; NONE
                   )
             | {dir = _, file = filepart} =>
                 if Util.isDir file then
                   SOME {suiteName = filepart, suiteDir = file}
                 else
                   fail ("Cannot find suite at " ^ file ^ ", ignoring")
           end)
  end

end
